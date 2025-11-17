using ElmTime.Platform.WebService;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using Pine.Core.Files;
using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;

namespace ElmTime;

public class RunServer
{
    public static Microsoft.AspNetCore.Builder.WebApplication BuildWebHostToRunServer(
        string? processStorePath,
        string? processStoreReadonlyPath,
        string? adminInterfaceUrls,
        string? adminPassword,
        IReadOnlyList<string>? publicAppUrls,
        bool deletePreviousProcess,
        string? copyProcess,
        string? deployApp)
    {
        if ((deletePreviousProcess || copyProcess is not null) && processStorePath is not null)
        {
            Console.WriteLine("Deleting the previous process state from '" + processStorePath + "'...");

            if (Directory.Exists(processStorePath))
                Directory.Delete(processStorePath, true);

            Console.WriteLine("Completed deleting the previous process state from '" + processStorePath + "'.");
        }

        IFileStore BuildProcessStoreFileStore()
        {
            if (processStorePath is not null)
            {
                var retryOptions =
                    new FileStoreFromSystemIOFile.FileStoreRetryOptions(
                        InitialRetryDelay: TimeSpan.FromMilliseconds(100),
                        MaxRetryDelay: TimeSpan.FromSeconds(4),
                        MaxRetryAttempts: 10);

                return new FileStoreFromSystemIOFile(
                    processStorePath,
                    retryOptions: retryOptions);
            }

            Console.WriteLine("I got no path to a persistent store for the process. This process will not be persisted!");

            var inMemoryFileStore = new FileStoreFromConcurrentDictionary();

            return
                new FileStoreFromWriterAndReader(inMemoryFileStore, inMemoryFileStore);
        }

        var processStoreFileStore = BuildProcessStoreFileStore();

        if (processStoreReadonlyPath is not null)
        {
            Console.WriteLine("Merging read-only process store from '" + processStoreReadonlyPath + "'.");

            processStoreFileStore =
                processStoreFileStore.MergeReader(
                    new FileStoreFromSystemIOFile(processStoreReadonlyPath),
                    promoteOnReadFileContentFromSecondary: true);
        }

        if (copyProcess is not null)
        {
            var copyFiles =
                LoadFilesForRestoreFromPathAndLogToConsole(
                    sourcePath: copyProcess,
                    sourcePassword: null);

            foreach (var file in copyFiles)
                processStoreFileStore.SetFileContent(file.Key.ToImmutableList(), file.Value.ToArray());
        }

        if (deployApp is not null)
        {
            Console.WriteLine("Loading app config to deploy...");

            var appConfigZipArchive =
                BuildConfigurationFromArguments.BuildConfigurationZipArchiveFromPath(
                    sourcePath: deployApp).configZipArchive;

            var appConfigTree =
                FileTree.FromSetOfFilesWithCommonFilePath(
                    ZipArchive.EntriesFromZipArchive(appConfigZipArchive));

            var appConfigComponent = FileTreeEncoding.Encode(appConfigTree);

            var processStoreWriter =
                new Platform.WebService.ProcessStoreSupportingMigrations.ProcessStoreWriterInFileStore(
                    processStoreFileStore,
                    getTimeForCompositionLogBatch: () => DateTimeOffset.UtcNow,
                    processStoreFileStore,
                    skipWritingComponentSecondTime: true);

            processStoreWriter.StoreComponent(appConfigComponent);

            var appConfigValueInFile =
                new Platform.WebService.ProcessStoreSupportingMigrations.ValueInFileStructure
                {
                    HashBase16 = Convert.ToHexStringLower(PineValueHashTree.ComputeHash(appConfigComponent).Span)
                };

            var initElmAppState =
                (deletePreviousProcess || processStorePath is null) && copyProcess is null;

            var compositionLogEvent =
                Platform.WebService.ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent.EventForDeployAppConfig(
                    appConfigValueInFile: appConfigValueInFile,
                    initElmAppState: initElmAppState);

            var testDeployResult =
                PersistentProcessLive.TestContinueWithCompositionEvent(
                    compositionLogEvent: compositionLogEvent,
                    fileStoreReader: processStoreFileStore)
                .Extract(error => throw new Exception("Attempt to deploy app config failed: " + error));

            foreach (var (filePath, fileContent) in testDeployResult.ProjectedFiles)
                processStoreFileStore.SetFileContent(filePath, fileContent);
        }

        var builder = Microsoft.AspNetCore.Builder.WebApplication.CreateBuilder();

        builder.Configuration.AddEnvironmentVariables("APPSETTING_");

        if (adminInterfaceUrls is not null)
            builder.WebHost.UseUrls(adminInterfaceUrls);

        builder.Services.AddSingleton(new FileStoreForProcessStore(processStoreFileStore));

        if (publicAppUrls is not null)
        {
            builder.Configuration[Configuration.PublicWebHostUrlsSettingKey] = string.Join(",", publicAppUrls);
        }

        if (adminPassword is not null)
        {
            builder.Configuration[Configuration.AdminPasswordSettingKey] = adminPassword;
        }

        StartupAdminInterface.ConfigureServices(builder.Services);

        var app = builder.Build();

        var startup = new StartupAdminInterface(app.Services.GetRequiredService<ILogger<StartupAdminInterface>>());

        startup.Configure(
            app,
            app.Environment,
            app.Lifetime,
            app.Services.GetRequiredService<Func<DateTimeOffset>>(),
            app.Services.GetRequiredService<FileStoreForProcessStore>());

        return app;
    }

    public static IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> LoadFilesForRestoreFromPathAndLogToConsole(
        string sourcePath, string? sourcePassword)
    {
        if (!Program.LooksLikeLocalSite(sourcePath))
        {
            Console.WriteLine("Begin reading process history from '" + sourcePath + "' ...");

            var (files, lastCompositionLogRecordHashBase16) = ReadFilesForRestoreProcessFromAdminInterface(
                sourceAdminInterface: sourcePath,
                sourceAdminPassword: sourcePassword);

            Console.WriteLine("Completed reading files to restore process " + lastCompositionLogRecordHashBase16 + ". Read " + files.Count + " files from '" + sourcePath + "'.");

            return files;
        }

        var archive = File.ReadAllBytes(sourcePath);

        var zipArchiveEntries = ZipArchive.EntriesFromZipArchive(archive);

        return
            FileTreeExtensions.ToFlatDictionaryWithPathComparer(
                FileTree.FromSetOfFilesWithCommonFilePath(zipArchiveEntries)
                .EnumerateFilesTransitive());
    }

    public static void ReplicateProcessAndLogToConsole(
        string site,
        string sitePassword,
        string sourcePath,
        string sourcePassword)
    {
        var restoreFiles =
            LoadFilesForRestoreFromPathAndLogToConsole(sourcePath: sourcePath, sourcePassword: sourcePassword);

        var processHistoryTree =
            FileTree.FromSetOfFilesWithStringPath(restoreFiles);

        var processHistoryComponentHash = PineValueHashTree.ComputeHashNotSorted(processHistoryTree);
        var processHistoryComponentHashBase16 = Convert.ToHexStringLower(processHistoryComponentHash.Span);

        var processHistoryZipArchive = ZipArchive.ZipArchiveFromFiles(restoreFiles);

        using var httpClient = new System.Net.Http.HttpClient();

        httpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
            "Basic",
            Convert.ToBase64String(Encoding.UTF8.GetBytes(Configuration.BasicAuthenticationForAdmin(sitePassword))));

        var deployAddress =
            site.TrimEnd('/') +
            StartupAdminInterface.PathApiReplaceProcessHistory;

        Console.WriteLine("Beginning to place process history '" + processHistoryComponentHashBase16 + "' at '" + deployAddress + "'...");

        var httpContent = new System.Net.Http.ByteArrayContent(processHistoryZipArchive);

        httpContent.Headers.ContentType = new System.Net.Http.Headers.MediaTypeHeaderValue("application/zip");
        httpContent.Headers.ContentDisposition =
            new System.Net.Http.Headers.ContentDispositionHeaderValue("attachment") { FileName = processHistoryComponentHashBase16 + ".zip" };

        var httpResponse = httpClient.PostAsync(deployAddress, httpContent).Result;

        Console.WriteLine(
            "Server response: " + httpResponse.StatusCode + "\n" +
             httpResponse.Content.ReadAsStringAsync().Result);
    }

    public static (IImmutableDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> files, string lastCompositionLogRecordHashBase16) ReadFilesForRestoreProcessFromAdminInterface(
        string sourceAdminInterface,
        string? sourceAdminPassword)
    {
        using var sourceHttpClient = new System.Net.Http.HttpClient { BaseAddress = new Uri(sourceAdminInterface) };

        sourceHttpClient.DefaultRequestHeaders.Authorization = new System.Net.Http.Headers.AuthenticationHeaderValue(
            "Basic",
            Convert.ToBase64String(Encoding.UTF8.GetBytes(Configuration.BasicAuthenticationForAdmin(sourceAdminPassword))));

        var processHistoryFileStoreRemoteReader = new DelegatingFileStoreReader
        (
            ListFilesInDirectoryDelegate: directoryPath =>
            {
                var httpRequestPath =
                    StartupAdminInterface.PathApiProcessHistoryFileStoreListFilesInDirectory + "/" +
                    string.Join("/", directoryPath);

                var response = sourceHttpClient.GetAsync(httpRequestPath).Result;

                if (!response.IsSuccessStatusCode)
                    throw new Exception("Unexpected response status code: " + (int)response.StatusCode + " (" + response.StatusCode + ").");

                return
                    response.Content.ReadAsStringAsync().Result.Split('\n', StringSplitOptions.RemoveEmptyEntries)
                    .Select(path => path.Split('/').ToImmutableList());
            },
            GetFileContentDelegate: filePath =>
            {
                var httpRequestPath =
                    StartupAdminInterface.PathApiProcessHistoryFileStoreGetFileContent + "/" +
                    string.Join("/", filePath);

                var response = sourceHttpClient.GetAsync(httpRequestPath).Result;

                if (response.StatusCode is System.Net.HttpStatusCode.NotFound)
                    return null;

                if (!response.IsSuccessStatusCode)
                    throw new Exception("Unexpected response status code: " + (int)response.StatusCode + " (" + response.StatusCode + ").");

                return response.Content.ReadAsByteArrayAsync().Result;
            }
        );

        return PersistentProcessLive.GetFilesForRestoreProcess(processHistoryFileStoreRemoteReader);
    }
}
