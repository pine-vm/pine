using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using ElmTime.JavaScript;
using ElmTime.Platform.WebService;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.Configuration;
using Pine;

namespace ElmTime;

public class RunServer
{
    public static IWebHost BuildWebHostToRunServer(
        string? processStorePath,
        string? adminInterfaceUrls,
        string? adminPassword,
        IReadOnlyList<string>? publicAppUrls,
        ElmInteractive.ElmEngineType elmEngineType,
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

        IFileStore buildProcessStoreFileStore()
        {
            if (processStorePath is not null)
                return new FileStoreFromSystemIOFile(processStorePath);

            Console.WriteLine("I got no path to a persistent store for the process. This process will not be persisted!");

            var files = new System.Collections.Concurrent.ConcurrentDictionary<IImmutableList<string>, ReadOnlyMemory<byte>>(EnumerableExtension.EqualityComparer<IImmutableList<string>>());

            var fileStoreWriter = new DelegatingFileStoreWriter
            (
                SetFileContentDelegate: file => files[file.path] = file.fileContent,
                AppendFileContentDelegate: file => files.AddOrUpdate(
                    file.path, _ => file.fileContent,
                    (_, fileBefore) => CommonConversion.Concat(fileBefore.Span, file.fileContent.Span)),
                DeleteFileDelegate: path => files.Remove(path, out var _)
            );

            var fileStoreReader = new DelegatingFileStoreReader
            (
                ListFilesInDirectoryDelegate: path =>
                    files.Select(file =>
                        !file.Key.Take(path.Count).SequenceEqual(path)
                            ?
                            null
                            :
                            file.Key.Skip(path.Count).ToImmutableList()).WhereNotNull(),
                GetFileContentDelegate: path =>
                {
                    if (!files.TryGetValue(path, out var fileContent))
                        return null;

                    return fileContent;
                }
            );

            return new FileStoreFromWriterAndReader(fileStoreWriter, fileStoreReader);

        }

        var processStoreFileStore = buildProcessStoreFileStore();

        if (copyProcess is not null)
        {
            var copyFiles =
                LoadFilesForRestoreFromPathAndLogToConsole(
                    sourcePath: copyProcess,
                    sourcePassword: null);

            foreach (var file in copyFiles)
                processStoreFileStore.SetFileContent(file.Key.ToImmutableList(), file.Value.ToArray());
        }

        var javaScriptEngineFactory =
            elmEngineType switch
            {
                ElmInteractive.ElmEngineType.JavaScript_Jint => JavaScriptEngineJintOptimizedForElmApps.Create,
                ElmInteractive.ElmEngineType.JavaScript_V8 => new Func<IJavaScriptEngine>(JavaScriptEngineFromJavaScriptEngineSwitcher.ConstructJavaScriptEngine),

                object other => throw new NotImplementedException("Engine type not implemented here: " + other)
            };

        if (deployApp is not null)
        {
            Console.WriteLine("Loading app config to deploy...");

            var appConfigZipArchive =
                BuildConfigurationFromArguments.BuildConfigurationZipArchiveFromPath(
                    sourcePath: deployApp).configZipArchive;

            var appConfigTree =
                PineValueComposition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                    ZipArchive.EntriesFromZipArchive(appConfigZipArchive));

            var appConfigComponent = PineValueComposition.FromTreeWithStringPath(appConfigTree);

            var processStoreWriter =
                new Platform.WebService.ProcessStoreSupportingMigrations.ProcessStoreWriterInFileStore(
                    processStoreFileStore,
                    getTimeForCompositionLogBatch: () => DateTimeOffset.UtcNow,
                    processStoreFileStore);

            processStoreWriter.StoreComponent(appConfigComponent);

            var appConfigValueInFile =
                new Platform.WebService.ProcessStoreSupportingMigrations.ValueInFileStructure
                {
                    HashBase16 = CommonConversion.StringBase16(PineValueHashTree.ComputeHash(appConfigComponent))
                };

            var initElmAppState =
                (deletePreviousProcess || processStorePath == null) && copyProcess is null;

            var compositionLogEvent =
                Platform.WebService.ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent.EventForDeployAppConfig(
                    appConfigValueInFile: appConfigValueInFile,
                    initElmAppState: initElmAppState);

            var testDeployResult = PersistentProcessLiveRepresentation.TestContinueWithCompositionEvent(
                compositionLogEvent: compositionLogEvent,
                fileStoreReader: processStoreFileStore,
                overrideJavaScriptEngineFactory: javaScriptEngineFactory)
            .Extract(error => throw new Exception("Attempt to deploy app config failed: " + error));

            foreach (var (filePath, fileContent) in testDeployResult.projectedFiles)
                processStoreFileStore.SetFileContent(filePath, fileContent);
        }

        var webHostBuilder =
            Microsoft.AspNetCore.WebHost.CreateDefaultBuilder()
                .ConfigureAppConfiguration(builder => builder.AddEnvironmentVariables("APPSETTING_"))
                .UseUrls(adminInterfaceUrls)
                .UseStartup<StartupAdminInterface>()
                .WithSettingPublicWebHostUrls(publicAppUrls)
                .WithJavaScriptEngineFactory(javaScriptEngineFactory)
                .WithProcessStoreFileStore(processStoreFileStore);

        if (adminPassword is not null)
            webHostBuilder = webHostBuilder.WithSettingAdminPassword(adminPassword);

        return webHostBuilder.Build();
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
            PineValueComposition.ToFlatDictionaryWithPathComparer(
                PineValueComposition.SortedTreeFromSetOfBlobsWithCommonFilePath(zipArchiveEntries)
                .EnumerateBlobsTransitive());
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
            PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(restoreFiles);

        var processHistoryComponentHash = PineValueHashTree.ComputeHashNotSorted(processHistoryTree);
        var processHistoryComponentHashBase16 = CommonConversion.StringBase16(processHistoryComponentHash);

        var processHistoryZipArchive = ZipArchive.ZipArchiveFromEntries(restoreFiles);

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

                if (response.StatusCode == System.Net.HttpStatusCode.NotFound)
                    return null;

                if (!response.IsSuccessStatusCode)
                    throw new Exception("Unexpected response status code: " + (int)response.StatusCode + " (" + response.StatusCode + ").");

                return response.Content.ReadAsByteArrayAsync().Result;
            }
        );

        return PersistentProcessLiveRepresentation.GetFilesForRestoreProcess(processHistoryFileStoreRemoteReader);
    }
}