using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Net.Http.Headers;
using System.Text;
using ElmFullstack.WebHost;
using Microsoft.AspNetCore.Hosting;
using Newtonsoft.Json;
using Pine;

namespace test_elm_fullstack;

public class WebHostAdminInterfaceTestSetup : IDisposable
{
    static string PublicWebHostUrlDefault => "http://localhost:35491";

    static string AdminWebHostUrlDefault => "http://localhost:19372";

    readonly string publicWebHostUrlOverride;

    readonly string adminWebHostUrlOverride;

    public string PublicWebHostUrl => publicWebHostUrlOverride ?? PublicWebHostUrlDefault;

    public string AdminWebHostUrl => adminWebHostUrlOverride ?? AdminWebHostUrlDefault;

    readonly string testDirectory;

    readonly string adminPassword;

    readonly Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap;

    public string ProcessStoreDirectory => Path.Combine(testDirectory, "process-store");

    IFileStore defaultFileStore => new FileStoreFromSystemIOFile(ProcessStoreDirectory);

    readonly IFileStore fileStore;

    public IWebHost StartWebHost(
         Func<IFileStore, IFileStore> processStoreFileStoreMap = null)
    {
        var webHost =
            (webHostBuilderMap ?? (builder => builder))
            (Microsoft.AspNetCore.WebHost.CreateDefaultBuilder()
            .UseUrls(AdminWebHostUrl)
            .WithSettingPublicWebHostUrls(new[] { PublicWebHostUrl })
            .WithSettingAdminPassword(adminPassword)
            .UseStartup<StartupAdminInterface>()
            .WithProcessStoreFileStore(processStoreFileStoreMap?.Invoke(fileStore) ?? fileStore))
            .Build();

        webHost?.StartAsync().Wait();

        return webHost;
    }

    static public WebHostAdminInterfaceTestSetup Setup(
        Func<DateTimeOffset> persistentProcessHostDateTime = null,
        string adminPassword = null,
        IFileStore fileStore = null,
        Composition.Component deployAppConfigAndInitElmState = null) =>
        Setup(
            adminPassword: adminPassword,
            fileStore: fileStore,
            deployAppConfigAndInitElmState: deployAppConfigAndInitElmState,
            webHostBuilderMap: builder => builder.WithSettingDateTimeOffsetDelegate(persistentProcessHostDateTime ?? (() => DateTimeOffset.UtcNow)),
            persistentProcessHostDateTime: persistentProcessHostDateTime);

    static public WebHostAdminInterfaceTestSetup Setup(
        Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap,
        string adminPassword = null,
        IFileStore fileStore = null,
        Composition.Component deployAppConfigAndInitElmState = null,
        string adminWebHostUrlOverride = null,
        string publicWebHostUrlOverride = null,
        Func<DateTimeOffset> persistentProcessHostDateTime = null)
    {
        var testDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

        var setup = new WebHostAdminInterfaceTestSetup(
            testDirectory,
            adminPassword: adminPassword,
            fileStore: fileStore,
            deployAppConfigAndInitElmState: deployAppConfigAndInitElmState,
            webHostBuilderMap: webHostBuilderMap,
            adminWebHostUrlOverride: adminWebHostUrlOverride,
            publicWebHostUrlOverride: publicWebHostUrlOverride,
            persistentProcessHostDateTime: persistentProcessHostDateTime);

        return setup;
    }

    public System.Net.Http.HttpClient BuildPublicAppHttpClient()
    {
        var handler = new System.Net.Http.HttpClientHandler()
        {
            AutomaticDecompression = System.Net.DecompressionMethods.GZip | System.Net.DecompressionMethods.Deflate
        };

        return new System.Net.Http.HttpClient(handler)
        {
            BaseAddress = new Uri(PublicWebHostUrl),
        };
    }

    public System.Net.Http.HttpClient BuildAdminInterfaceHttpClient()
    {
        return new System.Net.Http.HttpClient
        {
            BaseAddress = new Uri(AdminWebHostUrl),
        };
    }

    public System.Net.Http.HttpClient SetDefaultRequestHeaderAuthorizeForAdmin(System.Net.Http.HttpClient client)
    {
        if (adminPassword == null)
            return client;

        client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue(
            "Basic",
            Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                ElmFullstack.WebHost.Configuration.BasicAuthenticationForAdmin(adminPassword))));

        return client;
    }

    public void Dispose()
    {
        Directory.Delete(testDirectory, true);
    }

    WebHostAdminInterfaceTestSetup(
        string testDirectory,
        string adminPassword,
        IFileStore fileStore,
        Composition.Component deployAppConfigAndInitElmState,
        Func<IWebHostBuilder, IWebHostBuilder> webHostBuilderMap,
        string adminWebHostUrlOverride,
        string publicWebHostUrlOverride,
        Func<DateTimeOffset> persistentProcessHostDateTime = null)
    {
        this.testDirectory = testDirectory;

        fileStore ??= defaultFileStore;

        this.adminPassword = adminPassword ?? "notempty";
        this.fileStore = fileStore;
        this.webHostBuilderMap = webHostBuilderMap;
        this.adminWebHostUrlOverride = adminWebHostUrlOverride;
        this.publicWebHostUrlOverride = publicWebHostUrlOverride;

        if (deployAppConfigAndInitElmState != null)
        {
            var compositionLogEvent =
                new ElmFullstack.WebHost.ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent
                {
                    DeployAppConfigAndInitElmAppState =
                        new ElmFullstack.WebHost.ProcessStoreSupportingMigrations.ValueInFileStructure
                        {
                            HashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(deployAppConfigAndInitElmState))
                        }
                };

            var processStoreWriter =
                new ElmFullstack.WebHost.ProcessStoreSupportingMigrations.ProcessStoreWriterInFileStore(
                fileStore,
                getTimeForCompositionLogBatch: persistentProcessHostDateTime ?? (() => DateTimeOffset.UtcNow),
                fileStore);

            processStoreWriter.StoreComponent(deployAppConfigAndInitElmState);

            processStoreWriter.AppendCompositionLogRecord(compositionLogEvent);
        }
    }

    public Pine.IFileStoreReader BuildProcessStoreFileStoreReaderInFileDirectory() =>
            new FileStoreFromSystemIOFile(ProcessStoreDirectory);

    public ElmFullstack.WebHost.ProcessStoreSupportingMigrations.ProcessStoreReaderInFileStore BuildProcessStoreReaderInFileDirectory() =>
        new ElmFullstack.WebHost.ProcessStoreSupportingMigrations.ProcessStoreReaderInFileStore(
            BuildProcessStoreFileStoreReaderInFileDirectory());

    public IEnumerable<ElmFullstack.WebHost.InterfaceToHost.AppEventStructure> EnumerateStoredUpdateElmAppStateForEvents()
    {
        var processStoreReader = BuildProcessStoreReaderInFileDirectory();

        ElmFullstack.WebHost.InterfaceToHost.AppEventStructure eventLogEntry(ElmFullstack.WebHost.ProcessStoreSupportingMigrations.ValueInFileStructure logEntry)
        {
            var component =
                logEntry.LiteralStringUtf8 != null
                ?
                Composition.Component.Blob(Encoding.UTF8.GetBytes(logEntry.LiteralStringUtf8))
                :
                processStoreReader.LoadComponent(logEntry.HashBase16);

            if (component == null)
                throw new Exception("component == null");

            if (component.BlobContent == null)
                throw new Exception("component.BlobContent == null");

            var eventString = Encoding.UTF8.GetString(component.BlobContent.ToArray());

            return JsonConvert.DeserializeObject<ElmFullstack.WebHost.InterfaceToHost.AppEventStructure>(eventString);
        }

        return
            BuildProcessStoreReaderInFileDirectory()
            .EnumerateSerializedCompositionLogRecordsReverse()
            .Select(Encoding.UTF8.GetString)
            .Select(JsonConvert.DeserializeObject<ElmFullstack.WebHost.ProcessStoreSupportingMigrations.CompositionLogRecordInFile>)
            .Select(compositionLogRecord => compositionLogRecord.compositionEvent?.UpdateElmAppStateForEvent)
            .WhereNotNull()
            .Select(updateElmAppStateForEvent => eventLogEntry(updateElmAppStateForEvent));
    }
}
