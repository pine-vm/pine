using ElmTime.Platform.WebService;
using Microsoft.AspNetCore.Hosting;
using Pine;
using Pine.Core;
using System;
using System.IO;
using System.Net.Http.Headers;
using System.Text;
using System.Threading;

namespace TestElmTime;

public class WebHostAdminInterfaceTestSetup : IDisposable
{
    private static int instanceCounter = 0;

    private readonly int instanceId = Interlocked.Increment(ref instanceCounter);

    private string PublicWebHostUrlDefault => "http://localhost:" + (35491 + instanceId);

    private string AdminWebHostUrlDefault => "http://localhost:" + (19372 + instanceId);


    private readonly string? publicWebHostUrlOverride;

    private readonly string? adminWebHostUrlOverride;

    public string PublicWebHostUrl => publicWebHostUrlOverride ?? PublicWebHostUrlDefault;

    public string AdminWebHostUrl => adminWebHostUrlOverride ?? AdminWebHostUrlDefault;

    private readonly string testDirectory;

    private readonly string? adminPassword;

    private readonly Func<IWebHostBuilder, IWebHostBuilder>? webHostBuilderMap;

    public string ProcessStoreDirectory => Path.Combine(testDirectory, "process-store");

    private IFileStore DefaultFileStore =>
        new FileStoreFromSystemIOFile(
            ProcessStoreDirectory,
            retryOptions:
            new FileStoreFromSystemIOFile.FileStoreRetryOptions
            (
                MaxRetryAttempts: 5,
                InitialRetryDelay: TimeSpan.FromMilliseconds(100),
                MaxRetryDelay: TimeSpan.FromMilliseconds(1000)
            ));

    private readonly IFileStore fileStore;

    public IWebHost StartWebHost(
         Func<IFileStore, IFileStore>? processStoreFileStoreMap = null)
    {
        var webHost =
            (webHostBuilderMap ?? (builder => builder))
            (Microsoft.AspNetCore.WebHost.CreateDefaultBuilder()
            .UseUrls(AdminWebHostUrl)
            .WithSettingPublicWebHostUrls([PublicWebHostUrl])
            .WithSettingAdminPassword(adminPassword)
            .UseStartup<StartupAdminInterface>()
            .WithProcessStoreFileStore(processStoreFileStoreMap?.Invoke(fileStore) ?? fileStore))
            .Build();

        webHost.StartAsync().Wait();

        return webHost;
    }

    public static WebHostAdminInterfaceTestSetup Setup(
        Func<DateTimeOffset>? persistentProcessHostDateTime = null,
        string? adminPassword = null,
        IFileStore? fileStore = null,
        PineValue? deployAppAndInitElmState = null) =>
        Setup(
            adminPassword: adminPassword,
            fileStore: fileStore,
            deployAppAndInitElmState: deployAppAndInitElmState,
            webHostBuilderMap:
            builder => builder.WithSettingDateTimeOffsetDelegate(persistentProcessHostDateTime ?? (() => DateTimeOffset.UtcNow)),
            persistentProcessHostDateTime: persistentProcessHostDateTime);

    public static WebHostAdminInterfaceTestSetup Setup(
        Func<IWebHostBuilder, IWebHostBuilder>? webHostBuilderMap,
        string? adminPassword = null,
        IFileStore? fileStore = null,
        PineValue? deployAppAndInitElmState = null,
        string? adminWebHostUrlOverride = null,
        string? publicWebHostUrlOverride = null,
        Func<DateTimeOffset>? persistentProcessHostDateTime = null)
    {
        var testDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

        var setup = new WebHostAdminInterfaceTestSetup(
            testDirectory,
            adminPassword: adminPassword,
            fileStore: fileStore,
            deployAppAndInitElmState: deployAppAndInitElmState,
            webHostBuilderMap: webHostBuilderMap,
            adminWebHostUrlOverride: adminWebHostUrlOverride,
            publicWebHostUrlOverride: publicWebHostUrlOverride,
            persistentProcessHostDateTime: persistentProcessHostDateTime);

        return setup;
    }

    public System.Net.Http.HttpClient BuildPublicAppHttpClient()
    {
        var handler = new System.Net.Http.HttpClientHandler
        {
            AutomaticDecompression = System.Net.DecompressionMethods.GZip | System.Net.DecompressionMethods.Deflate
        };

        return new System.Net.Http.HttpClient(handler)
        {
            BaseAddress = new Uri(PublicWebHostUrl),
            Timeout = TimeSpan.FromMinutes(3),
        };
    }

    public System.Net.Http.HttpClient BuildAdminInterfaceHttpClient()
    {
        return new System.Net.Http.HttpClient
        {
            BaseAddress = new Uri(AdminWebHostUrl),
            Timeout = TimeSpan.FromMinutes(3),
        };
    }

    public System.Net.Http.HttpClient SetDefaultRequestHeaderAuthorizeForAdmin(System.Net.Http.HttpClient client)
    {
        if (adminPassword == null)
            return client;

        client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue(
            "Basic",
            Convert.ToBase64String(Encoding.UTF8.GetBytes(
                Configuration.BasicAuthenticationForAdmin(adminPassword))));

        return client;
    }

    public void Dispose()
    {
        Directory.Delete(testDirectory, true);

        GC.SuppressFinalize(this);
    }

    private WebHostAdminInterfaceTestSetup(
        string testDirectory,
        string? adminPassword,
        IFileStore? fileStore,
        PineValue? deployAppAndInitElmState,
        Func<IWebHostBuilder, IWebHostBuilder>? webHostBuilderMap,
        string? adminWebHostUrlOverride,
        string? publicWebHostUrlOverride,
        Func<DateTimeOffset>? persistentProcessHostDateTime = null)
    {
        this.testDirectory = testDirectory;

        fileStore ??= DefaultFileStore;

        this.adminPassword = adminPassword ?? "notempty";
        this.fileStore = fileStore;
        this.webHostBuilderMap = webHostBuilderMap;
        this.adminWebHostUrlOverride = adminWebHostUrlOverride;
        this.publicWebHostUrlOverride = publicWebHostUrlOverride;

        if (deployAppAndInitElmState is null)
            return;

        var compositionLogEvent =
            new ElmTime.Platform.WebService.ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent
            {
                DeployAppConfigAndInitElmAppState =
                    new ElmTime.Platform.WebService.ProcessStoreSupportingMigrations.ValueInFileStructure
                    {
                        HashBase16 =
                        Convert.ToHexStringLower(PineValueHashTree.ComputeHash(deployAppAndInitElmState).Span)
                    }
            };

        var processStoreWriter =
            new ElmTime.Platform.WebService.ProcessStoreSupportingMigrations.ProcessStoreWriterInFileStore(
                fileStore,
                getTimeForCompositionLogBatch: persistentProcessHostDateTime ?? (() => DateTimeOffset.UtcNow),
                fileStore,
                skipWritingComponentSecondTime: true);

        processStoreWriter.StoreComponent(deployAppAndInitElmState);

        processStoreWriter.AppendCompositionLogRecord(compositionLogEvent);
    }

    public IFileStoreReader BuildProcessStoreFileStoreReaderInFileDirectory() =>
        new FileStoreFromSystemIOFile(ProcessStoreDirectory);

    public ElmTime.Platform.WebService.ProcessStoreSupportingMigrations.ProcessStoreReaderInFileStore BuildProcessStoreReaderInFileDirectory() =>
        new(BuildProcessStoreFileStoreReaderInFileDirectory());
}
