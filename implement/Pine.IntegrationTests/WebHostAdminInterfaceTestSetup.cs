using ElmTime.Platform.WebService;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.IO;
using System;
using System.IO;
using System.Net.Http.Headers;
using System.Text;
using System.Threading;

namespace Pine.IntegrationTests;

public class WebHostAdminInterfaceTestSetup : IDisposable
{
    private static int s_instanceCounter = 0;

    private readonly int _instanceId = Interlocked.Increment(ref s_instanceCounter);

    private string PublicWebHostUrlDefault =>
        "http://localhost:" + (35491 + _instanceId);

    private string AdminWebHostUrlDefault =>
        "http://localhost:" + (19372 + _instanceId);


    private readonly string? _publicWebHostUrlOverride;

    private readonly string? _adminWebHostUrlOverride;

    public string PublicWebHostUrl =>
        _publicWebHostUrlOverride ?? PublicWebHostUrlDefault;

    public string AdminWebHostUrl =>
        _adminWebHostUrlOverride ?? AdminWebHostUrlDefault;

    private readonly string _testDirectory;

    private readonly string? _adminPassword;

    private readonly Func<WebApplicationBuilder, WebApplicationBuilder>? _webAppBuilderMap;

    public string ProcessStoreDirectory => Path.Combine(_testDirectory, "process-store");

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

    private readonly IFileStore _fileStore;

    public WebApplication StartWebHost(
         Func<IFileStore, IFileStore>? processStoreFileStoreMap = null)
    {
        var builder = WebApplication.CreateBuilder();

        builder.WebHost.UseUrls(AdminWebHostUrl);

        builder.Configuration[Configuration.PublicWebHostUrlsSettingKey] = PublicWebHostUrl;
        builder.Configuration[Configuration.AdminPasswordSettingKey] = _adminPassword;

        var effectiveFileStore = processStoreFileStoreMap?.Invoke(_fileStore) ?? _fileStore;

        builder.Services.AddSingleton(new FileStoreForProcessStore(effectiveFileStore));

        if (_webAppBuilderMap is not null)
            builder = _webAppBuilderMap(builder);

        StartupAdminInterface.ConfigureServices(builder.Services);

        var app = builder.Build();

        var startup = new StartupAdminInterface(app.Services.GetRequiredService<ILogger<StartupAdminInterface>>());

        startup.Configure(
            app,
            app.Environment,
            app.Lifetime,
            app.Services.GetRequiredService<Func<DateTimeOffset>>(),
            app.Services.GetRequiredService<FileStoreForProcessStore>());

        app.StartAsync().Wait();

        return app;
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
            webAppBuilderMap:
            builder =>
            {
                builder.Services.AddSingleton(persistentProcessHostDateTime ?? (() => DateTimeOffset.UtcNow));
                return builder;
            },
            persistentProcessHostDateTime: persistentProcessHostDateTime);

    public static WebHostAdminInterfaceTestSetup Setup(
        Func<WebApplicationBuilder, WebApplicationBuilder>? webAppBuilderMap,
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
            webAppBuilderMap: webAppBuilderMap,
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
            Timeout = TimeSpan.FromMinutes(5),
        };
    }

    public System.Net.Http.HttpClient BuildAdminInterfaceHttpClient()
    {
        return new System.Net.Http.HttpClient
        {
            BaseAddress = new Uri(AdminWebHostUrl),
            Timeout = TimeSpan.FromMinutes(5),
        };
    }

    public System.Net.Http.HttpClient SetDefaultRequestHeaderAuthorizeForAdmin(System.Net.Http.HttpClient client)
    {
        if (_adminPassword is null)
            return client;

        client.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue(
            "Basic",
            Convert.ToBase64String(Encoding.UTF8.GetBytes(
                Configuration.BasicAuthenticationForAdmin(_adminPassword))));

        return client;
    }

    public void Dispose()
    {
        Directory.Delete(_testDirectory, true);

        GC.SuppressFinalize(this);
    }

    private WebHostAdminInterfaceTestSetup(
        string testDirectory,
        string? adminPassword,
        IFileStore? fileStore,
        PineValue? deployAppAndInitElmState,
        Func<WebApplicationBuilder, WebApplicationBuilder>? webAppBuilderMap,
        string? adminWebHostUrlOverride,
        string? publicWebHostUrlOverride,
        Func<DateTimeOffset>? persistentProcessHostDateTime = null)
    {
        _testDirectory = testDirectory;

        fileStore ??= DefaultFileStore;

        _adminPassword = adminPassword ?? "notempty";
        _fileStore = fileStore;
        _webAppBuilderMap = webAppBuilderMap;
        _adminWebHostUrlOverride = adminWebHostUrlOverride;
        _publicWebHostUrlOverride = publicWebHostUrlOverride;

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
