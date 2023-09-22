using ElmTime.JavaScript;
using ElmTime.Platform.WebService.ProcessStoreSupportingMigrations;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Net.Http.Headers;
using System.Text;

using ApiRouteMethodConfig =
    System.Func<
        Microsoft.AspNetCore.Http.HttpContext,
        ElmTime.Platform.WebService.StartupAdminInterface.PublicHostConfiguration?,
        System.Threading.Tasks.Task>;

namespace ElmTime.Platform.WebService;

public class StartupAdminInterface
{
    public static string PathApiDeployAndInitAppState => "/api/deploy-and-init-app-state";

    public static string PathApiDeployAndMigrateAppState => "/api/deploy-and-migrate-app-state";

    public static string PathApiRevertProcessTo => "/api/revert-process-to";

    public static string PathApiElmAppState => "/api/elm-app-state";

    public static string PathApiGetDeployedAppConfig => "/api/get-deployed-app-config";

    public static string PathApiReplaceProcessHistory => "/api/replace-process-history";

    public static string PathApiTruncateProcessHistory => "/api/truncate-process-history";

    public static string PathApiProcessHistoryFileStore => "/api/process-history-file-store";

    public static string PathApiProcessHistoryFileStoreGetFileContent => PathApiProcessHistoryFileStore + "/get-file-content";

    public static string PathApiProcessHistoryFileStoreListFilesInDirectory => PathApiProcessHistoryFileStore + "/list-files-in-directory";

    public static string PathApiListDatabaseFunctions => "/api/list-database-functions";

    public static string PathApiApplyDatabaseFunction => "/api/apply-database-function";

    public static string PathApiGuiRequest => "/api/gui";

    public static IReadOnlyList<string> WebServiceConfigFilePathDefault => ["web-service.json"];

    public static IReadOnlyList<IReadOnlyList<string>> WebServiceConfigFilePathAlternatives =>
        [
            WebServiceConfigFilePathDefault,

            /*
             * Support smooth migration of projects with backwards compatibility here:
             * Support the name used before 2023-05 as alternative.
             * */
            ["web-server.json"],
            ["elm-fullstack.json"]
        ];

    private readonly ILogger<StartupAdminInterface> logger;

    public StartupAdminInterface(ILogger<StartupAdminInterface> logger)
    {
        this.logger = logger;

        System.Threading.Tasks.TaskScheduler.UnobservedTaskException += (sender, eventArgs) =>
        {
            logger.LogError(eventArgs.Exception, "Unobserved task exception in sender {Sender}", sender?.ToString());
        };

        BeginMakeAdminGuiHtml();
    }

    public static void ConfigureServices(IServiceCollection services)
    {
        var serviceProvider = services.BuildServiceProvider();

        var getDateTimeOffset = serviceProvider.GetService<Func<DateTimeOffset>>();

        if (getDateTimeOffset != null)
            return;

        getDateTimeOffset = () => DateTimeOffset.UtcNow;
        services.AddSingleton(getDateTimeOffset);
    }

    public record PublicHostConfiguration(
        PersistentProcessLiveRepresentation processLiveRepresentation,
        IHost webHost);

    public void Configure(
        IApplicationBuilder app,
        IWebHostEnvironment env,
        IHostApplicationLifetime appLifetime,
        Func<DateTimeOffset> getDateTimeOffset,
        FileStoreForProcessStore processStoreForFileStore)
    {
        if (env.IsDevelopment())
        {
            app.UseDeveloperExceptionPage();
        }

        var configuration = app.ApplicationServices.GetService<IConfiguration>();

        var javaScriptEngineFactory = app.ApplicationServices.GetService<Func<IJavaScriptEngine>>();

        var adminPassword = configuration?.GetValue<string>(Configuration.AdminPasswordSettingKey);

        object avoidConcurrencyLock = new();

        var processStoreFileStore = processStoreForFileStore.fileStore;

        PublicHostConfiguration? publicAppHost = null;

        void stopPublicApp()
        {
            lock (avoidConcurrencyLock)
            {
                if (publicAppHost is null)
                    return;

                logger.LogInformation("Begin to stop the public app.");

                publicAppHost?.webHost?.StopAsync(TimeSpan.FromSeconds(10)).Wait();
                publicAppHost?.webHost?.Dispose();
                publicAppHost?.processLiveRepresentation?.Dispose();
                publicAppHost = null;
            }
        }

        appLifetime.ApplicationStopping.Register(stopPublicApp);

        var processStoreWriter =
            new ProcessStoreWriterInFileStore(
                processStoreFileStore,
                getTimeForCompositionLogBatch: getDateTimeOffset,
                processStoreFileStore);

        void startPublicApp()
        {
            lock (avoidConcurrencyLock)
            {
                stopPublicApp();

                logger.LogInformation("Begin to build the process live representation.");

                var restoreProcessResult =
                    PersistentProcessLiveRepresentation.LoadFromStoreAndRestoreProcess(
                        new ProcessStoreReaderInFileStore(processStoreFileStore),
                        logger: logEntry => logger.LogInformation(logEntry),
                        overrideJavaScriptEngineFactory: javaScriptEngineFactory);

                restoreProcessResult
                    .Unpack(
                    fromErr: err =>
                    {
                        logger.LogError(err);
                        return 0;
                    },
                    fromOk: restoreProcessOk =>
                    {
                        var processLiveRepresentation = restoreProcessOk.process;

                        logger.LogInformation("Completed building the process live representation.");

                        var cyclicReductionStoreLock = new object();
                        DateTimeOffset? cyclicReductionStoreLastTime = null;
                        var cyclicReductionStoreDistanceSeconds = (int)TimeSpan.FromMinutes(10).TotalSeconds;

                        void maintainStoreReductions()
                        {
                            var currentDateTime = getDateTimeOffset();

                            System.Threading.Thread.MemoryBarrier();
                            var cyclicReductionStoreLastAge = currentDateTime - cyclicReductionStoreLastTime;

                            if (!(cyclicReductionStoreLastAge?.TotalSeconds < cyclicReductionStoreDistanceSeconds))
                            {
                                if (System.Threading.Monitor.TryEnter(cyclicReductionStoreLock))
                                {
                                    try
                                    {
                                        var afterLockCyclicReductionStoreLastAge = currentDateTime - cyclicReductionStoreLastTime;

                                        if (afterLockCyclicReductionStoreLastAge?.TotalSeconds < cyclicReductionStoreDistanceSeconds)
                                            return;

                                        lock (avoidConcurrencyLock)
                                        {
                                            var (_, _) = processLiveRepresentation.StoreReductionRecordForCurrentState(processStoreWriter!);
                                        }

                                        cyclicReductionStoreLastTime = currentDateTime;
                                        System.Threading.Thread.MemoryBarrier();
                                    }
                                    finally
                                    {
                                        System.Threading.Monitor.Exit(cyclicReductionStoreLock);
                                    }
                                }
                            }
                        }

                        IHost buildWebHost(
                            ProcessAppConfig processAppConfig,
                            IReadOnlyList<string> publicWebHostUrls)
                        {
                            var appConfigTree =
                                PineValueComposition.ParseAsTreeWithStringPath(processAppConfig.appConfigComponent)
                                .Extract(error => throw new Exception(error.ToString()));

                            var appConfigFilesNamesAndContents =
                                appConfigTree.EnumerateBlobsTransitive();

                            var webServiceConfigFile =
                                appConfigFilesNamesAndContents
                                .Where(filePathAndContent => WebServiceConfigFilePathAlternatives.Any(configFilePath => filePathAndContent.path.SequenceEqual(configFilePath)))
                                .Select(filePathAndContent => filePathAndContent.blobContent)
                                .Cast<ReadOnlyMemory<byte>?>()
                                .FirstOrDefault();

                            var webServiceConfig =
                                webServiceConfigFile == null
                                ?
                                null
                                :
                                System.Text.Json.JsonSerializer.Deserialize<WebServiceConfigJson>(Encoding.UTF8.GetString(webServiceConfigFile.Value.Span));

                            var serverAndElmAppConfig =
                                new ServerAndElmAppConfig(
                                    ServerConfig: webServiceConfig,
                                    ProcessEventInElmApp: serializedEvent =>
                                    {
                                        lock (avoidConcurrencyLock)
                                        {
                                            var elmEventResponse =
                                                processLiveRepresentation.ProcessElmAppEvent(
                                                    processStoreWriter!, serializedEvent);

                                            maintainStoreReductions();

                                            return elmEventResponse;
                                        }
                                    },
                                    SourceComposition: processAppConfig.appConfigComponent,
                                    InitOrMigrateCmds: restoreProcessOk.initOrMigrateCmds
                                );

                            var publicAppState = new PublicAppState(
                                serverAndElmAppConfig: serverAndElmAppConfig,
                                getDateTimeOffset: getDateTimeOffset);

                            var appBuilder = WebApplication.CreateBuilder();

                            var app =
                                publicAppState.Build(
                                    appBuilder,
                                    env,
                                    publicWebHostUrls: publicWebHostUrls);

                            publicAppState.ProcessEventTimeHasArrived();

                            return app;
                        }

                        if (processLiveRepresentation?.lastAppConfig != null)
                        {
                            var publicWebHostUrls = configuration.GetSettingPublicWebHostUrls();

                            var webHost = buildWebHost(
                                processLiveRepresentation.lastAppConfig,
                                publicWebHostUrls: publicWebHostUrls);

                            webHost.StartAsync(appLifetime.ApplicationStopping).Wait();

                            logger.LogInformation("Started the public app at '" + string.Join(",", publicWebHostUrls) + "'.");

                            publicAppHost = new PublicHostConfiguration(
                                processLiveRepresentation: processLiveRepresentation,
                                webHost: webHost);
                        }

                        return 0;
                    });
            }
        }

        startPublicApp();

        app.Run(
            AdminInterfaceRun(
                logger: logger,
                processStoreFileStore: processStoreFileStore,
                processStoreWriter: processStoreWriter,
                adminPassword: adminPassword,
                getPublicAppHost: () => publicAppHost,
                avoidConcurrencyLock: avoidConcurrencyLock,
                startPublicApp: startPublicApp,
                stopPublicApp: stopPublicApp));
    }

    private static RequestDelegate AdminInterfaceRun(
        ILogger logger,
        IFileStore processStoreFileStore,
        IProcessStoreWriter processStoreWriter,
        string? adminPassword,
        Func<PublicHostConfiguration?> getPublicAppHost,
        object avoidConcurrencyLock,
        Action startPublicApp,
        Action stopPublicApp)
    {
        return
        async context =>
        {
            var bodyControlFeature = context.Features.Get<Microsoft.AspNetCore.Http.Features.IHttpBodyControlFeature>();
            if (bodyControlFeature != null)
            {
                bodyControlFeature.AllowSynchronousIO = true;
            }

            {
                context.Request.Headers.TryGetValue("Authorization", out var requestAuthorizationHeaderValue);

                context.Response.Headers.XPoweredBy = "Elm-Time";

                AuthenticationHeaderValue.TryParse(
                    requestAuthorizationHeaderValue.FirstOrDefault(), out var requestAuthorization);

                if (!(0 < adminPassword?.Length))
                {
                    context.Response.StatusCode = 403;
                    await context.Response.WriteAsync("The admin interface is not available because the admin password is not yet configured.");
                    return;
                }

                var buffer = new byte[400];

                var decodedRequestAuthorizationParameter =
                    Convert.TryFromBase64String(requestAuthorization?.Parameter ?? "", buffer, out var bytesWritten) ?
                    Encoding.UTF8.GetString(buffer, 0, bytesWritten) : null;

                var requestAuthorizationPassword =
                    decodedRequestAuthorizationParameter?.Split(':')?.ElementAtOrDefault(1);

                if (!(string.Equals(adminPassword, requestAuthorizationPassword) &&
                    string.Equals("basic", requestAuthorization?.Scheme, StringComparison.OrdinalIgnoreCase)))
                {
                    context.Response.StatusCode = 401;
                    context.Response.Headers.WWWAuthenticate = @"Basic realm=""" + context.Request.Host + @""", charset=""UTF-8""";
                    await context.Response.WriteAsync("Unauthorized");
                    return;
                }
            }

            async System.Threading.Tasks.Task deployElmApp(bool initElmAppState)
            {
                var deploymentZipArchive = await Asp.CopyRequestBody(context.Request);

                {
                    try
                    {
                        var filesFromZipArchive = ZipArchive.EntriesFromZipArchive(deploymentZipArchive).ToImmutableList();

                        if (filesFromZipArchive.Count < 1)
                            throw new Exception("Contains no files.");
                    }
                    catch (Exception e)
                    {
                        context.Response.StatusCode = 400;
                        await context.Response.WriteAsync("Malformed web app config zip-archive:\n" + e);
                        return;
                    }
                }

                var deploymentTree =
                    PineValueComposition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                        ZipArchive.EntriesFromZipArchive(deploymentZipArchive));

                var deploymentPineValue = PineValueComposition.FromTreeWithStringPath(deploymentTree);

                var deploymentHashBase16 = CommonConversion.StringBase16(PineValueHashTree.ComputeHash(deploymentPineValue));

                logger.LogInformation("Got request to deploy app " + deploymentHashBase16);

                processStoreWriter.StoreComponent(deploymentPineValue);

                var deploymentEventValueInFile =
                    new ValueInFileStructure
                    {
                        HashBase16 = deploymentHashBase16
                    };

                var compositionLogEvent =
                    CompositionLogRecordInFile.CompositionEvent.EventForDeployAppConfig(
                        appConfigValueInFile: deploymentEventValueInFile,
                        initElmAppState: initElmAppState);

                await attemptContinueWithCompositionEventAndSendHttpResponse(compositionLogEvent);
            }

            Result<string, IReadOnlyList<StateShim.InterfaceToHost.NamedExposedFunction>> listDatabaseFunctions()
            {
                if (getPublicAppHost() is not { } publicAppHost)
                    return Result<string, IReadOnlyList<StateShim.InterfaceToHost.NamedExposedFunction>>.err(
                        "No application deployed.");

                return publicAppHost.processLiveRepresentation.ListDatabaseFunctions();
            }

            Result<string, AdminInterface.ApplyDatabaseFunctionSuccess> applyDatabaseFunction(
                AdminInterface.ApplyDatabaseFunctionRequest request)
            {
                lock (avoidConcurrencyLock)
                {
                    if (getPublicAppHost() is not { } publicAppHost)
                        return Result<string, AdminInterface.ApplyDatabaseFunctionSuccess>.err(
                            "No application deployed.");

                    return publicAppHost.processLiveRepresentation.ApplyFunctionOnMainBranch(storeWriter: processStoreWriter, request);
                }
            }

            IReadOnlyList<ApiRoute> apiRoutes = null;

            IEnumerable<Gui.EventToElmApp> handleMessageFromGui(
                Gui.MessageToHost messageFromGui) =>
                messageFromGui switch
                {
                    Gui.MessageToHost.ReadAdminInterfaceConfigRequest =>
                    ImmutableList.Create(
                        new Gui.EventToElmApp.ReadAdminInterfaceConfigEvent(
                            new Gui.AdminInterfaceConfig(
                                elmTimeVersionId: Program.AppVersionId,
                                httpRoutes:
                                apiRoutes.Select(apiRoute => new Gui.HttpRoute(
                                    path: apiRoute.path,
                                    methods: apiRoute.methods.Keys.ToImmutableList())).ToImmutableList(),
                                databaseFunctions:
                                listDatabaseFunctions()
                                .Extract(_ => ImmutableList<StateShim.InterfaceToHost.NamedExposedFunction>.Empty))
                            )
                        ),

                    _ =>
                    throw new Exception("Unknown message from GUI: " + System.Text.Json.JsonSerializer.Serialize(messageFromGui))
                };

            apiRoutes = new[]
            {
                    new ApiRoute
                    (
                        path : PathApiGetDeployedAppConfig,
                        methods : ImmutableDictionary<string, ApiRouteMethodConfig>.Empty
                        .Add("get", async (context, publicAppHost) =>
                        {
                            var appConfig = publicAppHost?.processLiveRepresentation?.lastAppConfig.appConfigComponent;

                            if (appConfig == null)
                            {
                                context.Response.StatusCode = 404;
                                await context.Response.WriteAsync("I did not find an app config in the history. Looks like no app was deployed so far.");
                                return;
                            }

                            var appConfigHashBase16 = CommonConversion.StringBase16(PineValueHashTree.ComputeHash(appConfig));

                            var appConfigTreeResult = PineValueComposition.ParseAsTreeWithStringPath(appConfig);

                            var appConfigZipArchive =
                            appConfigTreeResult
                            .Unpack(
                                fromErr: _ => throw   new Exception("Failed to parse as tree with string path"),
                                fromOk: appConfigTree =>
                                ZipArchive.ZipArchiveFromEntries(
                                    PineValueComposition.TreeToFlatDictionaryWithPathComparer(appConfigTree)));

                            context.Response.StatusCode = 200;
                            context.Response.Headers.ContentLength = appConfigZipArchive.LongLength;
                            context.Response.Headers.ContentDisposition = new ContentDispositionHeaderValue("attachment") { FileName = appConfigHashBase16 + ".zip" }.ToString();
                            context.Response.Headers.ContentType = new MediaTypeHeaderValue("application/zip").ToString();

                            await context.Response.Body.WriteAsync(appConfigZipArchive);
                        })
                    ),
                    new ApiRoute
                    (
                        path: PathApiElmAppState,
                        methods: ImmutableDictionary<string, ApiRouteMethodConfig>.Empty
                        .Add("get", async (context, publicAppHost) =>
                        {
                            if (publicAppHost == null)
                            {
                                context.Response.StatusCode = 400;
                                await context.Response.WriteAsync("Not possible because there is no app (state).");
                                return;
                            }

                            var processLiveRepresentation = publicAppHost?.processLiveRepresentation;

                            var components = new List<PineValue>();

                            var storeWriter = new DelegatingProcessStoreWriter
                            (
                                StoreComponentDelegate: components.Add,
                                StoreProvisionalReductionDelegate: _ => { },
                                AppendCompositionLogRecordDelegate: _ => throw new Exception("Unexpected use of interface.")
                            );

                            var reductionRecord =
                                processLiveRepresentation?.StoreReductionRecordForCurrentState(storeWriter).reductionRecord;

                            if (reductionRecord == null)
                            {
                                context.Response.StatusCode = 500;
                                await context.Response.WriteAsync("Not possible because there is no Elm app deployed at the moment.");
                                return;
                            }

                            var elmAppStateReductionHashBase16 = reductionRecord.elmAppState?.HashBase16;

                            var elmAppStateReductionComponent =
                                components.First(c => CommonConversion.StringBase16(PineValueHashTree.ComputeHash(c)) == elmAppStateReductionHashBase16);

                            if (elmAppStateReductionComponent is not PineValue.BlobValue elmAppStateReductionComponentBlob)
                                throw new Exception("elmAppStateReductionComponent is not a blob");

                            var elmAppStateReductionString =
                                Encoding.UTF8.GetString(elmAppStateReductionComponentBlob.Bytes.Span);

                            context.Response.StatusCode = 200;
                            context.Response.ContentType = "application/json";
                            await context.Response.WriteAsync(elmAppStateReductionString);
                        })
                        .Add("post", async (context, publicAppHost) =>
                        {
                            var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();
                            var beginTime = CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow);

                            var elmAppStateToSet = await System.Text.Json.JsonSerializer.DeserializeAsync<System.Text.Json.JsonElement>(context.Request.Body);

                            var setAppStateResult =
                            Result<string, PublicHostConfiguration?>.ok(publicAppHost)
                            .AndThen(maybeNull => Maybe.NothingFromNull(maybeNull).ToResult("Not possible because there is no app (state)."))
                            .AndThen(publicAppHost =>
                            publicAppHost.processLiveRepresentation.SetStateOnMainBranch(
                                storeWriter: processStoreWriter,
                                elmAppStateToSet))
                            .Map(compositionLogEventAndResponse =>
                            new AttemptContinueWithCompositionEventReport
                            (
                                beginTime: beginTime,
                                compositionEvent: compositionLogEventAndResponse.compositionLogEvent,
                                storeReductionReport: null,
                                storeReductionTimeSpentMilli: null,
                                totalTimeSpentMilli: (int)totalStopwatch.ElapsedMilliseconds,
                                testContinueTimeSpentMilli: null,
                                logEntries: null,
                                result: Result<string, string>.ok("Successfully applied this composition event to the process.")
                            ));

                            await writeAsHttpResponse(setAppStateResult);
                        })
                    ),
                    new ApiRoute
                    (
                        path: PathApiDeployAndInitAppState,
                        methods: ImmutableDictionary<string, ApiRouteMethodConfig>.Empty
                        .Add("post", async (_, _) => await deployElmApp(initElmAppState: true))
                    ),
                    new ApiRoute
                    (
                        path: PathApiDeployAndMigrateAppState,
                        methods: ImmutableDictionary<string, ApiRouteMethodConfig>.Empty
                        .Add("post", async (_, _) => await deployElmApp(initElmAppState: false))
                    ),
                    new ApiRoute
                    (
                        path: PathApiListDatabaseFunctions,
                        methods: ImmutableDictionary<string, ApiRouteMethodConfig>.Empty
                        .Add("get", async (context, _) =>
                        {
                            try
                            {
                                var result = listDatabaseFunctions();

                                context.Response.StatusCode = result.Unpack(fromErr: _ => 400, fromOk: _ => 200);
                                await context.Response.WriteAsJsonAsync(result);
                            }
                            catch (Exception ex)
                            {
                                context.Response.StatusCode = 422;
                                await context.Response.WriteAsJsonAsync("Failed with runtime exception: " + ex);
                            }
                        })
                    ),
                    new ApiRoute
                    (
                        path: PathApiApplyDatabaseFunction,
                        methods: ImmutableDictionary<string, ApiRouteMethodConfig>.Empty
                        .Add("post", async (context, _) =>
                        {
                            try
                            {
                                var applyFunctionRequest =
                                    await context.Request.ReadFromJsonAsync<AdminInterface.ApplyDatabaseFunctionRequest>();

                                var result = applyDatabaseFunction(applyFunctionRequest);

                                context.Response.StatusCode = result.Unpack(fromErr: _ => 400, fromOk: _ => 200);
                                await context.Response.WriteAsJsonAsync(result);
                            }
                            catch (Exception ex)
                            {
                                context.Response.StatusCode = 422;
                                await context.Response.WriteAsJsonAsync("Failed with runtime exception: " + ex);
                            }
                        })
                    ),
                    new ApiRoute
                    (
                        path: PathApiGuiRequest,
                        methods: ImmutableDictionary<string, ApiRouteMethodConfig>.Empty
                        .Add("post", async (context, _) =>
                        {
                            try
                            {
                                var guiRequest = await context.Request.ReadFromJsonAsync<Gui.MessageToHost>();

                                var eventsToGui = handleMessageFromGui(guiRequest);

                                context.Response.StatusCode = 200;

                                await context.Response.WriteAsJsonAsync(eventsToGui);
                            }
                            catch (Exception ex)
                            {
                                context.Response.StatusCode = 422;
                                await context.Response.WriteAsJsonAsync("Failed with runtime exception: " + ex);
                            }
                        })
                    ),
                    new ApiRoute
                    (
                        path: PathApiReplaceProcessHistory,
                        methods: ImmutableDictionary<string, ApiRouteMethodConfig>.Empty
                        .Add("post", async (context, _) =>
                        {
                            var historyZipArchive = await Asp.CopyRequestBody(context.Request);

                            var replacementFiles =
                                ZipArchive.EntriesFromZipArchive(historyZipArchive)
                                .Select(filePathAndContent =>
                                    (path: filePathAndContent.name.Split('/', '\\').ToImmutableList()
                                    , filePathAndContent.content))
                                .ToImmutableList();

                            lock (avoidConcurrencyLock)
                            {
                                stopPublicApp();

                                foreach (var filePath in processStoreFileStore.ListFilesInDirectory(ImmutableList<string>.Empty).ToImmutableList())
                                    processStoreFileStore.DeleteFile(filePath);

                                foreach (var (path, content) in replacementFiles)
                                    processStoreFileStore.SetFileContent(path, content.ToArray());

                                startPublicApp();
                            }

                            context.Response.StatusCode = 200;
                            await context.Response.WriteAsync("Successfully replaced the process history.");
                        })
                    ),
                };

            foreach (var apiRoute in apiRoutes)
            {
                if (!context.Request.Path.Equals(new PathString(apiRoute.path)))
                    continue;

                var matchingMethod =
                    apiRoute.methods
                    .FirstOrDefault(m => string.Equals(m.Key, context.Request.Method, StringComparison.InvariantCultureIgnoreCase));

                if (matchingMethod.Value is not { } matchingMethodDelegate)
                {
                    var supportedMethodsNames =
                        apiRoute.methods.Keys.Select(m => m.ToUpperInvariant()).ToList();

                    var guide =
                        HtmlFromLines(
                            "<h2>Method Not Allowed</h2>",
                            "",
                            context.Request.Path.ToString() +
                            " is a valid path, but the method " + context.Request.Method.ToUpperInvariant() +
                            " is not supported here.",
                            "Only following " +
                            (supportedMethodsNames.Count == 1 ? "method is" : "methods are") +
                            " supported here: " + string.Join(", ", supportedMethodsNames));

                    context.Response.StatusCode = 405;
                    await context.Response.WriteAsync(HtmlDocument(guide));
                    return;
                }

                await matchingMethodDelegate.Invoke(context, getPublicAppHost());
                return;
            }

            if (context.Request.Path.StartsWithSegments(new PathString(PathApiRevertProcessTo),
                out var revertToRemainingPath))
            {
                if (!string.Equals(context.Request.Method, "post", StringComparison.InvariantCultureIgnoreCase))
                {
                    context.Response.StatusCode = 405;
                    await context.Response.WriteAsync("Method not supported.");
                    return;
                }

                var processVersionId = revertToRemainingPath.ToString().Trim('/');

                var processVersionCompositionRecord =
                    new ProcessStoreReaderInFileStore(processStoreFileStore)
                    .EnumerateSerializedCompositionLogRecordsReverse()
                    .FirstOrDefault(compositionEntry => CompositionLogRecordInFile.HashBase16FromCompositionRecord(compositionEntry) == processVersionId);

                if (processVersionCompositionRecord == null)
                {
                    context.Response.StatusCode = 404;
                    await context.Response.WriteAsync("Did not find process version '" + processVersionId + "'.");
                    return;
                }

                await attemptContinueWithCompositionEventAndSendHttpResponse(new CompositionLogRecordInFile.CompositionEvent
                {
                    RevertProcessTo = new ValueInFileStructure { HashBase16 = processVersionId },
                });
                return;
            }

            TruncateProcessHistoryReport truncateProcessHistory(TimeSpan productionBlockDurationLimit)
            {
                var beginTime = CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow);

                var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

                const int numbersOfThreadsToDeleteFiles = 4;

                var filePathsInProcessStorePartitions =
                    processStoreFileStore.ListFiles()
                    .Select((s, i) => (s, i))
                    .GroupBy(x => x.i % numbersOfThreadsToDeleteFiles)
                    .Select(g => g.Select(x => x.s).ToImmutableList())
                    .ToImmutableList();

                logger.LogInformation(
                    message: nameof(truncateProcessHistory) + ": Found {filePathCount} file paths to delete",
                    filePathsInProcessStorePartitions.Sum(partition => partition.Count));

                lock (avoidConcurrencyLock)
                {
                    var lockStopwatch = System.Diagnostics.Stopwatch.StartNew();

                    var storeReductionStopwatch = System.Diagnostics.Stopwatch.StartNew();

                    var storeReductionReport =
                        getPublicAppHost()
                        ?.processLiveRepresentation?.StoreReductionRecordForCurrentState(processStoreWriter).report;

                    storeReductionStopwatch.Stop();

                    logger.LogInformation(
                        message: nameof(truncateProcessHistory) + ": Stored reduction in {storeReductionDurationMs} ms",
                        storeReductionStopwatch.ElapsedMilliseconds);

                    var getFilesForRestoreStopwatch = System.Diagnostics.Stopwatch.StartNew();

                    var filesForRestore =
                        PersistentProcessLiveRepresentation.GetFilesForRestoreProcess(
                            processStoreFileStore).files
                        .Select(filePathAndContent => filePathAndContent.Key)
                        .ToImmutableHashSet(EnumerableExtension.EqualityComparer<IReadOnlyList<string>>());

                    getFilesForRestoreStopwatch.Stop();

                    var deleteFilesStopwatch = System.Diagnostics.Stopwatch.StartNew();

                    var partitionsTasks =
                        filePathsInProcessStorePartitions
                        .Select(partitionFilePaths => System.Threading.Tasks.Task.Run(() =>
                        {
                            int partitionDeletedFilesCount = 0;

                            foreach (var filePath in partitionFilePaths)
                            {
                                if (filesForRestore.Contains(filePath))
                                    continue;

                                if (productionBlockDurationLimit < lockStopwatch.Elapsed)
                                    break;

                                processStoreFileStore.DeleteFile(filePath);
                                ++partitionDeletedFilesCount;
                            }

                            return partitionDeletedFilesCount;
                        }))
                        .ToImmutableList();

                    var totalDeletedFilesCount = partitionsTasks.Sum(task => task.Result);

                    deleteFilesStopwatch.Stop();

                    logger.LogInformation(
                        message: nameof(truncateProcessHistory) + ": Deleted {totalDeletedFilesCount} files in {storeReductionDurationMs} ms",
                        totalDeletedFilesCount,
                        deleteFilesStopwatch.ElapsedMilliseconds);

                    return new TruncateProcessHistoryReport
                    (
                        beginTime: beginTime,
                        filesForRestoreCount: filesForRestore.Count,
                        discoveredFilesCount: filePathsInProcessStorePartitions.Sum(partition => partition.Count),
                        deletedFilesCount: totalDeletedFilesCount,
                        storeReductionTimeSpentMilli: (int)storeReductionStopwatch.ElapsedMilliseconds,
                        storeReductionReport: storeReductionReport,
                        getFilesForRestoreTimeSpentMilli: (int)getFilesForRestoreStopwatch.ElapsedMilliseconds,
                        deleteFilesTimeSpentMilli: (int)deleteFilesStopwatch.ElapsedMilliseconds,
                        lockedTimeSpentMilli: (int)lockStopwatch.ElapsedMilliseconds,
                        totalTimeSpentMilli: (int)totalStopwatch.ElapsedMilliseconds
                    );
                }
            }

            if (context.Request.Path.Equals(new PathString(PathApiTruncateProcessHistory)))
            {
                var truncateResult = truncateProcessHistory(productionBlockDurationLimit: TimeSpan.FromMinutes(1));

                context.Response.StatusCode = 200;
                context.Response.ContentType = "application/json";
                await context.Response.WriteAsync(System.Text.Json.JsonSerializer.Serialize(truncateResult));
                return;
            }

            {
                if (context.Request.Path.StartsWithSegments(
                    new PathString(PathApiProcessHistoryFileStoreGetFileContent), out var remainingPathString))
                {
                    if (!string.Equals(context.Request.Method, "get", StringComparison.InvariantCultureIgnoreCase))
                    {
                        context.Response.StatusCode = 405;
                        await context.Response.WriteAsync("Method not supported.");
                        return;
                    }

                    var filePathInStore =
                        remainingPathString.ToString().Trim('/').Split('/').ToImmutableList();

                    var fileContent = processStoreFileStore.GetFileContent(filePathInStore);

                    if (fileContent == null)
                    {
                        context.Response.StatusCode = 404;
                        await context.Response.WriteAsync("No file at '" + string.Join("/", filePathInStore) + "'.");
                        return;
                    }

                    context.Response.StatusCode = 200;
                    context.Response.ContentType = "application/octet-stream";
                    await context.Response.Body.WriteAsync(fileContent.Value);
                    return;
                }
            }

            {
                if (context.Request.Path.StartsWithSegments(
                    new PathString(PathApiProcessHistoryFileStoreListFilesInDirectory), out var remainingPathString))
                {
                    if (!string.Equals(context.Request.Method, "get", StringComparison.InvariantCultureIgnoreCase))
                    {
                        context.Response.StatusCode = 405;
                        await context.Response.WriteAsync("Method not supported.");
                        return;
                    }

                    var filePathInStore =
                        remainingPathString.ToString().Trim('/').Split('/').ToImmutableList();

                    var filesPaths = processStoreFileStore.ListFilesInDirectory(filePathInStore);

                    var filesPathsList =
                        string.Join('\n', filesPaths.Select(path => string.Join('/', path)));

                    context.Response.StatusCode = 200;
                    context.Response.ContentType = "application/octet-stream";
                    await context.Response.Body.WriteAsync(Encoding.UTF8.GetBytes(filesPathsList));
                    return;
                }
            }

            (int statusCode, AttemptContinueWithCompositionEventReport responseReport) attemptContinueWithCompositionEvent(
                CompositionLogRecordInFile.CompositionEvent compositionLogEvent)
            {
                lock (avoidConcurrencyLock)
                {
                    var storeReductionStopwatch = System.Diagnostics.Stopwatch.StartNew();

                    var storeReductionReport =
                        getPublicAppHost()
                        ?.processLiveRepresentation?.StoreReductionRecordForCurrentState(processStoreWriter).report;

                    storeReductionStopwatch.Stop();

                    var (statusCode, report) =
                        AttemptContinueWithCompositionEventAndCommit(
                            compositionLogEvent,
                            processStoreFileStore,
                            testContinueLogger: logEntry => logger.LogInformation(logEntry));

                    report = report with
                    {
                        storeReductionTimeSpentMilli = (int)storeReductionStopwatch.ElapsedMilliseconds,
                        storeReductionReport = storeReductionReport
                    };

                    startPublicApp();

                    return (statusCode, report);
                }
            }

            async System.Threading.Tasks.Task writeAsHttpResponse<ErrT, OkT>(Result<ErrT, OkT> result)
            {
                static string reportAsString(object report)
                {
                    return report switch
                    {
                        null => "",

                        string alreadyString => alreadyString,

                        _ => System.Text.Json.JsonSerializer.Serialize(report)
                    };
                }

                var (statusCode, responseBodyString) = result.Unpack(err => (400, reportAsString(err)), ok => (200, reportAsString(ok)));

                context.Response.StatusCode = statusCode;
                await context.Response.WriteAsync(responseBodyString);
            }

            async System.Threading.Tasks.Task attemptContinueWithCompositionEventAndSendHttpResponse(
                CompositionLogRecordInFile.CompositionEvent compositionLogEvent,
                ILogger? logger = null)
            {
                logger?.LogInformation(
                    "Begin attempt to continue with composition event: " +
                    System.Text.Json.JsonSerializer.Serialize(compositionLogEvent));

                var (statusCode, attemptReport) = attemptContinueWithCompositionEvent(compositionLogEvent);

                var responseBodyString =
                System.Text.Json.JsonSerializer.Serialize(
                    attemptReport,
                    options: new System.Text.Json.JsonSerializerOptions
                    {
                        WriteIndented = true
                    });

                context.Response.StatusCode = statusCode;
                await context.Response.WriteAsync(responseBodyString);
            }

            if (context.Request.Path.Equals(PathString.Empty) || context.Request.Path.Equals(new PathString("/")))
            {
                var html = ComposeAdminGuiHtml();

                context.Response.StatusCode = 200;
                await context.Response.WriteAsync(html);
                return;
            }

            context.Response.StatusCode = 404;
            await context.Response.WriteAsync("Not Found");
        };
    }

    private static void BeginMakeAdminGuiHtml() =>
        System.Threading.Tasks.Task.Run(BuildAdminGuiInteractiveHtml);

    private static string ComposeAdminGuiHtml() =>
        BuildAdminGuiInteractiveHtml()
        .Unpack(
            fromErr: ComposeAdminGuiStaticHtml,
            fromOk: html => html);

    private static Result<string, string> BuildAdminGuiInteractiveHtml() =>
        Gui.MakeGuiCache.MakeGuiHtmlTask.Value.Result;

    private static string ComposeAdminGuiStaticHtml(string buildInteractiveGuiError)
    {
        var describeErrorElement =
            "<p " + HtmlAttributeCssStyle(
                ("color", "red"),
                ("white-space", "pre-wrap"),
                ("font-family", "monospace")) +
            ">Failed to build the interactive GUI:\n" + buildInteractiveGuiError + "</p>";

        return
            HtmlDocument(
                HtmlFromLines(
                    "Welcome to the Elm-Time admin interface version " + Program.AppVersionId + ".",
                    describeErrorElement));
    }

    public static string HtmlAttributeCssStyle(params (string property, string value)[] styles) =>
        "style=\"" +
        string.Join(" ", styles.Select(style => style.property + ": " + style.value + ";"))
        + "\"";

    private static string LinkHtmlElementFromUrl(string url) =>
        "<a href='" + url + "'>" + url + "</a>";

    private static string HtmlFromLines(params string[] lines) =>
        string.Join("<br>\n", lines);

    private record ApiRoute(
        string path,
        ImmutableDictionary<string, ApiRouteMethodConfig> methods);

    public static string HtmlDocument(string body) =>
        string.Join("\n", "<html>", "<body>", body, "</body>", "</html>");

    public static (int statusCode, AttemptContinueWithCompositionEventReport responseReport) AttemptContinueWithCompositionEventAndCommit(
        CompositionLogRecordInFile.CompositionEvent compositionLogEvent,
        IFileStore processStoreFileStore,
        Action<string>? testContinueLogger = null)
    {
        var beginTime = CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow);

        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var logEntries = new List<StringMessageWithTimeMilli>();

        var testContinueResult = PersistentProcessLiveRepresentation.TestContinueWithCompositionEvent(
            compositionLogEvent: compositionLogEvent,
            fileStoreReader: processStoreFileStore,
            logger: message =>
            {
                logEntries.Add(new StringMessageWithTimeMilli(message, totalStopwatch.ElapsedMilliseconds));
                testContinueLogger?.Invoke(message);
            });

        var testContinueTimeSpentMilli = totalStopwatch.ElapsedMilliseconds;

        return
            testContinueResult
            .Unpack(
                fromErr: error =>
                (statusCode: 400, new AttemptContinueWithCompositionEventReport
                (
                    beginTime: beginTime,
                    compositionEvent: compositionLogEvent,
                    storeReductionReport: null,
                    storeReductionTimeSpentMilli: null,
                    testContinueTimeSpentMilli: (int)testContinueTimeSpentMilli,
                    totalTimeSpentMilli: (int)totalStopwatch.ElapsedMilliseconds,
                    logEntries: logEntries,
                    result: Result<string, string>.err(error)
                )),
                fromOk: testContinueOk =>
                {
                    foreach (var (filePath, fileContent) in testContinueOk.projectedFiles)
                        processStoreFileStore.SetFileContent(filePath, fileContent);

                    return (statusCode: 200, new AttemptContinueWithCompositionEventReport
                    (
                        beginTime: beginTime,
                        compositionEvent: compositionLogEvent,
                        storeReductionReport: null,
                        storeReductionTimeSpentMilli: null,
                        testContinueTimeSpentMilli: (int)testContinueTimeSpentMilli,
                        totalTimeSpentMilli: (int)totalStopwatch.ElapsedMilliseconds,
                        logEntries: logEntries,
                        result: Result<string, string>.ok("Successfully applied this composition event to the process.")
                    ));
                });
    }
}

public record AttemptContinueWithCompositionEventReport(
    string beginTime,
    CompositionLogRecordInFile.CompositionEvent compositionEvent,
    StoreProvisionalReductionReport? storeReductionReport,
    int? storeReductionTimeSpentMilli,
    int? testContinueTimeSpentMilli,
    int totalTimeSpentMilli,
    IReadOnlyList<StringMessageWithTimeMilli>? logEntries,
    Result<string, string> result);

public record StringMessageWithTimeMilli(
    string message,
    long timeMilli);

public record TruncateProcessHistoryReport(
    string beginTime,
    int filesForRestoreCount,
    int discoveredFilesCount,
    int deletedFilesCount,
    int lockedTimeSpentMilli,
    int totalTimeSpentMilli,
    int storeReductionTimeSpentMilli,
    StoreProvisionalReductionReport? storeReductionReport,
    int getFilesForRestoreTimeSpentMilli,
    int deleteFilesTimeSpentMilli);
