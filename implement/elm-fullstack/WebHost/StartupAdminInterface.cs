using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Net.Http.Headers;
using System.Text;
using ElmFullstack.WebHost.ProcessStoreSupportingMigrations;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Pine;

namespace ElmFullstack.WebHost
{
    public class StartupAdminInterface
    {
        static public string PathApiDeployAppConfigAndInitElmAppState => "/api/deploy-app-config-and-init-elm-app-state";

        static public string PathApiDeployAppConfigAndMigrateElmAppState => "/api/deploy-app-config-and-migrate-elm-app-state";

        static public string PathApiRevertProcessTo => "/api/revert-process-to";

        static public string PathApiElmAppState => "/api/elm-app-state";

        static public string PathApiGetDeployedAppConfig => "/api/get-deployed-app-config";

        static public string PathApiReplaceProcessHistory => "/api/replace-process-history";

        static public string PathApiTruncateProcessHistory => "/api/truncate-process-history";

        static public string PathApiProcessHistoryFileStore => "/api/process-history-file-store";

        static public string PathApiProcessHistoryFileStoreGetFileContent => PathApiProcessHistoryFileStore + "/get-file-content";

        static public string JsonFileName => "elm-fullstack.json";

        static public IImmutableList<string> JsonFilePath => ImmutableList.Create(JsonFileName);

        private readonly ILogger<StartupAdminInterface> logger;

        public StartupAdminInterface(ILogger<StartupAdminInterface> logger)
        {
            this.logger = logger;
        }

        public void ConfigureServices(IServiceCollection services)
        {
            var serviceProvider = services.BuildServiceProvider();

            var getDateTimeOffset = serviceProvider.GetService<Func<DateTimeOffset>>();

            if (getDateTimeOffset == null)
            {
                getDateTimeOffset = () => DateTimeOffset.UtcNow;
                services.AddSingleton<Func<DateTimeOffset>>(getDateTimeOffset);
            }
        }

        class PublicHostConfiguration
        {
            public PersistentProcess.PersistentProcessVolatileRepresentation processVolatileRepresentation;

            public IWebHost webHost;
        }

        public void Configure(
            IApplicationBuilder app,
            IWebHostEnvironment env,
            IHostApplicationLifetime appLifetime,
            Func<DateTimeOffset> getDateTimeOffset)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }

            var configuration = app.ApplicationServices.GetService<IConfiguration>();

            var adminPassword = configuration.GetValue<string>(Configuration.AdminPasswordSettingKey);

            var publicWebHostUrls =
                configuration.GetValue<string>(Configuration.PublicWebHostUrlsSettingKey).Split(new[] { ',', ';' });

            var processStoreFileStore = app.ApplicationServices.GetService<FileStoreForProcessStore>().fileStore;

            object avoidConcurrencyLock = new object();

            PublicHostConfiguration publicAppHost = null;

            void stopPublicApp()
            {
                lock (avoidConcurrencyLock)
                {
                    if (publicAppHost != null)
                    {
                        logger.LogInformation("Begin to stop the public app.");

                        publicAppHost?.webHost?.StopAsync(TimeSpan.FromSeconds(10)).Wait();
                        publicAppHost?.webHost?.Dispose();
                        publicAppHost?.processVolatileRepresentation?.Dispose();
                        publicAppHost = null;
                    }
                }
            }

            appLifetime.ApplicationStopping.Register(() =>
            {
                stopPublicApp();
            });

            var processStoreWriter =
                new ProcessStoreSupportingMigrations.ProcessStoreWriterInFileStore(processStoreFileStore);

            void startPublicApp()
            {
                lock (avoidConcurrencyLock)
                {
                    stopPublicApp();

                    var newPublicAppConfig = new PublicHostConfiguration { };

                    logger.LogInformation("Begin to build the process volatile representation.");

                    var processVolatileRepresentation =
                        PersistentProcess.PersistentProcessVolatileRepresentation.LoadFromStoreAndRestoreProcess(
                            new ProcessStoreSupportingMigrations.ProcessStoreReaderInFileStore(processStoreFileStore),
                            logger: logEntry => logger.LogInformation(logEntry));

                    logger.LogInformation("Completed building the process volatile representation.");

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
                                        var (reductionRecord, _) = processVolatileRepresentation.StoreReductionRecordForCurrentState(processStoreWriter);
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

                    IWebHost buildWebHost()
                    {
                        var appConfigTree = Composition.ParseAsTreeWithStringPath(
                            processVolatileRepresentation.lastAppConfig.Value.appConfigComponent).Ok;

                        var appConfigFilesNamesAndContents =
                            appConfigTree.EnumerateBlobsTransitive();

                        var webAppConfigurationFile =
                            appConfigFilesNamesAndContents
                            .FirstOrDefault(filePathAndContent => filePathAndContent.path.SequenceEqual(JsonFilePath))
                            .blobContent;

                        var webAppConfiguration =
                            webAppConfigurationFile == null
                            ?
                            null
                            :
                            Newtonsoft.Json.JsonConvert.DeserializeObject<WebAppConfigurationJsonStructure>(Encoding.UTF8.GetString(webAppConfigurationFile.ToArray()));

                        return
                            Microsoft.AspNetCore.WebHost.CreateDefaultBuilder()
                            .ConfigureLogging((hostingContext, logging) =>
                            {
                                logging.AddConfiguration(hostingContext.Configuration.GetSection("Logging"));
                                logging.AddConsole();
                                logging.AddDebug();
                            })
                            .ConfigureKestrel(kestrelOptions =>
                            {
                                kestrelOptions.ConfigureHttpsDefaults(httpsOptions =>
                                {
                                    httpsOptions.ServerCertificateSelector = (c, s) => FluffySpoon.AspNet.LetsEncrypt.LetsEncryptRenewalService.Certificate;
                                });
                            })
                            .UseUrls(publicWebHostUrls)
                            .UseStartup<StartupPublicApp>()
                            .WithSettingDateTimeOffsetDelegate(getDateTimeOffset)
                            .ConfigureServices(services =>
                            {
                                services.AddSingleton<WebAppAndElmAppConfig>(
                                    new WebAppAndElmAppConfig
                                    {
                                        WebAppConfiguration = webAppConfiguration,
                                        ProcessEventInElmApp = serializedEvent =>
                                        {
                                            lock (avoidConcurrencyLock)
                                            {
                                                var elmEventResponse =
                                                    processVolatileRepresentation.ProcessElmAppEvent(
                                                        processStoreWriter, serializedEvent);

                                                maintainStoreReductions();

                                                return elmEventResponse;
                                            }
                                        },
                                        SourceComposition = processVolatileRepresentation.lastAppConfig.Value.appConfigComponent,
                                    });
                            })
                            .Build();
                    }

                    var webHost =
                        processVolatileRepresentation?.lastAppConfig?.appConfigComponent == null
                        ?
                        null
                        :
                        buildWebHost();

                    newPublicAppConfig.processVolatileRepresentation = processVolatileRepresentation;
                    newPublicAppConfig.webHost = webHost;

                    webHost?.StartAsync(appLifetime.ApplicationStopping).Wait();

                    logger.LogInformation("Started the public app at '" + string.Join(",", publicWebHostUrls) + "'.");

                    publicAppHost = newPublicAppConfig;
                }
            }

            startPublicApp();

            app.Run(async (context) =>
                {
                    var syncIOFeature = context.Features.Get<Microsoft.AspNetCore.Http.Features.IHttpBodyControlFeature>();
                    if (syncIOFeature != null)
                    {
                        syncIOFeature.AllowSynchronousIO = true;
                    }

                    {
                        context.Request.Headers.TryGetValue("Authorization", out var requestAuthorizationHeaderValue);

                        context.Response.Headers.Add("X-Powered-By", "Elm Fullstack " + elm_fullstack.Program.AppVersionId);

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
                            context.Response.Headers.Add(
                                "WWW-Authenticate",
                                @"Basic realm=""" + context.Request.Host + @""", charset=""UTF-8""");
                            await context.Response.WriteAsync("Unauthorized");
                            return;
                        }
                    }

                    async System.Threading.Tasks.Task deployElmApp(bool initElmAppState)
                    {
                        var memoryStream = new MemoryStream();
                        context.Request.Body.CopyTo(memoryStream);

                        var webAppConfigZipArchive = memoryStream.ToArray();

                        {
                            try
                            {
                                var filesFromZipArchive = ZipArchive.EntriesFromZipArchive(webAppConfigZipArchive).ToImmutableList();

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

                        var appConfigTree =
                            Composition.SortedTreeFromSetOfBlobsWithCommonFilePath(
                                ZipArchive.EntriesFromZipArchive(webAppConfigZipArchive));

                        var appConfigComponent = Composition.FromTreeWithStringPath(appConfigTree);

                        processStoreWriter.StoreComponent(appConfigComponent);

                        var appConfigValueInFile =
                            new ValueInFileStructure
                            {
                                HashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(appConfigComponent))
                            };

                        var compositionLogEvent =
                            CompositionLogRecordInFile.CompositionEvent.EventForDeployAppConfig(
                                appConfigValueInFile: appConfigValueInFile,
                                initElmAppState: initElmAppState);

                        await attemptContinueWithCompositionEventAndSendHttpResponse(compositionLogEvent);
                    }

                    var apiRoutes = new[]
                    {
                        new ApiRoute
                        {
                            path = PathApiGetDeployedAppConfig,
                            methods = ImmutableDictionary<string, Func<HttpContext, PublicHostConfiguration, System.Threading.Tasks.Task>>.Empty
                            .Add("get", async (context, publicAppHost) =>
                            {
                                var appConfig = publicAppHost?.processVolatileRepresentation?.lastAppConfig?.appConfigComponent;

                                if (appConfig == null)
                                {
                                    context.Response.StatusCode = 404;
                                    await context.Response.WriteAsync("I did not find an app config in the history. Looks like no app was deployed so far.");
                                    return;
                                }

                                var appConfigHashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(appConfig));

                                var appConfigTree = Composition.ParseAsTreeWithStringPath(appConfig).Ok;

                                var appConfigZipArchive =
                                    ZipArchive.ZipArchiveFromEntries(
                                        Composition.TreeToFlatDictionaryWithPathComparer(appConfigTree));

                                context.Response.StatusCode = 200;
                                context.Response.Headers.ContentLength = appConfigZipArchive.LongLength;
                                context.Response.Headers.Add("Content-Disposition", new ContentDispositionHeaderValue("attachment") { FileName = appConfigHashBase16 + ".zip" }.ToString());
                                context.Response.Headers.Add("Content-Type", new MediaTypeHeaderValue("application/zip").ToString());

                                await context.Response.Body.WriteAsync(appConfigZipArchive);
                            }),
                        },
                        new ApiRoute
                        {
                            path = PathApiElmAppState,
                            methods = ImmutableDictionary<string, Func<HttpContext, PublicHostConfiguration, System.Threading.Tasks.Task>>.Empty
                            .Add("get", async (context, publicAppHost) =>
                            {
                                if (publicAppHost == null)
                                {
                                    context.Response.StatusCode = 400;
                                    await context.Response.WriteAsync("Not possible because there is no app (state).");
                                    return;
                                }

                                var processVolatileRepresentation = publicAppHost?.processVolatileRepresentation;

                                var components = new List<Composition.Component>();

                                var storeWriter = new DelegatingProcessStoreWriter
                                {
                                    StoreComponentDelegate = components.Add,
                                    StoreProvisionalReductionDelegate = _ => { },
                                    SetCompositionLogHeadRecordDelegate = _ => throw new Exception("Unexpected use of interface."),
                                };

                                var reductionRecord =
                                    processVolatileRepresentation?.StoreReductionRecordForCurrentState(storeWriter).reductionRecord;

                                if (reductionRecord == null)
                                {
                                    context.Response.StatusCode = 500;
                                    await context.Response.WriteAsync("Not possible because there is no Elm app deployed at the moment.");
                                    return;
                                }

                                var elmAppStateReductionHashBase16 = reductionRecord.elmAppState?.HashBase16;

                                var elmAppStateReductionComponent =
                                    components.First(c => CommonConversion.StringBase16FromByteArray(Composition.GetHash(c)) == elmAppStateReductionHashBase16);

                                var elmAppStateReductionString =
                                    Encoding.UTF8.GetString(elmAppStateReductionComponent.BlobContent.ToArray());

                                context.Response.StatusCode = 200;
                                context.Response.ContentType = "application/json";
                                await context.Response.WriteAsync(elmAppStateReductionString);
                            })
                            .Add("post", async (context, publicAppHost) =>
                            {
                                if (publicAppHost == null)
                                {
                                    context.Response.StatusCode = 400;
                                    await context.Response.WriteAsync("Not possible because there is no app (state).");
                                    return;
                                }

                                var elmAppStateToSet = new StreamReader(context.Request.Body, System.Text.Encoding.UTF8).ReadToEndAsync().Result;

                                var elmAppStateComponent = Composition.Component.Blob(Encoding.UTF8.GetBytes(elmAppStateToSet));

                                var appConfigValueInFile =
                                    new ValueInFileStructure
                                    {
                                        HashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(elmAppStateComponent))
                                    };

                                processStoreWriter.StoreComponent(elmAppStateComponent);

                                await attemptContinueWithCompositionEventAndSendHttpResponse(
                                    new CompositionLogRecordInFile.CompositionEvent
                                    {
                                        SetElmAppState = appConfigValueInFile
                                    });
                            }),
                        },
                        new ApiRoute
                        {
                            path = PathApiReplaceProcessHistory,
                            methods = ImmutableDictionary<string, Func<HttpContext, PublicHostConfiguration, System.Threading.Tasks.Task>>.Empty
                            .Add("post", async (context, publicAppHost) =>
                            {
                                var memoryStream = new MemoryStream();
                                context.Request.Body.CopyTo(memoryStream);

                                var webAppConfigZipArchive = memoryStream.ToArray();

                                var replacementFiles =
                                    ZipArchive.EntriesFromZipArchive(webAppConfigZipArchive)
                                    .Select(filePathAndContent =>
                                        (path: filePathAndContent.name.Split(new[] { '/', '\\' }).ToImmutableList()
                                        , filePathAndContent.content))
                                    .ToImmutableList();

                                lock (avoidConcurrencyLock)
                                {
                                    stopPublicApp();

                                    foreach (var filePath in processStoreFileStore.ListFilesInDirectory(ImmutableList<string>.Empty).ToImmutableList())
                                        processStoreFileStore.DeleteFile(filePath);

                                    foreach (var replacementFile in replacementFiles)
                                        processStoreFileStore.SetFileContent(replacementFile.path, replacementFile.content);

                                    startPublicApp();
                                }

                                context.Response.StatusCode = 200;
                                await context.Response.WriteAsync("Successfully replaced the process history.");
                            }),
                        },
                        new ApiRoute
                        {
                            path = PathApiDeployAppConfigAndInitElmAppState,
                            methods = ImmutableDictionary<string, Func<HttpContext, PublicHostConfiguration, System.Threading.Tasks.Task>>.Empty
                            .Add("post", async (context, publicAppHost) => await deployElmApp(initElmAppState: true)),
                        },
                        new ApiRoute
                        {
                            path = PathApiDeployAppConfigAndMigrateElmAppState,
                            methods = ImmutableDictionary<string, Func<HttpContext, PublicHostConfiguration, System.Threading.Tasks.Task>>.Empty
                            .Add("post", async (context, publicAppHost) => await deployElmApp(initElmAppState: false)),
                        },
                    };

                    foreach (var apiRoute in apiRoutes)
                    {
                        if (!context.Request.Path.Equals(new PathString(apiRoute.path)))
                            continue;

                        var matchingMethod =
                            apiRoute.methods
                            ?.FirstOrDefault(m => m.Key.ToUpperInvariant() == context.Request.Method.ToUpperInvariant());

                        if (matchingMethod?.Value == null)
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
                                    " supported here: " + string.Join(", ", supportedMethodsNames),
                                    "", "",
                                    ApiGuide);

                            context.Response.StatusCode = 405;
                            await context.Response.WriteAsync(HtmlDocument(guide));
                            return;
                        }

                        matchingMethod?.Value?.Invoke(context, publicAppHost);
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

                        var processVersionComponent =
                            new ProcessStoreReaderInFileStore(processStoreFileStore).LoadComponent(processVersionId);

                        if (processVersionComponent == null)
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

                        var numbersOfThreadsToDeleteFiles = 4;

                        var filePathsInProcessStorePartitions =
                            processStoreFileStore.ListFilesInDirectory(ImmutableList<string>.Empty)
                            .Select((s, i) => (s, i))
                            .GroupBy(x => x.i % numbersOfThreadsToDeleteFiles)
                            .Select(g => g.Select(x => x.s).ToImmutableList())
                            .ToImmutableList();

                        lock (avoidConcurrencyLock)
                        {
                            var lockStopwatch = System.Diagnostics.Stopwatch.StartNew();

                            var storeReductionStopwatch = System.Diagnostics.Stopwatch.StartNew();

                            var storeReductionReport =
                                publicAppHost?.processVolatileRepresentation?.StoreReductionRecordForCurrentState(processStoreWriter).report;

                            storeReductionStopwatch.Stop();

                            var getFilesForRestoreStopwatch = System.Diagnostics.Stopwatch.StartNew();

                            var filesForRestore =
                                PersistentProcess.PersistentProcessVolatileRepresentation.GetFilesForRestoreProcess(
                                    processStoreFileStore).files
                                .Select(filePathAndContent => filePathAndContent.Key)
                                .ToImmutableHashSet(EnumerableExtension.EqualityComparer<string>());

                            getFilesForRestoreStopwatch.Stop();

                            var deleteFilesStopwatch = System.Diagnostics.Stopwatch.StartNew();

                            var totalDeletedFilesCount =
                                filePathsInProcessStorePartitions
                                .AsParallel()
                                .WithDegreeOfParallelism(numbersOfThreadsToDeleteFiles)
                                .Select(partitionFilePaths =>
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
                                })
                                .Sum();

                            deleteFilesStopwatch.Stop();

                            return new TruncateProcessHistoryReport
                            {
                                beginTime = beginTime,
                                deletedFilesCount = totalDeletedFilesCount,
                                storeReductionTimeSpentMilli = (int)storeReductionStopwatch.ElapsedMilliseconds,
                                storeReductionReport = storeReductionReport,
                                getFilesForRestoreTimeSpentMilli = (int)getFilesForRestoreStopwatch.ElapsedMilliseconds,
                                deleteFilesTimeSpentMilli = (int)deleteFilesStopwatch.ElapsedMilliseconds,
                                lockedTimeSpentMilli = (int)lockStopwatch.ElapsedMilliseconds,
                                totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds,
                            };
                        }
                    }

                    if (context.Request.Path.Equals(new PathString(PathApiTruncateProcessHistory)))
                    {
                        var truncateResult = truncateProcessHistory(productionBlockDurationLimit: TimeSpan.FromMinutes(1));

                        context.Response.StatusCode = 200;
                        context.Response.ContentType = "application/json";
                        await context.Response.WriteAsync(Newtonsoft.Json.JsonConvert.SerializeObject(truncateResult));
                        return;
                    }

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
                        await context.Response.Body.WriteAsync(fileContent as byte[] ?? fileContent.ToArray());
                        return;
                    }

                    (int statusCode, AttemptContinueWithCompositionEventReport responseReport) attemptContinueWithCompositionEvent(
                        CompositionLogRecordInFile.CompositionEvent compositionLogEvent)
                    {
                        lock (avoidConcurrencyLock)
                        {
                            var storeReductionStopwatch = System.Diagnostics.Stopwatch.StartNew();

                            var storeReductionReport =
                                publicAppHost?.processVolatileRepresentation?.StoreReductionRecordForCurrentState(processStoreWriter).report;

                            storeReductionStopwatch.Stop();

                            var (statusCode, report) =
                                AttemptContinueWithCompositionEventAndCommit(compositionLogEvent, processStoreFileStore);

                            report.storeReductionTimeSpentMilli = (int)storeReductionStopwatch.ElapsedMilliseconds;
                            report.storeReductionReport = storeReductionReport;

                            startPublicApp();

                            return (statusCode, report);
                        }
                    }

                    async System.Threading.Tasks.Task attemptContinueWithCompositionEventAndSendHttpResponse(
                        CompositionLogRecordInFile.CompositionEvent compositionLogEvent)
                    {
                        var (statusCode, attemptReport) = attemptContinueWithCompositionEvent(compositionLogEvent);

                        var responseBodyString = Newtonsoft.Json.JsonConvert.SerializeObject(attemptReport);

                        context.Response.StatusCode = statusCode;
                        await context.Response.WriteAsync(responseBodyString);
                    }

                    if (context.Request.Path.Equals(PathString.Empty) || context.Request.Path.Equals(new PathString("/")))
                    {
                        var httpApiGuide =
                            HtmlFromLines(
                                "<h3>HTTP APIs</h3>\n" +
                                HtmlFromLines(apiRoutes.Select(HtmlToDescribeApiRoute).ToArray())
                            );

                        context.Response.StatusCode = 200;
                        await context.Response.WriteAsync(
                            HtmlDocument(
                                HtmlFromLines(
                                    "Welcome to the Elm Fullstack admin interface version " + elm_fullstack.Program.AppVersionId + ".",
                                    httpApiGuide,
                                    "",
                                    ApiGuide)));
                        return;
                    }

                    context.Response.StatusCode = 404;
                    await context.Response.WriteAsync("Not Found");
                    return;
                });
        }

        static string ApiGuide =>
            HtmlFromLines(
                "The easiest way to use the APIs is via the command-line interface in the elm-fs executable file.",
                "To learn about the admin interface and how to deploy an app, see  " + LinkHtmlElementFromUrl(LinkToGuideUrl)
            );

        static string LinkToGuideUrl => "https://github.com/elm-fullstack/elm-fullstack/blob/main/guide/how-to-configure-and-deploy-an-elm-fullstack-app.md";

        static string LinkHtmlElementFromUrl(string url) =>
            "<a href='" + url + "'>" + url + "</a>";

        static string HtmlFromLines(params string[] lines) =>
            string.Join("<br>\n", lines);

        static string HtmlToDescribeApiRoute(ApiRoute apiRoute) =>
            LinkHtmlElementFromUrl(apiRoute.path) +
            " [ " + string.Join(", ", apiRoute.methods.Select(m => m.Key.ToUpperInvariant())) + " ]";

        class ApiRoute
        {
            public string path;

            public ImmutableDictionary<string, Func<HttpContext, PublicHostConfiguration, System.Threading.Tasks.Task>> methods;
        }

        static public string HtmlDocument(string body) =>
            String.Join("\n",
            new[]
            {
                "<html>",
                "<body>",
                body,
                "</body>",
                "</html>"
            });

        static public (int statusCode, AttemptContinueWithCompositionEventReport responseReport) AttemptContinueWithCompositionEventAndCommit(
            CompositionLogRecordInFile.CompositionEvent compositionLogEvent,
            IFileStore processStoreFileStore)
        {
            var beginTime = CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow);

            var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

            var testContinueResult = PersistentProcess.PersistentProcessVolatileRepresentation.TestContinueWithCompositionEvent(
                compositionLogEvent: compositionLogEvent,
                fileStoreReader: processStoreFileStore);

            var projectionResult = IProcessStoreReader.ProjectFileStoreReaderForAppendedCompositionLogEvent(
                originalFileStore: processStoreFileStore,
                compositionLogEvent: compositionLogEvent);

            if (testContinueResult.Ok.projectedFiles == null)
            {
                return (statusCode: 400, new AttemptContinueWithCompositionEventReport
                {
                    beginTime = beginTime,
                    parentCompositionHashBase16 = projectionResult.parentHashBase16,
                    compositionEvent = compositionLogEvent,
                    totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds,
                    result = Composition.Result<string, string>.err(testContinueResult.Err),
                });
            }

            foreach (var projectedFilePathAndContent in testContinueResult.Ok.projectedFiles)
                processStoreFileStore.SetFileContent(
                    projectedFilePathAndContent.filePath, projectedFilePathAndContent.fileContent);

            return (statusCode: 200, new AttemptContinueWithCompositionEventReport
            {
                beginTime = beginTime,
                parentCompositionHashBase16 = projectionResult.parentHashBase16,
                compositionEvent = compositionLogEvent,
                totalTimeSpentMilli = (int)totalStopwatch.ElapsedMilliseconds,
                result = Composition.Result<string, string>.ok("Successfully applied this composition event to the process."),
            });
        }
    }

    public class AttemptContinueWithCompositionEventReport
    {
        public string beginTime;

        public string parentCompositionHashBase16;

        public CompositionLogRecordInFile.CompositionEvent compositionEvent;

        public int storeReductionTimeSpentMilli;

        public PersistentProcess.StoreProvisionalReductionReport? storeReductionReport;

        public int totalTimeSpentMilli;

        public Composition.Result<string, string> result;
    }

    public class TruncateProcessHistoryReport
    {
        public string beginTime;

        public int deletedFilesCount;

        public int lockedTimeSpentMilli;

        public int totalTimeSpentMilli;

        public int storeReductionTimeSpentMilli;

        public PersistentProcess.StoreProvisionalReductionReport? storeReductionReport;

        public int getFilesForRestoreTimeSpentMilli;

        public int deleteFilesTimeSpentMilli;
    }
}
