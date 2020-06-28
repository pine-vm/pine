using Kalmit.PersistentProcess.WebHost.ProcessStoreSupportingMigrations;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Net.Http.Headers;
using System.Text;

namespace Kalmit.PersistentProcess.WebHost
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
                        PersistentProcess.PersistentProcessVolatileRepresentation.Restore(
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
                                        var reductionRecord = processVolatileRepresentation.StoreReductionRecordForCurrentState(processStoreWriter);
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
                        var appConfigTree = Composition.ParseAsTree(
                            processVolatileRepresentation.lastAppConfig.Value.appConfigComponent).Ok;

                        var appConfigFilesNamesAndContents =
                            appConfigTree.EnumerateBlobsTransitive()
                            .Select(blobPathAndContent => (
                                fileName: (IImmutableList<string>)blobPathAndContent.path.Select(name => System.Text.Encoding.UTF8.GetString(name.ToArray())).ToImmutableList(),
                                fileContent: blobPathAndContent.blobContent))
                                .ToImmutableList();

                        var webAppConfigurationFile =
                            appConfigFilesNamesAndContents
                            .FirstOrDefault(filePathAndContent => filePathAndContent.fileName.SequenceEqual(JsonFilePath))
                            .fileContent;

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

                        AuthenticationHeaderValue.TryParse(
                            requestAuthorizationHeaderValue.FirstOrDefault(), out var requestAuthorization);

                        if (!(0 < adminPassword?.Length))
                        {
                            context.Response.StatusCode = 403;
                            await context.Response.WriteAsync("Forbidden");
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

                    var requestPathIsDeployAppConfigAndInitElmAppState =
                        context.Request.Path.Equals(new PathString(PathApiDeployAppConfigAndInitElmAppState));

                    if (context.Request.Path.Equals(new PathString(PathApiGetDeployedAppConfig)))
                    {
                        if (!string.Equals(context.Request.Method, "get", StringComparison.InvariantCultureIgnoreCase))
                        {
                            context.Response.StatusCode = 405;
                            await context.Response.WriteAsync("Method not supported.");
                            return;
                        }

                        var appConfig = publicAppHost?.processVolatileRepresentation?.lastAppConfig?.appConfigComponent;

                        if (appConfig == null)
                        {
                            context.Response.StatusCode = 404;
                            await context.Response.WriteAsync("I did not find an app config in the history. Looks like no app was deployed so far.");
                            return;
                        }

                        var appConfigHashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(appConfig));

                        var appConfigTree = Composition.ParseAsTree(appConfig).Ok;

                        var appConfigFilesNamesAndContents =
                            appConfigTree.EnumerateBlobsTransitive()
                            .Select(blobPathAndContent => (
                                fileName: (IImmutableList<string>)blobPathAndContent.path.Select(name => Encoding.UTF8.GetString(name.ToArray())).ToImmutableList(),
                                fileContent: blobPathAndContent.blobContent))
                            .ToImmutableList();

                        var appConfigZipArchive =
                            ZipArchive.ZipArchiveFromEntries(
                                ElmApp.ToFlatDictionaryWithPathComparer(appConfigFilesNamesAndContents));

                        context.Response.StatusCode = 200;
                        context.Response.Headers.ContentLength = appConfigZipArchive.LongLength;
                        context.Response.Headers.Add("Content-Disposition", new ContentDispositionHeaderValue("attachment") { FileName = appConfigHashBase16 + ".zip" }.ToString());
                        context.Response.Headers.Add("Content-Type", new MediaTypeHeaderValue("application/zip").ToString());

                        await context.Response.Body.WriteAsync(appConfigZipArchive);
                        return;
                    }

                    if (requestPathIsDeployAppConfigAndInitElmAppState ||
                        context.Request.Path.Equals(new PathString(PathApiDeployAppConfigAndMigrateElmAppState)))
                    {
                        if (!string.Equals(context.Request.Method, "post", StringComparison.InvariantCultureIgnoreCase))
                        {
                            context.Response.StatusCode = 405;
                            await context.Response.WriteAsync("Method not supported.");
                            return;
                        }

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
                            Composition.TreeFromSetOfBlobsWithCommonFilePath(
                                ZipArchive.EntriesFromZipArchive(webAppConfigZipArchive));

                        var appConfigComponent = Composition.FromTree(appConfigTree);

                        processStoreWriter.StoreComponent(appConfigComponent);

                        var appConfigValueInFile =
                            new ProcessStoreSupportingMigrations.ValueInFileStructure
                            {
                                HashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(appConfigComponent))
                            };

                        var compositionLogEvent =
                            ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent.EventForDeployAppConfig(
                                appConfigValueInFile: appConfigValueInFile,
                                initElmAppState: requestPathIsDeployAppConfigAndInitElmAppState);

                        await attemptContinueWithCompositionEventAndSendHttpResponse(compositionLogEvent);
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

                    if (context.Request.Path.Equals(new PathString(PathApiElmAppState)))
                    {
                        if (publicAppHost == null)
                        {
                            context.Response.StatusCode = 400;
                            await context.Response.WriteAsync("Not possible because there is no app (state).");
                            return;
                        }

                        if (string.Equals(context.Request.Method, "get", StringComparison.InvariantCultureIgnoreCase))
                        {
                            var processVolatileRepresentation = publicAppHost?.processVolatileRepresentation;

                            var components = new List<Composition.Component>();

                            var storeWriter = new DelegatingProcessStoreWriter
                            {
                                StoreComponentDelegate = components.Add,
                                StoreProvisionalReductionDelegate = _ => { },
                                SetCompositionLogHeadRecordDelegate = _ => throw new Exception("Unexpected use of interface."),
                            };

                            var reductionRecord =
                                processVolatileRepresentation?.StoreReductionRecordForCurrentState(storeWriter);

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
                            return;
                        }
                        else
                        {
                            if (string.Equals(context.Request.Method, "post", StringComparison.InvariantCultureIgnoreCase))
                            {
                                var elmAppStateToSet = new StreamReader(context.Request.Body, System.Text.Encoding.UTF8).ReadToEndAsync().Result;

                                var elmAppStateComponent = Composition.Component.Blob(Encoding.UTF8.GetBytes(elmAppStateToSet));

                                var appConfigValueInFile =
                                    new ProcessStoreSupportingMigrations.ValueInFileStructure
                                    {
                                        HashBase16 = CommonConversion.StringBase16FromByteArray(Composition.GetHash(elmAppStateComponent))
                                    };

                                processStoreWriter.StoreComponent(elmAppStateComponent);

                                await attemptContinueWithCompositionEventAndSendHttpResponse(
                                    new ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent
                                    {
                                        SetElmAppState = appConfigValueInFile
                                    });
                                return;
                            }
                            else
                            {
                                context.Response.StatusCode = 405;
                                await context.Response.WriteAsync("Method not supported.");
                                return;
                            }
                        }
                    }

                    if (context.Request.Path.Equals(new PathString(PathApiReplaceProcessHistory)))
                    {
                        var memoryStream = new MemoryStream();
                        context.Request.Body.CopyTo(memoryStream);

                        var webAppConfigZipArchive = memoryStream.ToArray();

                        var replacementFiles =
                            ZipArchive.EntriesFromZipArchive(webAppConfigZipArchive)
                            .Select(filePathAndContent =>
                                (path: filePathAndContent.name.Split(new[] { '/', '\\' }).ToImmutableList()
                                , content: filePathAndContent.content))
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
                        return;
                    }

                    TruncateProcessHistoryReport truncateProcessHistory(TimeSpan productionBlockDurationLimit)
                    {
                        var beginTime = CommonConversion.TimeStringViewForReport(DateTimeOffset.UtcNow);

                        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

                        lock (avoidConcurrencyLock)
                        {
                            publicAppHost?.processVolatileRepresentation?.StoreReductionRecordForCurrentState(processStoreWriter);

                            var filesForRestore =
                                PersistentProcess.PersistentProcessVolatileRepresentation.GetFilesForRestoreProcess(
                                    processStoreFileStore).files
                                .Select(filePathAndContent => filePathAndContent.Key)
                                .ToImmutableHashSet(EnumerableExtension.EqualityComparer<string>());

                            var deleteFilesStopwatch = System.Diagnostics.Stopwatch.StartNew();

                            int deletedFilesCount = 0;

                            foreach (var filePath in processStoreFileStore.ListFilesInDirectory(ImmutableList<string>.Empty))
                            {
                                if (filesForRestore.Contains(filePath))
                                    continue;

                                if (productionBlockDurationLimit < totalStopwatch.Elapsed)
                                    break;

                                processStoreFileStore.DeleteFile(filePath);
                                ++deletedFilesCount;
                            }

                            deleteFilesStopwatch.Stop();

                            return new TruncateProcessHistoryReport
                            {
                                beginTime = beginTime,
                                deletedFilesCount = deletedFilesCount,
                                deleteFilesTimeSpentMilli = (int)deleteFilesStopwatch.ElapsedMilliseconds,
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
                        await context.Response.Body.WriteAsync(fileContent);
                        return;
                    }

                    (int statusCode, AttemptContinueWithCompositionEventReport responseReport) attemptContinueWithCompositionEvent(
                        ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent compositionLogEvent)
                    {
                        lock (avoidConcurrencyLock)
                        {
                            publicAppHost?.processVolatileRepresentation?.StoreReductionRecordForCurrentState(processStoreWriter);

                            var response =
                                AttemptContinueWithCompositionEventAndCommit(compositionLogEvent, processStoreFileStore);

                            startPublicApp();

                            return response;
                        }
                    }

                    async System.Threading.Tasks.Task attemptContinueWithCompositionEventAndSendHttpResponse(
                        ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent compositionLogEvent)
                    {
                        var (statusCode, attemptReport) = attemptContinueWithCompositionEvent(compositionLogEvent);

                        var responseBodyString = Newtonsoft.Json.JsonConvert.SerializeObject(attemptReport);

                        context.Response.StatusCode = statusCode;
                        await context.Response.WriteAsync(responseBodyString);
                        return;
                    }

                    if (context.Request.Path.Equals(PathString.Empty) || context.Request.Path.Equals(new PathString("/")))
                    {
                        context.Response.StatusCode = 200;
                        await context.Response.WriteAsync(
                            "Welcome to Elm-fullstack version " + Program.AppVersionId + ".\n" +
                            "To learn about this admin interface, see http://elm-fullstack.org/");
                        return;
                    }

                    context.Response.StatusCode = 404;
                    await context.Response.WriteAsync("Not Found");
                    return;
                });
        }

        static public (int statusCode, AttemptContinueWithCompositionEventReport responseReport) AttemptContinueWithCompositionEventAndCommit(
            ProcessStoreSupportingMigrations.CompositionLogRecordInFile.CompositionEvent compositionLogEvent,
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

        public int totalTimeSpentMilli;

        public Composition.Result<string, string> result;
    }

    public class TruncateProcessHistoryReport
    {
        public string beginTime;

        public int deletedFilesCount;

        public int totalTimeSpentMilli;

        public int deleteFilesTimeSpentMilli;
    }
}
