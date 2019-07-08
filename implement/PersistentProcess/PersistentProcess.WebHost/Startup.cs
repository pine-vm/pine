using FluffySpoon.AspNet.LetsEncrypt;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Logging;
using System;
using System.Collections.Concurrent;
using System.Linq;

namespace Kalmit.PersistentProcess.WebHost
{
    public class Startup
    {
        private readonly ILogger<Startup> _logger;

        public Startup(ILogger<Startup> logger)
        {
            _logger = logger;

            _logger.LogTrace("Testing LogTrace");
            _logger.LogDebug("Testing LogDebug");
        }

        public void ConfigureServices(IServiceCollection services)
        {
            var serviceProvider = services.BuildServiceProvider();
            var config = serviceProvider.GetService<IConfiguration>();

            var webAppConfigFile = System.IO.File.ReadAllBytes(config.GetValue<string>(Configuration.WebAppConfigurationFilePathSettingKey));

            _logger.LogInformation("Loaded configuration " +
                CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(webAppConfigFile)));

            var webAppConfig = WebAppConfiguration.FromFiles(ZipArchive.EntriesFromZipArchive(webAppConfigFile).ToList());
            services.AddSingleton<WebAppConfiguration>(webAppConfig);

            var getDateTimeOffset = serviceProvider.GetService<Func<DateTimeOffset>>();

            if (getDateTimeOffset == null)
            {
                getDateTimeOffset = () => DateTimeOffset.UtcNow;
                services.AddSingleton<Func<DateTimeOffset>>(getDateTimeOffset);
            }

            var processStoreDirectory = config.GetValue<string>(Configuration.ProcessStoreDirectoryPathSettingKey);
            var processStore = new Kalmit.ProcessStore.ProcessStoreInFileDirectory(
                processStoreDirectory,
                () =>
                {
                    var time = getDateTimeOffset();
                    var directoryName = time.ToString("yyyy-MM-dd");
                    return System.IO.Path.Combine(directoryName, directoryName + "T" + time.ToString("HH") + ".composition.jsonl");
                });

            services.AddSingleton<ProcessStore.IProcessStoreReader>(processStore);
            services.AddSingleton<ProcessStore.IProcessStoreWriter>(processStore);
            services.AddSingleton<IPersistentProcess>(BuildPersistentProcess);

            services.AddCors();

            var letsEncryptOptions = webAppConfig?.Map?.letsEncryptOptions;
            if (letsEncryptOptions == null)
            {
                _logger.LogInformation("I did not find 'letsEncryptOptions' in the configuration. I continue without Let's Encrypt.");
            }
            else
            {
                _logger.LogInformation("I found 'letsEncryptOptions' in the configuration.");
                services.AddFluffySpoonLetsEncryptRenewalService(webAppConfig?.Map?.letsEncryptOptions);
                services.AddFluffySpoonLetsEncryptFileCertificatePersistence();
                services.AddFluffySpoonLetsEncryptMemoryChallengePersistence();
            }

            Asp.ConfigureServices(services);
        }

        static PersistentProcessWithHistoryOnFileFromElm019Code BuildPersistentProcess(IServiceProvider services)
        {
            var logger = services.GetService<ILogger<Startup>>();
            var elmAppFile = services.GetService<WebAppConfiguration>()?.ElmAppFile;

            if (elmAppFile == null)
            {
                logger.LogInformation("Found no ElmAppFile in configuration.");
                return null;
            }

            logger.LogInformation("Begin to build the persistent process for Elm app " +
                CommonConversion.StringBase16FromByteArray(CommonConversion.HashSHA256(elmAppFile)));

            var persistentProcess =
                new PersistentProcessWithHistoryOnFileFromElm019Code(
                    services.GetService<ProcessStore.IProcessStoreReader>(),
                    elmAppFile,
                    logger: logEntry => logger.LogInformation(logEntry));

            logger.LogInformation("Completed building the persistent process.");

            return persistentProcess;
        }

        public void Configure(
            IApplicationBuilder app,
            IHostingEnvironment env,
            WebAppConfiguration webAppConfig,
            ProcessStore.IProcessStoreWriter processStoreWriter,
            Func<DateTimeOffset> getDateTimeOffset)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }

            var nextHttpRequestIndex = 0;

            var cyclicReductionStoreLock = new object();
            DateTimeOffset? cyclicReductionStoreLastTime = null;
            var cyclicReductionStoreDistanceSeconds = (int)TimeSpan.FromHours(1).TotalSeconds;

            if (webAppConfig?.Map?.corsAllowAnything ?? false)
            {
                app.UseCors(builder => builder
                    .AllowAnyOrigin()
                    .AllowAnyMethod()
                    .AllowAnyHeader()
                    .AllowCredentials());
            }

            if (webAppConfig?.Map?.letsEncryptOptions != null)
                app.UseFluffySpoonLetsEncryptChallengeApprovalMiddleware();

            var createVolatileHostAttempts = 0;

            var volatileHosts = new ConcurrentDictionary<string, CSharpScriptContext>();

            InterfaceToHost.Result<InterfaceToHost.TaskResult.RunInVolatileHostError, InterfaceToHost.TaskResult.RunInVolatileHostComplete>
                performProcessTaskRunInVolatileHost(
                InterfaceToHost.Task.RunInVolatileHost runInVolatileHost)
            {
                if (!volatileHosts.TryGetValue(runInVolatileHost.hostId, out var volatileHost))
                {
                    return new InterfaceToHost.Result<InterfaceToHost.TaskResult.RunInVolatileHostError, InterfaceToHost.TaskResult.RunInVolatileHostComplete>
                    {
                        err = new InterfaceToHost.TaskResult.RunInVolatileHostError
                        {
                            hostNotFound = new object(),
                        }
                    };
                }

                var stopwatch = System.Diagnostics.Stopwatch.StartNew();

                var fromVolatileHostResult = volatileHost.RunScript(runInVolatileHost.script);

                stopwatch.Stop();

                return new InterfaceToHost.Result<InterfaceToHost.TaskResult.RunInVolatileHostError, InterfaceToHost.TaskResult.RunInVolatileHostComplete>
                {
                    ok = new InterfaceToHost.TaskResult.RunInVolatileHostComplete
                    {
                        exceptionToString = fromVolatileHostResult.Exception?.ToString(),
                        returnValueToString = fromVolatileHostResult.ReturnValue?.ToString(),
                        durationInMilliseconds = stopwatch.ElapsedMilliseconds,
                    }
                };
            }

            InterfaceToHost.TaskResult performProcessTask(InterfaceToHost.Task task)
            {
                if (task?.createVolatileHost != null)
                {
                    var volatileHostId = System.Threading.Interlocked.Increment(ref createVolatileHostAttempts).ToString();

                    volatileHosts[volatileHostId] = new CSharpScriptContext(BlobLibrary.GetBlobWithSHA256);

                    return new InterfaceToHost.TaskResult
                    {
                        createVolatileHostResponse = new InterfaceToHost.Result<object, InterfaceToHost.TaskResult.CreateVolatileHostComplete>
                        {
                            ok = new InterfaceToHost.TaskResult.CreateVolatileHostComplete
                            {
                                hostId = volatileHostId,
                            },
                        },
                    };
                }

                if (task?.releaseVolatileHost != null)
                {
                    volatileHosts.TryRemove(task?.releaseVolatileHost.hostId, out var volatileHost);

                    return new InterfaceToHost.TaskResult
                    {
                        completeWithoutResult = new object(),
                    };
                }

                if (task?.runInVolatileHost != null)
                {
                    return new InterfaceToHost.TaskResult
                    {
                        runInVolatileHostResponse = performProcessTaskRunInVolatileHost(task?.runInVolatileHost),
                    };
                }

                throw new NotImplementedException("Unexpected task structure.");
            }

            void performProcessTaskAndFeedbackEvent(InterfaceToHost.StartTask taskWithId)
            {
                var taskResult = performProcessTask(taskWithId.task);

                var interfaceEvent = new InterfaceToHost.Event
                {
                    taskComplete = new InterfaceToHost.ResultFromTaskWithId
                    {
                        taskId = taskWithId.taskId,
                        taskResult = taskResult,
                    }
                };

                processEventAndResultingRequests(interfaceEvent);
            }

            var processRequestCompleteHttpResponse = new ConcurrentDictionary<string, InterfaceToHost.HttpResponse>();

            var persistentProcess = app.ApplicationServices.GetService<IPersistentProcess>();

            void processEventAndResultingRequests(InterfaceToHost.Event interfaceEvent)
            {
                lock (processStoreWriter)
                {
                    var serializedInterfaceEvent = Newtonsoft.Json.JsonConvert.SerializeObject(interfaceEvent, jsonSerializerSettings);

                    var (eventResponses, compositionRecord) = persistentProcess.ProcessEvents(new[] { serializedInterfaceEvent });

                    processStoreWriter.AppendSerializedCompositionRecord(compositionRecord.serializedCompositionRecord);

                    var serializedResponse = eventResponses.Single();

                    InterfaceToHost.ResponseOverSerialInterface structuredResponse = null;

                    try
                    {
                        structuredResponse =
                            Newtonsoft.Json.JsonConvert.DeserializeObject<InterfaceToHost.ResponseOverSerialInterface>(
                                serializedResponse);
                    }
                    catch (Exception parseException)
                    {
                        throw new Exception(
                            "Failed to parse event response from app. Looks like the loaded elm app is not compatible with the interface.\nResponse from app follows:\n" + serializedResponse,
                            parseException);
                    }

                    if (structuredResponse?.decodeEventSuccess == null)
                    {
                        throw new Exception("Hosted app failed to decode the event: " + structuredResponse.decodeEventError);
                    }

                    foreach (var requestFromProcess in structuredResponse.decodeEventSuccess)
                    {
                        if (requestFromProcess.completeHttpResponse != null)
                            processRequestCompleteHttpResponse[requestFromProcess.completeHttpResponse.httpRequestId] =
                                requestFromProcess.completeHttpResponse.response;

                        if (requestFromProcess.startTask != null)
                            System.Threading.Tasks.Task.Run(() => performProcessTaskAndFeedbackEvent(requestFromProcess.startTask));
                    }
                }
            }

            app
            .Use(async (context, next) => await Asp.MiddlewareFromWebAppConfig(webAppConfig, context, next))
            .Run(async (context) =>
            {
                var currentDateTime = getDateTimeOffset();
                var timeMilli = currentDateTime.ToUnixTimeMilliseconds();
                var httpRequestIndex = System.Threading.Interlocked.Increment(ref nextHttpRequestIndex);

                var httpRequestId = timeMilli.ToString() + "-" + httpRequestIndex.ToString();

                {
                    var httpEvent = AsPersistentProcessInterfaceHttpRequestEvent(context, httpRequestId, currentDateTime);

                    var httpRequestInterfaceEvent = new InterfaceToHost.Event
                    {
                        httpRequest = httpEvent,
                    };

                    processEventAndResultingRequests(httpRequestInterfaceEvent);
                }

                var waitForHttpResponseClock = System.Diagnostics.Stopwatch.StartNew();

                while (true)
                {
                    if (processRequestCompleteHttpResponse.TryRemove(httpRequestId, out var httpResponse))
                    {
                        context.Response.StatusCode = httpResponse.statusCode;

                        foreach (var headerToAdd in (httpResponse.headersToAdd).EmptyIfNull())
                            context.Response.Headers[headerToAdd.name] =
                                new Microsoft.Extensions.Primitives.StringValues(headerToAdd.values);

                        await context.Response.WriteAsync(httpResponse?.bodyAsString ?? "");
                        break;
                    }

                    if (60 <= waitForHttpResponseClock.Elapsed.TotalSeconds)
                        throw new TimeoutException(
                            "Persistent process did not return a HTTP response within " +
                            (int)waitForHttpResponseClock.Elapsed.TotalSeconds +
                            " seconds.");

                    System.Threading.Thread.Sleep(100);
                }

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

                            var reductionRecord = persistentProcess.ReductionRecordForCurrentState();

                            lock (processStoreWriter)
                            {
                                processStoreWriter.StoreReduction(reductionRecord);
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
            });
        }

        static Newtonsoft.Json.JsonSerializerSettings jsonSerializerSettings = new Newtonsoft.Json.JsonSerializerSettings
        {
            DefaultValueHandling = Newtonsoft.Json.DefaultValueHandling.Ignore,
        };

        static InterfaceToHost.HttpRequestEvent AsPersistentProcessInterfaceHttpRequestEvent(
            HttpContext httpContext,
            string httpRequestId,
            DateTimeOffset time)
        {
            return new InterfaceToHost.HttpRequestEvent
            {
                posixTimeMilli = time.ToUnixTimeMilliseconds(),

                httpRequestId = httpRequestId,

                requestContext = new InterfaceToHost.HttpRequestContext
                {
                    clientAddress = httpContext.Connection.RemoteIpAddress?.ToString(),
                },

                request = Asp.AsPersistentProcessInterfaceHttpRequest(httpContext.Request),
            };
        }
    }
}
