using FluffySpoon.AspNet.LetsEncrypt;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using System;
using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Linq;

namespace Kalmit.PersistentProcess.WebHost
{
    public class StartupPublicApp
    {
        private readonly ILogger<StartupPublicApp> _logger;

        public StartupPublicApp(ILogger<StartupPublicApp> logger)
        {
            _logger = logger;

            _logger.LogTrace("Testing LogTrace");
            _logger.LogDebug("Testing LogDebug");
        }

        public void ConfigureServices(IServiceCollection services)
        {
            var serviceProvider = services.BuildServiceProvider();
            var config = serviceProvider.GetService<IConfiguration>();

            var getDateTimeOffset = serviceProvider.GetService<Func<DateTimeOffset>>();

            if (getDateTimeOffset == null)
            {
                getDateTimeOffset = () => DateTimeOffset.UtcNow;
                services.AddSingleton<Func<DateTimeOffset>>(getDateTimeOffset);
            }

            var persistentProcessMap =
                serviceProvider.GetService<PersistentProcessMap>()?.mapPersistentProcess
                ??
                new Func<PersistentProcess.IPersistentProcess, PersistentProcess.IPersistentProcess>(persistentProcess => persistentProcess);

            services.AddSingleton<PersistentProcess.IPersistentProcess>(
                serviceProvider => persistentProcessMap(BuildPersistentProcess(serviceProvider)));

            {
                var processStoreReader =
                    serviceProvider.GetService<ProcessStoreSupportingMigrations.IProcessStoreReader>();

                var appConfigComponent =
                    PersistentProcess.PersistentProcessVolatileRepresentation.Restore(
                        processStoreReader,
                        _ => { })
                    ?.lastAppConfig?.appConfigComponent;

                if (appConfigComponent == null)
                    throw new Exception("This process store contains no app config.");

                var appConfigTree = Composition.ParseAsTree(appConfigComponent).Ok;

                var appConfigFilesNamesAndContents =
                    appConfigTree.EnumerateBlobsTransitive()
                    .Select(blobPathAndContent => (
                        fileName: (IImmutableList<string>)blobPathAndContent.path.Select(name => System.Text.Encoding.UTF8.GetString(name.ToArray())).ToImmutableList(),
                        fileContent: blobPathAndContent.blobContent))
                        .ToImmutableList();

                var webAppConfigObject = WebAppConfiguration.FromFiles(appConfigFilesNamesAndContents);

                services.AddSingleton<WebAppConfiguration>(webAppConfigObject);

                var letsEncryptOptions = webAppConfigObject?.JsonStructure?.letsEncryptOptions;

                if (letsEncryptOptions == null)
                {
                    _logger.LogInformation("I did not find 'letsEncryptOptions' in the configuration. I continue without Let's Encrypt.");
                }
                else
                {
                    _logger.LogInformation("I found 'letsEncryptOptions' in the configuration.");
                    services.AddFluffySpoonLetsEncryptRenewalService(letsEncryptOptions);
                    services.AddFluffySpoonLetsEncryptFileCertificatePersistence();
                    services.AddFluffySpoonLetsEncryptMemoryChallengePersistence();
                }
            }

            Asp.ConfigureServices(services);
        }

        static PersistentProcess.IPersistentProcess BuildPersistentProcess(IServiceProvider services)
        {
            var logger = services.GetService<ILogger<StartupPublicApp>>();
            var elmAppFiles = services.GetService<WebAppConfiguration>()?.ElmAppFiles;

            if (!(0 < elmAppFiles?.Count))
            {
                logger.LogInformation("Found no ElmAppFile in configuration.");
                return null;
            }

            var elmAppComposition =
                Composition.FromTree(Composition.TreeFromSetOfBlobsWithStringPath(elmAppFiles));

            logger.LogInformation("Begin to build the persistent process for Elm app " +
                CommonConversion.StringBase16FromByteArray(Composition.GetHash(elmAppComposition)));

            var persistentProcess =
                PersistentProcess.PersistentProcessVolatileRepresentation.Restore(
                    services.GetService<ProcessStoreSupportingMigrations.IProcessStoreReader>(),
                    logger: logEntry => logger.LogInformation(logEntry));

            logger.LogInformation("Completed building the persistent process.");

            return persistentProcess;
        }

        public void Configure(
            IApplicationBuilder app,
            IWebHostEnvironment env,
            WebAppConfiguration webAppConfig,
            ProcessStoreSupportingMigrations.IProcessStoreWriter processStoreWriter,
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

            if (webAppConfig?.JsonStructure?.letsEncryptOptions != null)
                app.UseFluffySpoonLetsEncryptChallengeApprovalMiddleware();

            var createVolatileHostAttempts = 0;

            var volatileHosts = new ConcurrentDictionary<string, VolatileHost>();

            InterfaceToHost.Result<InterfaceToHost.TaskResult.RequestToVolatileHostError, InterfaceToHost.TaskResult.RequestToVolatileHostComplete>
                performProcessTaskRequestToVolatileHost(
                InterfaceToHost.Task.RequestToVolatileHostStructure requestToVolatileHost)
            {
                if (!volatileHosts.TryGetValue(requestToVolatileHost.hostId, out var volatileHost))
                {
                    return new InterfaceToHost.Result<InterfaceToHost.TaskResult.RequestToVolatileHostError, InterfaceToHost.TaskResult.RequestToVolatileHostComplete>
                    {
                        Err = new InterfaceToHost.TaskResult.RequestToVolatileHostError
                        {
                            HostNotFound = new object(),
                        }
                    };
                }

                var stopwatch = System.Diagnostics.Stopwatch.StartNew();

                var fromVolatileHostResult = volatileHost.ProcessRequest(requestToVolatileHost.request);

                stopwatch.Stop();

                return new InterfaceToHost.Result<InterfaceToHost.TaskResult.RequestToVolatileHostError, InterfaceToHost.TaskResult.RequestToVolatileHostComplete>
                {
                    Ok = new InterfaceToHost.TaskResult.RequestToVolatileHostComplete
                    {
                        exceptionToString = fromVolatileHostResult.Exception?.ToString(),
                        returnValueToString = fromVolatileHostResult.ReturnValue?.ToString(),
                        durationInMilliseconds = stopwatch.ElapsedMilliseconds,
                    }
                };
            }

            InterfaceToHost.TaskResult performProcessTask(InterfaceToHost.Task task)
            {
                if (task?.CreateVolatileHost != null)
                {
                    try
                    {
                        var volatileHost = new VolatileHost(BlobLibrary.GetBlobWithSHA256, task?.CreateVolatileHost.script);

                        var volatileHostId = System.Threading.Interlocked.Increment(ref createVolatileHostAttempts).ToString();

                        volatileHosts[volatileHostId] = volatileHost;

                        return new InterfaceToHost.TaskResult
                        {
                            CreateVolatileHostResponse = new InterfaceToHost.Result<InterfaceToHost.TaskResult.CreateVolatileHostErrorStructure, InterfaceToHost.TaskResult.CreateVolatileHostComplete>
                            {
                                Ok = new InterfaceToHost.TaskResult.CreateVolatileHostComplete
                                {
                                    hostId = volatileHostId,
                                },
                            },
                        };
                    }
                    catch (Exception createVolatileHostException)
                    {
                        return new InterfaceToHost.TaskResult
                        {
                            CreateVolatileHostResponse = new InterfaceToHost.Result<InterfaceToHost.TaskResult.CreateVolatileHostErrorStructure, InterfaceToHost.TaskResult.CreateVolatileHostComplete>
                            {
                                Err = new InterfaceToHost.TaskResult.CreateVolatileHostErrorStructure
                                {
                                    exceptionToString = createVolatileHostException.ToString(),
                                },
                            },
                        };
                    }
                }

                if (task?.ReleaseVolatileHost != null)
                {
                    volatileHosts.TryRemove(task?.ReleaseVolatileHost.hostId, out var volatileHost);

                    return new InterfaceToHost.TaskResult
                    {
                        CompleteWithoutResult = new object(),
                    };
                }

                if (task?.RequestToVolatileHost != null)
                {
                    return new InterfaceToHost.TaskResult
                    {
                        RequestToVolatileHostResponse = performProcessTaskRequestToVolatileHost(task?.RequestToVolatileHost),
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

            var persistentProcess = app.ApplicationServices.GetService<PersistentProcess.IPersistentProcess>();

            void processEventAndResultingRequests(InterfaceToHost.Event interfaceEvent)
            {
                lock (processStoreWriter)
                {
                    var serializedInterfaceEvent = Newtonsoft.Json.JsonConvert.SerializeObject(interfaceEvent, jsonSerializerSettings);

                    var eventResponses = persistentProcess.ProcessElmAppEvents(
                        processStoreWriter, new[] { serializedInterfaceEvent });

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
                        var headerContentType =
                            httpResponse.headersToAdd
                            ?.FirstOrDefault(header => header.name?.ToLowerInvariant() == "content-type")
                            ?.values?.FirstOrDefault();

                        context.Response.StatusCode = httpResponse.statusCode;

                        foreach (var headerToAdd in (httpResponse.headersToAdd).EmptyIfNull())
                            context.Response.Headers[headerToAdd.name] =
                                new Microsoft.Extensions.Primitives.StringValues(headerToAdd.values);

                        if (headerContentType != null)
                            context.Response.ContentType = headerContentType;

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

                            lock (processStoreWriter)
                            {
                                var reductionRecord = persistentProcess.StoreReductionRecordForCurrentState(processStoreWriter);
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

    public class PersistentProcessMap
    {
        public Func<PersistentProcess.IPersistentProcess, PersistentProcess.IPersistentProcess> mapPersistentProcess;
    }
}
