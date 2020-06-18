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

            var webAppAndElmAppConfig = serviceProvider.GetService<WebAppAndElmAppConfig>();

            if (webAppAndElmAppConfig == null)
            {
                throw new Exception("Missing reference to the web app config.");
            }

            {
                var letsEncryptOptions = webAppAndElmAppConfig.WebAppConfiguration?.letsEncryptOptions;

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

        public void Configure(
            IApplicationBuilder app,
            IWebHostEnvironment env,
            WebAppAndElmAppConfig webAppAndElmAppConfig,
            Func<DateTimeOffset> getDateTimeOffset,
            IHostApplicationLifetime appLifetime)
        {
            if (env.IsDevelopment())
            {
                app.UseDeveloperExceptionPage();
            }

            if (webAppAndElmAppConfig == null)
            {
                throw new Exception("Missing reference to the web app config.");
            }

            var tasksCancellationTokenSource = new System.Threading.CancellationTokenSource();

            appLifetime.ApplicationStopping.Register(() =>
            {
                tasksCancellationTokenSource.Cancel();
                _logger?.LogInformation("Public app noticed ApplicationStopping.");
            });

            var nextHttpRequestIndex = 0;

            if (webAppAndElmAppConfig.WebAppConfiguration?.letsEncryptOptions != null)
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

            void processEventAndResultingRequests(InterfaceToHost.Event interfaceEvent)
            {
                if (tasksCancellationTokenSource.IsCancellationRequested)
                    return;

                var serializedInterfaceEvent = Newtonsoft.Json.JsonConvert.SerializeObject(interfaceEvent, jsonSerializerSettings);

                var serializedResponse = webAppAndElmAppConfig.ProcessEventInElmApp(serializedInterfaceEvent);

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

                    if (requestFromProcess.NotifyWhenArrivedAtTimeRequest != null)
                        System.Threading.Tasks.Task.Run(() =>
                            {
                                var delayMilliseconds =
                                    requestFromProcess.NotifyWhenArrivedAtTimeRequest.posixTimeMilli -
                                    getDateTimeOffset().ToUnixTimeMilliseconds();

                                if (0 < delayMilliseconds)
                                    System.Threading.Tasks.Task.Delay((int)delayMilliseconds, tasksCancellationTokenSource.Token);

                                processEventAndResultingRequests(new InterfaceToHost.Event
                                {
                                    ArrivedAtTimeEvent = new InterfaceToHost.ArrivedAtTimeEventStructure
                                    {
                                        posixTimeMilli = getDateTimeOffset().ToUnixTimeMilliseconds()
                                    }
                                });
                            });
                }
            }

            app
            .Use(async (context, next) => await Asp.MiddlewareFromWebAppConfig(webAppAndElmAppConfig.WebAppConfiguration, context, next))
                .Run(async (context) =>
                {
                    var currentDateTime = getDateTimeOffset();
                    var timeMilli = currentDateTime.ToUnixTimeMilliseconds();
                    var httpRequestIndex = System.Threading.Interlocked.Increment(ref nextHttpRequestIndex);

                    var httpRequestId = timeMilli.ToString() + "-" + httpRequestIndex.ToString();

                    {
                        var httpEvent = await AsPersistentProcessInterfaceHttpRequestEvent(context, httpRequestId, currentDateTime);

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

                            byte[] contentAsByteArray = null;

                            if (httpResponse?.bodyAsBase64 != null)
                            {
                                var buffer = new byte[httpResponse.bodyAsBase64.Length * 3 / 4];

                                if (!Convert.TryFromBase64String(httpResponse.bodyAsBase64, buffer, out var bytesWritten))
                                {
                                    throw new FormatException(
                                    "Failed to convert from base64. bytesWritten=" + bytesWritten +
                                    ", input.length=" + httpResponse.bodyAsBase64.Length + ", input:\n" +
                                    httpResponse.bodyAsBase64);
                                }

                                contentAsByteArray = buffer.AsSpan(0, bytesWritten).ToArray();
                            }

                            context.Response.ContentLength = contentAsByteArray?.Length ?? 0;

                            if (contentAsByteArray != null)
                                await context.Response.Body.WriteAsync(contentAsByteArray);

                            break;
                        }

                        if (60 <= waitForHttpResponseClock.Elapsed.TotalSeconds)
                            throw new TimeoutException(
                            "The app did not return a HTTP response within " +
                            (int)waitForHttpResponseClock.Elapsed.TotalSeconds +
                            " seconds.");

                        System.Threading.Thread.Sleep(100);
                    }


                });
        }

        static Newtonsoft.Json.JsonSerializerSettings jsonSerializerSettings = new Newtonsoft.Json.JsonSerializerSettings
        {
            DefaultValueHandling = Newtonsoft.Json.DefaultValueHandling.Ignore,
        };

        static async System.Threading.Tasks.Task<InterfaceToHost.HttpRequestEvent> AsPersistentProcessInterfaceHttpRequestEvent(
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

                request = await Asp.AsPersistentProcessInterfaceHttpRequest(httpContext.Request),
            };
        }
    }

    public class WebAppAndElmAppConfig
    {
        public WebAppConfigurationJsonStructure WebAppConfiguration;

        public Func<string, string> ProcessEventInElmApp;
    }
}
