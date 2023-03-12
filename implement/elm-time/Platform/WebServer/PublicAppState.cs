using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using FluffySpoon.AspNet.LetsEncrypt;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Pine;

namespace ElmTime.Platform.WebServer;

public class PublicAppState
{
    static TimeSpan NotifyTimeHasArrivedMaximumDistance => TimeSpan.FromSeconds(10);

    long nextHttpRequestIndex = 0;

    int createVolatileProcessAttempts = 0;

    readonly ConcurrentDictionary<string, InterfaceToHost.HttpResponse> appTaskCompleteHttpResponse = new();

    readonly ConcurrentDictionary<string, VolatileProcess> volatileProcesses = new();

    readonly public System.Threading.CancellationTokenSource applicationStoppingCancellationTokenSource = new();

    readonly ServerAndElmAppConfig serverAndElmAppConfig;
    readonly Func<DateTimeOffset> getDateTimeOffset;

    readonly System.Threading.Timer notifyTimeHasArrivedTimer;

    readonly object nextTimeToNotifyLock = new();

    DateTimeOffset? lastAppEventTimeHasArrived = null;
    InterfaceToHost.NotifyWhenPosixTimeHasArrivedRequestStruct? nextTimeToNotify = null;

    public PublicAppState(
        ServerAndElmAppConfig serverAndElmAppConfig,
        Func<DateTimeOffset> getDateTimeOffset)
    {
        this.serverAndElmAppConfig = serverAndElmAppConfig;
        this.getDateTimeOffset = getDateTimeOffset;

        if (serverAndElmAppConfig.InitOrMigrateCmds != null)
            ForwardTasksFromResponseCmds(serverAndElmAppConfig.InitOrMigrateCmds);

        notifyTimeHasArrivedTimer = new System.Threading.Timer(
            callback: _ =>
            {
                if (applicationStoppingCancellationTokenSource.IsCancellationRequested)
                {
                    notifyTimeHasArrivedTimer?.Dispose();
                    return;
                }

                lock (nextTimeToNotifyLock)
                {
                    if (applicationStoppingCancellationTokenSource.IsCancellationRequested)
                    {
                        notifyTimeHasArrivedTimer?.Dispose();
                        return;
                    }

                    var localNextTimeToNotify = nextTimeToNotify;

                    if (localNextTimeToNotify != null && localNextTimeToNotify.minimumPosixTimeMilli <= getDateTimeOffset().ToUnixTimeMilliseconds())
                    {
                        nextTimeToNotify = null;
                        ProcessEventTimeHasArrived();
                        return;
                    }
                }

                if (!lastAppEventTimeHasArrived.HasValue ||
                    NotifyTimeHasArrivedMaximumDistance <= getDateTimeOffset() - lastAppEventTimeHasArrived.Value)
                {
                    ProcessEventTimeHasArrived();
                }
            },
            state: null,
            dueTime: TimeSpan.Zero,
            period: TimeSpan.FromMilliseconds(10));
    }

    public WebApplication Build(
        WebApplicationBuilder appBuilder,
        IHostEnvironment env,
        IReadOnlyList<string> publicWebHostUrls)
    {
        appBuilder.Services.AddLogging(logging =>
        {
            logging.AddConsole();
            logging.AddDebug();
        });

        using var loggerFactory = LoggerFactory.Create(logging =>
        {
            logging.AddConsole();
            logging.AddDebug();
        });

        var logger = loggerFactory.CreateLogger<PublicAppState>();

        var webHostBuilder =
            appBuilder.WebHost
            .ConfigureKestrel(kestrelOptions =>
            {
                kestrelOptions.ConfigureHttpsDefaults(httpsOptions =>
                {
                    httpsOptions.ServerCertificateSelector = (c, s) => LetsEncryptRenewalService.Certificate;
                });
            })
            .UseUrls(publicWebHostUrls.ToArray())
            .WithSettingDateTimeOffsetDelegate(getDateTimeOffset)
            .ConfigureServices(services => ConfigureServices(services, logger));

        var app = appBuilder.Build();

        if (env.IsDevelopment())
        {
            app.UseDeveloperExceptionPage();
        }

        if (serverAndElmAppConfig.ServerConfig?.letsEncryptOptions != null)
            app.UseFluffySpoonLetsEncryptChallengeApprovalMiddleware();

        app.Lifetime.ApplicationStopping.Register(() =>
        {
            applicationStoppingCancellationTokenSource.Cancel();
            app.Logger?.LogInformation("Public app noticed ApplicationStopping.");
        });

        app.Run(async context =>
        {
            await Asp.MiddlewareFromWebServerConfig(
                serverAndElmAppConfig.ServerConfig,
                context,
                () => HandleRequestAsync(context));
        });

        return app;
    }

    void ConfigureServices(
        IServiceCollection services,
        ILogger logger)
    {
        var letsEncryptOptions = serverAndElmAppConfig.ServerConfig?.letsEncryptOptions;

        if (letsEncryptOptions == null)
        {
            logger.LogInformation("I did not find 'letsEncryptOptions' in the configuration. I continue without Let's Encrypt.");
        }
        else
        {
            logger.LogInformation("I found 'letsEncryptOptions' in the configuration.");
            services.AddFluffySpoonLetsEncryptRenewalService(letsEncryptOptions);
            services.AddFluffySpoonLetsEncryptFileCertificatePersistence();
            services.AddFluffySpoonLetsEncryptMemoryChallengePersistence();
        }

        Asp.ConfigureServices(services);
    }

    async System.Threading.Tasks.Task HandleRequestAsync(HttpContext context)
    {
        var currentDateTime = getDateTimeOffset();
        var timeMilli = currentDateTime.ToUnixTimeMilliseconds();
        var httpRequestIndex = System.Threading.Interlocked.Increment(ref nextHttpRequestIndex);

        var httpRequestId = timeMilli.ToString() + "-" + httpRequestIndex.ToString();

        var httpRequestEvent =
            await AsPersistentProcessInterfaceHttpRequestEvent(context, httpRequestId, currentDateTime);

        var httpRequestInterfaceEvent = new InterfaceToHost.BackendEventStruct.HttpRequestEvent(httpRequestEvent);

        var preparedProcessEvent = PrepareProcessEventAndResultingRequests(httpRequestInterfaceEvent);

        if (serverAndElmAppConfig.ServerConfig?.httpRequestEventSizeLimit < preparedProcessEvent.serializedInterfaceEvent?.Length)
        {
            context.Response.StatusCode = StatusCodes.Status413RequestEntityTooLarge;
            await context.Response.WriteAsync("Request is too large.");
            return;
        }

        preparedProcessEvent.processEventAndResultingRequests();

        var waitForHttpResponseClock = System.Diagnostics.Stopwatch.StartNew();

        InterfaceToHost.HttpResponse? GetResponseFromAppOrTimeout(TimeSpan timeout)
        {
            if (appTaskCompleteHttpResponse.TryRemove(httpRequestId, out var httpResponse))
            {
                return httpResponse;
            }

            if (timeout <= waitForHttpResponseClock.Elapsed)
                return new InterfaceToHost.HttpResponse(
                    statusCode: 500,
                    bodyAsBase64: Maybe.NothingFromNull(Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                        "The app did not return an HTTP response within " +
                        (int)waitForHttpResponseClock.Elapsed.TotalSeconds +
                        " seconds."))),
                    headersToAdd: Array.Empty<InterfaceToHost.HttpHeader>());

            return null;
        }

        while (true)
        {
            var httpResponse = GetResponseFromAppOrTimeout(TimeSpan.FromSeconds(60));

            if (httpResponse is null)
            {
                await System.Threading.Tasks.Task.Delay(TimeSpan.FromMilliseconds(30));
                continue;
            }

            var headerContentType =
                httpResponse.headersToAdd
                ?.FirstOrDefault(header => header.name?.ToLowerInvariant() == "content-type")
                ?.values?.FirstOrDefault();

            context.Response.StatusCode = httpResponse.statusCode;

            foreach (var headerToAdd in httpResponse.headersToAdd.EmptyIfNull())
                context.Response.Headers[headerToAdd.name] = new Microsoft.Extensions.Primitives.StringValues(headerToAdd.values);

            if (headerContentType != null)
                context.Response.ContentType = headerContentType;

            ReadOnlyMemory<byte>? contentAsByteArray = null;

            if (httpResponse.bodyAsBase64.WithDefault(null) is string responseBodyAsBase64)
            {
                var buffer = new byte[responseBodyAsBase64.Length * 3 / 4];

                if (!Convert.TryFromBase64String(responseBodyAsBase64, buffer, out var bytesWritten))
                {
                    throw new FormatException(
                        "Failed to convert from base64. bytesWritten=" + bytesWritten +
                        ", input.length=" + responseBodyAsBase64.Length + ", input:\n" +
                        httpResponse.bodyAsBase64);
                }

                contentAsByteArray = buffer.AsMemory(0, bytesWritten);
            }

            context.Response.ContentLength = contentAsByteArray?.Length ?? 0;

            if (contentAsByteArray != null)
                await context.Response.Body.WriteAsync(contentAsByteArray.Value);

            break;
        }
    }

    public void ProcessEventTimeHasArrived()
    {
        var currentTime = getDateTimeOffset();

        lastAppEventTimeHasArrived = currentTime;

        ProcessEventAndResultingRequests(new InterfaceToHost.BackendEventStruct.PosixTimeHasArrivedEvent
        (new InterfaceToHost.PosixTimeHasArrivedEventStruct
            (
                posixTimeMilli: currentTime.ToUnixTimeMilliseconds()
            )
        ));
    }

    (string serializedInterfaceEvent, Action processEventAndResultingRequests) PrepareProcessEventAndResultingRequests(
        InterfaceToHost.BackendEventStruct interfaceEvent)
    {
        var serializedInterfaceEvent =
            new StateShim.InterfaceToHost.StateShimRequestStruct.ApplyFunctionShimRequest(
                new StateShim.InterfaceToHost.ApplyFunctionShimRequestStruct(
                    functionName: "processEvent",
                    arguments: new StateShim.InterfaceToHost.ApplyFunctionArguments<Maybe<StateShim.InterfaceToHost.StateSource>>(
                        stateArgument: Maybe<StateShim.InterfaceToHost.StateSource>.just(new StateShim.InterfaceToHost.StateSource.BranchStateSource("main")),
                        serializedArgumentsJson: ImmutableList.Create(System.Text.Json.JsonSerializer.Serialize(interfaceEvent))),
                    stateDestinationBranches: ImmutableList.Create("main")))
            .SerializeToJsonString();

        var processEvent = new Action(() =>
        {
            if (applicationStoppingCancellationTokenSource.IsCancellationRequested)
                return;

            try
            {
                var serializedResponse = serverAndElmAppConfig.ProcessEventInElmApp(serializedInterfaceEvent);

                try
                {
                    var stateShimResponse =
                        System.Text.Json.JsonSerializer.Deserialize<Result<string, StateShim.InterfaceToHost.StateShimResponseStruct>>(
                            serializedResponse)!;

                    var backendEventResponseSerial =
                        stateShimResponse
                        .AndThen(decodeOk => decodeOk switch
                        {
                            StateShim.InterfaceToHost.StateShimResponseStruct.ApplyFunctionShimResponse applyFunctionResponse =>
                            applyFunctionResponse.Result,

                            _ =>
                            Result<string, StateShim.InterfaceToHost.FunctionApplicationResult>.err(
                                "Unexpected type of response: " + System.Text.Json.JsonSerializer.Serialize(decodeOk))
                        })
                        .AndThen(applyFunctionOk => applyFunctionOk.resultLessStateJson.ToResult("Apply function response is missing resultLessStateJson"))
                        .Extract(err => throw new Exception("Hosted app failed to decode the event: " + err));

                    var backendEventResponse =
                    System.Text.Json.JsonSerializer.Deserialize<InterfaceToHost.BackendEventResponseStruct>(backendEventResponseSerial);

                    var notifyWhenPosixTimeHasArrived = backendEventResponse.notifyWhenPosixTimeHasArrived.WithDefault(null);

                    if (notifyWhenPosixTimeHasArrived is not null)
                    {
                        System.Threading.Tasks.Task.Run(() =>
                        {
                            lock (nextTimeToNotifyLock)
                            {
                                nextTimeToNotify = notifyWhenPosixTimeHasArrived;
                            }
                        });
                    }

                    ForwardTasksFromResponseCmds(backendEventResponse);
                }
                catch (Exception parseException)
                {
                    throw new Exception(
                        "Failed to parse event response from app. Looks like the loaded elm app is not compatible with the interface.\nResponse from app follows:\n" + serializedResponse,
                        parseException);
                }
            }
            catch (Exception) when (applicationStoppingCancellationTokenSource.IsCancellationRequested)
            {
                return;
            }
        });

        return (serializedInterfaceEvent, processEvent);
    }

    void PerformProcessTaskAndFeedbackEvent(InterfaceToHost.StartTask taskWithId)
    {
        var taskResult = PerformProcessTask(taskWithId.task);

        var interfaceEvent = new InterfaceToHost.BackendEventStruct.TaskCompleteEvent
            (
                Result: new InterfaceToHost.ResultFromTaskWithId
                (
                    taskId: taskWithId.taskId,
                    taskResult: taskResult
                )
            );

        ProcessEventAndResultingRequests(interfaceEvent);
    }

    void ProcessEventAndResultingRequests(InterfaceToHost.BackendEventStruct interfaceEvent)
    {
        var prepareProcessEvent = PrepareProcessEventAndResultingRequests(interfaceEvent);

        prepareProcessEvent.processEventAndResultingRequests();
    }

    System.Threading.Tasks.Task ForwardTasksFromResponseCmds(InterfaceToHost.BackendEventResponseStruct response)
    {
        var startTasks =
            response.startTasks
            .Select(startTask => System.Threading.Tasks.Task.Run(() => PerformProcessTaskAndFeedbackEvent(startTask), applicationStoppingCancellationTokenSource.Token));

        foreach (var completeHttpResponse in response.completeHttpResponses)
        {
            appTaskCompleteHttpResponse[completeHttpResponse.httpRequestId] = completeHttpResponse.response;
        }

        return System.Threading.Tasks.Task.WhenAll(startTasks);
    }

    InterfaceToHost.TaskResult PerformProcessTask(InterfaceToHost.Task task) =>
        task switch
        {
            InterfaceToHost.Task.CreateVolatileProcess create =>
            PerformProcessTaskCreateVolatileProcess(create.Create),

            InterfaceToHost.Task.RequestToVolatileProcess requestTo =>
            new InterfaceToHost.TaskResult.RequestToVolatileProcessResponse(PerformProcessTaskRequestToVolatileProcess(requestTo.RequestTo)),

            InterfaceToHost.Task.TerminateVolatileProcess terminate => PerformProcessTaskTerminateVolatileProcess(terminate.Terminate),

            _ => throw new NotImplementedException("Unexpected task structure.")
        };
    byte[]? GetBlobWithSHA256(byte[] sha256)
    {
        var matchFromSourceComposition =
            serverAndElmAppConfig?.SourceComposition == null ? null :
            Composition.FindComponentByHash(serverAndElmAppConfig.SourceComposition, sha256);

        if (matchFromSourceComposition != null)
        {
            if (matchFromSourceComposition is not PineValue.BlobValue matchFromSourceCompositionBlobs)
                throw new Exception(CommonConversion.StringBase16FromByteArray(sha256) + " is not a blob");

            return matchFromSourceCompositionBlobs.Bytes.ToArray();
        }

        return BlobLibrary.GetBlobWithSHA256(sha256)?.ToArray();
    }

    InterfaceToHost.TaskResult PerformProcessTaskCreateVolatileProcess(InterfaceToHost.CreateVolatileProcessStruct createVolatileProcess)
    {
        try
        {
            var volatileProcess = new VolatileProcess(GetBlobWithSHA256, createVolatileProcess.programCode);

            var volatileProcessId = System.Threading.Interlocked.Increment(ref createVolatileProcessAttempts).ToString();

            volatileProcesses[volatileProcessId] = volatileProcess;

            var completeStructure = new InterfaceToHost.CreateVolatileProcessComplete
            (
                processId: volatileProcessId
            );

            return new InterfaceToHost.TaskResult.CreateVolatileProcessResponse
            (
                Result: Result<InterfaceToHost.CreateVolatileProcessErrorStructure, InterfaceToHost.CreateVolatileProcessComplete>.ok(completeStructure)
            );
        }
        catch (Exception createVolatileProcessException)
        {
            return new InterfaceToHost.TaskResult.CreateVolatileProcessResponse
            (
                Result: Result<InterfaceToHost.CreateVolatileProcessErrorStructure, InterfaceToHost.CreateVolatileProcessComplete>.err
                (
                    new InterfaceToHost.CreateVolatileProcessErrorStructure
                    (
                        exceptionToString: createVolatileProcessException.ToString()
                    )
                )
            );
        }
    }

    Result<InterfaceToHost.RequestToVolatileProcessError, InterfaceToHost.RequestToVolatileProcessComplete>
        PerformProcessTaskRequestToVolatileProcess(
        InterfaceToHost.RequestToVolatileProcessStruct requestToVolatileProcess)
    {
        if (!volatileProcesses.TryGetValue(requestToVolatileProcess.processId, out var volatileProcess))
        {
            return Result<InterfaceToHost.RequestToVolatileProcessError, InterfaceToHost.RequestToVolatileProcessComplete>.err
            (
                new InterfaceToHost.RequestToVolatileProcessError
                (
                    ProcessNotFound: new object()
                )
            );
        }

        var stopwatch = System.Diagnostics.Stopwatch.StartNew();

        var fromVolatileProcessResult = volatileProcess.ProcessRequest(requestToVolatileProcess.request);

        stopwatch.Stop();

        return Result<InterfaceToHost.RequestToVolatileProcessError, InterfaceToHost.RequestToVolatileProcessComplete>.ok
        (
            new InterfaceToHost.RequestToVolatileProcessComplete
            (
                exceptionToString: Maybe.NothingFromNull(fromVolatileProcessResult.Exception?.ToString()),
                returnValueToString: Maybe.NothingFromNull(fromVolatileProcessResult.ReturnValue?.ToString()),
                durationInMilliseconds: stopwatch.ElapsedMilliseconds
            )
        );
    }

    InterfaceToHost.TaskResult PerformProcessTaskTerminateVolatileProcess(InterfaceToHost.TerminateVolatileProcessStruct terminateVolatileProcess)
    {
        volatileProcesses.TryRemove(terminateVolatileProcess.processId, out var volatileProcess);

        return new InterfaceToHost.TaskResult.CompleteWithoutResult();
    }

    static async System.Threading.Tasks.Task<InterfaceToHost.HttpRequestEventStruct> AsPersistentProcessInterfaceHttpRequestEvent(
        HttpContext httpContext,
        string httpRequestId,
        DateTimeOffset time)
    {
        return new InterfaceToHost.HttpRequestEventStruct
        (
            posixTimeMilli: time.ToUnixTimeMilliseconds(),
            httpRequestId: httpRequestId,
            requestContext: new InterfaceToHost.HttpRequestContext
            (
                clientAddress: Maybe.NothingFromNull(httpContext.Connection.RemoteIpAddress?.ToString())
            ),

            request: await Asp.AsPersistentProcessInterfaceHttpRequest(httpContext.Request)
        );
    }
}

public record ServerAndElmAppConfig(
    WebServerConfigJson? ServerConfig,
    Func<string, string> ProcessEventInElmApp,
    PineValue SourceComposition,
    InterfaceToHost.BackendEventResponseStruct? InitOrMigrateCmds);
