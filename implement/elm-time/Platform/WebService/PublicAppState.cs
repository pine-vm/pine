using FluffySpoon.AspNet.LetsEncrypt;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Pine;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;

namespace ElmTime.Platform.WebService;

public class PublicAppState
{
    private static TimeSpan NotifyTimeHasArrivedMaximumDistance => TimeSpan.FromSeconds(10);

    private long nextHttpRequestIndex = 0;

    private int createVolatileProcessAttempts = 0;

    private readonly ConcurrentDictionary<string, InterfaceToHost.HttpResponse> appTaskCompleteHttpResponse = new();

    private readonly ConcurrentDictionary<string, VolatileProcess> volatileProcesses = new();

    public readonly System.Threading.CancellationTokenSource applicationStoppingCancellationTokenSource = new();

    private readonly ServerAndElmAppConfig serverAndElmAppConfig;
    private readonly Func<DateTimeOffset> getDateTimeOffset;

    private readonly System.Threading.Timer notifyTimeHasArrivedTimer;

    private readonly object nextTimeToNotifyLock = new();

    private DateTimeOffset? lastAppEventTimeHasArrived = null;
    private InterfaceToHost.NotifyWhenPosixTimeHasArrivedRequestStruct? nextTimeToNotify = null;

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

        appBuilder.Services.AddResponseCompression(options =>
        {
            options.EnableForHttps = true;
        });

        var webHostBuilder =
            appBuilder.WebHost
            .ConfigureKestrel(kestrelOptions =>
            {
                kestrelOptions.ConfigureHttpsDefaults(httpsOptions =>
                {
                    httpsOptions.ServerCertificateSelector = (_, _) => LetsEncryptRenewalService.Certificate;
                });
            })
            .UseUrls([.. publicWebHostUrls])
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
            DisposeAsync().Wait();
        });

        app.UseResponseCompression();

        app.Run(async context =>
        {
            await Asp.MiddlewareFromWebServiceConfig(
                serverAndElmAppConfig.ServerConfig,
                context,
                () => HandleRequestAsync(context));
        });

        return app;
    }

    async System.Threading.Tasks.Task DisposeAsync()
    {
        await
            System.Threading.Tasks.Task.WhenAll(
                volatileProcesses.Select(kvp =>
                System.Threading.Tasks.Task.Run(() =>
                {
                    (kvp.Value as IDisposable)?.Dispose();
                })));
    }

    private void ConfigureServices(
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

    private async System.Threading.Tasks.Task HandleRequestAsync(HttpContext context)
    {
        var currentDateTime = getDateTimeOffset();
        var timeMilli = currentDateTime.ToUnixTimeMilliseconds();
        var httpRequestIndex = System.Threading.Interlocked.Increment(ref nextHttpRequestIndex);

        var httpRequestId = timeMilli + "-" + httpRequestIndex;

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

            context.Response.Headers.XPoweredBy = "Elm-Time";

            ReadOnlyMemory<byte>? contentAsByteArray = null;

            if (httpResponse.bodyAsBase64.WithDefault(null) is { } responseBodyAsBase64)
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

    private (string serializedInterfaceEvent, Action processEventAndResultingRequests) PrepareProcessEventAndResultingRequests(
        InterfaceToHost.BackendEventStruct appEvent)
    {
        var serializedAppEvent = System.Text.Json.JsonSerializer.Serialize(appEvent);

        var processEvent = new Action(() =>
        {
            if (applicationStoppingCancellationTokenSource.IsCancellationRequested)
                return;

            try
            {
                var processEventResponse = serverAndElmAppConfig.ProcessEventInElmApp(serializedAppEvent);

                var backendEventResponseJson =
                    processEventResponse
                    .AndThen(applyFunctionOk => applyFunctionOk.resultLessStateJson.ToResult("Apply function response is missing resultLessStateJson"))
                    .Extract(err => throw new Exception("Failed to process event in Elm app: " + err));

                var backendEventResponse =
                System.Text.Json.JsonSerializer.Deserialize<InterfaceToHost.BackendEventResponseStruct>(backendEventResponseJson);

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
            catch (Exception) when (applicationStoppingCancellationTokenSource.IsCancellationRequested)
            {
            }
        });

        return (serializedAppEvent, processEvent);
    }

    private void PerformProcessTaskAndFeedbackEvent(InterfaceToHost.StartTask taskWithId)
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

    private void ProcessEventAndResultingRequests(InterfaceToHost.BackendEventStruct interfaceEvent)
    {
        var prepareProcessEvent = PrepareProcessEventAndResultingRequests(interfaceEvent);

        prepareProcessEvent.processEventAndResultingRequests();
    }

    private System.Threading.Tasks.Task ForwardTasksFromResponseCmds(InterfaceToHost.BackendEventResponseStruct response)
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

    private InterfaceToHost.TaskResult PerformProcessTask(InterfaceToHost.Task task) =>
        task switch
        {
            InterfaceToHost.Task.ReadRuntimeInformationTask =>
            new InterfaceToHost.TaskResult.ReadRuntimeInformationResponse(
                ReadRuntimeInformation()),

            InterfaceToHost.Task.CreateVolatileProcess create =>
            PerformProcessTaskCreateVolatileProcess(create.Create),

            InterfaceToHost.Task.CreateVolatileProcessNativeTask create =>
            new InterfaceToHost.TaskResult.CreateVolatileProcessResponse(
                CreateVolatileProcessNative(create.Create)),

            InterfaceToHost.Task.RequestToVolatileProcess requestTo =>
            new InterfaceToHost.TaskResult.RequestToVolatileProcessResponse(
                PerformProcessTaskRequestToVolatileProcess(requestTo.RequestTo)),

            InterfaceToHost.Task.WriteToVolatileProcessNativeStdInTask requestTo =>
            new InterfaceToHost.TaskResult.WriteToVolatileProcessNativeStdInTaskResponse(
                WriteToVolatileProcessStdIn(requestTo.WriteToProcess)),

            InterfaceToHost.Task.ReadAllFromVolatileProcessNativeTask request =>
            new InterfaceToHost.TaskResult.ReadAllFromVolatileProcessNativeTaskResponse(
                ReadAllFromVolatileProcessNative(request)),

            InterfaceToHost.Task.TerminateVolatileProcess terminate =>
            PerformProcessTaskTerminateVolatileProcess(terminate.Terminate),

            _ =>
            throw new NotImplementedException("Unexpected task type: " + task?.GetType().FullName)
        };

    private byte[]? GetBlobWithSHA256(byte[] sha256)
    {
        var matchFromSourceComposition =
            serverAndElmAppConfig?.SourceComposition is null
            ?
            null
            :
            PineValueHashTree.FindNodeByHash(serverAndElmAppConfig.SourceComposition, sha256);

        if (matchFromSourceComposition is not null)
        {
            if (matchFromSourceComposition is not PineValue.BlobValue matchFromSourceCompositionBlobs)
                throw new Exception(CommonConversion.StringBase16FromByteArray(sha256) + " is not a blob");

            return matchFromSourceCompositionBlobs.Bytes.ToArray();
        }

        return BlobLibrary.GetBlobWithSHA256(sha256)?.ToArray();
    }

    private static Result<string, InterfaceToHost.RuntimeInformationRecord> ReadRuntimeInformation()
    {
        try
        {
            var osPlatform =
                Maybe.NothingFromNull(
                    new[]
                    {
                        System.Runtime.InteropServices.OSPlatform.Windows,
                        System.Runtime.InteropServices.OSPlatform.Linux,
                        System.Runtime.InteropServices.OSPlatform.OSX,
                    }
                    .Where(System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform)
                    .Cast<System.Runtime.InteropServices.OSPlatform?>()
                    .FirstOrDefault()?.ToString());

            return
                Result<string, InterfaceToHost.RuntimeInformationRecord>.ok(
                    new InterfaceToHost.RuntimeInformationRecord(
                        runtimeIdentifier: System.Runtime.InteropServices.RuntimeInformation.RuntimeIdentifier,
                        osPlatform: osPlatform));
        }
        catch (Exception exception)
        {
            return
                Result<string, InterfaceToHost.RuntimeInformationRecord>.err(exception.ToString());
        }
    }

    private InterfaceToHost.TaskResult PerformProcessTaskCreateVolatileProcess(
        InterfaceToHost.CreateVolatileProcessStruct createVolatileProcess)
    {
        try
        {
            var volatileProcess = new VolatileProcessCSharp(GetBlobWithSHA256, createVolatileProcess.programCode, scriptGlobals: null);

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

    private Result<InterfaceToHost.CreateVolatileProcessErrorStructure, InterfaceToHost.CreateVolatileProcessComplete>
        CreateVolatileProcessNative(
        InterfaceToHost.CreateVolatileProcessNativeStruct createVolatileProcess)
    {
        try
        {
            var volatileProcess = new VolatileProcessNative(GetBlobWithSHA256, createVolatileProcess);

            var volatileProcessId = System.Threading.Interlocked.Increment(ref createVolatileProcessAttempts).ToString();

            volatileProcesses[volatileProcessId] = volatileProcess;

            var completeStructure = new InterfaceToHost.CreateVolatileProcessComplete
            (
                processId: volatileProcessId
            );

            return
                Result<InterfaceToHost.CreateVolatileProcessErrorStructure, InterfaceToHost.CreateVolatileProcessComplete>.ok(completeStructure);
        }
        catch (Exception exception)
        {
            return
                Result<InterfaceToHost.CreateVolatileProcessErrorStructure, InterfaceToHost.CreateVolatileProcessComplete>.err
                (
                    new InterfaceToHost.CreateVolatileProcessErrorStructure
                    (
                        exceptionToString: exception.ToString()
                    )
                );
        }
    }

    private Result<InterfaceToHost.RequestToVolatileProcessError, InterfaceToHost.RequestToVolatileProcessComplete>
        PerformProcessTaskRequestToVolatileProcess(
        InterfaceToHost.RequestToVolatileProcessStruct requestToVolatileProcess)
    {
        if (!volatileProcesses.TryGetValue(requestToVolatileProcess.processId, out var volatileProcess))
        {
            return Result<InterfaceToHost.RequestToVolatileProcessError, InterfaceToHost.RequestToVolatileProcessComplete>.err
            (
                new InterfaceToHost.RequestToVolatileProcessError
                (
                    ProcessNotFound: new object(),
                    RequestToVolatileProcessOtherError: null
                )
            );
        }

        if (volatileProcess is not VolatileProcessCSharp { } volatileProcessCSharp)
            return Result<InterfaceToHost.RequestToVolatileProcessError, InterfaceToHost.RequestToVolatileProcessComplete>.err
            (
                new InterfaceToHost.RequestToVolatileProcessError
                (
                    ProcessNotFound: null,
                    RequestToVolatileProcessOtherError:
                    "Process " + requestToVolatileProcess.processId + " is not a C# process: " + volatileProcess.GetType().FullName
                )
            );

        var stopwatch = System.Diagnostics.Stopwatch.StartNew();

        var fromVolatileProcessResult = volatileProcessCSharp.ProcessRequest(requestToVolatileProcess.request);

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

    private Result<InterfaceToHost.RequestToVolatileProcessError, object>
        WriteToVolatileProcessStdIn(
        InterfaceToHost.WriteToVolatileProcessNativeStdInStruct writeToVolatileProcess)
    {
        if (!volatileProcesses.TryGetValue(writeToVolatileProcess.processId, out var volatileProcess))
        {
            return Result<InterfaceToHost.RequestToVolatileProcessError, object>.err
            (
                new InterfaceToHost.RequestToVolatileProcessError
                (
                    ProcessNotFound: new object(),
                    RequestToVolatileProcessOtherError: null
                )
            );
        }

        if (volatileProcess is not VolatileProcessNative { } volatileProcessNative)
            return Result<InterfaceToHost.RequestToVolatileProcessError, object>.err
            (
                new InterfaceToHost.RequestToVolatileProcessError
                (
                    ProcessNotFound: null,
                    RequestToVolatileProcessOtherError:
                    "Process " + writeToVolatileProcess.processId + " is not a native process: " + volatileProcess.GetType().FullName
                )
            );

        byte[] inputBytes;

        try
        {
            inputBytes = Convert.FromBase64String(writeToVolatileProcess.stdInBase64);
        }
        catch (Exception exception)
        {
            return Result<InterfaceToHost.RequestToVolatileProcessError, object>.err
                (
                new InterfaceToHost.RequestToVolatileProcessError
                (
                    ProcessNotFound: null,
                    RequestToVolatileProcessOtherError:
                    "Could not convert stdInBase64 to bytes: " + exception
                    )
                );
        }

        volatileProcessNative.WriteToStdIn(inputBytes);

        return Result<InterfaceToHost.RequestToVolatileProcessError, object>.ok(new object());
    }

    private Result<InterfaceToHost.RequestToVolatileProcessError, InterfaceToHost.ReadAllFromVolatileProcessNativeSuccessStruct>
        ReadAllFromVolatileProcessNative(
        InterfaceToHost.Task.ReadAllFromVolatileProcessNativeTask request)
    {
        if (!volatileProcesses.TryGetValue(request.ProcessId, out var volatileProcess))
        {
            return Result<InterfaceToHost.RequestToVolatileProcessError, InterfaceToHost.ReadAllFromVolatileProcessNativeSuccessStruct>.err
            (
                new InterfaceToHost.RequestToVolatileProcessError
                (
                    ProcessNotFound: new object(),
                    RequestToVolatileProcessOtherError: null
                )
            );
        }

        if (volatileProcess is not VolatileProcessNative { } volatileProcessNative)
            return Result<InterfaceToHost.RequestToVolatileProcessError, InterfaceToHost.ReadAllFromVolatileProcessNativeSuccessStruct>.err
            (
                new InterfaceToHost.RequestToVolatileProcessError
                (
                    ProcessNotFound: null,
                    RequestToVolatileProcessOtherError:
                    "Process " + request.ProcessId + " is not a native process: " + volatileProcess.GetType().FullName
                )
            );

        var read = volatileProcessNative.ReadAll();

        return Result<InterfaceToHost.RequestToVolatileProcessError, InterfaceToHost.ReadAllFromVolatileProcessNativeSuccessStruct>.ok(
            new InterfaceToHost.ReadAllFromVolatileProcessNativeSuccessStruct(
                stdOutBase64: Convert.ToBase64String(read.StdOut.Span),
                stdErrBase64: Convert.ToBase64String(read.StdErr.Span),
                exitCode: Maybe.NothingFromNull(read.ExitCode)));
    }

    private InterfaceToHost.TaskResult PerformProcessTaskTerminateVolatileProcess(
        InterfaceToHost.TerminateVolatileProcessStruct terminateVolatileProcess)
    {
        volatileProcesses.TryRemove(terminateVolatileProcess.processId, out var volatileProcess);

        (volatileProcess as IDisposable)?.Dispose();

        return new InterfaceToHost.TaskResult.CompleteWithoutResult();
    }

    private static async System.Threading.Tasks.Task<InterfaceToHost.HttpRequestEventStruct> AsPersistentProcessInterfaceHttpRequestEvent(
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
    WebServiceConfigJson? ServerConfig,
    Func<string, Result<string, StateShim.InterfaceToHost.FunctionApplicationResult>> ProcessEventInElmApp,
    PineValue SourceComposition,
    InterfaceToHost.BackendEventResponseStruct? InitOrMigrateCmds);
