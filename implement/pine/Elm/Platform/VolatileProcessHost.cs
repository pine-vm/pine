using ElmTime.ElmInteractive;
using Pine.Core;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Pine.Elm.Platform;

using CreateVolatileProcessResult =
    Result<WebServiceInterface.CreateVolatileProcessErrorStruct, WebServiceInterface.CreateVolatileProcessComplete>;

using RequestToVolatileProcessResult =
    Result<WebServiceInterface.RequestToVolatileProcessError, WebServiceInterface.RequestToVolatileProcessComplete>;

public class VolatileProcessHost(
    IReadOnlyList<PineValue> SourceCompositions)
    : IAsyncDisposable
{
    private int createVolatileProcessAttempts = 0;

    private readonly ConcurrentDictionary<string, VolatileProcess> volatileProcesses = new();

    private readonly ConcurrentQueue<WebServiceInterface.Command> commands = new();

    public IReadOnlyList<WebServiceInterface.Command> DequeueCommands() =>
        [.. commands.DequeueAllEnumerable()];

    private readonly ConcurrentBag<Task<TaskResult>> tasks = [];

    record TaskResult(
        ElmInteractiveEnvironment.FunctionRecord UpdateFunction,
        PineValue ResponseValue);

    public async Task ExchangeAsync(MutatingWebServiceApp webServiceApp)
    {
        var tasksSnapshot = tasks.ToArray();

        foreach (var task in tasks.TakeWhile(task => task.IsCompleted))
        {
            var taskResult = await task;

            webServiceApp.ApplyUpdate(
                taskResult.UpdateFunction,
                [taskResult.ResponseValue]);
        }

        foreach (var cmd in webServiceApp.DequeueCommands())
        {
            if (TaskForCmdAsync(cmd) is { } task)
            {
                tasks.Add(task);
            }
            else
            {
                commands.Enqueue(cmd);
            }
        }
    }

    private Task<TaskResult>? TaskForCmdAsync(WebServiceInterface.Command cmd)
    {
        if (cmd is WebServiceInterface.Command.CreateVolatileProcess createVolatileProcess)
        {
            return
                CreateTaskAsync(
                    updateFunction: createVolatileProcess.Create.Update,
                    responseDelegate:
                    () => PerformProcessTaskCreateVolatileProcessAndEncode(createVolatileProcess.Create));
        }

        if (cmd is WebServiceInterface.Command.RequestToVolatileProcess requestToVolatileProcess)
        {
            return
                CreateTaskAsync(
                    updateFunction: requestToVolatileProcess.Request.Update,
                    responseDelegate:
                    () => PerformProcessTaskRequestToVolatileProcessAndEncode(requestToVolatileProcess.Request));
        }

        if (cmd is WebServiceInterface.Command.TerminateVolatileProcess terminateVolatileProcess)
        {
            volatileProcesses.TryRemove(terminateVolatileProcess.ProcessId, out var volatileProcess);

            (volatileProcess as IDisposable)?.Dispose();
        }

        return null;
    }

    private static Task<TaskResult> CreateTaskAsync(
        ElmInteractiveEnvironment.FunctionRecord updateFunction,
        Func<PineValue> responseDelegate)
    {
        return Task.Run(
            () =>
            {
                var responseValue = responseDelegate();

                return new TaskResult(
                    UpdateFunction: updateFunction,
                    ResponseValue: responseValue);
            });
    }

    public PineValue PerformProcessTaskCreateVolatileProcessAndEncode(
        WebServiceInterface.CreateVolatileProcessStruct createVolatileProcess)
    {
        var result = PerformProcessTaskCreateVolatileProcess(createVolatileProcess);

        return WebServiceInterface.EncodeCreateVolatileProcessResult(result);
    }

    public CreateVolatileProcessResult PerformProcessTaskCreateVolatileProcess(
        WebServiceInterface.CreateVolatileProcessStruct createVolatileProcess)
    {
        try
        {
            var volatileProcess =
                new VolatileProcessCSharp(
                    GetBlobWithSHA256,
                    createVolatileProcess.ProgramCode,
                    scriptGlobals: null);

            var volatileProcessId =
                System.Threading.Interlocked.Increment(ref createVolatileProcessAttempts).ToString();

            volatileProcesses[volatileProcessId] = volatileProcess;

            return
                new WebServiceInterface.CreateVolatileProcessComplete(
                    ProcessId: volatileProcessId);
        }
        catch (Exception createVolatileProcessException)
        {
            return new WebServiceInterface.CreateVolatileProcessErrorStruct(
                ExceptionToString: createVolatileProcessException.ToString());
        }
    }

    public PineValue PerformProcessTaskRequestToVolatileProcessAndEncode(
        WebServiceInterface.RequestToVolatileProcessStruct requestToVolatileProcess)
    {
        var result = PerformProcessTaskRequestToVolatileProcess(requestToVolatileProcess);

        return WebServiceInterface.EncodeRequestToVolatileProcessResult(result);
    }

    public RequestToVolatileProcessResult PerformProcessTaskRequestToVolatileProcess(
        WebServiceInterface.RequestToVolatileProcessStruct requestToVolatileProcess)
    {
        volatileProcesses.TryGetValue(requestToVolatileProcess.ProcessId, out var volatileProcess);

        if (volatileProcess is null)
        {
            return new WebServiceInterface.RequestToVolatileProcessError.ProcessNotFound();
        }

        string? returnValueToString = null;
        string? exceptionToString = null;

        var clock = System.Diagnostics.Stopwatch.StartNew();

        try
        {
            switch (volatileProcess)
            {
                case VolatileProcessCSharp volatileProcessCSharp:

                    var returnValue =
                        volatileProcessCSharp.ProcessRequest(requestToVolatileProcess.Request);

                    exceptionToString = returnValue.Exception?.ToString();
                    returnValueToString = returnValue.ReturnValue?.ToString();

                    break;

                case VolatileProcessNative volatileProcessNative:
                    // TODO
                    break;

                default:
                    throw new NotImplementedException(
                        "Unsupported volatile process type: " + volatileProcess.GetType().Name);
            }
        }
        catch (Exception requestToVolatileProcessException)
        {
            exceptionToString = requestToVolatileProcessException.ToString();
        }

        clock.Stop();

        return
            new WebServiceInterface.RequestToVolatileProcessComplete(
                ReturnValueToString: returnValueToString,
                ExceptionToString: exceptionToString,
                DurationInMilliseconds: (int)clock.ElapsedMilliseconds);
    }

    private byte[]? GetBlobWithSHA256(byte[] sha256)
    {
        var matchFromSourceComposition =
            SourceCompositions
            .Select(sourceComposition => PineValueHashTree.FindNodeByHash(sourceComposition, sha256));

        if (matchFromSourceComposition is not null)
        {
            if (matchFromSourceComposition is not PineValue.BlobValue matchFromSourceCompositionBlobs)
            {
                throw new Exception(
                    CommonConversion.StringBase16FromByteArray(sha256) + " is not a blob");
            }

            return matchFromSourceCompositionBlobs.Bytes.ToArray();
        }

        return BlobLibrary.GetBlobWithSHA256(sha256)?.ToArray();
    }

    public async ValueTask DisposeAsync()
    {
        await
            Task.WhenAll(
                volatileProcesses.Select(kvp =>
                Task.Run(() =>
                {
                    (kvp.Value as IDisposable)?.Dispose();
                })));
    }
}
