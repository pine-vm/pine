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

    private readonly ConcurrentDictionary<Task<TaskResult>, object?> tasks = [];

    record TaskResult(
        FunctionRecordValueAndParsed UpdateFunction,
        PineValue ResponseValue);

    public async Task<IReadOnlyList<ApplyUpdateReport<WebServiceInterface.Command>>> ExchangeAsync(
        MutatingWebServiceApp webServiceApp)
    {
        var tasksSnapshot = tasks.ToArray();

        var completedTasks =
            tasksSnapshot
            .Where(task => task.Key.IsCompleted)
            .ToArray();

        var updateReports =
            new List<ApplyUpdateReport<WebServiceInterface.Command>>();

        foreach (var task in completedTasks)
        {
            if (!tasks.TryRemove(task.Key, out _))
                continue;

            var taskResult = await task.Key;

            var updateReport =
                webServiceApp.ApplyUpdate(
                    taskResult.UpdateFunction,
                    [taskResult.ResponseValue]);

            updateReports.Add(updateReport);
        }

        foreach (var cmd in webServiceApp.DequeueCommands())
        {
            if (TaskForCmdAsync(cmd) is { } task)
            {
                tasks[task] = null;
            }
            else
            {
                commands.Enqueue(cmd);
            }
        }

        return updateReports;
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

        if (cmd is WebServiceInterface.Command.ReadRuntimeInformationCommand readRuntimeInformationCommand)
        {
            return
                CreateTaskAsync(
                    updateFunction: readRuntimeInformationCommand.Read.Update,
                    responseDelegate:
                    () => WebServiceInterface.EncodeReadRuntimeInformationResult(
                        ReadRuntimeInformation()));
        }

        if (cmd is WebServiceInterface.Command.CreateVolatileProcessNativeCommand createVolatileProcessNativeCommand)
        {
            return
                CreateTaskAsync(
                    updateFunction: createVolatileProcessNativeCommand.Create.Update,
                    responseDelegate:
                    () => PerformProcessTaskCreateVolatileProcessNativeAndEncode(
                        createVolatileProcessNativeCommand.Create.Request));
        }

        if (cmd is WebServiceInterface.Command.RequestToVolatileProcess requestToVolatileProcess)
        {
            return
                CreateTaskAsync(
                    updateFunction: requestToVolatileProcess.Request.Update,
                    responseDelegate:
                    () => PerformProcessTaskRequestToVolatileProcessAndEncode(requestToVolatileProcess.Request));
        }

        if (cmd is WebServiceInterface.Command.WriteToVolatileProcessNativeStdInCommand writeToVolatileProcessNativeStdInCommand)
        {
            return
                CreateTaskAsync(
                    updateFunction: writeToVolatileProcessNativeStdInCommand.Write.Update,
                    responseDelegate:
                    () => PerformWriteToVolatileProcessNativeStdInAndEncode(
                        writeToVolatileProcessNativeStdInCommand.Write));
        }

        if (cmd is WebServiceInterface.Command.ReadAllFromVolatileProcessNativeCommand readAllFromVolatileProcessNativeCommand)
        {
            return
                CreateTaskAsync(
                    updateFunction: readAllFromVolatileProcessNativeCommand.Read.Update,
                    responseDelegate:
                    () => PerformReadAllFromVolatileProcessNativeAndEncode(
                        readAllFromVolatileProcessNativeCommand.Read));
        }

        if (cmd is WebServiceInterface.Command.TerminateVolatileProcess terminateVolatileProcess)
        {
            volatileProcesses.TryRemove(terminateVolatileProcess.ProcessId, out var volatileProcess);

            (volatileProcess as IDisposable)?.Dispose();
        }

        return null;
    }

    public static WebServiceInterface.RuntimeInformationRecord ReadRuntimeInformation()
    {
        var osPlatform =
            new[]
            {
                System.Runtime.InteropServices.OSPlatform.Windows,
                System.Runtime.InteropServices.OSPlatform.Linux,
                System.Runtime.InteropServices.OSPlatform.OSX,
            }
            .Where(System.Runtime.InteropServices.RuntimeInformation.IsOSPlatform)
            .Cast<System.Runtime.InteropServices.OSPlatform?>()
            .FirstOrDefault()?.ToString();

        return new WebServiceInterface.RuntimeInformationRecord(
            RuntimeIdentifier:
            System.Runtime.InteropServices.RuntimeInformation.RuntimeIdentifier,
            OsPlatform:
            osPlatform);
    }

    private static Task<TaskResult> CreateTaskAsync(
        FunctionRecordValueAndParsed updateFunction,
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

    public PineValue PerformProcessTaskCreateVolatileProcessNativeAndEncode(
        WebServiceInterface.CreateVolatileProcessNativeRequestStruct createVolatileProcess)
    {
        var result =
            PerformProcessTaskCreateVolatileProcessNative(createVolatileProcess);

        return WebServiceInterface.EncodeCreateVolatileProcessResult(result);
    }

    public CreateVolatileProcessResult PerformProcessTaskCreateVolatileProcessNative(
        WebServiceInterface.CreateVolatileProcessNativeRequestStruct createVolatileProcess)
    {
        try
        {
            var volatileProcess =
                new VolatileProcessNative(
                    GetBlobWithSHA256,
                    createVolatileProcess);

            var volatileProcessId =
                System.Threading.Interlocked.Increment(ref createVolatileProcessAttempts).ToString();

            volatileProcesses[volatileProcessId] = volatileProcess;

            return
                new WebServiceInterface.CreateVolatileProcessComplete(ProcessId: volatileProcessId);
        }
        catch (Exception exception)
        {
            return
                new WebServiceInterface.CreateVolatileProcessErrorStruct(
                    ExceptionToString: exception.ToString());
        }
    }

    public PineValue PerformProcessTaskRequestToVolatileProcessAndEncode(
        WebServiceInterface.RequestToVolatileProcessStruct requestToVolatileProcess)
    {
        var result =
            PerformProcessTaskRequestToVolatileProcess(requestToVolatileProcess);

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

                    exceptionToString = "This request type is not supported for native processes";

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

    public PineValue PerformWriteToVolatileProcessNativeStdInAndEncode(
        WebServiceInterface.WriteToVolatileProcessNativeStdInStruct writeToVolatileProcessNativeStdIn)
    {
        var result =
            PerformWriteToVolatileProcessNativeStdIn(writeToVolatileProcessNativeStdIn);

        return WebServiceInterface.EncodeWriteToVolatileProcessNativeStdInResult(result);
    }

    public Result<WebServiceInterface.RequestToVolatileProcessError, object> PerformWriteToVolatileProcessNativeStdIn(
        WebServiceInterface.WriteToVolatileProcessNativeStdInStruct writeToVolatileProcessNativeStdIn)
    {
        volatileProcesses.TryGetValue(writeToVolatileProcessNativeStdIn.ProcessId, out var volatileProcess);

        if (volatileProcess is null)
        {
            return new WebServiceInterface.RequestToVolatileProcessError.ProcessNotFound();
        }

        if (volatileProcess is not VolatileProcessNative volatileProcessNative)
        {
            return
                new WebServiceInterface.RequestToVolatileProcessError.RequestToVolatileProcessOtherError(
                    "Process is not a native process");
        }

        try
        {
            volatileProcessNative.WriteToStdIn(writeToVolatileProcessNativeStdIn.StdIn);

            return new object();
        }
        catch (Exception writeToVolatileProcessNativeStdInException)
        {
            return
                new WebServiceInterface.RequestToVolatileProcessError.RequestToVolatileProcessOtherError(
                    writeToVolatileProcessNativeStdInException.ToString());
        }
    }

    public PineValue PerformReadAllFromVolatileProcessNativeAndEncode(
        WebServiceInterface.ReadAllFromVolatileProcessNativeStruct readAllFromVolatileProcessNative)
    {
        var result =
            PerformReadAllFromVolatileProcessNative(readAllFromVolatileProcessNative);

        return WebServiceInterface.EncodeEncodeReadAllFromVolatileProcessNativeResult(result);
    }

    public Result<WebServiceInterface.RequestToVolatileProcessError, WebServiceInterface.ReadAllFromVolatileProcessNativeSuccessStruct>
        PerformReadAllFromVolatileProcessNative(
        WebServiceInterface.ReadAllFromVolatileProcessNativeStruct readAllFromVolatileProcessNative)
    {
        volatileProcesses.TryGetValue(readAllFromVolatileProcessNative.ProcessId, out var volatileProcess);

        if (volatileProcess is null)
        {
            return new WebServiceInterface.RequestToVolatileProcessError.ProcessNotFound();
        }

        if (volatileProcess is not VolatileProcessNative volatileProcessNative)
        {
            return
                new WebServiceInterface.RequestToVolatileProcessError.RequestToVolatileProcessOtherError(
                    "Process is not a native process");
        }
        try
        {
            var read = volatileProcessNative.ReadAll();

            return
                new WebServiceInterface.ReadAllFromVolatileProcessNativeSuccessStruct(
                    StdOut: read.StdOut,
                    StdErr: read.StdErr,
                    ExitCode: read.ExitCode);
        }
        catch (Exception readAllFromVolatileProcessNativeException)
        {
            return
                new WebServiceInterface.RequestToVolatileProcessError.RequestToVolatileProcessOtherError(
                    readAllFromVolatileProcessNativeException.ToString());
        }
    }

    private byte[]? GetBlobWithSHA256(byte[] sha256)
    {
        var matchFromSourceComposition =
            SourceCompositions
            .Select(sourceComposition => PineValueHashTree.FindNodeByHash(sourceComposition, sha256))
            .WhereNotNull()
            .FirstOrDefault();

        if (matchFromSourceComposition is not null)
        {
            if (matchFromSourceComposition is not PineValue.BlobValue matchFromSourceCompositionBlobs)
            {
                throw new Exception(
                    Convert.ToHexStringLower(sha256) + " is not a blob");
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
