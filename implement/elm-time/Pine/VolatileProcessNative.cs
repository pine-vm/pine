using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace Pine;

public class VolatileProcessNative : VolatileProcess, IDisposable
{
    public record ReadAllFromNativeProcess(
        ReadOnlyMemory<byte> StdOut,
        ReadOnlyMemory<byte> StdErr,
        int? ExitCode);

    private static readonly ConcurrentDictionary<string, IEnumerable<TreeNodeWithStringPath>> loadedTreesFromUrl = new();

    private readonly Process process;

    private readonly string containerDirectory;

    private readonly ConcurrentQueue<ReadOnlyMemory<byte>> stdOutQueue = new();

    private readonly ConcurrentQueue<ReadOnlyMemory<byte>> stdErrQueue = new();

    private readonly CancellationTokenSource disposedCancellationTokenSource = new();

    public VolatileProcessNative(
        Func<byte[], byte[]?>? getFileFromHashSHA256,
        ElmTime.Platform.WebService.InterfaceToHost.CreateVolatileProcessNativeStruct createRequest)
    {
        var executableFile =
            LoadBlob(
                loadedTreesFromUrl: loadedTreesFromUrl,
                getFileFromHashSHA256: getFileFromHashSHA256,
                hashSha256Base16: createRequest.executableFile.hashSha256Base16,
                hintUrls: createRequest.executableFile.hintUrls)
            .Extract(err => throw new Exception("Failed to load executable file: " + err));

        containerDirectory = Filesystem.CreateRandomDirectoryInTempDirectory();

        var executableFileNameAppendix = ExecutableFile.ExecutableFileNameAppendix;

        var mainExecutableFileName = "name-used-to-execute-file" + executableFileNameAppendix;
        var mainExecutableFilePathAbsolute = Path.Combine(containerDirectory, mainExecutableFileName);

        ExecutableFile.CreateAndWriteFileToPath(
            mainExecutableFilePathAbsolute,
            executableFile,
            makeExecutable: true);

        process = new Process
        {
            StartInfo = new ProcessStartInfo
            {
                WorkingDirectory = containerDirectory,
                FileName = mainExecutableFilePathAbsolute,
                Arguments = createRequest.arguments,
                UseShellExecute = false,
                RedirectStandardOutput = true,
                RedirectStandardError = true,
                RedirectStandardInput = true,
                CreateNoWindow = true,
            },
        };

        foreach (var envString in createRequest.environmentVariables)
            process.StartInfo.Environment[envString.key] = envString.value;

        process.Start();

        Task.Run(() => ReadFromStreamToQueueLoopAsync(
            stdOutQueue,
            process.StandardOutput.BaseStream,
            cancellationToken: disposedCancellationTokenSource.Token));

        Task.Run(() => ReadFromStreamToQueueLoopAsync(
            stdErrQueue,
            process.StandardError.BaseStream,
            cancellationToken: disposedCancellationTokenSource.Token));
    }

    public void WriteToStdIn(ReadOnlyMemory<byte> data)
    {
        process.StandardInput.BaseStream.Write(data.Span);

        process.StandardInput.BaseStream.Flush();
    }

    public ReadAllFromNativeProcess ReadAll()
    {
        var stdOut = CommonConversion.Concat(stdOutQueue.DequeueEnumerable().ToList());
        var stdErr = CommonConversion.Concat(stdErrQueue.DequeueEnumerable().ToList());

        var exitCode =
            process.HasExited ? (int?)process.ExitCode : null;

        return new ReadAllFromNativeProcess(StdOut: stdOut, StdErr: stdErr, ExitCode: exitCode);
    }

    public static ReadOnlyMemory<byte> ReadAllFromStream(Stream stream)
    {
        var buffers = new List<ReadOnlyMemory<byte>>();

        while (true)
        {
            var buffer = new byte[0x10_000];

            var bytesRead = stream.Read(buffer, offset: 0, buffer.Length);

            if (bytesRead == 0)
                break;

            buffers.Add(buffer);
        }

        return CommonConversion.Concat(buffers);
    }

    public static async Task<long> ReadFromStreamToQueueLoopAsync(
        ConcurrentQueue<ReadOnlyMemory<byte>> queue,
        Stream stream,
        CancellationToken cancellationToken)
    {
        long aggregateBytesRead = 0;

        try
        {
            while (!cancellationToken.IsCancellationRequested)
            {
                var buffer = new byte[0x100_000];

                var bytesRead =
                    await stream.ReadAtLeastAsync(
                        buffer,
                        minimumBytes: 1,
                        throwOnEndOfStream: true,
                        cancellationToken: cancellationToken);

                if (bytesRead == 0)
                    continue;

                Interlocked.Add(ref aggregateBytesRead, bytesRead);

                queue.Enqueue(buffer.AsMemory()[..bytesRead]);
            }
        }
        catch (EndOfStreamException)
        {
        }

        return aggregateBytesRead;
    }

    public void Dispose()
    {
        try
        {
            disposedCancellationTokenSource.Cancel();

            if (process is not null && !process.HasExited)
                process.Kill();

            if (containerDirectory is not null)
                Directory.Delete(containerDirectory, recursive: true);
        }
        catch { }
    }
}
