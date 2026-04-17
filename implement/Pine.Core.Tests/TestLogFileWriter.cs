using Microsoft.Win32.SafeHandles;
using System;
using System.Collections.Generic;
using System.IO;
using System.Runtime.CompilerServices;
using System.Runtime.InteropServices;
using System.Text;
using System.Threading;

namespace Pine.Core.Tests;

/// <summary>
/// Writes a log file containing the full terminal output of every <c>dotnet run</c>
/// invocation of this test project, so coding agents (and humans) can inspect the
/// output later without re-running the expensive test binary.
/// <para>
/// The log file path is <c>artifacts/test-logs/&lt;AssemblyName&gt;/&lt;timestamp&gt;_{full|filtered}.log</c>
/// relative to the current working directory. The first lines of the file record the
/// command line that was used. After the test run completes, the last line of both
/// the terminal output and the log file itself is <c>Test log written to: &lt;path&gt;</c>.
/// </para>
/// <para>
/// The writer is installed via a module initializer so it captures output from the
/// moment the assembly is loaded. On Linux/macOS it tees the full OS-level stdout and
/// stderr streams (so output written directly through <c>Console.OpenStandardOutput</c>
/// by <c>Microsoft.Testing.Platform</c> is captured too). On Windows it does the
/// equivalent via <c>CreatePipe</c> / <c>SetStdHandle</c>. On any other platform it
/// falls back to teeing <see cref="Console.Out"/> and <see cref="Console.Error"/> only.
/// </para>
/// </summary>
internal static class TestLogFileWriter
{
    [ModuleInitializer]
    internal static void Initialize()
    {
        try
        {
            Install();
        }
        catch
        {
            // Never let logging failures break the test run.
        }
    }

    private static void Install()
    {
        var commandLineArgs = Environment.GetCommandLineArgs();
        var mode = HasFilterArgument(commandLineArgs) ? "filtered" : "full";

        var assemblyName =
            typeof(TestLogFileWriter).Assembly.GetName().Name ?? "UnknownTestProject";

        var timestamp = DateTime.UtcNow.ToString("yyyy-MM-ddTHH-mm-ss");

        var logDirectory =
            Path.Combine(Directory.GetCurrentDirectory(), "artifacts", "test-logs", assemblyName);

        Directory.CreateDirectory(logDirectory);

        var logFilePath = Path.Combine(logDirectory, $"{timestamp}_{mode}.log");

        var logFileStream =
            new FileStream(logFilePath, FileMode.Create, FileAccess.Write, FileShare.Read);

        Console.WriteLine("Test log file: " + logFilePath);

        var header = new StringBuilder();
        header.AppendLine("Command line: " + FormatCommandLine(commandLineArgs));
        header.AppendLine("Mode: " + mode);
        header.AppendLine("Started (UTC): " + DateTime.UtcNow.ToString("o"));
        header.AppendLine("Working directory: " + Directory.GetCurrentDirectory());
        header.AppendLine(new string('-', 80));

        var headerBytes = Encoding.UTF8.GetBytes(header.ToString());
        logFileStream.Write(headerBytes, 0, headerBytes.Length);
        logFileStream.Flush();

        if (RuntimeInformation.IsOSPlatform(OSPlatform.Linux)
            || RuntimeInformation.IsOSPlatform(OSPlatform.OSX))
        {
            try
            {
                InstallUnixFdRedirect(logFileStream, logFilePath);
                return;
            }
            catch
            {
                // Fall through to the best-effort Console.SetOut tee below.
            }
        }

        if (RuntimeInformation.IsOSPlatform(OSPlatform.Windows))
        {
            try
            {
                InstallWindowsHandleRedirect(logFileStream, logFilePath);
                return;
            }
            catch
            {
                // Fall through to the best-effort Console.SetOut tee below.
            }
        }

        InstallConsoleOnlyTee(logFileStream, logFilePath);
    }

    /// <summary>
    /// On Unix, replace file descriptors 1 (stdout) and 2 (stderr) with the write
    /// ends of pipes. Spawn two background threads that read from the pipes and
    /// forward every byte both to the original terminal and to the log file.
    /// </summary>
    private static void InstallUnixFdRedirect(FileStream logFileStream, string logFilePath)
    {
        const int STDOUT_FILENO = 1;
        const int STDERR_FILENO = 2;

        var savedStdoutFd = Unix.dup(STDOUT_FILENO);

        if (savedStdoutFd < 0)
        {
            throw new InvalidOperationException("dup(stdout) failed: " + Marshal.GetLastPInvokeError());
        }

        var savedStderrFd = Unix.dup(STDERR_FILENO);

        if (savedStderrFd < 0)
        {
            throw new InvalidOperationException("dup(stderr) failed: " + Marshal.GetLastPInvokeError());
        }

        var outPipe = new int[2];
        var errPipe = new int[2];

        if (Unix.pipe(outPipe) != 0)
        {
            throw new InvalidOperationException("pipe() failed: " + Marshal.GetLastPInvokeError());
        }

        if (Unix.pipe(errPipe) != 0)
        {
            throw new InvalidOperationException("pipe() failed: " + Marshal.GetLastPInvokeError());
        }

        if (Unix.dup2(outPipe[1], STDOUT_FILENO) < 0)
        {
            throw new InvalidOperationException("dup2(stdout) failed: " + Marshal.GetLastPInvokeError());
        }

        if (Unix.dup2(errPipe[1], STDERR_FILENO) < 0)
        {
            throw new InvalidOperationException("dup2(stderr) failed: " + Marshal.GetLastPInvokeError());
        }

        Unix.close(outPipe[1]);
        Unix.close(errPipe[1]);

        var savedStdoutStream =
            new FileStream(new SafeFileHandle(new IntPtr(savedStdoutFd), ownsHandle: true), FileAccess.Write);

        var savedStderrStream =
            new FileStream(new SafeFileHandle(new IntPtr(savedStderrFd), ownsHandle: true), FileAccess.Write);

        // Force .NET to (re)initialize Console.Out and Console.Error against the (now redirected)
        // standard file descriptors, in case Console.Out was created before our module initializer ran.
        try
        {
            Console.SetOut(new StreamWriter(Console.OpenStandardOutput()) { AutoFlush = true });
            Console.SetError(new StreamWriter(Console.OpenStandardError()) { AutoFlush = true });
        }
        catch
        {
            // Best-effort.
        }

        var logFileLock = new object();

        var outThread =
            new Thread(() => PumpPipe(outPipe[0], savedStdoutStream, logFileStream, logFileLock))
            {
                IsBackground = true,
                Name = "TestLogFileWriter.stdout-pump",
            };

        var errThread =
            new Thread(() => PumpPipe(errPipe[0], savedStderrStream, logFileStream, logFileLock))
            {
                IsBackground = true,
                Name = "TestLogFileWriter.stderr-pump",
            };

        outThread.Start();
        errThread.Start();

        AppDomain.CurrentDomain.ProcessExit +=
            (_, _) =>
            {
                try
                {
                    try
                    {
                        Console.Out.Flush();
                    }
                    catch { }

                    try
                    {
                        Console.Error.Flush();
                    }
                    catch { }

                    // Restore original fd 1/2 first so we can still print to the terminal after the pumps drain.
                    // Restoring also closes the pipe write ends (via dup2), which causes read() on the pipes
                    // to return 0 and the pump threads to exit after draining any buffered data.
                    Unix.dup2(savedStdoutFd, STDOUT_FILENO);
                    Unix.dup2(savedStderrFd, STDERR_FILENO);

                    outThread.Join(TimeSpan.FromSeconds(2));
                    errThread.Join(TimeSpan.FromSeconds(2));

                    var finalMessage = "\nTest log written to: " + logFilePath + Environment.NewLine;
                    var finalBytes = Encoding.UTF8.GetBytes(finalMessage);

                    lock (logFileLock)
                    {
                        logFileStream.Write(finalBytes, 0, finalBytes.Length);
                        logFileStream.Flush();
                        logFileStream.Dispose();
                    }

                    try
                    {
                        savedStdoutStream.Write(finalBytes, 0, finalBytes.Length);
                        savedStdoutStream.Flush();
                    }
                    catch
                    {
                    }
                }
                catch
                {
                    // Best-effort on process exit.
                }
            };
    }

    private static void PumpPipe(int readFd, FileStream terminal, FileStream logFile, object logFileLock)
    {
        var buffer = new byte[4096];

        while (true)
        {
            int n;

            try
            {
                n = Unix.read(readFd, buffer, (UIntPtr)buffer.Length).ToInt32();
            }
            catch
            {
                break;
            }

            if (n <= 0)
            {
                break;
            }

            try
            {
                terminal.Write(buffer, 0, n);
                terminal.Flush();
            }
            catch
            {
                // Best-effort: if writing to the terminal fails, keep writing to the log file.
            }

            try
            {
                lock (logFileLock)
                {
                    logFile.Write(buffer, 0, n);
                    logFile.Flush();
                }
            }
            catch
            {
                // Best-effort.
            }
        }

        Unix.close(readFd);
    }

    /// <summary>
    /// On Windows, replace the process stdout and stderr handles with the write
    /// ends of anonymous pipes. Spawn two background threads that read from the
    /// pipes and forward every byte both to the original terminal handle and to
    /// the log file.
    /// </summary>
    private static void InstallWindowsHandleRedirect(FileStream logFileStream, string logFilePath)
    {
        const int STD_OUTPUT_HANDLE = -11;
        const int STD_ERROR_HANDLE = -12;

        var savedStdout = Win32.GetStdHandle(STD_OUTPUT_HANDLE);
        var savedStderr = Win32.GetStdHandle(STD_ERROR_HANDLE);

        if (!Win32.CreatePipe(out var outRead, out var outWrite, IntPtr.Zero, 0))
        {
            throw new InvalidOperationException("CreatePipe(stdout) failed: " + Marshal.GetLastPInvokeError());
        }

        if (!Win32.CreatePipe(out var errRead, out var errWrite, IntPtr.Zero, 0))
        {
            var lastError = Marshal.GetLastPInvokeError();
            Win32.CloseHandle(outRead);
            Win32.CloseHandle(outWrite);
            throw new InvalidOperationException("CreatePipe(stderr) failed: " + lastError);
        }

        if (!Win32.SetStdHandle(STD_OUTPUT_HANDLE, outWrite))
        {
            var lastError = Marshal.GetLastPInvokeError();
            Win32.CloseHandle(outRead);
            Win32.CloseHandle(outWrite);
            Win32.CloseHandle(errRead);
            Win32.CloseHandle(errWrite);
            throw new InvalidOperationException("SetStdHandle(stdout) failed: " + lastError);
        }

        if (!Win32.SetStdHandle(STD_ERROR_HANDLE, errWrite))
        {
            var lastError = Marshal.GetLastPInvokeError();
            // Roll back the stdout redirection so writes to stdout don't block on an orphaned pipe.
            Win32.SetStdHandle(STD_OUTPUT_HANDLE, savedStdout);
            Win32.CloseHandle(outRead);
            Win32.CloseHandle(outWrite);
            Win32.CloseHandle(errRead);
            Win32.CloseHandle(errWrite);
            throw new InvalidOperationException("SetStdHandle(stderr) failed: " + lastError);
        }

        // Force .NET to (re)initialize Console.Out and Console.Error against the (now redirected)
        // standard handles, in case Console.Out was created before our module initializer ran.
        try
        {
            Console.SetOut(new StreamWriter(Console.OpenStandardOutput()) { AutoFlush = true });
            Console.SetError(new StreamWriter(Console.OpenStandardError()) { AutoFlush = true });
        }
        catch
        {
            // Best-effort.
        }

        var logFileLock = new object();

        var outThread =
            new Thread(() => PumpWindowsPipe(outRead, savedStdout, logFileStream, logFileLock))
            {
                IsBackground = true,
                Name = "TestLogFileWriter.stdout-pump",
            };

        var errThread =
            new Thread(() => PumpWindowsPipe(errRead, savedStderr, logFileStream, logFileLock))
            {
                IsBackground = true,
                Name = "TestLogFileWriter.stderr-pump",
            };

        outThread.Start();
        errThread.Start();

        AppDomain.CurrentDomain.ProcessExit +=
            (_, _) =>
            {
                try
                {
                    try
                    {
                        Console.Out.Flush();
                    }
                    catch { }

                    try
                    {
                        Console.Error.Flush();
                    }
                    catch { }

                    // Restore original stdio handles first so we can still print to the terminal
                    // after the pumps drain. Unlike Unix dup2, SetStdHandle does not close the
                    // previous handle, so we must explicitly close the pipe write ends to cause
                    // ReadFile on the read ends to return with ERROR_BROKEN_PIPE.
                    Win32.SetStdHandle(STD_OUTPUT_HANDLE, savedStdout);
                    Win32.SetStdHandle(STD_ERROR_HANDLE, savedStderr);

                    Win32.CloseHandle(outWrite);
                    Win32.CloseHandle(errWrite);

                    outThread.Join(TimeSpan.FromSeconds(2));
                    errThread.Join(TimeSpan.FromSeconds(2));

                    var finalMessage = "\nTest log written to: " + logFilePath + Environment.NewLine;
                    var finalBytes = Encoding.UTF8.GetBytes(finalMessage);

                    lock (logFileLock)
                    {
                        logFileStream.Write(finalBytes, 0, finalBytes.Length);
                        logFileStream.Flush();
                        logFileStream.Dispose();
                    }

                    try
                    {
                        Win32.WriteFile(savedStdout, finalBytes, (uint)finalBytes.Length, out _, IntPtr.Zero);
                    }
                    catch
                    {
                    }
                }
                catch
                {
                    // Best-effort on process exit.
                }
            };
    }

    private static void PumpWindowsPipe(IntPtr readHandle, IntPtr terminalHandle, FileStream logFile, object logFileLock)
    {
        var buffer = new byte[4096];

        while (true)
        {
            bool ok;
            uint bytesRead;

            try
            {
                ok = Win32.ReadFile(readHandle, buffer, (uint)buffer.Length, out bytesRead, IntPtr.Zero);
            }
            catch
            {
                break;
            }

            if (!ok || bytesRead == 0)
            {
                // ERROR_BROKEN_PIPE (109) when the write end is closed: normal EOF.
                break;
            }

            try
            {
                Win32.WriteFile(terminalHandle, buffer, bytesRead, out _, IntPtr.Zero);
            }
            catch
            {
                // Best-effort: if writing to the terminal fails, keep writing to the log file.
            }

            try
            {
                lock (logFileLock)
                {
                    logFile.Write(buffer, 0, (int)bytesRead);
                    logFile.Flush();
                }
            }
            catch
            {
                // Best-effort.
            }
        }

        Win32.CloseHandle(readHandle);
    }

    /// <summary>
    /// Fallback for platforms where the handle/fd-level redirection isn't implemented:
    /// tee <see cref="Console.Out"/> and <see cref="Console.Error"/>. This does NOT
    /// capture output that is written directly to the underlying stdout stream (for
    /// example via <c>Console.OpenStandardOutput</c>), but covers ordinary
    /// <c>Console.WriteLine</c> usage.
    /// </summary>
    private static void InstallConsoleOnlyTee(FileStream logFileStream, string logFilePath)
    {
        var logWriter =
            new StreamWriter(
                logFileStream,
                new UTF8Encoding(encoderShouldEmitUTF8Identifier: false))
            {
                AutoFlush = true,
            };

        var originalOut = Console.Out;
        var originalError = Console.Error;

        var teeOut = TextWriter.Synchronized(new TeeTextWriter(originalOut, logWriter));
        var teeError = TextWriter.Synchronized(new TeeTextWriter(originalError, logWriter));

        Console.SetOut(teeOut);
        Console.SetError(teeError);

        AppDomain.CurrentDomain.ProcessExit +=
            (_, _) =>
            {
                try
                {
                    teeOut.Flush();
                    teeError.Flush();

                    var message = "Test log written to: " + logFilePath;

                    logWriter.WriteLine();
                    logWriter.WriteLine(message);
                    logWriter.Flush();

                    originalOut.WriteLine();
                    originalOut.WriteLine(message);
                    originalOut.Flush();

                    logWriter.Dispose();
                }
                catch
                {
                    // Best-effort on process exit.
                }
            };
    }

    private static bool HasFilterArgument(IReadOnlyList<string> args)
    {
        for (var i = 0; i < args.Count; i++)
        {
            if (IsFilterArgument(args[i]))
            {
                return true;
            }
        }

        return false;
    }

    private static bool IsFilterArgument(string arg)
    {
        // Microsoft.Testing.Platform filter-related flags all start with "--filter"
        // (e.g. --filter, --filter-method, --filter-class, --filter-namespace,
        // --filter-query) or with "--treenode-filter" / "--test-node-filter".
        if (arg.StartsWith("--filter", StringComparison.Ordinal))
        {
            return true;
        }

        if (arg.StartsWith("--treenode-filter", StringComparison.Ordinal))
        {
            return true;
        }

        if (arg.StartsWith("--test-node-filter", StringComparison.Ordinal))
        {
            return true;
        }

        return false;
    }

    private static string FormatCommandLine(string[] args)
    {
        var builder = new StringBuilder();

        for (var i = 0; i < args.Length; i++)
        {
            if (i > 0)
            {
                builder.Append(' ');
            }

            builder.Append(QuoteIfNeeded(args[i]));
        }

        return builder.ToString();
    }

    private static string QuoteIfNeeded(string arg)
    {
        if (arg.Length == 0)
        {
            return "\"\"";
        }

        var needsQuoting = false;

        foreach (var c in arg)
        {
            if (char.IsWhiteSpace(c) || c == '"')
            {
                needsQuoting = true;
                break;
            }
        }

        if (!needsQuoting)
        {
            return arg;
        }

        return "\"" + arg.Replace("\"", "\\\"") + "\"";
    }

    private sealed class TeeTextWriter : TextWriter
    {
        private readonly TextWriter _first;

        private readonly TextWriter _second;

        public TeeTextWriter(TextWriter first, TextWriter second)
        {
            _first = first;
            _second = second;
        }

        public override Encoding Encoding => _first.Encoding;

        public override void Write(char value)
        {
            _first.Write(value);
            _second.Write(value);
        }

        public override void Write(string? value)
        {
            _first.Write(value);
            _second.Write(value);
        }

        public override void Write(char[] buffer, int index, int count)
        {
            _first.Write(buffer, index, count);
            _second.Write(buffer, index, count);
        }

        public override void WriteLine()
        {
            _first.WriteLine();
            _second.WriteLine();
        }

        public override void WriteLine(string? value)
        {
            _first.WriteLine(value);
            _second.WriteLine(value);
        }

        public override void Flush()
        {
            _first.Flush();
            _second.Flush();
        }
    }

    private static class Unix
    {
        [DllImport("libc", SetLastError = true)]
        internal static extern int dup(int fd);

        [DllImport("libc", SetLastError = true)]
        internal static extern int dup2(int oldfd, int newfd);

        [DllImport("libc", SetLastError = true)]
        internal static extern int pipe(int[] pipefd);

        [DllImport("libc", SetLastError = true)]
        internal static extern int close(int fd);

        [DllImport("libc", SetLastError = true)]
        internal static extern IntPtr read(int fd, byte[] buf, UIntPtr count);
    }

    private static class Win32
    {
        [DllImport("kernel32.dll", SetLastError = true)]
        internal static extern IntPtr GetStdHandle(int nStdHandle);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool SetStdHandle(int nStdHandle, IntPtr hHandle);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool CreatePipe(
            out IntPtr hReadPipe,
            out IntPtr hWritePipe,
            IntPtr lpPipeAttributes,
            uint nSize);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool ReadFile(
            IntPtr hFile,
            byte[] lpBuffer,
            uint nNumberOfBytesToRead,
            out uint lpNumberOfBytesRead,
            IntPtr lpOverlapped);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool WriteFile(
            IntPtr hFile,
            byte[] lpBuffer,
            uint nNumberOfBytesToWrite,
            out uint lpNumberOfBytesWritten,
            IntPtr lpOverlapped);

        [DllImport("kernel32.dll", SetLastError = true)]
        [return: MarshalAs(UnmanagedType.Bool)]
        internal static extern bool CloseHandle(IntPtr hObject);
    }
}
