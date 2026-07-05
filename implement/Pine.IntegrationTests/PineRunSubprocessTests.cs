using AwesomeAssertions;
using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests;

/// <summary>
/// Integration tests that drive a Pine command-line Elm app end-to-end:
/// they spawn the built <c>pine</c> executable with the <c>run</c>
/// subcommand pointed at the Elm app committed under
/// <c>TestAppInterface/PineRunCliApp/</c>, and communicate with the
/// resulting process via redirected stdin/stdout/stderr.
///
/// <para>
/// In contrast to <see cref="ElmCommandLineAppTests"/>, which exercises
/// the Elm CLI infrastructure in-process via
/// <see cref="Pine.Elm.Platform.MutatingCommandLineApp"/>, these tests
/// exercise the full subprocess boundary including process-level exit
/// codes — so they confirm that the modelled <c>exit</c> value reaches
/// the parent process and that bytes flow correctly over the OS pipes.
/// </para>
/// </summary>
public class PineRunSubprocessTests
{
    // The end-to-end test path requires building the full Elm app
    // inside `pine run`, which can take significant time on a cold
    // runner; each [Fact] gets its own budget.
    private const int TestTimeoutMs = 1000 * 60 * 4;

    /// <summary>
    /// Sub-path under <c>implement/Pine.IntegrationTests/</c> that
    /// contains the Elm CLI app driven by these tests. The directory
    /// (with <c>elm.json</c> and the <c>src/</c> tree) is committed to
    /// the repository so the test does not need to materialise any
    /// files at run time.
    /// </summary>
    private static readonly IReadOnlyList<string> s_appDirectoryPathParts =
        ["TestAppInterface", "PineRunCliApp"];

    private const string AppEntryPoint = "src/App.elm";

    [Fact(Timeout = TestTimeoutMs, Skip = "Skip temporary while performance optimizations are WIP")]
    public async Task Run_command_line_app_via_pine_run_subprocess_reports_greeting_request_response_stderr_and_exit_code_7_Async()
    {
        var appDir = FindTestAppDirectory();

        await using var driver = await PineRunDriver.StartAsync(appDir);

        // (1) Initial greeting from the Elm app on stdout. The pine
        // `run` command also emits a "Starting Elm app from ..." line
        // on stdout before the Elm app starts, so we wait for the
        // marker emitted by the Elm app itself.
        var greeting =
            await driver.WaitForStdOutContainingAsync(
                "Started with the following command line:",
                timeoutMs: TestTimeoutMs);

        greeting.Should().Contain("Started with the following command line:");

        // (2) Request → response over stdin/stdout. The Elm app
        // echoes the stdin bytes verbatim and then emits a
        // "Received line:\n<line>\n" block once a newline terminates a
        // line.
        await driver.WriteStdInAsync("hello\n");

        var lineResp =
            await driver.WaitForStdOutContainingAsync(
                "Received line:\nhello",
                timeoutMs: TestTimeoutMs);

        lineResp.Should().Contain("Received line:\nhello");

        // (3) Stderr from the Elm app + (4) exit code modelled by
        // the Elm app: writing "fail\n" triggers the explicit failure
        // branch in the test app, which emits a message on stderr and
        // sets `exit = Just 7`.
        await driver.WriteStdInAsync("fail\n");

        var errResp =
            await driver.WaitForStdErrContainingAsync(
                "failing on demand",
                timeoutMs: TestTimeoutMs);

        errResp.Should().Contain("failing on demand");

        var exitCode = await driver.WaitForExitAsync(TestTimeoutMs);
        exitCode.Should().Be(7);
    }

    [Fact(Timeout = TestTimeoutMs, Skip = "Skip temporary while performance optimizations are WIP")]
    public async Task Run_command_line_app_via_pine_run_subprocess_propagates_exit_code_0_Async()
    {
        // Independent run that exits with code 0 via the "11+ lines"
        // branch in the Elm app. Combined with the exit-7 case in the
        // sibling test, verifying two distinct exit values rules out a
        // hard-coded propagation path on the parent side.
        var appDir = FindTestAppDirectory();

        await using var driver = await PineRunDriver.StartAsync(appDir);

        await driver.WaitForStdOutContainingAsync(
            "Started with the following command line:",
            timeoutMs: TestTimeoutMs);

        // Send 11 lines in one chunk so the Elm app trips its
        // `10 < List.length state.completedLines` branch and exits
        // with code 0 emitting "Exiting after 11 lines received".
        await driver.WriteStdInAsync(string.Concat(Enumerable.Repeat("x\n", 11)));

        await driver.WaitForStdOutContainingAsync(
            "Exiting after 11 lines received",
            timeoutMs: TestTimeoutMs);

        var exitCode = await driver.WaitForExitAsync(TestTimeoutMs);
        exitCode.Should().Be(0);
    }

    /// <summary>
    /// Locates the <c>TestAppInterface/PineRunCliApp/</c> directory
    /// committed alongside this test by walking up to the nearest
    /// <c>implement</c> directory and then descending into
    /// <c>Pine.IntegrationTests/TestAppInterface/PineRunCliApp/</c>.
    /// </summary>
    private static string FindTestAppDirectory()
    {
        var implementDirectory = FindImplementDirectory();

        var pathPartsList =
            new List<string>
            {
                implementDirectory.FullName,
                "Pine.IntegrationTests",
            };

        pathPartsList.AddRange(s_appDirectoryPathParts);

        var candidate = Path.Combine([.. pathPartsList]);

        if (!Directory.Exists(candidate))
            throw new Exception("Could not find test app directory at " + candidate);

        return candidate;
    }

    /// <summary>
    /// Helper that wraps a launched <c>pine run</c> subprocess and
    /// exposes async, append-and-poll style access to its stdout /
    /// stderr streams plus its stdin. Output is read asynchronously on
    /// background threads and accumulated into thread-safe builders so
    /// the test can wait for matching substrings without racing the
    /// reader.
    ///
    /// <para>
    /// The driver also emits periodic heartbeat diagnostics to the
    /// test console while waiting for output. This is intentional: an
    /// earlier version of this test class would hang for the full
    /// 10-minute per-test budget producing no output, making it
    /// impossible to tell whether the subprocess was busy compiling,
    /// stuck on a buffered stream, blocked waiting for stdin, or
    /// already exited with the expected bytes lost in a buffer. The
    /// heartbeat shows current stdout/stderr buffer sizes, last-read
    /// timestamps and process status so a future hang exposes its
    /// cause within seconds.
    /// </para>
    /// </summary>
    private sealed class PineRunDriver : IAsyncDisposable
    {
        private readonly Process _process;

        private readonly StringBuilder _stdOut = new();

        private readonly StringBuilder _stdErr = new();

        private readonly object _stdOutLock = new();

        private readonly object _stdErrLock = new();

        private readonly Task _readStdOutTask;

        private readonly Task _readStdErrTask;

        private readonly DateTimeOffset _startedAt;

        // Updated on every successful chunk read from the corresponding
        // stream. Exposed via diagnostic snapshots so a hung test can
        // tell whether a stream is still flowing or has gone silent.
        private DateTimeOffset _lastStdOutReadAt;

        private DateTimeOffset _lastStdErrReadAt;

        // Heartbeats are throttled so a long wait does not flood the
        // test log. 5 seconds is short enough to expose the kind of
        // hangs we saw earlier (subprocess silently producing nothing
        // for minutes) without being noisy on normal runs.
        private static readonly TimeSpan s_heartbeatInterval =
            TimeSpan.FromSeconds(5);

        private PineRunDriver(Process process)
        {
            _process = process;
            _startedAt = DateTimeOffset.UtcNow;
            _lastStdOutReadAt = _startedAt;
            _lastStdErrReadAt = _startedAt;

            _readStdOutTask =
                Task.Run(
                    () => PumpStream(
                        process.StandardOutput,
                        _stdOut,
                        _stdOutLock,
                        ts => _lastStdOutReadAt = ts));

            _readStdErrTask =
                Task.Run(
                    () => PumpStream(
                        process.StandardError,
                        _stdErr,
                        _stdErrLock,
                        ts => _lastStdErrReadAt = ts));
        }

        public static Task<PineRunDriver> StartAsync(string appDirectory)
        {
            var executablePath = FindPineExecutableFilePath();

            var psi =
                new ProcessStartInfo(executablePath)
                {
                    RedirectStandardInput = true,
                    RedirectStandardOutput = true,
                    RedirectStandardError = true,
                    UseShellExecute = false,
                    StandardOutputEncoding = Encoding.UTF8,
                    StandardErrorEncoding = Encoding.UTF8,
                };

            psi.ArgumentList.Add("run");
            psi.ArgumentList.Add(AppEntryPoint);
            psi.ArgumentList.Add("--input-directory=" + appDirectory);

            var process =
                Process.Start(psi)
                ?? throw new Exception("Failed starting pine subprocess");

            return Task.FromResult(new PineRunDriver(process));
        }

        public Task WriteStdInAsync(string text) =>
            WriteStdInBytesAsync(Encoding.UTF8.GetBytes(text));

        public async Task WriteStdInBytesAsync(byte[] bytes)
        {
            await _process.StandardInput.BaseStream.WriteAsync(bytes);

            await _process.StandardInput.BaseStream.FlushAsync();
        }

        public Task<string> WaitForStdOutContainingAsync(string needle, int timeoutMs) =>
            WaitForContainingAsync(_stdOut, _stdOutLock, needle, timeoutMs, "stdout");

        public Task<string> WaitForStdErrContainingAsync(string needle, int timeoutMs) =>
            WaitForContainingAsync(_stdErr, _stdErrLock, needle, timeoutMs, "stderr");

        private async Task<string> WaitForContainingAsync(
            StringBuilder buffer,
            object bufferLock,
            string needle,
            int timeoutMs,
            string streamName)
        {
            var deadline = Environment.TickCount64 + timeoutMs;
            var nextHeartbeatAt = DateTimeOffset.UtcNow + s_heartbeatInterval;

            Console.WriteLine(
                "[PineRunDriver] waiting for " + Describe(needle) +
                " on " + streamName + " (timeout " + timeoutMs + "ms)");

            while (true)
            {
                string snapshot;

                lock (bufferLock)
                {
                    snapshot = buffer.ToString();
                }

                if (snapshot.Contains(needle))
                {
                    Console.WriteLine(
                        "[PineRunDriver] matched " + Describe(needle) +
                        " on " + streamName + " after " +
                        (int)(DateTimeOffset.UtcNow - _startedAt).TotalMilliseconds + "ms");

                    return snapshot;
                }

                if (_process.HasExited && _readStdOutTask.IsCompleted && _readStdErrTask.IsCompleted)
                {
                    lock (bufferLock)
                    {
                        snapshot = buffer.ToString();
                    }

                    if (snapshot.Contains(needle))
                        return snapshot;

                    throw new Exception(
                        "Process exited before " + Describe(needle) +
                        " appeared on " + streamName + ".\n" +
                        FormatDiagnosticSnapshot());
                }

                if (Environment.TickCount64 > deadline)
                {
                    throw new TimeoutException(
                        "Timed out waiting for " + Describe(needle) +
                        " on " + streamName + " after " + timeoutMs + "ms.\n" +
                        FormatDiagnosticSnapshot());
                }

                if (DateTimeOffset.UtcNow > nextHeartbeatAt)
                {
                    Console.WriteLine(
                        "[PineRunDriver] still waiting for " + Describe(needle) +
                        " on " + streamName + " " + FormatStatusLine());

                    nextHeartbeatAt = DateTimeOffset.UtcNow + s_heartbeatInterval;
                }

                await Task.Delay(50);
            }
        }

        public async Task<int> WaitForExitAsync(int timeoutMs)
        {
            var nextHeartbeatAt = DateTimeOffset.UtcNow + s_heartbeatInterval;
            var deadline = Environment.TickCount64 + timeoutMs;

            Console.WriteLine(
                "[PineRunDriver] waiting for process exit (timeout " + timeoutMs + "ms)");

            while (!_process.HasExited)
            {
                if (Environment.TickCount64 > deadline)
                {
                    throw new TimeoutException(
                        "Process did not exit within " + timeoutMs + "ms.\n" +
                        FormatDiagnosticSnapshot());
                }

                if (DateTimeOffset.UtcNow > nextHeartbeatAt)
                {
                    Console.WriteLine(
                        "[PineRunDriver] still waiting for exit " + FormatStatusLine());

                    nextHeartbeatAt = DateTimeOffset.UtcNow + s_heartbeatInterval;
                }

                await Task.Delay(50);
            }

            await Task.WhenAll(_readStdOutTask, _readStdErrTask);

            Console.WriteLine(
                "[PineRunDriver] process exited with code " + _process.ExitCode);

            return _process.ExitCode;
        }

        /// <summary>
        /// Returns a multi-line block with the captured stdout / stderr
        /// contents plus stream-flow timestamps and process status,
        /// suitable for embedding in an exception message. Inspectable
        /// from <see cref="Snapshot"/> too so tests can dump it on
        /// demand without throwing.
        /// </summary>
        public string FormatDiagnosticSnapshot()
        {
            var stdOut = GetStdOutSnapshot();
            var stdErr = GetStdErrSnapshot();

            var sb = new StringBuilder();
            sb.AppendLine("--- PineRunDriver diagnostic snapshot ---");
            sb.AppendLine(FormatStatusLine());
            sb.AppendLine("Captured stdout (" + stdOut.Length + " chars):");
            sb.AppendLine(stdOut);
            sb.AppendLine("Captured stderr (" + stdErr.Length + " chars):");
            sb.AppendLine(stdErr);
            sb.AppendLine("------------------------------------------");
            return sb.ToString();
        }

        /// <summary>
        /// Compact, one-line status string covering process state,
        /// buffer sizes and last-read timestamps. Used by the heartbeat
        /// log and by error messages.
        /// </summary>
        private string FormatStatusLine()
        {
            var now = DateTimeOffset.UtcNow;

            var stdOutLen = GetStdOutSnapshot().Length;
            var stdErrLen = GetStdErrSnapshot().Length;

            string processState;

            try
            {
                processState =
                    _process.HasExited
                    ?
                    "exited(code=" + _process.ExitCode + ")"
                    :
                    "running(pid=" + _process.Id + ")";
            }
            catch
            {
                processState = "unknown";
            }

            return
                "[elapsed=" + (int)(now - _startedAt).TotalMilliseconds + "ms" +
                " process=" + processState +
                " stdout=" + stdOutLen + "ch (last+" +
                (int)(now - _lastStdOutReadAt).TotalMilliseconds + "ms)" +
                " stderr=" + stdErrLen + "ch (last+" +
                (int)(now - _lastStdErrReadAt).TotalMilliseconds + "ms)" +
                " stdoutReader=" + _readStdOutTask.Status +
                " stderrReader=" + _readStdErrTask.Status + "]";
        }

        /// <summary>
        /// Read-only point-in-time view of the driver's observable
        /// state. Useful for assertions and ad-hoc inspection from
        /// future tests without going through the throw-on-timeout
        /// helpers.
        /// </summary>
        public DriverSnapshot Snapshot() =>
            new(
                StdOut: GetStdOutSnapshot(),
                StdErr: GetStdErrSnapshot(),
                HasExited: _process.HasExited,
                ExitCode: _process.HasExited ? _process.ExitCode : null,
                ElapsedSinceStart: DateTimeOffset.UtcNow - _startedAt,
                SinceLastStdOutRead: DateTimeOffset.UtcNow - _lastStdOutReadAt,
                SinceLastStdErrRead: DateTimeOffset.UtcNow - _lastStdErrReadAt);

        private static string Describe(string needle) =>
            "'" + needle.Replace("\n", "\\n").Replace("\r", "\\r") + "'";

        private string GetStdOutSnapshot()
        {
            lock (_stdOutLock)
            {
                return _stdOut.ToString();
            }
        }

        private string GetStdErrSnapshot()
        {
            lock (_stdErrLock)
            {
                return _stdErr.ToString();
            }
        }

        private static void PumpStream(
            StreamReader reader,
            StringBuilder buffer,
            object bufferLock,
            Action<DateTimeOffset> onChunkRead)
        {
            var chunk = new char[4096];

            while (true)
            {
                int read;

                try
                {
                    read = reader.Read(chunk, 0, chunk.Length);
                }
                catch
                {
                    return;
                }

                if (read <= 0)
                    return;

                lock (bufferLock)
                {
                    buffer.Append(chunk, 0, read);
                }

                onChunkRead(DateTimeOffset.UtcNow);
            }
        }

        public async ValueTask DisposeAsync()
        {
            try
            {
                if (!_process.HasExited)
                {
                    try
                    {
                        _process.StandardInput.Close();
                    }
                    catch
                    {
                        // Best effort.
                    }

                    using var cts = new CancellationTokenSource(TimeSpan.FromSeconds(5));

                    try
                    {
                        await _process.WaitForExitAsync(cts.Token);
                    }
                    catch (OperationCanceledException)
                    {
                        try
                        {
                            _process.Kill(entireProcessTree: true);
                        }
                        catch
                        {
                            // Best effort.
                        }
                    }
                }
            }
            finally
            {
                try
                {
                    await Task.WhenAll(_readStdOutTask, _readStdErrTask)
                    .WaitAsync(TimeSpan.FromSeconds(5));
                }
                catch
                {
                    // Best effort.
                }

                _process.Dispose();
            }
        }
    }

    /// <summary>
    /// Read-only point-in-time view of <see cref="PineRunDriver"/>
    /// state. Lets tests assert on captured output, process status and
    /// stream-flow timestamps without having to peek at private state.
    /// </summary>
    private sealed record DriverSnapshot(
        string StdOut,
        string StdErr,
        bool HasExited,
        int? ExitCode,
        TimeSpan ElapsedSinceStart,
        TimeSpan SinceLastStdOutRead,
        TimeSpan SinceLastStdErrRead);

    /// <summary>
    /// Locates the built <c>pine</c> executable by walking up to the
    /// nearest <c>implement</c> directory and searching
    /// <c>pine/**/bin/</c> for <c>pine</c> or <c>pine.exe</c>. Mirrors
    /// the helper used by <see cref="ElmLanguageServerTests"/>.
    /// </summary>
    private static string FindPineExecutableFilePath()
    {
        var implementDirectory = FindImplementDirectory();

        var pineDirectoryPath = Path.Combine(implementDirectory.FullName, "pine");

        var pineDirectory = new FileStoreFromSystemIOFile(pineDirectoryPath);

        var allFiles = pineDirectory.ListFiles().ToImmutableArray();

        foreach (var fileSubPath in allFiles)
        {
            if (!fileSubPath.Contains("bin"))
                continue;

            var fileName = fileSubPath.Last();

            if (fileName is "pine" or "pine.exe")
                return Path.Combine(pineDirectoryPath, string.Join('/', fileSubPath));
        }

        throw new Exception("Could not find 'pine' executable under " + pineDirectoryPath);
    }

    private static DirectoryInfo FindImplementDirectory()
    {
        var currentDirectory = Directory.GetCurrentDirectory();

        var implementDirectory = new DirectoryInfo(currentDirectory);

        while (implementDirectory.Name is not "implement")
        {
            implementDirectory =
                implementDirectory.Parent
                ?? throw new Exception("Could not find 'implement' directory");
        }

        return implementDirectory;
    }
}
