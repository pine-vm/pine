using AwesomeAssertions;
using Pine.CLI;
using Pine.Core.CLI;
using System;
using System.Collections.Generic;
using System.IO;
using System.Text;
using System.Threading;
using System.Threading.Channels;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests.CLI;

public sealed class RunWatchCommandTests : IDisposable
{
    private static readonly TimeSpan s_wait = TimeSpan.FromSeconds(60);

    private readonly string _directory =
        Path.Combine(Path.GetTempPath(), "pine-apply-watch-" + Guid.NewGuid().ToString("N"));

    private string Module => Path.Combine(_directory, "Batch.elm");

    public RunWatchCommandTests()
    {
        Directory.CreateDirectory(_directory);

        File.WriteAllText(
            Module,
            """
            module Batch exposing (..)
            import Bytes exposing (Bytes)

            join : String -> String -> String -> String
            join first second suffix = first ++ second ++ suffix

            text : String -> String
            text value = value

            bytes : Bytes -> Bytes
            bytes value = value

            app : { init : () -> (), subscriptions : () -> () }
            app = { init = \_ -> (), subscriptions = \_ -> () }
            """);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Run_watch_rereads_multiple_files_handles_editor_replacement_and_recovers(bool batchOnly)
    {
        var first = Path.Combine(_directory, "first.txt");
        var second = Path.Combine(Directory.CreateDirectory(Path.Combine(_directory, "other")).FullName, "second.txt");
        var destination = Path.Combine(_directory, "result.txt");
        File.WriteAllText(first, "first");
        File.WriteAllText(second, "second");
        using var cancellation = new CancellationTokenSource(s_wait);
        using var error = new NotifyingWriter();
        using var output = new StringWriter();
        var runs = 0;
        error.OnWaiting = () => Interlocked.Increment(ref runs);

        var watching =
            Task.Run(
                () => Execute(
                    "join",
                    [new(first, true), new(second, true), new("!")],
                    destination,
                    cancellation.Token,
                    error,
                    output,
                    batchOnly: batchOnly));

        try
        {
            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("firstsecond!");
            output.ToString().Should().BeEmpty();

            File.WriteAllText(Path.Combine(_directory, "unrelated.txt"), "ignore");
            File.WriteAllText(destination, "manually edited output");
            await Task.Delay(500);
            Volatile.Read(ref runs).Should().Be(1, "output and unrelated files must not trigger feedback loops");

            File.WriteAllText(first, "saved");
            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("savedsecond!");

            var replacement = Path.Combine(_directory, "replacement.tmp");
            File.WriteAllText(replacement, "replaced");
            File.Move(replacement, second, overwrite: true);
            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("savedreplaced!");

            File.Delete(second);
            await error.NextWaiting();
            error.ToString().Should().Contain("Error:");
            File.ReadAllText(destination).Should().Be("savedreplaced!");

            File.WriteAllText(second, "recovered");
            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("savedrecovered!");
        }
        finally
        {
            cancellation.Cancel();
            (await watching.WaitAsync(s_wait)).Should().Be(130);
        }
    }

    [Theory]
    [InlineData("text")]
    [InlineData("bytes")]
    public async Task Run_watch_preserves_text_encoding_and_binary_output(string declaration)
    {
        var input = Path.Combine(_directory, "input");
        var destination = Path.Combine(_directory, "output");
        var encoding = new UnicodeEncoding(bigEndian: true, byteOrderMark: true, throwOnInvalidBytes: true);

        var original =
            declaration == "text"
            ?
            [.. encoding.GetPreamble(), .. encoding.GetBytes("caf\u00e9")]
            :
            new byte[] { 0xff, 0, 0xfe, 1 };

        File.WriteAllBytes(input, original);
        using var cancellation = new CancellationTokenSource(s_wait);
        using var error = new NotifyingWriter();
        using var output = new StringWriter();

        var watching =
            Task.Run(() => Execute(declaration, [new(input, true)], destination, cancellation.Token, error, output));

        try
        {
            await error.NextWaiting();
            File.ReadAllBytes(destination).Should().Equal(original);

            var changed = declaration == "text" ? Encoding.UTF8.GetBytes("new encoding") : new byte[] { 1, 2, 3 };
            File.WriteAllBytes(input, changed);
            await error.NextWaiting();
            File.ReadAllBytes(destination).Should().Equal(changed);
            output.ToString().Should().BeEmpty();
        }
        finally
        {
            cancellation.Cancel();
            (await watching.WaitAsync(s_wait)).Should().Be(130);
        }
    }

    [Theory]
    [InlineData("missing-output", "--output-file")]
    [InlineData("missing-argument", "every argument explicitly")]
    [InlineData("no-files", "local --input-file")]
    [InlineData("remote-input", "remote input files")]
    [InlineData("same-file", "distinct from every input")]
    [InlineData("transform", "--transform-file")]
    [InlineData("application", "batch functions only")]
    public void Run_watch_rejects_unsupported_combinations_without_prompting(string scenario, string expected)
    {
        var input = Path.Combine(_directory, "input");
        var destination = Path.Combine(_directory, "output");
        File.WriteAllText(input, "original");
        File.WriteAllText(destination, "keep");
        using var error = new StringWriter();
        using var output = new StringWriter();
        var inputs = new List<RunCommand.BatchInput> { new(input, true) };

        if (scenario == "missing-argument")
            inputs.Clear();

        if (scenario == "no-files")
            inputs = [new("literal")];

        if (scenario == "remote-input")
            inputs = [new("https://github.com/example/repo/blob/main/input", true)];

        var exit =
            Execute(
                scenario == "application" ? "app" : "text",
                scenario is "application" or "transform" ? [] : inputs,
                scenario is "missing-output" or "transform" ? null : scenario == "same-file" ? input : destination,
                CancellationToken.None,
                error,
                output,
                transformFile: scenario == "transform" ? input : null,
                createWatcher: _ => throw new InvalidOperationException("Invalid options must not start a watcher"));

        exit.Should().Be(1);
        error.ToString().Should().Contain(expected).And.NotContain("Input> ");
        output.ToString().Should().BeEmpty();
        File.ReadAllText(input).Should().Be("original");
        File.ReadAllText(destination).Should().Be("keep");
    }

    [Fact]
    public void Run_watch_rejects_output_aliases_through_directory_links()
    {
        if (OperatingSystem.IsWindows())
            return;

        var directory = Directory.CreateDirectory(Path.Combine(_directory, "files")).FullName;
        var alias = Path.Combine(_directory, "alias");
        Directory.CreateSymbolicLink(alias, directory);
        var input = Path.Combine(directory, "input");
        File.WriteAllText(input, "keep");
        using var error = new StringWriter();
        using var output = new StringWriter();

        Execute(
            "text",
            [new(input, true)],
            Path.Combine(alias, "input"),
            CancellationToken.None,
            error,
            output,
            createWatcher: _ => throw new InvalidOperationException("Output aliases must not start a watcher"))
            .Should().Be(1);

        error.ToString().Should().Contain("distinct from every input");
        File.ReadAllText(input).Should().Be("keep");
    }

    [Fact]
    public async Task Run_watch_recovers_when_the_input_is_created_after_the_initial_run()
    {
        var input = Path.Combine(_directory, "not-created-yet");
        var destination = Path.Combine(_directory, "output");
        File.WriteAllText(destination, "keep");
        using var cancellation = new CancellationTokenSource(s_wait);
        using var error = new NotifyingWriter();
        using var output = new StringWriter();

        var watching =
            Task.Run(() => Execute("text", [new(input, true)], destination, cancellation.Token, error, output));

        try
        {
            await error.NextWaiting();
            error.ToString().Should().Contain("Error:");
            File.ReadAllText(destination).Should().Be("keep");

            File.WriteAllText(input, "created");
            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("created");
        }
        finally
        {
            cancellation.Cancel();
            (await watching.WaitAsync(s_wait)).Should().Be(130);
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Run_watch_option_is_wired_for_both_commands(bool batchOnly)
    {
        var result = RunCommand.Create(batchOnly).Parse([Module + "/text", "--watch", "--input", "literal"]);
        using var output = new StringWriter();
        using var error = new StringWriter();
        result.Errors.Should().BeEmpty();
        result.Invoke(new() { Output = output, Error = error }).Should().Be(1);
        error.ToString().Should().Contain("--watch requires --output-file");
    }

    [Theory]
    [InlineData("cancel", 130)]
    [InlineData("timeout", 124)]
    [InlineData("error", 1)]
    [InlineData("start-error", 1)]
    public async Task Run_watch_stops_and_disposes_watchers(string stop, int expectedExit)
    {
        var input = Path.Combine(_directory, "input");
        var destination = Path.Combine(_directory, "output");
        File.WriteAllText(input, "value");
        using var cancellation = new CancellationTokenSource(s_wait);
        using var error = new NotifyingWriter();
        using var output = new StringWriter();
        var watcher = new FakeWatcher(stop == "start-error");

        var watching =
            Task.Run(
                () => Execute(
                    "text",
                    [new(input, true)],
                    destination,
                    cancellation.Token,
                    error,
                    output,
                    timeout: stop == "timeout" ? TimeSpan.FromSeconds(3) : null,
                    createWatcher: _ => watcher));

        try
        {
            if (stop != "start-error")
            {
                await error.NextWaiting();

                if (stop == "cancel")
                    cancellation.Cancel();

                else if (stop == "error")
                    watcher.Fail();
            }

            (await watching.WaitAsync(s_wait)).Should().Be(expectedExit);
            watcher.Disposed.Should().BeTrue();
            watcher.Started.Should().BeFalse();
            watcher.HasSubscriptions.Should().BeFalse();

            if (stop is "error" or "start-error")
                error.ToString().Should().Contain("File watcher failed:");

            if (stop == "timeout")
                error.ToString().Should().Contain("Run timed out");
        }
        finally
        {
            cancellation.Cancel();
            await watching.WaitAsync(s_wait);
        }
    }

    private int Execute(
        string declaration,
        IReadOnlyList<RunCommand.BatchInput> inputs,
        string? destination,
        CancellationToken token,
        TextWriter error,
        TextWriter output,
        bool batchOnly = true,
        string? transformFile = null,
        TimeSpan? timeout = null,
        Func<IReadOnlyList<string>, IFileWatcher>? createWatcher = null) =>
        RunCommand.Execute(
            Module + "/" + declaration,
            inputs: inputs,
            outputFile: destination,
            transformFile: transformFile,
            color: CommandLineColorMode.Never,
            timeout: timeout ?? s_wait,
            output: output,
            error: error,
            cacheDirectory: Path.Combine(_directory, "cache"),
            cancellationToken: token,
            inputReader: TextReader.Null,
            batchOnly: batchOnly,
            watch: true,
            createWatcher: createWatcher);

    private sealed class NotifyingWriter : StringWriter
    {
        private readonly Channel<bool> _completedRuns = Channel.CreateUnbounded<bool>();

        internal Action? OnWaiting { get; set; }

        public override void WriteLine(string? value)
        {
            base.WriteLine(value);

            if (value == "Waiting for input changes...")
            {
                OnWaiting?.Invoke();
                _completedRuns.Writer.TryWrite(true);
            }
        }

        internal async Task NextWaiting() => await _completedRuns.Reader.ReadAsync().AsTask().WaitAsync(s_wait);
    }

    private sealed class FakeWatcher(bool failOnStart) : IFileWatcher
    {
        public event Action? Changed;

        public event Action<Exception>? Error;

        internal bool Started { get; private set; }

        internal bool Disposed { get; private set; }

        internal bool HasSubscriptions => Changed is not null || Error is not null;

        internal void Fail() => Error?.Invoke(new InternalBufferOverflowException("Simulated overflow"));

        public void Start()
        {
            Started = true;

            if (failOnStart)
                throw new IOException("Simulated start failure");
        }

        public void Stop() => Started = false;

        public void Dispose() => Disposed = true;
    }

    public void Dispose() => Directory.Delete(_directory, recursive: true);
}
