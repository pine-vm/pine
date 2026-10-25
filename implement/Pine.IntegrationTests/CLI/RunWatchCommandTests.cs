using AwesomeAssertions;
using Pine.CLI;
using Pine.Core.CLI;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
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

            foreach (var name in new[] { ".git", "elm-stuff", "bin", "obj", "node_modules" })
            {
                var ignored = Directory.CreateDirectory(Path.Combine(_directory, name)).FullName;
                File.WriteAllText(Path.Combine(ignored, "Ignored.elm"), "not a program source");
            }

            await Task.Delay(500);
            Volatile.Read(ref runs).Should().Be(1, "output and unrelated files must not trigger feedback loops");

            File.WriteAllText(first, "saved");
            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("savedsecond!");
            error.ToString().Should().Contain("Input file changed: " + first);

            var replacement = Path.Combine(_directory, "replacement.tmp");
            File.WriteAllText(replacement, "replaced");
            File.Move(replacement, second, overwrite: true);
            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("savedreplaced!");
            error.ToString().Should().Contain("Input file changed: " + second);

            File.Delete(second);
            await error.NextWaiting();
            error.ToString().Should().Contain("Error:");
            File.ReadAllText(destination).Should().Be("savedreplaced!");

            File.WriteAllText(second, "recovered");
            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("savedrecovered!");

            File.WriteAllText(
                Module,
                File.ReadAllText(Module).Replace(
                    "first ++ second ++ suffix",
                    "\"updated:\" ++ first ++ second ++ suffix"));

            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("updated:savedrecovered!");
            error.ToString().Should().Contain("Program source changed: " + Module);
        }
        finally
        {
            cancellation.Cancel();
            (await watching.WaitAsync(s_wait)).Should().Be(130);
        }
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, true)]
    public async Task Run_watch_recompiles_program_sources_with_literal_or_remote_arguments(
        bool remoteInput, bool reportDurations)
    {
        var helper =
            Path.Combine(
                Directory.CreateDirectory(Path.Combine(_directory, "Formatting")).FullName,
                "Helper.elm");

        File.WriteAllText(
            helper,
            "module Formatting.Helper exposing (decorate)\ndecorate : String -> String\ndecorate value = value\n");

        var original =
            File.ReadAllText(Module)
            .Replace("import Bytes exposing (Bytes)", "import Bytes exposing (Bytes)\nimport Formatting.Helper")
            .Replace("text value = value", "text value = Formatting.Helper.decorate value");

        File.WriteAllText(Module, original);
        var fixture = RemoteFixture();
        var remote = "https://github.com/owner/repo/blob/" + fixture.Commit + "/input.txt";

        var loader =
            new RunSource.Loader(
                Path.Combine(_directory, "cache"),
                TextWriter.Null,
                openRemote: (_, _, _) => fixture.Open());

        var destination = Path.Combine(_directory, "result.elm");
        using var cancellation = new CancellationTokenSource(s_wait);
        using var error = new NotifyingWriter();
        using var output = new StringWriter();
        var selections = 0;

        var watching =
            Task.Run(
                () => Execute(
                    "text",
                    [new(remoteInput ? remote : "value", remoteInput)],
                    destination,
                    cancellation.Token,
                    error,
                    output,
                    reportDurations: reportDurations,
                    entryPoint: Module,
                    sourceLoader: loader,
                    selectDeclaration:
                    _ =>
                    {
                        Interlocked.Increment(ref selections);
                        return "text";
                    }));

        try
        {
            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("value");
            error.ToString().Should().Contain("Watching program sources in " + _directory);
            AssertDurations(compilations: 1, executions: 1);

            // Waiting must not be included in the next run's execution duration.
            await Task.Delay(TimeSpan.FromSeconds(2));
            var replacement = Path.Combine(_directory, "replacement.tmp");

            File.WriteAllText(
                replacement,
                File.ReadAllText(helper).Replace("decorate value = value", "decorate value = value ++ \"!\""));

            File.Move(replacement, helper, overwrite: true);
            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("value!");
            error.ToString().Should().Contain("Program source changed: " + helper);
            AssertDurations(compilations: 2, executions: 2);

            if (reportDurations)
            {
                var latest =
                    Regex.Matches(error.ToString(), @"(?m)^execution: ([0-9.]+) s")
                    .Last().Groups[1].Value;

                double.Parse(latest, System.Globalization.CultureInfo.InvariantCulture).Should().BeLessThan(2);
            }

            File.WriteAllText(Module, "not an Elm module");
            await error.NextWaiting();
            error.ToString().Should().Contain("Program source changed: " + Module).And.Contain("Error:");
            File.ReadAllText(destination).Should().Be("value!");
            AssertDurations(compilations: 3, executions: 2);

            File.WriteAllText(
                Module,
                original.Replace("text : String -> String", "text : String -> String -> String")
                .Replace("text value = Formatting.Helper.decorate value", "text value suffix = value ++ suffix"));

            await error.NextWaiting();
            error.ToString().Should().Contain("every argument explicitly").And.NotContain("Input> ");
            File.ReadAllText(destination).Should().Be("value!");
            AssertDurations(compilations: 4, executions: 3);

            File.WriteAllText(
                Module,
                original.Replace(
                    "text value = Formatting.Helper.decorate value",
                    "text value = Formatting.Helper.decorate value ++ \"?\""));

            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("value!?");
            AssertDurations(compilations: 5, executions: 4);

            var renamed = Path.Combine(_directory, "Renamed");
            Directory.Move(Path.GetDirectoryName(helper)!, renamed);
            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("value!?");
            error.ToString().Should().Contain("Program source changed: " + renamed);
            AssertDurations(compilations: 6, executions: 5);

            helper = Path.Combine(renamed, "Helper.elm");
            File.Delete(helper);
            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("value!?");
            AssertDurations(compilations: 7, executions: 5);

            File.WriteAllText(
                helper,
                "module Formatting.Helper exposing (decorate)\ndecorate : String -> String\ndecorate value = value ++ \"#\"\n");

            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("value#?");
            AssertDurations(compilations: 8, executions: 6);
            selections.Should().Be(1, "a watch rerun must keep the selected declaration without reopening the picker");
            output.ToString().Should().BeEmpty();
        }
        finally
        {
            cancellation.Cancel();
            (await watching.WaitAsync(s_wait)).Should().Be(130);
        }

        AssertDurations(compilations: 8, executions: 6);
        error.ToString().Should().NotContain("Count:");

        void AssertDurations(int compilations, int executions)
        {
            Regex.Matches(error.ToString(), @"(?m)^compilation: [0-9.]+ s").Count.Should()
                .Be(reportDurations ? compilations : 0);

            Regex.Matches(error.ToString(), @"(?m)^execution: [0-9.]+ s").Count.Should()
                .Be(reportDurations ? executions : 0);
        }
    }

    [Fact]
    public async Task Run_watch_reloads_project_manifests()
    {
        var src = Directory.CreateDirectory(Path.Combine(_directory, "src")).FullName;
        File.Move(Module, Path.Combine(src, "Batch.elm"));

        File.WriteAllText(
            Path.Combine(src, "Batch.elm"),
            "module Batch exposing (text)\nimport Helper\ntext : String -> String\ntext value = Helper.decorate value\n");

        foreach (var name in new[] { "first", "second" })
        {
            var directory = Directory.CreateDirectory(Path.Combine(_directory, name)).FullName;

            File.WriteAllText(
                Path.Combine(directory, "Helper.elm"),
                "module Helper exposing (decorate)\ndecorate : String -> String\ndecorate value = value ++ \"" + name +
                "\"\n");
        }

        var manifest = Path.Combine(_directory, "elm.json");

        var json =
            """
            {"type":"application","source-directories":["src","first"],"elm-version":"0.19.1","dependencies":{"direct":{"elm/core":"1.0.5"},"indirect":{}},"test-dependencies":{"direct":{},"indirect":{}}}
            """;

        File.WriteAllText(manifest, json);
        var destination = Path.Combine(_directory, "output");
        using var cancellation = new CancellationTokenSource(s_wait);
        using var error = new NotifyingWriter();
        using var output = new StringWriter();

        var watching =
            Task.Run(
                () => Execute(
                    "text",
                    [new("value")],
                    destination,
                    cancellation.Token,
                    error,
                    output,
                    entryPoint: "src/Batch.elm/text",
                    inputDirectory: _directory));

        try
        {
            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("valuefirst");
            File.WriteAllText(manifest, json.Replace("first", "second"));
            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("valuesecond");
            error.ToString().Should().Contain("Program source changed: " + manifest);
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
    public async Task Run_watch_remote_program_requires_a_local_input_but_accepts_mixed_inputs(bool localInput)
    {
        var fixture = RemoteFixture();
        var url = "https://github.com/owner/repo/blob/" + fixture.Commit + "/";

        var loader =
            new RunSource.Loader(
                Path.Combine(_directory, "cache"),
                TextWriter.Null,
                openRemote: (_, _, _) => fixture.Open());

        var input = Path.Combine(_directory, "input.txt");
        File.WriteAllText(input, "local");
        var destination = Path.Combine(_directory, "output");
        using var cancellation = new CancellationTokenSource(s_wait);
        using var error = new NotifyingWriter();
        using var output = new StringWriter();

        var watching =
            Task.Run(
                () => Execute(
                    "join",
                    [new(localInput ? input : "literal", localInput), new(url + "input.txt", true), new("!")],
                    destination,
                    cancellation.Token,
                    error,
                    output,
                    entryPoint: url + "Batch.elm/join",
                    sourceLoader: loader,
                    createWatcher:
                    localInput
                    ?
                    null
                    :
                    _ => throw new InvalidOperationException("No local sources must not start a watcher")));

        try
        {
            if (!localInput)
            {
                (await watching.WaitAsync(s_wait)).Should().Be(1);
                error.ToString().Should().Contain("local program sources or at least one local --input-file");
                File.Exists(destination).Should().BeFalse();
                return;
            }

            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("localvalue!");
            File.WriteAllText(input, "changed");
            await error.NextWaiting();
            File.ReadAllText(destination).Should().Be("changedvalue!");
            error.ToString().Should().Contain("Input file changed: " + input).And.NotContain("Watching program sources");
        }
        finally
        {
            cancellation.Cancel();
            (await watching.WaitAsync(s_wait)).Should().Be(localInput ? 130 : 1);
        }
    }

    private GitRepositoryFixture RemoteFixture() =>
        new(
            FileTree.FromSetOfFilesWithStringPath(
                new[]
                {
                    ((IReadOnlyList<string>)["Batch.elm"], (ReadOnlyMemory<byte>)File.ReadAllBytes(Module)),
                    ((IReadOnlyList<string>)["input.txt"], (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes("value"))
                }));

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
        Func<IReadOnlyList<string>, IFileWatcher>? createWatcher = null,
        bool reportDurations = false,
        string? entryPoint = null,
        string? inputDirectory = null,
        RunSource.Loader? sourceLoader = null,
        Func<IReadOnlyList<RunDeclarationSelection.Candidate>, string>? selectDeclaration = null) =>
        RunCommand.Execute(
            entryPoint ?? Module + "/" + declaration,
            inputDirectory: inputDirectory,
            inputs: inputs,
            outputFile: destination,
            transformFile: transformFile,
            color: CommandLineColorMode.Never,
            timeout: timeout ?? s_wait,
            output: output,
            error: error,
            reportDurations: reportDurations,
            cacheDirectory: Path.Combine(_directory, "cache"),
            sourceLoader: sourceLoader,
            cancellationToken: token,
            inputReader: TextReader.Null,
            batchOnly: batchOnly,
            selectDeclaration: selectDeclaration,
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
