using Pine.Core;
using Pine.Core.Elm.Testing;
using Spectre.Console;
using System;
using System.CommandLine;
using System.Globalization;
using System.IO;
using System.Linq;
using System.Security.Cryptography;
using System.Text.Json;

namespace Pine.CLI.Elm;

public enum TestProfileSort
{
    Invocations, Instructions, Loops
}

public sealed record TestProfileSettings
{
    public ElmTestInstrumentationOptions Instrumentation { get; init; } = new();

    public string? OutputPath { get; init; }

    public int Top { get; init; } = 20;

    public TestProfileSort Sort { get; init; }

    public bool ShowStackTraces { get; init; } = true;

    public bool ShowExpressions { get; init; }
}

public static class TestProfileCommand
{
    private static readonly Style s_headingStyle = new(Color.Cyan, decoration: Decoration.Bold);

    private static readonly Style s_mutedStyle = new(Color.Grey);

    private static readonly Style s_successStyle = new(Color.Green, decoration: Decoration.Bold);

    private static readonly Style s_warningStyle = new(Color.Yellow, decoration: Decoration.Bold);

    private static readonly Style s_failureStyle = new(Color.Red, decoration: Decoration.Bold);

    internal sealed record Options(
        TestBudgetOptions Budgets,
        Option<double> Interval, Option<int> TraceDepth,
        Option<int> Top, Option<TestProfileSort> Sort, Option<string?> Output,
        Option<bool> Inputs, Option<bool> Locals, Option<bool> Expressions, Option<bool> NoStacks,
        Option<bool> NoLeaves, Option<bool> NoCache, Option<bool> NoTail, Option<bool> NoReduction)
    {
        public TestProfileSettings Read(ParseResult result)
        {
            var limits = Budgets.Read(result);

            return
                new()
                {
                    OutputPath = result.GetValue(Output),
                    Top = result.GetValue(Top),
                    Sort = result.GetValue(Sort),
                    ShowStackTraces = !result.GetValue(NoStacks),
                    ShowExpressions = result.GetValue(Expressions),
                    Instrumentation =
                    new()
                    {
                        InvocationBudget = limits.InvocationBudget,
                        LoopBudget = limits.LoopBudget,
                        Timeout = limits.Timeout,
                        SnapshotInterval = TimeSpan.FromSeconds(result.GetValue(Interval)),
                        StackDepthLimit = limits.StackDepthLimit,
                        StackTraceDepth = result.GetValue(TraceDepth),
                        IncludeInputs = result.GetValue(Inputs),
                        IncludeLocals = result.GetValue(Locals),
                        DisablePrecompiledLeaves = result.GetValue(NoLeaves),
                        DisableInvocationCache = result.GetValue(NoCache),
                        DisableTailRecursion = result.GetValue(NoTail),
                        DisableReduction = result.GetValue(NoReduction),
                    },
                };
        }
    }

    internal static Options AddOptions(Command command, TestBudgetOptions budgets)
    {
        var options =
            new Options(
                budgets,
                new("--interval") { Description = "Live status/stack sampling interval in seconds; 0 disables periodic samples.", DefaultValueFactory = _ => 5 },
                new("--stack-depth") { Description = "Maximum recorded frames per stack trace.", DefaultValueFactory = _ => 20 },
                new("--top") { Description = "Expressions displayed in the ranking; all recorded expressions are saved.", DefaultValueFactory = _ => 20 },
                new("--sort") { Description = "Rank by Invocations, Instructions, or Loops." },
                new("--output") { Description = "JSON report path; defaults to elm-stuff/pine/test-profiles in the project." },
                new("--include-inputs") { Description = "Record input paths and already materialized input values; never force lazy values." },
                new("--include-locals") { Description = "Query and record VM locals in sampled/stopped stack frames." },
                new("--expressions") { Description = "Display expression descriptions; complete encoded expressions are always saved." },
                new("--no-stacks") { Description = "Hide stack traces in terminal output; recorded traces remain in JSON." },
                new("--no-precompiled-leaves") { Description = "Disable native precompiled leaves to inspect pure Pine execution." },
                new("--no-invocation-cache") { Description = "Disable invocation-result caching." },
                new("--no-tail-recursion") { Description = "Disable tail-call frame replacement (compiler-generated backward jumps can remain)." },
                new("--no-reduction") { Description = "Disable expression reduction during VM compilation." });

        foreach (var option in new Option[]
        {
            options.Interval, options.TraceDepth, options.Top, options.Sort, options.Output,
            options.Inputs, options.Locals, options.Expressions, options.NoStacks,
            options.NoLeaves, options.NoCache, options.NoTail, options.NoReduction,
        })
            command.Add(option);

        options.Interval.Validators.Add(
            result =>
            {
                if (result.Tokens.Count == 1 &&
                    double.TryParse(
                        result.Tokens[0].Value,
                        NumberStyles.Float,
                        CultureInfo.InvariantCulture,
                        out var seconds) &&
                    (!double.IsFinite(seconds) || seconds < 0 || seconds >= TimeSpan.MaxValue.TotalSeconds))
                    result.AddError("--interval must be finite, nonnegative, and fit the TimeSpan range.");
            });

        return options;
    }

    public static int Execute(
        string source,
        string? filter,
        TestProfileSettings settings,
        Func<ElmTestInstrumentation, int> run,
        IAnsiConsole? console = null,
        FormatCommandColorMode? colorMode = null)
    {
        FormatCommandColorMode resolvedColorMode;

        try
        {
            resolvedColorMode =
                FormatCommandShared.ResolveColorMode(
                    colorMode,
                    Environment.GetEnvironmentVariable(FormatCommandShared.ColorEnvironmentVariable));
        }
        catch (ArgumentException exception)
        {
            (console?.Profile.Out.Writer ?? Console.Error).WriteLine("Error: " + exception.Message);
            return 1;
        }

        console ??= TestCommand.CreateSystemConsole(Console.Out, resolvedColorMode);

        void Write(string text, Style? style = null) =>
            console.Write(
                new Text(
                    text + "\n",
                    resolvedColorMode is FormatCommandColorMode.Never ? Style.Plain : style ?? Style.Plain));

        string path;

        try
        {
            settings.Instrumentation.Validate();

            if (settings.Top <= 0)
                throw new ArgumentException("--top must be positive.");

            if (!Enum.IsDefined(settings.Sort))
                throw new ArgumentException("Unknown expression ranking: " + settings.Sort);

            path =
                Path.GetFullPath(
                    settings.OutputPath ??
                    Path.Combine(
                        source,
                        "elm-stuff",
                        "pine",
                        "test-profiles",
                        DateTime.UtcNow.ToString("yyyyMMddTHHmmssfffffff") + "-" + Guid.NewGuid().ToString("N")[..8] +
                        ".json"));
        }
        catch (Exception exception) when (exception is ArgumentException or NotSupportedException)
        {
            Write("Error: " + exception.Message, s_failureStyle);
            return 1;
        }

        TestBudgetOptions.PrintEffective(console.Profile.Out.Writer, settings.Instrumentation);

        var saveAnnounced = false;

        void AnnounceSave()
        {
            if (saveAnnounced)
                return;

            saveAnnounced = true;
            Write("Saving all recorded profile details to: " + path, s_headingStyle);

            Write(
                "Serializing expression/value graphs and captured diagnostics; this may take additional time.",
                s_mutedStyle);

            console.Profile.Out.Writer.Flush();
        }

        void Stats(ElmTestProfileSummary summary, string heading)
        {
            Write(
                heading + (summary.StopReason is { } reason ? " " + reason : ""),
                summary.StopReason is null ? s_headingStyle : s_warningStyle);

            Write($"Phase: {summary.Phase}; context: {summary.Context}", s_mutedStyle);

            Write(
                $"Elapsed: {CommandLineInterface.FormatIntegerForDisplay((long)Math.Round(summary.ElapsedMilliseconds))} ms; " +
                $"invocations: {CommandLineInterface.FormatIntegerForDisplay(summary.Counters.InvocationCount)}; " +
                $"loops: {CommandLineInterface.FormatIntegerForDisplay(summary.Counters.LoopIterationCount)}; " +
                $"instructions: {CommandLineInterface.FormatIntegerForDisplay(summary.Counters.InstructionCount)}");
        }

        using var instrumentation =
            new ElmTestInstrumentation(
                settings.Instrumentation,
                precompiledLeavesProvider: () => IntermediateVM.SetupVM.DefaultPrecompiledLeaves)
            {
                OnProgress = summary => Stats(summary, "Instrumentation progress."),
                OnStopped =
                summary =>
                {
                    Stats(summary, "Execution stopped.");
                    AnnounceSave();
                },
            };

        void Cancel(object? _, ConsoleCancelEventArgs args)
        {
            args.Cancel = true;
            instrumentation.Cancel();
        }

        Console.CancelKeyPress += Cancel;

        try
        {
            var exitCode = run(instrumentation);
            instrumentation.SetOutcome(exitCode == 0 ? "completed" : "failed");
            Stats(instrumentation.GetSummary(), "Overall instrumentation stats.");
            AnnounceSave();
            var report = instrumentation.GetReport();
            string hash;

            try
            {
                Directory.CreateDirectory(Path.GetDirectoryName(path)!);

                using (var stream = File.Create(path))
                    JsonSerializer.Serialize(stream, report, new JsonSerializerOptions { WriteIndented = true });

                using var savedFile = File.OpenRead(path);
                hash = Convert.ToHexStringLower(SHA256.HashData(savedFile));
            }
            catch (Exception exception) when (exception is IOException or UnauthorizedAccessException)
            {
                Write("Error saving instrumentation report: " + exception.Message, s_failureStyle);
                return 1;
            }

            Write("JSON SHA256: " + hash, s_successStyle);
            Write("Saved JSON report: " + path, s_successStyle);
            Stats(report.Summary, "Overall run stats (excluding serialization).");
            Render(report, settings, Write);
            return exitCode;
        }
        finally
        {
            Console.CancelKeyPress -= Cancel;
        }
    }

    internal static void Render(
        ElmTestProfileReport report,
        TestProfileSettings settings,
        Action<string, Style?> write)
    {
        long Metric(ElmTestExpressionProfile row) =>
            settings.Sort switch
            {
                TestProfileSort.Invocations => row.Invocations,
                TestProfileSort.Instructions => row.Instructions,
                TestProfileSort.Loops => row.LoopIterations,

                _ =>
                throw new NotImplementedException("Render does not handle ranking: " + settings.Sort),
            };

        var rows =
            report.Expressions
            .OrderByDescending(Metric).ThenBy(row => row.Hash, StringComparer.Ordinal).Take(settings.Top)
            .Select(
                row => (Profile: row,
                Invocations: CommandLineInterface.FormatIntegerForDisplay(row.Invocations),
                Loops: CommandLineInterface.FormatIntegerForDisplay(row.LoopIterations),
                Instructions: CommandLineInterface.FormatIntegerForDisplay(row.Instructions)))
            .ToArray();

        var invocationWidth =
            Math.Max("Invocations".Length, rows.Select(row => row.Invocations.Length).DefaultIfEmpty().Max());

        var loopWidth = Math.Max("Loops".Length, rows.Select(row => row.Loops.Length).DefaultIfEmpty().Max());

        var instructionWidth =
            Math.Max("Instructions".Length, rows.Select(row => row.Instructions.Length).DefaultIfEmpty().Max());

        write("Pine expression ranking (" + settings.Sort + "):", s_headingStyle);

        write(
            "Expression".PadRight(16) + "  " + "Invocations".PadLeft(invocationWidth) +
            "  " +
            "Loops".PadLeft(loopWidth) +
            "  " +
            "Instructions".PadLeft(instructionWidth) +
            "  Declarations",
            s_headingStyle);

        foreach (var row in rows)
        {
            write(
                row.Profile.Hash[..16] + "  " + row.Invocations.PadLeft(invocationWidth) +
                "  " + row.Loops.PadLeft(loopWidth) + "  " + row.Instructions.PadLeft(instructionWidth) +
                "  " + string.Join(", ", row.Profile.Declarations),
                null);

            if (settings.ShowExpressions)
                write("  " + row.Profile.Description, s_mutedStyle);
        }

        if (settings.ShowStackTraces && report.Samples.LastOrDefault(sample => sample.StackTrace.Count > 0) is { } last)
        {
            write("Last recorded stack trace (current frame first):", s_headingStyle);

            foreach (var frame in last.StackTrace)
            {
                write(
                    $"  {frame.ExpressionHash[..16]} at instruction {CommandLineInterface.FormatIntegerForDisplay(frame.InstructionPointer)}",
                    null);

                if (frame.Inputs is { } inputs)
                {
                    for (var i = 0; i < inputs.Count; i++)
                        write(
                            $"    input [{string.Join(",", frame.ParameterPaths![i].Select(number => CommandLineInterface.FormatIntegerForDisplay(number)))}]: {inputs[i].Preview}",
                            s_mutedStyle);
                }

                if (frame.Locals is { } locals)
                {
                    for (var i = 0; i < locals.Count; i++)
                        write(
                            $"    local {CommandLineInterface.FormatIntegerForDisplay(i)}: {locals[i].Preview}",
                            s_mutedStyle);
                }
            }
        }
    }
}
