using Pine.Core.CLI;
using Pine.Core.Elm.Testing;
using System;
using System.CommandLine;
using System.Globalization;
using System.IO;
using System.Linq;

namespace Pine.CLI.Elm;

internal sealed record TestBudgetOptions(
    Option<int?> Budget, Option<int?> Invocations, Option<int?> Loops,
    Option<double?> Timeout, Option<int> StackDepth)
{
    private sealed class CancellationLease : IDisposable
    {
        private readonly ConsoleCancelEventHandler _handler;

        public CancellationLease(ElmTestInstrumentation tracker)
        {
            _handler =
                (_, args) =>
                {
                    args.Cancel = true;
                    tracker.Cancel();
                };

            Console.CancelKeyPress += _handler;
        }

        public void Dispose() => Console.CancelKeyPress -= _handler;
    }

    public static IDisposable RegisterCancellation(ElmTestInstrumentation tracker) => new CancellationLease(tracker);

    public bool HasExplicitOptions(ParseResult result) =>
        new Option[] { Budget, Invocations, Loops, Timeout, StackDepth }
        .Any(option => result.GetResult(option)?.IdentifierTokenCount > 0);

    public void MoveAfterCommonOptions(Command command)
    {
        foreach (var option in new Option[] { Budget, Invocations, Loops, Timeout, StackDepth })
        {
            command.Options.Remove(option);
            command.Add(option);
        }
    }

    public static TestBudgetOptions AddTo(Command command)
    {
        var options =
            new TestBudgetOptions(
                new("--budget") { Description = "Shortcut setting both invocation and loop budgets; explicit per-kind options override it. Counts accept underscores and SI units k/M/G." },
                new("--invocation-budget") { Description = "Command-wide VM invocation budget, including preparation. Counts accept underscores and SI units k/M/G." },
                new("--loop-budget") { Description = "Command-wide backward-jump iteration budget, including preparation. Counts accept underscores and SI units k/M/G." },
                new("--timeout") { Description = "Cooperative wall-clock timeout in seconds, including preparation. Integer times accept underscores and ms/s/min/h units." },
                new("--max-stack-depth") { Description = "VM stack-depth safety limit. Counts accept underscores and SI units k/M/G.", DefaultValueFactory = _ => 100_000 });

        foreach (var option in new Option[] { options.Budget, options.Invocations, options.Loops, options.Timeout, options.StackDepth })
        {
            option.Recursive = true;
            command.Add(option);

            option.Validators.Add(
                result =>
                {
                    if (result.IdentifierTokenCount > 1)
                        result.AddError($"{option.Name} can only be specified once.");
                });
        }

        foreach (var option in new[] { options.Budget, options.Invocations, options.Loops })
            NumericOptionParsing.SetCountParser(
                option,
                value => value is <= 0 ? $"{option.Name} must be positive." : null);

        NumericOptionParsing.SetCountParser(
            options.StackDepth,
            value => value <= 0 ? "--max-stack-depth must be positive." : null);

        NumericOptionParsing.SetTimeParser(
            options.Timeout,
            TimeUnit.Seconds,
            value =>
            value is { } seconds &&
            (!double.IsFinite(seconds) || seconds <= 0 || seconds * 1000 > uint.MaxValue - 1)
            ?
            "--timeout must be finite, positive, and fit the timer range."
            :
            null);

        return options;
    }

    public ElmTestEvaluationOptions Read(ParseResult result)
    {
        var common = result.GetValue(Budget);

        return
            new()
            {
                InvocationBudget = result.GetValue(Invocations) ?? common,
                LoopBudget = result.GetValue(Loops) ?? common,
                Timeout = result.GetValue(Timeout) is { } seconds ? TimeSpan.FromSeconds(seconds) : null,
                StackDepthLimit = result.GetValue(StackDepth),
            };
    }

    public static void PrintEffective(TextWriter writer, ElmTestEvaluationOptions limits)
    {
        static string Count(int? value) =>
            value is { } number ? CommandLineInterface.FormatIntegerForDisplay(number) : "unbounded";

        writer.WriteLine("Effective execution limits:");
        writer.WriteLine("  Invocation budget: " + Count(limits.InvocationBudget));
        writer.WriteLine("  Loop budget: " + Count(limits.LoopBudget));

        writer.WriteLine(
            "  Timeout: " +
            (limits.Timeout is { } timeout
            ?
            FormatSeconds(timeout) + " seconds (cooperative)"
            :
            "unbounded"));

        writer.WriteLine("  Stack-depth limit: " + CommandLineInterface.FormatIntegerForDisplay(limits.StackDepthLimit));

        if (limits.InvocationBudget is not null && limits.LoopBudget is null)
        {
            writer.WriteLine(
                "Warning: an invocation budget alone does not bound backward-jump loops. Set --loop-budget or --budget; a timeout also provides cooperative cancellation.");
        }

        writer.Flush();
    }

    private static string FormatSeconds(TimeSpan timeout)
    {
        var seconds = timeout.Ticks / TimeSpan.TicksPerSecond;
        var remainder = timeout.Ticks % TimeSpan.TicksPerSecond;

        return
            CommandLineInterface.FormatIntegerForDisplay(seconds) +
            (remainder is 0 ? "" : "." + remainder.ToString("D7", CultureInfo.InvariantCulture).TrimEnd('0'));
    }
}
