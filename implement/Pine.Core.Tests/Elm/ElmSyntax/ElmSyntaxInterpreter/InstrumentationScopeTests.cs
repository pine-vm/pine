using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System;
using System.Linq;
using Xunit;

using ElmInterpreter = Pine.Core.Elm.ElmSyntax.ElmSyntaxInterpreter;
using FunctionStepCountingInvocationLogger = Pine.Core.Elm.ElmSyntax.FunctionStepCountingInvocationLogger;
using PineValueInProcess = Pine.Core.Internal.PineValueInProcess;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxInterpreter;

/// <summary>
/// Exercises the ambient instrumentation hook
/// (<see cref="ElmInterpreter.BeginInstrumentationScope(Pine.Core.Elm.ElmSyntax.IInvocationLogger)"/>)
/// together with <see cref="FunctionStepCountingInvocationLogger"/>. The scope makes it
/// possible to attribute interpreter steps to individual functions even for entry points that
/// don't otherwise thread a logger through their signature — which is how the hottest
/// functions of a workload are identified for promotion to direct C# builtins.
/// </summary>
public class InstrumentationScopeTests
{
    private static readonly Lazy<ElmInterpreter.Prepared> s_prepared =
        new(() => InterpreterTestHelper.PrepareKernelModules(
            "List.elm",
            "Basics.elm",
            "Maybe.elm",
            "Char.elm"));

    [Fact]
    public void Ambient_logger_attributes_direct_applications_per_function()
    {
        var logger = new FunctionStepCountingInvocationLogger();

        using (ElmInterpreter.BeginInstrumentationScope(logger))
        {
            ElmInterpreter.AmbientInvocationLogger.Should().BeSameAs(logger);

            // Uses the direct-call Interpret path, which does not accept a logger argument and
            // therefore relies on the ambient logger installed by the scope.
            var arguments = new[] { PineValueInProcess.Create(IntegerEncoding.EncodeSignedInteger(5)) };

            var result =
                ElmInterpreter.Interpret(
                    DeclQualifiedName.Create(["List"], "range"),
                    [PineValueInProcess.Create(IntegerEncoding.EncodeSignedInteger(1)), .. arguments],
                    s_prepared.Value);

            result.IsOkOrNull().Should().NotBeNull();
        }

        // The scope has been disposed, so no ambient logger remains installed.
        ElmInterpreter.AmbientInvocationLogger.Should().BeNull();

        // The evaluation dispatched at least one direct application, and the aggregate matches
        // the sum of the per-function tallies.
        logger.Counters.DirectFunctionApplicationCount.Should().BeGreaterThan(0);

        logger.DirectApplicationCounts.Values.Sum()
            .Should().Be(logger.Counters.DirectFunctionApplicationCount);

        // The recursive helper List.rangeHelp is dispatched from the body, so it must appear in
        // the per-function breakdown. (The root call itself enters via the direct-call path and
        // is not re-counted as a nested application.)
        logger.DirectApplicationCounts.Keys
            .Select(name => name.FullName)
            .Should().Contain("List.rangeHelp");

        // The "top functions" view is ordered by descending step count.
        var top = logger.TopFunctionsByStepCount();

        top.Should().BeInDescendingOrder(kv => kv.Value);
    }

    [Fact]
    public void Instrumentation_scopes_nest_and_restore_the_previous_logger()
    {
        ElmInterpreter.AmbientInvocationLogger.Should().BeNull();

        var outer = new FunctionStepCountingInvocationLogger();
        var inner = new FunctionStepCountingInvocationLogger();

        using (ElmInterpreter.BeginInstrumentationScope(outer))
        {
            ElmInterpreter.AmbientInvocationLogger.Should().BeSameAs(outer);

            using (ElmInterpreter.BeginInstrumentationScope(inner))
            {
                ElmInterpreter.AmbientInvocationLogger.Should().BeSameAs(inner);
            }

            ElmInterpreter.AmbientInvocationLogger.Should().BeSameAs(outer);
        }

        ElmInterpreter.AmbientInvocationLogger.Should().BeNull();
    }
}
