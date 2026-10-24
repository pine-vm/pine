using AwesomeAssertions;
using Pine.Core.Elm.Testing;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Threading;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmTest;

public class TimedScopeTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Timed_scope_disposal_stops_timing_and_disposes_instrumentation_once(bool instrumented)
    {
        var disposalCount = 0;

        using var scope =
            new TimedScope(
                () => instrumented ? new CallbackScope(() => disposalCount++) : null);

        SpinWait.SpinUntil(() => scope.Elapsed > TimeSpan.Zero, TimeSpan.FromSeconds(5))
            .Should().BeTrue();

        scope.Dispose();
        var elapsed = scope.Elapsed;
        Thread.Sleep(10);
        scope.Dispose();

        elapsed.Should().BeGreaterThan(TimeSpan.Zero);
        scope.Elapsed.Should().Be(elapsed);
        disposalCount.Should().Be(instrumented ? 1 : 0);
    }

    [Fact]
    public void Timed_scope_repeated_disposal_does_not_restore_over_a_new_phase()
    {
        using var instrumentation =
            new ElmTestInstrumentation(new() { DisablePrecompiledLeaves = true });

        var vm = instrumentation.CreateVm(new ConcurrentInvocationCache(), new PineVMSharedCaches());

        using var preparation =
            new TimedScope(() => instrumentation.EnterScope("preparation", "suite"));

        preparation.Dispose();

        using var execution =
            new TimedScope(() => instrumentation.EnterScope("execution", "test"));

        preparation.Dispose();

        vm.EvaluateExpression(Expression.EnvironmentInstance, PineValue.EmptyList)
            .IsOkOrNull().Should().NotBeNull();

        instrumentation.GetSummary().CountersByPhase["execution"].Total.InstructionCount
            .Should().BeGreaterThan(0);

        instrumentation.GetSummary().CountersByPhase["preparation"].Total.InstructionCount
            .Should().Be(0);
    }

    [Fact]
    public void Timed_scope_stops_timing_when_instrumentation_disposal_throws()
    {
        using var scope =
            new TimedScope(
                () => new CallbackScope(() => throw new InvalidOperationException("Disposal failed")));

        Action dispose = scope.Dispose;
        dispose.Should().Throw<InvalidOperationException>();
        var elapsed = scope.Elapsed;
        scope.Dispose();
        scope.Elapsed.Should().Be(elapsed);
    }

    [Fact]
    public void Timed_scope_ends_both_on_exceptional_exit()
    {
        var disposalCount = 0;
        TimedScope? scope = null;

        Action run =
            () =>
            {
                using var activeScope =
                    new TimedScope(() => new CallbackScope(() => disposalCount++));

                scope = activeScope;
                throw new InvalidOperationException("Work failed");
            };

        run.Should().Throw<InvalidOperationException>();
        disposalCount.Should().Be(1);
        scope.Should().NotBeNull();
        var elapsed = scope!.Elapsed;
        scope.Dispose();
        scope.Elapsed.Should().Be(elapsed);
    }

    private sealed class CallbackScope(Action dispose) : IDisposable
    {
        public void Dispose() => dispose();
    }
}
