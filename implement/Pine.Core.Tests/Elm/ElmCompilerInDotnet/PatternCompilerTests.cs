using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class PatternCompilerTests
{
    [Fact]
    public void Case_scrutinee_with_runtime_invocation_and_high_duplication_is_not_inlined()
    {
        var runtimeInvocation =
            new Expression.Eval(
                encoded: Expression.LitralInst(PineValue.EmptyList),
                environment: Expression.EnvironmentInstance);

        PatternCompiler.ShouldCompileCaseBlockViaInvocation(runtimeInvocation, inlinedScrutineeOccurrences: 600)
            .Should().BeTrue();
    }

    [Fact]
    public void Case_scrutinee_with_single_runtime_invocation_use_is_inlined()
    {
        var runtimeInvocation =
            new Expression.Eval(
                encoded: Expression.LitralInst(PineValue.EmptyList),
                environment: Expression.EnvironmentInstance);

        PatternCompiler.ShouldCompileCaseBlockViaInvocation(runtimeInvocation, inlinedScrutineeOccurrences: 1)
            .Should().BeFalse();
    }

    [Fact]
    public void Small_case_scrutinee_with_runtime_invocation_is_inlined_within_expansion_budget()
    {
        var runtimeInvocation =
            new Expression.Eval(
                encoded: Expression.LitralInst(PineValue.EmptyList),
                environment: Expression.EnvironmentInstance);

        PatternCompiler.ShouldCompileCaseBlockViaInvocation(runtimeInvocation, inlinedScrutineeOccurrences: 400)
            .Should().BeFalse();
    }

    [Fact]
    public void Cheap_case_scrutinee_is_inlined()
    {
        PatternCompiler.ShouldCompileCaseBlockViaInvocation(
            Expression.EnvironmentInstance,
            inlinedScrutineeOccurrences: 2)
            .Should().BeFalse();
    }
}
