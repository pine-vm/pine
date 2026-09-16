using AwesomeAssertions;
using Xunit;

namespace Pine.Core.Tests.CodeAnalysis;

public class CodeAnalysisTests
{
    [Fact]
    public void Count_occurrences_in_self()
    {
        var expression =
            new Expression.Label(
                "target",
                Expression.LitralInst(PineValue.Blob([1])));

        Pine.Core.CodeAnalysis.CodeAnalysis.CountOccurrencesInSelfAndDescendants(
            expression,
            expression)
            .Should().Be(1);
    }

    [Fact]
    public void Count_occurrences_in_every_composite_expression_branch()
    {
        var target =
            new Expression.Label(
                "target",
                Expression.LitralInst(PineValue.Blob([2])));

        var expression =
            Expression.ListInst(
                [
                target,
                Expression.BuiltinInst("identity", target),
                new Expression.Eval(
                    encoded: target,
                    environment: target),
                Expression.ConditionalInst(
                    condition: target,
                    falseBranch: target,
                    trueBranch: target),
                new Expression.Label("wrapper", target)
                ]);

        Pine.Core.CodeAnalysis.CodeAnalysis.CountOccurrencesInSelfAndDescendants(
            expression,
            target)
            .Should().Be(8);
    }

    [Fact]
    public void Count_occurrences_finds_equal_expressions_with_different_references()
    {
        var target =
            new Expression.Label(
                "target",
                Expression.LitralInst(PineValue.Blob([3])));

        var structurallyEqual =
            new Expression.Label(
                "target",
                Expression.LitralInst(PineValue.Blob([3])));

        structurallyEqual.Should().Be(target);
        structurallyEqual.Should().NotBeSameAs(target);

        var expression =
            Expression.ListInst(
                [
                structurallyEqual,
                target,
                structurallyEqual
                ]);

        Pine.Core.CodeAnalysis.CodeAnalysis.CountOccurrencesInSelfAndDescendants(
            expression,
            target)
            .Should().Be(3);
    }

    [Fact]
    public void Count_occurrences_returns_zero_when_absent_from_leaf_expressions()
    {
        var searchedExpression =
            new Expression.Label(
                "target",
                Expression.LitralInst(PineValue.Blob([4])));

        Pine.Core.CodeAnalysis.CodeAnalysis.CountOccurrencesInSelfAndDescendants(
            Expression.EnvironmentInstance,
            searchedExpression)
            .Should().Be(0);

        Pine.Core.CodeAnalysis.CodeAnalysis.CountOccurrencesInSelfAndDescendants(
            Expression.LitralInst(PineValue.Blob([5])),
            searchedExpression)
            .Should().Be(0);
    }

    [Fact]
    public void Count_occurrences_prunes_using_each_aggregate_stat()
    {
        var literal = Expression.LitralInst(PineValue.EmptyList);

        (Expression current, Expression searched)[] testCases =
            [
            (new SyntheticExpression(
                subexpressionCount: 0,
                referencesEnvironment: false,
                evalCount: 0,
                conditionCount: 0,
                builtinCount: 0,
                maxDepth: 1),
            Expression.ListInst([literal])),
            (new SyntheticExpression(
                subexpressionCount: 2,
                referencesEnvironment: false,
                evalCount: 0,
                conditionCount: 0,
                builtinCount: 0,
                maxDepth: 1),
            new Expression.Eval(literal, literal)),
            (new SyntheticExpression(
                subexpressionCount: 3,
                referencesEnvironment: false,
                evalCount: 0,
                conditionCount: 0,
                builtinCount: 0,
                maxDepth: 1),
            Expression.ConditionalInst(literal, literal, literal)),
            (new SyntheticExpression(
                subexpressionCount: 1,
                referencesEnvironment: false,
                evalCount: 0,
                conditionCount: 0,
                builtinCount: 0,
                maxDepth: 1),
            Expression.BuiltinInst("identity", literal)),
            (new SyntheticExpression(
                subexpressionCount: 0,
                referencesEnvironment: false,
                evalCount: 0,
                conditionCount: 0,
                builtinCount: 0,
                maxDepth: 0),
            Expression.EnvironmentInstance),
            (new SyntheticExpression(
                subexpressionCount: 1,
                referencesEnvironment: false,
                evalCount: 0,
                conditionCount: 0,
                builtinCount: 0,
                maxDepth: 0),
            new Expression.Label("target", literal))
            ];

        foreach (var (current, searched) in testCases)
        {
            Pine.Core.CodeAnalysis.CodeAnalysis.CountOccurrencesInSelfAndDescendants(
                current,
                searched)
                .Should().Be(0);
        }
    }

    private sealed record SyntheticExpression : Expression
    {
        public SyntheticExpression(
            long subexpressionCount,
            bool referencesEnvironment,
            long evalCount,
            long conditionCount,
            long builtinCount,
            int maxDepth)
        {
            SubexpressionCount = subexpressionCount;
            ReferencesEnvironment = referencesEnvironment;
            EvalCount = evalCount;
            ConditionCount = conditionCount;
            BuiltinCount = builtinCount;
            MaxDepth = maxDepth;
        }

        public override long SubexpressionCount { get; }

        public override bool ReferencesEnvironment { get; }

        public override long EvalCount { get; }

        public override long ConditionCount { get; }

        public override long BuiltinCount { get; }

        public override int MaxDepth { get; }
    }
}
