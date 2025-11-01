using AwesomeAssertions;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests;

public class ExpressionTests
{
    [Fact]
    public void Expression_aggregate_properties_equal_derived_from_generic_enumeration()
    {
        IReadOnlyList<Expression> testCases =
            [
            Expression.EnvironmentInstance,

            Expression.LiteralInstance(PineValue.EmptyList),

            Expression.ListInstance([]),
            Expression.ListInstance([Expression.EnvironmentInstance]),
            Expression.ListInstance(
                [
                Expression.EnvironmentInstance,
                Expression.LiteralInstance(PineValue.EmptyList)
                ]),

            Expression.ListInstance(
                [
                Expression.EnvironmentInstance,
                Expression.ListInstance(
                    [
                    Expression.EnvironmentInstance,
                    Expression.LiteralInstance(PineValue.EmptyList),
                    ]),
                Expression.ListInstance([]),
                ]),

            Expression.ConditionalInstance(
                Expression.ListInstance([]),
                Expression.ListInstance(
                    [
                    Expression.EnvironmentInstance,
                    Expression.LiteralInstance(PineValue.EmptyList),
                    ]),
                Expression.ListInstance([])),

            Expression.ConditionalInstance(
                Expression.ListInstance([]),
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(PineValue.EmptyBlob),
                    Expression.LiteralInstance(PineValue.EmptyList),
                    ]),
                Expression.ListInstance([])),

            new Expression.ParseAndEval(
                Expression.EnvironmentInstance,
                Expression.EnvironmentInstance),

            new Expression.KernelApplication(
                "function",
                Expression.EnvironmentInstance),

            new Expression.StringTag(
                "test",
                Expression.ListInstance([])),

            new Expression.StringTag(
                "test",
                Expression.EnvironmentInstance),

            new Expression.StringTag(
                "test",
                Expression.ListInstance([Expression.EnvironmentInstance])),
            ];

        foreach (var testCase in testCases)
        {
            var rootAndSubexpressions =
                Expression.EnumerateSelfAndDescendants(testCase)
                .ToList();

            var subexpressions =
                rootAndSubexpressions
                .Skip(1)
                .ToList();

            var anyNodeIsEnvironment =
                rootAndSubexpressions.OfType<Expression.Environment>().Any();

            testCase.SubexpressionCount.Should().Be(subexpressions.Count);
            testCase.ReferencesEnvironment.Should().Be(anyNodeIsEnvironment);
        }
    }
}
