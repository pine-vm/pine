using AwesomeAssertions;
using Pine.Core.CommonEncodings;
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

            Expression.KernelApplicationInstance(
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

            // ParseAndEval nested inside a List subexpression.
            Expression.ListInstance(
                [
                Expression.LiteralInstance(PineValue.EmptyList),
                new Expression.ParseAndEval(
                    Expression.EnvironmentInstance,
                    Expression.EnvironmentInstance),
                ]),

            // ParseAndEval nested in the input of a KernelApplication.
            Expression.KernelApplicationInstance(
                "function",
                new Expression.ParseAndEval(
                    Expression.LiteralInstance(PineValue.EmptyList),
                    Expression.EnvironmentInstance)),

            // ParseAndEval nested in a branch of a Conditional.
            Expression.ConditionalInstance(
                Expression.ListInstance([]),
                Expression.ListInstance([]),
                new Expression.ParseAndEval(
                    Expression.EnvironmentInstance,
                    Expression.LiteralInstance(PineValue.EmptyList))),

            // ParseAndEval nested inside a StringTag.
            new Expression.StringTag(
                "test",
                new Expression.ParseAndEval(
                    Expression.EnvironmentInstance,
                    Expression.EnvironmentInstance)),

            // ParseAndEval with another ParseAndEval nested in its parts.
            new Expression.ParseAndEval(
                new Expression.ParseAndEval(
                    Expression.EnvironmentInstance,
                    Expression.LiteralInstance(PineValue.EmptyList)),
                Expression.EnvironmentInstance),

            Expression.KernelApplicationInstance(
                nameof(BuiltinFunction.int_add),
                Expression.KernelApplicationInstance(
                    nameof(BuiltinFunction.int_mul),
                    Expression.LiteralInstance(PineValue.EmptyList))),

            Expression.ListInstance(
                [
                Expression.KernelApplicationInstance(
                    nameof(BuiltinFunction.int_add),
                    Expression.LiteralInstance(PineValue.EmptyList)),

                Expression.KernelApplicationInstance(
                    nameof(BuiltinFunction.int_add),
                    Expression.LiteralInstance(PineValue.EmptyList)),
                ]),

            Expression.ConditionalInstance(
                Expression.KernelApplicationInstance(
                    nameof(BuiltinFunction.int_add),
                    Expression.LiteralInstance(PineValue.EmptyList)),
                Expression.KernelApplicationInstance(
                    nameof(BuiltinFunction.int_add),
                    Expression.LiteralInstance(PineValue.EmptyList)),
                Expression.KernelApplicationInstance(
                    nameof(BuiltinFunction.int_add),
                    Expression.LiteralInstance(PineValue.EmptyList))),

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

            var evalCount =
                rootAndSubexpressions.OfType<Expression.ParseAndEval>().Count();

            var conditionCount =
                rootAndSubexpressions.OfType<Expression.Conditional>().Count();

            var builtinCount =
                rootAndSubexpressions.OfType<Expression.KernelApplication>().Count();

            testCase.SubexpressionCount.Should().Be(subexpressions.Count);
            testCase.ReferencesEnvironment.Should().Be(anyNodeIsEnvironment);
            testCase.EvalCount.Should().Be(evalCount);
            testCase.ConditionCount.Should().Be(conditionCount);
            testCase.BuiltinCount.Should().Be(builtinCount);
        }
    }

    [Fact]
    public void Expression_deeply_nested_list_equality_does_not_overflow_stack()
    {
        // Build a deeply nested list expression. A recursive equality implementation would
        // overflow the call stack for nesting this deep.

        static Expression BuildDeeplyNestedList(int depth)
        {
            Expression current =
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(31));

            for (var i = 0; i < depth; ++i)
            {
                current = Expression.ListInstance([current]);
            }

            return current;
        }

        const int depth = 100_000;

        var exprA = BuildDeeplyNestedList(depth);
        var exprB = BuildDeeplyNestedList(depth);

        exprA.Equals(exprB).Should().BeTrue();
        exprA.GetHashCode().Should().Be(exprB.GetHashCode());

        // A difference at the deepest level must still be detected.

        static Expression BuildDeeplyNestedListWithInnerValue(int depth, long innerValue)
        {
            Expression current =
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(innerValue));

            for (var i = 0; i < depth; ++i)
            {
                current = Expression.ListInstance([current]);
            }

            return current;
        }

        var exprC = BuildDeeplyNestedListWithInnerValue(depth, 41);

        exprA.Equals(exprC).Should().BeFalse();
    }

    [Fact]
    public void Expression_aggregate_property_max_depth()
    {
        var testCases =
            new List<(Expression expression, int expectedMaxDepth)>
        {
            (Expression.EnvironmentInstance, 0),

            (Expression.LiteralInstance(PineValue.EmptyList), 0),

            (Expression.ListInstance([]), 0),

            (Expression.ListInstance([Expression.EnvironmentInstance]), 1),

            (Expression.ListInstance(
                [
                Expression.EnvironmentInstance,
                Expression.LiteralInstance(PineValue.EmptyList)
                ]),
                1),

            (Expression.ConditionalInstance(
                Expression.ListInstance([]),
                Expression.ListInstance(
                    [
                    Expression.EnvironmentInstance,
                    Expression.LiteralInstance(PineValue.EmptyList),
                    ]),
                Expression.ListInstance([])), 2),

            (new Expression.ParseAndEval(
                Expression.EnvironmentInstance,
                Expression.EnvironmentInstance), 1),

            (Expression.KernelApplicationInstance(
                "function",
                Expression.EnvironmentInstance), 1),
        };

        for (var i = 0; i < testCases.Count; i++)
        {
            var (expression, expectedMaxDepth) = testCases[i];

            expression.MaxDepth.Should().Be(expectedMaxDepth, $"Test case {i} failed.");
        }
    }

    [Fact]
    public void Expression_nested_list_equality()
    {
        var exprA =
            Expression.ListInstance(
                [
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(31)),
                    ]),
                Expression.ListInstance(
                    [
                    Expression.ListInstance(
                        [
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(41)),
                        ]),
                    ]),
                ]);

        var exprB =
            Expression.ListInstance(
                [
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(31)),
                    ]),
                Expression.ListInstance(
                    [
                    Expression.ListInstance(
                        [
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(41)),
                        ]),
                    ]),
                ]);

        exprA.Should().Be(exprB);
        exprA.GetHashCode().Should().Be(exprB.GetHashCode());
    }
}
