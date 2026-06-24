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
                nameof(KernelFunction.int_add),
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.int_mul),
                    Expression.LiteralInstance(PineValue.EmptyList))),

            Expression.ListInstance(
                [
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.int_add),
                    Expression.LiteralInstance(PineValue.EmptyList)),

                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.int_add),
                    Expression.LiteralInstance(PineValue.EmptyList)),
                ]),

            Expression.ConditionalInstance(
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.int_add),
                    Expression.LiteralInstance(PineValue.EmptyList)),
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.int_add),
                    Expression.LiteralInstance(PineValue.EmptyList)),
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.int_add),
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

            var anyNodeIsParseAndEval =
                rootAndSubexpressions.OfType<Expression.ParseAndEval>().Any();

            var conditionCount =
                rootAndSubexpressions.OfType<Expression.Conditional>().Count();

            var builtinCount =
                rootAndSubexpressions.OfType<Expression.KernelApplication>().Count();

            testCase.SubexpressionCount.Should().Be(subexpressions.Count);
            testCase.ReferencesEnvironment.Should().Be(anyNodeIsEnvironment);
            testCase.ContainsParseAndEval.Should().Be(anyNodeIsParseAndEval);
            testCase.ConditionCount.Should().Be(conditionCount);
            testCase.BuiltinCount.Should().Be(builtinCount);
        }
    }
}
