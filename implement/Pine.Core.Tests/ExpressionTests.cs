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

            Expression.LitralInst(PineValue.EmptyList),

            Expression.ListInst([]),

            Expression.ListInst([Expression.EnvironmentInstance]),

            Expression.ListInst(
                [
                Expression.EnvironmentInstance,
                Expression.LitralInst(PineValue.EmptyList)
                ]),

            Expression.ListInst(
                [
                Expression.EnvironmentInstance,
                Expression.ListInst(
                    [
                    Expression.EnvironmentInstance,
                    Expression.LitralInst(PineValue.EmptyList),
                    ]),
                Expression.ListInst([]),
                ]),

            Expression.ConditionalInst(
                Expression.ListInst([]),
                Expression.ListInst(
                    [
                    Expression.EnvironmentInstance,
                    Expression.LitralInst(PineValue.EmptyList),
                    ]),
                Expression.ListInst([])),

            Expression.ConditionalInst(
                Expression.ListInst([]),
                Expression.ListInst(
                    [
                    Expression.LitralInst(PineValue.EmptyBlob),
                    Expression.LitralInst(PineValue.EmptyList),
                    ]),
                Expression.ListInst([])),

            new Expression.Eval(
                Expression.EnvironmentInstance,
                Expression.EnvironmentInstance),

            Expression.BuiltinInst(
                "function",
                Expression.EnvironmentInstance),

            new Expression.Label(
                "test",
                Expression.ListInst([])),

            new Expression.Label(
                "test",
                Expression.EnvironmentInstance),

            new Expression.Label(
                "test",
                Expression.ListInst([Expression.EnvironmentInstance])),

            // Eval nested inside a List subexpression.
            Expression.ListInst(
                [
                Expression.LitralInst(PineValue.EmptyList),
                new Expression.Eval(
                    Expression.EnvironmentInstance,
                    Expression.EnvironmentInstance),
                ]),

            // Eval nested in the input of a KernelApplication.
            Expression.BuiltinInst(
                "function",
                new Expression.Eval(
                    Expression.LitralInst(PineValue.EmptyList),
                    Expression.EnvironmentInstance)),

            // Eval nested in a branch of a Conditional.
            Expression.ConditionalInst(
                Expression.ListInst([]),
                Expression.ListInst([]),
                new Expression.Eval(
                    Expression.EnvironmentInstance,
                    Expression.LitralInst(PineValue.EmptyList))),

            // Eval nested inside a StringTag.
            new Expression.Label(
                "test",
                new Expression.Eval(
                    Expression.EnvironmentInstance,
                    Expression.EnvironmentInstance)),

            // Eval with another Eval nested in its parts.
            new Expression.Eval(
                new Expression.Eval(
                    Expression.EnvironmentInstance,
                    Expression.LitralInst(PineValue.EmptyList)),
                Expression.EnvironmentInstance),

            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_add),
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.int_mul),
                    Expression.LitralInst(PineValue.EmptyList))),

            Expression.ListInst(
                [
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.int_add),
                    Expression.LitralInst(PineValue.EmptyList)),

                Expression.BuiltinInst(
                    nameof(BuiltinFunction.int_add),
                    Expression.LitralInst(PineValue.EmptyList)),
                ]),

            Expression.ConditionalInst(
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.int_add),
                    Expression.LitralInst(PineValue.EmptyList)),
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.int_add),
                    Expression.LitralInst(PineValue.EmptyList)),
                Expression.BuiltinInst(
                    nameof(BuiltinFunction.int_add),
                    Expression.LitralInst(PineValue.EmptyList))),

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
                rootAndSubexpressions.OfType<Expression.Eval>().Count();

            var conditionCount =
                rootAndSubexpressions.OfType<Expression.Conditional>().Count();

            var builtinCount =
                rootAndSubexpressions.OfType<Expression.Builtin>().Count();

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
                Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(31));

            for (var i = 0; i < depth; ++i)
            {
                current = Expression.ListInst([current]);
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
                Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(innerValue));

            for (var i = 0; i < depth; ++i)
            {
                current = Expression.ListInst([current]);
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

            (Expression.LitralInst(PineValue.EmptyList), 0),

            (Expression.ListInst([]), 0),

            (Expression.ListInst([Expression.EnvironmentInstance]), 1),

            (Expression.ListInst(
                [
                Expression.EnvironmentInstance,
                Expression.LitralInst(PineValue.EmptyList)
                ]),
                1),

            (Expression.ConditionalInst(
                Expression.ListInst([]),
                Expression.ListInst(
                    [
                    Expression.EnvironmentInstance,
                    Expression.LitralInst(PineValue.EmptyList),
                    ]),
                Expression.ListInst([])), 2),

            (new Expression.Eval(
                Expression.EnvironmentInstance,
                Expression.EnvironmentInstance), 1),

            (Expression.BuiltinInst(
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
            Expression.ListInst(
                [
                Expression.ListInst(
                    [
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(31)),
                    ]),
                Expression.ListInst(
                    [
                    Expression.ListInst(
                        [
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(41)),
                        ]),
                    ]),
                ]);

        var exprB =
            Expression.ListInst(
                [
                Expression.ListInst(
                    [
                    Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(31)),
                    ]),
                Expression.ListInst(
                    [
                    Expression.ListInst(
                        [
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(41)),
                        ]),
                    ]),
                ]);

        exprA.Should().Be(exprB);
        exprA.GetHashCode().Should().Be(exprB.GetHashCode());
    }
}
