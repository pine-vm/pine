using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.CodeAnalysis;

public class SubstituteEnvironmentNodeTests
{
    [Fact]
    public void Environment_root_is_replaced_with_the_provided_instance()
    {
        var replacement = Expression.LitralInst(PineValue.Blob([1]));

        var substituted =
            ReducePineExpression.SubstituteEnvironmentNode(
                Expression.EnvironmentInstance,
                replacement);

        substituted.Should().BeSameAs(replacement);
    }

    [Fact]
    public void Substitutes_environment_nodes_in_every_composite_expression_type()
    {
        var replacement = Expression.LitralInst(PineValue.Blob([1]));
        var unchanged = Expression.LitralInst(PineValue.Blob([2]));
        var labelValue = PineValue.List([PineValue.Blob([3])]);

        var expression =
            new Expression.Label(
                labelValue,
                Expression.ConditionalInst(
                    condition:
                    new Expression.Eval(
                        encoded: Expression.EnvironmentInstance,
                        environment:
                        Expression.BuiltinInst(
                            "head",
                            Expression.ListInst(
                                [
                                Expression.EnvironmentInstance,
                                unchanged
                                ]))),
                    falseBranch: Expression.EnvironmentInstance,
                    trueBranch: unchanged));

        var substituted =
            ReducePineExpression.SubstituteEnvironmentNode(expression, replacement);

        substituted.Should().NotBeSameAs(expression);

        substituted.Should().Be(
            new Expression.Label(
                labelValue,
                Expression.ConditionalInst(
                    condition:
                    new Expression.Eval(
                        encoded: replacement,
                        environment:
                        Expression.BuiltinInst(
                            "head",
                            Expression.ListInst(
                                [
                                replacement,
                                unchanged
                                ]))),
                    falseBranch: replacement,
                    trueBranch: unchanged)));

        var substitutedLabel = substituted.Should().BeOfType<Expression.Label>().Subject;
        substitutedLabel.LabelValue.Should().BeSameAs(labelValue);

        var descendants = Expression.EnumerateSelfAndDescendants(substituted).ToArray();

        descendants.Should().NotContain(descendant => descendant is Expression.Environment);
        descendants.Count(descendant => ReferenceEquals(descendant, replacement)).Should().Be(3);
        descendants.Should().Contain(descendant => ReferenceEquals(descendant, unchanged));
    }

    [Fact]
    public void Reuses_every_environment_independent_node_instance()
    {
        var literal = Expression.LitralInst(PineValue.Blob([4]));
        var list = Expression.ListInst([literal]);
        var builtin = Expression.BuiltinInst("head", list);
        var eval = new Expression.Eval(literal, list);
        var conditional = Expression.ConditionalInst(builtin, eval, list);
        var label = new Expression.Label("label", conditional);
        var replacement = Expression.LitralInst(PineValue.Blob([5]));

        Expression[] expressions = [literal, list, builtin, eval, conditional, label];

        foreach (var expression in expressions)
        {
            ReducePineExpression.SubstituteEnvironmentNode(expression, replacement)
                .Should().BeSameAs(expression);
        }
    }

    [Fact]
    public void Reuses_unchanged_siblings_when_rebuilding_ancestors()
    {
        var unchanged =
            new Expression.Label(
                "unchanged",
                Expression.LitralInst(PineValue.Blob([6])));

        var expression =
            Expression.ListInst(
                [
                unchanged,
                Expression.EnvironmentInstance
                ]);

        var replacement = Expression.LitralInst(PineValue.Blob([7]));

        var substituted =
            ReducePineExpression.SubstituteEnvironmentNode(expression, replacement)
            .Should().BeOfType<Expression.List>().Subject;

        substituted.Should().NotBeSameAs(expression);
        substituted.Items[0].Should().BeSameAs(unchanged);
        substituted.Items[1].Should().BeSameAs(replacement);
    }

    [Fact]
    public void Reuses_substituted_instances_for_shared_subtrees()
    {
        var shared =
            Expression.BuiltinInst(
                "head",
                Expression.EnvironmentInstance);

        var expression = Expression.ListInst([shared, shared]);
        var replacement = Expression.LitralInst(PineValue.Blob([8]));

        var substituted =
            ReducePineExpression.SubstituteEnvironmentNode(expression, replacement)
            .Should().BeOfType<Expression.List>().Subject;

        substituted.Items[0].Should().BeSameAs(substituted.Items[1]);
    }

    [Fact]
    public void Creates_no_heap_instances_when_the_expression_value_remains_the_same()
    {
        var equivalentEnvironment = new Expression.Environment();

        Expression expression =
            new Expression.Label(
                "label",
                Expression.ConditionalInst(
                    condition:
                    new Expression.Eval(
                        Expression.EnvironmentInstance,
                        Expression.ListInst([Expression.EnvironmentInstance])),
                    falseBranch:
                    Expression.BuiltinInst(
                        "head",
                        Expression.EnvironmentInstance),
                    trueBranch: Expression.EnvironmentInstance));

        for (var i = 0; i < 10; ++i)
            _ = ReducePineExpression.SubstituteEnvironmentNode(expression, equivalentEnvironment);

        var allocatedBefore = GC.GetAllocatedBytesForCurrentThread();

        Expression? substituted = null;

        for (var i = 0; i < 100; ++i)
        {
            substituted =
                ReducePineExpression.SubstituteEnvironmentNode(
                    expression,
                    equivalentEnvironment);
        }

        var allocatedBytes =
            GC.GetAllocatedBytesForCurrentThread() - allocatedBefore;

        substituted.Should().BeSameAs(expression);
        allocatedBytes.Should().Be(0);
    }
}
