using AwesomeAssertions;
using Pine.Core.Interpreter.IntermediateVM;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class EvalPathCostTests
{
    [Fact(Timeout = 30_000)]
    public void Equality_of_distinct_shared_expression_graphs_is_not_exponential()
    {
        static Expression Build()
        {
            var graph = Expression.EnvironmentInstance;

            for (var depth = 0; depth < 35; ++depth)
                graph = new Expression.Eval(Expression.EmptyList, Expression.ListInst([graph, graph]));

            return Expression.ListInst([graph]);
        }

        var left = Build();
        var right = Build();
        ReferenceEquals(left, right).Should().BeFalse();
        left.Equals(right).Should().BeTrue();
        left.GetHashCode().Should().Be(right.GetHashCode());
    }

    [Fact]
    public void Shared_subexpressions_are_measured_once_but_counted_for_every_executed_edge()
    {
        Expression expression = new Expression.Eval(Expression.EmptyList, Expression.EmptyList);

        for (var depth = 0; depth < 35; ++depth)
            expression = Expression.ListInst([expression, expression]);

        var calls = 0;
        var cost = ExpressionCompilation.ComputeEvalPathMax(expression, _ => { ++calls; return true; });
        cost.Should().Be(int.MaxValue);
        calls.Should().Be(1);
    }

    [Fact]
    public void Conditional_cost_includes_the_condition_and_only_the_worst_branch()
    {
        var eval = new Expression.Eval(Expression.EmptyList, Expression.EmptyList);

        var expression =
            Expression.ConditionalInst(
                condition: eval,
                trueBranch: Expression.ListInst([eval, eval]),
                falseBranch: eval);

        ExpressionCompilation.ComputeEvalPathMax(expression, _ => true).Should().Be(3);
    }
}
