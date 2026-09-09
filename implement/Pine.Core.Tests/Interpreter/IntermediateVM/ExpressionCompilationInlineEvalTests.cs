using AwesomeAssertions;
using Pine.Core.Interpreter.IntermediateVM;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class ExpressionCompilationInlineEvalTests
{
    [Fact]
    public void Traverses_all_composite_expression_types_and_tracks_conditional_branches()
    {
        var literal = Expression.LitralInst(PineValue.Blob([1]));
        var replacement = Expression.LitralInst(PineValue.Blob([2]));
        var targetEval = new Expression.Eval(literal, Expression.EnvironmentInstance);
        var containingEval = new Expression.Eval(targetEval, targetEval);

        var expression =
            new Expression.Label(
                "label",
                Expression.ListInst(
                    [
                    targetEval,
                    targetEval,
                    Expression.BuiltinInst("head", targetEval),
                    containingEval,
                    Expression.ConditionalInst(
                        targetEval,
                        targetEval,
                        targetEval)
                    ]));

        var visits = new List<(Expression.Eval eval, bool underConditional)>();

        var inlined =
            ExpressionCompilation.InlineEvalRecursive(
                expression,
                underConditional: false,
                (eval, underConditional) =>
                {
                    visits.Add((eval, underConditional));

                    return eval == targetEval ? replacement : null;
                });

        var inlinedList =
            inlined
            .Should()
            .BeOfType<Expression.Label>().Subject.Tagged
            .Should()
            .BeOfType<Expression.List>().Subject;

        inlinedList.Items[0].Should().BeSameAs(replacement);
        inlinedList.Items[1].Should().BeSameAs(replacement);

        inlinedList.Items[2]
            .Should()
            .BeOfType<Expression.Builtin>().Subject.Input
            .Should()
            .BeSameAs(replacement);

        var inlinedContainingEval =
            inlinedList.Items[3]
            .Should()
            .BeOfType<Expression.Eval>().Subject;

        inlinedContainingEval.Encoded.Should().BeSameAs(replacement);
        inlinedContainingEval.Environment.Should().BeSameAs(replacement);

        var inlinedConditional =
            inlinedList.Items[4]
            .Should()
            .BeOfType<Expression.Conditional>().Subject;

        inlinedConditional.Condition.Should().BeSameAs(replacement);
        inlinedConditional.FalseBranch.Should().BeSameAs(replacement);
        inlinedConditional.TrueBranch.Should().BeSameAs(replacement);

        visits.Count(visit => visit.eval == containingEval).Should().Be(1);
        visits.Count(visit => visit.eval == targetEval && !visit.underConditional).Should().Be(1);
        visits.Count(visit => visit.eval == targetEval && visit.underConditional).Should().Be(1);
    }

    [Fact]
    public void Does_not_traverse_an_inlined_replacement()
    {
        var literal = Expression.LitralInst(PineValue.Blob([1]));
        var expression = new Expression.Eval(literal, Expression.EnvironmentInstance);
        var replacement = new Expression.Eval(literal, literal);
        var visits = 0;

        var inlined =
            ExpressionCompilation.InlineEvalRecursive(
                expression,
                underConditional: false,
                (eval, _) =>
                {
                    ++visits;
                    return replacement;
                });

        inlined.Should().BeSameAs(replacement);
        visits.Should().Be(1);
    }

    [Fact]
    public void Reuses_an_unchanged_expression_tree()
    {
        var literal = Expression.LitralInst(PineValue.Blob([1]));
        var eval = new Expression.Eval(literal, Expression.EnvironmentInstance);

        var expression =
            new Expression.Label(
                "label",
                Expression.ConditionalInst(
                    Expression.BuiltinInst("head", Expression.ListInst([eval, eval])),
                    eval,
                    eval));

        var inlined =
            ExpressionCompilation.InlineEvalRecursive(
                expression,
                underConditional: false,
                (eval, _) => null);

        inlined.Should().BeSameAs(expression);
    }

    [Fact]
    public void Reuses_unchanged_list_and_visits_shared_items_once_per_conditional_context()
    {
        var literal = Expression.LitralInst(PineValue.Blob([1]));
        var eval = new Expression.Eval(literal, Expression.EnvironmentInstance);
        var list = Expression.ListInst([eval, eval]);

        var expression =
            Expression.ConditionalInst(
                list,
                list,
                list);

        var visits = new List<bool>();

        var inlined =
            ExpressionCompilation.InlineEvalRecursive(
                expression,
                underConditional: false,
                (_, underConditional) =>
                {
                    visits.Add(underConditional);
                    return null;
                });

        inlined.Should().BeSameAs(expression);
        visits.Should().Equal(false, true);
    }
}
