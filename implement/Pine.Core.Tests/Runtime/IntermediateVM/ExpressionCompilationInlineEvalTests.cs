using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen.Pine;
using Pine.Core.Encodings;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class ExpressionCompilationInlineEvalTests
{
    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Mutually_exclusive_projections_can_still_be_inlined(bool environmentAware)
    {
        var parseCache = new PineVMParseCache();

        var producer =
            new Expression.Eval(
                ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance),
                ExpressionBuilder.BuildExpressionForPathInExpression([1], Expression.EnvironmentInstance));

        var body =
            Expression.ConditionalInst(
                condition: ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance),
                trueBranch: ExpressionBuilder.BuildExpressionForPathInExpression([1, 2], Expression.EnvironmentInstance),
                falseBranch:
                ExpressionBuilder.BuildExpressionForPathInExpression([1, 4], Expression.EnvironmentInstance));

        var call =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(body)),
                Expression.ListInst(
                    [
                    ExpressionBuilder.BuildExpressionForPathInExpression([2], Expression.EnvironmentInstance),
                    producer
                    ]));

        var inlined =
            environmentAware
            ?
            ExpressionCompilation.ReduceExpressionAndInlineRecursive(
                rootExpression: call,
                rootExprAlternativeForms: [],
                envConstraintId: null,
                maxDepth: 4,
                maxSubexpressionCount: 4_000,
                parseCache,
                disableRecurseAfterInline: false,
                skipInlining: (_, _) => false)
            :
            ExpressionCompilation.InlineStaticInvocationsAndReduceRecursive(
                call,
                [],
                maxDepth: 4,
                maxSubexpressionCount: 4_000,
                parseCache,
                disableRecurseAfterInline: false,
                skipInlining: _ => false);

        inlined.Should().BeOfType<Expression.Conditional>();
        ExpressionCompilation.ComputeEvalPathMax(inlined, _ => true).Should().Be(1);
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Deep_projections_do_not_duplicate_computed_arguments(bool environmentAware, bool repeated)
    {
        var parseCache = new PineVMParseCache();

        var producer =
            new Expression.Eval(
                ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance),
                ExpressionBuilder.BuildExpressionForPathInExpression([1], Expression.EnvironmentInstance));

        var first = ExpressionBuilder.BuildExpressionForPathInExpression([1, 2], Expression.EnvironmentInstance);
        var second = ExpressionBuilder.BuildExpressionForPathInExpression([1, 4], Expression.EnvironmentInstance);
        var body = Expression.ListInst(repeated ? [first, second] : [first]);

        var call =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(body)),
                Expression.ListInst([Expression.LitralInst(PineValue.EmptyList), producer]));

        var inlined =
            environmentAware
            ?
            ExpressionCompilation.ReduceExpressionAndInlineRecursive(
                rootExpression: call,
                rootExprAlternativeForms: [],
                envConstraintId: null,
                maxDepth: 4,
                maxSubexpressionCount: 4_000,
                parseCache,
                disableRecurseAfterInline: false,
                skipInlining: (_, _) => false)
            :
            ExpressionCompilation.InlineStaticInvocationsAndReduceRecursive(
                call,
                [],
                maxDepth: 4,
                maxSubexpressionCount: 4_000,
                parseCache,
                disableRecurseAfterInline: false,
                skipInlining: _ => false);

        Core.CodeAnalysis.CodeAnalysis.CountOccurrencesInSelfAndDescendants(inlined, producer).Should().Be(1);

        if (repeated)
            inlined.Should().BeOfType<Expression.Eval>();

        else
            inlined.Should().BeOfType<Expression.List>();

        var producerBody =
            Expression.ListInst(
                [
                Expression.LitralInst(PineValue.EmptyList),
                Expression.LitralInst(StringEncoding.ValueFromString("a")),
                Expression.EnvironmentInstance,
                Expression.LitralInst(StringEncoding.ValueFromString("b")),
                Expression.EnvironmentInstance
                ]);

        var environment =
            PineValue.List(
                [ExpressionEncoding.EncodeExpressionAsValue(producerBody), IntegerEncoding.EncodeSignedInteger(7)]);

        var actual =
            DirectInterpreter.WithoutEvalCaching(parseCache)
            .EvaluateExpression(inlined, environment).Extract(error => throw new Exception(error));

        actual.Should().Be(
            PineValue.List(
                repeated
                ?
                [IntegerEncoding.EncodeSignedInteger(7), IntegerEncoding.EncodeSignedInteger(7)]
                :
                [IntegerEncoding.EncodeSignedInteger(7)]));
    }

    [Fact]
    public void Traverses_all_composite_expression_types_and_tracks_condition_count()
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

        var visits = new List<(Expression.Eval eval, int conditionCount)>();

        var inlined =
            ExpressionCompilation.InlineEvalRecursive(
                expression,
                conditionCount: 0,
                (eval, conditionCount) =>
                {
                    visits.Add((eval, conditionCount));

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
        visits.Count(visit => visit.eval == targetEval && visit.conditionCount is 0).Should().Be(1);
        visits.Count(visit => visit.eval == targetEval && visit.conditionCount is 1).Should().Be(1);
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
                conditionCount: 0,
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
                conditionCount: 0,
                (eval, _) => null);

        inlined.Should().BeSameAs(expression);
    }

    [Fact]
    public void Reuses_unchanged_list_and_visits_shared_items_once_per_condition_count()
    {
        var literal = Expression.LitralInst(PineValue.Blob([1]));
        var eval = new Expression.Eval(literal, Expression.EnvironmentInstance);
        var list = Expression.ListInst([eval, eval]);

        var expression =
            Expression.ConditionalInst(
                list,
                list,
                list);

        var visits = new List<int>();

        var inlined =
            ExpressionCompilation.InlineEvalRecursive(
                expression,
                conditionCount: 0,
                (_, conditionCount) =>
                {
                    visits.Add(conditionCount);
                    return null;
                });

        inlined.Should().BeSameAs(expression);
        visits.Should().Equal(0, 1);
    }

    [Fact]
    public void Increments_condition_count_for_each_nested_conditional_branch()
    {
        var literal = Expression.LitralInst(PineValue.Blob([1]));
        var eval = new Expression.Eval(literal, Expression.EnvironmentInstance);

        var expression =
            Expression.ConditionalInst(
                eval,
                Expression.ConditionalInst(
                    eval,
                    eval,
                    eval),
                eval);

        var visits = new List<int>();

        _ =
            ExpressionCompilation.InlineEvalRecursive(
                expression,
                conditionCount: 3,
                (_, conditionCount) =>
                {
                    visits.Add(conditionCount);
                    return null;
                });

        visits.Should().Equal(3, 4, 5);
    }
}
