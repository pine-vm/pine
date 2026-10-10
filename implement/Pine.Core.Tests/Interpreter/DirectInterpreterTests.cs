using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Interpreter;

public class DirectInterpreterTests
{
    [Fact]
    public void Direct_counters_include_recursive_kinds_but_only_one_external_entry()
    {
        var expression =
            new Expression.Label(
                "root",
                Expression.ListInst(
                    [
                    Expression.LitralInst(PineValue.EmptyList),
                    Expression.EnvironmentInstance,
                    new Expression.Conditional(
                        condition: Expression.LitralInst(PineKernelValues.TrueValue),
                        trueBranch: Expression.BuiltinInst(
                            nameof(BuiltinFunction.length),
                            Expression.ListInst(
                                [
                                Expression.LitralInst(PineValue.EmptyList),
                                Expression.EnvironmentInstance
                                ])),
                        falseBranch: new Expression.Eval(Expression.EnvironmentInstance, Expression.EnvironmentInstance))
                    ]));

        var interpreter = DirectInterpreter.WithoutEvalCaching(new PineVMParseCache());
        interpreter.EvaluateExpression(expression, PineValue.EmptyList).IsOkOrNull().Should().NotBeNull();

        PerformanceCountersFormatting.FormatCounts(interpreter.Counters).ShouldBeWithDiff(
            """
            InvocationCount: 0
            BuildListCount: 2
            BuildListItemCount: 5
            LoopIterationCount: 0
            InstructionCount: 0
            ExpressionTemplatePlanParseCount: 0
            DeferredTemplateValueAllocationCount: 0
            TemplateDirectInvocationCount: 0
            DeferredTemplateValueMaterializationCount: 0
            DirectInterpreterInvocationCount: 1
            DirectInterpreterExpressionCount: 10
            DirectInterpreterLiteralCount: 3
            DirectInterpreterListCount: 2
            DirectInterpreterEvalCount: 0
            DirectInterpreterBuiltinCount: 1
            DirectInterpreterConditionalCount: 1
            DirectInterpreterEnvironmentCount: 2
            """);
    }

    [Theory]
    [InlineData(1)]
    [InlineData(20)]
    public void Specialized_direct_entry_count_is_independent_of_tree_depth(int depth)
    {
        var expression = Expression.ListInst([Expression.EnvironmentInstance]);

        for (var i = 1; i < depth; ++i)
            expression = Expression.ListInst([expression]);

        var interpreter = DirectInterpreter.WithoutEvalCaching(new PineVMParseCache());
        interpreter.EvaluateListExpression(expression, PineValue.EmptyList);
        interpreter.Counters.DirectInterpreterInvocationCount.Should().Be(1);
        interpreter.Counters.DirectInterpreterExpressionCount.Should().Be(depth + 1);
        interpreter.Counters.BuildListItemCount.Should().Be(depth);
    }

    [Fact]
    public void Direct_eval_cache_hits_count_the_work_performed_not_the_cached_body()
    {
        var target = Expression.ListInst([Expression.EnvironmentInstance, Expression.LitralInst(PineValue.EmptyList)]);

        var expression =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(target)),
                Expression.LitralInst(PineValue.EmptyList));

        var interpreter = DirectInterpreter.WithLocalEvalCache(new PineVMParseCache());
        interpreter.EvaluateParseAndEvalExpression(expression, PineValue.EmptyList);
        var first = interpreter.Counters;
        interpreter.EvaluateExpressionDefault(expression, PineValue.EmptyList);
        var second = PerformanceCounters.Subtract(interpreter.Counters, first);
        first.DirectInterpreterExpressionCount.Should().Be(6);
        first.BuildListItemCount.Should().Be(2);
        second.DirectInterpreterInvocationCount.Should().Be(1);
        second.DirectInterpreterExpressionCount.Should().Be(3);
        second.DirectInterpreterEvalCount.Should().Be(1);
        second.DirectInterpreterLiteralCount.Should().Be(2);
        second.BuildListItemCount.Should().Be(0);
    }

    [Fact]
    public void Direct_counters_preserve_work_when_parsing_fails()
    {
        var interpreter = DirectInterpreter.WithoutEvalCaching(new PineVMParseCache());

        var invalid =
            new Expression.Eval(
                Expression.LitralInst(PineValue.EmptyList),
                Expression.LitralInst(PineValue.EmptyList));

        Action evaluate = () => interpreter.EvaluateExpressionDefault(invalid, PineValue.EmptyList);
        evaluate.Should().Throw<ParseExpressionException>();
        interpreter.Counters.DirectInterpreterInvocationCount.Should().Be(1);
        interpreter.Counters.DirectInterpreterExpressionCount.Should().Be(3);
        interpreter.Counters.DirectInterpreterEvalCount.Should().Be(1);
    }

    [Fact]
    public void Evaluate_populates_eval_cache()
    {
        var targetExpression = Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(42));

        var expression =
            new Expression.Eval(
                Expression.LitralInst(ExpressionEncoding.EncodeExpressionAsValue(targetExpression)),
                Expression.EmptyList);

        var evalCache = new Dictionary<DirectInterpreter.EvalCacheEntryKey, PineValue>();

        var interpreter =
            DirectInterpreter.WithSharedEvalCache(new PineVMParseCache(), evalCache);

        interpreter.EvaluateExpressionDefault(expression, PineValue.EmptyList)
            .Should().Be(IntegerEncoding.EncodeSignedInteger(42));

        evalCache.Should().ContainSingle();

        var cacheKey = evalCache.Keys.Should().ContainSingle().Which;
        var replacementResult = IntegerEncoding.EncodeSignedInteger(99);
        evalCache[cacheKey] = replacementResult;

        interpreter.EvaluateExpressionDefault(expression, PineValue.EmptyList)
            .Should().Be(replacementResult);
    }
}
