using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class StackInstructionBatchTests
{
    [Fact]
    public void Local_set_descending_and_counted_pop_preserve_value_order()
    {
        var targetExpression = Expression.EnvironmentInstance;

        var first = IntegerEncoding.EncodeSignedInteger(11);
        var second = IntegerEncoding.EncodeSignedInteger(22);
        var third = IntegerEncoding.EncodeSignedInteger(33);

        var instructions =
            new StackFrameInstructions(
                Parameters: StaticFunctionInterface.FromPathsSorted([]),
                Instructions:
                [
                StackInstruction.Push_Literal(first),
                StackInstruction.Push_Literal(second),
                StackInstruction.Push_Literal(third),
                StackInstruction.Local_Set_Descending(index: 2, takeCount: 3),
                StackInstruction.PopMultiple(3),
                StackInstruction.Local_Get(0),
                StackInstruction.Local_Get(1),
                StackInstruction.Local_Get(2),
                StackInstruction.Build_List(3),
                StackInstruction.Return,
                ]);

        var rootExpression =
            new Expression.Eval(
                encoded:
                Expression.LitralInst(
                    ExpressionEncoding.EncodeExpressionAsValue(targetExpression)),
                environment: Expression.EnvironmentInstance);

        var vm =
            Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: true,
                selectPrecompiled: null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: false,
                parseCache: null,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                expressionCompilationOverrides:
                new Dictionary<Expression, ExpressionCompilation>
                {
                    [targetExpression] =
                    new ExpressionCompilation(
                        Generic: instructions,
                        Specialized: [])
                });

        var report =
            vm.EvaluateExpressionOnCustomStack(
                rootExpression,
                PineValue.EmptyList,
                new Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig(
                    InvocationCountLimit: null,
                    LoopIterationCountLimit: null,
                    StackDepthLimit: null))
            .Extract(error => throw new InvalidOperationException(error.ToString()));

        report.ReturnValue.Evaluate().Should().Be(PineValue.List([first, second, third]));
        report.InstructionCount.Should().Be(12);
    }
}
