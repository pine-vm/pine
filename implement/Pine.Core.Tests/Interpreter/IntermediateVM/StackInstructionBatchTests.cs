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
    [Theory]
    [InlineData(-1, 11, 3, 8)]
    [InlineData(0, 11, 3, 11)]
    [InlineData(4, 11, 3, 23)]
    public void Int_mul_const_add_binary_combines_top_two_values(
        int multiplier,
        int addend,
        int multiplied,
        int expected)
    {
        ExecuteIntMulConstAddBinary(
            multiplier,
            IntegerEncoding.EncodeSignedInteger(addend),
            IntegerEncoding.EncodeSignedInteger(multiplied))
            .Should()
            .Be(IntegerEncoding.EncodeSignedInteger(expected));
    }

    [Fact]
    public void Int_mul_const_add_binary_returns_empty_list_for_non_integer_operand()
    {
        ExecuteIntMulConstAddBinary(
            4,
            PineValue.EmptyList,
            IntegerEncoding.EncodeSignedInteger(3))
            .Should()
            .Be(PineValue.EmptyList);

        ExecuteIntMulConstAddBinary(
            4,
            IntegerEncoding.EncodeSignedInteger(11),
            PineValue.EmptyList)
            .Should()
            .Be(PineValue.EmptyList);
    }

    [Fact]
    public void Local_set_multiple_and_counted_pop_preserve_ordered_indices()
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
                StackInstruction.Local_Set([2, 0, 4], popCount: 3),
                StackInstruction.Local_Get(0),
                StackInstruction.Local_Get(2),
                StackInstruction.Local_Get(4),
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
                    [rootExpression] =
                    new ExpressionCompilation(
                        Generic:
                        new StackFrameInstructions(
                            Parameters: StaticFunctionInterface.Generic,
                            Instructions:
                            [
                            StackInstruction.Local_Get(0),
                            StackInstruction.Eval_Const(
                                ExpressionEncoding.EncodeExpressionAsValue(targetExpression)),
                            StackInstruction.Return,
                            ],
                            TrackEnvConstraint: null),
                        Specialized: []),
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

        report.ReturnValue.Evaluate().Should().Be(PineValue.List([second, third, first]));
        report.InstructionCount.Should().Be(11);
    }

    [Fact]
    public void Empty_local_indices_cannot_be_executed_in_a_stack_frame()
    {
        foreach (var kind in new[]
        {
            StackInstructionKind.Local_Set,
            StackInstructionKind.Local_Get,
            StackInstructionKind.Local_Get_Skip_Head_Const,
            StackInstructionKind.Local_Int_Add_Const
        })
        {
            Action createFrame =
                () => _ =
                    new StackFrameInstructions(
                        StaticFunctionInterface.Generic,
                        [new StackInstruction(kind, LocalIndices: []), StackInstruction.Return]);

            createFrame.Should().Throw<InvalidOperationException>();
        }
    }

    private static PineValue ExecuteIntMulConstAddBinary(
        int multiplier,
        PineValue addend,
        PineValue multiplied)
    {
        var targetExpression = Expression.EnvironmentInstance;

        var instructions =
            new StackFrameInstructions(
                Parameters: StaticFunctionInterface.FromPathsSorted([]),
                Instructions:
                [
                StackInstruction.Push_Literal(addend),
                StackInstruction.Push_Literal(multiplied),
                StackInstruction.Int_Mul_Const_Add_Binary(multiplier),
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
                    [rootExpression] =
                    new ExpressionCompilation(
                        Generic:
                        new StackFrameInstructions(
                            Parameters: StaticFunctionInterface.Generic,
                            Instructions:
                            [
                            StackInstruction.Local_Get(0),
                            StackInstruction.Eval_Const(
                                ExpressionEncoding.EncodeExpressionAsValue(targetExpression)),
                            StackInstruction.Return,
                            ],
                            TrackEnvConstraint: null),
                        Specialized: []),
                    [targetExpression] =
                    new ExpressionCompilation(
                        Generic: instructions,
                        Specialized: [])
                });

        return
            vm.EvaluateExpressionOnCustomStack(
                rootExpression,
                PineValue.EmptyList,
                new Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig(
                    InvocationCountLimit: null,
                    LoopIterationCountLimit: null,
                    StackDepthLimit: null))
            .Extract(error => throw new InvalidOperationException(error.ToString()))
            .ReturnValue
            .Evaluate();
    }
}
