using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CodeGen.Pine;
using Pine.Core.Encodings;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Numerics;
using System.Threading;
using Xunit;

namespace Pine.Core.Tests.Runtime.IntermediateVM;

public class IntegerDivisionTests
{
    [Fact]
    public void Integer_division_backward_zero_jump_observes_loop_quota()
    {
        var frame =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Push_Literal(Integer(0)),
                StackInstruction.Int_Div_Binary(jumpOffset: -2),
                StackInstruction.Return
                ]);

        var result =
            CreateVM(frame).EvaluateExpressionOnCustomStack(
                Expression.EnvironmentInstance,
                Integer(22),
                Core.Runtime.IntermediateVM.PineVM.EvaluationConfig.Default with { LoopIterationCountLimit = 2 });

        result.IsErrOrNull()!.Reason.Should().Be(
            new EvaluationErrorReason.QuotaExhausted(EvaluationQuotaKind.LoopIterationCount, 2));
    }

    [Fact]
    public void Integer_division_zero_jump_observes_cancellation_at_the_instruction()
    {
        using var cancellation = new CancellationTokenSource();

        var frame =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [
                StackInstruction.Local_Get(0),
                StackInstruction.Push_Literal(Integer(0)),
                StackInstruction.Int_Div_Binary(jumpOffset: 2),
                StackInstruction.Return,
                StackInstruction.Push_Literal(Integer(100)),
                StackInstruction.Return
                ]);

        void Observe(in ExecutedStackInstruction executed)
        {
            if (executed.Instruction.Kind == StackInstructionKind.Int_Div_Binary)
            {
                cancellation.Cancel();
            }
        }

        var result =
            CreateVM(frame, Observe).EvaluateExpressionOnCustomStack(
                Expression.EnvironmentInstance,
                Integer(22),
                Core.Runtime.IntermediateVM.PineVM.EvaluationConfig.Default,
                cancellation.Token);

        result.IsErrOrNull()!.Reason.Should().BeOfType<EvaluationErrorReason.CancellationRequested>();
    }

    [Fact]
    public void Integer_division_zero_jump_preserves_surrounding_stack_and_accounts_for_handler_depth()
    {
        var frame =
            new StackFrameInstructions(
                StaticFunctionInterface.Generic,
                [
                StackInstruction.Push_Literal(Integer(99)),
                StackInstruction.Push_Literal(Integer(22)),
                StackInstruction.Local_Get(0),
                StackInstruction.Int_Div_Binary(jumpOffset: 3),
                StackInstruction.Build_List(2),
                StackInstruction.Return,
                StackInstruction.Push_Literal(Integer(100)),
                StackInstruction.Push_Literal(Integer(200)),
                StackInstruction.Push_Literal(Integer(300)),
                StackInstruction.Build_List(3),
                StackInstruction.Build_List(2),
                StackInstruction.Return
                ]);

        frame.MaxStackUsage.Should().Be(4);
        Execute(frame, Integer(7)).Should().Be(PineValue.List([Integer(99), Integer(3)]));

        Execute(frame, Integer(0)).Should().Be(
            PineValue.List([Integer(99), PineValue.List([Integer(100), Integer(200), Integer(300)])]));

        Execute(frame, PineValue.EmptyList).Should().Be(PineValue.List([Integer(99), Integer(0)]));
        AssertPersistence(frame);
    }

    [Fact]
    public void Integer_division_rejects_invalid_instruction_forms_and_inconsistent_stack_edges()
    {
        foreach (var instruction in new[]
        {
            new StackInstruction(StackInstructionKind.Int_Div_Binary),
            StackInstruction.Int_Div_Binary(PineValue.EmptyList) with { JumpOffset = 2 },
            new StackInstruction(StackInstructionKind.Int_Div_Const),
            new StackInstruction(StackInstructionKind.Int_Div_Const, IntegerLiteral: 0)
        })
        {
            Action createFrame =
                () => _ =
                    new StackFrameInstructions(
                        StaticFunctionInterface.Generic,
                        [StackInstruction.Local_Get([0, 0]), instruction, StackInstruction.Return]);

            createFrame.Should().Throw<InvalidOperationException>();
        }

        Action createZeroConstant = () => StackInstruction.Int_Div_Const(0);
        createZeroConstant.Should().Throw<ArgumentOutOfRangeException>();

        Action createSameSuccessor =
            () => _ =
                new StackFrameInstructions(
                    StaticFunctionInterface.Generic,
                    [
                    StackInstruction.Local_Get([0, 0]),
                    StackInstruction.Int_Div_Binary(jumpOffset: 1),
                    StackInstruction.Return
                    ]);

        createSameSuccessor.Should().Throw<InvalidOperationException>();

        Action appendJumpOperation =
            () => PineControlFlowFragment.FromOperations([StackInstruction.Int_Div_Binary(jumpOffset: 2)]);

        appendJumpOperation.Should().Throw<ArgumentException>();
    }

    private static PineValue Integer(BigInteger value) => IntegerEncoding.EncodeSignedInteger(value);

    private static Expression Argument(int index) =>
        ExpressionBuilder.BuildExpressionForPathInExpression([index], Expression.EnvironmentInstance);

    private static StackFrameInstructions Compile(Expression expression, bool reduce) =>
        ExpressionCompilation.CompileExpression(
            expression,
            specializations: [],
            new PineVMParseCache(),
            disableReduction: !reduce,
            enableTailRecursionOptimization: false,
            skipInlining: (_, _) => false)
        .Generic;

    private static PineValue Execute(StackFrameInstructions frame, PineValue environment)
    {
        return
            CreateVM(frame).EvaluateExpressionOnCustomStack(
                Expression.EnvironmentInstance,
                environment,
                Core.Runtime.IntermediateVM.PineVM.EvaluationConfig.Default)
            .Extract(error => throw new InvalidOperationException(error.ToString()))
            .ReturnValue.Evaluate();
    }

    private static Core.Runtime.IntermediateVM.PineVM CreateVM(
        StackFrameInstructions frame,
        ReportExecutedStackInstruction? observeInstruction = null) =>
        Core.Runtime.IntermediateVM.PineVM.CreateCustom(
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
            reportExecutedStackInstruction: observeInstruction,
            expressionCompilationOverrides:
            new Dictionary<Expression, ExpressionCompilation>
            {
                [Expression.EnvironmentInstance] = new(frame, Specialized: [])
            });

    private static void AssertPersistence(StackFrameInstructions frame)
    {
        var rendered =
            StackInstructionTraceRenderer.RenderStackFrameInstructions(
                frame,
                StackInstructionTraceRenderer.RenderingOptions.Persistence);

        var restored =
            StackInstructionTextFormat.ParseStackFrameInstructions(
                rendered.Text,
                hash => rendered.ObservedValues[hash]);

        restored.Instructions.Should().Equal(frame.Instructions);
        restored.MaxStackUsage.Should().Be(frame.MaxStackUsage);
        StackInstructionTraceRenderer.RenderStackFrameInstructions(restored).Should().Contain("Int_Div_");
    }
}
