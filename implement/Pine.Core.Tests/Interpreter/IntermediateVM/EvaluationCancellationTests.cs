using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;
using Xunit;

using IntermediatePineVM = Pine.Core.Interpreter.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class EvaluationCancellationTests
{
    private static readonly IntermediatePineVM.EvaluationConfig s_unboundedConfig =
        new(
            InvocationCountLimit: null,
            LoopIterationCountLimit: null,
            StackDepthLimit: null);

    [Fact]
    public void Cancellation_is_checked_before_evaluation_starts()
    {
        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();

        var error =
            CreateVm()
            .EvaluateExpressionOnCustomStack(
                Expression.EnvironmentInstance,
                PineValue.EmptyList,
                s_unboundedConfig,
                cancellationToken: cancellation.Token)
            .IsErrOrNull();

        error.Should().NotBeNull();
        error!.Reason.Should().Be(new EvaluationErrorReason.CancellationRequested());
        error.StackTrace.Should().BeEmpty();

        error.Counters.Should().Be(
            new PerformanceCounters(
                InvocationCount: 0,
                BuildListCount: 0,
                LoopIterationCount: 0,
                InstructionCount: 0));
    }

    [Fact]
    public void Cancellable_VM_interface_reports_cancellation_to_host()
    {
        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();

        Action evaluate =
            () =>
            ((ICancellablePineVM)CreateVm())
            .EvaluateExpression(
                Expression.EnvironmentInstance,
                PineValue.EmptyList,
                cancellation.Token);

        evaluate.Should().Throw<OperationCanceledException>();
    }

    [Fact]
    public void Cancellation_is_checked_before_an_invocation()
    {
        var headOfEnvironment =
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.head),
                input: Expression.EnvironmentInstance);

        var expression =
            new Expression.Eval(
                encoded: headOfEnvironment,
                environment: Expression.EnvironmentInstance);

        var environment =
            PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(expression),
                ]);

        using var cancellation = new CancellationTokenSource();

        var error =
            CreateVm(disableDirectContinueForSimpleEval: true)
            .EvaluateExpressionOnCustomStack(
                expression,
                environment,
                s_unboundedConfig,
                reportEnteredStackFrame:
                (in EnteredStackFrame enteredFrame) =>
                {
                    if (enteredFrame.FrameExpression == expression)
                    {
                        cancellation.Cancel();
                    }
                },
                cancellationToken: cancellation.Token)
            .IsErrOrNull();

        error.Should().NotBeNull();
        error!.Reason.Should().Be(new EvaluationErrorReason.CancellationRequested());
        error.StackTrace.Should().ContainSingle();
        error.StackTrace[0].Expression.Should().Be(expression);
    }

    [Fact]
    public void Cancellation_is_checked_on_every_jump_instruction_kind()
    {
        StackInstruction[][] instructionVariants =
            [
            [
                StackInstruction.Jump_Unconditional(1),
                StackInstruction.Push_Literal(PineValue.EmptyList),
                StackInstruction.Return,
            ],
            [
                StackInstruction.Push_Literal(PineValue.EmptyList),
                StackInstruction.Jump_If_Equal(1, PineValue.EmptyList),
                StackInstruction.Push_Literal(PineValue.EmptyList),
                StackInstruction.Return,
            ],
            [
                StackInstruction.Push_Literal(PineValue.EmptyList),
                new StackInstruction(
                    StackInstructionKind.Switch_Jump_If_Equal_Const,
                    SwitchJumpTable: ImmutableDictionary<PineValue, int>.Empty),
                StackInstruction.Push_Literal(PineValue.EmptyList),
                StackInstruction.Return,
            ],
            ];

        foreach (var instructions in instructionVariants)
        {
            var targetExpression = Expression.EnvironmentInstance;

            var targetInstructions =
                new StackFrameInstructions(
                    Parameters: StaticFunctionInterface.FromPathsSorted([]),
                    Instructions: instructions,
                    TrackEnvConstraint: null);

            var rootExpression =
                new Expression.Eval(
                    encoded:
                    Expression.LitralInst(
                        ExpressionEncoding.EncodeExpressionAsValue(targetExpression)),
                    environment: Expression.EnvironmentInstance);

            var expressionCompilationOverrides =
                new Dictionary<Expression, ExpressionCompilation>
                {
                    [targetExpression] =
                    new ExpressionCompilation(
                        Generic: targetInstructions,
                        Specialized: []),
                };

            using var cancellation = new CancellationTokenSource();

            var error =
                CreateVm(expressionCompilationOverrides: expressionCompilationOverrides)
                .EvaluateExpressionOnCustomStack(
                    rootExpression,
                    PineValue.EmptyList,
                    s_unboundedConfig,
                    reportEnteredStackFrame:
                    (in EnteredStackFrame enteredFrame) =>
                    {
                        if (enteredFrame.FrameExpression == targetExpression)
                        {
                            cancellation.Cancel();
                        }
                    },
                    cancellationToken: cancellation.Token)
                .IsErrOrNull();

            error.Should().NotBeNull();
            error!.Reason.Should().Be(new EvaluationErrorReason.CancellationRequested());
            error.StackTrace[0].Expression.Should().Be(targetExpression);
        }
    }

    private static IntermediatePineVM CreateVm(
        bool disableDirectContinueForSimpleEval = false,
        IReadOnlyDictionary<Expression, ExpressionCompilation>? expressionCompilationOverrides = null) =>
        IntermediatePineVM.CreateCustom(
            evalCache: null,
            evaluationConfigDefault: null,
            reportFunctionApplication: null,
            compilationEnvClasses: null,
            disableReductionInCompilation: true,
            disableDirectContinueForSimpleEval: disableDirectContinueForSimpleEval,
            selectPrecompiled: null,
            skipInlineForExpression: _ => false,
            enableTailRecursionOptimization: false,
            parseCache: null,
            precompiledLeaves: null,
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null,
            reportExecutedStackInstruction: null,
            expressionCompilationOverrides: expressionCompilationOverrides);
}
