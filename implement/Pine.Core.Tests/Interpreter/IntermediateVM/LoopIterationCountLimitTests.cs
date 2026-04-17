using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

/// <summary>
/// Tests verifying that the intermediate <see cref="Core.Interpreter.IntermediateVM.PineVM"/>
/// honours <see cref="Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig.LoopIterationCountLimit"/>
/// and returns an error when the configured number of loop iterations (backward jumps) is exceeded.
///
/// Each test injects a sequence of IR instructions that contains an unconditional backward jump,
/// which produces an infinite loop inside a single stack frame.
/// </summary>
public class LoopIterationCountLimitTests
{
    [Fact]
    public void Unconditional_backwards_jump_returns_error_when_loop_iteration_count_limit_exceeded()
    {
        const int loopIterationCountLimit = 5;

        var targetExpression = Expression.EnvironmentInstance;

        // A frame whose body is an infinite loop:
        //   0: Push_Literal(EmptyList)  (stack: [[]])
        //   1: Pop                       (stack: [])
        //   2: Jump_Unconditional(-2)    (IP 2 -> 0)
        //
        // The unconditional backward jump at index 2 increments the loop iteration counter
        // on every iteration; there is no path out of this frame.
        var loopingInstructions =
            new StackFrameInstructions(
                Parameters: StaticFunctionInterface.FromPathsSorted([]),
                Instructions:
                [
                StackInstruction.Push_Literal(PineValue.EmptyList),
                StackInstruction.Pop,
                StackInstruction.Jump_Unconditional(-2),
                ],
                TrackEnvConstraint: null);

        var evaluationResult =
            EvaluateWithInjectedCompilation(
                targetExpression: targetExpression,
                rootEnvironment: PineValue.EmptyList,
                genericInstructions: loopingInstructions,
                loopIterationCountLimit: loopIterationCountLimit);

        var error =
            evaluationResult
            .Unpack(
                fromErr: err => err,
                fromOk: _ => (string?)null);

        error.Should().NotBeNull(
            "evaluating an infinite loop with a loop iteration count limit must return an error");

        error!.Should().Contain(
            "Loop iteration count limit exceeded",
            "the error returned by the VM should identify the kind of limit that was hit");

        error!.Should().Contain(
            loopIterationCountLimit.ToString(),
            "the error message should report the configured limit value");
    }

    [Fact]
    public void Unconditional_backwards_jump_error_reports_configured_limit()
    {
        // Varying the configured limit should change the reported limit value in the error,
        // which demonstrates that the VM is actually observing the configuration option.
        const int loopIterationCountLimit = 17;

        var targetExpression = Expression.EnvironmentInstance;

        var loopingInstructions =
            new StackFrameInstructions(
                Parameters: StaticFunctionInterface.FromPathsSorted([]),
                Instructions:
                [
                StackInstruction.Push_Literal(PineValue.EmptyList),
                StackInstruction.Pop,
                StackInstruction.Jump_Unconditional(-2),
                ],
                TrackEnvConstraint: null);

        var evaluationResult =
            EvaluateWithInjectedCompilation(
                targetExpression: targetExpression,
                rootEnvironment: PineValue.EmptyList,
                genericInstructions: loopingInstructions,
                loopIterationCountLimit: loopIterationCountLimit);

        var error =
            evaluationResult
            .Unpack(
                fromErr: err => err,
                fromOk: _ => (string?)null);

        error.Should().NotBeNull();

        error!.Should().Contain("Loop iteration count limit exceeded");

        error!.Should().Contain(loopIterationCountLimit.ToString());
    }

    private static Result<string, EvaluationReport>
        EvaluateWithInjectedCompilation(
        Expression targetExpression,
        PineValue rootEnvironment,
        StackFrameInstructions genericInstructions,
        int? loopIterationCountLimit)
    {
        var rootExpression =
            new Expression.ParseAndEval(
                encoded:
                Expression.LiteralInstance(
                    ExpressionEncoding.EncodeExpressionAsValue(targetExpression)),
                environment: Expression.EnvironmentInstance);

        var expressionCompilationOverrides =
            new Dictionary<Expression, ExpressionCompilation>
            {
                [targetExpression] =
                new ExpressionCompilation(
                    Generic: genericInstructions,
                    Specialized: []),
            };

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
                reportExecutedStackInstruction: null,
                expressionCompilationOverrides: expressionCompilationOverrides);

        return
            vm.EvaluateExpressionOnCustomStack(
                rootExpression: rootExpression,
                rootEnvironment: rootEnvironment,
                config:
                new Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig(
                    InvocationCountLimit: null,
                    LoopIterationCountLimit: loopIterationCountLimit,
                    StackDepthLimit: null));
    }
}
