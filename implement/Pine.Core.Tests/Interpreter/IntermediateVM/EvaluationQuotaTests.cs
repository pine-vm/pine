using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using Xunit;

using IntermediatePineVM = Pine.Core.Interpreter.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

public class EvaluationQuotaTests
{
    [Fact]
    public void Backward_jump_exhausts_loop_quota_with_current_frame_details()
    {
        const int quota = 5;

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

        var error =
            EvaluateWithInjectedCompilationExpectingError(
                targetExpression,
                loopingInstructions,
                new IntermediatePineVM.EvaluationConfig(
                    InvocationCountLimit: null,
                    LoopIterationCountLimit: quota,
                    StackDepthLimit: null));

        error.Reason.Should().Be(
            new EvaluationErrorReason.QuotaExhausted(
                EvaluationQuotaKind.LoopIterationCount,
                quota));

        error.Counters.LoopIterationCount.Should().Be(quota + 1);
        error.StackTrace.Should().NotBeEmpty();
        error.StackTrace[0].Expression.Should().Be(targetExpression);
        error.StackTrace[0].Input.Should().NotBeNull();
        error.StackTrace[0].Instructions.Should().BeSameAs(loopingInstructions);
        error.StackTrace[0].InstructionPointer.Should().Be(0);

        EvaluationError.RenderDisplayString(error)
            .Should().Contain("Loop iteration count limit exceeded: 5");
    }

    [Fact]
    public void Recursive_invocations_exhaust_invocation_quota_with_inputs_for_analysis()
    {
        const int quota = 12;

        var (expression, environment) = BuildDirectlyRecursiveExpression();

        var error =
            EvaluateExpressionExpectingError(
                expression,
                environment,
                new IntermediatePineVM.EvaluationConfig(
                    InvocationCountLimit: quota,
                    LoopIterationCountLimit: null,
                    StackDepthLimit: null));

        error.Reason.Should().Be(
            new EvaluationErrorReason.QuotaExhausted(
                EvaluationQuotaKind.InvocationCount,
                quota));

        error.Counters.InvocationCount.Should().Be(quota + 1);
        error.StackTrace.Should().HaveCountGreaterThan(2);
        error.StackTrace[0].Expression.Should().Be(expression);
        error.StackTrace[1].Expression.Should().Be(expression);
        error.StackTrace[0].Input.Should().NotBeNull();
        error.StackTrace[1].Input.Should().NotBeNull();

        error.StackTrace[0].Input!.Equals(error.StackTrace[1].Input).Should().BeTrue(
            "a caller can inspect repeated expressions and inputs to diagnose recursion");
    }

    [Fact]
    public void Recursive_invocations_exhaust_stack_depth_quota()
    {
        const int quota = 5;

        var (expression, environment) = BuildDirectlyRecursiveExpression();

        var error =
            EvaluateExpressionExpectingError(
                expression,
                environment,
                new IntermediatePineVM.EvaluationConfig(
                    InvocationCountLimit: null,
                    LoopIterationCountLimit: null,
                    StackDepthLimit: quota));

        error.Reason.Should().Be(
            new EvaluationErrorReason.QuotaExhausted(
                EvaluationQuotaKind.StackDepth,
                quota));

        error.StackTrace.Should().HaveCount(quota + 1);
        error.StackTrace.Should().OnlyContain(frame => frame.Expression == expression);
    }

    [Fact]
    public void Precompiled_continue_eval_exhausts_invocation_quota()
    {
        const int quota = 5;

        var targetExpression = Expression.EnvironmentInstance;
        var encodedTargetExpression = ExpressionEncoding.EncodeExpressionAsValue(targetExpression);

        var rootExpression =
            new Expression.Eval(
                encoded: Expression.LitralInst(encodedTargetExpression),
                environment: Expression.EnvironmentInstance);

        var vm =
            IntermediatePineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: true,
                selectPrecompiled:
                (expression, environment, _) =>
                expression == targetExpression
                ?
                () =>
                new PrecompiledResult.ContinueEval(
                    EnvironmentValue: environment.Evaluate(),
                    ExpressionValue: encodedTargetExpression)
                :
                null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: false,
                parseCache: null,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                reportExecutedStackInstruction: null);

        var error =
            vm.EvaluateExpressionOnCustomStack(
                rootExpression,
                PineValue.EmptyList,
                new IntermediatePineVM.EvaluationConfig(
                    InvocationCountLimit: quota,
                    LoopIterationCountLimit: null,
                    StackDepthLimit: null))
            .IsErrOrNull();

        error.Should().NotBeNull();

        error!.Reason.Should().Be(
            new EvaluationErrorReason.QuotaExhausted(
                EvaluationQuotaKind.InvocationCount,
                quota));

        error.Counters.InvocationCount.Should().Be(quota + 1);
    }

    private static (Expression Expression, PineValue Environment) BuildDirectlyRecursiveExpression()
    {
        var headOfEnvironment =
            Expression.BuiltinInst(
                function: nameof(BuiltinFunction.head),
                input: Expression.EnvironmentInstance);

        var recursiveInvocation =
            new Expression.Eval(
                encoded: headOfEnvironment,
                environment: Expression.EnvironmentInstance);

        var expression =
            Expression.ListInst(
                [
                recursiveInvocation,
                ]);

        var environment =
            PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(expression),
                ]);

        return (expression, environment);
    }

    private static EvaluationError EvaluateWithInjectedCompilationExpectingError(
        Expression targetExpression,
        StackFrameInstructions genericInstructions,
        IntermediatePineVM.EvaluationConfig config)
    {
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
                    Generic: genericInstructions,
                    Specialized: []),
            };

        var vm =
            IntermediatePineVM.CreateCustom(
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
                rootExpression,
                PineValue.EmptyList,
                config)
            .Unpack(
                fromErr: error => error,
                fromOk: report =>
                throw new InvalidOperationException(
                    "Expected quota exhaustion, but evaluation returned: " +
                    report.ReturnValue.Evaluate()));
    }

    private static EvaluationError EvaluateExpressionExpectingError(
        Expression rootExpression,
        PineValue rootEnvironment,
        IntermediatePineVM.EvaluationConfig config)
    {
        var vm =
            IntermediatePineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: true,
                disableDirectContinueForSimpleEval: true,
                selectPrecompiled: null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: false,
                parseCache: null,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                reportExecutedStackInstruction: null);

        return
            vm.EvaluateExpressionOnCustomStack(
                rootExpression,
                rootEnvironment,
                config)
            .Unpack(
                fromErr: error => error,
                fromOk: report =>
                throw new InvalidOperationException(
                    "Expected quota exhaustion, but evaluation returned: " +
                    report.ReturnValue.Evaluate()));
    }
}
