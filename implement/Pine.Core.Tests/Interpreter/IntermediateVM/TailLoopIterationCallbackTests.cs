using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

/// <summary>
/// Tests for the per-iteration callback (<see cref="ReportTailLoopIteration"/>)
/// of <see cref="Core.Interpreter.IntermediateVM.PineVM"/>. The callback is
/// distinct from <c>reportFunctionApplication</c> and fires once per tail-loop
/// iteration, where an iteration is either a backward jump within a single
/// stack frame, or a tail-call invocation that replaces the current stack
/// frame.
/// </summary>
public class TailLoopIterationCallbackTests
{
    [Fact]
    public void Callback_fires_for_each_backward_jump_in_same_frame()
    {
        // Arrange: a frame whose body is an infinite backward-jump loop,
        // bounded by a small LoopIterationCountLimit so the evaluation
        // terminates with a known number of backward jumps.
        const int loopIterationCountLimit = 5;

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

        var rootExpression =
            new Expression.ParseAndEval(
                encoded:
                Expression.LiteralInstance(
                    ExpressionEncoding.EncodeExpressionAsValue(targetExpression)),
                environment: Expression.EnvironmentInstance);

        var iterations = new List<TailLoopIteration>();

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
                        Generic: loopingInstructions,
                        Specialized: []),
                },
                reportTailLoopIteration:
                (in tailLoopIteration) =>
                iterations.Add(tailLoopIteration));

        // Act: evaluate. The infinite loop is terminated by the
        // LoopIterationCountLimit, returning an error, but the callback should
        // already have observed each backward jump.
        var evalResult =
            vm.EvaluateExpressionOnCustomStack(
                rootExpression: rootExpression,
                rootEnvironment: PineValue.EmptyList,
                config:
                new Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig(
                    InvocationCountLimit: null,
                    LoopIterationCountLimit: loopIterationCountLimit,
                    StackDepthLimit: null));

        evalResult.IsErrOrNull().Should().NotBeNull(
            "the bounded infinite loop is expected to terminate with a limit error");

        // Backward-jump iterations from the inner frame.
        var backwardJumpIterations =
            iterations
            .Where(item => item.Kind is TailLoopIterationKind.BackwardJump)
            .ToList();

        // The callback fires once per backward jump executed.
        // The VM reports the limit error after the (limit + 1)-th iteration.
        backwardJumpIterations.Should().HaveCount(
            loopIterationCountLimit + 1,
            "the callback fires once per backward jump, including the iteration that exceeds the limit");

        // All backward-jump iterations originate from the looping inner frame.
        backwardJumpIterations.Should().AllSatisfy(
            iteration =>
            {
                iteration.FrameExpression.Should().Be(targetExpression);
                iteration.StackFrameDepth.Should().Be(1);
            });

        // IterationIndex values are a strictly increasing sequence starting at 0.
        iterations.Select(item => item.IterationIndex)
            .Should().Equal(Enumerable.Range(0, iterations.Count).Select(i => (long)i));
    }

    [Fact]
    public void Callback_fires_for_tail_call_frame_replacement()
    {
        // The root expression is a ParseAndEval whose tail-position invocation
        // (followed immediately by Return) triggers tail-call optimization:
        // the new frame REPLACES the current one instead of growing the stack.
        // This must produce exactly one TailCallReplace notification.
        var nestedEnvironment = IntegerEncoding.EncodeSignedInteger(7);

        var rootExpression =
            new Expression.ParseAndEval(
                encoded:
                Expression.LiteralInstance(
                    ExpressionEncoding.EncodeExpressionAsValue(Expression.EnvironmentInstance)),
                environment:
                Expression.LiteralInstance(nestedEnvironment));

        var iterations = new List<TailLoopIteration>();

        var vm =
            Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: true,
                selectPrecompiled: null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: true,
                parseCache: null,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                reportTailLoopIteration:
                (in tailLoopIteration) =>
                iterations.Add(tailLoopIteration));

        var result = vm.EvaluateExpression(rootExpression, PineValue.EmptyBlob);

        result.IsOkOrNull().Should().Be(nestedEnvironment);

        // Exactly one tail-call replacement was performed (root frame replaced
        // by the nested ParseAndEval frame).
        iterations.Should().HaveCount(1);

        var only = iterations[0];

        only.Kind.Should().Be(TailLoopIterationKind.TailCallReplace);
        only.IterationIndex.Should().Be(0);

        only.StackFrameDepth.Should().Be(
            1,
            "tail-call replacement must not grow the stack");

        only.FrameExpression.Should().Be(
            Expression.EnvironmentInstance,
            "the FrameExpression of a TailCallReplace iteration is the new (replacing) frame's expression");

        only.FrameInput.EvaluatedArguments.Should().Equal(
            [nestedEnvironment],
            "the FrameInput of a TailCallReplace iteration is the live input of the replacing frame");
    }

    [Fact]
    public void Callback_fires_for_each_step_in_a_tail_call_chain()
    {
        // Two-step tail-call chain:
        //   root      = ParseAndEval(targetMid, env=Environment)
        //   targetMid = ParseAndEval(targetInner, env=Environment)
        //   targetInner = Environment
        //
        // Evaluation: root frame tail-calls targetMid (replacement #1),
        // targetMid tail-calls targetInner (replacement #2), targetInner
        // returns the environment value.
        var nestedEnvironment = IntegerEncoding.EncodeSignedInteger(42);

        var targetInner = Expression.EnvironmentInstance;

        var targetMid =
            new Expression.ParseAndEval(
                encoded:
                Expression.LiteralInstance(
                    ExpressionEncoding.EncodeExpressionAsValue(targetInner)),
                environment: Expression.EnvironmentInstance);

        var rootExpression =
            new Expression.ParseAndEval(
                encoded:
                Expression.LiteralInstance(
                    ExpressionEncoding.EncodeExpressionAsValue(targetMid)),
                environment:
                Expression.LiteralInstance(nestedEnvironment));

        var iterations = new List<TailLoopIteration>();

        var vm =
            Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: true,
                selectPrecompiled: null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: true,
                parseCache: null,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                reportTailLoopIteration:
                (in tailLoopIteration) =>
                iterations.Add(tailLoopIteration));

        var result = vm.EvaluateExpression(rootExpression, PineValue.EmptyBlob);

        result.IsOkOrNull().Should().Be(nestedEnvironment);

        // Both tail-call replacements should have been observed.
        iterations.Should().HaveCount(2);

        iterations.Should().AllSatisfy(
            item =>
            {
                item.Kind.Should().Be(TailLoopIterationKind.TailCallReplace);

                item.StackFrameDepth.Should().Be(
                    1,
                    "tail-call replacement keeps the stack at depth 1 throughout the chain");
            });

        iterations[0].IterationIndex.Should().Be(0);
        iterations[1].IterationIndex.Should().Be(1);

        iterations[0].FrameExpression.Should().Be(targetMid);
        iterations[1].FrameExpression.Should().Be(targetInner);
    }

    [Fact]
    public void Callback_is_not_invoked_when_no_tail_loop_iterations_occur()
    {
        // A pure leaf computation (no backward jumps, no tail calls) must
        // produce zero notifications.
        var iterations = new List<TailLoopIteration>();

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
                reportTailLoopIteration:
                (in tailLoopIteration) =>
                iterations.Add(tailLoopIteration));

        var expression =
            Expression.KernelApplicationInstance(
                nameof(KernelFunction.int_add),
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(3)),
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(4))
                    ]));

        var result = vm.EvaluateExpression(expression, PineValue.EmptyBlob);

        result.IsOkOrNull().Should().Be(IntegerEncoding.EncodeSignedInteger(7));

        iterations.Should().BeEmpty(
            "evaluating a pure leaf expression performs no tail-loop iterations");
    }
}
