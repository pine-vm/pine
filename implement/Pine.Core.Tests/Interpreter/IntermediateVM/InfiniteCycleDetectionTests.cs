using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

/// <summary>
/// Regression tests for infinite-cycle detection in the intermediate
/// <see cref="Core.Interpreter.IntermediateVM.PineVM"/>.
///
/// <para />
/// The VM is required to detect a number of distinct infinite-cycle shapes
/// and to report them with an error message that explicitly mentions either
/// "infinite loop" or "infinite recursion", states the length of the cycle,
/// and includes a stack trace ending where the cycle was entered. The check
/// is amortised: the VM does not perform the (relatively expensive) check on
/// every loop / jump / invocation, but only every
/// <see cref="Core.Interpreter.IntermediateVM.PineVM.InfiniteCycleCheckIterationInterval"/>
/// iterations of the main interpreter loop. In addition the VM is required
/// to perform a check whenever it is about to return one of the configured
/// limit errors so that an infinite cycle is reported as such even when
/// execution would otherwise hit a configured limit first.
/// </summary>
public class InfiniteCycleDetectionTests
{
    [Fact]
    public void Direct_infinite_loop_via_backward_jump_is_detected()
    {
        // A frame whose body is an infinite loop:
        //   0: Push_Literal(EmptyList)  (stack: [[]])
        //   1: Pop                       (stack: [])
        //   2: Jump_Unconditional(-2)    (IP 2 -> 0)
        //
        // The unconditional backward jump at index 2 keeps the frame in an
        // infinite cycle that never advances. The check interval is 40 000
        // VM instructions; this loop body executes 3 instructions per
        // iteration, so the cycle period in VM instructions does not divide
        // the check interval. The VM must still detect the cycle within a
        // small number of checks.
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

        var targetExpression = Expression.EnvironmentInstance;

        var error =
            EvaluateWithInjectedCompilationExpectingError(
                targetExpression: targetExpression,
                rootEnvironment: PineValue.EmptyList,
                genericInstructions: loopingInstructions,
                config: new Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig(
                    InvocationCountLimit: null,
                    // Pick a loop-iteration limit that is large enough to allow
                    // the cycle detector several check intervals worth of work
                    // before the limit fires, but small enough to keep the test
                    // fast in case detection regresses.
                    LoopIterationCountLimit: 200_000,
                    StackDepthLimit: null));

        error.Message.Should().Contain(
            "infinite loop",
            "the VM must explicitly identify the failure as an infinite loop");

        error.Message.Should().Contain(
            "Cycle length",
            "the error must explicitly state the length of the detected cycle");

        error.Message.Should().Contain(
            "Stack trace",
            "the error must include a stack trace ending where the cycle entered");

        error.StackTrace.Should().NotBeEmpty(
            "the structured stack trace must include the looping frame");

        error.StackTrace[0].Should().Be(
            targetExpression,
            "the innermost frame in the trace must be the frame that is looping (cycle entry point)");
    }

    [Fact]
    public void Direct_infinite_recursion_with_same_expression_and_same_environment_is_detected()
    {
        // Build an expression whose evaluation tail-calls itself with the
        // exact same environment value:
        //
        //   ParseAndEval(encoded = literal(self), environment = Environment)
        //
        // The encoded sub-expression is a literal that, when evaluated,
        // yields the encoded form of the outer expression. Therefore each
        // invocation re-enters the same expression with the same environment
        // value, producing a cycle of length 1.
        var selfExpressionPlaceholder =
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(PineValue.EmptyList),
                environment: Expression.EnvironmentInstance);

        // Replace the literal with the encoded form of the placeholder itself
        // by going via a separate ParseAndEval that reads the encoded
        // expression from the environment:
        //   environment = [encoded_self_expression, ...]
        //   target      = ParseAndEval(encoded = head env, environment = env)
        var headOfEnv =
            Expression.KernelApplicationInstance(
                function: nameof(KernelFunction.head),
                input: Expression.EnvironmentInstance);

        var selfRecursiveExpression =
            new Expression.ParseAndEval(
                encoded: headOfEnv,
                environment: Expression.EnvironmentInstance);

        var rootEnvironment =
            PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(selfRecursiveExpression),
                ]);

        var error =
            EvaluateExpressionExpectingError(
                rootExpression: selfRecursiveExpression,
                rootEnvironment: rootEnvironment,
                config: new Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig(
                    InvocationCountLimit: 200_000,
                    LoopIterationCountLimit: null,
                    StackDepthLimit: null));

        error.Message.Should().Contain(
            "infinite recursion",
            "the VM must explicitly identify the failure as infinite recursion");

        error.Message.Should().Contain(
            "Cycle length: 1 invocation",
            "direct same-expression / same-environment recursion has a cycle of exactly one invocation");

        error.Message.Should().Contain(
            "Stack trace",
            "the error must include a stack trace ending where the cycle entered");

        error.StackTrace.Should().NotBeEmpty(
            "the trace must contain at least the cycling frame");
    }

    [Fact]
    public void Indirect_infinite_recursion_with_same_function_cycling_argument_values_is_detected()
    {
        // Build a tail-recursive function that toggles its single argument
        // between two values forever:
        //
        //   selfExpr = ParseAndEval(
        //                encoded     = head env,
        //                environment = [head env, toggle (head (skip 1 env))])
        //
        // where toggle(x) computes (1 - x). Starting from arg = 0 the
        // sequence of arguments observed is 0, 1, 0, 1, ... which is a
        // cycle of length 2 invocations of the same expression.
        var selfReference =
            Expression.KernelApplicationInstance(
                function: nameof(KernelFunction.head),
                input: Expression.EnvironmentInstance);

        var argReference =
            Expression.KernelApplicationInstance(
                function: nameof(KernelFunction.head),
                input:
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.skip),
                    input:
                    Expression.ListInstance(
                        [
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                        Expression.EnvironmentInstance,
                        ])));

        // toggledArg = 1 + (-1) * argReference  =  1 - argReference
        var toggledArg =
            Expression.KernelApplicationInstance(
                function: nameof(KernelFunction.int_add),
                input:
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                    Expression.KernelApplicationInstance(
                        function: nameof(KernelFunction.negate),
                        input: argReference),
                    ]));

        var selfRecursiveExpression =
            new Expression.ParseAndEval(
                encoded: selfReference,
                environment:
                Expression.ListInstance(
                    [
                    selfReference,
                    toggledArg,
                    ]));

        var rootEnvironment =
            PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(selfRecursiveExpression),
                IntegerEncoding.EncodeSignedInteger(0),
                ]);

        var error =
            EvaluateExpressionExpectingError(
                rootExpression: selfRecursiveExpression,
                rootEnvironment: rootEnvironment,
                config: new Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig(
                    InvocationCountLimit: 200_000,
                    LoopIterationCountLimit: null,
                    StackDepthLimit: null));

        error.Message.Should().Contain(
            "infinite recursion",
            "the VM must explicitly identify the failure as infinite recursion");

        error.Message.Should().Contain(
            "Cycle length: 2 invocations",
            "the cycle alternates between two distinct argument values, so its length is 2 invocations");

        error.Message.Should().Contain(
            "Stack trace",
            "the error must include a stack trace ending where the cycle entered");
    }

    [Fact]
    public void Indirect_infinite_recursion_via_two_mutually_recursive_functions_is_detected()
    {
        // Two distinct expressions that tail-call each other forever:
        //   f := ParseAndEval(encoded = (head (skip 1 env)), environment = env)
        //   g := ParseAndEval(encoded = (head env),         environment = env)
        //
        // Initial environment is [encoded(g), encoded(f)] so that:
        //   evaluating f re-enters with encoded = (head (skip 1 env)) = encoded(f),
        //                                wait — let's design this carefully.
        //
        // Instead, use:
        //   f calls g with env = [encoded(g), encoded(f)]
        //   g calls f with env = [encoded(g), encoded(f)]
        //
        // f: ParseAndEval(encoded = head env,                     environment = env)
        //    -> evaluates encoded(g); next frame is g.
        // g: ParseAndEval(encoded = head (skip 1 env),            environment = env)
        //    -> evaluates encoded(f); next frame is f.
        //
        // So the sequence of frame expressions is f, g, f, g, ... a cycle of
        // length 2.
        var headOfEnv =
            Expression.KernelApplicationInstance(
                function: nameof(KernelFunction.head),
                input: Expression.EnvironmentInstance);

        var headOfSkipOneEnv =
            Expression.KernelApplicationInstance(
                function: nameof(KernelFunction.head),
                input:
                Expression.KernelApplicationInstance(
                    function: nameof(KernelFunction.skip),
                    input:
                    Expression.ListInstance(
                        [
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                        Expression.EnvironmentInstance,
                        ])));

        var fExpression =
            new Expression.ParseAndEval(
                encoded: headOfEnv,
                environment: Expression.EnvironmentInstance);

        var gExpression =
            new Expression.ParseAndEval(
                encoded: headOfSkipOneEnv,
                environment: Expression.EnvironmentInstance);

        var rootEnvironment =
            PineValue.List(
                [
                ExpressionEncoding.EncodeExpressionAsValue(gExpression),
                ExpressionEncoding.EncodeExpressionAsValue(fExpression),
                ]);

        // Sanity-check the design: the two expressions must actually be
        // distinct. If they collide the test would degenerate to the
        // direct-recursion case which is already covered.
        fExpression.Should().NotBe(gExpression);

        var error =
            EvaluateExpressionExpectingError(
                rootExpression: fExpression,
                rootEnvironment: rootEnvironment,
                config: new Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig(
                    InvocationCountLimit: 200_000,
                    LoopIterationCountLimit: null,
                    StackDepthLimit: null));

        error.Message.Should().Contain(
            "infinite recursion",
            "the VM must explicitly identify the failure as infinite recursion");

        error.Message.Should().Contain(
            "Cycle length: 2 invocations",
            "two mutually recursive functions form a cycle of length 2 invocations");

        error.Message.Should().Contain(
            "Stack trace",
            "the error must include a stack trace ending where the cycle entered");
    }

    private static EvaluationError EvaluateWithInjectedCompilationExpectingError(
        Expression targetExpression,
        PineValue rootEnvironment,
        StackFrameInstructions genericInstructions,
        Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig config)
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

        var result =
            vm.EvaluateExpressionOnCustomStack(
                rootExpression: rootExpression,
                rootEnvironment: rootEnvironment,
                config: config);

        return
            result
            .Unpack(
                fromErr: err => err,
                fromOk: ok =>
                    throw new InvalidOperationException(
                        "Expected the evaluation to fail with an infinite-cycle error " +
                        "but it returned successfully with value: " + ok.ReturnValue.Evaluate()));
    }

    private static EvaluationError EvaluateExpressionExpectingError(
        Expression rootExpression,
        PineValue rootEnvironment,
        Core.Interpreter.IntermediateVM.PineVM.EvaluationConfig config)
    {
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
                reportExecutedStackInstruction: null);

        var result =
            vm.EvaluateExpressionOnCustomStack(
                rootExpression: rootExpression,
                rootEnvironment: rootEnvironment,
                config: config);

        return
            result
            .Unpack(
                fromErr: err => err,
                fromOk: ok =>
                    throw new InvalidOperationException(
                        "Expected the evaluation to fail with an infinite-cycle error " +
                        "but it returned successfully with value: " + ok.ReturnValue.Evaluate()));
    }
}
