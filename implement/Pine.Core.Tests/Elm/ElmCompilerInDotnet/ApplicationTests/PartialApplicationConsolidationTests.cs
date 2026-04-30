using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System.Linq;
using Xunit;

using PineCodeAnalysis = Pine.Core.CodeAnalysis.CodeAnalysis;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ApplicationTests;

/// <summary>
/// Tests demonstrating that the PineVM's intermediate-VM expression reduction consolidates
/// the chain of nested <see cref="Expression.ParseAndEval"/> expressions emitted by the
/// Elm compiler in ElmCompilerInDotnet for the generic form of partial application.
/// <para>
/// The Elm compiler emits only two forms of applications: a saturated form when the
/// argument count is at least the parameter count, and a generic form that uses one
/// <see cref="Expression.ParseAndEval"/> nesting per applied argument
/// (constructed via
/// <see cref="PineCodeAnalysis.BuildGenericFunctionApplication(Expression, System.Collections.Generic.IReadOnlyList{Expression})"/>).
/// The new optimization in <see cref="ReducePineExpression"/> recognizes this generic form
/// when the head function expression is a literal and consolidates the chain of K nested
/// ParseAndEval expressions into a single one — independent of K.
/// </para>
/// <para>
/// The tests exercise two scenarios using gamma functions whose body matches the shape
/// suggested in the task: a 3-parameter and a 4-parameter version of <c>gamma</c>, each
/// dispatching to one of two helper functions <c>alfa</c> and <c>beta</c>. In each scenario,
/// gamma is invoked through a partial-then-apply path where the partial application happens
/// in one context (the "partial helper") and the final argument is applied in another
/// context (the "apply-one-arg helper"). The "across contexts" boundary is modeled directly
/// as it appears in compiled code: the partial helper's body has the literal-headed chain
/// emitted by <c>BuildGenericFunctionApplication</c>; the apply-one-arg helper's body is a
/// <see cref="Expression.ParseAndEval"/> whose head is an environment-access expression
/// (a function parameter, not a literal) — so the consolidation cannot fire across the
/// boundary.
/// </para>
/// <para>
/// The test asserts that after consolidation only two ParseAndEval invocations remain per
/// call to gamma — one for the partial batch, one for the final argument applied in the
/// other context — regardless of how many arguments are in the partial batch (two for the
/// 3-parameter scenario, three for the 4-parameter scenario).
/// </para>
/// </summary>
public class PartialApplicationConsolidationTests
{
    private const string TestModuleText =
        """"
        module TestModule exposing (..)


        alfa : Int -> Int -> Int -> Int
        alfa p0 p1 p2 =
            Pine_kernel.int_add
                [ p0
                , Pine_kernel.int_mul [ 13, p1, Pine_kernel.int_add [ 17, p2 ] ]
                ]


        beta : Int -> Int -> Int -> Int
        beta p0 p1 p2 =
            Pine_kernel.int_add
                [ p2
                , Pine_kernel.int_mul [ 19, p1, Pine_kernel.int_add [ 21, p0 ] ]
                ]


        gamma3 : Int -> Int -> Int -> Int
        gamma3 p0 p1 p2 =
            if Pine_kernel.equal [ 41, p0 ] then
                alfa 43 p1 p2

            else
                beta 49 p1 p2


        alfa4 : Int -> Int -> Int -> Int -> Int
        alfa4 p0 p1 p2 p3 =
            Pine_kernel.int_add
                [ p0
                , Pine_kernel.int_mul [ 13, p1, p3, Pine_kernel.int_add [ 17, p2 ] ]
                ]


        beta4 : Int -> Int -> Int -> Int -> Int
        beta4 p0 p1 p2 p3 =
            Pine_kernel.int_add
                [ p2
                , Pine_kernel.int_mul [ 19, p1, p3, Pine_kernel.int_add [ 21, p0 ] ]
                ]


        gamma4 : Int -> Int -> Int -> Int -> Int
        gamma4 p0 p1 p2 p3 =
            if Pine_kernel.equal [ 41, p0 ] then
                alfa4 43 p1 p2 p3

            else
                beta4 49 p1 p2 p3
        """";

    private static readonly ElmInteractiveEnvironment.ParsedInteractiveEnvironment s_parsedEnv =
        ElmCompilerTestHelper.CompileElmModules(
            [TestModuleText],
            disableInlining: true).parsedEnv;

    private static PineValue GetTestFunction(string name) =>
        s_parsedEnv.Modules
        .First(m => m.moduleName is "TestModule")
        .moduleContent.FunctionDeclarations[name];

    private static readonly PineVMParseCache s_parseCache = new();

    /// <summary>
    /// Counts the number of <see cref="Expression.ParseAndEval"/> nodes in an expression tree.
    /// </summary>
    private static int CountParseAndEval(Expression expression)
    {
        var count = 0;

        foreach (var sub in Expression.EnumerateSelfAndDescendants(expression))
        {
            if (sub is Expression.ParseAndEval)
            {
                ++count;
            }
        }

        return count;
    }

    /// <summary>
    /// Builds the body of a "partial application helper": the literal-headed chain of K
    /// nested ParseAndEval expressions emitted by the Elm compiler for the generic form.
    /// Argument expressions are treated as readings from the helper's local environment
    /// (which, in compiled code, is what the Elm compiler generates for the helper's
    /// parameters).
    /// </summary>
    private static Expression BuildPartialHelperBody(
        PineValue functionValue,
        int partialArgsCount)
    {
        var argExpressions = new Expression[partialArgsCount];

        for (var i = 0; i < partialArgsCount; ++i)
        {
            // Each argument is read from the helper's environment as a list element.
            argExpressions[i] = ListEnvAccess(i);
        }

        return
            PineCodeAnalysis.BuildGenericFunctionApplication(
                Expression.LiteralInstance(functionValue),
                argExpressions);
    }

    /// <summary>
    /// Builds the body of the "apply-one-arg helper": a single ParseAndEval whose encoded
    /// operand is a parameter of the helper (an environment access, not a literal). This
    /// is the form the Elm compiler emits for <c>p q -&gt; p q</c> when <c>p</c> is a
    /// parameter rather than a known function.
    /// </summary>
    private static Expression BuildApplyOneArgHelperBody() =>
        new Expression.ParseAndEval(
            encoded: ListEnvAccess(0),
            environment: ListEnvAccess(1));

    /// <summary>
    /// Helper: build a <c>head</c>-and-<c>skip</c> kernel application reading the
    /// <paramref name="index"/>-th element of the current environment.
    /// </summary>
    private static Expression ListEnvAccess(int index)
    {
        Expression current = Expression.EnvironmentInstance;

        for (var i = 0; i < index; ++i)
        {
            current =
                Expression.KernelApplicationInstance(
                    nameof(KernelFunction.skip),
                    Expression.ListInstance(
                        [
                        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                        current
                        ]));
        }

        return Expression.KernelApplicationInstance(nameof(KernelFunction.head), current);
    }

    /// <summary>
    /// 3-parameter gamma: partial application with two arguments in one context, then
    /// receiving the final argument in another context. The partial helper's body has 2
    /// nested ParseAndEval over a literal head (gamma3); the apply-one-arg helper's body
    /// has 1 ParseAndEval over a non-literal head (its function-parameter).
    /// </summary>
    [Theory]
    [InlineData(10)]
    [InlineData(20)]
    public void Three_param_gamma_partial_two_args_then_final_consolidates_to_two_parse_and_eval(
        int callCount)
    {
        var gamma3 = GetTestFunction("gamma3");

        var partialHelperBody =
            BuildPartialHelperBody(gamma3, partialArgsCount: 2);

        var applyOneArgHelperBody = BuildApplyOneArgHelperBody();

        // The partial helper body uses gamma `callCount` times in one expression (modeling
        // the "use the outer function gamma ten/twenty times in one invocation" requirement
        // from the task statement, applied to the helper's body).
        var partialHelperBatch =
            Expression.ListInstance(
                [.. Enumerable.Repeat(partialHelperBody, callCount)]);

        var applyOneArgHelperBatch =
            Expression.ListInstance(
                [.. Enumerable.Repeat(applyOneArgHelperBody, callCount)]);

        // Before reduction:
        //   - the partial helper's body has 2 ParseAndEval per gamma call (the chain),
        //   - the apply-one-arg helper's body has 1 ParseAndEval per gamma call.
        //
        // For a `callCount`-sized batch, that is 3 * callCount ParseAndEval nodes total.
        (CountParseAndEval(partialHelperBatch) + CountParseAndEval(applyOneArgHelperBatch))
            .Should().Be(3 * callCount);

        var reducedPartialHelperBatch =
            ReducePineExpression.ReduceExpressionBottomUp(
                partialHelperBatch,
                s_parseCache,
                reducedExpressionCache: null);

        var reducedApplyOneArgHelperBatch =
            ReducePineExpression.ReduceExpressionBottomUp(
                applyOneArgHelperBatch,
                s_parseCache,
                reducedExpressionCache: null);

        // After consolidation:
        //   - the partial helper's body collapses from 2 to 1 ParseAndEval per gamma call,
        //   - the apply-one-arg helper's body is unchanged at 1 ParseAndEval per gamma call.
        //
        // Across the two batches we now observe exactly two ParseAndEval invocations per
        // call to gamma, regardless of the partial batch size.
        (CountParseAndEval(reducedPartialHelperBatch) +
        CountParseAndEval(reducedApplyOneArgHelperBatch))
            .Should().Be(2 * callCount);
    }

    /// <summary>
    /// 4-parameter gamma: partial application with three arguments in one context, then
    /// receiving the final argument in another context. The partial helper's body has 3
    /// nested ParseAndEval over a literal head (gamma4); the apply-one-arg helper's body
    /// has 1 ParseAndEval over a non-literal head. After consolidation, the partial
    /// helper's body collapses to a single ParseAndEval and we observe exactly two
    /// ParseAndEval invocations per call to gamma — the same total as in the 3-parameter
    /// scenario, demonstrating the optimization's independence from the partial batch size.
    /// </summary>
    [Theory]
    [InlineData(10)]
    [InlineData(20)]
    public void Four_param_gamma_partial_three_args_then_final_consolidates_to_two_parse_and_eval(
        int callCount)
    {
        var gamma4 = GetTestFunction("gamma4");

        var partialHelperBody =
            BuildPartialHelperBody(gamma4, partialArgsCount: 3);

        var applyOneArgHelperBody = BuildApplyOneArgHelperBody();

        var partialHelperBatch =
            Expression.ListInstance(
                [.. Enumerable.Repeat(partialHelperBody, callCount)]);

        var applyOneArgHelperBatch =
            Expression.ListInstance(
                [.. Enumerable.Repeat(applyOneArgHelperBody, callCount)]);

        // Before reduction: 3 + 1 = 4 ParseAndEval per gamma call.
        (CountParseAndEval(partialHelperBatch) + CountParseAndEval(applyOneArgHelperBatch))
            .Should().Be(4 * callCount);

        var reducedPartialHelperBatch =
            ReducePineExpression.ReduceExpressionBottomUp(
                partialHelperBatch,
                s_parseCache,
                reducedExpressionCache: null);

        var reducedApplyOneArgHelperBatch =
            ReducePineExpression.ReduceExpressionBottomUp(
                applyOneArgHelperBatch,
                s_parseCache,
                reducedExpressionCache: null);

        // After consolidation: 1 + 1 = 2 ParseAndEval per gamma call.
        (CountParseAndEval(reducedPartialHelperBatch) +
        CountParseAndEval(reducedApplyOneArgHelperBatch))
            .Should().Be(2 * callCount);
    }
}
