using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Interpreter.IntermediateVM;

/// <summary>
/// Focused regression test for the work tracked from
/// <c>SkipWhileWithoutLinebreakHelp_alpha_longer</c> in
/// <c>ParserFastTests</c>: the intermediate <see cref="PineVM"/> should inline
/// small non-recursive callees (such as <c>Char.isAlpha</c>) into a recursive
/// helper's compiled IR, so that each per-character iteration costs one
/// invocation rather than one for the helper plus one for the predicate.
/// <para>
/// This test constructs the Pine code directly via the <see cref="Expression"/>
/// API (no Elm compiler) and runs it on a <see cref="PineVM"/> whose
/// <c>reportExpressionCompiled</c> callback captures every distinct
/// expression that gets compiled. Two design points mirror how the predicate
/// is actually represented in the production scenario:
/// </para>
/// <list type="bullet">
///   <item>
///     <description>
///       The predicate body is a <c>Char.isAlpha</c>-equivalent: a let-binding
///       that prepends a positive sign byte to the input character (mapping
///       the raw 1-byte <c>Char</c> blob to a proper signed integer), then
///       checks two ranges via <c>int_is_sorted_asc</c> — the
///       upper-case <c>['A'..'Z']</c> range and the lower-case
///       <c>['a'..'z']</c> range. This matches the body in
///       <c>elm-in-elm/elm-kernel-modules/Char.elm:119-130</c>.
///     </description>
///   </item>
///   <item>
///     <description>
///       The predicate is referenced as a LITERAL expression embedded directly
///       in the helper body — not via an <c>env[1]</c> slot constrained by an
///       env-class. This models how the Elm compiler ultimately presents the
///       call site after lambda-lifting + call-site inlining at the
///       lifted-lambda layer: by the time the recursive helper's body is
///       compiled, the <c>isGood</c> reference has been substituted with the
///       <c>Char.isAlpha</c> literal-encoded value, so the per-iteration
///       <c>ParseAndEval</c> sees its <c>encoded</c> operand as a literal
///       independent of any environment slot.
///     </description>
///   </item>
/// </list>
/// <para>
/// The <em>helper-body snapshot</em> is the primary signal of the inlining
/// work: index 0–N should contain the inlined predicate's reduced kernel ops
/// (<c>concat</c> + two <c>int_is_sorted_asc</c>-derived range checks)
/// directly, with NO <c>Parse_And_Eval_Binary</c> for the predicate dispatch.
/// Any future inlining-threshold regression that re-rejects this body will
/// fail the snapshot here with a clear diff.
/// </para>
/// <para>
/// Per the testing convention for snapshot tests, the return-value assertion
/// comes first so any miscompilation surfaces before snapshot drift.
/// </para>
/// </summary>
public class InlineSmallNonRecursiveCalleeRegressionTests
{
    [Fact]
    public void Recursive_helper_body_inlines_small_non_recursive_predicate()
    {
        // -------- Predicate (Char.isAlpha analogue) --------
        //
        // Single-arg, body mirrors `Char.isAlpha` in elm-kernel-modules/Char.elm:
        //
        //   isAlpha char =
        //       let
        //           code = Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]
        //       in
        //       if Pine_kernel.int_is_sorted_asc [ 0x41, code, 0x5A ] then
        //           True
        //       else
        //           Pine_kernel.int_is_sorted_asc [ 0x61, code, 0x7A ]
        //
        // The `take [ 1, 0 ]` extracts the single sign byte from the
        // canonical encoding of `0` (positive sign byte 0x04, no magnitude),
        // and `concat` prepends it to the raw 1-byte char blob, producing a
        // valid positive-integer encoding (e.g. 'a' = blob 0x61 becomes the
        // integer encoding [0x04, 0x61] = +0x61 = 97). The two
        // `int_is_sorted_asc` calls then test 0x41 ≤ code ≤ 0x5A and
        // 0x61 ≤ code ≤ 0x7A.
        //
        // The body has no `let`-binding at the Pine level (lets are inlined
        // by the Elm-to-Pine lowering for non-shared subexpressions), so we
        // duplicate the `code` subexpression in both range checks.

        var charArg = Expression.EnvironmentInstance;

        var codeExpr =
            Expression.KernelApplicationInstance(
                function: nameof(KernelFunction.concat),
                input:
                Expression.ListInstance(
                    [
                    Expression.KernelApplicationInstance(
                        function: nameof(KernelFunction.take),
                        input:
                        Expression.ListInstance(
                            [
                            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                            Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(0)),
                            ])),
                    charArg,
                    ]));

        var upperRange =
            Expression.KernelApplicationInstance(
                function: nameof(KernelFunction.int_is_sorted_asc),
                input:
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(0x41)),
                    codeExpr,
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(0x5A)),
                    ]));

        var lowerRange =
            Expression.KernelApplicationInstance(
                function: nameof(KernelFunction.int_is_sorted_asc),
                input:
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(0x61)),
                    codeExpr,
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(0x7A)),
                    ]));

        // True/False in Elm-on-Pine are blob singletons 0x04 / 0x02.
        var trueLiteral =
            Expression.LiteralInstance(PineValue.Blob(new byte[] { 0x04 }));

        var predicateBody =
            Expression.ConditionalInstance(
                condition: upperRange,
                falseBranch: lowerRange,
                trueBranch: trueLiteral);

        var predicateBodyEncoded =
            ExpressionEncoding.EncodeExpressionAsValue(predicateBody);

        // -------- Recursive helper (skipWhileWithoutLinebreakHelp analogue) --------
        //
        // Env layout: [ self, srcBytes, offset ]
        //
        // The predicate is referenced as a LITERAL expression embedded in the
        // helper body — no env-class needed, no env[1] indirection. This
        // models the post-lambda-lifting / post-inlining shape where the
        // helper's `isGood` parameter has been substituted with the
        // `Char.isAlpha` literal at every occurrence in the body.
        //
        //   helperBody(env) =
        //       let
        //           nextChar = take [ 1, skip [ offset, srcBytes ] ]
        //       in
        //       if equal [ length nextChar, 0 ]
        //           then offset
        //       else if (ParseAndEval(
        //                  encoded     = literal(predicateBody),
        //                  environment = nextChar)) == True
        //           then ParseAndEval(
        //                  encoded     = env[0],
        //                  environment = [ env[0], env[1], offset + 1 ])
        //       else offset

        var selfRef = EnvironmentPathExpression([0]);
        var srcBytesRef = EnvironmentPathExpression([1]);
        var offsetRef = EnvironmentPathExpression([2]);

        var nextChar =
            Expression.KernelApplicationInstance(
                function: nameof(KernelFunction.take),
                input:
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                    Expression.KernelApplicationInstance(
                        function: nameof(KernelFunction.skip),
                        input:
                        Expression.ListInstance(
                            [
                            offsetRef,
                            srcBytesRef,
                            ])),
                    ]));

        var endOfInputCheck =
            Expression.KernelApplicationInstance(
                function: nameof(KernelFunction.equal),
                input:
                Expression.ListInstance(
                    [
                    Expression.KernelApplicationInstance(
                        function: nameof(KernelFunction.length),
                        input: nextChar),
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(0)),
                    ]));

        var predicateCall =
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(predicateBodyEncoded),
                environment: nextChar);

        var offsetPlusOne =
            Expression.KernelApplicationInstance(
                function: nameof(KernelFunction.int_add),
                input:
                Expression.ListInstance(
                    [
                    offsetRef,
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                    ]));

        var recursiveCall =
            new Expression.ParseAndEval(
                encoded: selfRef,
                environment:
                Expression.ListInstance(
                    [
                    selfRef,
                    srcBytesRef,
                    offsetPlusOne,
                    ]));

        var helperBody =
            Expression.ConditionalInstance(
                condition: endOfInputCheck,
                falseBranch:
                Expression.ConditionalInstance(
                    condition: predicateCall,
                    falseBranch: offsetRef,
                    trueBranch: recursiveCall),
                trueBranch: offsetRef);

        var helperBodyEncoded =
            ExpressionEncoding.EncodeExpressionAsValue(helperBody);

        // -------- Driver (root) --------
        //
        // Invokes helper(srcBytes = "abc", offset = 0). The per-iteration
        // predicate returns True for 'a','b','c'; at offset 3 nextChar is the
        // empty blob and the helper returns offset = 3.
        //
        //   rootExpression = ParseAndEval(
        //     encoded     = literal(helperBody),
        //     environment = [ literal(helperBody), "abc", 0 ])

        var srcBytesValue = PineValue.Blob(new byte[] { 0x61, 0x62, 0x63 }); // "abc"

        var rootExpression =
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(helperBodyEncoded),
                environment:
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(helperBodyEncoded),
                    Expression.LiteralInstance(srcBytesValue),
                    Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(0)),
                    ]));

        // -------- Run on a VM that captures every IR compilation --------

        var compiled = new List<ExpressionCompiled>();

        var vm =
            Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: null,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: false,
                selectPrecompiled: null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: false,
                parseCache: null,
                precompiledLeaves: null,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                reportExpressionCompiled:
                (in expressionCompiled) =>
                compiled.Add(expressionCompiled));

        var result = vm.EvaluateExpression(rootExpression, PineValue.EmptyBlob);

        // -------- Assertions (in order: return value first, then count, then snapshots) --------

        // 1. Return value: helper iterates 'a','b','c' (all alpha → True),
        //    then sees the empty nextChar at offset 3 and returns offset = 3.
        //    A miscompilation would surface here first.
        result.IsOkOrNull().Should().Be(IntegerEncoding.EncodeSignedInteger(3));

        // 2. With the predicate referenced as a literal directly in the
        //    helper body, the inliner has all the static information it needs
        //    to fold the predicate body into the helper's IR. The predicate
        //    body should NOT be compiled standalone — only the root and the
        //    helper get compiled.
        compiled.Select(item => item.Expression).Distinct().Should().HaveCount(2);
        compiled.Where(item => item.Expression.Equals(predicateBody)).Should().BeEmpty(
            "the small predicate body must be inlined into the helper frame, not compiled standalone");

        var rootCompiled =
            compiled.Single(item => item.Expression.Equals(rootExpression));
        var helperCompiled =
            compiled.Single(item => item.Expression.Equals(helperBody));

        // 3. Snapshot the rendered IR of each compiled expression. The
        //    helper-body snapshot is the primary signal: the predicate body
        //    appears INLINED as kernel ops directly in the helper frame
        //    (indices 6–17), with NO `Parse_And_Eval_Binary` for the
        //    predicate dispatch. Several subexpression-level reductions
        //    cooperate with the inlining:
        //
        //      - `take[1, skip[offset, srcBytes]]` → single fused
        //        `Slice_Skip_Var_Take_Const (1)` op (indices 0–2).
        //      - The `Char.isAlpha` sign-byte prepend
        //        (`concat[take[1, 0], char]`) is recognized by the reducer as
        //        equivalent to the raw byte being interpreted unsigned, so
        //        the two range checks lower to
        //        `Int_Unsigned_Greater_Than_Or_Equal_Const (65/97)` /
        //        `Int_Unsigned_Less_Than_Or_Equal_Const (90/122)` pairs
        //        (indices 7/9 and 13/15) — no concat, no take, no
        //        sign-byte literal in the IR.
        //
        //    The only `Parse_And_Eval_Binary` left in the helper IR is at
        //    index 28 — the recursive self-call (the inliner refuses to
        //    expand a self-recursive callee into its own body).

        var rootIR =
            StackInstructionTraceRenderer.RenderStackFrameInstructions(
                rootCompiled.Compilation.Generic);

        rootIR.Should().Be(
            """
            0: Push_Literal (List [3] (348))
            1: Push_Literal (List [2] (345))
            2: Parse_And_Eval_Binary
            3: Return
            """);

        var helperGenericIR =
            StackInstructionTraceRenderer.RenderStackFrameInstructions(
                helperCompiled.Compilation.Generic);

        helperGenericIR.Should().Be(
            """
             0: Local_Get (1)
             1: Local_Get (2)
             2: Slice_Skip_Var_Take_Const (1)
             3: Local_Set (3)
             4: Length
             5: Jump_If_Equal_Const (Blob [2] (0x0400 | int 0) , 25)
             6: Local_Get (3)
             7: Int_Unsigned_Greater_Than_Or_Equal_Const (65)
             8: Local_Get (3)
             9: Int_Unsigned_Less_Than_Or_Equal_Const (90)
            10: Logical_And_Binary
            11: Jump_If_Equal_Const (Blob [1] (0x04) , 7)
            12: Local_Get (3)
            13: Int_Unsigned_Greater_Than_Or_Equal_Const (97)
            14: Local_Get (3)
            15: Int_Unsigned_Less_Than_Or_Equal_Const (122)
            16: Logical_And_Binary
            17: Jump_Const (2)
            18: Push_Literal (Blob [1] (0x04))
            19: Jump_If_Equal_Const (Blob [1] (0x04) , 3)
            20: Local_Get (2)
            21: Return
            22: Local_Get (0)
            23: Local_Get (1)
            24: Local_Get (2)
            25: Int_Add_Const (1)
            26: Build_List (3)
            27: Local_Get (0)
            28: Parse_And_Eval_Binary
            29: Return
            30: Local_Get (2)
            31: Return
            """);
    }

    private static Expression EnvironmentPathExpression(
        System.ReadOnlySpan<int> path) =>
        ExpressionBuilder.BuildExpressionForPathInExpression(
            path,
            Expression.EnvironmentInstance);
}
