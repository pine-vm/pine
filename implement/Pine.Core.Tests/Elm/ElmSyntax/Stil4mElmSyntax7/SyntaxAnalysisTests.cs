using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mElmSyntax7;

public class SyntaxAnalysisTests
{
    // =========================================================================
    // CollectNamesBoundByPattern
    // =========================================================================

    [Fact]
    public void CollectNamesBoundByPattern_var_pattern()
    {
        AssertParameterBindings("foo a = a", expected: ["a"]);
    }

    [Fact]
    public void CollectNamesBoundByPattern_wildcard_binds_nothing()
    {
        AssertParameterBindings("foo _ = 0", expected: []);
    }

    [Fact]
    public void CollectNamesBoundByPattern_tuple_pattern()
    {
        AssertParameterBindings("foo (a, b) = a", expected: ["a", "b"]);
    }

    [Fact]
    public void CollectNamesBoundByPattern_constructor_pattern_args()
    {
        AssertParameterBindings("foo (Wrap inner) = inner", expected: ["inner"]);
    }

    [Fact]
    public void CollectNamesBoundByPattern_record_pattern_fields()
    {
        AssertParameterBindings("foo { x, y } = x", expected: ["x", "y"]);
    }

    [Fact]
    public void CollectNamesBoundByPattern_as_pattern()
    {
        AssertParameterBindings(
            "foo ((Wrap inner) as outer) = inner",
            expected: ["inner", "outer"]);
    }

    [Fact]
    public void CollectNamesBoundByPattern_uncons_pattern()
    {
        AssertParameterBindings("foo (h :: t) = h", expected: ["h", "t"]);
    }

    [Fact]
    public void CollectNamesBoundByPattern_list_pattern()
    {
        AssertParameterBindings("foo [ a, b ] = a", expected: ["a", "b"]);
    }

    [Fact]
    public void CollectNamesBoundByPattern_nested_tuple_constructor()
    {
        AssertParameterBindings(
            "foo (Wrap (Wrap (a, b))) = a",
            expected: ["a", "b"]);
    }

    // =========================================================================
    // CollectRemainingFreeVariables
    // =========================================================================

    [Fact]
    public void FreeVariables_of_function_body_includes_parameter_name()
    {
        AssertFreeVariablesOfBody(
            """
            module Test exposing (..)

            foo a = a
            """,
            functionName: "foo",
            expected: ["a"]);
    }

    [Fact]
    public void FreeVariables_excludes_qualified_module_references()
    {
        AssertFreeVariablesOfBody(
            """
            module Test exposing (..)

            foo a = Pine_kernel.int_add [ a, 1 ]
            """,
            functionName: "foo",
            expected: ["a"]);
    }

    [Fact]
    public void FreeVariables_through_let_value_includes_outer_free_only()
    {
        // In `foo a = let x = a in x`, the body's free variables are { a }
        // (x is bound by the let, a comes from the parameter and is free
        // when analysing the body in isolation).
        AssertFreeVariablesOfBody(
            """
            module Test exposing (..)

            foo a =
                let
                    x = a
                in
                x
            """,
            functionName: "foo",
            expected: ["a"]);
    }

    [Fact]
    public void FreeVariables_let_function_parameters_are_bound()
    {
        // The local let-function's parameters `p, q` are bound; `a` flows in
        // from outside the let; `foo` is free (recursive reference).
        AssertFreeVariablesOfBody(
            """
            module Test exposing (..)

            foo a =
                let
                    plus p q = p
                in
                plus a 1
            """,
            functionName: "foo",
            expected: ["a"]);
    }

    [Fact]
    public void FreeVariables_lambda_argument_is_bound_inside_lambda()
    {
        AssertFreeVariablesOfBody(
            """
            module Test exposing (..)

            foo a = (\x -> a)
            """,
            functionName: "foo",
            expected: ["a"]);
    }

    [Fact]
    public void FreeVariables_case_branch_pattern_binds_names()
    {
        // In each branch, names introduced by the pattern are bound;
        // outer parameter `xs` is free.
        AssertFreeVariablesOfBody(
            """
            module Test exposing (..)

            foo xs =
                case xs of
                    h :: t -> h
                    [] -> 0
            """,
            functionName: "foo",
            expected: ["xs"]);
    }

    [Fact]
    public void FreeVariables_record_update_records_name()
    {
        // Record name on the LHS of record update is a value reference.
        AssertFreeVariablesOfBody(
            """
            module Test exposing (..)

            foo r = { r | age = 1 }
            """,
            functionName: "foo",
            expected: ["r"]);
    }

    [Fact]
    public void FreeVariables_of_nested_lambdas_only_outer_free()
    {
        AssertFreeVariablesOfBody(
            """
            module Test exposing (..)

            foo a = (\x -> (\y -> a))
            """,
            functionName: "foo",
            expected: ["a"]);
    }

    // =========================================================================
    // CollectRemainingFreeVariables — rebinding / refreeing scenarios
    //
    // The walker reports the free variables of an expression independently of
    // the surrounding lexical context: an outer reference to a name `x` is
    // reported as free even when an inner sub-expression (case arm pattern,
    // lambda parameter, let binding) introduces `x` as a fresh local.
    // Surface Elm cannot express these shapes — they arise from intermediate
    // lowering stages that synthesize syntax and may reuse names.
    // =========================================================================

    [Fact]
    public void FreeVariables_outer_reference_remains_free_when_inner_case_arm_binds_same_name()
    {
        // The `Just x -> x` arm binds `x` locally, but the outer `x +` reference
        // is OUTSIDE the case-arm scope and must still be reported as free.
        AssertFreeVariablesOfBody(
            """
            module Test exposing (..)

            foo y =
                x + (case y of
                        Just x -> x
                        Nothing -> 0)
            """,
            functionName: "foo",
            expected: ["x", "y"]);
    }

    [Fact]
    public void FreeVariables_outer_reference_remains_free_when_inner_lambda_param_binds_same_name()
    {
        // The lambda `(\x -> x)` binds `x` for its own body, but the outer
        // `x +` reference is still free at the function-body level.
        AssertFreeVariablesOfBody(
            """
            module Test exposing (..)

            foo y =
                x + ((\x -> x) y)
            """,
            functionName: "foo",
            expected: ["x", "y"]);
    }

    [Fact]
    public void FreeVariables_outer_reference_remains_free_when_inner_let_function_binds_same_name()
    {
        // The inner `let x = y in x` binds `x` for its own body; the outer
        // `x +` reference must remain in the reported set.
        AssertFreeVariablesOfBody(
            """
            module Test exposing (..)

            foo y =
                x + (let
                        x = y
                     in
                        x)
            """,
            functionName: "foo",
            expected: ["x", "y"]);
    }

    [Fact]
    public void FreeVariables_outer_reference_remains_free_when_inner_let_destructure_binds_same_name()
    {
        // The let-destructuring `Just x = y` binds `x` for the body of the
        // let-block; the outer `x +` reference outside that block is still
        // free.
        AssertFreeVariablesOfBody(
            """
            module Test exposing (..)

            foo y =
                x + (let
                        (Just x) = y
                     in
                        x)
            """,
            functionName: "foo",
            expected: ["x", "y"]);
    }

    [Fact]
    public void FreeVariables_outer_reference_remains_free_when_nested_inner_binders_shadow_name()
    {
        // Nested binders (lambda inside a case arm both binding `x`) do not
        // affect the outer `x` reference.
        AssertFreeVariablesOfBody(
            """
            module Test exposing (..)

            foo y =
                x + (case y of
                        Just z -> (\x -> x) z
                        Nothing -> 0)
            """,
            functionName: "foo",
            expected: ["x", "y"]);
    }

    // =========================================================================
    // ComputeNamesFlowingIntoApplicationFunctions
    // =========================================================================

    [Fact]
    public void Flow_direct_parameter_used_as_application_head()
    {
        AssertFlowFromBody(
            """
            module Test exposing (..)

            apply f x = f x
            """,
            functionName: "apply",
            expectedFlows: ["f"]);
    }

    [Fact]
    public void Flow_parameter_not_used_as_function()
    {
        AssertFlowFromBody(
            """
            module Test exposing (..)

            second a b = b
            """,
            functionName: "second",
            expectedFlows: []);
    }

    [Fact]
    public void Flow_parameter_only_appears_as_argument_not_function()
    {
        // `g` is called with `x` — `g` flows into a function head, `x` does not.
        AssertFlowFromBody(
            """
            module Test exposing (..)

            callOn g x = g x
            """,
            functionName: "callOn",
            expectedFlows: ["g"]);
    }

    [Fact]
    public void Flow_via_local_let_binding()
    {
        // `let g = f in g x` — the head `g` flows through the let binding
        // to `f`, which is then reported.
        AssertFlowFromBody(
            """
            module Test exposing (..)

            apply f x =
                let
                    g = f
                in
                g x
            """,
            functionName: "apply",
            expectedFlows: ["f"]);
    }

    [Fact]
    public void Flow_via_local_let_function()
    {
        // `let g a = f a in g x` — `g` is invoked, its body references `f`
        // which is then reported through the synthetic-lambda expansion.
        AssertFlowFromBody(
            """
            module Test exposing (..)

            apply f x =
                let
                    g a = f a
                in
                g x
            """,
            functionName: "apply",
            expectedFlows: ["f"]);
    }

    [Fact]
    public void Flow_via_let_destructuring_constructor()
    {
        // `let (Wrap inner) = f in inner x` — the head `inner` flows through
        // the destructuring to `f` (over-approximated to the whole RHS).
        AssertFlowFromBody(
            """
            module Test exposing (..)

            apply f x =
                let
                    (Wrap inner) = f
                in
                inner x
            """,
            functionName: "apply",
            expectedFlows: ["f"]);
    }

    [Fact]
    public void Flow_via_case_branch_pattern()
    {
        // In branch `Just g -> g x`, head `g` is bound by the pattern; the
        // analysis traces back to the scrutinee `maybeFn` by mapping each
        // branch-pattern binding to the scrutinee expression in the local
        // let-RHS scope (same over-approximation as let-destructuring).
        AssertFlowFromBody(
            """
            module Test exposing (..)

            apply maybeFn x =
                case maybeFn of
                    Just g -> g x
                    Nothing -> x
            """,
            functionName: "apply",
            // Branch-pattern names carry information from the scrutinee, so
            // a later use of one as the head of an Application flows back
            // to the scrutinee's free variables (`maybeFn`).
            expectedFlows: ["maybeFn"]);
    }

    [Fact]
    public void Flow_nested_lambda_inside_body()
    {
        // The inner lambda invokes `f` as a function — the outer parameter `f`
        // flows in.
        AssertFlowFromBody(
            """
            module Test exposing (..)

            wrap f = (\x -> f x)
            """,
            functionName: "wrap",
            expectedFlows: ["f"]);
    }

    [Fact]
    public void Flow_lambda_parameter_does_not_leak_out()
    {
        // The lambda's parameter `x` is bound within the lambda; using `x` as
        // a function head inside the lambda contributes only the lambda's
        // own bound name, not an outer-scope name.
        AssertFlowFromBody(
            """
            module Test exposing (..)

            inLambda y = (\x -> x y)
            """,
            functionName: "inLambda",
            expectedFlows: []);
    }

    [Fact]
    public void Flow_through_argument_to_local_let_function()
    {
        // `g a = a x` makes its local parameter `a` the head of an Application,
        // but `a` is bound by `g`'s own parameter list (modeled here as a
        // synthetic lambda parameter); the outer `x` is the only free name
        // flowing into that head from the let-function's body.
        //
        // The outer parameter `f` is the *argument* to `g` at the call site
        // `g f`. Detecting that `f` flows into the head requires beta-reducing
        // the call (substituting `f` for `a` in `g`'s body). The current
        // analysis does not perform that substitution and so reports only `x`.
        // The data-flow result is therefore a sound but incomplete
        // approximation; the higher-order-parameter analysis still reports
        // `f` as higher-order by other means (see
        // <c>HigherOrderParameterAnalysisTests.Reports_parameter_passed_as_argument_to_local_let_function</c>).
        AssertFlowFromBody(
            """
            module Test exposing (..)

            apply f x =
                let
                    g a = a x
                in
                g f
            """,
            functionName: "apply",
            expectedFlows: ["x"]);
    }

    [Fact]
    public void Flow_chained_lets_transitive()
    {
        AssertFlowFromBody(
            """
            module Test exposing (..)

            apply f x =
                let
                    g = f
                    h = g
                in
                h x
            """,
            functionName: "apply",
            expectedFlows: ["f"]);
    }

    [Fact]
    public void Flow_does_not_infinite_loop_on_mutually_recursive_lets()
    {
        // `g` and `h` reference each other via the let; the cycle guard
        // prevents infinite recursion.
        AssertFlowFromBody(
            """
            module Test exposing (..)

            apply f x =
                let
                    g a = h a
                    h a = g a
                in
                g x
            """,
            functionName: "apply",
            // Neither g nor h reference an outer name as a function — only
            // each other. Both are bound by the let. No outer flow reported.
            expectedFlows: []);
    }

    [Fact]
    public void Flow_record_access_chain_does_not_extract_root()
    {
        // `r.f x` invokes `r.f` as a function. `r` flows in via FunctionOrValue
        // reference inside the RecordAccess.
        AssertFlowFromBody(
            """
            module Test exposing (..)

            dispatch r x = r.handler x
            """,
            functionName: "dispatch",
            expectedFlows: ["r"]);
    }

    [Fact]
    public void Flow_qualified_reference_in_head_excluded()
    {
        // A qualified reference like `List.map` is not a free variable in
        // the expression-local sense; it's not reported.
        AssertFlowFromBody(
            """
            module Test exposing (..)

            useStd a = List.map (\x -> a) [ 1 ]
            """,
            functionName: "useStd",
            // `a` only appears as an argument to the inner lambda's body and
            // is never the head of an Application.
            expectedFlows: []);
    }

    // =========================================================================
    // Helpers
    // =========================================================================

    private static void AssertParameterBindings(
        string oneLineDecl,
        IReadOnlyList<string> expected)
    {
        var moduleText = "module Test exposing (..)\n\n\n" + oneLineDecl + "\n";
        var func = ParseFirstFunction(moduleText);
        var pattern = func.Arguments[0].Value;

        var actual =
            SyntaxAnalysis.CollectNamesBoundByPattern(pattern)
            .OrderBy(s => s).ToImmutableArray();

        actual.Should().BeEquivalentTo(expected.OrderBy(s => s));
    }

    private static void AssertFreeVariablesOfBody(
        string moduleText,
        string functionName,
        IReadOnlyList<string> expected)
    {
        var func = ParseFunctionByName(moduleText, functionName);

        var actual =
            SyntaxAnalysis.CollectRemainingFreeVariables(func.Expression.Value)
            .OrderBy(s => s).ToImmutableArray();

        actual.Should().BeEquivalentTo(expected.OrderBy(s => s));
    }

    private static void AssertFlowFromBody(
        string moduleText,
        string functionName,
        IReadOnlyList<string> expectedFlows)
    {
        var func = ParseFunctionByName(moduleText, functionName);

        var actual =
            SyntaxAnalysis.ComputeNamesFlowingIntoApplicationFunctions(
                func.Expression.Value)
            .OrderBy(s => s).ToImmutableArray();

        actual.Should().BeEquivalentTo(expectedFlows.OrderBy(s => s));
    }

    private static FunctionImplementation ParseFirstFunction(string moduleText)
    {
        var parsed =
            Core.Elm.ElmSyntax.ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new System.Exception("Failed parsing: " + err));

        var converted =
            FromFullSyntaxModel.Convert(parsed);

        foreach (var declNode in converted.Declarations)
        {
            if (declNode.Value is Declaration.FunctionDeclaration funcDecl)
                return funcDecl.Function.Declaration.Value;
        }

        throw new System.Exception("No function declaration found in module.");
    }

    private static FunctionImplementation ParseFunctionByName(
        string moduleText, string functionName)
    {
        var parsed =
            Core.Elm.ElmSyntax.ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new System.Exception("Failed parsing: " + err));

        var converted =
            FromFullSyntaxModel.Convert(parsed);

        foreach (var declNode in converted.Declarations)
        {
            if (declNode.Value is Declaration.FunctionDeclaration funcDecl &&
                funcDecl.Function.Declaration.Value.Name.Value == functionName)
            {
                return funcDecl.Function.Declaration.Value;
            }
        }

        throw new System.Exception("Function '" + functionName + "' not found.");
    }

    // =========================================================================
    // TryPredictOutermostConstructorTag
    // =========================================================================

    [Fact]
    public void TryPredictOutermostConstructorTag_bare_uppercase_FunctionOrValue()
    {
        AssertOutermostConstructorTagOfBody(
            """
            module Test exposing (..)


            foo = Nothing
            """,
            functionName: "foo",
            expectedTagFullName: "Nothing");
    }

    [Fact]
    public void TryPredictOutermostConstructorTag_qualified_constructor()
    {
        AssertOutermostConstructorTagOfBody(
            """
            module Test exposing (..)


            foo = Maybe.Nothing
            """,
            functionName: "foo",
            expectedTagFullName: "Maybe.Nothing");
    }

    [Fact]
    public void TryPredictOutermostConstructorTag_applied_constructor()
    {
        AssertOutermostConstructorTagOfBody(
            """
            module Test exposing (..)


            foo x = Maybe.Just x
            """,
            functionName: "foo",
            expectedTagFullName: "Maybe.Just");
    }

    [Fact]
    public void TryPredictOutermostConstructorTag_through_let_wrapper()
    {
        AssertOutermostConstructorTagOfBody(
            """
            module Test exposing (..)


            foo x =
                let
                    y = x
                in
                Maybe.Just y
            """,
            functionName: "foo",
            expectedTagFullName: "Maybe.Just");
    }

    [Fact]
    public void TryPredictOutermostConstructorTag_through_paren_wrapper()
    {
        AssertOutermostConstructorTagOfBody(
            """
            module Test exposing (..)


            foo x = (Maybe.Just x)
            """,
            functionName: "foo",
            expectedTagFullName: "Maybe.Just");
    }

    [Fact]
    public void TryPredictOutermostConstructorTag_lowercase_call_returns_null()
    {
        AssertOutermostConstructorTagOfBody(
            """
            module Test exposing (..)


            foo x = identity x
            """,
            functionName: "foo",
            expectedTagFullName: null);
    }

    [Fact]
    public void TryPredictOutermostConstructorTag_kernel_call_returns_null()
    {
        AssertOutermostConstructorTagOfBody(
            """
            module Test exposing (..)


            foo a = Pine_kernel.int_add [ a, 1 ]
            """,
            functionName: "foo",
            expectedTagFullName: null);
    }

    [Fact]
    public void TryPredictOutermostConstructorTag_lambda_returns_null()
    {
        AssertOutermostConstructorTagOfBody(
            """
            module Test exposing (..)


            foo = \x -> x
            """,
            functionName: "foo",
            expectedTagFullName: null);
    }

    [Fact]
    public void TryPredictOutermostConstructorTag_record_literal_returns_null()
    {
        AssertOutermostConstructorTagOfBody(
            """
            module Test exposing (..)


            foo = { a = 1 }
            """,
            functionName: "foo",
            expectedTagFullName: null);
    }

    [Fact]
    public void TryPredictOutermostConstructorTag_if_both_branches_same_tag()
    {
        AssertOutermostConstructorTagOfBody(
            """
            module Test exposing (..)


            foo b x y = if b then Maybe.Just x else Maybe.Just y
            """,
            functionName: "foo",
            expectedTagFullName: "Maybe.Just");
    }

    [Fact]
    public void TryPredictOutermostConstructorTag_if_mixed_branches_returns_null()
    {
        AssertOutermostConstructorTagOfBody(
            """
            module Test exposing (..)


            foo b x = if b then Maybe.Just x else Maybe.Nothing
            """,
            functionName: "foo",
            expectedTagFullName: null);
    }

    // =========================================================================
    // EnumerateConstructorTaggedLeavesOfIfChain
    // =========================================================================

    [Fact]
    public void EnumerateConstructorTaggedLeavesOfIfChain_two_branch_if()
    {
        AssertIfChainLeaves(
            """
            module Test exposing (..)


            foo b x = if b then Maybe.Just x else Maybe.Nothing
            """,
            functionName: "foo",
            expectedTagFullNames: ["Maybe.Just", "Maybe.Nothing"]);
    }

    [Fact]
    public void EnumerateConstructorTaggedLeavesOfIfChain_else_if_chain()
    {
        AssertIfChainLeaves(
            """
            module Test exposing (..)


            foo b1 b2 x =
                if b1 then
                    Maybe.Just x

                else if b2 then
                    Maybe.Just x

                else
                    Maybe.Nothing
            """,
            functionName: "foo",
            expectedTagFullNames: ["Maybe.Just", "Maybe.Just", "Maybe.Nothing"]);
    }

    [Fact]
    public void EnumerateConstructorTaggedLeavesOfIfChain_through_let_root()
    {
        AssertIfChainLeaves(
            """
            module Test exposing (..)


            foo b x =
                let
                    z = x
                in
                if b then Maybe.Just z else Maybe.Nothing
            """,
            functionName: "foo",
            expectedTagFullNames: ["Maybe.Just", "Maybe.Nothing"]);
    }

    [Fact]
    public void EnumerateConstructorTaggedLeavesOfIfChain_non_if_returns_null()
    {
        var func =
            ParseFunctionByName(
                """
                module Test exposing (..)


                foo = Maybe.Just 1
                """,
                "foo");

        var leaves =
            SyntaxAnalysis.EnumerateConstructorTaggedLeavesOfIfChain(func.Expression.Value);

        leaves.Should().BeNull();
    }

    [Fact]
    public void EnumerateConstructorTaggedLeavesOfIfChain_mismatched_leaf_returns_null()
    {
        var func =
            ParseFunctionByName(
                """
                module Test exposing (..)


                foo b x = if b then Maybe.Just x else identity x
                """,
                "foo");

        var leaves =
            SyntaxAnalysis.EnumerateConstructorTaggedLeavesOfIfChain(func.Expression.Value);

        leaves.Should().BeNull();
    }

    // =========================================================================
    // TryPredictOutermostConstructorTagInArmBody
    // =========================================================================

    [Fact]
    public void TryPredictOutermostConstructorTagInArmBody_direct_constructor_body()
    {
        AssertArmBodyTag(
            armPatternText: "Bad x",
            armBodyText: "Bad x",
            expectedTagFullName: "Bad");
    }

    [Fact]
    public void TryPredictOutermostConstructorTagInArmBody_as_alias_body_with_named_inner()
    {
        AssertArmBodyTag(
            armPatternText: "(Good a b) as good",
            armBodyText: "good",
            expectedTagFullName: "Good");
    }

    [Fact]
    public void TryPredictOutermostConstructorTagInArmBody_as_alias_body_mismatched_name_returns_null()
    {
        AssertArmBodyTag(
            armPatternText: "(Good a b) as good",
            armBodyText: "other",
            expectedTagFullName: null);
    }

    [Fact]
    public void TryPredictOutermostConstructorTagInArmBody_var_pattern_returns_null()
    {
        AssertArmBodyTag(
            armPatternText: "x",
            armBodyText: "x",
            expectedTagFullName: null);
    }

    [Fact]
    public void TryPredictOutermostConstructorTagInArmBody_qualified_constructor_body()
    {
        AssertArmBodyTag(
            armPatternText: "Bad x",
            armBodyText: "ParserFast.Bad Basics.True x",
            expectedTagFullName: "ParserFast.Bad");
    }

    // =========================================================================
    // Helpers for tag prediction tests
    // =========================================================================

    private static void AssertOutermostConstructorTagOfBody(
        string moduleText,
        string functionName,
        string? expectedTagFullName)
    {
        var func = ParseFunctionByName(moduleText, functionName);

        var actual = SyntaxAnalysis.TryPredictOutermostConstructorTag(func.Expression.Value);

        if (expectedTagFullName is null)
        {
            actual.Should().BeNull();
        }
        else
        {
            actual.Should().NotBeNull();
            FormatQualifiedNameRef(actual!).Should().Be(expectedTagFullName);
        }
    }

    private static void AssertIfChainLeaves(
        string moduleText,
        string functionName,
        IReadOnlyList<string> expectedTagFullNames)
    {
        var func = ParseFunctionByName(moduleText, functionName);

        var leaves =
            SyntaxAnalysis.EnumerateConstructorTaggedLeavesOfIfChain(func.Expression.Value);

        leaves.Should().NotBeNull();

        leaves!.Select(l => FormatQualifiedNameRef(l.PredictedTag))
            .Should().Equal(expectedTagFullNames);
    }

    private static void AssertArmBodyTag(
        string armPatternText,
        string armBodyText,
        string? expectedTagFullName)
    {
        var moduleText =
            "module Test exposing (..)\n\n\n" +
            "foo scrutinee =\n" +
            "    case scrutinee of\n" +
            "        " + armPatternText + " ->\n" +
            "            " + armBodyText + "\n";

        var func = ParseFunctionByName(moduleText, "foo");

        var body = func.Expression.Value;

        // Unwrap parens around the case body.
        while (body is Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.ParenthesizedExpression paren)
        {
            body = paren.Expression.Value;
        }

        var caseExpr = (Core.Elm.ElmSyntax.Stil4mElmSyntax7.Expression.CaseExpression)body;
        var firstArm = caseExpr.CaseBlock.Cases[0];

        var actual =
            SyntaxAnalysis.TryPredictOutermostConstructorTagInArmBody(
                firstArm.Pattern.Value,
                firstArm.Expression.Value);

        if (expectedTagFullName is null)
        {
            actual.Should().BeNull();
        }
        else
        {
            actual.Should().NotBeNull();
            FormatQualifiedNameRef(actual!).Should().Be(expectedTagFullName);
        }
    }

    private static string FormatQualifiedNameRef(QualifiedNameRef name)
    {
        return
            name.ModuleName.Count is 0
            ?
            name.Name
            :
            string.Join(".", name.ModuleName) + "." + name.Name;
    }
}
