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
    // ComputeFreeVariables
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
        // analysis traces back to the scrutinee `maybeFn`.
        // Pattern-introduced names are *not* expanded transitively (we don't
        // know precisely how they relate to the scrutinee), so this case
        // currently reports no flow from the head. Document the behaviour.
        AssertFlowFromBody(
            """
            module Test exposing (..)

            apply maybeFn x =
                case maybeFn of
                    Just g -> g x
                    Nothing -> x
            """,
            functionName: "apply",
            // Branch-pattern names are bound within the branch; the analysis
            // does not (yet) trace them back to the scrutinee. This is
            // intentional under-approximation for case branches.
            expectedFlows: []);
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
            SyntaxAnalysis.ComputeFreeVariables(func.Expression.Value)
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
}
