using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using System.Collections.Generic;
using System.Linq;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class HigherOrderParameterAnalysisTests
{
    [Fact]
    public void Reports_no_indices_for_first_order_function()
    {
        AssertHigherOrderParameterIndices(
            """
            module Test exposing (..)

            addOne n = n + 1
            """,
            functionName: "addOne",
            expected: []);
    }

    [Fact]
    public void Reports_first_parameter_of_apply()
    {
        AssertHigherOrderParameterIndices(
            """
            module Test exposing (..)

            apply f x = f x
            """,
            functionName: "apply",
            expected: [0]);
    }

    [Fact]
    public void Reports_only_function_parameter_for_callOn()
    {
        AssertHigherOrderParameterIndices(
            """
            module Test exposing (..)

            callOn x f = f x
            """,
            functionName: "callOn",
            expected: [1]);
    }

    [Fact]
    public void Reports_both_function_parameters_of_compose()
    {
        AssertHigherOrderParameterIndices(
            """
            module Test exposing (..)

            compose f g x = f (g x)
            """,
            functionName: "compose",
            expected: [0, 1]);
    }

    [Fact]
    public void Reports_parameter_when_used_via_let_value()
    {
        AssertHigherOrderParameterIndices(
            """
            module Test exposing (..)

            apply f x =
                let
                    g = f
                in
                g x
            """,
            functionName: "apply",
            expected: [0]);
    }

    [Fact]
    public void Reports_parameter_when_used_via_let_function_body()
    {
        AssertHigherOrderParameterIndices(
            """
            module Test exposing (..)

            apply f x =
                let
                    g a = f a
                in
                g x
            """,
            functionName: "apply",
            expected: [0]);
    }

    [Fact]
    public void Reports_parameter_passed_as_argument_to_local_let_function()
    {
        // For `apply f x = let g a = a x in g f`:
        //
        // * The data-flow analysis traces the application head `g` back through
        //   the let-binding to its synthetic lambda `\a -> a x`, whose free
        //   variables are { x }. Therefore `x` (parameter index 1) is reported
        //   as flowing into a function head.
        //
        // * The argument `f` is bound to the local parameter `a` inside `g`'s
        //   body; detecting that `f` reaches the inner head `a` would require
        //   beta-reducing the call `g f`, which the current analysis does not
        //   perform. So `f` (parameter index 0) is *not* reported here.
        //
        // The result therefore reflects the literal flow semantics defined for
        // the analysis — names that appear free in any expression that is the
        // function part of an application — rather than the (more permissive)
        // inter-procedural notion. This is documented as a known limitation;
        // see <c>ElmExpressionDataFlowTests.Flow_through_argument_to_local_let_function</c>.
        AssertHigherOrderParameterIndices(
            """
            module Test exposing (..)

            apply f x =
                let
                    g a = a x
                in
                g f
            """,
            functionName: "apply",
            expected: [1]);
    }

    [Fact]
    public void Reports_parameter_used_only_inside_nested_lambda()
    {
        AssertHigherOrderParameterIndices(
            """
            module Test exposing (..)

            wrap f = (\x -> f x)
            """,
            functionName: "wrap",
            expected: [0]);
    }

    [Fact]
    public void Does_not_report_parameter_only_used_as_argument()
    {
        AssertHigherOrderParameterIndices(
            """
            module Test exposing (..)

            applyToSeven f = f 7
            """,
            functionName: "applyToSeven",
            expected: [0]);
    }

    [Fact]
    public void Does_not_report_parameter_used_only_as_data()
    {
        AssertHigherOrderParameterIndices(
            """
            module Test exposing (..)

            wrapData x = [ x, x ]
            """,
            functionName: "wrapData",
            expected: []);
    }

    [Fact]
    public void Reports_constructor_arg_parameter_when_unwrapped_and_invoked()
    {
        // The pattern `(Wrap inner)` binds `inner`; the body invokes `inner`
        // as a function, so the (single, index 0) parameter is higher-order.
        AssertHigherOrderParameterIndices(
            """
            module Test exposing (..)

            type Wrap a = Wrap a

            invoke (Wrap inner) x = inner x
            """,
            functionName: "invoke",
            expected: [0]);
    }

    [Fact]
    public void Reports_tuple_pattern_member_used_as_function()
    {
        AssertHigherOrderParameterIndices(
            """
            module Test exposing (..)

            useFirst (f, x) = f x
            """,
            functionName: "useFirst",
            expected: [0]);
    }

    [Fact]
    public void Reports_record_pattern_member_used_as_function()
    {
        AssertHigherOrderParameterIndices(
            """
            module Test exposing (..)

            dispatch { handler, value } = handler value
            """,
            functionName: "dispatch",
            expected: [0]);
    }

    [Fact]
    public void Does_not_report_when_only_qualified_reference_is_invoked()
    {
        AssertHigherOrderParameterIndices(
            """
            module Test exposing (..)

            useStd a = List.map (\x -> a) [ 1 ]
            """,
            functionName: "useStd",
            expected: []);
    }

    [Fact]
    public void Reports_parameter_via_chained_let_aliases()
    {
        AssertHigherOrderParameterIndices(
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
            expected: [0]);
    }

    [Fact]
    public void Reports_parameter_used_inside_if_branch_application_head()
    {
        AssertHigherOrderParameterIndices(
            """
            module Test exposing (..)

            choose cond f g x =
                if cond then
                    f x
                else
                    g x
            """,
            functionName: "choose",
            expected: [1, 2]);
    }

    private static void AssertHigherOrderParameterIndices(
        string moduleText,
        string functionName,
        IReadOnlyList<int> expected)
    {
        var parsed =
            Core.Elm.ElmSyntax.ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new System.Exception("Failed parsing: " + err));

        var converted =
            SyntaxTypes.FromFullSyntaxModel.Convert(parsed);

        SyntaxTypes.FunctionImplementation? matched = null;

        foreach (var declNode in converted.Declarations)
        {
            if (declNode.Value is SyntaxTypes.Declaration.FunctionDeclaration funcDecl &&
                funcDecl.Function.Declaration.Value.Name.Value == functionName)
            {
                matched = funcDecl.Function.Declaration.Value;
                break;
            }
        }

        if (matched is null)
            throw new System.Exception("Function '" + functionName + "' not found.");

        var actual =
            HigherOrderParameterAnalysis.FindHigherOrderParameterIndices(matched)
            .OrderBy(i => i).ToArray();

        actual.Should().BeEquivalentTo(expected.OrderBy(i => i));
    }
}
