using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.Avh4Format;

using static FormatTestHelper;

/// <summary>
/// Regression tests covering an Avh4Format defect where an <c>if</c> (or
/// <c>else if</c>) whose condition is an intrinsically multi-line expression
/// (a <c>let ... in ...</c>, <c>case ... of</c>, or nested <c>if ... then ... else ...</c>)
/// was emitted as if the condition were single-line. That produced output where
/// the <c>if</c>/<c>else&#160;if</c> keyword sat on the same line as the opening
/// <c>let</c>/<c>case</c>/<c>if</c> token of the condition while the body of
/// the condition rendered across additional lines, leaving <c>then</c> dangling
/// at the end of the bracketed condition body — a layout that the
/// <c>avh4 elm-format</c> binary never produces.
///
/// <para>The avh4 elm-format binary instead always places the <c>if</c> keyword
/// on its own line whenever the condition itself spans multiple lines, with
/// <c>then</c> on its own line at the base indent.</para>
/// </summary>
public class IfWithLetConditionFormatTests
{
    [Fact]
    public void If_with_let_condition_compact_source_formats_to_multiline_layout()
    {
        // Source uses a one-line `if let ... in ... then ...` form; avh4 elm-format
        // expands the if to its multi-line layout because the let condition is
        // intrinsically multi-line.
        var input =
            """"
            module Test exposing (..)


            decl a b =
                if let x = 1 in x then a else b
            """";

        AssertModuleTextFormatsToExpected(
            input,
            """"
            module Test exposing (..)


            decl a b =
                if
                    let
                        x =
                            1
                    in
                    x
                then
                    a

                else
                    b
            """");
    }

    [Fact]
    public void Else_if_with_let_condition_compact_source_formats_to_multiline_layout()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b c =
                if a then
                    1
                else if let x = 1 in x then
                    2
                else
                    3
            """";

        AssertModuleTextFormatsToExpected(
            input,
            """"
            module Test exposing (..)


            decl a b c =
                if a then
                    1

                else if
                    let
                        x =
                            1
                    in
                    x
                then
                    2

                else
                    3
            """");
    }

    [Fact]
    public void If_with_case_condition_compact_source_formats_to_multiline_layout()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b =
                if case a of
                    True -> True
                    False -> False
                then
                    a
                else
                    b
            """";

        AssertModuleTextFormatsToExpected(
            input,
            """"
            module Test exposing (..)


            decl a b =
                if
                    case a of
                        True ->
                            True

                        False ->
                            False
                then
                    a

                else
                    b
            """");
    }
}
