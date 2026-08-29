using AwesomeAssertions;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.Avh4Format;

using static FormatTestHelper;

public class OperatorPrecedenceMultilineTests
{
    [Fact]
    public void Multiline_boolean_chain_keeps_higher_precedence_comparisons_inline()
    {
        var input =
            """"
            module Test exposing (..)


            view relevantItems config =
                if
                    relevantItems == []
                        && config.notFoundStyles == []
                        && config.notFoundStyles == []
                        && config.notFoundStyles == []
                then
                    style config.notFoundHidden

                else
                    style (viewStyles config)
            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Line_break_in_left_associative_chain_makes_same_precedence_level_multiline()
    {
        var input =
            """"
            module Test exposing (..)


            value =
                alfa + beta
                    - gamma + delta
            """";

        var expected =
            """"
            module Test exposing (..)


            value =
                alfa
                    + beta
                    - gamma
                    + delta
            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Line_break_in_right_associative_chain_makes_same_precedence_level_multiline()
    {
        var input =
            """"
            module Test exposing (..)


            value =
                alfa ++ beta
                    ++ gamma ++ delta
            """";

        var expected =
            """"
            module Test exposing (..)


            value =
                alfa
                    ++ beta
                    ++ gamma
                    ++ delta
            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Same_precedence_multiplicative_operators_share_multiline_layout()
    {
        var input =
            """"
            module Test exposing (..)


            value =
                alfa * beta
                    / gamma // delta
            """";

        var expected =
            """"
            module Test exposing (..)


            value =
                alfa
                    * beta
                    / gamma
                    // delta
            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Multiline_addition_chain_keeps_multiplication_and_division_inline()
    {
        var input =
            """"
            module Test exposing (..)


            value =
                width * height + depth * length
                    - margin / scale
            """";

        var expected =
            """"
            module Test exposing (..)


            value =
                width * height
                    + depth * length
                    - margin / scale
            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Explicit_line_breaks_at_multiple_precedence_levels_remain_multiline()
    {
        var input =
            """"
            module Test exposing (..)


            value =
                alfa
                    + beta
                    * gamma
                    * delta
            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Single_line_mixed_precedence_expression_remains_single_line()
    {
        var input =
            """"
            module Test exposing (..)


            value =
                alfa == beta && gamma == delta || epsilon == zeta
            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Parentheses_bound_same_precedence_multiline_propagation()
    {
        var input =
            """"
            module Test exposing (..)


            value =
                outside + (inside
                    + sibling
                )
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            value =
                outside
                    + (inside
                        + sibling
                      )

            """".Trim());

        AssertModuleTextFormatsToItself(formatted);
    }

    [Fact]
    public void Commented_line_break_makes_same_precedence_level_multiline()
    {
        var input =
            """"
            module Test exposing (..)


            value =
                alfa + beta
                    -- Keep this operator explanation.
                    + gamma + delta
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            value =
                alfa
                    + beta
                    -- Keep this operator explanation.
                    + gamma
                    + delta
            
            """".Trim());

        AssertModuleTextFormatsToItself(formatted);
    }

    [Fact]
    public void Left_pipe_does_not_leak_alignment_into_multiline_right_operand()
    {
        var input =
            """"
            module Test exposing (..)


            value =
                apply <|
                    (let
                        first =
                            True
                     in
                     first
                    )
                        || (let
                                second =
                                    False
                            in
                            second
                           )
            """";

        AssertModuleTextFormatsToItself(input);
    }
}
