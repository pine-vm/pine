using AwesomeAssertions;
using System;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.Avh4Format;

using static FormatTestHelper;

/// <summary>
/// Tests for the Avh4Format functionality used in the `elm-format` command.
/// </summary>
public class FormatTests
{
    [Fact]
    public void FormatToString_simple_module()
    {
        var input =
            """"
            module   Test       exposing (..)
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)
            """"
            .Trim());
    }

    [Fact]
    public void FormatToString_simple_declaration()
    {
        var input =
            """"
            module Test exposing (..)


            decl = 42
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl =
                42
            """"
            .Trim());
    }

    [Fact]
    public void FormatToString_aligns_function_application_arguments_multiline()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b c =
                a
                                  b
                  c
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl a b c =
                a
                    b
                    c
            """"
            .Trim());
    }

    [Fact]
    public void FormatToString_trims_whitespace_in_empty_list()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                [
                ]
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl =
                []
            """"
            .Trim());
    }

    [Fact]
    public void FormatToString_formats_if_then_else()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b c =
                if a < b then
                    c + 1            
                else
                    c - 1
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl a b c =
                if a < b then
                    c + 1

                else
                    c - 1
            """"
            .Trim());
    }

    [Fact]
    public void FormatToString_orders_imports()
    {
        var input =
            """"
            module Test exposing (..)

            import Set
            import Dict


            decl =
                1
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)

            import Dict
            import Set


            decl =
                1

            """"
            .Trim());
    }

    [Fact]
    public void FormatToString_formats_record_type_alias()
    {
        var input =
            """"
            module Test exposing (..)
            
            
            type alias MyRecord =
               {   field1  :    Int,   field2 :   String           }
            
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            type alias MyRecord =
                { field1 : Int, field2 : String }
            """"
            .Trim());
    }

    [Fact]
    public void FormatToString_formats_choice_type_declaration()
    {
        var input =
            """"
            module Test exposing (..)


            type Maybe a
               =   Nothing

                 | Just  a
            
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            type Maybe a
                = Nothing
                | Just a
            """"
            .Trim());
    }

    [Fact]
    public void FormatToString_file_ends_with_trailing_newline()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                42
            """";

        var formatted = FormatString(input);

        // Verify the formatted output ends with exactly one newline
        formatted.Should().EndWith("\n");

        // Verify it doesn't end with multiple newlines (just one)
        formatted.TrimEnd('\n').Should().NotEndWith("\n");
    }

    [Fact]
    public void FormatToString_preserves_comments()
    {
        var input =
            """"
            module Test exposing (..)


            -- Simple comment
            decl =
                71
                        
            """";

        var formatted = FormatString(input);

        // AVH4 format: 2 blank lines before first comment, 2 blank lines after non-doc comment
        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)

            -- Simple comment


            decl =
                71
            """"
            .Trim());
    }

    [Fact]
    public void FormatToString_stable_configurations_sample()
    {
        /*
         * Sample of module texts that are already formatted according to Avh4 style.
         * Therefore, formatting them again should yield the same text.
         * */

        IReadOnlyList<string> testCases =
            [
            """"
            module Test exposing (..)


            decl a b c =
                a b c

            """",

            """"
            module Test exposing (..)


            decl a b c =
                a
                    b
                    c
            """",

            """"
            module Test exposing (..)


            decl a b c =
                [ a, b, c ]

            """",

            """"
            module Test exposing (..)


            decl a b c =
                [ a
                , b
                , c
                ]
            """",
            ];

        for (var i = 0; i < testCases.Count; i++)
        {
            var testCase = testCases[i];

            try
            {
                var formatted = FormatString(testCase);

                formatted.Trim().Should().Be(testCase.Trim());
            }
            catch (Exception ex)
            {
                throw new Exception($"Failed in test case index: {i}", ex);
            }
        }
    }
}
