using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.Stil4mConcretized;
using System;
using Xunit;

using Location = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.Location;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mConcretized.Avh4Format;

using Avh4Format = Core.Elm.ElmSyntax.Stil4mConcretized.Avh4Format;

public class FormatIncompleteTests
{
    /*
     * Scenarios to test formatting of Elm modules containing some incomplete declarations.
     * */

    private static string FormatString(
        string input)
    {
        var parsed =
            ElmSyntaxParser.ParseModuleText(
                input,
                enableMaxPreservation: true)
            .Extract(err => throw new Exception($"Parsing failed: {err}"));

        return Avh4Format.FormatToString(parsed);
    }

    private static File ParseFile(string input)
    {
        return ElmSyntaxParser.ParseModuleText(
            input,
            enableMaxPreservation: true)
            .Extract(err => throw new Exception($"Parsing failed: {err}"));
    }

    [Fact]
    public void Simple_module()
    {
        var input =
            """"
            module   Test       exposing (..)


            decl =
            """";

        var expected =
            """"
            module Test exposing (..)


            decl =
            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Incomplete_list_declaration()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                [ 17
                ,


            decl = 42
            """";

        var expected =
            """"
            module Test exposing (..)


            decl =
                [ 17
                ,


            decl =
                42
            """";

        AssertModuleTextFormatsToExpected(input, expected);

        // Also verify that we have proper error information
        var parsed = ParseFile(input);

        parsed.IncompleteDeclarations.Should().HaveCount(1);

        var incompleteDecl = parsed.IncompleteDeclarations[0];

        // The Range should point to the start of the incomplete declaration "decl =" at row 4
        incompleteDecl.Range.Start.Row.Should().Be(4);
        incompleteDecl.Range.Start.Column.Should().Be(1);

        // The ErrorLocation should point to where the actual error occurred (somewhere after the comma)
        incompleteDecl.Value.ErrorLocation.Row.Should().BeGreaterThanOrEqualTo(6);

        // The ErrorMessage should be non-empty
        incompleteDecl.Value.ErrorMessage.Should().NotBeEmpty();
    }

    [Fact]
    public void Format_declarations_around_incomplete_type_alias_declaration_record_in_the_middle()
    {
        var input =
            """"
            module Test exposing (..)


            aList =
                [ 31, 41
                , 59 ]


            type alias Point =
                { x : Float
                , y : Float
                , z :
                [


            anotherList = [ 81,83
            , 87 ]

            """";

        var expected =
            """"
            module Test exposing (..)


            aList =
                [ 31
                , 41
                , 59
                ]


            type alias Point =
                { x : Float
                , y : Float
                , z :
                [


            anotherList =
                [ 81
                , 83
                , 87
                ]

            """";

        AssertModuleTextFormatsToExpected(input, expected);

        // Also verify that we have proper error information
        var parsed = ParseFile(input);

        parsed.IncompleteDeclarations.Should().HaveCount(1);

        var incompleteDecl = parsed.IncompleteDeclarations[0];

        // The Range should point to the start of the incomplete type alias declaration "type alias Point =" at row 9
        incompleteDecl.Range.Start.Should().Be(new Location(9, 1));

        incompleteDecl.Value.ErrorLocation.Should().Be(new Location(13, 5));

        // The ErrorMessage should describe the issue unexpected open bracket
        incompleteDecl.Value.ErrorMessage.Should().Contain("OpenBracket");
    }

    private static void AssertModuleTextFormatsToItself(
        string elmModuleText)
    {
        var formatted = FormatString(elmModuleText);

        formatted.Trim().Should().Be(elmModuleText.Trim());
    }

    private static void AssertModuleTextFormatsToExpected(
        string elmModuleText,
        string expectedFormattedElmModuleText)
    {
        var formatted = FormatString(elmModuleText);

        formatted.Trim().Should().Be(expectedFormattedElmModuleText.Trim());
    }

    private static void AssertSyntaxNodesValueEqualityForModuleText(
        string elmModuleText)
    {
        /*
         * Verify the C# type declarations implement value-based equality and
         * hash code generation correctly, by parsing the same module text twice
         * and comparing the resulting syntax nodes.
         * */

        var parsed_0 =
            ElmSyntaxParser.ParseModuleText(elmModuleText, enableMaxPreservation: true)
            .Extract(err => throw new Exception("Parsing failed: " + err.ToString()));

        var parsed_1 =
            ElmSyntaxParser.ParseModuleText(elmModuleText, enableMaxPreservation: true)
            .Extract(err => throw new Exception("Reparsing failed: " + err.ToString()));

        parsed_0.Should().Be(parsed_1);

        parsed_0.GetHashCode().Should().Be(parsed_1.GetHashCode());
    }
}
