using AwesomeAssertions;
using Xunit;

using Location = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7.Location;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mConcretized.Avh4Format;

public class FormatIncompleteTests
{
    /*
     * Scenarios to test formatting of Elm modules containing some incomplete declarations.
     * */

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

        FormatTestHelper.AssertModuleTextFormatsToExpected(input, expected);
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

        FormatTestHelper.AssertModuleTextFormatsToExpected(input, expected);

        // Also verify that we have proper error information
        var parsed = FormatTestHelper.ParseFile(input);

        parsed.IncompleteDeclarations.Should().HaveCount(1);

        var incompleteDecl = parsed.IncompleteDeclarations[0];

        // The Range should point to the start of the incomplete declaration "decl =" at row 4
        incompleteDecl.Range.Start.Should().Be(new Location(4, 1));

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

        FormatTestHelper.AssertModuleTextFormatsToExpected(input, expected);

        // Also verify that we have proper error information
        var parsed = FormatTestHelper.ParseFile(input);

        parsed.IncompleteDeclarations.Should().HaveCount(1);

        var incompleteDecl = parsed.IncompleteDeclarations[0];

        // The Range should point to the start of the incomplete type alias declaration "type alias Point =" at row 9
        incompleteDecl.Range.Start.Should().Be(new Location(9, 1));

        incompleteDecl.Value.ErrorLocation.Should().Be(new Location(13, 5));

        // The ErrorMessage should describe the issue unexpected open bracket
        incompleteDecl.Value.ErrorMessage.Should().Contain("OpenBracket");
    }
}
