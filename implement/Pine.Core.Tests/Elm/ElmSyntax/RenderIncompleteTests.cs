using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.SyntaxModel;
using System;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax;

/// <summary>
/// Tests for rendering parsed Elm modules back to text, despite containing incomplete declarations.
/// </summary>
public class RenderIncompleteTests
{
    private static File ParseModule(string input)
    {
        return
            ElmSyntaxParser.ParseModuleText(input)
            .Extract(err => throw new Exception($"Parsing failed: {err}"));
    }

    private static void AssertRoundtrip(string input)
    {
        var parsed = ParseModule(input);

        var rendered = Rendering.ToString(parsed);

        rendered.Trim().Should().Be(input.Trim());
    }

    [Fact]
    public void Roundtrip_first_declaration_complete_second_incomplete()
    {
        var elmModuleText =
            """"       
            module App exposing (..)


            decl_a =
                42


            decl_b =

            """";

        AssertRoundtrip(elmModuleText);
    }

    [Fact]
    public void Roundtrip_middle_declaration_incomplete_list()
    {
        var elmModuleText =
            """"       
            module App exposing (..)


            decl_a =
                71


            decl_b =
                [ 73
                , 79
                , 81

            decl_c =
                91

            """";

        AssertRoundtrip(elmModuleText);
    }

    [Fact]
    public void Roundtrip_middle_declaration_incomplete_tuple()
    {
        var elmModuleText =
            """"       
            module App exposing (..)


            decl_a =
                71


            decl_b =
                ( 73
                , 79
                ,

            decl_c =
                91

            """";

        AssertRoundtrip(elmModuleText);
    }
}
