using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.Stil4mConcretized;
using System;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mConcretized;

/// <summary>
/// Tests for the Rendering class in the Stil4mConcretized namespace.
/// </summary>
public class RenderingCompleteTests
{
    private static File ParseModule(string input)
    {
        return ElmSyntaxParser.ParseModuleText(input, enableMaxPreservation: true)
            .Extract(err => throw new Exception($"Parsing failed: {err}"));
    }

    private static void AssertRoundtrip(string input)
    {
        var parsed = ParseModule(input);

        var rendered = Rendering.ToString(parsed);

        rendered.Trim().Should().Be(input.Trim());
    }

    [Fact]
    public void Roundtrip_simple_module()
    {
        var input =
            """"
            module Test exposing (..)
            """";

        AssertRoundtrip(input);
    }

    [Fact]
    public void Roundtrip_module_with_declaration()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                42
            """";

        AssertRoundtrip(input);
    }

    [Fact]
    public void Roundtrip_module_with_import()
    {
        var input =
            """"
            module Test exposing (..)

            import Dict


            decl =
                1
            """";

        AssertRoundtrip(input);
    }

    [Fact]
    public void Roundtrip_module_with_import_not_formatted()
    {
        var input =
            """"
            module Test exposing (..)

            import   Dict  exposing  (  set  , get  )


            decl =
                1
            """";

        AssertRoundtrip(input);
    }

    [Fact]
    public void Roundtrip_list_expression()
    {
        var input =
            """"
            module Test exposing (..)
            
            
            decl =
                [ 1, 2, 3 ]
            """";

        AssertRoundtrip(input);
    }

    [Fact]
    public void Roundtrip_list_expression_not_formatted()
    {
        var input =
            """"
            module Test exposing (..)
            
            
            decl =
                 [ 1  ,   2,
                3 ]

            """";

        AssertRoundtrip(input);
    }

    [Fact]
    public void Roundtrip_type_alias()
    {
        var input =
            """"
            module Test exposing (..)


            type alias MyRecord =
                { field1 : Int, field2 : String }
            """";

        AssertRoundtrip(input);
    }

    [Fact]
    public void Roundtrip_custom_type()
    {
        var input =
            """"
            module Test exposing (..)


            type Maybe a
                = Nothing
                | Just a
            """";

        AssertRoundtrip(input);
    }

    [Fact]
    public void Roundtrip_if_expression()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b =
                if a < b then
                    a

                else
                    b
            """";

        AssertRoundtrip(input);
    }

    [Fact]
    public void Roundtrip_import_with_alias()
    {
        var input =
            """"
            module Test exposing (..)

            import Dict as D


            decl =
                1
            """";

        AssertRoundtrip(input);
    }

    [Fact]
    public void Roundtrip_lambda_expression()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                \x -> x + 1
            """";

        AssertRoundtrip(input);
    }

    [Fact]
    public void Roundtrip_case_expression()
    {
        var input =
            """"
            module Test exposing (..)


            decl a =
                case a of
                    Just x ->
                        x

                    Nothing ->
                        0
            """";

        AssertRoundtrip(input);
    }

    [Fact]
    public void Roundtrip_let_expression()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                let
                    x =
                        1
                in
                x + 1
            """";

        AssertRoundtrip(input);
    }

    [Fact]
    public void Roundtrip_record_expression()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                { field = 1 }
            """";

        AssertRoundtrip(input);
    }

    [Fact]
    public void Roundtrip_tuple_expression_not_formatted()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
               (
                  a
              , b,
                   c
              )

            """";

        AssertRoundtrip(input);
    }

    [Fact]
    public void Roundtrip_record_expression_not_formatted()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
              { field   = 1  ,
                  someOther  =    17
                , nextField=19
                    }
            """";

        AssertRoundtrip(input);
    }


    [Fact]
    public void Roundtrip_comments_in_record_type_alias_declaration()
    {
        var input =
            """"
            module Test exposing (..)


            type alias Record =
                { fa : List String
                , fb : Int

                -- La loro conformazione

                , fc : List String

                         --   è profondamente modificata
                , fd : Bool
                , fe : Int   --  dando luogo a
                }

            """";

        AssertRoundtrip(input);
    }

    [Fact]
    public void Roundtrip_left_pipe_operator_not_formatted()
    {
        var input =
            """"
            module Test exposing (..)


            decl b =
                Ok
                   <|
                    if b then
                        "YES"

                    else
                        "NO"

            """";

        AssertRoundtrip(input);
    }
}
