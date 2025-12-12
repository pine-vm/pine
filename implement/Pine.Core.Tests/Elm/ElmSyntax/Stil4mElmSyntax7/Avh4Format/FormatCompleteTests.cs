using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using System;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mElmSyntax7.Avh4Format;

using Rendering = Core.Elm.ElmSyntax.Stil4mElmSyntax7.Rendering;
using Avh4Format = Core.Elm.ElmSyntax.Stil4mElmSyntax7.Avh4Format;

public class FormatCompleteTests
{
    private static readonly Rendering.Config s_renderingDefaultConfig =
        Rendering.ConfigPreserveLocations();

    private static string RenderDefault(File file) =>
        Rendering.ToString(
            file,
            s_renderingDefaultConfig);

    private static string FormatString(
        string input)
    {
        var parsed =
            ElmSyntaxParser.ParseModuleText(input, enableMaxPreservation: true)
            .Extract(err => throw new Exception($"Parsing failed: {err}"));

        var formatted =
            Avh4Format.Format(parsed);

        var rendered =
            RenderDefault(formatted);

        return rendered;
    }

    [Fact]
    public void Simple_module()
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
    public void Simple_declaration()
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
    public void Aligns_function_application_arguments_multiline()
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
    public void Breaks_function_application_arguments_multiline()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b c =
                a
                    b c
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
    public void Trims_whitespace_in_empty_list()
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
    public void Indents_items_in_nested_list_multiline()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                [  [   13
                     , 17
                   ,     21
                 ]
                ]
            
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl =
                [ [ 13
                  , 17
                  , 21
                  ]
                ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Linebreak_between_then_and_else()
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
    public void Adds_Linebreak_between_case_of_branch()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b c =
                case a of
                    Just x ->
                        b + x            
                    Nothing ->
                        c
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl a b c =
                case a of
                    Just x ->
                        b + x

                    Nothing ->
                        c
            """"
            .Trim());
    }

    [Fact]
    public void Adds_linebreak_between_top_level_declarations()
    {
        var input =
            """"
            module Test exposing (..)
            
            
            beta =
                37
            
            alfa arg0 =
                [ arg0 ]

            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            beta =
                37
            
            
            alfa arg0 =
                [ arg0 ]

            """"
            .Trim());
    }

    [Fact]
    public void Spaces_between_infix_operators()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b c =
                a+b*c

            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl a b c =
                a + b * c

            """"
            .Trim());
    }

    [Fact]
    public void Aligns_chain_of_if_then_else()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b c =
                if a < 100 then
                    [ b, c ]
            
                else
                if a > 200 then
                    [ c, b ]
            
                else
                    []

            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl a b c =
                if a < 100 then
                    [ b, c ]

                else if a > 200 then
                    [ c, b ]

                else
                    []

            """"
            .Trim());
    }

    [Fact]
    public void Groups_imports()
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
    public void Orders_imports()
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
    public void Formats_import_alias()
    {
        var input =
            """"
            module Test exposing (..)
            


            import Dict   as     OtherName


            decl =
                1
            
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)

            import Dict as OtherName


            decl =
                1
            """"
            .Trim());
    }

    [Fact]
    public void Breaks_exposing_list_if_multiline()
    {
        var input =
            """"
            module Test exposing (alfa, beta,
                gamma)


            decl =
                1
            
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing
                ( alfa
                , beta
                , gamma
                )


            decl =
                1
            
            """"
            .Trim());
    }

    [Fact]
    public void Formats_record_type_alias_single_line()
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
    public void Removes_superfluous_line_breaks_between_let_declarations()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b =
                let
                    c =
                        a + b


                    d =
                        a - b
                in
                c * 2
            
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl a b =
                let
                    c =
                        a + b

                    d =
                        a - b
                in
                c * 2
            
            """"
            .Trim());
    }

    [Fact]
    public void Adds_missing_line_breaks_between_let_declarations()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b =
                let
                    c =
                        a + b
                    d =
                        a - b
                in
                c * 2
            
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl a b =
                let
                    c =
                        a + b

                    d =
                        a - b
                in
                c * 2
            
            """"
            .Trim());
    }

    [Fact]
    public void Formats_choice_type_declaration_tags()
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
    public void Removes_superfluous_linebreaks_before_first_decl_simple()
    {
        var input =
            """"
            module Test exposing (..)



            decl : Int
            decl =
                71
            
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)
            
            
            decl : Int
            decl =
                71
            
            """"
            .Trim());
    }

    [Fact]
    public void Removes_superfluous_linebreaks_before_first_decl_typed()
    {
        var input =
            """"
            module Test exposing (..)



            decl : List Int
            decl =
                []
            
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)
            
            
            decl : List Int
            decl =
                []
            
            """"
            .Trim());
    }

    [Fact]
    public void Removes_superfluous_linebreaks_in_type_annotation_in_let_block()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                let
                    inner :
                    Int
                    inner =
                        5
                in
                inner + 10
            
            
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl =
                let
                    inner : Int
                    inner =
                        5
                in
                inner + 10
            
            
            """"
            .Trim());
    }

    [Fact]
    public void Formats_record_update_single_line()
    {
        var input =
            """"
            module Test exposing (..)


            decl r fv =
                {   r     |   field  =     fv,     otherField   =    42      }
            
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl r fv =
                { r | field = fv, otherField = 42 }
            
            """"
            .Trim());
    }

    [Fact]
    public void Formats_record_update_multi_line()
    {
        var input =
            """"
            module Test exposing (..)
            
            
            decl r fv =
                 {   r
                     |   field =     fv
                    ,     otherField =     42
               }
            
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl r fv =
                { r
                    | field = fv
                    , otherField = 42
                }
                        
            """"
            .Trim());
    }

    [Fact]
    public void Stable_configurations()
    {
        /*
         * Set of module texts that are already formatted according to Avh4 style.
         * Therefore, formatting them again should yield the same text.
         * */

        IReadOnlyList<string> testCases =
            [
            """"
            module Test exposing
                ( alfa
                , beta
                , gamma
                )
            
            
            decl =
                1

            """",

            """"
            module Test exposing (alfa, beta, gamma)
            
            
            decl =
                1

            """",

            """"
            module Test exposing (..)


            decl a b c =
                a b c

            """",

            """"
            module Test exposing (..)


            infix right 0 (<|) = apL
            infix left  0 (|>) = apR


            decl a =
                1

            """",

            """"
            module Test exposing (..)

            import Browser.Dom as Dom
            import Svg.Attributes exposing (..)


            decl =
                71

            """",

            """"
            module Test exposing (..)


            decl a =
                \b c -> a + b + c

            """",

            """"
            module Test exposing (..)


            decl a t =
                (\b -> a + b) t

            """",

            """"
            module Test exposing (..)


            decl a t =
                (\b ->
                    a + b
                )
                    t

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

            """"
            module Test exposing (..)


            decl =
                [ [ 13, 17, 21 ] ]

            """",

            """"
            module Test exposing (..)


            decl =
                [ [ 13, 17, 21 ]
                ]

            """",

            """"
            module Test exposing (..)


            decl =
                [ [ 13
                  , 17
                  , 21
                  ]
                ]

            """",

            """"
            module Test exposing (..)


            decl =
                [ 71
                , func
                    []
                    91
                ]

            """",

            """"
            module Test exposing (..)


            decl =
                [ 1
                , 13
                , [ 41
                  , func
                        []
                        123
                  ]
                , 71
                , 114
                ]

            """",

            """"
            module Test exposing (..)


            decl =
                [ []
                , [ []
                  , [ 17
                    , 19
                    ]
                  ]
                ]

            """",

            """"
            module Test exposing (..)


            decl1 =
                { a = []
                , b =
                    { a = []
                    }
                }


            decl2 =
                [ { a = []
                  }
                , { a =
                        [ { a = [ 1, 2, 3 ]
                          }
                        ]
                  }
                ]

            """",

            """"
            module Test exposing (..)


            decl : Int -> Int -> Int -> List Int
            decl a b c =
                []

            """",

            """"
            module Test exposing (..)


            decl :
                Int
                -> Int
                -> Int
                -> List Int
            decl a b c =
                []
            """",

            """"
            module Test exposing (..)


            decl a b c =
                (a + b) * c

            """",

            """"
            module Test exposing (..)


            type alias MyRecord =
                { field1 : Int, field2 : String }
            """",

            """"
            module Test exposing (..)


            type alias MyRecord =
                { field1 : Int
                , field2 : String
                }
            """",

            """"
            module Test exposing (..)


            type Maybe a
                = Nothing
                | Just a

            """",

            """"
            module Test exposing (..)


            decl =
                let
                    b =
                        13

                    a =
                        41
                in
                let
                    d =
                        a
                            b
                            17
                in
                e
                    d
                    43

            """",

            """"
            module Test exposing (..)


            decl =
                let
                    ( a, b ) =
                        c
                            41
                            13
                in
                c
                    a
                    b

            """",

            """"
            module Test exposing (..)


            decl =
                let
                    ( a, b ) =
                        c
                            41
                            13
                in
                if a < b then
                    a

                else
                    b

            """",

            """"
            module Test exposing (..)


            decl =
                let
                    a : Maybe Int
                    a =
                        Just
                            5
                in
                case a of
                    Just value ->
                        value + 2

                    Nothing ->
                        0

            """",

            """"
            module Test exposing (..)


            decl =
                """

            not_a_decl  =   42


            """

            """",

            """"
            module Test exposing (..)


            decl a =
                case a of
                    "hello" ->
                        1

                    _ ->
                        0

            """",

            """"
            module Test exposing (..)


            decl a =
                case a of
                    't' ->
                        1

                    _ ->
                        0

            """",

            """"
            module Test exposing (..)


            decl a =
                case a of
                    '\\' ->
                        51

                    '\n' ->
                        71

                    '\u{000D}' ->
                        73

                    '\t' ->
                        77

                    '"' ->
                        79

                    _ ->
                        0

            """",

            """"
            module Test exposing (..)


            decl a =
                case a of
                    ( 13, 17 ) ->
                        23

                    _ ->
                        0

            """",

            """"
            module Test exposing (..)


            decl a =
                case a of
                    [ 13, 17 ] ->
                        21

                    _ ->
                        0

            """",

            """"
            module Test exposing (..)


            decl a =
                case a of
                    () ->
                        71

            """",

            """"
            module Test exposing (..)


            decl a =
                case a of
                    0x81 ->
                        71

                    _ ->
                        42

            """",

            """"
            module Test exposing (..)


            decl a =
                case a of
                    0x0123 ->
                        71

                    0x00012345 ->
                        73
            
                    _ ->
                        42

            """",

            """"
            module Test exposing (..)


            decl a =
                -a

            """",

            """"
            module Test exposing (..)


            decl a =
                a ()

            """",

            """"
            module Test exposing (..)


            decl a =
                a 0x17

            """",

            """"
            module Test exposing (..)


            decl a =
                (*) a 0x17

            """",

            """"
            module Test exposing (..)


            decl a =
                OtherModule.otherDecl a

            """",

            """"
            module Test exposing (..)


            decl r fv =
                { r | field = fv }

            """",

            """"
            module Test exposing (..)


            decl r fv =
                { r
                    | field = fv
                }

            """",

            """"
            module Test exposing (..)


            decl r fv =
                { r | field = fv, otherField = 42 }

            """",

            """"
            module Test exposing (..)


            decl r fv =
                { r
                    | field = fv
                    , otherField = 42
                }

            """",

            """"
            module Test exposing (..)


            decl =
                (\s ->
                    case attemptSecond s of
                        (ParserFast.Good _ _) as secondGood ->
                            secondGood

                        (ParserFast.Bad secondCommitted secondX) as secondBad ->
                            if secondCommitted then
                                secondBad

                            else
                                ParserFast.Bad
                                    Basics.False
                                    (ParserFast.ExpectingOneOf
                                        firstX
                                        secondX
                                        []
                                    )
                )

            """"

            ];

        for (var i = 0; i < testCases.Count; i++)
        {
            var input = testCases[i];

            try
            {
                var formatted = FormatString(input);

                formatted.Trim().Should().Be(input.Trim());
            }
            catch (Exception ex)
            {
                throw new Exception($"Failed in test case index: {i}", ex);
            }
        }
    }
}
