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
    private static string RenderDefault(File file) =>
        Rendering.ToString(file);

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
    public void Preserves_comment_despite_shifting_declarations()
    {
        var input =
            """"
            module Test exposing (..)


            decl_alfa =
                a
                b c d e f g h

            decl_beta =
                -- Simple comment
                71
            
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl_alfa =
                a
                    b
                    c
                    d
                    e
                    f
                    g
                    h


            decl_beta =
                -- Simple comment
                71
                        
            """"
            .Trim());
    }

    [Fact]
    public void Preserves_comment_despite_shifting_list_items()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                [ a + 13, b + 17, a + 19 , b + 21
                -- Simple comment
                , a + 23, b + 29, a + 31, b + 37, a + 41
                ]
            
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl =
                [ a + 13
                , b + 17
                , a + 19
                , b + 21

                -- Simple comment
                , a + 23
                , b + 29
                , a + 31
                , b + 37
                , a + 41
                ]
                        
            """"
            .Trim());
    }

    [Fact]
    public void Preserves_comment_despite_shifting_nested_list_items()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                [ [ 13, 17, 19, 21 ]
                , [ a + 13 , b + 17, a + 19, b + 21
            -- Simple comment
                  , a + 23, b + 29, a + 31
                  , b + 37, a + 41
                  ]
                , [ 43, 47, 53, 59 ]
                ]
            
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl =
                [ [ 13, 17, 19, 21 ]
                , [ a + 13
                  , b + 17
                  , a + 19
                  , b + 21

                  -- Simple comment
                  , a + 23
                  , b + 29
                  , a + 31
                  , b + 37
                  , a + 41
                  ]
                , [ 43, 47, 53, 59 ]
                ]
                        
            """"
            .Trim());
    }

    [Fact]
    public void Connects_doc_comment_to_declaration()
    {
        var input =
            """"
            module Test exposing (..)

            import Dict



            {- One multiline comment

               Another line
            -}
            alfa =
                41


            {-| A doc comment
            -}


            beta =
                39
            
            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)

            import Dict



            {- One multiline comment

               Another line
            -}


            alfa =
                41


            {-| A doc comment
            -}
            beta =
                39
                        
            """"
            .Trim());
    }

    [Fact]
    public void Indents_single_line_comment_between_choice_type_tags()
    {
        var input =
            """"
            module Test exposing (..)


            type String
                = String Int
            -- another tag
                | AnyOtherKind_String

            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            type String
                = String Int
                  -- another tag
                | AnyOtherKind_String

            """"
            .Trim());
    }

    [Fact]
    public void Aligns_record_type_annotation_multiline()
    {
        var input =
            """"
            module Test exposing (..)


            decl :
                { beta :   Char
                  ,        alfa :    List Int
                , gamma     : Dict.Dict  Int     Char
                }
            decl =
                other

            """";

        var formatted = FormatString(input);

        formatted.Trim().Should().Be(
            """"
            module Test exposing (..)


            decl :
                { beta : Char
                , alfa : List Int
                , gamma : Dict.Dict Int Char
                }
            decl =
                other

            """"
            .Trim());
    }

    [Fact]
    public void File_ends_with_trailing_newline()
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
    public void Stable_configurations_units()
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


            alfa =
                ( 13, 17 )


            beta =
                ( 19
                , 23
                )


            gamma =
                ( 29
                , ( 31, 37 )
                , 41
                )

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


            foldl : (a -> b -> b) -> b -> Array a -> b
            foldl foldItem seed array =
                []

            """",

            """"
            module Test exposing (..)


            composeL : (b -> c) -> (a -> b) -> (a -> c)
            composeL g f x =
                g (f x)

            """",

            """"
            module Test exposing (..)


            alfa : (b -> c) -> ((a -> b) -> (a -> c))
            alfa =
                other


            beta : ((b -> c) -> (a -> b)) -> (a -> c)
            beta =
                other

            """",

            """"
            module Test exposing (..)


            decl : { beta : Char, alfa : List Int, gamme : Dict.Dict Int Char }
            decl =
                other

            """",

            """"
            module Test exposing (..)


            decl :
                { beta : Char
                , alfa : List Int
                }
            decl =
                other

            """",

            """"
            module Test exposing (..)


            decl :
                { beta : Char
                , alfa : { f71 : Char, f79 : Int, f83 : Char }
                , gamma : Dict.Dict Int Char
                }
            decl =
                other

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
                    ( a, b ) =
                        c
                            41
                            13
                in
                if a < b then
                    a

                else if a > b then
                    b

                else if a > c then
                    c

                else
                    0

            """",

            """"
            module Test exposing (..)


            decl =
                if
                    func
                        []
                then
                    []

                else
                    []

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
                func
                    (if a b then
                        c

                     else
                        d
                    )

            """",

            """"
            module Test exposing (..)
            
            
            decl =
                [ ( "other"
                  , { field_a =
                        ( 13
                        , 17
                        )
                    }
                  )
                ]

            """",

            """"
            module Test exposing (..)
            
            
            decl =
                [ 13
                , func a b
                    |> modBy c
                ]

            """",

            """"
            module Test exposing (..)
            
            
            decl =
                """
            A line
            B line
            """

            """",

            """"
            module Test exposing (..)
            
            
            decl =
                """ single line """

            """",

            """"
            module Test exposing (..)
            
            
            decl =
                [ """

            a
            """
                , """
            b

            """
                ]

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

                    '\u{0000}' ->
                        83

                    '\u{0008}' ->
                        85

                    '\u{000C}' ->
                        87

                    _ ->
                        0

            """",

            """"
            module Test exposing (..)


            decl =
                "\n"

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

                    0x7FFFFFFF ->
                        79

                    0x0000000100000000 ->
                        83

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


            decl =
                []
                    |> identity

            """",

            """"
            module Test exposing (..)


            decl =
                [ ( 13
                  , []
                        |> identity
                  , 17
                  )
                ]

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

            """",

            """"
            module Test exposing (..)


            type alias EmitStack =
                { fa : List String
                , fb : Int

                -- La loro conformazione
                , fc : List String

                -- è profondamente modificata
                , fd : Bool
                , fe : Int
                }

            """",

            """"
            module Test exposing (..)


            type Expression
                = LiteralExpression Value
                | ListExpression (List Expression)
                | ParseAndEvalExpression
                    -- Encoded
                    Expression
                    -- Environment
                    Expression
                | KernelApplicationExpression String Expression
                | ConditionalExpression
                    -- Condition
                    Expression
                    -- False Branch
                    Expression
                    -- True Branch
                    Expression
                | EnvironmentExpression

            """",

            """"
            module Test exposing (..)


            decl =
                -- Simple comment
                71
                        
            """",

            """"
            module Test exposing (..)


            toCode : Char -> Int
            toCode char =
                Pine_kernel.int_add
                    [ -- Add the sign prefix byte
                      Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]

                    -- Use kernel function 'add' to ensure canonical form
                    , 0
                    ]
                        
            """",

            """"
            module Test exposing (..)


            charCode =
                Pine_kernel.int_add
                    [ Pine_kernel.int_mul [ firstFourBits, 4096 ] -- Multiply by 2^12
                    , Pine_kernel.int_mul [ secondSixBits, 64 ] -- Multiply by 2^6
                    , thirdSixBits
                    ]
                        
            """",

            """"
            module Test exposing (..)


            decl a b =
                if
                    -- A simple comment
                    a < b
                then
                    -- Another simple comment
                    [ 13 ]

                else
                    -- Yet another simple comment
                    [ 17 ]
                        
            """",

            """"
            module Test exposing (..)


            decl a b =
                if a < b then
                    [ 13 ]

                else if
                    -- A simple comment
                    a > b
                then
                    -- Another simple comment
                    [ 17 ]

                else
                    -- Yet another simple comment
                    [ 21 ]
                        
            """",

            """"
            module Test exposing (..)


            decl a b =
                let
                    alfa =
                        42
                in
                if a == 71 then
                    [ 13 ]

                else if
                    -- A comment before condition
                    (a == 73)
                        || (a == 77)
                    -- Comment between conditions
                then
                    [ 17 ]

                else if
                    -- Another simple comment
                    a == 79
                then
                    -- And another simple comment
                    [ 21 ]

                else
                    -- Yet another simple comment
                    [ 23 ]
                        
            """",

            """"
            module Test exposing (..)


            decl =
                [ 13
                , 17

                -- Fra
                -- i
                -- pesci
                , 23

                -- cartilaginei
                , 19
                ]
                        
            """",

            """"
            module Test exposing (..)


            type String
                = String Int
                  -- another tag
                | AnyOtherKind_String
                        
            """",

            """"
            module Test exposing (..)


            decl =
                {- Multi-line comment
                   Test
                -}
                71
                        
            """",

            """"
            module Test exposing (..)

            import Dict



            {- One multiline comment

               Another line
            -}


            decl =
                []

            """",

            """"
            module Test exposing (..)

            import Dict


            {-| One multiline comment

            Another line

            -}
            decl =
                []

            """",

            """"
            module Test exposing (..)

            import Char


            {-| Represents the relative ordering of two things.
            The relations are less than, equal to, and greater than.
            -}
            type Order
                = LT
                | EQ
                | GT

            """",

            """"
            module Bitwise exposing
                ( and
                , complement
                )

            {-
               Functions in the 'Bitwise' module emulate limits of JavaScript bitwise operations for backwards-compatibility.

               To provide an Elm core library that is backward-compatible with libraries and apps implemented for
               legacy platforms, simulate mapping from integer to two's complement and wrapping to 32-bit.
            -}


            decl =
                42

            """",

            """"
            module FNV1a exposing (hash, hashWithSeed, initialSeed)

            {-|

            @docs hash, hashWithSeed, initialSeed

            -}

            import Bitwise


            hash =
                Bitwise.and


            hashWithSeed =
                Bitwise.and


            initialSeed =
                0

            """",

            ];

        for (var i = 0; i < testCases.Count; i++)
        {
            var testCase = testCases[i];

            try
            {
                AssertSyntaxNodesValueEqualityForModuleText(testCase);

                var formatted = FormatString(testCase);

                formatted.Trim().Should().Be(testCase.Trim());
            }
            catch (Exception ex)
            {
                throw new Exception($"Failed in test case index: {i}", ex);
            }
        }
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
