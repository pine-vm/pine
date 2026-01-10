using AwesomeAssertions;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mConcretized.Avh4Format;

using static FormatTestHelper;

public class FormatCompleteTests
{
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
    public void Trims_linebreaks_and_whitespaces_in_case_block()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b c =
                
                case   a    of
                
                    
                    Just   x    ->
                    
                          b + x
            
                    Nothing->

                       c
            """";

        var expected =
            """"
            module Test exposing (..)


            decl a b c =
                case a of
                    Just x ->
                        b + x

                    Nothing ->
                        c
            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Supports_varying_indent_of_arms_in_case_block()
    {
        /*
         * The original avh4/elm-format failed to format this module.
         * The original Elm compiler gave the following parsing error:
         * 
         * Detected problems in 1 module.
         * -- UNEXPECTED NAME ------------------------------------------------ src\Test.elm
         * 
         * I got stuck on this name:
         * 
         * 9|        Nothing ->
         *           ^^^^^^^
         * It is confusing me a lot! Normally I can give fairly specific hints, but
         * something is really tripping me up this time.
         * */

        var input =
            """"
            module Test exposing (..)


            decl a =
                case a of
                     Just x ->
                        b
            
                 Nothing ->
                        c
            """";

        var expected =
            """"
            module Test exposing (..)


            decl a =
                case a of
                    Just x ->
                        b

                    Nothing ->
                        c
            """";

        AssertModuleTextFormatsToExpected(input, expected);
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
              -- Another line of comment
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
                -- Another line of comment
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
    public void Roundtrip_nested_infix_operators_multiline()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b c =
                a
                    + b
                    * c
            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_function_application_with_first_arg_on_first_line()
    {
        /*
         * Rule of avh4/elm-format:
         * Even if the overall application expression is multi-line,
         * the first argument is allowed to stay on the same line as the function name.
         * */

        var input =
            """"
            module Test exposing (..)


            decl a b c d =
                a b
                    c
                    d
            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_type_annotation_arrow_on_own_line()
    {
        /*
         * Rule of avh4/elm-format:
         * When the -> operator is on its own line in a function type annotation,
         * the result type should also go on a new line with additional indentation.
         * */

        var input =
            """"
            module Test exposing (..)


            func :
                ArgType
                ->
                    Result
                        ErrorType
                        OkType
            func =
                Debug.todo ""
            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_import_ordering_case_sensitive()
    {
        /*
         * Rule of avh4/elm-format:
         * Imports are sorted using case-sensitive (ordinal) string comparison.
         * FNV should come before File because 'N' (ASCII 78) < 'i' (ASCII 105).
         * */

        var input =
            """"
            module Test exposing (..)

            import FNVModule
            import FileModule


            decl =
                0
            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_application_argument_with_hex_in_parens()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                modBy 0x0100 (charCode // 0x0100)

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_joining_strings_containing_escaped_chars_on_single_line()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                "\n" ++ string

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_lambda_with_comment_multi_line_at_root()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                \c ->
                    {- some comment
                    -}
                    [ c ]
            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_module_doc_comment()
    {
        /*
         * Rule of avh4/elm-format:
         * A doc comment following the module declaration should stay attached to the module,
         * instead of attaching to the following declaration.
         * */

        var input =
            """"
            module Frontend.MonacoEditor exposing (..)

            {-| Types for exchanging messages with the JavaScript wrapping the Monaco Editor.
            -}


            type MessageToEditor
                = OpenDocumentEvent OpenDocumentEventStruct

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_multiline_record_type_annotation_on_let_declaration()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                let
                    innerDecl :
                        { a : String
                        , b : Int
                        }
                    innerDecl =
                        other
                in
                71

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_list_containing_escaped_char()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                [ '\\', 'n' ]

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_signature_with_multiline_type_annotation_containing_record()
    {
        var input =
            """"
            module Test exposing (..)


            decl :
                String
                ->
                    { arancini : List Int
                    }
                -> Int
            decl =
                71

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_application_containing_singleline_comments()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b c =
                -- Alcuni gruppi
                --
                b
                    -- di squali,
                    --
                    a
                    --
                    -- in particolare
                    --
                    c

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_application_containing_multiline_comments()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b c =
                {- Alcuni gruppi

                -}
                b
                    {- di squali,

                    -}
                    a
                    {- in particolare

                    -}
                    c

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Keep_left_pipe_operator_on_end_of_preceding_line()
    {
        /*
         * https://github.com/avh4/elm-format/blob/e7e5da37716acbfb4954a88128b5cc72b2c911d9/Style%20Guide/Expressions.md#left-pipe-operator
         * Rule from avh4/elm-format:
         * Unlike other binary operators, the left pipe <| operator is places at the end of the preceding line,
         * and the following lines are indented.
         * */

        var input =
            """"
            module Test exposing (..)


            decl b =
                Ok <|
                    if b then
                        "YES"

                    else
                        "NO"

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Keep_left_pipe_operator_followed_by_parens()
    {
        /*
         * https://github.com/avh4/elm-format/blob/e7e5da37716acbfb4954a88128b5cc72b2c911d9/Style%20Guide/Expressions.md#left-pipe-operator
         * Rule from avh4/elm-format:
         * Parentheses are not used around the second term unless it is a binary operator expression:
         * */

        var input =
            """"
            module Test exposing (..)


            declA =
                Ok <| ([ 1, 2, 3 ] |> List.tail)


            declB =
                Ok <| ("a" ++ "b")


            declC =
                test "should pass" <|
                    \() ->
                        Expect.pass

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_comments_in_record_expression()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                { field_beta =
                    {-
                       presentano anch'essi
                       ampie pinne pettorali

                    -}
                    -- []
                    71

                {-
                   che ricordano
                   il disco
                -}
                , {-
                     tipico delle razze
                     ma si distinguono
                  -}
                  field_alfa = 73
                }

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_singleline_comments_in_type_alias_declaration_record()
    {
        var input =
            """"
            module Test exposing (..)


            type alias Record =
                -- Nelle razze,
                { -- le cinque
                  a
                  -- o sei
                    :
                    -- paia di branchie
                    Int

                -- sono situate invece
                , -- in posizione
                  b : String

                -- ventrale
                }

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_multiline_comments_in_type_alias_declaration_record()
    {
        var input =
            """"
            module Test exposing (..)


            type alias Record =
                {- Nelle razze,

                -}
                { {- le cinque -}
                  a
                  {- o sei

                  -}
                    :
                    {- paia di branchie -} Int

                {- sono situate invece -}
                , {- in posizione

                  -}
                  b : String

                {- ventrale -}
                }

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_singleline_comments_in_let_block()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                let
                    -- In molte specie
                    -- il capo
                    a =
                        -- fa parte
                        -- del disco
                        41

                    b =
                        -- mentre in altre
                        -- la testa è distinta
                        71

                    -- e le pinne pettorali
                    -- vi si raccodano.
                in
                []

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_singleline_comments_between_infix_operators()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b c =
                -- In molte specie
                -- il capo
                a
                    -- fa parte
                    -- del disco
                    && -- mentre in altre
                       b
                    -- la testa è distinta
                    && c

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_singleline_comments_in_tuple()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                ( -- gli occhi
                  71
                  -- e gli spiracoli
                , -- sono solitamente
                  -- disposti
                  73
                  -- sulla sommità
                  -- del capo.
                , 79
                  -- In alcune specie
                  -- abissali di Torpedinoidei,
                )

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_singleline_comments_in_if_then_else()
    {
        var input =
            """"
            module Test exposing (..)


            declD a b =
                -- gli occhi
                if
                    -- e gli spiracoli
                    -- sono solitamente
                    a > b
                    -- disposti
                then
                    -- sulla sommità
                    Just [ a ]
                    -- del capo.

                else
                    -- In alcune specie
                    -- abissali di Torpedinoidei,
                    Nothing

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_singleline_comments_in_nested_if_then_else()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b =
                OTag
                    (\c ->
                        let
                            n =
                                []
                        in
                        if a > b then
                            Just [ a ]

                        else
                        -- gli occhi
                        if
                            -- e gli spiracoli
                            -- sono solitamente
                            a == b
                        then
                            -- disposti
                            TagName FA
                                ()
                                (c
                                    d
                                    e
                                )
                            -- sulla sommità

                        else
                            -- del capo.
                            Nothing
                    )


            reducedRegression isGood expecting =
                \s ->
                    -- not found
                    if Pine_kernel.equal [ newOffset, -1 ] then
                        Bad False (fromState s expecting)
                        -- newline

                    else if Pine_kernel.equal [ newOffset, -2 ] then
                        Good True
                            ()
                            (PState
                                srcBytes
                            )
                        -- found

                    else
                        Good True
                            ()
                            (PState
                                srcBytes
                            )

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_block_comments_in_various_patterns()
    {
        var input =
            """"
            module Test exposing (..)


            decl arg =
                case arg of
                    ({- La loro -} 17 :: b) as c ->
                        ()

                    (19 {- conformazione -} :: b) as c ->
                        ()

                    (21 :: {- è profondamente modificata -} b) as c ->
                        ()

                    (23 :: b {- dando luogo -}) as c ->
                        ()

                    27 :: ( {- a evidenti forme -} b, c ) :: _ ->
                        ()

                    29 :: ( b {- di specializzazione -}, c ) :: _ ->
                        ()

                    31 :: ( b, {- Sono pesci -} c ) :: _ ->
                        ()

                    37 :: ( b, c {- dal corpo depresso -} ) :: _ ->
                        ()

                    _ ->
                        ()

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_singleline_comment_before_right_pipe_operator()
    {
        var input =
            """"
            module Test exposing (..)


            decl a b c =
                a
                    -- Nelle razze bentoniche
                    -- cioè diffuse
                    |> b
                        -- sui fondali
                        c

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_multiline_string_literals_in_record_expression()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                { a =
                    [ """

            da esse

            """
                    , """

            per la presenza

             """
                    ]
                , b = """
            di fessure branchiali

              """ ++ """

            ai lati del capo
                """
                }

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_case_block_in_tuple_in_list()
    {
        var input =
            """"
            module Test exposing (..)


            decl a =
                [ ( "DecodedArguments"
                  , case
                        a
                    of
                        Err err ->
                            71

                        Ok evalOk ->
                            73
                  )
                ]

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_record_expressions_in_pipeline_and_lambda()
    {
        /*
         * Indent caused by pipeline and opening of lambda is > 3,
         * so inner nodes are moved to the next indent level
         * */

        var input =
            """"
            module Test exposing (..)


            decl =
                a
                    |> b
                        { fa = 71
                        , fb = 79
                        }
                    |> (\result ->
                            { fc = result.encodeFunction.text
                            , fd = result.decodeFunction.text
                            }
                       )

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_record_update_expression_in_tuple()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                let
                    state =
                        ( { stateBefore
                            | elmPackages =
                                ( packageVersionIdentifer, parsedModules )
                                    :: stateBefore.elmPackages
                          }
                        , 91
                        )
                in
                []

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_case_of_expression_in_tuple()
    {
        var input =
            """"
            module Test exposing (..)


            decl a =
                let
                    state =
                        ( case a of
                            71 ->
                                [ 31 ]

                            79 ->
                                [ 27 ]
                        , 91
                        )
                in
                []

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_record_update_expressions_in_let_block()
    {
        var input =
            """"
            module Test exposing (..)


            decl =
                let
                    state : LanguageServiceState
                    state =
                        { stateBefore
                            | elmPackages =
                                ( packageVersionIdentifer, parsedModules )
                                    :: stateBefore.elmPackages
                        }
                in
                []

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_choice_type_tag_argument_in_parens()
    {
        var input =
            """"
            module Test exposing (..)


            decl a =
                TagName
                    (let
                        x =
                            [ a ]
                     in
                     []
                    )

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_multiple_multiline_imports()
    {
        var input =
            """"
            module Test exposing (..)

            import Alfa
                exposing
                    ( a1
                    , b1
                    )
            import Beta
                exposing
                    ( a2
                    , b2
                    )
            import Dict


            type alias RecordName =
                {}

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_multiline_comment_in_nested_list_uncons_pattern()
    {
        var input =
            """"
            module Test exposing (..)


            decl a =
                case a of
                    LiteralExpression (Pine.ListValue ({- 'String' tag -} _ :: (Pine.ListValue [ literalChars ]) :: _)) ->
                        71

                    _ ->
                        73

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Adding_line_breaks_does_not_interfere_with_following_declarations()
    {
        var input =
            """"
            module Test exposing (..)

            
            declA =
                [ 31, 37, 41
                , 43, 47
                ]


            type alias RecordA =
                { a : Int
                , b : String
                , c : List String
                }


            type alias ParsedDeclaration =
                Declaration ( DeclarationRange, CookedDocumentation )


            declB a =
                case a of
                    True ->
                        "YES"

                    False ->
                        "NO"


            declC =
                \a b c ->
                    a
                        b
                        c

            declD :
                List Int
                -> Int
                -> Int
                -> Int
            declD a ( b, c ) =
            
               case
            
            
                    a
                
                    
                 of
                    [] ->
                        b + c
            
                    41 :: _ ->
                        b * c
            
                    43 :: _ ->
                        let
                            innerF a1 b1 c1 =
                                a1 - b1 + c1
            
                            ( a2, b2 ) =
                                delta
                        in
                        innerF a b c

                    47 :: _ ->
                        let
                            zeta =
                                delta
                                    |> List.map
                                        (\i ->
                                            case i of
                                                Nothing ->
                                                    []

                                                Just fst ->
                                                    fst
                                                        |> (\( ia, ib, ic ) ->
                                                                []
                                                           )
                                        )
                        in
                        []

                    fst :: following ->
                        b - c
            """";

        var expected =
            """"
            module Test exposing (..)

            
            declA =
                [ 31
                , 37
                , 41
                , 43
                , 47
                ]


            type alias RecordA =
                { a : Int
                , b : String
                , c : List String
                }


            type alias ParsedDeclaration =
                Declaration ( DeclarationRange, CookedDocumentation )


            declB a =
                case a of
                    True ->
                        "YES"

                    False ->
                        "NO"


            declC =
                \a b c ->
                    a
                        b
                        c


            declD :
                List Int
                -> Int
                -> Int
                -> Int
            declD a ( b, c ) =
                case
                    a
                of
                    [] ->
                        b + c

                    41 :: _ ->
                        b * c

                    43 :: _ ->
                        let
                            innerF a1 b1 c1 =
                                a1 - b1 + c1

                            ( a2, b2 ) =
                                delta
                        in
                        innerF a b c

                    47 :: _ ->
                        let
                            zeta =
                                delta
                                    |> List.map
                                        (\i ->
                                            case i of
                                                Nothing ->
                                                    []
            
                                                Just fst ->
                                                    fst
                                                        |> (\( ia, ib, ic ) ->
                                                                []
                                                           )
                                        )
                        in
                        []

                    fst :: following ->
                        b - c

            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Trimming_line_breaks_does_not_interfere_with_following_declarations()
    {
        var input =
            """"
            module Test exposing (..)

            
            declA =
                [


                ]


            type alias RecordA =
                { a : Int
                , b : String
                , c : List String
                }


            type alias ParsedDeclaration =
                Declaration ( DeclarationRange, CookedDocumentation )


            declB a =
                case a of
                    True ->
                        "YES"

                    False ->
                        "NO"


            declC =
                \a b c ->
                    a
                        b
                        c

            
            declD a ( b, c ) =
               case


                    a
                
                    
                 of
                    [] ->
                        b + c
            
                    41 :: _ ->
                        b * c
            
                    43 :: _ ->
                        let
                            innerF a1 b1 c1 =
                                a1 - b1 + c1
            
                            ( a2, b2 ) =
                                delta
                        in
                        innerF a b c
            
                    fst :: following ->
                        b - c
            """";

        var expected =
            """"
            module Test exposing (..)

            
            declA =
                []


            type alias RecordA =
                { a : Int
                , b : String
                , c : List String
                }


            type alias ParsedDeclaration =
                Declaration ( DeclarationRange, CookedDocumentation )


            declB a =
                case a of
                    True ->
                        "YES"

                    False ->
                        "NO"


            declC =
                \a b c ->
                    a
                        b
                        c


            declD a ( b, c ) =
                case
                    a
                of
                    [] ->
                        b + c

                    41 :: _ ->
                        b * c

                    43 :: _ ->
                        let
                            innerF a1 b1 c1 =
                                a1 - b1 + c1

                            ( a2, b2 ) =
                                delta
                        in
                        innerF a b c

                    fst :: following ->
                        b - c
            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Restores_let_declaration_from_type_annotation_broken_colon_on_new_line()
    {
        var input =
            """"
            module Test exposing (..)
            
            
            decl =
                let
                    inner
            : Int
                    inner =
                        71
                in
                inner

            """";

        var expected =
            """"
            module Test exposing (..)


            decl =
                let
                    inner : Int
                    inner =
                        71
                in
                inner

            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Restores_let_declaration_from_type_annotation_broken_type_on_new_line()
    {
        var input =
            """"
            module Test exposing (..)
            
            
            decl =
                let
                    inner :
            Int
                    inner =
                        71
                in
                inner

            """";

        var expected =
            """"
            module Test exposing (..)


            decl =
                let
                    inner : Int
                    inner =
                        71
                in
                inner

            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Preserves_parens_around_infix_operators_depending_on_precedence()
    {
        var input =
            """"
            module Test exposing (..)


            declA a b c d =
                (a + b) * (c - d)


            declB =
                (a == b) || ((c /= d) && (e < f))

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Preserves_single_superfluous_parens_around_infix_operators()
    {
        var input =
            """"
            module Test exposing (..)


            declA =
                (a + b) + c


            declB =
                a + (b + c)


            declC =
                (a * b) * c


            declD =
                ((a + b) + c) + d

            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Trims_duplicate_superfluous_parens_around_infix_operators()
    {
        var input =
            """"
            module Test exposing (..)


            declA a b c d =
                ((a + b)) * (((c - d)))

            declB =
                (((a == b))) || ((((c /= d)) && ((e < f))))

            """";

        var expected =
            """"
            module Test exposing (..)


            declA a b c d =
                (a + b) * (c - d)


            declB =
                (a == b) || ((c /= d) && (e < f))

            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Trims_superfluous_parens_around_literals_and_names()
    {
        var input =
            """"
            module Test exposing (..)


            declA =
                ("test")


            declB =
                (17)


            declC =
                (True)


            declD =
                (test)

            """";

        var expected =
            """"
            module Test exposing (..)


            declA =
                "test"


            declB =
                17


            declC =
                True


            declD =
                test

            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Preserve_float_expressions_in_various_forms()
    {
        /*
         * Test we avoid these issues:
         * 
         * + https://github.com/avh4/elm-format/issues/680
         * + https://github.com/stil4m/elm-syntax/issues/108
         * */

        var input =
            """"
            module Test exposing (..)


            decl =
                [ 6.022e23
                , 3.0e8
                , 1.6e-19
                , 2.99792458e8
                , 9.81e0
                , 1.0e-10
                , 5.0e4
                , 1.0e12
                , 7.5e-3
                , 6.000022e39
                , 18446744073709549568.0
                , 18446744073709550000.0
                , 0.000000000000000000123456789e-20
                , 1.000
                ]

            """";

        AssertModuleTextFormatsToItself(input);
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
            module Test exposing (..)


            decl a =
                case a of
                    -- dando luogo
                    0 ->
                        -- a evidenti
                        13

                    1 ->
                        17

            """",

            """"
            module Test exposing (..)


            decl =
                [ 17
                , -- A comment
                  19
                , 23
                ]

            """",

            """"
            module Test exposing (..)


            decl =
                [ 17

                -- A comment
                , 19
                , 23
                ]

            """",

            """"
            module Test exposing (..)


            decl =
                [ 17
                , 19 -- A comment
                , 23
                ]

            """",

            """"
            module Test exposing (..)


            decl =
                """

            testing 

            escape \t sequences \u{000D} in \\\\ strings " '


            """

            """",

            """"
            module Test exposing (..)


            type alias RecordType =
                { fieldA : Int
                , fieldB :
                    List
                        ( String
                        , List Int
                        )
                }

            """",

            """"
            module Test exposing (..)


            decl =
                [ ( "a string constant"
                  , Tag
                        { files = []
                        }
                  )
                ]

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

                    '\u{00A0}' ->
                        91

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
                --
                , fc : List String

                --
                -- è profondamente modificata
                --
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
                    a == 79
                    -- Another simple comment
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


            decl a =
                case a of
                    -- dando luogo
                    0 ->
                        -- a evidenti
                        13

                    1 ->
                        17

            """",

            """"
            module Test exposing (..)


            decl =
                [ 17
                , -- A comment
                  19
                , 23
                ]

            """",

            """"
            module Test exposing (..)


            decl =
                """

            testing 

            escape \t sequences \u{000D} in \\\\ strings " '


            """

            """",

            """"
            module Test exposing (..)


            type alias RecordType =
                { fieldA : Int
                , fieldB :
                    List
                        ( String
                        , List Int
                        )
                }

            """",

            """"
            module Test exposing (..)


            decl =
                [ ( "a string constant"
                  , Tag
                        { files = []
                        }
                  )
                ]

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

            """"
            module Records exposing (..)


            updateRecord_0 record =
                { record
                    | field1 = 13
                }


            updateRecord_1 record =
                { record
                    | field1 = 17
                    , field2 = 21
                }
            """",

            """"
            module Elm.Parser.Declarations exposing (..)


            infixDirectionOnlyTwo_noRange : ParserFast.Parser Elm.Parser.Declarations.InfixDirection
            infixDirectionOnlyTwo_noRange =
                ParserFast.Parser
                    (\s ->
                        case attemptFirst s of
                            (ParserFast.Good _ _) as firstGood ->
                                firstGood

                            (ParserFast.Bad firstCommitted firstX) as firstBad ->
                                if firstCommitted then
                                    firstBad

                                else
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

            ];

        for (var i = 0; i < testCases.Count; i++)
        {
            var testCase = testCases[i];

            try
            {
                AssertModuleTextFormatsToItself(testCase);
            }
            catch (Exception ex)
            {
                throw new Exception($"Failed in test case index: {i}", ex);
            }
        }
    }

    [Fact]
    public void Roundtrip_function_type_annotation_with_nested_generic_records()
    {
        // Regression test: Function type annotation with nested generic records
        // was adding an extra space after closing braces
        var input =
            """"
            module Test exposing (..)


            mapErrorStringForFunctionDeclaration : Elm.Syntax.Node.Node { a | declaration : Elm.Syntax.Node.Node { b | name : Elm.Syntax.Node.Node String } } -> String -> String
            mapErrorStringForFunctionDeclaration node baseErr =
                baseErr
            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Roundtrip_commented_out_list_item_before_closing_bracket()
    {
        // Regression test: Comment (commented-out list item) before closing bracket being lost
        var input =
            """"
            module Test exposing (..)


            mapToValueDict =
                [ ( [ "base64" ], ( mappingBase64, Nothing ) )
                , ( [ "bytes" ], mappingBytes )
                , ( [], mappingBytes )
                , ( [ "utf8" ], ( mappingUtf8, Nothing ) )

                -- , ( [ "gzip", "base64" ], ( mappingGZipBase64, Nothing ) )
                ]
            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact(Skip = "Fix formatter egde cases before enabling regression tests")]
    public async System.Threading.Tasks.Task Stable_configurations_from_remote_repositories()
    {
        IReadOnlyList<string> testCasesSourceDirectories =
            [
            "https://github.com/pine-vm/pine/tree/64f5258d6f110737df6f3c5aea83f118552803cb/implement/pine/Elm/elm-compiler/src",
            // "https://github.com/pine-vm/pine/tree/64f5258d6f110737df6f3c5aea83f118552803cb/implement/example-apps",
            "https://github.com/guida-lang/compiler/tree/8af68e3c4124f52a60ceb4b66193cc5b46962562/src",
            "https://github.com/Viir/bots/tree/977a1a6c63add836f979aa993ca7b6d844e3f969/implement",
            ];

        static bool AssumeIsElmModuleFile(
            IReadOnlyList<string> filePath)
        {
            var lastSegment = filePath[filePath.Count - 1];

            return lastSegment.EndsWith(".elm", StringComparison.OrdinalIgnoreCase);
        }

        foreach (var sourceDirectory in testCasesSourceDirectories)
        {
            try
            {
                var sourceFiles =
                    await GitCore.LoadFromUrl.LoadTreeContentsFromUrlAsync(sourceDirectory);

                var elmModuleFiles =
                    sourceFiles
                    .Where(filePathAndContent => AssumeIsElmModuleFile(filePathAndContent.Key))
                    .ToImmutableArray();

                Console.WriteLine(
                    "Found Elm module files from source directory: " +
                    sourceDirectory +
                    " (" +
                    elmModuleFiles.Length +
                    " .elm files)");

                foreach (var elmModuleFile in elmModuleFiles)
                {
                    try
                    {
                        var elmModuleText =
                            System.Text.Encoding.UTF8.GetString(elmModuleFile.Value.Span);

                        var formatted = FormatString(elmModuleText);

                        formatted.Trim().Should().Be(elmModuleText.Trim());
                    }
                    catch (Exception exFile)
                    {
                        throw new Exception(
                            "Failed in test case file: " + string.Join("/", elmModuleFile.Key) +
                            " from source directory: " + sourceDirectory, exFile);
                    }
                }
            }
            catch (Exception e)
            {
                throw new Exception(
                    "Failed for test case source: " + sourceDirectory, e);
            }
        }
    }
}
