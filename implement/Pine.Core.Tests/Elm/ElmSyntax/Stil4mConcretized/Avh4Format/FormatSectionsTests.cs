using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.Stil4mConcretized.Avh4Format;

using static FormatTestHelper;

public class FormatSectionsTests
{
    [Fact]
    public void Keep_line_breaks_around_top_level_section_singleline_comment()
    {
        /*
         * https://github.com/avh4/elm-format/blob/e7e5da37716acbfb4954a88128b5cc72b2c911d9/Style%20Guide/Sections.md
         * Rule from avh4/elm-format:
         * Comments (--, {- -}) can be used to create sections within your code.
         * */

        var input =
            """"
            module Test exposing (..)

            -- Test file for checking comments and spacing
            -- No detail


            x =
                ()



            -- This comment introduces a new section
            -- Mo detail


            a =
                []



            -- Another comment block
            -- More detail


            """";

        AssertModuleTextFormatsToItself(input);
    }

    [Fact]
    public void Add_line_breaks_around_top_level_section_singleline_comment()
    {
        /*
         * https://github.com/avh4/elm-format/blob/e7e5da37716acbfb4954a88128b5cc72b2c911d9/Style%20Guide/Sections.md
         * Rule from avh4/elm-format:
         * Comments (--, {- -}) can be used to create sections within your code.
         * */

        var input =
            """"
            module Test exposing (..)


            x =
                ()

            -- This comment introduces a new section

            a =
                ()

            """";


        var expected =
            """"
            module Test exposing (..)


            x =
                ()



            -- This comment introduces a new section


            a =
                ()

            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Trim_line_breaks_around_top_level_section_singleline_comment()
    {
        /*
         * https://github.com/avh4/elm-format/blob/e7e5da37716acbfb4954a88128b5cc72b2c911d9/Style%20Guide/Sections.md
         * Rule from avh4/elm-format:
         * Comments (--, {- -}) can be used to create sections within your code.
         * */

        var input =
            """"
            module Test exposing (..)


            x =
                ()





            -- This comment introduces a new section


            -- Mo detail






            a =
                ()

            """";


        var expected =
            """"
            module Test exposing (..)


            x =
                ()



            -- This comment introduces a new section
            -- Mo detail


            a =
                ()

            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Formats_stray_doc_comment_appearing_before_section_before_value_decl()
    {
        var input =
            """"
            module Test exposing (..)

            import Dict


            declA =
                13





            {-| indexedTraverse and indexedForA are defined on `Utils`
            -}

            -- VERIFIED/INDEXED ZIP


            declB =
                17

            """";


        var expected =
            """"
            module Test exposing (..)

            import Dict


            declA =
                13


            {-| indexedTraverse and indexedForA are defined on `Utils`
            -}



            -- VERIFIED/INDEXED ZIP


            declB =
                17

            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Formats_stray_doc_comment_appearing_before_section_before_type_decl()
    {
        var input =
            """"
            module Test exposing (..)

            import Dict


            declA =
                13






            {-| indexedTraverse and indexedForA are defined on `Utils`
            -}

            -- VERIFIED/INDEXED ZIP


            type RootLocation
                = LInside ModuleName.Raw
                | LOutside FilePath

            """";


        var expected =
            """"
            module Test exposing (..)

            import Dict


            declA =
                13


            {-| indexedTraverse and indexedForA are defined on `Utils`
            -}



            -- VERIFIED/INDEXED ZIP


            type RootLocation
                = LInside ModuleName.Raw
                | LOutside FilePath
            

            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }
}
