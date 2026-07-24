using Xunit;

namespace Pine.Core.Tests.Elm.ElmSyntax.Avh4Format;

using static FormatTestHelper;

public class ElmFormatIssueRegressionTests
{
    [Fact]
    public void Issue_842_aligns_pipeline_with_single_line_case_argument()
    {
        var input =
            """"
            module Test exposing (..)


            v =
                seed |> fn |> gn (case sel of Just w -> w)
            """";

        AssertModuleTextFormatsToExpected(input, PipelineWithCaseArgumentExpected);
    }

    [Fact]
    public void Issue_842_aligns_pipeline_with_prebroken_case_argument()
    {
        var input =
            """"
            module Test exposing (..)


            v =
                seed |> fn |> gn (case sel of
                    Just w ->
                        w
                )
            """";

        AssertModuleTextFormatsToExpected(input, PipelineWithCaseArgumentExpected);
    }

    [Fact]
    public void Issues_634_and_825_flatten_parenthesized_else_if_in_one_pass()
    {
        var input =
            """"
            module Test exposing (..)


            f =
                if x then
                    y

                else
                    (if a then
                        b

                     else
                        c
                    )
            """";

        var expected =
            """"
            module Test exposing (..)


            f =
                if x then
                    y

                else if a then
                    b

                else
                    c
            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Issue_825_reorders_exports_after_normalizing_docs_type_exposing()
    {
        var input =
            """"
            module A exposing
                ( a
                , b
                , A(..)
                )

            {-|

            @docs A(..), a
            @docs b

            -}
            """";

        var expected =
            """"
            module A exposing
                ( A(..), a
                , b
                )

            {-|

            @docs A, a
            @docs b

            -}
            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Issue_825_removes_parentheses_and_negation_from_zero_literals()
    {
        var input =
            """"
            module Test exposing (..)


            decimal =
                -(0)


            hexadecimal =
                -(0x0)


            bareDecimal =
                -0


            bareHexadecimal =
                -0x0
            """";

        var expected =
            """"
            module Test exposing (..)


            decimal =
                0


            hexadecimal =
                0x0


            bareDecimal =
                0


            bareHexadecimal =
                0x0
            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    [Fact]
    public void Issue_825_removes_empty_docs_and_condenses_leading_blank_lines()
    {
        var input =
            """"
            module A exposing (a)

            {-|

            @docs

            @docs a

            -}


            a =
                1
            """";

        var expected =
            """"
            module A exposing (a)

            {-|

            @docs a

            -}


            a =
                1
            """";

        AssertModuleTextFormatsToExpected(input, expected);
    }

    private const string PipelineWithCaseArgumentExpected =
        """"
        module Test exposing (..)


        v =
            seed
                |> fn
                |> gn
                    (case sel of
                        Just w ->
                            w
                    )
        """";
}
