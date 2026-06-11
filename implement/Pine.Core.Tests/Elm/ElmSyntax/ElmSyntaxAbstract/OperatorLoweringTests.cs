using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using System;
using System.Linq;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Tests.Elm.ElmSyntax.ElmSyntaxAbstract;

public class OperatorLoweringTests
{
    [Fact]
    public void Lowers_int_add_operator_application()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                add : Int -> Int -> Int
                add left right =
                    left + right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.add : Int -> Int -> Int
            Test.add left right =
                Pine_builtin.int_add
                    [ left, right ]
            """.Trim());
    }

    [Fact]
    public void Lowers_int_sub_operator_application()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                sub : Int -> Int -> Int
                sub left right =
                    left - right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.sub : Int -> Int -> Int
            Test.sub left right =
                Pine_builtin.int_add
                    [ left
                    , Pine_builtin.int_mul
                        [ -1, right ]
                    ]
            """.Trim());
    }

    [Fact]
    public void Lowers_int_mul_operator_application()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                mul : Int -> Int -> Int
                mul left right =
                    left * right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.mul : Int -> Int -> Int
            Test.mul left right =
                Pine_builtin.int_mul
                    [ left, right ]
            """.Trim());
    }

    [Fact]
    public void Lowers_number_mul_operator_application()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                mul : number -> number -> number
                mul left right =
                    left * right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.mul : number -> number -> number
            Test.mul left right =
                Basics.mul
                    left
                    right
            """.Trim());
    }

    [Fact]
    public void Lowers_eq_operator_application_for_Int_arguments()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                eqInt : Int -> Int -> Bool
                eqInt left right =
                    left == right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.eqInt : Int -> Int -> Bool
            Test.eqInt left right =
                Pine_builtin.equal
                    [ left, right ]
            """.Trim());
    }

    [Fact]
    public void Lowers_eq_operator_application_for_generic_arguments()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                alfa left right =
                    left == right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.alfa left right =
                Basics.eq
                    left
                    right
            """.Trim());
    }

    [Fact]
    public void Lowers_neq_operator_application_for_Int_arguments()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                neqInt : Int -> Int -> Bool
                neqInt left right =
                    left /= right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.neqInt : Int -> Int -> Bool
            Test.neqInt left right =
                if
                    Pine_builtin.equal
                        [ left, right ]
                then
                    Basics.False

                else
                    Basics.True
            """.Trim());
    }


    [Fact]
    public void Lowers_chained_int_lt_to_merged_int_is_sorted_asc()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                isChainedLt : Int -> Int -> Int -> Bool
                isChainedLt alfa beta gamma =
                    alfa < beta && beta < gamma
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.isChainedLt : Int -> Int -> Int -> Bool
            Test.isChainedLt alfa beta gamma =
                Pine_builtin.int_is_sorted_asc
                    [ Pine_builtin.int_add
                        [ alfa, 1 ]
                    , beta
                    , Pine_builtin.int_add
                        [ beta, 1 ]
                    , gamma
                    ]
            """.Trim());
    }

    [Fact]
    public void Lowers_Basics_add_and_sub_inside_lambda_from_expected_parameter_types()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                mapNode : (Int -> Int -> Int -> Int -> ( Int, Int )) -> ( Int, Int )
                mapNode nodeBuilder =
                    nodeBuilder 11 13 17 19


                buildBounds : ( Int, Int )
                buildBounds =
                    mapNode
                        (\startRow startColumn endRow endColumn ->
                            ( Basics.sub startColumn 1
                            , Basics.add endColumn 1
                            )
                        )
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.buildBounds : ( Int, Int )
            Test.buildBounds =
                Test.mapNode
                    (\startRow startColumn endRow endColumn ->
                        ( Pine_builtin.int_add
                            [ startColumn
                            , Pine_builtin.int_mul
                                [ -1, 1 ]
                            ]
                        , Pine_builtin.int_add
                            [ endColumn, 1 ]
                        )
                    )


            Test.mapNode : (Int -> Int -> Int -> Int -> ( Int, Int )) -> ( Int, Int )
            Test.mapNode nodeBuilder =
                nodeBuilder
                    11
                    13
                    17
                    19
            """.Trim());
    }


    // ============================================================
    // Lowering of `&&` / `||` / `Basics.and` / `Basics.or` to
    // short-circuiting `if-then-else` expressions.
    //
    // In Elm:
    //   `a && b`  ≡  `if a then b else False`
    //   `a || b`  ≡  `if a then True else b`
    //
    // Lowering them to `if-then-else` makes the short-circuiting evaluation
    // semantics explicit at the syntax level, removes a redundant builtin
    // dispatch, and gives downstream stages a single canonical form to
    // optimize (e.g. constant folding when the condition reduces to a literal
    // Bool).
    // ============================================================

    [Fact]
    public void Lowers_bool_and_operator_to_if_then_else()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                andBool : Bool -> Bool -> Bool
                andBool left right =
                    left && right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.andBool : Bool -> Bool -> Bool
            Test.andBool left right =
                if left then
                    right

                else
                    Basics.False
            """.Trim());
    }

    [Fact]
    public void Lowers_Basics_and_application_to_if_then_else()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                andBool : Bool -> Bool -> Bool
                andBool left right =
                    Basics.and left right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.andBool : Bool -> Bool -> Bool
            Test.andBool left right =
                if left then
                    right

                else
                    Basics.False
            """.Trim());
    }

    [Fact]
    public void Lowers_bool_or_operator_to_if_then_else()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                orBool : Bool -> Bool -> Bool
                orBool left right =
                    left || right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.orBool : Bool -> Bool -> Bool
            Test.orBool left right =
                if left then
                    Basics.True

                else
                    right
            """.Trim());
    }

    [Fact]
    public void Lowers_Basics_or_application_to_if_then_else()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                orBool : Bool -> Bool -> Bool
                orBool left right =
                    Basics.or left right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.orBool : Bool -> Bool -> Bool
            Test.orBool left right =
                if left then
                    Basics.True

                else
                    right
            """.Trim());
    }

    // ============================================================
    // Lowering of pipe operators `|>` / `<|` (canonicalized to
    // `Basics.apR` / `Basics.apL`) to plain function applications
    // where all arguments are given locally.
    //
    // In Elm:
    //   `x |> f`  ≡  `f x`
    //   `f <| x`  ≡  `f x`
    //
    // When the function operand is itself an application (e.g.
    // `x |> f a b`), the piped value is appended to the existing
    // argument list (`f a b x`), keeping all arguments local to a
    // single application. Nested pipelines lower to nested
    // applications.
    // ============================================================

    [Fact]
    public void Lowers_pipe_right_to_application()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                foo value =
                    value |> Other.foo
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.foo value =
                Other.foo
                    value
            """.Trim());
    }

    [Fact]
    public void Lowers_pipe_left_to_application()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                foo value =
                    Other.foo <| value
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.foo value =
                Other.foo
                    value
            """.Trim());
    }

    [Fact]
    public void Lowers_pipe_right_into_function_application_with_multiple_arguments()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                combine first second third =
                    first


                foo a b c =
                    c |> combine a b
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.combine first second third =
                first


            Test.foo a b c =
                Test.combine
                    a
                    b
                    c
            """.Trim());
    }

    [Fact]
    public void Lowers_pipe_left_into_function_application_with_multiple_arguments()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                combine first second third =
                    first


                foo a b c =
                    combine a b <| c
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.combine first second third =
                first


            Test.foo a b c =
                Test.combine
                    a
                    b
                    c
            """.Trim());
    }

    [Fact]
    public void Lowers_nested_pipe_right_pipeline_to_nested_applications()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                f x =
                    x


                g x =
                    x


                foo value =
                    value |> f |> g
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.f x =
                x


            Test.foo value =
                Test.g
                    (Test.f
                        value
                    )


            Test.g x =
                x
            """.Trim());
    }

    [Fact]
    public void Lowers_nested_pipe_right_pipeline_with_varying_argument_counts()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                toList x =
                    x


                prepend item list =
                    list


                foo item value =
                    value |> toList |> prepend item
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.foo item value =
                Test.prepend
                    item
                    (Test.toList
                        value
                    )


            Test.prepend item list =
                list


            Test.toList x =
                x
            """.Trim());
    }

    [Fact]
    public void Lowers_nested_pipe_left_pipeline_with_varying_argument_counts()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                toList x =
                    x


                prepend item list =
                    list


                foo item value =
                    prepend item <| toList <| value
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.foo item value =
                Test.prepend
                    item
                    (Test.toList
                        value
                    )


            Test.prepend item list =
                list


            Test.toList x =
                x
            """.Trim());
    }

    private static SyntaxTypes.File LowerOperators(string moduleText)
    {
        var parsedModule =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception("Failed parsing: " + err));

        var parsedModulesStil4mElmSyntax7 =
            new[] { Core.Elm.ElmSyntax.Stil4mElmSyntax7.FromFullSyntaxModel.Convert(parsedModule) };

        var canonicalizedModulesStil4mElmSyntax7 =
            Canonicalization.Canonicalize(parsedModulesStil4mElmSyntax7)
            .Extract(err => throw new Exception("Failed canonicalization: " + err));

        var canonicalizedModuleStil4mElmSyntax7 =
            canonicalizedModulesStil4mElmSyntax7.Single()
            .Value.Extract(err => throw new Exception("Failed extracting canonicalized module: " + err));

        var canonicalizedModule =
            SyntaxTypes.ConvertFromConcrete.FromFile(
                Core.Elm.ElmSyntax.Stil4mElmSyntax7.ToFullSyntaxModel.Convert(canonicalizedModuleStil4mElmSyntax7));

        var loweringConfig =
            new SyntaxTypes.OperatorLowering.Config(
                LowerPipes: true,
                LowerBasicsArithmeticOperators: true,
                LowerBasicsEqualityOperators: true,
                LowerBasicsComparisonOperators: true,
                LowerBasicsLogicalOperators: true);

        var operatorLoweredModule =
            SyntaxTypes.OperatorLowering.RewriteOperators(canonicalizedModule, loweringConfig);

        return operatorLoweredModule;
    }

    private static string RenderCanonicalized(SyntaxTypes.File module) =>
        ElmSyntaxAbstractTestHelper.RenderModuleForSnapshotTests(module).Trim();
}
