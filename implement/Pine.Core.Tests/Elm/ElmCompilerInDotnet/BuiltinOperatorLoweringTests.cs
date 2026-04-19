using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;
using System;
using System.Linq;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class BuiltinOperatorLoweringTests
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
    public void Lowers_eq_operator_application_for_String_arguments()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                eqString : String -> String -> Bool
                eqString left right =
                    left == right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.eqString : String -> String -> Bool
            Test.eqString left right =
                Pine_builtin.equal
                    [ left, right ]
            """.Trim());
    }

    [Fact]
    public void Lowers_Basics_eq_application_for_String_arguments()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                eqString : String -> String -> Bool
                eqString left right =
                    Basics.eq left right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.eqString : String -> String -> Bool
            Test.eqString left right =
                Pine_builtin.equal
                    [ left, right ]
            """.Trim());
    }

    [Fact]
    public void Lowers_Basics_eq_application_for_let_bound_Int_from_function_result()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                charOrEnd : Int -> String -> Int
                charOrEnd offset stringBytes =
                    offset


                isSentinel : Int -> String -> Bool
                isSentinel sOffsetBytes sSrcBytes =
                    let
                        sOffsetBytesInt : Int
                        sOffsetBytesInt =
                            sOffsetBytes

                        newOffset : Int
                        newOffset =
                            charOrEnd sOffsetBytesInt sSrcBytes
                    in
                    if Basics.eq newOffset -1 then
                        True

                    else if Basics.eq newOffset -2 then
                        False

                    else
                        True
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.charOrEnd : Int -> String -> Int
            Test.charOrEnd offset stringBytes =
                offset


            Test.isSentinel : Int -> String -> Bool
            Test.isSentinel sOffsetBytes sSrcBytes =
                let
                    sOffsetBytesInt : Int
                    sOffsetBytesInt =
                        sOffsetBytes

                    newOffset : Int
                    newOffset =
                        Test.charOrEnd
                            sOffsetBytesInt
                            sSrcBytes
                in
                if
                    Pine_builtin.equal
                        [ newOffset, -1 ]
                then
                    Basics.True

                else if
                    Pine_builtin.equal
                        [ newOffset, -2 ]
                then
                    Basics.False

                else
                    Basics.True
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

    [Fact]
    public void Lowers_int_le_operator_application()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                isLe : Int -> Int -> Bool
                isLe left right =
                    left <= right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.isLe : Int -> Int -> Bool
            Test.isLe left right =
                Pine_builtin.int_is_sorted_asc
                    [ left, right ]
            """.Trim());
    }

    [Fact]
    public void Lowers_int_ge_operator_application()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                isGe : Int -> Int -> Bool
                isGe left right =
                    left >= right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.isGe : Int -> Int -> Bool
            Test.isGe left right =
                Pine_builtin.int_is_sorted_asc
                    [ right, left ]
            """.Trim());
    }

    [Fact]
    public void Lowers_int_lt_operator_application()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                isLt : Int -> Int -> Bool
                isLt left right =
                    left < right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.isLt : Int -> Int -> Bool
            Test.isLt left right =
                Pine_builtin.int_is_sorted_asc
                    [ Pine_builtin.int_add
                        [ left, 1 ]
                    , right
                    ]
            """.Trim());
    }

    [Fact]
    public void Lowers_int_lt_with_literal_left_operand()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                isLtLiteralLeft : Int -> Bool
                isLtLiteralLeft right =
                    3 < right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.isLtLiteralLeft : Int -> Bool
            Test.isLtLiteralLeft right =
                Pine_builtin.int_is_sorted_asc
                    [ 4, right ]
            """.Trim());
    }

    [Fact]
    public void Lowers_int_lt_with_literal_right_operand()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                isLtLiteralRight : Int -> Bool
                isLtLiteralRight left =
                    left < 5
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.isLtLiteralRight : Int -> Bool
            Test.isLtLiteralRight left =
                Pine_builtin.int_is_sorted_asc
                    [ left, 4 ]
            """.Trim());
    }

    [Fact]
    public void Lowers_int_gt_operator_application()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                isGt : Int -> Int -> Bool
                isGt left right =
                    left > right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.isGt : Int -> Int -> Bool
            Test.isGt left right =
                Pine_builtin.int_is_sorted_asc
                    [ Pine_builtin.int_add
                        [ right, 1 ]
                    , left
                    ]
            """.Trim());
    }

    [Fact]
    public void Lowers_int_gt_with_literal_left_operand()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                isGtLiteralLeft : Int -> Bool
                isGtLiteralLeft right =
                    5 > right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.isGtLiteralLeft : Int -> Bool
            Test.isGtLiteralLeft right =
                Pine_builtin.int_is_sorted_asc
                    [ right, 4 ]
            """.Trim());
    }

    [Fact]
    public void Lowers_int_gt_with_literal_right_operand()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                isGtLiteralRight : Int -> Bool
                isGtLiteralRight left =
                    left > 3
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.isGtLiteralRight : Int -> Bool
            Test.isGtLiteralRight left =
                Pine_builtin.int_is_sorted_asc
                    [ 4, left ]
            """.Trim());
    }

    [Fact]
    public void Lowers_chained_int_le_to_merged_int_is_sorted_asc()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                isChainedLe : Int -> Int -> Int -> Bool
                isChainedLe alfa beta gamma =
                    alfa <= beta && beta <= gamma
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.isChainedLe : Int -> Int -> Int -> Bool
            Test.isChainedLe alfa beta gamma =
                Pine_builtin.int_is_sorted_asc
                    [ alfa, beta, gamma ]
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
    public void Lowers_eq_operator_application_for_Char_arguments()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                eqChar : Char -> Char -> Bool
                eqChar left right =
                    left == right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.eqChar : Char -> Char -> Bool
            Test.eqChar left right =
                Pine_builtin.equal
                    [ left, right ]
            """.Trim());
    }

    [Fact]
    public void Lowers_eq_operator_application_for_Bool_arguments()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                eqBool : Bool -> Bool -> Bool
                eqBool left right =
                    left == right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.eqBool : Bool -> Bool -> Bool
            Test.eqBool left right =
                Pine_builtin.equal
                    [ left, right ]
            """.Trim());
    }

    [Fact]
    public void Lowers_eq_operator_application_for_tuple_of_Int()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                eqTuple : ( Int, Int ) -> ( Int, Int ) -> Bool
                eqTuple left right =
                    left == right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.eqTuple : ( Int, Int ) -> ( Int, Int ) -> Bool
            Test.eqTuple left right =
                Pine_builtin.equal
                    [ left, right ]
            """.Trim());
    }

    [Fact]
    public void Lowers_eq_operator_application_for_nested_tuple_in_tuple()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                eqNestedTuple : ( ( Int, String ), ( Bool, Char ) ) -> ( ( Int, String ), ( Bool, Char ) ) -> Bool
                eqNestedTuple left right =
                    left == right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.eqNestedTuple : ( ( Int, String ), ( Bool, Char ) ) -> ( ( Int, String ), ( Bool, Char ) ) -> Bool
            Test.eqNestedTuple left right =
                Pine_builtin.equal
                    [ left, right ]
            """.Trim());
    }

    [Fact]
    public void Lowers_eq_operator_application_for_List_of_Int()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                eqListInt : List Int -> List Int -> Bool
                eqListInt left right =
                    left == right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.eqListInt : List.List Int -> List.List Int -> Bool
            Test.eqListInt left right =
                Pine_builtin.equal
                    [ left, right ]
            """.Trim());
    }

    [Fact]
    public void Lowers_eq_operator_application_for_choice_type_with_primitive_args()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                type Shape
                    = Circle Int
                    | Rectangle Int Int
                    | Empty


                eqShape : Shape -> Shape -> Bool
                eqShape left right =
                    left == right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            type Test.Shape
                = Circle Int
                | Rectangle Int Int
                | Empty


            Test.eqShape : Test.Shape -> Test.Shape -> Bool
            Test.eqShape left right =
                Pine_builtin.equal
                    [ left, right ]
            """.Trim());
    }

    [Fact]
    public void Does_not_lower_eq_for_unknown_type()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                eqGeneric : a -> a -> Bool
                eqGeneric left right =
                    left == right
                """);

        // Should NOT be lowered because type variable 'a' could contain Dict/Set
        var rendered = RenderCanonicalized(loweredModule);
        rendered.Should().NotContain("Pine_builtin.equal");
    }

    [Fact]
    public void Does_not_lower_eq_for_List_of_unknown_type()
    {
        var loweredModule =
            LowerOperators(
                """
                module Test exposing (..)


                eqListGeneric : List a -> List a -> Bool
                eqListGeneric left right =
                    left == right
                """);

        // Should NOT be lowered because list element type 'a' could contain Dict/Set
        var rendered = RenderCanonicalized(loweredModule);
        rendered.Should().NotContain("Pine_builtin.equal");
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

    private static SyntaxTypes.File LowerOperators(string moduleText)
    {
        var parsedModule =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception("Failed parsing: " + err));

        var parsedModules =
            new[] { SyntaxTypes.FromFullSyntaxModel.Convert(parsedModule) };

        var canonicalizedModules =
            Canonicalization.Canonicalize(parsedModules)
            .Extract(err => throw new Exception("Failed canonicalization: " + err));

        var orderedCanonicalizedModules =
            parsedModules
            .Select(
                module =>
                canonicalizedModules[SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value]
                .Extract(err => throw new Exception("Module has errors: " + err)))
            .ToList();

        var flatDecls = ElmCompiler.FlattenModulesToDeclarationDictionary(orderedCanonicalizedModules);

        var loweredDecls =
            BuiltinOperatorLowering.Apply(flatDecls)
            .Extract(err => throw new Exception("Failed builtin operator lowering: " + err));

        var loweredModules = ElmCompiler.ReconstructModulesFromFlatDict(loweredDecls, orderedCanonicalizedModules);

        return
            loweredModules
            .Single(m => SyntaxTypes.Module.GetModuleName(m.ModuleDefinition.Value).Value.SequenceEqual(["Test"]));
    }

    private static string RenderCanonicalized(SyntaxTypes.File module) =>
        InliningTestHelper.RenderModuleForSnapshotTests(module).Trim();
}
