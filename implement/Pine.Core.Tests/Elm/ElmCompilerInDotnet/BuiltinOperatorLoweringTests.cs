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
            RenderCanonicalized(
                """
                module Test exposing (..)


                add : Int -> Int -> Int
                add left right =
                    Pine_builtin.int_add
                        [ left
                        , right
                        ]
                """));
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
            RenderCanonicalized(
                """
                module Test exposing (..)


                sub : Int -> Int -> Int
                sub left right =
                    Pine_builtin.int_add
                        [ left
                        , Pine_builtin.int_mul
                            [ -1
                            , right
                            ]
                        ]
                """));
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
            RenderCanonicalized(
                """
                module Test exposing (..)


                mul : Int -> Int -> Int
                mul left right =
                    Pine_builtin.int_mul
                        [ left
                        , right
                        ]
                """));
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
            RenderCanonicalized(
                """
                module Test exposing (..)


                eqInt : Int -> Int -> Bool
                eqInt left right =
                    Pine_builtin.equal
                        [ left
                        , right
                        ]
                """));
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
            RenderCanonicalized(
                """
                module Test exposing (..)


                eqString : String -> String -> Bool
                eqString left right =
                    Pine_builtin.equal
                        [ left
                        , right
                        ]
                """));
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
            RenderCanonicalized(
                """
                module Test exposing (..)


                eqString : String -> String -> Bool
                eqString left right =
                    Pine_builtin.equal
                        [ left
                        , right
                        ]
                """));
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
            RenderCanonicalized(
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
                            Test.charOrEnd sOffsetBytesInt sSrcBytes
                    in
                    if
                        Pine_builtin.equal
                            [ newOffset
                            , -1
                            ]
                    then
                        Basics.True

                    else if
                        Pine_builtin.equal
                            [ newOffset
                            , -2
                            ]
                    then
                        Basics.False

                    else
                        Basics.True
                """));
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
            RenderCanonicalized(
                """
                module Test exposing (..)


                mapNode : (Int -> Int -> Int -> Int -> ( Int, Int )) -> ( Int, Int )
                mapNode nodeBuilder =
                    nodeBuilder 11 13 17 19


                buildBounds : ( Int, Int )
                buildBounds =
                    Test.mapNode
                        (\startRow startColumn endRow endColumn ->
                            ( Pine_builtin.int_add
                                [ startColumn
                                , Pine_builtin.int_mul
                                    [ -1
                                    , 1
                                    ]
                                ]
                            , Pine_builtin.int_add
                                [ endColumn
                                , 1
                                ]
                            )
                        )
                """));
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
            RenderCanonicalized(
                """
                module Test exposing (..)


                isLe : Int -> Int -> Bool
                isLe left right =
                    Pine_builtin.int_is_sorted_asc
                        [ left
                        , right
                        ]
                """));
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
            RenderCanonicalized(
                """
                module Test exposing (..)


                isGe : Int -> Int -> Bool
                isGe left right =
                    Pine_builtin.int_is_sorted_asc
                        [ right
                        , left
                        ]
                """));
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
            RenderCanonicalized(
                """
                module Test exposing (..)


                isLt : Int -> Int -> Bool
                isLt left right =
                    Pine_builtin.int_is_sorted_asc
                        [ Pine_builtin.int_add
                            [ left
                            , 1
                            ]
                        , right
                        ]
                """));
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
            RenderCanonicalized(
                """
                module Test exposing (..)


                isLtLiteralLeft : Int -> Bool
                isLtLiteralLeft right =
                    Pine_builtin.int_is_sorted_asc
                        [ 4
                        , right
                        ]
                """));
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
            RenderCanonicalized(
                """
                module Test exposing (..)


                isLtLiteralRight : Int -> Bool
                isLtLiteralRight left =
                    Pine_builtin.int_is_sorted_asc
                        [ left
                        , 4
                        ]
                """));
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
            RenderCanonicalized(
                """
                module Test exposing (..)


                isGt : Int -> Int -> Bool
                isGt left right =
                    Pine_builtin.int_is_sorted_asc
                        [ Pine_builtin.int_add
                            [ right
                            , 1
                            ]
                        , left
                        ]
                """));
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
            RenderCanonicalized(
                """
                module Test exposing (..)


                isGtLiteralLeft : Int -> Bool
                isGtLiteralLeft right =
                    Pine_builtin.int_is_sorted_asc
                        [ right
                        , 4
                        ]
                """));
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
            RenderCanonicalized(
                """
                module Test exposing (..)


                isGtLiteralRight : Int -> Bool
                isGtLiteralRight left =
                    Pine_builtin.int_is_sorted_asc
                        [ 4
                        , left
                        ]
                """));
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
            RenderCanonicalized(
                """
                module Test exposing (..)


                isChainedLe : Int -> Int -> Int -> Bool
                isChainedLe alfa beta gamma =
                    Pine_builtin.int_is_sorted_asc
                        [ alfa
                        , beta
                        , gamma
                        ]
                """));
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
            RenderCanonicalized(
                """
                module Test exposing (..)


                isChainedLt : Int -> Int -> Int -> Bool
                isChainedLt alfa beta gamma =
                    Pine_builtin.int_is_sorted_asc
                        [ Pine_builtin.int_add
                            [ alfa
                            , 1
                            ]
                        , beta
                        , Pine_builtin.int_add
                            [ beta
                            , 1
                            ]
                        , gamma
                        ]
                """));
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

        var loweredModules =
            BuiltinOperatorLowering.Apply(orderedCanonicalizedModules)
            .Extract(err => throw new Exception("Failed builtin operator lowering: " + err));

        return
            loweredModules
            .Single(kvp => kvp.Key.SequenceEqual(["Test"]))
            .Value;
    }

    private static string RenderCanonicalized(SyntaxTypes.File module) =>
        InliningTestHelper.CanonicalizeRenderedSnapshotText(
            InliningTestHelper.RenderModuleForSnapshotTests(module).Trim());

    private static string RenderCanonicalized(string moduleText) =>
        InliningTestHelper.CanonicalizeRenderedSnapshotText(moduleText.Trim());
}
