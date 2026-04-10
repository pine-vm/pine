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
