using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet.Inlining;
using System;
using System.Linq;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class ElmCoreBasicsLoweringTests
{
    [Fact]
    public void Lowers_int_min_application()
    {
        var loweredModule =
            LowerElmCoreBasics(
                """
                module Test exposing (..)


                type alias Token =
                    { column : Int }


                branchIndent : Int -> Token -> Int
                branchIndent indentMin token =
                    min indentMin token.column
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            type alias Test.Token =
                { column : Int }


            Test.branchIndent : Int -> Test.Token -> Int
            Test.branchIndent indentMin token =
                if
                    Pine_builtin.int_is_sorted_asc
                        [ indentMin, token.column ]
                then
                    indentMin

                else
                    token.column
            """.Trim());
    }

    [Fact]
    public void Keeps_min_application_for_unknown_comparable_type()
    {
        var loweredModule =
            LowerElmCoreBasics(
                """
                module Test exposing (..)


                minimum : comparable -> comparable -> comparable
                minimum left right =
                    min left right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.minimum : comparable -> comparable -> comparable
            Test.minimum left right =
                Basics.min
                    left
                    right
            """.Trim());
    }

    [Fact]
    public void Lowers_int_max_application()
    {
        var loweredModule =
            LowerElmCoreBasics(
                """
                module Test exposing (..)


                maximum : Int -> Int -> Int
                maximum left right =
                    max left right
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.maximum : Int -> Int -> Int
            Test.maximum left right =
                if
                    Pine_builtin.int_is_sorted_asc
                        [ left, right ]
                then
                    right

                else
                    left
            """.Trim());
    }

    [Fact]
    public void Lowers_int_negate_application()
    {
        var loweredModule =
            LowerElmCoreBasics(
                """
                module Test exposing (..)


                negated : Int -> Int
                negated value =
                    negate value
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.negated : Int -> Int
            Test.negated value =
                Pine_builtin.int_mul
                    [ -1, value ]
            """.Trim());
    }

    [Fact]
    public void Lowers_int_abs_application()
    {
        var loweredModule =
            LowerElmCoreBasics(
                """
                module Test exposing (..)


                absolute : Int -> Int
                absolute value =
                    abs value
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.absolute : Int -> Int
            Test.absolute value =
                if
                    Pine_builtin.int_is_sorted_asc
                        [ 0, value ]
                then
                    value

                else
                    Pine_builtin.int_mul
                        [ -1, value ]
            """.Trim());
    }

    [Fact]
    public void Lowers_int_clamp_application()
    {
        var loweredModule =
            LowerElmCoreBasics(
                """
                module Test exposing (..)


                clamped : Int -> Int -> Int -> Int
                clamped low high value =
                    clamp low high value
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.clamped : Int -> Int -> Int -> Int
            Test.clamped low high value =
                if
                    Pine_builtin.int_is_sorted_asc
                        [ low, value ]
                then
                    if
                        Pine_builtin.int_is_sorted_asc
                            [ value, high ]
                    then
                        value

                    else
                        high

                else
                    low
            """.Trim());
    }

    [Fact]
    public void Keeps_basics_applications_for_float_type()
    {
        var loweredModule =
            LowerElmCoreBasics(
                """
                module Test exposing (..)


                maximum : Float -> Float -> Float
                maximum left right =
                    max left right


                negated : Float -> Float
                negated value =
                    negate value


                absolute : Float -> Float
                absolute value =
                    abs value


                clamped : Float -> Float -> Float -> Float
                clamped low high value =
                    clamp low high value
                """);

        RenderCanonicalized(loweredModule).Should().Be(
            """
            Test.absolute : Basics.Float -> Basics.Float
            Test.absolute value =
                Basics.abs
                    value


            Test.clamped : Basics.Float -> Basics.Float -> Basics.Float -> Basics.Float
            Test.clamped low high value =
                Basics.clamp
                    low
                    high
                    value


            Test.maximum : Basics.Float -> Basics.Float -> Basics.Float
            Test.maximum left right =
                Basics.max
                    left
                    right


            Test.negated : Basics.Float -> Basics.Float
            Test.negated value =
                Basics.negate
                    value
            """.Trim());
    }

    [Fact]
    public void Combined_configuration_lowers_core_basics_and_builtin_operators()
    {
        var loweredModule =
            LowerElmCoreBasics(
                """
                module Test exposing (..)


                minimumAfterIncrement : Int -> Int -> Int
                minimumAfterIncrement left right =
                    min (left + 1) right
                """,
                new BuiltinOperatorLowering.Configuration(
                    LowerBuiltinOperators: true,
                    LowerElmCoreBasics: true));

        var rendered = RenderCanonicalized(loweredModule);

        rendered.Should().Be(
            """
            Test.minimumAfterIncrement : Int -> Int -> Int
            Test.minimumAfterIncrement left right =
                if
                    Pine_builtin.int_is_sorted_asc
                        [ Pine_builtin.int_add
                            [ left, 1 ]
                        , right
                        ]
                then
                    Pine_builtin.int_add
                        [ left, 1 ]

                else
                    right
            """.Trim());
    }

    [Fact]
    public void Elm_core_basics_only_configuration_keeps_builtin_operators()
    {
        var loweredModule =
            LowerElmCoreBasics(
                """
                module Test exposing (..)


                minimumAfterIncrement : Int -> Int -> Int
                minimumAfterIncrement left right =
                    min (left + 1) right
                """,
                new BuiltinOperatorLowering.Configuration(
                    LowerBuiltinOperators: false,
                    LowerElmCoreBasics: true));

        var rendered = RenderCanonicalized(loweredModule);

        rendered.Should().Be(
            """"
            Test.minimumAfterIncrement : Int -> Int -> Int
            Test.minimumAfterIncrement left right =
                if
                    Pine_builtin.int_is_sorted_asc
                        [ Basics.add
                            left
                            1
                        , right
                        ]
                then
                    Basics.add
                        left
                        1

                else
                    right

            """".Trim());
    }

    [Fact]
    public void Builtin_operators_only_configuration_keeps_elm_core_basics()
    {
        var loweredModule =
            LowerElmCoreBasics(
                """
                module Test exposing (..)


                minimumAfterIncrement : Int -> Int -> Int
                minimumAfterIncrement left right =
                    min (left + 1) right
                """,
                new BuiltinOperatorLowering.Configuration(
                    LowerBuiltinOperators: true,
                    LowerElmCoreBasics: false));

        var rendered = RenderCanonicalized(loweredModule);

        rendered.Should().Be(
            """
            Test.minimumAfterIncrement : Int -> Int -> Int
            Test.minimumAfterIncrement left right =
                Basics.min
                    (Pine_builtin.int_add
                        [ left, 1 ]
                    )
                    right

            """.Trim());
    }

    private static SyntaxTypes.File LowerElmCoreBasics(
        string moduleText,
        BuiltinOperatorLowering.Configuration? configuration = null)
    {
        var parsedModule =
            ElmSyntaxParser.ParseModuleText(moduleText)
            .Extract(err => throw new Exception("Failed parsing: " + err));

        var parsedModules =
            new[] { SyntaxTypes.FromFullSyntaxModel.Convert(parsedModule) };

        var canonicalizedModules =
            Canonicalization.CanonicalizeOrThrow([parsedModule])
            .Extract(err => throw new Exception("Failed canonicalization: " + err));

        var orderedCanonicalizedModules =
            parsedModules
            .Select(
                module =>
                canonicalizedModules[SyntaxTypes.Module.GetModuleName(module.ModuleDefinition.Value).Value]
                .Extract(err => throw new Exception("Module has errors: " + err)))
            .ToList();

        var orderedCanonicalizedModulesAbstract =
            orderedCanonicalizedModules
            .Select(Core.Elm.ElmSyntax.ElmSyntaxAbstract.ConvertFromConcrete.FromFile)
            .ToList();

        var flatDecls = ElmCompiler.FlattenModulesToDeclarationDictionary(orderedCanonicalizedModulesAbstract);

        var loweredDecls =
            (configuration is null
            ?
            ElmCoreBasicsLowering.Apply(flatDecls)
            :
            BuiltinOperatorLowering.Apply(flatDecls, configuration))
            .Extract(err => throw new Exception("Failed Elm core Basics lowering: " + err));

        var loweredModules =
            ElmCompiler.ReconstructModulesFromFlatDict(
                loweredDecls,
                orderedCanonicalizedModulesAbstract);

        var moduleAbstract =
            loweredModules
            .Single(
                module =>
                Core.Elm.ElmSyntax.ElmSyntaxAbstract.Module.GetModuleName(module.ModuleDefinition)
                .SequenceEqual(["Test"]));

        return ElmSyntaxAbstractConversion.ToFile(moduleAbstract);
    }

    private static string RenderCanonicalized(SyntaxTypes.File module) =>
        InliningTestHelper.RenderModuleForSnapshotTests(module).Trim();
}
