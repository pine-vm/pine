using AwesomeAssertions;
using Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public class CoreModulesCanonicalizationTests
{
    /// <summary>
    /// Tests that all implicitly exposed functions from Basics module are properly canonicalized.
    /// Covers all function names from the Basics module specification:
    /// - Comparison: max, min, compare
    /// - Boolean: not, xor
    /// - Math: modBy, remainderBy, negate, abs, clamp, sqrt, logBase, e, pi
    /// - Trigonometry: cos, sin, tan, acos, asin, atan, atan2, degrees, radians, turns
    /// - Conversion: toFloat, round, floor, ceiling, truncate, toPolar, fromPolar
    /// - Float checks: isNaN, isInfinite
    /// - Utility: identity, always, never
    /// </summary>
    [Fact]
    public void Expands_implicitly_exposed_functions_from_Basics()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            comparison_functions x =
                [ compare 13 x, max 1 2, min 1 2 ]


            boolean_functions x =
                [ not x, xor x x ]


            math_functions x =
                [ modBy 3 x, remainderBy 3 x, negate x, abs x, clamp 0 100 x, sqrt x, logBase 10 x, e, pi ]


            trig_functions x =
                [ cos x, sin x, tan x, acos x, asin x, atan x, atan2 x x, degrees x, radians x, turns x ]


            conversion_functions x =
                [ toFloat x, round x, floor x, ceiling x, truncate x, toPolar x, fromPolar x, isNaN x, isInfinite x ]


            utility_functions x =
                [ identity x, always 1 x ]

            """";

        var canonicalizedModule =
            ElmCompilerTestHelper.CanonicalizeAndGetSingleModule([elmModuleText], ["Test"]);

        var renderedAppModule =
            Core.Elm.ElmSyntax.Stil4mConcretized.Avh4Format.FormatToString(
                ToStil4mConcretized.ToConcretized(canonicalizedModule));

        renderedAppModule.Trim().Should().Be(
            """"
            module Test exposing (..)


            comparison_functions x =
                [ Basics.compare 13 x, Basics.max 1 2, Basics.min 1 2 ]


            boolean_functions x =
                [ Basics.not x, Basics.xor x x ]


            math_functions x =
                [ Basics.modBy 3 x, Basics.remainderBy 3 x, Basics.negate x, Basics.abs x, Basics.clamp 0 100 x, Basics.sqrt x, Basics.logBase 10 x, Basics.e, Basics.pi ]


            trig_functions x =
                [ Basics.cos x, Basics.sin x, Basics.tan x, Basics.acos x, Basics.asin x, Basics.atan x, Basics.atan2 x x, Basics.degrees x, Basics.radians x, Basics.turns x ]


            conversion_functions x =
                [ Basics.toFloat x, Basics.round x, Basics.floor x, Basics.ceiling x, Basics.truncate x, Basics.toPolar x, Basics.fromPolar x, Basics.isNaN x, Basics.isInfinite x ]


            utility_functions x =
                [ Basics.identity x, Basics.always 1 x ]

            """".Trim());
    }

    /// <summary>
    /// Tests that all types from the Basics module are properly canonicalized in type annotations.
    /// Covers: Int, Float, Bool, Order, Never
    /// </summary>
    [Fact]
    public void Expands_implicitly_exposed_types_from_Basics()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            intValue : Int
            intValue =
                42


            floatValue : Float
            floatValue =
                3.14


            boolValue : Bool
            boolValue =
                Basics.True


            orderValue : Order
            orderValue =
                Basics.EQ


            neverValue : Never -> Int
            neverValue n =
                Basics.never n

            """";

        var canonicalizedModule =
            ElmCompilerTestHelper.CanonicalizeAndGetSingleModule([elmModuleText], ["Test"]);

        var renderedAppModule =
            Core.Elm.ElmSyntax.Stil4mConcretized.Avh4Format.FormatToString(
                ToStil4mConcretized.ToConcretized(canonicalizedModule));

        renderedAppModule.Trim().Should().Be(
            """"
            module Test exposing (..)


            intValue : Basics.Int
            intValue =
                42


            floatValue : Basics.Float
            floatValue =
                3.14


            boolValue : Basics.Bool
            boolValue =
                Basics.True


            orderValue : Basics.Order
            orderValue =
                Basics.EQ


            neverValue : Basics.Never -> Basics.Int
            neverValue n =
                Basics.never n

            """".Trim());
    }

    /// <summary>
    /// Tests that choice type tags (constructors) from the Basics module are properly canonicalized.
    /// Covers Bool constructors (True, False) and Order constructors (LT, EQ, GT).
    /// Tests both constructor syntax (value expressions) and pattern match syntax.
    /// </summary>
    [Fact]
    public void Expands_implicitly_exposed_choice_type_tags_from_Basics()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            boolTrue =
                True


            boolFalse =
                False


            orderLT =
                LT


            orderEQ =
                EQ


            orderGT =
                GT


            matchBool x =
                case x of
                    True ->
                        1

                    False ->
                        0


            matchOrder x =
                case x of
                    LT ->
                        -1

                    EQ ->
                        0

                    GT ->
                        1

            """";

        var canonicalizedModule =
            ElmCompilerTestHelper.CanonicalizeAndGetSingleModule([elmModuleText], ["Test"]);

        var renderedAppModule =
            Core.Elm.ElmSyntax.Stil4mConcretized.Avh4Format.FormatToString(
                ToStil4mConcretized.ToConcretized(canonicalizedModule));

        renderedAppModule.Trim().Should().Be(
            """"
            module Test exposing (..)


            boolTrue =
                Basics.True


            boolFalse =
                Basics.False


            orderLT =
                Basics.LT


            orderEQ =
                Basics.EQ


            orderGT =
                Basics.GT


            matchBool x =
                case x of
                    Basics.True ->
                        1

                    Basics.False ->
                        0


            matchOrder x =
                case x of
                    Basics.LT ->
                        -1

                    Basics.EQ ->
                        0

                    Basics.GT ->
                        1

            """".Trim());
    }
}
