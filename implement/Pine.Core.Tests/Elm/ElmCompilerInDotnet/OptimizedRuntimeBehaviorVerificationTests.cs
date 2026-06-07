using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using System.Collections.Generic;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class OptimizedRuntimeBehaviorVerificationTests
{
    [Fact]
    public void Passes_when_all_usages_match_and_no_optimization_opportunities_remain()
    {
        // A fully monomorphic, Int-only module. All operators bottom out in
        // `Pine_kernel.int_add`, leaving nothing for OptimizationOpportunityFinder
        // to flag.
        const string Source =
            """
            module Test exposing (..)


            doubleInt : Int -> Int
            doubleInt x =
                Pine_kernel.int_add [ x, x ]
            """;

        OptimizedRuntimeBehaviorVerification.VerifyOptimizedRuntimeBehavior(
            elmModuleTexts: [Source],
            entryPoints: [DeclQualifiedName.Create(["Test"], "doubleInt")],
            usages:
            [
            new KeyValuePair<string, string>(
                "doubleInt 7",
                "14"),
            new KeyValuePair<string, string>(
                "doubleInt 0",
                "0"),
            ]);
    }

    [Fact]
    public void Fails_when_actual_value_differs_from_expected_value()
    {
        const string Source =
            """
            module Test exposing (..)


            doubleInt : Int -> Int
            doubleInt x =
                Pine_kernel.int_add [ x, x ]
            """;

        var act =
            () =>
            OptimizedRuntimeBehaviorVerification.VerifyOptimizedRuntimeBehavior(
                elmModuleTexts: [Source],
                entryPoints: [DeclQualifiedName.Create(["Test"], "doubleInt")],
                usages:
                [
                // 2 * 7 = 14, not 99 — this should trip the correctness assertion.
                new KeyValuePair<string, string>(
                    "doubleInt 7",
                    "99"),
                ]);

        act.Should().Throw<System.Exception>();
    }
}
