module Test.Internal exposing (Test(..), blankDescriptionFailure, duplicatedName, failNow, toString)

import Random
import Debug
import Set exposing (Set)
import Test.Expectation exposing (Expectation)
import Test.Runner.Failure exposing (InvalidReason(..), Reason(..))


{-| All variants of this type has the `ElmTestVariant__` prefix so that
node-test-runner can recognize them in the compiled JavaScript. This lets us
add more variants here without having to update the runner.

For more information, see <https://github.com/elm-explorations/test/pull/153>

-}
type Test
    = ElmTestVariant__UnitTest (() -> List Expectation)
    | ElmTestVariant__FuzzTest (Random.Seed -> Int -> List Expectation)
    | ElmTestVariant__Labeled String Test
    | ElmTestVariant__Skipped Test
    | ElmTestVariant__Only Test
    | ElmTestVariant__Batch (List Test)
    | PineTodo String
    | PineEmptyGroup String
    | PineFuzzCount Int Test


{-| Create a test that always fails for the given reason and description.
-}
failNow : { description : String, reason : Reason } -> Test
failNow record =
    ElmTestVariant__UnitTest
        (\() -> [ Test.Expectation.fail record ])


blankDescriptionFailure : Test
blankDescriptionFailure =
    failNow
        { description = "This test has a blank description. Let's give it a useful one!"
        , reason = Invalid BadDescription
        }


duplicatedName : List Test -> Result (Set String) (Set String)
duplicatedName tests =
    let
        names : Test -> List String
        names test =
            case test of
                ElmTestVariant__Labeled str _ ->
                    [ str ]

                ElmTestVariant__Batch subtests ->
                    List.concatMap names subtests

                ElmTestVariant__UnitTest _ ->
                    []

                ElmTestVariant__FuzzTest _ ->
                    []

                PineTodo str ->
                    [ str ]

                PineEmptyGroup str ->
                    [ str ]

                PineFuzzCount _ subTest ->
                    names subTest

                ElmTestVariant__Skipped subTest ->
                    names subTest

                ElmTestVariant__Only subTest ->
                    names subTest

        accumDuplicates : String -> ( Set String, Set String ) -> ( Set String, Set String )
        accumDuplicates newName ( dups, uniques ) =
            if Set.member newName uniques then
                ( Set.insert newName dups, uniques )

            else
                ( dups, Set.insert newName uniques )

        ( dupsAccum, uniquesAccum ) =
            List.concatMap names tests
                |> List.foldl accumDuplicates ( Set.empty, Set.empty )
    in
    if Set.isEmpty dupsAccum then
        Ok uniquesAccum

    else
        Err dupsAccum


toString : a -> String
toString =
    Debug.toString
