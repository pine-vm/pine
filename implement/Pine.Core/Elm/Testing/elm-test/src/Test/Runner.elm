module Test.Runner exposing
    ( Runner, SeededRunners(..), fromTest
    , getFailureReason, isTodo
    , getDistributionReport
    , formatLabels
    , Simplifiable, fuzz, simplify
    )

{-| This is an "experts only" module that exposes functions needed to run and
display tests. A typical user will use an existing runner library for Node or
the browser, which is implemented using this interface. A list of these runners
can be found in the `README`.


## Runner

@docs Runner, SeededRunners, fromTest


## Expectations

@docs getFailureReason, isTodo


## Distribution

@docs getDistributionReport


## Formatting

@docs formatLabels


## Fuzzers

These functions give you the ability to run fuzzers separate of running fuzz tests.

@docs Simplifiable, fuzz, simplify

-}

import Bitwise
import Char
import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Fuzz.Internal
import GenResult exposing (GenResult(..))
import PRNG
import Random
import RandomRun exposing (RandomRun)
import Simplify
import String
import Test exposing (Test)
import Test.Distribution exposing (DistributionReport)
import Test.Expectation
import Test.Internal as Internal
import Test.Runner.Failure exposing (Reason(..))


{-| An unevaluated test.
-}
type Runnable
    = Thunk (() -> List Expectation) String String Int


{-| A function which, when evaluated, produces a list of expectations. Also a
list of labels which apply to this outcome.
-}
type alias Runner =
    { run : () -> List Expectation
    , labels : List String
    , kind : String
    , seedState : String
    , runs : Int
    }


{-| A structured test runner, incorporating:

  - The expectations to run
  - The hierarchy of description strings that describe the results

-}
type RunnableTree
    = Runnable Runnable
    | Labeled String RunnableTree


{-| Convert a `Test` into `SeededRunners`.

In order to run any fuzz tests that the `Test` may have, it requires a default run count as well
as an initial `Random.Seed`. `100` is a good run count. To obtain a good random seed, pass a
random 32-bit integer to `Random.initialSeed`. You can obtain such an integer by running
`Math.floor(Math.random()*0xFFFFFFFF)` in Node. It's typically fine to hard-code this value into
your Elm code; it's easy and makes your tests reproducible.

-}
fromTest : Int -> Random.Seed -> Test -> SeededRunners
fromTest runs seed test =
    if runs < 1 then
        Invalid ("Test runner run count must be at least 1, not " ++ String.fromInt runs)

    else
        let
            distribution =
                distributeSeeds runs seed test
        in
        if List.isEmpty distribution.only then
            if countAllRunnables distribution.skipped == 0 then
                distribution.all
                    |> List.concatMap fromRunnableTree
                    |> Plain

            else
                distribution.all
                    |> List.concatMap fromRunnableTree
                    |> Skipping

        else
            distribution.only
                |> List.concatMap fromRunnableTree
                |> Only


countAllRunnables : List RunnableTree -> Int
countAllRunnables =
    List.foldl (countRunnables >> (+)) 0


countRunnables : RunnableTree -> Int
countRunnables runnable =
    case runnable of
        Runnable _ ->
            1

        Labeled _ runner ->
            countRunnables runner


run : Runnable -> List Expectation
run (Thunk fn _ _ _) =
    case runThunk fn of
        Ok test ->
            test

        Err message ->
            [ Expect.fail ("This test failed because it threw an exception: \"" ++ message ++ "\"") ]


runThunk : (() -> a) -> Result String a
runThunk =
    \thunk -> Ok (thunk ())


fromRunnableTree : RunnableTree -> List Runner
fromRunnableTree =
    fromRunnableTreeHelp []


fromRunnableTreeHelp : List String -> RunnableTree -> List Runner
fromRunnableTreeHelp labels runner =
    case runner of
        Runnable ((Thunk _ kind seedState runs) as runnable) ->
            [ { labels = labels
              , run = \_ -> run runnable
              , kind = kind
              , seedState = seedState
              , runs = runs
              }
            ]

        Labeled label subRunner ->
            fromRunnableTreeHelp (label :: labels) subRunner


type alias Distribution =
    { seed : Random.Seed
    , only : List RunnableTree
    , all : List RunnableTree
    , skipped : List RunnableTree
    }


{-| Test Runners which have had seeds distributed to them, and which are now
either invalid or are ready to run. Seeded runners include some metadata:

  - `Invalid` runners had a problem (e.g. two sibling tests had the same description) making them un-runnable.
  - `Only` runners can be run, but `Test.only` was used somewhere, so ultimately they will lead to a failed test run even if each test that gets run passes.
  - `Skipping` runners can be run, but `Test.skip` was used somewhere, so ultimately they will lead to a failed test run even if each test that gets run passes.
  - `Plain` runners are ready to run, and have none of these issues.

-}
type SeededRunners
    = Plain (List Runner)
    | Only (List Runner)
    | Skipping (List Runner)
    | Invalid String


emptyDistribution : Random.Seed -> Distribution
emptyDistribution seed =
    { seed = seed
    , all = []
    , only = []
    , skipped = []
    }


{-| This breaks down a test into individual Runners, while assigning different
random number seeds to them. Along the way it also does a few other things:

1.  Collect any tests created with `Test.only` so later we can run only those.
2.  Collect any tests created with `Test.todo` so later we can fail the run.
3.  Validate that the run count is at least 1.

Some design notes:

1.  `only` tests and `skip` tests do not affect seed distribution. This is
    important for the case where a user runs tests, sees one failure, and decides
    to isolate it by using both `only` and providing the same seed as before. If
    `only` changes seed distribution, then that test result might not reproduce!
    This would be very frustrating, as it would mean you could reproduce the
    failure when not using `only`, but it magically disappeared as soon as you
    tried to isolate it. The same logic applies to `skip`.

2.  Theoretically this could become tail-recursive. However, the Labeled
    case would presumably become very gnarly, and it's unclear whether there would
    be a performance benefit or penalty in the end. If some brave soul wants to
    attempt it for kicks, beware that this is not a performance optimization for
    the faint of heart. Practically speaking, it seems unlikely to be worthwhile
    unless somehow people start seeing stack overflows during seed distribution -
    which would presumably require some absurdly deeply nested `describe` calls.

-}
distributeSeeds : Int -> Random.Seed -> Test -> Distribution
distributeSeeds =
    distributeSeedsHelp False


distributeSeedsHelp : Bool -> Int -> Random.Seed -> Test -> Distribution
distributeSeedsHelp hashed runs seed test =
    case test of
        Internal.ElmTestVariant__UnitTest aRun ->
            { seed = seed
            , all = [ Runnable (Thunk (\_ -> aRun ()) "unit" "" 0) ]
            , only = []
            , skipped = []
            }

        Internal.ElmTestVariant__FuzzTest aRun ->
            let
                ( firstSeed, nextSeed ) =
                    Random.step Random.independentSeed seed
            in
            { seed = nextSeed
            , all = [ Runnable (Thunk (\_ -> aRun firstSeed runs) "fuzz" (Debug.toString firstSeed) runs) ]
            , only = []
            , skipped = []
            }

        Internal.ElmTestVariant__Labeled description subTest ->
            -- This fixes https://github.com/elm-community/elm-test/issues/192
            -- The first time we hit a Labeled, we want to use the hash of
            -- that label, along with the original seed, as our starting
            -- point for distribution. Repeating this process more than
            -- once would be a waste.
            if hashed then
                let
                    next =
                        distributeSeedsHelp True runs seed subTest
                in
                { seed = next.seed
                , all = List.map (Labeled description) next.all
                , only = List.map (Labeled description) next.only
                , skipped = List.map (Labeled description) next.skipped
                }

            else
                let
                    intFromSeed =
                        -- At this point, this seed will be the original
                        -- one passed into distributeSeeds. We know this
                        -- because the only other branch that does a
                        -- Random.step on that seed is the Internal.Test
                        -- branch, and you can't have a Labeled inside a
                        -- Test, so that couldn't have come up yet.
                        seed
                            -- Convert the Seed back to an Int
                            |> Random.step (Random.int 0 Random.maxInt)
                            |> Tuple.first

                    hashedSeed =
                        description
                            -- Hash from String to Int
                            |> fnvHashString fnvInit
                            -- Incorporate the originally passed-in seed
                            |> fnvHash intFromSeed
                            -- Convert Int back to Seed
                            |> Random.initialSeed

                    next =
                        distributeSeedsHelp True runs hashedSeed subTest
                in
                -- Using seed instead of next.seed fixes https://github.com/elm-community/elm-test/issues/192
                -- by making it so that all the tests underneath this Label begin
                -- with the hashed seed, but subsequent sibling tests in this Batch
                -- get the same seed as before.
                { seed = seed
                , all = List.map (Labeled description) next.all
                , only = List.map (Labeled description) next.only
                , skipped = List.map (Labeled description) next.skipped
                }

        Internal.ElmTestVariant__Skipped subTest ->
            let
                -- Go through the motions in order to obtain the seed, but then
                -- move everything to skipped.
                next =
                    distributeSeedsHelp hashed runs seed subTest
            in
            { seed = next.seed
            , all = []
            , only = []
            , skipped = next.all
            }

        Internal.ElmTestVariant__Only subTest ->
            let
                next =
                    distributeSeedsHelp hashed runs seed subTest
            in
            -- `only` all the things!
            { next | only = next.all }

        Internal.ElmTestVariant__Batch tests ->
            List.foldl (batchDistribute hashed runs) (emptyDistribution seed) tests

        Internal.PineTodo description ->
            { seed = seed
            , all = [ Labeled description (Runnable (Thunk (\_ -> [ Expect.fail description ]) "todo" "" 0)) ]
            , only = []
            , skipped = []
            }

        Internal.PineFuzzCount count subTest ->
            distributeSeedsHelp hashed count seed subTest

        Internal.PineEmptyGroup description ->
            { seed = seed
            , all = [ Labeled description (Runnable (Thunk (\_ -> [ Expect.fail description ]) "empty" "" 0)) ]
            , only = []
            , skipped = []
            }


batchDistribute : Bool -> Int -> Test -> Distribution -> Distribution
batchDistribute hashed runs test prev =
    let
        next =
            distributeSeedsHelp hashed runs prev.seed test
    in
    { seed = next.seed
    , all = prev.all ++ next.all
    , only = prev.only ++ next.only
    , skipped = prev.skipped ++ next.skipped
    }


{-| FNV-1a initial hash value
-}
fnvInit : Int
fnvInit =
    2166136261


{-| FNV-1a helper for strings, using Char.toCode
-}
fnvHashString : Int -> String -> Int
fnvHashString hash str =
    str |> String.toList |> List.map Char.toCode |> List.foldl fnvHash hash


{-| FNV-1a implementation.
-}
fnvHash : Int -> Int -> Int
fnvHash a b =
    Bitwise.xor a b * 16777619 |> Bitwise.shiftRightZfBy 0


{-| Return `Nothing` if the given [`Expectation`](Expect#Expectation) is a [`pass`](Expect#pass).

If it is a [`fail`](Expect#fail), return a record containing the expectation
description, the [`Reason`](Test-Runner-Failure#Reason) the test failed, and the given inputs if
it was a fuzz test. (If it was not a fuzz test, the record's `given` field
will be `Nothing`).

For example:

    getFailureReason (Expect.equal 1 2)
    -- Just { reason = Equal 1 2, description = "Expect.equal", given = Nothing }

    getFailureReason (Expect.equal 1 1)
    -- Nothing

-}
getFailureReason :
    Expectation
    ->
        Maybe
            { given : Maybe String
            , description : String
            , reason : Reason
            }
getFailureReason expectation =
    case expectation of
        Test.Expectation.Pass _ ->
            Nothing

        Test.Expectation.Fail record ->
            Just
                { given = record.given
                , description = record.description
                , reason = record.reason
                }


{-| Returns a `DistributionReport` computed for a given test.
-}
getDistributionReport : Expectation -> DistributionReport
getDistributionReport expectation =
    case expectation of
        Test.Expectation.Pass { distributionReport } ->
            distributionReport

        Test.Expectation.Fail { distributionReport } ->
            distributionReport


{-| Determine if an expectation was created by a call to `Test.todo`. Runners
may treat these tests differently in their output.
-}
isTodo : Expectation -> Bool
isTodo expectation =
    case expectation of
        Test.Expectation.Pass _ ->
            False

        Test.Expectation.Fail { reason } ->
            reason == TODO


{-| A standard way to format descriptions and test labels, to keep things
consistent across test runner implementations.

The HTML, Node, String, and Log runners all use this.

What it does:

  - drop any labels that are empty strings
  - format the first label differently from the others
  - reverse the resulting list

Example:

    [ "the actual test that failed"
    , "nested description failure"
    , "top-level description failure"
    ]
    |> formatLabels ((++) "↓ ") ((++) "✗ ")

    {-
    [ "↓ top-level description failure"
    , "↓ nested description failure"
    , "✗ the actual test that failed"
    ]
    -}

-}
formatLabels :
    (String -> format)
    -> (String -> format)
    -> List String
    -> List format
formatLabels formatDescription formatTest labels =
    case List.filter (not << String.isEmpty) labels of
        [] ->
            []

        test :: descriptions ->
            descriptions
                |> List.map formatDescription
                |> (::) (formatTest test)
                |> List.reverse


{-| A `Simplifiable a` is an opaque type that allows you to obtain a value of type
`a` that is simpler than the one you've previously obtained.
-}
type Simplifiable a
    = Simplifiable
        { randomRun : RandomRun
        , fuzzer : Fuzzer a
        }


{-| Given a fuzzer, return a random generator to produce a value and a
Simplifiable. The value is what a fuzz test would have received as input.

Note that fuzzers aren't guaranteed to succeed, which is why this function returns
a Result. The String inside the Err case will contain a failure reason.

-}
fuzz : Fuzzer a -> Random.Generator (Result String ( a, Simplifiable a ))
fuzz fuzzer =
    Random.independentSeed
        |> Random.map
            (\seed ->
                case Fuzz.Internal.generate (PRNG.random seed) fuzzer of
                    Generated { value, prng } ->
                        Ok
                            ( value
                            , Simplifiable
                                { randomRun = PRNG.getRun prng
                                , fuzzer = fuzzer
                                }
                            )

                    Rejected { reason } ->
                        Err reason
            )


{-| Given a Simplifiable, simplify the value further. Pass your test function to
drive the simplification process: if a simplified value passes the test, it will
be discarded. In this sense, you will get the simplest value that still fails
your test.
-}
simplify : (a -> Expectation) -> ( a, Simplifiable a ) -> Maybe ( a, Simplifiable a )
simplify getExpectation ( value, Simplifiable { randomRun, fuzzer } ) =
    let
        ( newValue, newRandomRun, _ ) =
            Simplify.simplify
                { getExpectation = getExpectation
                , fuzzer = fuzzer
                , randomRun = randomRun
                , value = value
                , expectation = getExpectation value
                }
    in
    if RandomRun.equal newRandomRun randomRun then
        Nothing

    else
        Just
            ( newValue
            , Simplifiable
                { randomRun = newRandomRun
                , fuzzer = fuzzer
                }
            )
