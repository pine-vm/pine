module Test exposing
    ( Test, test
    , describe, concat, todo, skip, only
    , fuzz, fuzz2, fuzz3, fuzzWith, FuzzOptions
    , Distribution, noDistribution, reportDistribution, expectDistribution
    )

{-| A module containing functions for creating and managing tests.

@docs Test, test


## Organizing Tests

@docs describe, concat, todo, skip, only


## Fuzz Testing

@docs fuzz, fuzz2, fuzz3, fuzzWith, FuzzOptions
@docs Distribution, noDistribution, reportDistribution, expectDistribution

-}

import Expect exposing (Expectation)
import Fuzz exposing (Fuzzer)
import Set
import Test.Distribution exposing (ExpectedDistribution)
import Test.Distribution.Internal
import Test.Fuzz
import Test.Internal as Internal
import Test.Runner.Failure exposing (InvalidReason(..), Reason(..))


{-| A test which has yet to be evaluated. When evaluated, it produces one
or more [`Expectation`](../Expect#Expectation)s.

See [`test`](#test) and [`fuzz`](#fuzz) for some ways to create a `Test`.

-}
type alias Test =
    Internal.Test


{-| Run each of the given tests.

    concat [ testDecoder, testSorting ]

-}
concat : List Test -> Test
concat tests =
    if List.isEmpty tests then
        Internal.failNow
            { description = "This `concat` has no tests in it. Let's give it some!"
            , reason = Invalid EmptyList
            }

    else
        case Internal.duplicatedName tests of
            Err dups ->
                let
                    dupDescription duped =
                        "A test group contains multiple tests named '" ++ duped ++ "'. Do some renaming so that tests have unique names."
                in
                Internal.failNow
                    { description = String.join "\n" (List.map dupDescription <| Set.toList dups)
                    , reason = Invalid DuplicatedName
                    }

            Ok _ ->
                Internal.ElmTestVariant__Batch tests


{-| Apply a description to a list of tests.

    import Test exposing (describe, test, fuzz)
    import Fuzz exposing (int)
    import Expect


    describe "List"
        [ describe "reverse"
            [ test "has no effect on an empty list" <|
                \_ ->
                    List.reverse []
                        |> Expect.equal []
            , fuzz int "has no effect on a one-item list" <|
                \num ->
                     List.reverse [ num ]
                        |> Expect.equal [ num ]
            ]
        ]

Passing an empty list will result in a failing test, because you either made a
mistake or are creating a placeholder.

-}
describe : String -> List Test -> Test
describe untrimmedDesc tests =
    let
        desc =
            String.trim untrimmedDesc
    in
    if String.isEmpty desc then
        Internal.failNow
            { description = "This `describe` has a blank description. Let's give it a useful one!"
            , reason = Invalid BadDescription
            }

    else if List.isEmpty tests then
        Internal.PineEmptyGroup desc

    else
        case Internal.duplicatedName tests of
            Err dups ->
                let
                    dupDescription duped =
                        "Contains multiple tests named '" ++ duped ++ "'. Let's rename them so we know which is which."
                in
                Internal.ElmTestVariant__Labeled desc <|
                    Internal.failNow
                        { description = String.join "\n" (List.map dupDescription <| Set.toList dups)
                        , reason = Invalid DuplicatedName
                        }

            Ok childrenNames ->
                if Set.member desc childrenNames then
                    Internal.ElmTestVariant__Labeled desc <|
                        Internal.failNow
                            { description = "The test '" ++ desc ++ "' contains a child test of the same name. Let's rename them so we know which is which."
                            , reason = Invalid DuplicatedName
                            }

                else
                    Internal.ElmTestVariant__Labeled desc (Internal.ElmTestVariant__Batch tests)


{-| Return a [`Test`](#Test) that evaluates a single
[`Expectation`](../Expect#Expectation).

    import Test exposing (fuzz)
    import Expect


    test "the empty list has 0 length" <|
        \_ ->
            List.length []
                |> Expect.equal 0

-}
test : String -> (() -> Expectation) -> Test
test untrimmedDesc thunk =
    let
        desc =
            String.trim untrimmedDesc
    in
    if String.isEmpty desc then
        Internal.blankDescriptionFailure

    else
        Internal.ElmTestVariant__Labeled desc (Internal.ElmTestVariant__UnitTest (\() -> [ thunk () ]))


{-| Returns a [`Test`](#Test) that is "TODO" (not yet implemented). These tests
always fail, but test runners will only include them in their output if there
are no other failures.

These tests aren't meant to be committed to version control. Instead, use them
when you're brainstorming lots of tests you'd like to write, but you can't
implement them all at once. When you replace `todo` with a real test, you'll be
able to see if it fails without clutter from tests still not implemented. But,
unlike leaving yourself comments, you'll be prompted to implement these tests
because your suite will fail.

    describe "a new thing"
        [ todo "does what is expected in the common case"
        , todo "correctly handles an edge case I just thought of"
        ]

This functionality is similar to "pending" tests in other frameworks, except
that a TODO test is considered failing but a pending test often is not.

-}
todo : String -> Test
todo desc =
    Internal.PineTodo desc


{-| Returns a [`Test`](#Test) that causes other tests to be skipped, and
only runs the given one.

Calls to `only` aren't meant to be committed to version control. Instead, use
them when you want to focus on getting a particular subset of your tests to pass.
If you use `only`, your entire test suite will fail, even if
each of the individual tests pass. This is to help avoid accidentally
committing a `only` to version control.

If you you use `only` on multiple tests, only those tests will run. If you
put a `only` inside another `only`, only the outermost `only`
will affect which tests gets run.

See also [`skip`](#skip). Note that `skip` takes precedence over `only`;
if you use a `skip` inside an `only`, it will still get skipped, and if you use
an `only` inside a `skip`, it will also get skipped.

    describe "List"
        [ only <|
            describe "reverse"
                [ test "has no effect on an empty list" <|
                    \_ ->
                        List.reverse []
                            |> Expect.equal []
                , fuzz int "has no effect on a one-item list" <|
                    \num ->
                        List.reverse [ num ]
                            |> Expect.equal [ num ]
                ]
        , test "This will not get run, because of the `only` above!" <|
            \_ ->
                List.length []
                    |> Expect.equal 0
        ]

-}
only : Test -> Test
only =
    Internal.ElmTestVariant__Only


{-| Returns a [`Test`](#Test) that gets skipped.

Calls to `skip` aren't meant to be committed to version control. Instead, use
it when you want to focus on getting a particular subset of your tests to
pass. If you use `skip`, your entire test suite will fail, even if
each of the individual tests pass. This is to help avoid accidentally
committing a `skip` to version control.

See also [`only`](#only). Note that `skip` takes precedence over `only`;
if you use a `skip` inside an `only`, it will still get skipped, and if you use
an `only` inside a `skip`, it will also get skipped.

    describe "List"
        [ skip <|
            describe "reverse"
                [ test "has no effect on an empty list" <|
                    \_ ->
                        List.reverse []
                            |> Expect.equal []
                , fuzz int "has no effect on a one-item list" <|
                    \num ->
                        List.reverse [ num ]
                            |> Expect.equal [ num ]
                ]
        , test "This is the only test that will get run; the other was skipped!" <|
            \_ ->
                List.length []
                    |> Expect.equal 0
        ]

-}
skip : Test -> Test
skip =
    Internal.ElmTestVariant__Skipped


{-| Options [`fuzzWith`](#fuzzWith) accepts.


### `runs`

The number of times to run each fuzz test. (Default is 100.)

    import Test exposing (fuzzWith)
    import Fuzz exposing (list, int)
    import Expect

    fuzzWith { runs = 350, distribution = noDistribution }
        (list int)
        "List.length should never be negative" <|
        -- This anonymous function will be run 350 times, each time with a
        -- randomly-generated fuzzList value. (It will always be a list of ints
        -- because of (list int) above.)
        \fuzzList ->
            fuzzList
                |> List.length
                |> Expect.atLeast 0


### `distribution`

A way to report/enforce a statistical distribution of your input values.
(Default is `noDistribution`.)

    import Test exposing (fuzzWith)
    import Test.Distribution
    import Fuzz exposing (list, int)
    import Expect

    fuzzWith
        { runs = 350
        , distribution =
            expectDistribution
                [ ( Test.Distribution.zero, "empty", \xs -> List.length xs == 0 )
                , ( Test.Distribution.atLeast 10, "3+ items", \xs -> List.length xs >= 3 )
                ]
        }
        (list int)
        "Sum > Average"
    <|
        \xs ->
            List.sum xs
                |> Expect.greaterThan (average xs)

-}
type alias FuzzOptions a =
    { runs : Int
    , distribution : Distribution a
    }


{-| Run a [`fuzz`](#fuzz) test with the given [`FuzzOptions`](#FuzzOptions).

Note that there is no `fuzzWith2`, but you can always pass more fuzz values in
using [`Fuzz.pair`](Fuzz#pair), [`Fuzz.triple`](Fuzz#triple),
for example like this:

    import Test exposing (fuzzWith)
    import Fuzz exposing (pair, list, int)
    import Expect


    fuzzWith { runs = 4200, distribution = noDistribution }
        (pair (list int) int)
        "List.reverse never influences List.member" <|
            \(nums, target) ->
                List.member target (List.reverse nums)
                    |> Expect.equal (List.member target nums)

-}
fuzzWith : FuzzOptions a -> Fuzzer a -> String -> (a -> Expectation) -> Test
fuzzWith options fuzzer desc getTest =
    if options.runs < 1 then
        Internal.failNow
            { description = "Fuzz tests must have a run count of at least 1, not " ++ String.fromInt options.runs ++ "."
            , reason = Invalid NonpositiveFuzzCount
            }

    else
        fuzzWithHelp options (Test.Fuzz.fuzzTest options.distribution fuzzer desc getTest)


fuzzWithHelp : FuzzOptions a -> Test -> Test
fuzzWithHelp options aTest =
    case aTest of
        Internal.ElmTestVariant__UnitTest _ ->
            aTest

        Internal.PineTodo _ ->
            aTest

        Internal.PineEmptyGroup _ ->
            aTest

        Internal.ElmTestVariant__FuzzTest run ->
            Internal.PineFuzzCount options.runs aTest

        Internal.PineFuzzCount _ subTest ->
            Internal.PineFuzzCount options.runs subTest

        Internal.ElmTestVariant__Labeled label subTest ->
            Internal.ElmTestVariant__Labeled label (fuzzWithHelp options subTest)

        Internal.ElmTestVariant__Skipped subTest ->
            -- It's important to treat skipped tests exactly the same as normal,
            -- until after seed distribution has completed.
            fuzzWithHelp options subTest
                |> Internal.ElmTestVariant__Skipped

        Internal.ElmTestVariant__Only subTest ->
            fuzzWithHelp options subTest
                |> Internal.ElmTestVariant__Only

        Internal.ElmTestVariant__Batch tests ->
            tests
                |> List.map (fuzzWithHelp options)
                |> Internal.ElmTestVariant__Batch


{-| Take a function that produces a test, and calls it several (usually 100) times, using a randomly-generated input
from a [`Fuzzer`](http://package.elm-lang.org/packages/elm-explorations/test/latest/Fuzz) each time. This allows you to
test that a property that should always be true is indeed true under a wide variety of conditions. The function also
takes a string describing the test.

These are called "[fuzz tests](https://en.wikipedia.org/wiki/Fuzz_testing)" because of the randomness.
You may find them elsewhere called [property-based tests](http://blog.jessitron.com/2013/04/property-based-testing-what-is-it.html),
[generative tests](http://www.pivotaltracker.com/community/tracker-blog/generative-testing), or
[QuickCheck-style tests](https://en.wikipedia.org/wiki/QuickCheck).

    import Test exposing (fuzz)
    import Fuzz exposing (list, int)
    import Expect


    fuzz (list int) "List.length should never be negative" <|
        -- This anonymous function will be run 100 times, each time with a
        -- randomly-generated fuzzList value.
        \fuzzList ->
            fuzzList
                |> List.length
                |> Expect.atLeast 0

-}
fuzz :
    Fuzzer a
    -> String
    -> (a -> Expectation)
    -> Test
fuzz =
    Test.Fuzz.fuzzTest Test.Distribution.Internal.NoDistributionNeeded


{-| Run a [fuzz test](#fuzz) using two random inputs.

This is a convenience function that lets you skip calling [`Fuzz.pair`](Fuzz#pair).

See [`fuzzWith`](#fuzzWith) for an example of writing this using tuples.

    import Test exposing (fuzz2)
    import Fuzz exposing (list, int)


    fuzz2 (list int) int "List.reverse never influences List.member" <|
        \nums target ->
            List.member target (List.reverse nums)
                |> Expect.equal (List.member target nums)

-}
fuzz2 :
    Fuzzer a
    -> Fuzzer b
    -> String
    -> (a -> b -> Expectation)
    -> Test
fuzz2 fuzzA fuzzB desc =
    let
        fuzzer =
            Fuzz.pair fuzzA fuzzB
    in
    (\f ( a, b ) -> f a b) >> fuzz fuzzer desc


{-| Run a [fuzz test](#fuzz) using three random inputs.

This is a convenience function that lets you skip calling [`Fuzz.triple`](Fuzz#triple).

-}
fuzz3 :
    Fuzzer a
    -> Fuzzer b
    -> Fuzzer c
    -> String
    -> (a -> b -> c -> Expectation)
    -> Test
fuzz3 fuzzA fuzzB fuzzC desc =
    let
        fuzzer =
            Fuzz.triple fuzzA fuzzB fuzzC
    in
    uncurry3 >> fuzz fuzzer desc



-- Distribution --


{-| With `Distribution` you can observe statistics about your fuzz test inputs and
assert that a given proportion of test cases belong to a given class.

  - `noDistribution` opts out of these checks.

  - `reportDistribution` will collect statistics and report them after the test
    runs (both when it passes and fails) and so is mostly useful as a temporary
    setting when creating your fuzzers and tests.

  - `expectDistribution` will collect statistics, but only report them (and fail
    the test) if the `ExpectedDistribution` is not met. Handy for checking your
    fuzzers are giving interesting and relevant inputs to your tests.

```elm
fuzzWith { runs = 10000, distribution = noDistribution }

fuzzWith
    { runs = 10000
    , distribution =
        reportDistribution
            [ ( "fizz", \n -> (n |> modBy 3) == 0 )
            , ( "buzz", \n -> (n |> modBy 5) == 0 )
            , ( "even", \n -> (n |> modBy 2) == 0 )
            , ( "odd", \n -> (n |> modBy 2) == 1 )
            ]
    }

fuzzWith
    { runs = 10000
    , distribution =
        expectDistribution
            [ ( Test.Distribution.atLeast 30, "fizz", \n -> (n |> modBy 3) == 0 )
            , ( Test.Distribution.atLeast 15, "buzz", \n -> (n |> modBy 5) == 0 )
            , ( Test.Distribution.moreThanZero, "fizz buzz", \n -> (n |> modBy 15) == 0 )
            , ( Test.Distribution.zero, "outside range", \n -> n < 1 || n > 20 )
            ]
    }
```

The `a` type variable in `Distribution a` is the same type as your fuzzed type.

For example, if you're fuzzing a String with `Fuzzer String` and want to see
distribution information for values produced by this fuzzer, you need to provide
`String -> Bool` functions to your `reportDistribution` or `expectDistribution` calls,
which will in turn produce a `Distribution String`.

-}
type alias Distribution a =
    Test.Distribution.Internal.Distribution a


{-| Opts out of the test input distribution checking.
-}
noDistribution : Distribution a
noDistribution =
    Test.Distribution.Internal.NoDistributionNeeded


{-| Collects statistics and reports them after the test runs (both when it passes
and fails).
-}
reportDistribution : List ( String, a -> Bool ) -> Distribution a
reportDistribution =
    Test.Distribution.Internal.ReportDistribution


{-| Collects statistics and makes sure the expected distribution is met.

Fails the test and reports the distribution if the expected distribution is not met.

Uses a statistical test to make sure the distribution doesn't pass or fail the
distribution by accident (a flaky test). Will run more tests than specified with the
`runs` config option if needed.

This has the consequence of running more tests the closer your expected distribution
is to the true distribution. You can thus minimize and speed up this
"making sure" process by requesting slightly less % of your distribution than
needed.

Currently the statistical test is tuned to allow a false positive/negative in
1 in every 10^9 tests.

-}
expectDistribution : List ( ExpectedDistribution, String, a -> Bool ) -> Distribution a
expectDistribution =
    Test.Distribution.Internal.ExpectDistribution



-- INTERNAL HELPERS --


uncurry3 : (a -> b -> c -> d) -> ( a, b, c ) -> d
uncurry3 fn ( a, b, c ) =
    fn a b c
