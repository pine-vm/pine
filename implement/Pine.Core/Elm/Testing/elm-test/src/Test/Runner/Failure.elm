module Test.Runner.Failure exposing (Reason(..), InvalidReason(..))

{-| The reason a test failed.

@docs Reason, InvalidReason

-}


{-| The reason a test failed.

Test runners can use this to provide nice output, e.g. by doing diffs on the
two parts of an `Expect.equal` failure.

-}
type Reason
    = Custom
    | Equality String String
    | Comparison String String
      -- Expected, actual, (index of problem, expected element, actual element)
    | ListDiff (List String) (List String)
      {- I don't think we need to show the diff twice with + and - reversed. Just show it after the main vertical bar.
         "Extra" and "missing" are relative to the actual value.
      -}
    | CollectionDiff
        { expected : String
        , actual : String
        , extra : List String
        , missing : List String
        }
    | TODO
    | Invalid InvalidReason


{-| The reason a test run was invalid.

Test runners should report these to the user in whatever format is appropriate.

-}
type InvalidReason
    = EmptyList
    | NonpositiveFuzzCount
    | InvalidFuzzer
    | BadDescription
    | DuplicatedName
    | DistributionInsufficient
    | DistributionBug
