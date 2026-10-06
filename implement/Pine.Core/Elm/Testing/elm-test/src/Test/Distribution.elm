module Test.Distribution exposing
    ( ExpectedDistribution, atLeast, zero, moreThanZero
    , DistributionReport(..), distributionReportTable
    )

{-|


## Distribution

@docs ExpectedDistribution, atLeast, zero, moreThanZero
@docs DistributionReport, distributionReportTable

-}

import Dict exposing (Dict)
import Test.Distribution.Internal
import Test.Runner.Distribution


{-| Your input distribution requirement for the fuzzer used in a test.

For example, "this test shouldn't ever receive strings of length < 3 as an input"
or "at least 30% of the test input trees should be balanced".

-}
type alias ExpectedDistribution =
    Test.Distribution.Internal.ExpectedDistribution


{-| A requirement that a given value class should never happen in a given test.
-}
zero : ExpectedDistribution
zero =
    Test.Distribution.Internal.Zero


{-| A requirement that a given value class should happen at least once in a given
test.
-}
moreThanZero : ExpectedDistribution
moreThanZero =
    Test.Distribution.Internal.MoreThanZero


{-| A requirement that a given value class should happen at least N% of the time
in a given test.

The example below says that at least 30% of the fuzz test inputs should be
multiples of 3.

    fuzzWith
        { runs = 10000
        , distribution =
            expectDistribution
                [ ( atLeast 30, "multiple of 3", \n -> (n |> modBy 3) == 0 )
                ]
        }

-}
atLeast : Float -> ExpectedDistribution
atLeast =
    Test.Distribution.Internal.AtLeast


{-| A result of a distribution check.

Get it from your `Expectation` with `Test.Runner.getDistributionReport`.

-}
type DistributionReport
    = NoDistribution
    | DistributionToReport
        { distributionCount : Dict (List String) Int
        , runsElapsed : Int
        }
    | DistributionCheckSucceeded
        { distributionCount : Dict (List String) Int
        , runsElapsed : Int
        }
    | DistributionCheckFailed
        { distributionCount : Dict (List String) Int
        , runsElapsed : Int
        , badLabel : String
        , badLabelPercentage : Float
        , {- Would be great to return ExpectedDistribution here but it's defined (and
             used) in an internal module. The only way from this dependency cycle
             I can see involves exposing the constructors and having a duplicate
             definition of this type + conversion functions between them.
             ~janiczek
          -}
          expectedDistribution : String
        }


{-| Prettyprints the record inside `DistributionReport` into a table with histograms.
-}
distributionReportTable :
    { a
        | runsElapsed : Int
        , distributionCount : Dict (List String) Int
    }
    -> String
distributionReportTable r =
    Test.Runner.Distribution.formatTable r
