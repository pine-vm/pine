module Test.Expectation exposing
    ( Expectation(..)
    , fail
    , withDistributionReport
    , withGiven
    , withFuzzDetails
    , FuzzDetails
    , withRunCounts
    )

import Test.Distribution exposing (DistributionReport(..))
import Test.Runner.Failure exposing (Reason)


type Expectation
    = Pass { distributionReport : DistributionReport, fuzzDetails : Maybe FuzzDetails }
    | Fail
        { given : Maybe String
        , description : String
        , reason : Reason
        , distributionReport : DistributionReport
        , fuzzDetails : Maybe FuzzDetails
        }

type alias FuzzDetails =
    { runsElapsed : Int
    , runsRequested : Int
    , failingIteration : Int
    , originalInput : String
    , shrunkInput : String
    , originalChoices : List Int
    , shrunkChoices : List Int
    }


withFuzzDetails : FuzzDetails -> Expectation -> Expectation
withFuzzDetails details expectation =
    case expectation of
        Pass pass ->
            Pass { pass | fuzzDetails = Just details }

        Fail failure ->
            Fail { failure | fuzzDetails = Just details }

withRunCounts : Int -> Int -> Expectation -> Expectation
withRunCounts elapsed requested expectation =
    let
        existing =
            case expectation of
                Pass pass ->
                    pass.fuzzDetails

                Fail failure ->
                    failure.fuzzDetails

        details =
            Maybe.withDefault
                { runsElapsed = 0
                , runsRequested = 0
                , failingIteration = 0
                , originalInput = ""
                , shrunkInput = ""
                , originalChoices = []
                , shrunkChoices = []
                }
                existing
    in
    withFuzzDetails { details | runsElapsed = elapsed, runsRequested = requested } expectation

{-| Create a failure without specifying the given.
-}
fail : { description : String, reason : Reason } -> Expectation
fail { description, reason } =
    Fail
        { given = Nothing
        , description = description
        , reason = reason
        , distributionReport = NoDistribution
        , fuzzDetails = Nothing
        }


{-| Set the given (fuzz test input) of an expectation.
-}
withGiven : String -> Expectation -> Expectation
withGiven newGiven expectation =
    case expectation of
        Fail failure ->
            Fail { failure | given = Just newGiven }

        Pass _ ->
            expectation


{-| Set the distribution report of an expectation.
-}
withDistributionReport : DistributionReport -> Expectation -> Expectation
withDistributionReport newDistributionReport expectation =
    case expectation of
        Fail failure ->
            Fail { failure | distributionReport = newDistributionReport }

        Pass pass ->
            Pass { pass | distributionReport = newDistributionReport }
