module FirCompiler2024Tests exposing (..)

import Dict
import Expect
import FirCompiler2024
import Set
import Test


computeRecursionDomainsTests : Test.Test
computeRecursionDomainsTests =
    let
        testCases =
            [ ( []
              , []
              )
            , ( [ ( "mut_a"
                  , Set.fromList [ "mut_a", "mut_b", "alfa" ]
                  )
                , ( "mut_b"
                  , Set.fromList [ "mut_a", "mut_b", "alfa" ]
                  )
                , ( "alfa"
                  , Set.fromList []
                  )
                , ( "beta"
                  , Set.fromList [ "alfa", "mut_a", "mut_b" ]
                  )
                ]
              , [ Set.fromList [ "alfa" ]
                , Set.fromList [ "mut_a", "mut_b" ]
                , Set.fromList [ "beta" ]
                ]
              )
            ]
    in
    Test.describe "Compute recursion domains"
        (List.indexedMap
            (\testCaseIndex ( input, expected ) ->
                Test.test ("Scenario " ++ String.fromInt testCaseIndex)
                    (\_ ->
                        Expect.equal
                            expected
                            (FirCompiler2024.recursionDomainsFromDeclarationDependencies input)
                    )
            )
            testCases
        )
