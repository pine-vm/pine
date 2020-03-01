module Main exposing (main)

import ElmFullstackLoweringInterface.GenerateJsonCoders as GenerateJsonCoders
import Json.Encode
import OpaqueCustomType


tests : String
tests =
    [ { testName = "serialize non-exposed tag of custom type"
      , expected = """{"int":4,"opaqueCustomType":{"TagA":{}}}"""
      , derived =
            { int = 4
            , opaqueCustomType = OpaqueCustomType.constructTagA
            }
                |> GenerateJsonCoders.encodeMixedRecord
                |> Json.Encode.encode 0
      }
    ]
        |> GenerateJsonCoders.testsValueToInterface
        |> Json.Encode.encode 0


{-| Support function-level dead code elimination (<https://elm-lang.org/blog/small-assets-without-the-headache>) Elm code needed to inform the Elm compiler about our entry points.
-}
main : Program Int {} String
main =
    Platform.worker
        { init = \_ -> ( {}, Cmd.none )
        , update =
            \_ _ ->
                ( tests |> always {}, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
