module Main exposing (main)

import ElmFullstackCompilerInterface.GenerateJsonCoders as GenerateJsonCoders
import Json.Decode
import Json.Encode
import OpaqueCustomType
import Result.Extra
import Structures


tests : String
tests =
    [ { testName = "serialize non-exposed tag of custom type"
      , expected = """{"int":4,"opaqueCustomType":{"TagA":[]},"list_custom_type":[]}"""
      , derived =
            { int = 4
            , opaqueCustomType = OpaqueCustomType.constructTagA
            , list_custom_type = []
            }
                |> GenerateJsonCoders.encodeMixedRecord
                |> Json.Encode.encode 0
      }
    , { testName = "Custom type tag with one parameter"
      , expected = """{"int":4,"opaqueCustomType":{"TagA":[]},"list_custom_type":[{"CustomTagWithOneParameter":[13]}]}"""
      , derived =
            { int = 4
            , opaqueCustomType = OpaqueCustomType.constructTagA
            , list_custom_type = [ Structures.CustomTagWithOneParameter 13 ]
            }
                |> GenerateJsonCoders.encodeMixedRecord
                |> Json.Encode.encode 0
      }
    , { testName = "Custom type tag with two parameters"
      , expected = """{"int":4,"opaqueCustomType":{"TagA":[]},"list_custom_type":[{"CustomTagWithTwoParameters":["first arg",17]}]}"""
      , derived =
            { int = 4
            , opaqueCustomType = OpaqueCustomType.constructTagA
            , list_custom_type = [ Structures.CustomTagWithTwoParameters "first arg" 17 ]
            }
                |> GenerateJsonCoders.encodeMixedRecord
                |> Json.Encode.encode 0
      }
    , { testName = "Custom type tag with two parameters - roundtrip"
      , expected = """{"int":7,"opaqueCustomType":{"TagA":[]},"list_custom_type":[{"CustomTagWithTwoParameters":["first arg",19]}]}"""
      , derived =
            """{"int"   : 7 , "opaqueCustomType" : { "TagA" : []}, "list_custom_type":[{"CustomTagWithTwoParameters":["first arg",19]}]}"""
                |> Json.Decode.decodeString GenerateJsonCoders.decodeMixedRecord
                |> Result.map (GenerateJsonCoders.encodeMixedRecord >> Json.Encode.encode 0)
                |> Result.mapError Json.Decode.errorToString
                |> Result.Extra.merge
      }
    , { testName = "Maybe - Just roundtrip"
      , expected = """{"int":7,"opaqueCustomType":{"TagA":[]},"list_custom_type":[{"CustomTagWithMaybeInstance":[{"Just":[37]}]}]}"""
      , derived =
            """{"int" :     7, "opaqueCustomType":{"TagA":[]},"list_custom_type":[{"CustomTagWithMaybeInstance":[{"Just":[37]}]}]}"""
                |> Json.Decode.decodeString GenerateJsonCoders.decodeMixedRecord
                |> Result.map (GenerateJsonCoders.encodeMixedRecord >> Json.Encode.encode 0)
                |> Result.mapError Json.Decode.errorToString
                |> Result.Extra.merge
      }
    , { testName = "Maybe - Nothing roundtrip"
      , expected = """{"int":7,"opaqueCustomType":{"TagA":[]},"list_custom_type":[{"CustomTagWithMaybeInstance":[{"Nothing":[]}]}]}"""
      , derived =
            """{"int" :     7, "opaqueCustomType":{"TagA":[]},"list_custom_type":[{"CustomTagWithMaybeInstance":[{"Nothing":[]}]}]}"""
                |> Json.Decode.decodeString GenerateJsonCoders.decodeMixedRecord
                |> Result.map (GenerateJsonCoders.encodeMixedRecord >> Json.Encode.encode 0)
                |> Result.mapError Json.Decode.errorToString
                |> Result.Extra.merge
      }
    , { testName = "Result - Ok roundtrip"
      , expected = """{"int":7,"opaqueCustomType":{"TagA":[]},"list_custom_type":[{"CustomTagWithResultInstance":[{"Ok":[37]}]}]}"""
      , derived =
            """{"int" :     7, "opaqueCustomType":{"TagA":[]},"list_custom_type":[{"CustomTagWithResultInstance":[{"Ok":[37]}]}]}"""
                |> Json.Decode.decodeString GenerateJsonCoders.decodeMixedRecord
                |> Result.map (GenerateJsonCoders.encodeMixedRecord >> Json.Encode.encode 0)
                |> Result.mapError Json.Decode.errorToString
                |> Result.Extra.merge
      }
    , { testName = "Result - Err roundtrip"
      , expected = """{"int":7,"opaqueCustomType":{"TagA":[]},"list_custom_type":[{"CustomTagWithResultInstance":[{"Err":["error string"]}]}]}"""
      , derived =
            """{"int" :     7, "opaqueCustomType":{"TagA":[]},"list_custom_type":[{"CustomTagWithResultInstance":[{"Err":["error string"]}]}]}"""
                |> Json.Decode.decodeString GenerateJsonCoders.decodeMixedRecord
                |> Result.map (GenerateJsonCoders.encodeMixedRecord >> Json.Encode.encode 0)
                |> Result.mapError Json.Decode.errorToString
                |> Result.Extra.merge
      }
    , { testName = "Maybe - support easy migration from older format."

      -- 2020-03-07 Support easy migration of apps: Support decode from older JSON format for now. This case can be removed when the apps have been migrated.
      , expected = """{"int":7,"opaqueCustomType":{"TagA":[]},"list_custom_type":[{"CustomTagWithMaybeInstance":[{"Just":[37]}]}]}"""
      , derived =
            """{"int" :     7, "opaqueCustomType":{"TagA":[]},"list_custom_type":[{"CustomTagWithMaybeInstance":[{"Just":37}]}]}"""
                |> Json.Decode.decodeString GenerateJsonCoders.decodeMixedRecord
                |> Result.map (GenerateJsonCoders.encodeMixedRecord >> Json.Encode.encode 0)
                |> Result.mapError Json.Decode.errorToString
                |> Result.Extra.merge
      }
    , { testName = "Result - support easy migration from older format."

      -- 2020-03-07 Support easy migration of apps: Support decode from older JSON format for now. This case can be removed when the apps have been migrated.
      , expected = """{"int":7,"opaqueCustomType":{"TagA":[]},"list_custom_type":[{"CustomTagWithResultInstance":[{"Ok":[37]}]}]}"""
      , derived =
            """{"int" :     7, "opaqueCustomType":{"TagA":[]},"list_custom_type":[{"CustomTagWithResultInstance":[{"Ok":37}]}]}"""
                |> Json.Decode.decodeString GenerateJsonCoders.decodeMixedRecord
                |> Result.map (GenerateJsonCoders.encodeMixedRecord >> Json.Encode.encode 0)
                |> Result.mapError Json.Decode.errorToString
                |> Result.Extra.merge
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
