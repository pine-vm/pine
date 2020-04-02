module Main exposing (main, migrate)

import Json.Decode
import Json.Encode


migrate : String -> String
migrate =
    decodeState
        >> (\originalState -> originalState ++ (originalState |> String.length |> String.fromInt))
        >> encodeState



{- Temporary manual implementation of parts which are supplied by the framework in production:
   - Entry point (DCE)
   - JSON Coding
-}


main : Program Int {} String
main =
    Platform.worker
        { init = \_ -> ( {}, Cmd.none )
        , update =
            \_ _ ->
                ( migrate |> always {}, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }


decodeState : String -> String
decodeState =
    Json.Decode.decodeString Json.Decode.string
        >> Result.toMaybe
        >> Maybe.withDefault "Decoding error"


encodeState : String -> String
encodeState =
    Json.Encode.string >> Json.Encode.encode 0
