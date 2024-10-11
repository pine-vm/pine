module DecodeExtra exposing (..)

import Json.Decode


stringLength : Json.Decode.Decoder Int
stringLength =
    Json.Decode.map String.length Json.Decode.string


nullable : Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe a)
nullable decoder =
    Json.Decode.oneOf
        [ Json.Decode.null Nothing
        , Json.Decode.map Just decoder
        ]
