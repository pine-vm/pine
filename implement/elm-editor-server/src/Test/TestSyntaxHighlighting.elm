module Test.TestSyntaxHighlighting exposing
    ( RecordAlias
    , Route(..)
    , functionWithLetBlockAndLocalFunction
    , reference
    )

import Bytes
import Json.Decode exposing (keyValuePairs)


type Route
    = ApiRoute
    | StaticFileRoute String


type alias RecordAlias typeVar =
    { primitiveField : Basics.Int
    , var : typeVar

    -- Single line comment
    }


functionWithLetBlockAndLocalFunction : Bytes.Bytes -> Int
functionWithLetBlockAndLocalFunction param =
    let
        {- Multi-line
           comment
            {- nested -}
        -}
        localFunction : Bytes.Bytes -> Int
        localFunction =
            Bytes.width
    in
    if (param |> localFunction) < 123 then
        "testing string literal" ++ """string literal

containing newline""" |> String.length

    else
        case "hello" of
            "specific case" ->
                1

            _ ->
                3


reference : Json.Decode.Decoder a -> Json.Decode.Decoder (List ( String, a ))
reference =
    keyValuePairs
