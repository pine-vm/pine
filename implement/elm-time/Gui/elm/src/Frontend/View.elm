module Frontend.View exposing (..)

import Http


urlFromPath : List String -> String
urlFromPath =
    (++) pathCommonPrefix >> String.join "/" >> (++) "/"


pathCommonPrefix : List String
pathCommonPrefix =
    [ "api", "gui" ]


describeHttpError : Http.Error -> String
describeHttpError httpError =
    case httpError of
        Http.BadUrl errorMessage ->
            "Bad Url: " ++ errorMessage

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus statusCode ->
            "BadStatus: " ++ (statusCode |> String.fromInt)

        Http.BadBody errorMessage ->
            "BadPayload: " ++ errorMessage
