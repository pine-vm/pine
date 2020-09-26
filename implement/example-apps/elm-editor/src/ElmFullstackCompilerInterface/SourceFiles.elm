module ElmFullstackCompilerInterface.SourceFiles exposing (..)

import Bytes
import Bytes.Encode


file____src_monarch_js : Bytes.Bytes
file____src_monarch_js =
    "The Elm-fullstack compiler replaces this function."
        |> Bytes.Encode.string
        |> Bytes.Encode.encode


file____default_app_elm_json : Bytes.Bytes
file____default_app_elm_json =
    "The Elm-fullstack compiler replaces this function."
        |> Bytes.Encode.string
        |> Bytes.Encode.encode


file____default_app_src_Main_elm : Bytes.Bytes
file____default_app_src_Main_elm =
    "The Elm-fullstack compiler replaces this function."
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
