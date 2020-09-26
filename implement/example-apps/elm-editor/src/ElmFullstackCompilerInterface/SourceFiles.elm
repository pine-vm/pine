module ElmFullstackCompilerInterface.SourceFiles exposing (..)

import Bytes
import Bytes.Encode


file____src_monarch_js : Bytes.Bytes
file____src_monarch_js =
    "The Elm-fullstack compiler replaces this function."
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
