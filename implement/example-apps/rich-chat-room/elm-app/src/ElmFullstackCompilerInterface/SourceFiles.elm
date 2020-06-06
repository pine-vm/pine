module ElmFullstackCompilerInterface.SourceFiles exposing (..)

import Bytes
import Bytes.Encode


file____readme_md : Bytes.Bytes
file____readme_md =
    "The Elm-fullstack compiler replaces this function."
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
