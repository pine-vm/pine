module ElmFullstackCompilerInterface.SourceFiles exposing (..)

import Bytes
import Bytes.Encode


file____readme_md : Bytes.Bytes
file____readme_md =
    "The Elm-fullstack compiler replaces this function."
        |> Bytes.Encode.string
        |> Bytes.Encode.encode


file____static_chat_message_added_0_mp3 : Bytes.Bytes
file____static_chat_message_added_0_mp3 =
    "The Elm-fullstack compiler replaces this function."
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
