module CompilationInterface.SourceFiles exposing (..)

import Bytes
import Bytes.Encode


file____readme_md : { utf8 : String }
file____readme_md =
    { utf8 = "The compiler replaces this value." }


file____static_chat_message_added_0_mp3 : { bytes : Bytes.Bytes }
file____static_chat_message_added_0_mp3 =
    { bytes =
        "The compiler replaces this value."
            |> Bytes.Encode.string
            |> Bytes.Encode.encode
    }
