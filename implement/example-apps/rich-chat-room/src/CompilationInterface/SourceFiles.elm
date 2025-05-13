{- For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/customizing-elm-app-builds-with-compilation-interfaces.md#compilationinterfacesourcefiles-elm-module> -}


module CompilationInterface.SourceFiles exposing (..)

import Bytes
import Bytes.Encode


file____README_md : { utf8 : String }
file____README_md =
    { utf8 = "The compiler replaces this value." }


file____static_chat_message_added_0_mp3 : { bytes : Bytes.Bytes }
file____static_chat_message_added_0_mp3 =
    { bytes =
        "The compiler replaces this value."
            |> Bytes.Encode.string
            |> Bytes.Encode.encode
    }
