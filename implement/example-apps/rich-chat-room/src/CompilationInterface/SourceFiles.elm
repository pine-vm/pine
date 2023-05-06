module CompilationInterface.SourceFiles exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/elm-time/elm-time/blob/main/guide/how-to-configure-and-deploy-an-elm-backend-app.md#compilationinterfacesourcefiles-elm-module>
-}

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
