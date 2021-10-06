module CompilationInterface.ElmMake exposing (..)

import Bytes
import Bytes.Encode


elm_make__debug__javascript____src_Frontend_Main_elm : { base64 : String }
elm_make__debug__javascript____src_Frontend_Main_elm =
    { base64 = "The compiler replaces this value." }


elm_make__javascript____src_Frontend_Main_elm : { base64 : String, bytes : Bytes.Bytes }
elm_make__javascript____src_Frontend_Main_elm =
    { base64 = "The compiler replaces this value."
    , bytes = Bytes.Encode.encode (Bytes.Encode.string "The compiler replaces this value.")
    }
