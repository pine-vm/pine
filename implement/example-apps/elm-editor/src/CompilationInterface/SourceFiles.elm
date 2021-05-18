module CompilationInterface.SourceFiles exposing (..)

import Bytes
import Bytes.Encode


file____src_monarch_js : Bytes.Bytes
file____src_monarch_js =
    "The compiler replaces this value."
        |> Bytes.Encode.string
        |> Bytes.Encode.encode


file____default_app_elm_json : Bytes.Bytes
file____default_app_elm_json =
    "The compiler replaces this value."
        |> Bytes.Encode.string
        |> Bytes.Encode.encode


file____default_app_src_Main_elm : Bytes.Bytes
file____default_app_src_Main_elm =
    "The compiler replaces this value."
        |> Bytes.Encode.string
        |> Bytes.Encode.encode


file____static_favicon_svg : Bytes.Bytes
file____static_favicon_svg =
    "The compiler replaces this value."
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
