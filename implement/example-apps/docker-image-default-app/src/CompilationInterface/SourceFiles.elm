module CompilationInterface.SourceFiles exposing (..)

import Bytes
import Bytes.Encode


file____readme_md : Bytes.Bytes
file____readme_md =
    "The compiler replaces this value."
        |> Bytes.Encode.string
        |> Bytes.Encode.encode
