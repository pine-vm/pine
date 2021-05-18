module Common exposing (guideMarkdown)

import Bytes
import Bytes.Decode
import CompilationInterface.SourceFiles


guideMarkdown : String
guideMarkdown =
    CompilationInterface.SourceFiles.file____readme_md
        |> decodeBytesToString
        |> Maybe.withDefault "Failed to decode bytes to string"


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))
