module Backend.VolatileHost exposing
    ( jsonDecodeResponseStructure
    , requestToVolatileHost
    , volatileHostScript
    )

import CompilationInterface.GenerateJsonCoders
import CompilationInterface.SourceFiles
import FrontendBackendInterface
import Json.Decode
import Json.Encode


type alias RequestStructure =
    FrontendBackendInterface.RequestStructure


type alias ResponseStructure =
    FrontendBackendInterface.ResponseStructure


jsonDecodeResponseStructure : Json.Decode.Decoder ResponseStructure
jsonDecodeResponseStructure =
    CompilationInterface.GenerateJsonCoders.jsonDecodeResponseStructure


jsonEncodeRequestStructure : RequestStructure -> Json.Encode.Value
jsonEncodeRequestStructure =
    CompilationInterface.GenerateJsonCoders.jsonEncodeRequestStructure


requestToVolatileHost : RequestStructure -> String
requestToVolatileHost =
    jsonEncodeRequestStructure >> Json.Encode.encode 0


volatileHostScript : String
volatileHostScript =
    CompilationInterface.SourceFiles.file__utf8____src_Backend_VolatileHost_csx
