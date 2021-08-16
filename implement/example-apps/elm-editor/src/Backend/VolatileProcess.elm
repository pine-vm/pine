module Backend.VolatileProcess exposing
    ( jsonDecodeResponseStructure
    , requestToVolatileProcess
    , volatileProcessProgramCode
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


requestToVolatileProcess : RequestStructure -> String
requestToVolatileProcess =
    jsonEncodeRequestStructure >> Json.Encode.encode 0


volatileProcessProgramCode : String
volatileProcessProgramCode =
    CompilationInterface.SourceFiles.file__utf8____src_Backend_VolatileProcess_csx
