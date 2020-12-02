module ElmFullstackCompilerInterface.GenerateJsonCoders exposing (..)

import FrontendBackendInterface
import FrontendWeb.MonacoEditor
import Json.Decode
import Json.Encode
import ProjectState


jsonEncodeRequestStructure : FrontendBackendInterface.RequestStructure -> Json.Encode.Value
jsonEncodeRequestStructure =
    always (Json.Encode.string "The Elm-fullstack compiler replaces this function.")


jsonDecodeRequestStructure : Json.Decode.Decoder FrontendBackendInterface.RequestStructure
jsonDecodeRequestStructure =
    Json.Decode.fail "The Elm-fullstack compiler replaces this function."


jsonDecodeResponseStructure : Json.Decode.Decoder FrontendBackendInterface.ResponseStructure
jsonDecodeResponseStructure =
    Json.Decode.fail "The Elm-fullstack compiler replaces this function."


jsonEncodeMessageToMonacoEditor : FrontendWeb.MonacoEditor.MessageToEditor -> Json.Encode.Value
jsonEncodeMessageToMonacoEditor =
    always (Json.Encode.string "The Elm-fullstack compiler replaces this function.")


jsonDecodeMessageFromMonacoEditor : Json.Decode.Decoder FrontendWeb.MonacoEditor.MessageFromEditor
jsonDecodeMessageFromMonacoEditor =
    Json.Decode.fail "The Elm-fullstack compiler replaces this function."


jsonEncodeProjectState_2020_12 : ProjectState.ProjectState_2020_12 -> Json.Encode.Value
jsonEncodeProjectState_2020_12 =
    always (Json.Encode.string "The Elm-fullstack compiler replaces this function.")


jsonDecodeProjectState_2020_12 : Json.Decode.Decoder ProjectState.ProjectState_2020_12
jsonDecodeProjectState_2020_12 =
    Json.Decode.fail "The Elm-fullstack compiler replaces this function."
