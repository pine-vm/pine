module CompilationInterface.GenerateJsonConverters exposing (..)

{-| For documentation of the compilation interface, see <https://github.com/elm-time/elm-time/blob/main/guide/how-to-configure-and-deploy-an-elm-backend-app.md#compilationinterfacegeneratejsonconverters-elm-module>
-}

import Frontend.MonacoEditor
import FrontendBackendInterface
import Json.Decode
import Json.Encode
import ProjectState_2021_01


jsonEncodeRequestStructure : FrontendBackendInterface.RequestStructure -> Json.Encode.Value
jsonEncodeRequestStructure =
    always (Json.Encode.string "The compiler replaces this declaration.")


jsonDecodeRequestStructure : Json.Decode.Decoder FrontendBackendInterface.RequestStructure
jsonDecodeRequestStructure =
    Json.Decode.fail "The compiler replaces this declaration."


jsonDecodeResponseStructure : Json.Decode.Decoder FrontendBackendInterface.ResponseStructure
jsonDecodeResponseStructure =
    Json.Decode.fail "The compiler replaces this declaration."


jsonEncodeMessageToMonacoEditor : Frontend.MonacoEditor.MessageToEditor -> Json.Encode.Value
jsonEncodeMessageToMonacoEditor =
    always (Json.Encode.string "The compiler replaces this declaration.")


jsonDecodeMessageFromMonacoEditor : Json.Decode.Decoder Frontend.MonacoEditor.MessageFromEditor
jsonDecodeMessageFromMonacoEditor =
    Json.Decode.fail "The compiler replaces this declaration."


jsonEncodeFileTreeNode : FrontendBackendInterface.FileTreeNode -> Json.Encode.Value
jsonEncodeFileTreeNode =
    always (Json.Encode.string "The compiler replaces this declaration.")


jsonDecodeFileTreeNode : Json.Decode.Decoder FrontendBackendInterface.FileTreeNode
jsonDecodeFileTreeNode =
    Json.Decode.fail "The compiler replaces this declaration."


jsonEncodeProjectState_2021_01 : ProjectState_2021_01.ProjectState -> Json.Encode.Value
jsonEncodeProjectState_2021_01 =
    always (Json.Encode.string "The compiler replaces this declaration.")


jsonDecodeProjectState_2021_01 : Json.Decode.Decoder ProjectState_2021_01.ProjectState
jsonDecodeProjectState_2021_01 =
    Json.Decode.fail "The compiler replaces this declaration."


jsonEncodeProjectStateDiff_2021_01 : ProjectState_2021_01.ProjectStateDifference -> Json.Encode.Value
jsonEncodeProjectStateDiff_2021_01 =
    always (Json.Encode.string "The compiler replaces this declaration.")


jsonDecodeProjectStateDiff_2021_01 : Json.Decode.Decoder ProjectState_2021_01.ProjectStateDifference
jsonDecodeProjectStateDiff_2021_01 =
    Json.Decode.fail "The compiler replaces this declaration."
