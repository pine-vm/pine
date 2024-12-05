{- For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/how-to-use-elm-compilation-interfaces.md#compilationinterfacegeneratejsonconverters-elm-module> -}


module CompilationInterface.GenerateJsonConverters exposing (..)

import Frontend.ContainerHtml
import Frontend.MonacoEditor
import FrontendBackendInterface
import Json.Decode
import Json.Encode
import LanguageServiceInterface
import WorkspaceState_2021_01


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


jsonDecodeMessageFromContainerHtml : Json.Decode.Decoder Frontend.ContainerHtml.Message
jsonDecodeMessageFromContainerHtml =
    Json.Decode.fail "The compiler replaces this declaration."


jsonEncodeFileTreeNode : FrontendBackendInterface.FileTreeNode -> Json.Encode.Value
jsonEncodeFileTreeNode =
    always (Json.Encode.string "The compiler replaces this declaration.")


jsonDecodeFileTreeNode : Json.Decode.Decoder FrontendBackendInterface.FileTreeNode
jsonDecodeFileTreeNode =
    Json.Decode.fail "The compiler replaces this declaration."


jsonEncodeWorkspaceState_2021_01 : WorkspaceState_2021_01.WorkspaceState -> Json.Encode.Value
jsonEncodeWorkspaceState_2021_01 =
    always (Json.Encode.string "The compiler replaces this declaration.")


jsonDecodeWorkspaceState_2021_01 : Json.Decode.Decoder WorkspaceState_2021_01.WorkspaceState
jsonDecodeWorkspaceState_2021_01 =
    Json.Decode.fail "The compiler replaces this declaration."


jsonEncodeWorkspaceStateDiff_2021_01 : WorkspaceState_2021_01.WorkspaceStateDifference -> Json.Encode.Value
jsonEncodeWorkspaceStateDiff_2021_01 =
    always (Json.Encode.string "The compiler replaces this declaration.")


jsonDecodeWorkspaceStateDiff_2021_01 : Json.Decode.Decoder WorkspaceState_2021_01.WorkspaceStateDifference
jsonDecodeWorkspaceStateDiff_2021_01 =
    Json.Decode.fail "The compiler replaces this declaration."


jsonEncodeLanguageServiceRequestInWorkspace : LanguageServiceInterface.RequestInWorkspaceWithId -> Json.Encode.Value
jsonEncodeLanguageServiceRequestInWorkspace =
    always (Json.Encode.string "The compiler replaces this declaration.")


jsonDecodeLanguageServiceRequestInWorkspace : Json.Decode.Decoder LanguageServiceInterface.RequestInWorkspaceWithId
jsonDecodeLanguageServiceRequestInWorkspace =
    Json.Decode.fail "The compiler replaces this declaration."


jsonEncodeLanguageServiceResponse : LanguageServiceInterface.ResponseWithId -> Json.Encode.Value
jsonEncodeLanguageServiceResponse =
    always (Json.Encode.string "The compiler replaces this declaration.")


jsonDecodeLanguageServiceResponse : Json.Decode.Decoder LanguageServiceInterface.ResponseWithId
jsonDecodeLanguageServiceResponse =
    Json.Decode.fail "The compiler replaces this declaration."
