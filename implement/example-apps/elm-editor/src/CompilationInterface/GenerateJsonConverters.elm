{- For documentation of the compilation interface, see <https://github.com/pine-vm/pine/blob/main/guide/how-to-use-elm-compilation-interfaces.md#compilationinterfacegeneratejsonconverters-elm-module> -}


module CompilationInterface.GenerateJsonConverters exposing (..)

import Frontend.ContainerHtml
import Frontend.MonacoEditor
import FrontendBackendInterface
import Json.Decode
import Json.Encode
import LanguageServiceInterface
import WorkspaceState_2021_01
import CompilationInterface.GenerateJsonConverters.Generated_JsonConverters
import Dict
import Set
import Array
import Json.Decode
import Json.Encode
import Bytes
import Bytes.Decode
import Bytes.Encode
import FrontendBackendInterface
import Frontend.MonacoEditor
import Frontend.ContainerHtml
import FileTree
import WorkspaceState_2021_01
import LanguageServiceInterface


jsonEncodeRequestStructure : FrontendBackendInterface.RequestStructure -> Json.Encode.Value
jsonEncodeRequestStructure =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonEncode_2548601546


jsonDecodeRequestStructure : Json.Decode.Decoder FrontendBackendInterface.RequestStructure
jsonDecodeRequestStructure =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonDecode_2548601546


jsonDecodeResponseStructure : Json.Decode.Decoder FrontendBackendInterface.ResponseStructure
jsonDecodeResponseStructure =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonDecode_536068732


jsonEncodeMessageToMonacoEditor : Frontend.MonacoEditor.MessageToEditor -> Json.Encode.Value
jsonEncodeMessageToMonacoEditor =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonEncode_3939936410


jsonDecodeMessageFromMonacoEditor : Json.Decode.Decoder Frontend.MonacoEditor.MessageFromEditor
jsonDecodeMessageFromMonacoEditor =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonDecode_834299063


jsonDecodeMessageFromContainerHtml : Json.Decode.Decoder Frontend.ContainerHtml.Message
jsonDecodeMessageFromContainerHtml =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonDecode_360248148


jsonEncodeFileTreeNode : FrontendBackendInterface.FileTreeNode -> Json.Encode.Value
jsonEncodeFileTreeNode =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonEncode_2146051484


jsonDecodeFileTreeNode : Json.Decode.Decoder FrontendBackendInterface.FileTreeNode
jsonDecodeFileTreeNode =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonDecode_2146051484


jsonEncodeWorkspaceState_2021_01 : WorkspaceState_2021_01.WorkspaceState -> Json.Encode.Value
jsonEncodeWorkspaceState_2021_01 =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonEncode_2130805252


jsonDecodeWorkspaceState_2021_01 : Json.Decode.Decoder WorkspaceState_2021_01.WorkspaceState
jsonDecodeWorkspaceState_2021_01 =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonDecode_2130805252


jsonEncodeWorkspaceStateDiff_2021_01 : WorkspaceState_2021_01.WorkspaceStateDifference -> Json.Encode.Value
jsonEncodeWorkspaceStateDiff_2021_01 =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonEncode_500977338


jsonDecodeWorkspaceStateDiff_2021_01 : Json.Decode.Decoder WorkspaceState_2021_01.WorkspaceStateDifference
jsonDecodeWorkspaceStateDiff_2021_01 =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonDecode_500977338


jsonEncodeLanguageServiceRequestInWorkspace : LanguageServiceInterface.RequestInWorkspaceWithId -> Json.Encode.Value
jsonEncodeLanguageServiceRequestInWorkspace =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonEncode_423741305


jsonDecodeLanguageServiceRequestInWorkspace : Json.Decode.Decoder LanguageServiceInterface.RequestInWorkspaceWithId
jsonDecodeLanguageServiceRequestInWorkspace =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonDecode_423741305


jsonEncodeLanguageServiceResponse : LanguageServiceInterface.ResponseWithId -> Json.Encode.Value
jsonEncodeLanguageServiceResponse =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonEncode_3746757247


jsonDecodeLanguageServiceResponse : Json.Decode.Decoder LanguageServiceInterface.ResponseWithId
jsonDecodeLanguageServiceResponse =
    CompilationInterface.GenerateJsonConverters.Generated_JsonConverters.jsonDecode_3746757247
