module LanguageServiceInterface exposing (..)

{-| These type declarations describe the interface to the language service.
We use this interface description to generate the functions to serialize and deserialize messages to and from the language service.
The serialization of messages enables communicating with an instance contained in a web worker or a remote machine.
-}

import FileTree
import Frontend.MonacoEditor


type alias RequestInWorkspaceWithId =
    { request : RequestInWorkspace
    , id : String
    }


type alias ResponseWithId =
    { response : Result String Response
    , requestId : String
    }


type alias FileTreeNode =
    FileTree.FileTreeNode FileTreeBlobNode


type alias FileTreeBlobNode =
    { asBase64 : String
    , asText : Maybe String
    }


type alias RequestInWorkspace =
    { workspace : FileTreeNode
    , request : Request
    }


type Request
    = AddFileRequest (List String) FileTreeBlobNode
    | DeleteFileRequest (List String)
    | ProvideHoverRequest ProvideHoverRequestStruct
    | ProvideCompletionItemsRequest ProvideCompletionItemsRequestStruct
    | ProvideDefinitionRequest ProvideHoverRequestStruct


type Response
    = WorkspaceSummaryResponse
    | ProvideHoverResponse (List String)
    | ProvideCompletionItemsResponse (List Frontend.MonacoEditor.MonacoCompletionItem)
    | ProvideDefinitionResponse (List LocationUnderFilePath)


type alias ProvideHoverRequestStruct =
    { filePathOpenedInEditor : List String
    , positionLineNumber : Int
    , positionColumn : Int
    }


type alias ProvideCompletionItemsRequestStruct =
    { filePathOpenedInEditor : List String
    , cursorLineNumber : Int
    , cursorColumn : Int
    }


type alias ProvideDefinitionRequestStruct =
    ProvideHoverRequestStruct


type alias LocationUnderFilePath =
    { filePath : List String
    , range : Frontend.MonacoEditor.MonacoRange
    }
