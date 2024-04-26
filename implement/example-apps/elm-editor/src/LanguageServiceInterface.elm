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
    }


type alias RequestInWorkspace =
    { workspace : FileTreeNode
    , request : Request
    }


type Request
    = ProvideHoverRequest ProvideHoverRequestStruct
    | ProvideCompletionItemsRequest ProvideCompletionItemsRequestStruct


type Response
    = ProvideHoverResponse (List String)
    | ProvideCompletionItemsResponse (List Frontend.MonacoEditor.MonacoCompletionItem)


type alias ProvideHoverRequestStruct =
    { filePathOpenedInEditor : List String
    , positionLineNumber : Int
    , positionColumn : Int
    , lineText : String
    }


type alias ProvideCompletionItemsRequestStruct =
    { filePathOpenedInEditor : List String
    , cursorLineNumber : Int
    , textUntilPosition : String
    }
