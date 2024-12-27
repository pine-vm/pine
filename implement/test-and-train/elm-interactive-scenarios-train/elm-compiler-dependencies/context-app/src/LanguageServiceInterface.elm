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
    = AddWorkspaceFileRequest String FileTreeBlobNode
    | AddElmPackageVersionRequest ElmPackageVersionIdentifer (List ( List String, FileTreeBlobNode ))
    | DeleteWorkspaceFileRequest String
    | ProvideHoverRequest ProvideHoverRequestStruct
    | ProvideCompletionItemsRequest ProvideCompletionItemsRequestStruct
    | ProvideDefinitionRequest ProvideDefinitionRequestStruct
    | TextDocumentSymbolRequest String
    | TextDocumentReferencesRequest ProvideReferencesRequestStruct
    | TextDocumentRenameRequest RenameParams


type Response
    = WorkspaceSummaryResponse
    | ProvideHoverResponse (List String)
    | ProvideCompletionItemsResponse (List Frontend.MonacoEditor.MonacoCompletionItem)
    | ProvideDefinitionResponse (List LocationInFile)
    | TextDocumentSymbolResponse (List DocumentSymbol)
    | TextDocumentReferencesResponse (List LocationInFile)
    | TextDocumentRenameResponse WorkspaceEdit


type alias ProvideHoverRequestStruct =
    { fileLocation : FileLocation
    , positionLineNumber : Int
    , positionColumn : Int
    }


type alias ProvideCompletionItemsRequestStruct =
    { filePathOpenedInEditor : String
    , cursorLineNumber : Int
    , cursorColumn : Int
    }


type alias ProvideDefinitionRequestStruct =
    ProvideHoverRequestStruct


type alias ProvideReferencesRequestStruct =
    ProvideHoverRequestStruct


type alias RenameParams =
    { filePath : String
    , positionLineNumber : Int
    , positionColumn : Int
    , newName : String
    }


type alias LocationInFile =
    { fileLocation : FileLocation
    , range : Frontend.MonacoEditor.MonacoRange
    }


type DocumentSymbol
    = DocumentSymbol DocumentSymbolStruct


type alias DocumentSymbolStruct =
    { name : String
    , kind : SymbolKind
    , range : Frontend.MonacoEditor.MonacoRange
    , selectionRange : Frontend.MonacoEditor.MonacoRange
    , children : List DocumentSymbol
    }


type SymbolKind
    = SymbolKind_File
    | SymbolKind_Module
    | SymbolKind_Namespace
    | SymbolKind_Package
    | SymbolKind_Class
    | SymbolKind_Enum
    | SymbolKind_Interface
    | SymbolKind_Function
    | SymbolKind_Constant
    | SymbolKind_String
    | SymbolKind_Number
    | SymbolKind_Boolean
    | SymbolKind_Array
    | SymbolKind_EnumMember
    | SymbolKind_Struct


type alias WorkspaceEdit =
    List TextDocumentEdit


type alias TextDocumentEdit =
    { filePath : String
    , edits : List TextEdit
    }


type alias TextEdit =
    { range : Frontend.MonacoEditor.MonacoRange
    , newText : String
    }


type FileLocation
    = WorkspaceFileLocation String
    | ElmPackageFileLocation ElmPackageVersionIdentifer (List String)


type ElmPackageVersionIdentifer
    = ElmPackageVersion019Identifer
        -- Package name, like "elm/core"
        String
        -- version tag
        String
