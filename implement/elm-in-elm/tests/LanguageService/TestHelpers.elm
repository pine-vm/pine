module LanguageService.TestHelpers exposing (addFile, atMarker, blob, completionRequest, emptyState, location, packageId, packageState, pointRequest, range, request, stateFromFiles, workspace)

import Base64
import Expect
import FileTree
import Frontend.MonacoEditor
import LanguageService
import LanguageServiceInterface exposing (ElmPackageVersionIdentifer(..), FileLocation(..), Request, Response)


emptyState : LanguageService.LanguageServiceState
emptyState =
    LanguageService.initLanguageServiceState []


blob : String -> LanguageServiceInterface.FileTreeBlobNode
blob text =
    { asBase64 = Base64.fromString text |> Maybe.withDefault ""
    , asText = Just text
    }


addFile : String -> String -> LanguageService.LanguageServiceState -> LanguageService.LanguageServiceState
addFile path text state =
    LanguageService.handleRequestInCurrentWorkspace
        (LanguageServiceInterface.AddWorkspaceFileRequest path (blob text))
        state
        |> Tuple.second


stateFromFiles : List ( String, String ) -> LanguageService.LanguageServiceState
stateFromFiles files =
    List.foldl (\( path, text ) -> addFile path text) emptyState files


workspace : List ( String, String ) -> LanguageServiceInterface.FileTreeNode
workspace files =
    List.foldl
        (\( path, text ) tree ->
            FileTree.setNodeAtPathInSortedFileTree
                ( String.split "/" path, FileTree.BlobNode (blob text) )
                tree
        )
        (FileTree.TreeNode [])
        files


request : Request -> LanguageService.LanguageServiceState -> Result String Response
request serviceRequest state =
    LanguageService.handleRequestInCurrentWorkspace serviceRequest state
        |> Tuple.first


pointRequest : String -> Int -> Int -> LanguageServiceInterface.ProvideHoverRequestStruct
pointRequest path row column =
    { fileLocation = WorkspaceFileLocation path
    , positionLineNumber = row
    , positionColumn = column
    }


completionRequest : String -> Int -> Int -> LanguageServiceInterface.ProvideCompletionItemsRequestStruct
completionRequest path row column =
    { filePathOpenedInEditor = path
    , cursorLineNumber = row
    , cursorColumn = column
    }


range : ( Int, Int ) -> ( Int, Int ) -> Frontend.MonacoEditor.MonacoRange
range ( startRow, startColumn ) ( endRow, endColumn ) =
    { startLineNumber = startRow
    , startColumn = startColumn
    , endLineNumber = endRow
    , endColumn = endColumn
    }


location : String -> ( Int, Int ) -> ( Int, Int ) -> LanguageServiceInterface.LocationInFile
location path start end =
    { fileLocation = WorkspaceFileLocation path
    , range = range start end
    }


atMarker : String -> (String -> Int -> Int -> Expect.Expectation) -> Expect.Expectation
atMarker markedText expectation =
    case String.indexes "<caret>" markedText of
        [ offset ] ->
            let
                linesBefore =
                    String.left offset markedText |> String.lines

                column =
                    linesBefore |> List.reverse |> List.head |> Maybe.withDefault "" |> String.length |> (+) 1
            in
            expectation
                (String.replace "<caret>" "" markedText)
                (List.length linesBefore)
                column

        _ ->
            Expect.fail "A language-service scenario must contain exactly one <caret> marker."


packageId : ElmPackageVersionIdentifer
packageId =
    ElmPackageVersion019Identifer "example/tools" "1.0.0"


packageState : LanguageService.LanguageServiceState
packageState =
    LanguageService.handleRequestInCurrentWorkspace
        (LanguageServiceInterface.AddElmPackageVersionRequest packageId
            [ ( [ "src", "Tools.elm" ]
              , blob "module Tools exposing (answer)\n\n{-| The package answer. -}\nanswer : Int\nanswer = 42\n"
              )
            ]
        )
        emptyState
        |> Tuple.second
