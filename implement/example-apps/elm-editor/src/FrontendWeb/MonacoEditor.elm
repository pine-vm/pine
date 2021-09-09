module FrontendWeb.MonacoEditor exposing (..)


type MessageToEditor
    = SetValue String
    | SetModelMarkers (List EditorMarker)
    | RevealPositionInCenter { lineNumber : Int, column : Int }
    | ProvideCompletionItemsEvent (List MonacoCompletionItem)


type MessageFromEditor
    = CompletedSetupEvent
    | DidChangeContentEvent String
    | EditorActionCloseEditorEvent
    | EditorActionFormatDocumentEvent
    | EditorActionCompileEvent
    | RequestCompletionItemsEvent RequestCompletionItemsStruct


type alias RequestCompletionItemsStruct =
    { textUntilPosition : String }


{-| <https://microsoft.github.io/monaco-editor/api/interfaces/monaco.editor.imarkerdata.html>
<https://github.com/microsoft/monaco-editor/issues/2042#issue-657909285>
-}
type alias EditorMarker =
    { message : String
    , startLineNumber : Int
    , startColumn : Int
    , endLineNumber : Int
    , endColumn : Int
    , severity : EditorMarkerSeverity
    }


type EditorMarkerSeverity
    = ErrorSeverity
    | WarningSeverity
    | InfoSeverity
    | HintSeverity


{-| <https://microsoft.github.io/monaco-editor/api/interfaces/monaco.languages.completionitem.html>
-}
type alias MonacoCompletionItem =
    { label : String
    , kind : CompletionItemKind
    , documentation : String
    , insertText : String
    }


{-| <https://microsoft.github.io/monaco-editor/api/enums/monaco.languages.completionitemkind.html>
-}
type CompletionItemKind
    = ConstructorCompletionItemKind
    | EnumCompletionItemKind
    | EnumMemberCompletionItemKind
    | FunctionCompletionItemKind
    | ModuleCompletionItemKind
    | StructCompletionItemKind
