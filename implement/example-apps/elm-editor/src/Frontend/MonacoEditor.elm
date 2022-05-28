module Frontend.MonacoEditor exposing (..)

{-| Types for exchanging messages with the Javascript wrapping the Monaco Editor.
-}


type MessageToEditor
    = SetValue String
    | SetModelMarkers (List EditorMarker)
    | RevealPositionInCenter { lineNumber : Int, column : Int }
    | ProvideCompletionItemsEvent (List MonacoCompletionItem)
    | ProvideHoverEvent (List String)


type MessageFromEditor
    = CompletedSetupEvent
    | DidChangeContentEvent String
    | DidFocusEditorWidgetEvent
    | EditorActionCloseEditorEvent
    | EditorActionFormatDocumentEvent
    | EditorActionCompileEvent
    | EditorActionInspectSyntaxEvent
    | RequestCompletionItemsEvent RequestCompletionItemsStruct
    | RequestHoverEvent RequestHoverStruct


type alias RequestCompletionItemsStruct =
    { textUntilPosition : String
    , cursorLineNumber : Int
    }


type alias RequestHoverStruct =
    { positionLineNumber : Int
    , positionColumn : Int
    , lineText : String
    , word : String
    }


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
