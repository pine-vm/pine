module Frontend.MonacoEditor exposing (..)

{-| Types for exchanging messages with the Javascript wrapping the Monaco Editor.
-}


type MessageToEditor
    = OpenDocumentEvent OpenDocumentEventStruct
    | SetModelMarkers (List EditorMarker)
    | RevealPositionInCenter MonacoPosition
    | ProvideCompletionItemsEvent (List MonacoCompletionItem)
    | ProvideHoverEvent (List String)
      {-
         <https://microsoft.github.io/monaco-editor/typedoc/interfaces/languages.DefinitionProvider.html#provideDefinition.provideDefinition-1>
      -}
    | ProvideDefinitionEvent (List MonacoLocation)


type alias OpenDocumentEventStruct =
    { value : String
    , language : String
    , uri : String
    , position : MonacoPosition
    }


type MessageFromEditor
    = CompletedSetupEvent
    | DidChangeContentEvent DidChangeContentEventStruct
    | DidFocusEditorWidgetEvent
    | EditorActionCloseEditorEvent
    | EditorActionFormatDocumentEvent
    | EditorActionCompileEvent
    | EditorActionInspectSyntaxEvent
    | RequestCompletionItemsEvent RequestCompletionItemsStruct
    | RequestHoverEvent RequestHoverStruct
    | RequestDefinitionEvent RequestHoverStruct
    | OpenCodeEditorEvent OpenCodeEditorEventStruct


type alias DidChangeContentEventStruct =
    { textModelValue : String
    , uri : String
    }


type alias RequestCompletionItemsStruct =
    { uri : String
    , textUntilPosition : String
    , cursorLineNumber : Int
    }


type alias RequestHoverStruct =
    { uri : String
    , positionLineNumber : Int
    , positionColumn : Int
    , lineText : String
    , word : String
    }


type alias OpenCodeEditorEventStruct =
    { uri : String
    , position : MonacoPosition
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


type alias MonacoLocation =
    {-
       <https://microsoft.github.io/monaco-editor/typedoc/interfaces/languages.Location.html>
    -}
    { range : MonacoRange
    , uri : String
    }


type alias MonacoPosition =
    {-
       <https://microsoft.github.io/monaco-editor/typedoc/interfaces/IPosition.html>
    -}
    { lineNumber : Int
    , column : Int
    }


type alias MonacoRange =
    {-
       <https://microsoft.github.io/monaco-editor/typedoc/interfaces/IRange.html>
    -}
    { startLineNumber : Int
    , startColumn : Int
    , endLineNumber : Int
    , endColumn : Int
    }
