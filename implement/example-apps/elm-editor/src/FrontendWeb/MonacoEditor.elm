module FrontendWeb.MonacoEditor exposing (..)


type MessageToEditor
    = SetValue String
    | SetModelMarkers (List EditorMarker)


type MessageFromEditor
    = CompletedSetupEvent
    | DidChangeContentEvent String
    | EditorActionCloseFileEvent
    | EditorActionFormatDocumentEvent
    | EditorActionCompileEvent


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
