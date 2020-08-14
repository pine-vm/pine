module FrontendWeb.MonacoEditor exposing (..)


type MessageToEditor
    = SetValue String


type MessageFromEditor
    = CompletedSetupEvent
    | DidChangeContentEvent String
