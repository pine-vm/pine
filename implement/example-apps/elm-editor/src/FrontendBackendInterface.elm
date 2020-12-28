module FrontendBackendInterface exposing (..)


type RequestStructure
    = ElmMakeRequest ElmMakeRequestStructure
    | FormatElmModuleTextRequest String
    | LoadCompositionRequest String


type ResponseStructure
    = ElmMakeResponse ElmMakeResponseStructure
    | FormatElmModuleTextResponse FormatElmModuleTextResponseStructure
    | LoadCompositionResponse LoadCompositionResponseStructure
    | ErrorResponse String


type alias ElmMakeRequestStructure =
    { commandLineArguments : String
    , files : List FileWithPath
    }


type alias ElmMakeResponseStructure =
    { processOutput : ProcessOutputStructure
    , files : List FileWithPath
    }


type alias FormatElmModuleTextResponseStructure =
    { processOutput : ProcessOutputStructure
    , formattedText : Maybe String
    }


type alias LoadCompositionResponseStructure =
    { compositionId : String
    , filesAsFlatList : List FileWithPath
    }


type alias FileWithPath =
    { path : List String
    , contentBase64 : String
    }


type alias ProcessOutputStructure =
    { standardError : String
    , standardOutput : String
    , exitCode : Int
    }
