module FrontendBackendInterface exposing (..)


type RequestStructure
    = ElmMakeRequest ElmMakeRequestStructure
    | FormatElmModuleTextRequest String


type ResponseStructure
    = ElmMakeResponse ElmMakeResponseStructure
    | FormatElmModuleTextResponse FormatElmModuleTextResponseStructure


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


type alias FileWithPath =
    { path : List String
    , contentBase64 : String
    }


type alias ProcessOutputStructure =
    { standardError : String
    , standardOutput : String
    , exitCode : Int
    }
