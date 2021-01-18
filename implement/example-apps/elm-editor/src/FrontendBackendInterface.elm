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
    { files : List FileWithPath
    , entryPointFilePath : List String
    }


type alias ElmMakeResponseStructure =
    { processOutput : ProcessOutputStructure
    , outputFileContentBase64 : Maybe String
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
