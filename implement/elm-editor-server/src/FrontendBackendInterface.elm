module FrontendBackendInterface exposing (..)


type alias ElmMakeRequestStructure =
    { commandLineArguments : String
    , files : List FileWithPath
    }


type alias ElmMakeResponseStructure =
    { processOutput : ProcessOutputStructure
    , files : List FileWithPath
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
