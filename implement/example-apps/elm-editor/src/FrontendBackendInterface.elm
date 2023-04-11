module FrontendBackendInterface exposing (..)

import Bytes
import CompileElmApp
import FileTree


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
    , workingDirectoryPath : List String
    , entryPointFilePathFromWorkingDirectory : List String
    , makeOptionDebug : Bool
    , outputType : CompileElmApp.ElmMakeOutputType
    }


type alias ElmMakeResponseStructure =
    { processOutput : ProcessOutputStructure
    , outputFileContentBase64 : Maybe String
    , reportJsonProcessOutput : ProcessOutputStructure
    }


type alias FormatElmModuleTextResponseStructure =
    { processOutput : ProcessOutputStructure
    , formattedText : Maybe String
    }


type alias LoadCompositionResponseStructure =
    { compositionId : String
    , filesAsFlatList : List FileWithPath
    , urlInCommit : String
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


type alias FileTreeNode =
    FileTree.FileTreeNode Bytes.Bytes
