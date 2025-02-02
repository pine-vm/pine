module CompileElmAppMain exposing (..)

import Bytes
import CompileBackendApp
import CompileElmApp
    exposing
        ( CompilationIterationSuccess
        , DependencyKey
        , ElmMakeEntryPointKind
        , EntryPointClass
        , LocatedCompilationError
        )


type CompilationIterationSuccess
    = CompilationIterationSuccess (List ( List String, Bytes.Bytes )) (Result String ElmMakeEntryPointKind)


asCompletelyLoweredElmApp :
    List ( List String, Bytes.Bytes )
    -> List ( DependencyKey, Bytes.Bytes )
    -> List String
    -> List String
    -> List String
    -> Result (List LocatedCompilationError) CompilationIterationSuccess
asCompletelyLoweredElmApp sourceFiles dependencies compilationRootFilePath interfaceToHostRootModuleName interfaceElmModuleNamePrefixes =
    case
        CompileElmApp.asCompletelyLoweredElmApp
            defaultEntryPoints
            { sourceFiles = sourceFiles
            , dependencies = dependencies
            , compilationRootFilePath = compilationRootFilePath
            , interfaceToHostRootModuleName = interfaceToHostRootModuleName
            , compilationInterfaceElmModuleNamePrefixes = interfaceElmModuleNamePrefixes
            }
    of
        Err err ->
            Err err

        Ok success ->
            Ok
                (CompilationIterationSuccess
                    success.compiledFiles
                    success.rootModuleEntryPointKind
                )


defaultEntryPoints : List EntryPointClass
defaultEntryPoints =
    List.concat
        [ CompileElmApp.defaultEntryPoints
        , CompileBackendApp.entryPoints
        ]
