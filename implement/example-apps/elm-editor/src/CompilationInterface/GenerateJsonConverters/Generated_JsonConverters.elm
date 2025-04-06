module CompilationInterface.GenerateJsonConverters.Generated_JsonConverters exposing (..)

import Array
import Bytes
import Bytes.Decode
import Bytes.Encode
import CompilerGenerated.Base64 as Base64
import Dict
import FileTree
import Frontend.ContainerHtml
import Frontend.MonacoEditor
import FrontendBackendInterface
import Json.Decode
import Json.Encode
import LanguageServiceInterface
import Set
import WorkspaceState_2021_01


jsonEncode_2548601546 valueToEncode =
    jsonEncode_FrontendBackendInterface_RequestStructure valueToEncode


jsonDecode_2548601546 =
    jsonDecode_FrontendBackendInterface_RequestStructure


jsonEncode_536068732 valueToEncode =
    jsonEncode_FrontendBackendInterface_ResponseStructure valueToEncode


jsonDecode_536068732 =
    jsonDecode_FrontendBackendInterface_ResponseStructure


jsonEncode_3939936410 valueToEncode =
    jsonEncode_Frontend_MonacoEditor_MessageToEditor valueToEncode


jsonDecode_3939936410 =
    jsonDecode_Frontend_MonacoEditor_MessageToEditor


jsonEncode_834299063 valueToEncode =
    jsonEncode_Frontend_MonacoEditor_MessageFromEditor valueToEncode


jsonDecode_834299063 =
    jsonDecode_Frontend_MonacoEditor_MessageFromEditor


jsonEncode_360248148 valueToEncode =
    jsonEncode_Frontend_ContainerHtml_Message valueToEncode


jsonDecode_360248148 =
    jsonDecode_Frontend_ContainerHtml_Message


jsonEncode_2146051484 valueToEncode =
    jsonEncode_FileTree_FileTreeNode (\type_arg -> json_encode_Bytes type_arg) valueToEncode


jsonDecode_2146051484 =
    jsonDecode_FileTree_FileTreeNode json_decode_Bytes


jsonEncode_2130805252 valueToEncode =
    Json.Encode.object
        [ ( "base"
          , Json.Encode.string valueToEncode.base
          )
        , ( "differenceFromBase"
          , Json.Encode.object
                [ ( "removeNodes"
                  , jsonEncode__generic_List (\type_arg -> jsonEncode__generic_List (\type_arg_ -> Json.Encode.string type_arg_) type_arg) valueToEncode.differenceFromBase.removeNodes
                  )
                , ( "changeBlobs"
                  , jsonEncode__generic_List (\type_arg -> Json.Encode.list identity
                        [ jsonEncode__generic_List (\type_arg_ -> Json.Encode.string type_arg_) ((\( item_0, item_1 ) -> item_0) type_arg)
                        , jsonEncode__generic_List (\type_arg_ -> jsonEncode_WorkspaceState_2021_01_BlobChangeSequenceElement type_arg_) ((\( item_0, item_1 ) -> item_1) type_arg)
                        ]) valueToEncode.differenceFromBase.changeBlobs
                  )
                ]
          )
        ]


jsonDecode_2130805252 =
    Json.Decode.succeed (\base differenceFromBase -> { base = base, differenceFromBase = differenceFromBase })
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "base"
                [ "Base" ]
                Json.Decode.string
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "differenceFromBase"
                [ "DifferenceFromBase" ]
                ( Json.Decode.succeed (\removeNodes changeBlobs -> { removeNodes = removeNodes, changeBlobs = changeBlobs })
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "removeNodes"
                            [ "RemoveNodes" ]
                            ( jsonDecode__generic_List (jsonDecode__generic_List Json.Decode.string)
                            )
                        )
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "changeBlobs"
                            [ "ChangeBlobs" ]
                            ( jsonDecode__generic_List (Json.Decode.map2 (\item_0 item_1 -> ( item_0, item_1 ))
                                (Json.Decode.index 0 (jsonDecode__generic_List Json.Decode.string))
                                (Json.Decode.index 1 (jsonDecode__generic_List jsonDecode_WorkspaceState_2021_01_BlobChangeSequenceElement)))
                            )
                        )
                )
            )


jsonEncode_500977338 valueToEncode =
    Json.Encode.object
        [ ( "removeNodes"
          , jsonEncode__generic_List (\type_arg -> jsonEncode__generic_List (\type_arg_ -> Json.Encode.string type_arg_) type_arg) valueToEncode.removeNodes
          )
        , ( "changeBlobs"
          , jsonEncode__generic_List (\type_arg -> Json.Encode.list identity
                [ jsonEncode__generic_List (\type_arg_ -> Json.Encode.string type_arg_) ((\( item_0, item_1 ) -> item_0) type_arg)
                , jsonEncode__generic_List (\type_arg_ -> jsonEncode_WorkspaceState_2021_01_BlobChangeSequenceElement type_arg_) ((\( item_0, item_1 ) -> item_1) type_arg)
                ]) valueToEncode.changeBlobs
          )
        ]


jsonDecode_500977338 =
    Json.Decode.succeed (\removeNodes changeBlobs -> { removeNodes = removeNodes, changeBlobs = changeBlobs })
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "removeNodes"
                [ "RemoveNodes" ]
                ( jsonDecode__generic_List (jsonDecode__generic_List Json.Decode.string)
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "changeBlobs"
                [ "ChangeBlobs" ]
                ( jsonDecode__generic_List (Json.Decode.map2 (\item_0 item_1 -> ( item_0, item_1 ))
                    (Json.Decode.index 0 (jsonDecode__generic_List Json.Decode.string))
                    (Json.Decode.index 1 (jsonDecode__generic_List jsonDecode_WorkspaceState_2021_01_BlobChangeSequenceElement)))
                )
            )


jsonEncode_423741305 valueToEncode =
    Json.Encode.object
        [ ( "request"
          , Json.Encode.object
                [ ( "workspace"
                  , jsonEncode_FileTree_FileTreeNode (\type_arg -> Json.Encode.object
                        [ ( "asBase64"
                          , Json.Encode.string type_arg.asBase64
                          )
                        , ( "asText"
                          , jsonEncode__generic_Maybe (\type_arg_ -> Json.Encode.string type_arg_) type_arg.asText
                          )
                        ]) valueToEncode.request.workspace
                  )
                , ( "request"
                  , jsonEncode_LanguageServiceInterface_Request valueToEncode.request.request
                  )
                ]
          )
        , ( "id"
          , Json.Encode.string valueToEncode.id
          )
        ]


jsonDecode_423741305 =
    Json.Decode.succeed (\request id -> { request = request, id = id })
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "request"
                [ "Request" ]
                ( Json.Decode.succeed (\workspace request -> { workspace = workspace, request = request })
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "workspace"
                            [ "Workspace" ]
                            ( jsonDecode_FileTree_FileTreeNode (Json.Decode.succeed (\asBase64 asText -> { asBase64 = asBase64, asText = asText })
                                |> jsonDecode_andMap
                                    ( jsonDecode_field_withAlternateNames "asBase64"
                                        [ "AsBase64" ]
                                        Json.Decode.string
                                    )
                                |> jsonDecode_andMap
                                    ( jsonDecode_field_withAlternateNames "asText"
                                        [ "AsText" ]
                                        ( jsonDecode__generic_Maybe Json.Decode.string
                                        )
                                    ))
                            )
                        )
                    |> jsonDecode_andMap
                        ( jsonDecode_field_withAlternateNames "request"
                            [ "Request" ]
                            jsonDecode_LanguageServiceInterface_Request
                        )
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "id"
                [ "Id" ]
                Json.Decode.string
            )


jsonEncode_3746757247 valueToEncode =
    Json.Encode.object
        [ ( "response"
          , jsonEncode__generic_Result (\type_arg -> Json.Encode.string type_arg) (\type_arg -> jsonEncode_LanguageServiceInterface_Response type_arg) valueToEncode.response
          )
        , ( "requestId"
          , Json.Encode.string valueToEncode.requestId
          )
        ]


jsonDecode_3746757247 =
    Json.Decode.succeed (\response requestId -> { response = response, requestId = requestId })
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "response"
                [ "Response" ]
                ( jsonDecode__generic_Result Json.Decode.string jsonDecode_LanguageServiceInterface_Response
                )
            )
        |> jsonDecode_andMap
            ( jsonDecode_field_withAlternateNames "requestId"
                [ "RequestId" ]
                Json.Decode.string
            )


jsonEncode_FrontendBackendInterface_ElmMakeOutputType valueToEncode =
    case valueToEncode of
        FrontendBackendInterface.ElmMakeOutputTypeHtml ->
            Json.Encode.object [ ( "ElmMakeOutputTypeHtml", Json.Encode.list identity [] ) ]
        FrontendBackendInterface.ElmMakeOutputTypeJs ->
            Json.Encode.object [ ( "ElmMakeOutputTypeJs", Json.Encode.list identity [] ) ]


jsonDecode_FrontendBackendInterface_ElmMakeOutputType =
    Json.Decode.oneOf
        [ Json.Decode.field "ElmMakeOutputTypeHtml" (jsonDecodeSucceedWhenNotNull FrontendBackendInterface.ElmMakeOutputTypeHtml)
        , Json.Decode.field "ElmMakeOutputTypeJs" (jsonDecodeSucceedWhenNotNull FrontendBackendInterface.ElmMakeOutputTypeJs)
        ]


jsonEncode_FrontendBackendInterface_RequestStructure valueToEncode =
    case valueToEncode of
        FrontendBackendInterface.ElmMakeRequest tagArgument0 ->
            Json.Encode.object [ ( "ElmMakeRequest", Json.Encode.list identity [ Json.Encode.object
                [ ( "files"
                  , jsonEncode__generic_List (\type_arg -> Json.Encode.object
                        [ ( "path"
                          , jsonEncode__generic_List (\type_arg_ -> Json.Encode.string type_arg_) type_arg.path
                          )
                        , ( "contentBase64"
                          , Json.Encode.string type_arg.contentBase64
                          )
                        ]) tagArgument0.files
                  )
                , ( "workingDirectoryPath"
                  , jsonEncode__generic_List (\type_arg -> Json.Encode.string type_arg) tagArgument0.workingDirectoryPath
                  )
                , ( "entryPointFilePathFromWorkingDirectory"
                  , jsonEncode__generic_List (\type_arg -> Json.Encode.string type_arg) tagArgument0.entryPointFilePathFromWorkingDirectory
                  )
                , ( "makeOptionDebug"
                  , Json.Encode.bool tagArgument0.makeOptionDebug
                  )
                , ( "outputType"
                  , jsonEncode_FrontendBackendInterface_ElmMakeOutputType tagArgument0.outputType
                  )
                ] ] ) ]
        FrontendBackendInterface.FormatElmModuleTextRequest tagArgument0 ->
            Json.Encode.object [ ( "FormatElmModuleTextRequest", Json.Encode.list identity [ Json.Encode.string tagArgument0 ] ) ]
        FrontendBackendInterface.LoadCompositionRequest tagArgument0 ->
            Json.Encode.object [ ( "LoadCompositionRequest", Json.Encode.list identity [ Json.Encode.string tagArgument0 ] ) ]


jsonDecode_FrontendBackendInterface_RequestStructure =
    Json.Decode.oneOf
        [ Json.Decode.field "ElmMakeRequest" (Json.Decode.lazy (\_ -> Json.Decode.map FrontendBackendInterface.ElmMakeRequest (Json.Decode.index 0 (Json.Decode.succeed (\files workingDirectoryPath entryPointFilePathFromWorkingDirectory makeOptionDebug outputType -> { files = files, workingDirectoryPath = workingDirectoryPath, entryPointFilePathFromWorkingDirectory = entryPointFilePathFromWorkingDirectory, makeOptionDebug = makeOptionDebug, outputType = outputType })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "files"
                    [ "Files" ]
                    ( jsonDecode__generic_List (Json.Decode.succeed (\path contentBase64 -> { path = path, contentBase64 = contentBase64 })
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "path"
                                [ "Path" ]
                                ( jsonDecode__generic_List Json.Decode.string
                                )
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "contentBase64"
                                [ "ContentBase64" ]
                                Json.Decode.string
                            ))
                    )
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "workingDirectoryPath"
                    [ "WorkingDirectoryPath" ]
                    ( jsonDecode__generic_List Json.Decode.string
                    )
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "entryPointFilePathFromWorkingDirectory"
                    [ "EntryPointFilePathFromWorkingDirectory" ]
                    ( jsonDecode__generic_List Json.Decode.string
                    )
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "makeOptionDebug"
                    [ "MakeOptionDebug" ]
                    Json.Decode.bool
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "outputType"
                    [ "OutputType" ]
                    jsonDecode_FrontendBackendInterface_ElmMakeOutputType
                )))))
        , Json.Decode.field "FormatElmModuleTextRequest" (Json.Decode.lazy (\_ -> Json.Decode.map FrontendBackendInterface.FormatElmModuleTextRequest (Json.Decode.index 0 Json.Decode.string)))
        , Json.Decode.field "LoadCompositionRequest" (Json.Decode.lazy (\_ -> Json.Decode.map FrontendBackendInterface.LoadCompositionRequest (Json.Decode.index 0 Json.Decode.string)))
        ]


jsonEncode_FrontendBackendInterface_ResponseStructure valueToEncode =
    case valueToEncode of
        FrontendBackendInterface.ElmMakeResponse tagArgument0 ->
            Json.Encode.object [ ( "ElmMakeResponse", Json.Encode.list identity [ Json.Encode.object
                [ ( "processOutput"
                  , Json.Encode.object
                        [ ( "standardError"
                          , Json.Encode.string tagArgument0.processOutput.standardError
                          )
                        , ( "standardOutput"
                          , Json.Encode.string tagArgument0.processOutput.standardOutput
                          )
                        , ( "exitCode"
                          , Json.Encode.int tagArgument0.processOutput.exitCode
                          )
                        ]
                  )
                , ( "outputFileContentBase64"
                  , jsonEncode__generic_Maybe (\type_arg -> Json.Encode.string type_arg) tagArgument0.outputFileContentBase64
                  )
                , ( "reportJsonProcessOutput"
                  , Json.Encode.object
                        [ ( "standardError"
                          , Json.Encode.string tagArgument0.reportJsonProcessOutput.standardError
                          )
                        , ( "standardOutput"
                          , Json.Encode.string tagArgument0.reportJsonProcessOutput.standardOutput
                          )
                        , ( "exitCode"
                          , Json.Encode.int tagArgument0.reportJsonProcessOutput.exitCode
                          )
                        ]
                  )
                ] ] ) ]
        FrontendBackendInterface.ErrorResponse tagArgument0 ->
            Json.Encode.object [ ( "ErrorResponse", Json.Encode.list identity [ Json.Encode.string tagArgument0 ] ) ]
        FrontendBackendInterface.FormatElmModuleTextResponse tagArgument0 ->
            Json.Encode.object [ ( "FormatElmModuleTextResponse", Json.Encode.list identity [ Json.Encode.object
                [ ( "processOutput"
                  , Json.Encode.object
                        [ ( "standardError"
                          , Json.Encode.string tagArgument0.processOutput.standardError
                          )
                        , ( "standardOutput"
                          , Json.Encode.string tagArgument0.processOutput.standardOutput
                          )
                        , ( "exitCode"
                          , Json.Encode.int tagArgument0.processOutput.exitCode
                          )
                        ]
                  )
                , ( "formattedText"
                  , jsonEncode__generic_Maybe (\type_arg -> Json.Encode.string type_arg) tagArgument0.formattedText
                  )
                ] ] ) ]
        FrontendBackendInterface.LoadCompositionResponse tagArgument0 ->
            Json.Encode.object [ ( "LoadCompositionResponse", Json.Encode.list identity [ Json.Encode.object
                [ ( "compositionId"
                  , Json.Encode.string tagArgument0.compositionId
                  )
                , ( "filesAsFlatList"
                  , jsonEncode__generic_List (\type_arg -> Json.Encode.object
                        [ ( "path"
                          , jsonEncode__generic_List (\type_arg_ -> Json.Encode.string type_arg_) type_arg.path
                          )
                        , ( "contentBase64"
                          , Json.Encode.string type_arg.contentBase64
                          )
                        ]) tagArgument0.filesAsFlatList
                  )
                , ( "urlInCommit"
                  , Json.Encode.string tagArgument0.urlInCommit
                  )
                ] ] ) ]


jsonDecode_FrontendBackendInterface_ResponseStructure =
    Json.Decode.oneOf
        [ Json.Decode.field "ElmMakeResponse" (Json.Decode.lazy (\_ -> Json.Decode.map FrontendBackendInterface.ElmMakeResponse (Json.Decode.index 0 (Json.Decode.succeed (\processOutput outputFileContentBase64 reportJsonProcessOutput -> { processOutput = processOutput, outputFileContentBase64 = outputFileContentBase64, reportJsonProcessOutput = reportJsonProcessOutput })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "processOutput"
                    [ "ProcessOutput" ]
                    ( Json.Decode.succeed (\standardError standardOutput exitCode -> { standardError = standardError, standardOutput = standardOutput, exitCode = exitCode })
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "standardError"
                                [ "StandardError" ]
                                Json.Decode.string
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "standardOutput"
                                [ "StandardOutput" ]
                                Json.Decode.string
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "exitCode"
                                [ "ExitCode" ]
                                Json.Decode.int
                            )
                    )
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "outputFileContentBase64"
                    [ "OutputFileContentBase64" ]
                    ( jsonDecode__generic_Maybe Json.Decode.string
                    )
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "reportJsonProcessOutput"
                    [ "ReportJsonProcessOutput" ]
                    ( Json.Decode.succeed (\standardError standardOutput exitCode -> { standardError = standardError, standardOutput = standardOutput, exitCode = exitCode })
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "standardError"
                                [ "StandardError" ]
                                Json.Decode.string
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "standardOutput"
                                [ "StandardOutput" ]
                                Json.Decode.string
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "exitCode"
                                [ "ExitCode" ]
                                Json.Decode.int
                            )
                    )
                )))))
        , Json.Decode.field "ErrorResponse" (Json.Decode.lazy (\_ -> Json.Decode.map FrontendBackendInterface.ErrorResponse (Json.Decode.index 0 Json.Decode.string)))
        , Json.Decode.field "FormatElmModuleTextResponse" (Json.Decode.lazy (\_ -> Json.Decode.map FrontendBackendInterface.FormatElmModuleTextResponse (Json.Decode.index 0 (Json.Decode.succeed (\processOutput formattedText -> { processOutput = processOutput, formattedText = formattedText })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "processOutput"
                    [ "ProcessOutput" ]
                    ( Json.Decode.succeed (\standardError standardOutput exitCode -> { standardError = standardError, standardOutput = standardOutput, exitCode = exitCode })
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "standardError"
                                [ "StandardError" ]
                                Json.Decode.string
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "standardOutput"
                                [ "StandardOutput" ]
                                Json.Decode.string
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "exitCode"
                                [ "ExitCode" ]
                                Json.Decode.int
                            )
                    )
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "formattedText"
                    [ "FormattedText" ]
                    ( jsonDecode__generic_Maybe Json.Decode.string
                    )
                )))))
        , Json.Decode.field "LoadCompositionResponse" (Json.Decode.lazy (\_ -> Json.Decode.map FrontendBackendInterface.LoadCompositionResponse (Json.Decode.index 0 (Json.Decode.succeed (\compositionId filesAsFlatList urlInCommit -> { compositionId = compositionId, filesAsFlatList = filesAsFlatList, urlInCommit = urlInCommit })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "compositionId"
                    [ "CompositionId" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "filesAsFlatList"
                    [ "FilesAsFlatList" ]
                    ( jsonDecode__generic_List (Json.Decode.succeed (\path contentBase64 -> { path = path, contentBase64 = contentBase64 })
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "path"
                                [ "Path" ]
                                ( jsonDecode__generic_List Json.Decode.string
                                )
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "contentBase64"
                                [ "ContentBase64" ]
                                Json.Decode.string
                            ))
                    )
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "urlInCommit"
                    [ "UrlInCommit" ]
                    Json.Decode.string
                )))))
        ]


jsonEncode_Frontend_MonacoEditor_EditorMarkerSeverity valueToEncode =
    case valueToEncode of
        Frontend.MonacoEditor.ErrorSeverity ->
            Json.Encode.object [ ( "ErrorSeverity", Json.Encode.list identity [] ) ]
        Frontend.MonacoEditor.HintSeverity ->
            Json.Encode.object [ ( "HintSeverity", Json.Encode.list identity [] ) ]
        Frontend.MonacoEditor.InfoSeverity ->
            Json.Encode.object [ ( "InfoSeverity", Json.Encode.list identity [] ) ]
        Frontend.MonacoEditor.WarningSeverity ->
            Json.Encode.object [ ( "WarningSeverity", Json.Encode.list identity [] ) ]


jsonDecode_Frontend_MonacoEditor_EditorMarkerSeverity =
    Json.Decode.oneOf
        [ Json.Decode.field "ErrorSeverity" (jsonDecodeSucceedWhenNotNull Frontend.MonacoEditor.ErrorSeverity)
        , Json.Decode.field "HintSeverity" (jsonDecodeSucceedWhenNotNull Frontend.MonacoEditor.HintSeverity)
        , Json.Decode.field "InfoSeverity" (jsonDecodeSucceedWhenNotNull Frontend.MonacoEditor.InfoSeverity)
        , Json.Decode.field "WarningSeverity" (jsonDecodeSucceedWhenNotNull Frontend.MonacoEditor.WarningSeverity)
        ]


jsonEncode_Frontend_MonacoEditor_CompletionItemKind valueToEncode =
    case valueToEncode of
        Frontend.MonacoEditor.ConstructorCompletionItemKind ->
            Json.Encode.object [ ( "ConstructorCompletionItemKind", Json.Encode.list identity [] ) ]
        Frontend.MonacoEditor.EnumCompletionItemKind ->
            Json.Encode.object [ ( "EnumCompletionItemKind", Json.Encode.list identity [] ) ]
        Frontend.MonacoEditor.EnumMemberCompletionItemKind ->
            Json.Encode.object [ ( "EnumMemberCompletionItemKind", Json.Encode.list identity [] ) ]
        Frontend.MonacoEditor.FunctionCompletionItemKind ->
            Json.Encode.object [ ( "FunctionCompletionItemKind", Json.Encode.list identity [] ) ]
        Frontend.MonacoEditor.ModuleCompletionItemKind ->
            Json.Encode.object [ ( "ModuleCompletionItemKind", Json.Encode.list identity [] ) ]
        Frontend.MonacoEditor.StructCompletionItemKind ->
            Json.Encode.object [ ( "StructCompletionItemKind", Json.Encode.list identity [] ) ]


jsonDecode_Frontend_MonacoEditor_CompletionItemKind =
    Json.Decode.oneOf
        [ Json.Decode.field "ConstructorCompletionItemKind" (jsonDecodeSucceedWhenNotNull Frontend.MonacoEditor.ConstructorCompletionItemKind)
        , Json.Decode.field "EnumCompletionItemKind" (jsonDecodeSucceedWhenNotNull Frontend.MonacoEditor.EnumCompletionItemKind)
        , Json.Decode.field "EnumMemberCompletionItemKind" (jsonDecodeSucceedWhenNotNull Frontend.MonacoEditor.EnumMemberCompletionItemKind)
        , Json.Decode.field "FunctionCompletionItemKind" (jsonDecodeSucceedWhenNotNull Frontend.MonacoEditor.FunctionCompletionItemKind)
        , Json.Decode.field "ModuleCompletionItemKind" (jsonDecodeSucceedWhenNotNull Frontend.MonacoEditor.ModuleCompletionItemKind)
        , Json.Decode.field "StructCompletionItemKind" (jsonDecodeSucceedWhenNotNull Frontend.MonacoEditor.StructCompletionItemKind)
        ]


jsonEncode_Frontend_MonacoEditor_MessageToEditor valueToEncode =
    case valueToEncode of
        Frontend.MonacoEditor.OpenDocumentEvent tagArgument0 ->
            Json.Encode.object [ ( "OpenDocumentEvent", Json.Encode.list identity [ Json.Encode.object
                [ ( "value"
                  , Json.Encode.string tagArgument0.value
                  )
                , ( "language"
                  , Json.Encode.string tagArgument0.language
                  )
                , ( "uri"
                  , Json.Encode.string tagArgument0.uri
                  )
                , ( "position"
                  , Json.Encode.object
                        [ ( "lineNumber"
                          , Json.Encode.int tagArgument0.position.lineNumber
                          )
                        , ( "column"
                          , Json.Encode.int tagArgument0.position.column
                          )
                        ]
                  )
                ] ] ) ]
        Frontend.MonacoEditor.ProvideCompletionItemsEvent tagArgument0 ->
            Json.Encode.object [ ( "ProvideCompletionItemsEvent", Json.Encode.list identity [ jsonEncode__generic_List (\type_arg -> Json.Encode.object
                [ ( "label"
                  , Json.Encode.string type_arg.label
                  )
                , ( "kind"
                  , jsonEncode_Frontend_MonacoEditor_CompletionItemKind type_arg.kind
                  )
                , ( "documentation"
                  , Json.Encode.string type_arg.documentation
                  )
                , ( "insertText"
                  , Json.Encode.string type_arg.insertText
                  )
                ]) tagArgument0 ] ) ]
        Frontend.MonacoEditor.ProvideDefinitionEvent tagArgument0 ->
            Json.Encode.object [ ( "ProvideDefinitionEvent", Json.Encode.list identity [ jsonEncode__generic_List (\type_arg -> Json.Encode.object
                [ ( "range"
                  , Json.Encode.object
                        [ ( "startLineNumber"
                          , Json.Encode.int type_arg.range.startLineNumber
                          )
                        , ( "startColumn"
                          , Json.Encode.int type_arg.range.startColumn
                          )
                        , ( "endLineNumber"
                          , Json.Encode.int type_arg.range.endLineNumber
                          )
                        , ( "endColumn"
                          , Json.Encode.int type_arg.range.endColumn
                          )
                        ]
                  )
                , ( "uri"
                  , Json.Encode.string type_arg.uri
                  )
                ]) tagArgument0 ] ) ]
        Frontend.MonacoEditor.ProvideHoverEvent tagArgument0 ->
            Json.Encode.object [ ( "ProvideHoverEvent", Json.Encode.list identity [ jsonEncode__generic_List (\type_arg -> Json.Encode.string type_arg) tagArgument0 ] ) ]
        Frontend.MonacoEditor.RevealPositionInCenter tagArgument0 ->
            Json.Encode.object [ ( "RevealPositionInCenter", Json.Encode.list identity [ Json.Encode.object
                [ ( "lineNumber"
                  , Json.Encode.int tagArgument0.lineNumber
                  )
                , ( "column"
                  , Json.Encode.int tagArgument0.column
                  )
                ] ] ) ]
        Frontend.MonacoEditor.SetModelMarkers tagArgument0 ->
            Json.Encode.object [ ( "SetModelMarkers", Json.Encode.list identity [ jsonEncode__generic_List (\type_arg -> Json.Encode.object
                [ ( "message"
                  , Json.Encode.string type_arg.message
                  )
                , ( "startLineNumber"
                  , Json.Encode.int type_arg.startLineNumber
                  )
                , ( "startColumn"
                  , Json.Encode.int type_arg.startColumn
                  )
                , ( "endLineNumber"
                  , Json.Encode.int type_arg.endLineNumber
                  )
                , ( "endColumn"
                  , Json.Encode.int type_arg.endColumn
                  )
                , ( "severity"
                  , jsonEncode_Frontend_MonacoEditor_EditorMarkerSeverity type_arg.severity
                  )
                ]) tagArgument0 ] ) ]


jsonDecode_Frontend_MonacoEditor_MessageToEditor =
    Json.Decode.oneOf
        [ Json.Decode.field "OpenDocumentEvent" (Json.Decode.lazy (\_ -> Json.Decode.map Frontend.MonacoEditor.OpenDocumentEvent (Json.Decode.index 0 (Json.Decode.succeed (\value language uri position -> { value = value, language = language, uri = uri, position = position })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "value"
                    [ "Value" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "language"
                    [ "Language" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "uri"
                    [ "Uri" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "position"
                    [ "Position" ]
                    ( Json.Decode.succeed (\lineNumber column -> { lineNumber = lineNumber, column = column })
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "lineNumber"
                                [ "LineNumber" ]
                                Json.Decode.int
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "column"
                                [ "Column" ]
                                Json.Decode.int
                            )
                    )
                )))))
        , Json.Decode.field "ProvideCompletionItemsEvent" (Json.Decode.lazy (\_ -> Json.Decode.map Frontend.MonacoEditor.ProvideCompletionItemsEvent (Json.Decode.index 0 (jsonDecode__generic_List (Json.Decode.succeed (\label kind documentation insertText -> { label = label, kind = kind, documentation = documentation, insertText = insertText })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "label"
                    [ "Label" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "kind"
                    [ "Kind" ]
                    jsonDecode_Frontend_MonacoEditor_CompletionItemKind
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "documentation"
                    [ "Documentation" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "insertText"
                    [ "InsertText" ]
                    Json.Decode.string
                ))))))
        , Json.Decode.field "ProvideDefinitionEvent" (Json.Decode.lazy (\_ -> Json.Decode.map Frontend.MonacoEditor.ProvideDefinitionEvent (Json.Decode.index 0 (jsonDecode__generic_List (Json.Decode.succeed (\range uri -> { range = range, uri = uri })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "range"
                    [ "Range" ]
                    ( Json.Decode.succeed (\startLineNumber startColumn endLineNumber endColumn -> { startLineNumber = startLineNumber, startColumn = startColumn, endLineNumber = endLineNumber, endColumn = endColumn })
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "startLineNumber"
                                [ "StartLineNumber" ]
                                Json.Decode.int
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "startColumn"
                                [ "StartColumn" ]
                                Json.Decode.int
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "endLineNumber"
                                [ "EndLineNumber" ]
                                Json.Decode.int
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "endColumn"
                                [ "EndColumn" ]
                                Json.Decode.int
                            )
                    )
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "uri"
                    [ "Uri" ]
                    Json.Decode.string
                ))))))
        , Json.Decode.field "ProvideHoverEvent" (Json.Decode.lazy (\_ -> Json.Decode.map Frontend.MonacoEditor.ProvideHoverEvent (Json.Decode.index 0 (jsonDecode__generic_List Json.Decode.string))))
        , Json.Decode.field "RevealPositionInCenter" (Json.Decode.lazy (\_ -> Json.Decode.map Frontend.MonacoEditor.RevealPositionInCenter (Json.Decode.index 0 (Json.Decode.succeed (\lineNumber column -> { lineNumber = lineNumber, column = column })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "lineNumber"
                    [ "LineNumber" ]
                    Json.Decode.int
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "column"
                    [ "Column" ]
                    Json.Decode.int
                )))))
        , Json.Decode.field "SetModelMarkers" (Json.Decode.lazy (\_ -> Json.Decode.map Frontend.MonacoEditor.SetModelMarkers (Json.Decode.index 0 (jsonDecode__generic_List (Json.Decode.succeed (\message startLineNumber startColumn endLineNumber endColumn severity -> { message = message, startLineNumber = startLineNumber, startColumn = startColumn, endLineNumber = endLineNumber, endColumn = endColumn, severity = severity })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "message"
                    [ "Message" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "startLineNumber"
                    [ "StartLineNumber" ]
                    Json.Decode.int
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "startColumn"
                    [ "StartColumn" ]
                    Json.Decode.int
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "endLineNumber"
                    [ "EndLineNumber" ]
                    Json.Decode.int
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "endColumn"
                    [ "EndColumn" ]
                    Json.Decode.int
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "severity"
                    [ "Severity" ]
                    jsonDecode_Frontend_MonacoEditor_EditorMarkerSeverity
                ))))))
        ]


jsonEncode_Frontend_MonacoEditor_MessageFromEditor valueToEncode =
    case valueToEncode of
        Frontend.MonacoEditor.CompletedSetupEvent ->
            Json.Encode.object [ ( "CompletedSetupEvent", Json.Encode.list identity [] ) ]
        Frontend.MonacoEditor.DidChangeContentEvent tagArgument0 ->
            Json.Encode.object [ ( "DidChangeContentEvent", Json.Encode.list identity [ Json.Encode.object
                [ ( "textModelValue"
                  , Json.Encode.string tagArgument0.textModelValue
                  )
                , ( "uri"
                  , Json.Encode.string tagArgument0.uri
                  )
                ] ] ) ]
        Frontend.MonacoEditor.DidFocusEditorWidgetEvent ->
            Json.Encode.object [ ( "DidFocusEditorWidgetEvent", Json.Encode.list identity [] ) ]
        Frontend.MonacoEditor.EditorActionCloseEditorEvent ->
            Json.Encode.object [ ( "EditorActionCloseEditorEvent", Json.Encode.list identity [] ) ]
        Frontend.MonacoEditor.EditorActionCompileEvent ->
            Json.Encode.object [ ( "EditorActionCompileEvent", Json.Encode.list identity [] ) ]
        Frontend.MonacoEditor.EditorActionFormatDocumentEvent ->
            Json.Encode.object [ ( "EditorActionFormatDocumentEvent", Json.Encode.list identity [] ) ]
        Frontend.MonacoEditor.EditorActionInspectSyntaxEvent ->
            Json.Encode.object [ ( "EditorActionInspectSyntaxEvent", Json.Encode.list identity [] ) ]
        Frontend.MonacoEditor.OpenCodeEditorEvent tagArgument0 ->
            Json.Encode.object [ ( "OpenCodeEditorEvent", Json.Encode.list identity [ Json.Encode.object
                [ ( "uri"
                  , Json.Encode.string tagArgument0.uri
                  )
                , ( "position"
                  , Json.Encode.object
                        [ ( "lineNumber"
                          , Json.Encode.int tagArgument0.position.lineNumber
                          )
                        , ( "column"
                          , Json.Encode.int tagArgument0.position.column
                          )
                        ]
                  )
                ] ] ) ]
        Frontend.MonacoEditor.RequestCompletionItemsEvent tagArgument0 ->
            Json.Encode.object [ ( "RequestCompletionItemsEvent", Json.Encode.list identity [ Json.Encode.object
                [ ( "uri"
                  , Json.Encode.string tagArgument0.uri
                  )
                , ( "cursorLineNumber"
                  , Json.Encode.int tagArgument0.cursorLineNumber
                  )
                , ( "cursorColumn"
                  , Json.Encode.int tagArgument0.cursorColumn
                  )
                ] ] ) ]
        Frontend.MonacoEditor.RequestDefinitionEvent tagArgument0 ->
            Json.Encode.object [ ( "RequestDefinitionEvent", Json.Encode.list identity [ Json.Encode.object
                [ ( "uri"
                  , Json.Encode.string tagArgument0.uri
                  )
                , ( "positionLineNumber"
                  , Json.Encode.int tagArgument0.positionLineNumber
                  )
                , ( "positionColumn"
                  , Json.Encode.int tagArgument0.positionColumn
                  )
                , ( "lineText"
                  , Json.Encode.string tagArgument0.lineText
                  )
                , ( "word"
                  , Json.Encode.string tagArgument0.word
                  )
                ] ] ) ]
        Frontend.MonacoEditor.RequestHoverEvent tagArgument0 ->
            Json.Encode.object [ ( "RequestHoverEvent", Json.Encode.list identity [ Json.Encode.object
                [ ( "uri"
                  , Json.Encode.string tagArgument0.uri
                  )
                , ( "positionLineNumber"
                  , Json.Encode.int tagArgument0.positionLineNumber
                  )
                , ( "positionColumn"
                  , Json.Encode.int tagArgument0.positionColumn
                  )
                , ( "lineText"
                  , Json.Encode.string tagArgument0.lineText
                  )
                , ( "word"
                  , Json.Encode.string tagArgument0.word
                  )
                ] ] ) ]


jsonDecode_Frontend_MonacoEditor_MessageFromEditor =
    Json.Decode.oneOf
        [ Json.Decode.field "CompletedSetupEvent" (jsonDecodeSucceedWhenNotNull Frontend.MonacoEditor.CompletedSetupEvent)
        , Json.Decode.field "DidChangeContentEvent" (Json.Decode.lazy (\_ -> Json.Decode.map Frontend.MonacoEditor.DidChangeContentEvent (Json.Decode.index 0 (Json.Decode.succeed (\textModelValue uri -> { textModelValue = textModelValue, uri = uri })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "textModelValue"
                    [ "TextModelValue" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "uri"
                    [ "Uri" ]
                    Json.Decode.string
                )))))
        , Json.Decode.field "DidFocusEditorWidgetEvent" (jsonDecodeSucceedWhenNotNull Frontend.MonacoEditor.DidFocusEditorWidgetEvent)
        , Json.Decode.field "EditorActionCloseEditorEvent" (jsonDecodeSucceedWhenNotNull Frontend.MonacoEditor.EditorActionCloseEditorEvent)
        , Json.Decode.field "EditorActionCompileEvent" (jsonDecodeSucceedWhenNotNull Frontend.MonacoEditor.EditorActionCompileEvent)
        , Json.Decode.field "EditorActionFormatDocumentEvent" (jsonDecodeSucceedWhenNotNull Frontend.MonacoEditor.EditorActionFormatDocumentEvent)
        , Json.Decode.field "EditorActionInspectSyntaxEvent" (jsonDecodeSucceedWhenNotNull Frontend.MonacoEditor.EditorActionInspectSyntaxEvent)
        , Json.Decode.field "OpenCodeEditorEvent" (Json.Decode.lazy (\_ -> Json.Decode.map Frontend.MonacoEditor.OpenCodeEditorEvent (Json.Decode.index 0 (Json.Decode.succeed (\uri position -> { uri = uri, position = position })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "uri"
                    [ "Uri" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "position"
                    [ "Position" ]
                    ( Json.Decode.succeed (\lineNumber column -> { lineNumber = lineNumber, column = column })
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "lineNumber"
                                [ "LineNumber" ]
                                Json.Decode.int
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "column"
                                [ "Column" ]
                                Json.Decode.int
                            )
                    )
                )))))
        , Json.Decode.field "RequestCompletionItemsEvent" (Json.Decode.lazy (\_ -> Json.Decode.map Frontend.MonacoEditor.RequestCompletionItemsEvent (Json.Decode.index 0 (Json.Decode.succeed (\uri cursorLineNumber cursorColumn -> { uri = uri, cursorLineNumber = cursorLineNumber, cursorColumn = cursorColumn })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "uri"
                    [ "Uri" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "cursorLineNumber"
                    [ "CursorLineNumber" ]
                    Json.Decode.int
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "cursorColumn"
                    [ "CursorColumn" ]
                    Json.Decode.int
                )))))
        , Json.Decode.field "RequestDefinitionEvent" (Json.Decode.lazy (\_ -> Json.Decode.map Frontend.MonacoEditor.RequestDefinitionEvent (Json.Decode.index 0 (Json.Decode.succeed (\uri positionLineNumber positionColumn lineText word -> { uri = uri, positionLineNumber = positionLineNumber, positionColumn = positionColumn, lineText = lineText, word = word })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "uri"
                    [ "Uri" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "positionLineNumber"
                    [ "PositionLineNumber" ]
                    Json.Decode.int
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "positionColumn"
                    [ "PositionColumn" ]
                    Json.Decode.int
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "lineText"
                    [ "LineText" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "word"
                    [ "Word" ]
                    Json.Decode.string
                )))))
        , Json.Decode.field "RequestHoverEvent" (Json.Decode.lazy (\_ -> Json.Decode.map Frontend.MonacoEditor.RequestHoverEvent (Json.Decode.index 0 (Json.Decode.succeed (\uri positionLineNumber positionColumn lineText word -> { uri = uri, positionLineNumber = positionLineNumber, positionColumn = positionColumn, lineText = lineText, word = word })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "uri"
                    [ "Uri" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "positionLineNumber"
                    [ "PositionLineNumber" ]
                    Json.Decode.int
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "positionColumn"
                    [ "PositionColumn" ]
                    Json.Decode.int
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "lineText"
                    [ "LineText" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "word"
                    [ "Word" ]
                    Json.Decode.string
                )))))
        ]


jsonEncode_Frontend_ContainerHtml_Message valueToEncode =
    case valueToEncode of
        Frontend.ContainerHtml.ClickedLinkInPreview tagArgument0 ->
            Json.Encode.object [ ( "ClickedLinkInPreview", Json.Encode.list identity [ Json.Encode.object
                [ ( "href"
                  , Json.Encode.string tagArgument0.href
                  )
                ] ] ) ]


jsonDecode_Frontend_ContainerHtml_Message =
    Json.Decode.oneOf
        [ Json.Decode.field "ClickedLinkInPreview" (Json.Decode.lazy (\_ -> Json.Decode.map Frontend.ContainerHtml.ClickedLinkInPreview (Json.Decode.index 0 (Json.Decode.succeed (\href -> { href = href })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "href"
                    [ "Href" ]
                    Json.Decode.string
                )))))
        ]


jsonEncode_FileTree_FileTreeNode jsonEncode_type_parameter_blobStructure valueToEncode =
    case valueToEncode of
        FileTree.BlobNode tagArgument0 ->
            Json.Encode.object [ ( "BlobNode", Json.Encode.list identity [ jsonEncode_type_parameter_blobStructure tagArgument0 ] ) ]
        FileTree.TreeNode tagArgument0 ->
            Json.Encode.object [ ( "TreeNode", Json.Encode.list identity [ jsonEncode__generic_List (\type_arg -> Json.Encode.list identity
                [ Json.Encode.string ((\( item_0, item_1 ) -> item_0) type_arg)
                , jsonEncode_FileTree_FileTreeNode (\type_arg_ -> jsonEncode_type_parameter_blobStructure type_arg_) ((\( item_0, item_1 ) -> item_1) type_arg)
                ]) tagArgument0 ] ) ]


jsonDecode_FileTree_FileTreeNode jsonDecode_type_parameter_blobStructure =
    Json.Decode.oneOf
        [ Json.Decode.field "BlobNode" (Json.Decode.lazy (\_ -> Json.Decode.map FileTree.BlobNode (Json.Decode.index 0 jsonDecode_type_parameter_blobStructure)))
        , Json.Decode.field "TreeNode" (Json.Decode.lazy (\_ -> Json.Decode.map FileTree.TreeNode (Json.Decode.index 0 (jsonDecode__generic_List (Json.Decode.map2 (\item_0 item_1 -> ( item_0, item_1 ))
            (Json.Decode.index 0 Json.Decode.string)
            (Json.Decode.index 1 (jsonDecode_FileTree_FileTreeNode jsonDecode_type_parameter_blobStructure)))))))
        ]


jsonEncode_WorkspaceState_2021_01_BlobChangeSequenceElement valueToEncode =
    case valueToEncode of
        WorkspaceState_2021_01.AddBytes tagArgument0 ->
            Json.Encode.object [ ( "AddBytes", Json.Encode.list identity [ json_encode_Bytes tagArgument0 ] ) ]
        WorkspaceState_2021_01.RemoveBytes tagArgument0 ->
            Json.Encode.object [ ( "RemoveBytes", Json.Encode.list identity [ Json.Encode.int tagArgument0 ] ) ]
        WorkspaceState_2021_01.ReuseBytes tagArgument0 ->
            Json.Encode.object [ ( "ReuseBytes", Json.Encode.list identity [ Json.Encode.int tagArgument0 ] ) ]


jsonDecode_WorkspaceState_2021_01_BlobChangeSequenceElement =
    Json.Decode.oneOf
        [ Json.Decode.field "AddBytes" (Json.Decode.lazy (\_ -> Json.Decode.map WorkspaceState_2021_01.AddBytes (Json.Decode.index 0 json_decode_Bytes)))
        , Json.Decode.field "RemoveBytes" (Json.Decode.lazy (\_ -> Json.Decode.map WorkspaceState_2021_01.RemoveBytes (Json.Decode.index 0 Json.Decode.int)))
        , Json.Decode.field "ReuseBytes" (Json.Decode.lazy (\_ -> Json.Decode.map WorkspaceState_2021_01.ReuseBytes (Json.Decode.index 0 Json.Decode.int)))
        ]


jsonEncode_LanguageServiceInterface_ElmPackageVersionIdentifer valueToEncode =
    case valueToEncode of
        LanguageServiceInterface.ElmPackageVersion019Identifer tagArgument0 tagArgument1 ->
            Json.Encode.object [ ( "ElmPackageVersion019Identifer", Json.Encode.list identity [ Json.Encode.string tagArgument0, Json.Encode.string tagArgument1 ] ) ]


jsonDecode_LanguageServiceInterface_ElmPackageVersionIdentifer =
    Json.Decode.oneOf
        [ Json.Decode.field "ElmPackageVersion019Identifer" (Json.Decode.lazy (\_ -> Json.Decode.map2 LanguageServiceInterface.ElmPackageVersion019Identifer (Json.Decode.index 0 Json.Decode.string) (Json.Decode.index 1 Json.Decode.string)))
        ]


jsonEncode_LanguageServiceInterface_FileLocation valueToEncode =
    case valueToEncode of
        LanguageServiceInterface.ElmPackageFileLocation tagArgument0 tagArgument1 ->
            Json.Encode.object [ ( "ElmPackageFileLocation", Json.Encode.list identity [ jsonEncode_LanguageServiceInterface_ElmPackageVersionIdentifer tagArgument0, jsonEncode__generic_List (\type_arg -> Json.Encode.string type_arg) tagArgument1 ] ) ]
        LanguageServiceInterface.WorkspaceFileLocation tagArgument0 ->
            Json.Encode.object [ ( "WorkspaceFileLocation", Json.Encode.list identity [ Json.Encode.string tagArgument0 ] ) ]


jsonDecode_LanguageServiceInterface_FileLocation =
    Json.Decode.oneOf
        [ Json.Decode.field "ElmPackageFileLocation" (Json.Decode.lazy (\_ -> Json.Decode.map2 LanguageServiceInterface.ElmPackageFileLocation (Json.Decode.index 0 jsonDecode_LanguageServiceInterface_ElmPackageVersionIdentifer) (Json.Decode.index 1 (jsonDecode__generic_List Json.Decode.string))))
        , Json.Decode.field "WorkspaceFileLocation" (Json.Decode.lazy (\_ -> Json.Decode.map LanguageServiceInterface.WorkspaceFileLocation (Json.Decode.index 0 Json.Decode.string)))
        ]


jsonEncode_LanguageServiceInterface_Request valueToEncode =
    case valueToEncode of
        LanguageServiceInterface.AddElmPackageVersionRequest tagArgument0 tagArgument1 ->
            Json.Encode.object [ ( "AddElmPackageVersionRequest", Json.Encode.list identity [ jsonEncode_LanguageServiceInterface_ElmPackageVersionIdentifer tagArgument0, jsonEncode__generic_List (\type_arg -> Json.Encode.list identity
                [ jsonEncode__generic_List (\type_arg_ -> Json.Encode.string type_arg_) ((\( item_0, item_1 ) -> item_0) type_arg)
                , Json.Encode.object
                    [ ( "asBase64"
                      , Json.Encode.string ((\( item_0, item_1 ) -> item_1) type_arg).asBase64
                      )
                    , ( "asText"
                      , jsonEncode__generic_Maybe (\type_arg_ -> Json.Encode.string type_arg_) ((\( item_0, item_1 ) -> item_1) type_arg).asText
                      )
                    ]
                ]) tagArgument1 ] ) ]
        LanguageServiceInterface.AddWorkspaceFileRequest tagArgument0 tagArgument1 ->
            Json.Encode.object [ ( "AddWorkspaceFileRequest", Json.Encode.list identity [ Json.Encode.string tagArgument0, Json.Encode.object
                [ ( "asBase64"
                  , Json.Encode.string tagArgument1.asBase64
                  )
                , ( "asText"
                  , jsonEncode__generic_Maybe (\type_arg -> Json.Encode.string type_arg) tagArgument1.asText
                  )
                ] ] ) ]
        LanguageServiceInterface.DeleteWorkspaceFileRequest tagArgument0 ->
            Json.Encode.object [ ( "DeleteWorkspaceFileRequest", Json.Encode.list identity [ Json.Encode.string tagArgument0 ] ) ]
        LanguageServiceInterface.ProvideCompletionItemsRequest tagArgument0 ->
            Json.Encode.object [ ( "ProvideCompletionItemsRequest", Json.Encode.list identity [ Json.Encode.object
                [ ( "filePathOpenedInEditor"
                  , Json.Encode.string tagArgument0.filePathOpenedInEditor
                  )
                , ( "cursorLineNumber"
                  , Json.Encode.int tagArgument0.cursorLineNumber
                  )
                , ( "cursorColumn"
                  , Json.Encode.int tagArgument0.cursorColumn
                  )
                ] ] ) ]
        LanguageServiceInterface.ProvideDefinitionRequest tagArgument0 ->
            Json.Encode.object [ ( "ProvideDefinitionRequest", Json.Encode.list identity [ Json.Encode.object
                [ ( "fileLocation"
                  , jsonEncode_LanguageServiceInterface_FileLocation tagArgument0.fileLocation
                  )
                , ( "positionLineNumber"
                  , Json.Encode.int tagArgument0.positionLineNumber
                  )
                , ( "positionColumn"
                  , Json.Encode.int tagArgument0.positionColumn
                  )
                ] ] ) ]
        LanguageServiceInterface.ProvideHoverRequest tagArgument0 ->
            Json.Encode.object [ ( "ProvideHoverRequest", Json.Encode.list identity [ Json.Encode.object
                [ ( "fileLocation"
                  , jsonEncode_LanguageServiceInterface_FileLocation tagArgument0.fileLocation
                  )
                , ( "positionLineNumber"
                  , Json.Encode.int tagArgument0.positionLineNumber
                  )
                , ( "positionColumn"
                  , Json.Encode.int tagArgument0.positionColumn
                  )
                ] ] ) ]
        LanguageServiceInterface.TextDocumentReferencesRequest tagArgument0 ->
            Json.Encode.object [ ( "TextDocumentReferencesRequest", Json.Encode.list identity [ Json.Encode.object
                [ ( "fileLocation"
                  , jsonEncode_LanguageServiceInterface_FileLocation tagArgument0.fileLocation
                  )
                , ( "positionLineNumber"
                  , Json.Encode.int tagArgument0.positionLineNumber
                  )
                , ( "positionColumn"
                  , Json.Encode.int tagArgument0.positionColumn
                  )
                ] ] ) ]
        LanguageServiceInterface.TextDocumentRenameRequest tagArgument0 ->
            Json.Encode.object [ ( "TextDocumentRenameRequest", Json.Encode.list identity [ Json.Encode.object
                [ ( "filePath"
                  , Json.Encode.string tagArgument0.filePath
                  )
                , ( "positionLineNumber"
                  , Json.Encode.int tagArgument0.positionLineNumber
                  )
                , ( "positionColumn"
                  , Json.Encode.int tagArgument0.positionColumn
                  )
                , ( "newName"
                  , Json.Encode.string tagArgument0.newName
                  )
                ] ] ) ]
        LanguageServiceInterface.TextDocumentSymbolRequest tagArgument0 ->
            Json.Encode.object [ ( "TextDocumentSymbolRequest", Json.Encode.list identity [ Json.Encode.string tagArgument0 ] ) ]


jsonDecode_LanguageServiceInterface_Request =
    Json.Decode.oneOf
        [ Json.Decode.field "AddElmPackageVersionRequest" (Json.Decode.lazy (\_ -> Json.Decode.map2 LanguageServiceInterface.AddElmPackageVersionRequest (Json.Decode.index 0 jsonDecode_LanguageServiceInterface_ElmPackageVersionIdentifer) (Json.Decode.index 1 (jsonDecode__generic_List (Json.Decode.map2 (\item_0 item_1 -> ( item_0, item_1 ))
            (Json.Decode.index 0 (jsonDecode__generic_List Json.Decode.string))
            (Json.Decode.index 1 (Json.Decode.succeed (\asBase64 asText -> { asBase64 = asBase64, asText = asText })
                |> jsonDecode_andMap
                    ( jsonDecode_field_withAlternateNames "asBase64"
                        [ "AsBase64" ]
                        Json.Decode.string
                    )
                |> jsonDecode_andMap
                    ( jsonDecode_field_withAlternateNames "asText"
                        [ "AsText" ]
                        ( jsonDecode__generic_Maybe Json.Decode.string
                        )
                    ))))))))
        , Json.Decode.field "AddWorkspaceFileRequest" (Json.Decode.lazy (\_ -> Json.Decode.map2 LanguageServiceInterface.AddWorkspaceFileRequest (Json.Decode.index 0 Json.Decode.string) (Json.Decode.index 1 (Json.Decode.succeed (\asBase64 asText -> { asBase64 = asBase64, asText = asText })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "asBase64"
                    [ "AsBase64" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "asText"
                    [ "AsText" ]
                    ( jsonDecode__generic_Maybe Json.Decode.string
                    )
                )))))
        , Json.Decode.field "DeleteWorkspaceFileRequest" (Json.Decode.lazy (\_ -> Json.Decode.map LanguageServiceInterface.DeleteWorkspaceFileRequest (Json.Decode.index 0 Json.Decode.string)))
        , Json.Decode.field "ProvideCompletionItemsRequest" (Json.Decode.lazy (\_ -> Json.Decode.map LanguageServiceInterface.ProvideCompletionItemsRequest (Json.Decode.index 0 (Json.Decode.succeed (\filePathOpenedInEditor cursorLineNumber cursorColumn -> { filePathOpenedInEditor = filePathOpenedInEditor, cursorLineNumber = cursorLineNumber, cursorColumn = cursorColumn })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "filePathOpenedInEditor"
                    [ "FilePathOpenedInEditor" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "cursorLineNumber"
                    [ "CursorLineNumber" ]
                    Json.Decode.int
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "cursorColumn"
                    [ "CursorColumn" ]
                    Json.Decode.int
                )))))
        , Json.Decode.field "ProvideDefinitionRequest" (Json.Decode.lazy (\_ -> Json.Decode.map LanguageServiceInterface.ProvideDefinitionRequest (Json.Decode.index 0 (Json.Decode.succeed (\fileLocation positionLineNumber positionColumn -> { fileLocation = fileLocation, positionLineNumber = positionLineNumber, positionColumn = positionColumn })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "fileLocation"
                    [ "FileLocation" ]
                    jsonDecode_LanguageServiceInterface_FileLocation
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "positionLineNumber"
                    [ "PositionLineNumber" ]
                    Json.Decode.int
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "positionColumn"
                    [ "PositionColumn" ]
                    Json.Decode.int
                )))))
        , Json.Decode.field "ProvideHoverRequest" (Json.Decode.lazy (\_ -> Json.Decode.map LanguageServiceInterface.ProvideHoverRequest (Json.Decode.index 0 (Json.Decode.succeed (\fileLocation positionLineNumber positionColumn -> { fileLocation = fileLocation, positionLineNumber = positionLineNumber, positionColumn = positionColumn })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "fileLocation"
                    [ "FileLocation" ]
                    jsonDecode_LanguageServiceInterface_FileLocation
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "positionLineNumber"
                    [ "PositionLineNumber" ]
                    Json.Decode.int
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "positionColumn"
                    [ "PositionColumn" ]
                    Json.Decode.int
                )))))
        , Json.Decode.field "TextDocumentReferencesRequest" (Json.Decode.lazy (\_ -> Json.Decode.map LanguageServiceInterface.TextDocumentReferencesRequest (Json.Decode.index 0 (Json.Decode.succeed (\fileLocation positionLineNumber positionColumn -> { fileLocation = fileLocation, positionLineNumber = positionLineNumber, positionColumn = positionColumn })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "fileLocation"
                    [ "FileLocation" ]
                    jsonDecode_LanguageServiceInterface_FileLocation
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "positionLineNumber"
                    [ "PositionLineNumber" ]
                    Json.Decode.int
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "positionColumn"
                    [ "PositionColumn" ]
                    Json.Decode.int
                )))))
        , Json.Decode.field "TextDocumentRenameRequest" (Json.Decode.lazy (\_ -> Json.Decode.map LanguageServiceInterface.TextDocumentRenameRequest (Json.Decode.index 0 (Json.Decode.succeed (\filePath positionLineNumber positionColumn newName -> { filePath = filePath, positionLineNumber = positionLineNumber, positionColumn = positionColumn, newName = newName })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "filePath"
                    [ "FilePath" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "positionLineNumber"
                    [ "PositionLineNumber" ]
                    Json.Decode.int
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "positionColumn"
                    [ "PositionColumn" ]
                    Json.Decode.int
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "newName"
                    [ "NewName" ]
                    Json.Decode.string
                )))))
        , Json.Decode.field "TextDocumentSymbolRequest" (Json.Decode.lazy (\_ -> Json.Decode.map LanguageServiceInterface.TextDocumentSymbolRequest (Json.Decode.index 0 Json.Decode.string)))
        ]


jsonEncode_LanguageServiceInterface_SymbolKind valueToEncode =
    case valueToEncode of
        LanguageServiceInterface.SymbolKind_Array ->
            Json.Encode.object [ ( "SymbolKind_Array", Json.Encode.list identity [] ) ]
        LanguageServiceInterface.SymbolKind_Boolean ->
            Json.Encode.object [ ( "SymbolKind_Boolean", Json.Encode.list identity [] ) ]
        LanguageServiceInterface.SymbolKind_Class ->
            Json.Encode.object [ ( "SymbolKind_Class", Json.Encode.list identity [] ) ]
        LanguageServiceInterface.SymbolKind_Constant ->
            Json.Encode.object [ ( "SymbolKind_Constant", Json.Encode.list identity [] ) ]
        LanguageServiceInterface.SymbolKind_Enum ->
            Json.Encode.object [ ( "SymbolKind_Enum", Json.Encode.list identity [] ) ]
        LanguageServiceInterface.SymbolKind_EnumMember ->
            Json.Encode.object [ ( "SymbolKind_EnumMember", Json.Encode.list identity [] ) ]
        LanguageServiceInterface.SymbolKind_File ->
            Json.Encode.object [ ( "SymbolKind_File", Json.Encode.list identity [] ) ]
        LanguageServiceInterface.SymbolKind_Function ->
            Json.Encode.object [ ( "SymbolKind_Function", Json.Encode.list identity [] ) ]
        LanguageServiceInterface.SymbolKind_Interface ->
            Json.Encode.object [ ( "SymbolKind_Interface", Json.Encode.list identity [] ) ]
        LanguageServiceInterface.SymbolKind_Module ->
            Json.Encode.object [ ( "SymbolKind_Module", Json.Encode.list identity [] ) ]
        LanguageServiceInterface.SymbolKind_Namespace ->
            Json.Encode.object [ ( "SymbolKind_Namespace", Json.Encode.list identity [] ) ]
        LanguageServiceInterface.SymbolKind_Number ->
            Json.Encode.object [ ( "SymbolKind_Number", Json.Encode.list identity [] ) ]
        LanguageServiceInterface.SymbolKind_Package ->
            Json.Encode.object [ ( "SymbolKind_Package", Json.Encode.list identity [] ) ]
        LanguageServiceInterface.SymbolKind_String ->
            Json.Encode.object [ ( "SymbolKind_String", Json.Encode.list identity [] ) ]
        LanguageServiceInterface.SymbolKind_Struct ->
            Json.Encode.object [ ( "SymbolKind_Struct", Json.Encode.list identity [] ) ]


jsonDecode_LanguageServiceInterface_SymbolKind =
    Json.Decode.oneOf
        [ Json.Decode.field "SymbolKind_Array" (jsonDecodeSucceedWhenNotNull LanguageServiceInterface.SymbolKind_Array)
        , Json.Decode.field "SymbolKind_Boolean" (jsonDecodeSucceedWhenNotNull LanguageServiceInterface.SymbolKind_Boolean)
        , Json.Decode.field "SymbolKind_Class" (jsonDecodeSucceedWhenNotNull LanguageServiceInterface.SymbolKind_Class)
        , Json.Decode.field "SymbolKind_Constant" (jsonDecodeSucceedWhenNotNull LanguageServiceInterface.SymbolKind_Constant)
        , Json.Decode.field "SymbolKind_Enum" (jsonDecodeSucceedWhenNotNull LanguageServiceInterface.SymbolKind_Enum)
        , Json.Decode.field "SymbolKind_EnumMember" (jsonDecodeSucceedWhenNotNull LanguageServiceInterface.SymbolKind_EnumMember)
        , Json.Decode.field "SymbolKind_File" (jsonDecodeSucceedWhenNotNull LanguageServiceInterface.SymbolKind_File)
        , Json.Decode.field "SymbolKind_Function" (jsonDecodeSucceedWhenNotNull LanguageServiceInterface.SymbolKind_Function)
        , Json.Decode.field "SymbolKind_Interface" (jsonDecodeSucceedWhenNotNull LanguageServiceInterface.SymbolKind_Interface)
        , Json.Decode.field "SymbolKind_Module" (jsonDecodeSucceedWhenNotNull LanguageServiceInterface.SymbolKind_Module)
        , Json.Decode.field "SymbolKind_Namespace" (jsonDecodeSucceedWhenNotNull LanguageServiceInterface.SymbolKind_Namespace)
        , Json.Decode.field "SymbolKind_Number" (jsonDecodeSucceedWhenNotNull LanguageServiceInterface.SymbolKind_Number)
        , Json.Decode.field "SymbolKind_Package" (jsonDecodeSucceedWhenNotNull LanguageServiceInterface.SymbolKind_Package)
        , Json.Decode.field "SymbolKind_String" (jsonDecodeSucceedWhenNotNull LanguageServiceInterface.SymbolKind_String)
        , Json.Decode.field "SymbolKind_Struct" (jsonDecodeSucceedWhenNotNull LanguageServiceInterface.SymbolKind_Struct)
        ]


jsonEncode_LanguageServiceInterface_DocumentSymbol valueToEncode =
    case valueToEncode of
        LanguageServiceInterface.DocumentSymbol tagArgument0 ->
            Json.Encode.object [ ( "DocumentSymbol", Json.Encode.list identity [ Json.Encode.object
                [ ( "name"
                  , Json.Encode.string tagArgument0.name
                  )
                , ( "kind"
                  , jsonEncode_LanguageServiceInterface_SymbolKind tagArgument0.kind
                  )
                , ( "range"
                  , Json.Encode.object
                        [ ( "startLineNumber"
                          , Json.Encode.int tagArgument0.range.startLineNumber
                          )
                        , ( "startColumn"
                          , Json.Encode.int tagArgument0.range.startColumn
                          )
                        , ( "endLineNumber"
                          , Json.Encode.int tagArgument0.range.endLineNumber
                          )
                        , ( "endColumn"
                          , Json.Encode.int tagArgument0.range.endColumn
                          )
                        ]
                  )
                , ( "selectionRange"
                  , Json.Encode.object
                        [ ( "startLineNumber"
                          , Json.Encode.int tagArgument0.selectionRange.startLineNumber
                          )
                        , ( "startColumn"
                          , Json.Encode.int tagArgument0.selectionRange.startColumn
                          )
                        , ( "endLineNumber"
                          , Json.Encode.int tagArgument0.selectionRange.endLineNumber
                          )
                        , ( "endColumn"
                          , Json.Encode.int tagArgument0.selectionRange.endColumn
                          )
                        ]
                  )
                , ( "children"
                  , jsonEncode__generic_List (\type_arg -> jsonEncode_LanguageServiceInterface_DocumentSymbol type_arg) tagArgument0.children
                  )
                ] ] ) ]


jsonDecode_LanguageServiceInterface_DocumentSymbol =
    Json.Decode.oneOf
        [ Json.Decode.field "DocumentSymbol" (Json.Decode.lazy (\_ -> Json.Decode.map LanguageServiceInterface.DocumentSymbol (Json.Decode.index 0 (Json.Decode.succeed (\name kind range selectionRange children -> { name = name, kind = kind, range = range, selectionRange = selectionRange, children = children })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "name"
                    [ "Name" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "kind"
                    [ "Kind" ]
                    jsonDecode_LanguageServiceInterface_SymbolKind
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "range"
                    [ "Range" ]
                    ( Json.Decode.succeed (\startLineNumber startColumn endLineNumber endColumn -> { startLineNumber = startLineNumber, startColumn = startColumn, endLineNumber = endLineNumber, endColumn = endColumn })
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "startLineNumber"
                                [ "StartLineNumber" ]
                                Json.Decode.int
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "startColumn"
                                [ "StartColumn" ]
                                Json.Decode.int
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "endLineNumber"
                                [ "EndLineNumber" ]
                                Json.Decode.int
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "endColumn"
                                [ "EndColumn" ]
                                Json.Decode.int
                            )
                    )
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "selectionRange"
                    [ "SelectionRange" ]
                    ( Json.Decode.succeed (\startLineNumber startColumn endLineNumber endColumn -> { startLineNumber = startLineNumber, startColumn = startColumn, endLineNumber = endLineNumber, endColumn = endColumn })
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "startLineNumber"
                                [ "StartLineNumber" ]
                                Json.Decode.int
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "startColumn"
                                [ "StartColumn" ]
                                Json.Decode.int
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "endLineNumber"
                                [ "EndLineNumber" ]
                                Json.Decode.int
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "endColumn"
                                [ "EndColumn" ]
                                Json.Decode.int
                            )
                    )
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "children"
                    [ "Children" ]
                    ( jsonDecode__generic_List jsonDecode_LanguageServiceInterface_DocumentSymbol
                    )
                )))))
        ]


jsonEncode_LanguageServiceInterface_Response valueToEncode =
    case valueToEncode of
        LanguageServiceInterface.ProvideCompletionItemsResponse tagArgument0 ->
            Json.Encode.object [ ( "ProvideCompletionItemsResponse", Json.Encode.list identity [ jsonEncode__generic_List (\type_arg -> Json.Encode.object
                [ ( "label"
                  , Json.Encode.string type_arg.label
                  )
                , ( "kind"
                  , jsonEncode_Frontend_MonacoEditor_CompletionItemKind type_arg.kind
                  )
                , ( "documentation"
                  , Json.Encode.string type_arg.documentation
                  )
                , ( "insertText"
                  , Json.Encode.string type_arg.insertText
                  )
                ]) tagArgument0 ] ) ]
        LanguageServiceInterface.ProvideDefinitionResponse tagArgument0 ->
            Json.Encode.object [ ( "ProvideDefinitionResponse", Json.Encode.list identity [ jsonEncode__generic_List (\type_arg -> Json.Encode.object
                [ ( "fileLocation"
                  , jsonEncode_LanguageServiceInterface_FileLocation type_arg.fileLocation
                  )
                , ( "range"
                  , Json.Encode.object
                        [ ( "startLineNumber"
                          , Json.Encode.int type_arg.range.startLineNumber
                          )
                        , ( "startColumn"
                          , Json.Encode.int type_arg.range.startColumn
                          )
                        , ( "endLineNumber"
                          , Json.Encode.int type_arg.range.endLineNumber
                          )
                        , ( "endColumn"
                          , Json.Encode.int type_arg.range.endColumn
                          )
                        ]
                  )
                ]) tagArgument0 ] ) ]
        LanguageServiceInterface.ProvideHoverResponse tagArgument0 ->
            Json.Encode.object [ ( "ProvideHoverResponse", Json.Encode.list identity [ jsonEncode__generic_List (\type_arg -> Json.Encode.string type_arg) tagArgument0 ] ) ]
        LanguageServiceInterface.TextDocumentReferencesResponse tagArgument0 ->
            Json.Encode.object [ ( "TextDocumentReferencesResponse", Json.Encode.list identity [ jsonEncode__generic_List (\type_arg -> Json.Encode.object
                [ ( "fileLocation"
                  , jsonEncode_LanguageServiceInterface_FileLocation type_arg.fileLocation
                  )
                , ( "range"
                  , Json.Encode.object
                        [ ( "startLineNumber"
                          , Json.Encode.int type_arg.range.startLineNumber
                          )
                        , ( "startColumn"
                          , Json.Encode.int type_arg.range.startColumn
                          )
                        , ( "endLineNumber"
                          , Json.Encode.int type_arg.range.endLineNumber
                          )
                        , ( "endColumn"
                          , Json.Encode.int type_arg.range.endColumn
                          )
                        ]
                  )
                ]) tagArgument0 ] ) ]
        LanguageServiceInterface.TextDocumentRenameResponse tagArgument0 ->
            Json.Encode.object [ ( "TextDocumentRenameResponse", Json.Encode.list identity [ jsonEncode__generic_List (\type_arg -> Json.Encode.object
                [ ( "filePath"
                  , Json.Encode.string type_arg.filePath
                  )
                , ( "edits"
                  , jsonEncode__generic_List (\type_arg_ -> Json.Encode.object
                        [ ( "range"
                          , Json.Encode.object
                                [ ( "startLineNumber"
                                  , Json.Encode.int type_arg_.range.startLineNumber
                                  )
                                , ( "startColumn"
                                  , Json.Encode.int type_arg_.range.startColumn
                                  )
                                , ( "endLineNumber"
                                  , Json.Encode.int type_arg_.range.endLineNumber
                                  )
                                , ( "endColumn"
                                  , Json.Encode.int type_arg_.range.endColumn
                                  )
                                ]
                          )
                        , ( "newText"
                          , Json.Encode.string type_arg_.newText
                          )
                        ]) type_arg.edits
                  )
                ]) tagArgument0 ] ) ]
        LanguageServiceInterface.TextDocumentSymbolResponse tagArgument0 ->
            Json.Encode.object [ ( "TextDocumentSymbolResponse", Json.Encode.list identity [ jsonEncode__generic_List (\type_arg -> jsonEncode_LanguageServiceInterface_DocumentSymbol type_arg) tagArgument0 ] ) ]
        LanguageServiceInterface.WorkspaceSummaryResponse ->
            Json.Encode.object [ ( "WorkspaceSummaryResponse", Json.Encode.list identity [] ) ]


jsonDecode_LanguageServiceInterface_Response =
    Json.Decode.oneOf
        [ Json.Decode.field "ProvideCompletionItemsResponse" (Json.Decode.lazy (\_ -> Json.Decode.map LanguageServiceInterface.ProvideCompletionItemsResponse (Json.Decode.index 0 (jsonDecode__generic_List (Json.Decode.succeed (\label kind documentation insertText -> { label = label, kind = kind, documentation = documentation, insertText = insertText })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "label"
                    [ "Label" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "kind"
                    [ "Kind" ]
                    jsonDecode_Frontend_MonacoEditor_CompletionItemKind
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "documentation"
                    [ "Documentation" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "insertText"
                    [ "InsertText" ]
                    Json.Decode.string
                ))))))
        , Json.Decode.field "ProvideDefinitionResponse" (Json.Decode.lazy (\_ -> Json.Decode.map LanguageServiceInterface.ProvideDefinitionResponse (Json.Decode.index 0 (jsonDecode__generic_List (Json.Decode.succeed (\fileLocation range -> { fileLocation = fileLocation, range = range })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "fileLocation"
                    [ "FileLocation" ]
                    jsonDecode_LanguageServiceInterface_FileLocation
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "range"
                    [ "Range" ]
                    ( Json.Decode.succeed (\startLineNumber startColumn endLineNumber endColumn -> { startLineNumber = startLineNumber, startColumn = startColumn, endLineNumber = endLineNumber, endColumn = endColumn })
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "startLineNumber"
                                [ "StartLineNumber" ]
                                Json.Decode.int
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "startColumn"
                                [ "StartColumn" ]
                                Json.Decode.int
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "endLineNumber"
                                [ "EndLineNumber" ]
                                Json.Decode.int
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "endColumn"
                                [ "EndColumn" ]
                                Json.Decode.int
                            )
                    )
                ))))))
        , Json.Decode.field "ProvideHoverResponse" (Json.Decode.lazy (\_ -> Json.Decode.map LanguageServiceInterface.ProvideHoverResponse (Json.Decode.index 0 (jsonDecode__generic_List Json.Decode.string))))
        , Json.Decode.field "TextDocumentReferencesResponse" (Json.Decode.lazy (\_ -> Json.Decode.map LanguageServiceInterface.TextDocumentReferencesResponse (Json.Decode.index 0 (jsonDecode__generic_List (Json.Decode.succeed (\fileLocation range -> { fileLocation = fileLocation, range = range })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "fileLocation"
                    [ "FileLocation" ]
                    jsonDecode_LanguageServiceInterface_FileLocation
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "range"
                    [ "Range" ]
                    ( Json.Decode.succeed (\startLineNumber startColumn endLineNumber endColumn -> { startLineNumber = startLineNumber, startColumn = startColumn, endLineNumber = endLineNumber, endColumn = endColumn })
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "startLineNumber"
                                [ "StartLineNumber" ]
                                Json.Decode.int
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "startColumn"
                                [ "StartColumn" ]
                                Json.Decode.int
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "endLineNumber"
                                [ "EndLineNumber" ]
                                Json.Decode.int
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "endColumn"
                                [ "EndColumn" ]
                                Json.Decode.int
                            )
                    )
                ))))))
        , Json.Decode.field "TextDocumentRenameResponse" (Json.Decode.lazy (\_ -> Json.Decode.map LanguageServiceInterface.TextDocumentRenameResponse (Json.Decode.index 0 (jsonDecode__generic_List (Json.Decode.succeed (\filePath edits -> { filePath = filePath, edits = edits })
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "filePath"
                    [ "FilePath" ]
                    Json.Decode.string
                )
            |> jsonDecode_andMap
                ( jsonDecode_field_withAlternateNames "edits"
                    [ "Edits" ]
                    ( jsonDecode__generic_List (Json.Decode.succeed (\range newText -> { range = range, newText = newText })
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "range"
                                [ "Range" ]
                                ( Json.Decode.succeed (\startLineNumber startColumn endLineNumber endColumn -> { startLineNumber = startLineNumber, startColumn = startColumn, endLineNumber = endLineNumber, endColumn = endColumn })
                                    |> jsonDecode_andMap
                                        ( jsonDecode_field_withAlternateNames "startLineNumber"
                                            [ "StartLineNumber" ]
                                            Json.Decode.int
                                        )
                                    |> jsonDecode_andMap
                                        ( jsonDecode_field_withAlternateNames "startColumn"
                                            [ "StartColumn" ]
                                            Json.Decode.int
                                        )
                                    |> jsonDecode_andMap
                                        ( jsonDecode_field_withAlternateNames "endLineNumber"
                                            [ "EndLineNumber" ]
                                            Json.Decode.int
                                        )
                                    |> jsonDecode_andMap
                                        ( jsonDecode_field_withAlternateNames "endColumn"
                                            [ "EndColumn" ]
                                            Json.Decode.int
                                        )
                                )
                            )
                        |> jsonDecode_andMap
                            ( jsonDecode_field_withAlternateNames "newText"
                                [ "NewText" ]
                                Json.Decode.string
                            ))
                    )
                ))))))
        , Json.Decode.field "TextDocumentSymbolResponse" (Json.Decode.lazy (\_ -> Json.Decode.map LanguageServiceInterface.TextDocumentSymbolResponse (Json.Decode.index 0 (jsonDecode__generic_List jsonDecode_LanguageServiceInterface_DocumentSymbol))))
        , Json.Decode.field "WorkspaceSummaryResponse" (jsonDecodeSucceedWhenNotNull LanguageServiceInterface.WorkspaceSummaryResponse)
        ]


jsonEncode__generic_Maybe encodeJust valueToEncode =
    case valueToEncode of
        Nothing ->
            [ ( "Nothing", [] |> Json.Encode.list identity ) ] |> Json.Encode.object

        Just just ->
            [ ( "Just", [ just ] |> Json.Encode.list encodeJust ) ] |> Json.Encode.object


jsonDecode__generic_Maybe decoder =
    Json.Decode.oneOf
        [ Json.Decode.field "Nothing" (Json.Decode.succeed Nothing)
        , Json.Decode.field "Just" ((Json.Decode.index 0 decoder) |> Json.Decode.map Just)
        , Json.Decode.field "Just" (decoder |> Json.Decode.map Just) -- 2020-03-07 Support easy migration of apps: Support decode from older JSON format for now.
        , Json.Decode.null Nothing -- Temporary backwardscompatibility: Map 'null' to Nothing
        ]


jsonEncode__generic_List  = Json.Encode.list


jsonDecode__generic_List  = Json.Decode.list


jsonEncode__generic_Array  = Json.Encode.array


jsonDecode__generic_Array  = Json.Decode.array


jsonEncode__generic_Set encoder =
    Set.toList >> Json.Encode.list encoder


jsonDecode__generic_Set decoder =
    Json.Decode.list decoder |> Json.Decode.map Set.fromList


jsonEncode__generic_Dict encodeKey encodeValue =
    Dict.toList >> Json.Encode.list (jsonEncode__tuple_2 encodeKey encodeValue)


jsonDecode__generic_Dict decodeKey decodeValue =
        (Json.Decode.list (jsonDecode__tuple_2 decodeKey decodeValue))
            |> Json.Decode.map Dict.fromList


jsonEncode__generic_Result encodeErr encodeOk valueToEncode =
    case valueToEncode of
        Err valueToEncodeError ->
            [ ( "Err", [ valueToEncodeError ] |> Json.Encode.list encodeErr ) ] |> Json.Encode.object

        Ok valueToEncodeOk ->
            [ ( "Ok", [ valueToEncodeOk ] |> Json.Encode.list encodeOk ) ] |> Json.Encode.object


jsonDecode__generic_Result decodeErr decodeOk =
    Json.Decode.oneOf
        [ Json.Decode.field "Err" (Json.Decode.index 0 decodeErr) |> Json.Decode.map Err
        , Json.Decode.field "Ok" (Json.Decode.index 0 decodeOk) |> Json.Decode.map Ok
        , Json.Decode.field "Err" decodeErr |> Json.Decode.map Err -- 2020-03-07 Support easy migration of apps: Support decode from older JSON format for now.
        , Json.Decode.field "Ok" decodeOk |> Json.Decode.map Ok -- 2020-03-07 Support easy migration of apps: Support decode from older JSON format for now.
        ]


jsonEncode__tuple_2 encodeA encodeB ( a, b ) =
    [ a |> encodeA, b |> encodeB ]
        |> Json.Encode.list identity


jsonDecode__tuple_2 decodeA decodeB =
    Json.Decode.map2 (\a b -> ( a, b ))
        (Json.Decode.index 0 decodeA)
        (Json.Decode.index 1 decodeB)


jsonEncode__tuple_3 encodeA encodeB encodeC ( a, b, c ) =
    [ a |> encodeA, b |> encodeB, c |> encodeC ]
        |> Json.Encode.list identity


jsonDecode__tuple_3 decodeA decodeB decodeC =
    Json.Decode.map3 (\a b c -> ( a, b, c ))
        (Json.Decode.index 0 decodeA)
        (Json.Decode.index 1 decodeB)
        (Json.Decode.index 2 decodeC)


jsonDecode_andMap : Json.Decode.Decoder a -> Json.Decode.Decoder (a -> b) -> Json.Decode.Decoder b
jsonDecode_andMap =
    Json.Decode.map2 (|>)


json_encode_Bytes : Bytes.Bytes -> Json.Encode.Value
json_encode_Bytes bytes =
    [ ( "AsBase64", bytes |> Base64.fromBytes |> Maybe.withDefault "Error encoding to base64" |> Json.Encode.string ) ]
        |> Json.Encode.object


json_decode_Bytes : Json.Decode.Decoder Bytes.Bytes
json_decode_Bytes =
    Json.Decode.field "AsBase64"
        (Json.Decode.string
            |> Json.Decode.andThen
                (Base64.toBytes >> Maybe.map Json.Decode.succeed >> Maybe.withDefault (Json.Decode.fail "Failed to decode base64."))
        )


jsonDecodeSucceedWhenNotNull : a -> Json.Decode.Decoder a
jsonDecodeSucceedWhenNotNull valueIfNotNull =
    Json.Decode.value
        |> Json.Decode.andThen
            (\asValue ->
                if asValue == Json.Encode.null then
                    Json.Decode.fail "Is null."

                else
                    Json.Decode.succeed valueIfNotNull
            )


jsonDecode_field_withAlternateNames : String -> List String -> Json.Decode.Decoder a -> Json.Decode.Decoder a
jsonDecode_field_withAlternateNames fieldName alternateNames decoder =
    Json.Decode.oneOf
        ((fieldName :: alternateNames)
            |> List.map (\name -> Json.Decode.field name decoder)
        )