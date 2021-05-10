module Main exposing (..)

import Base64
import Bytes
import CompileFullstackApp
import Dict
import Json.Decode
import Json.Encode
import Platform


type alias CompilationArguments =
    { sourceFiles : CompileFullstackApp.AppFiles
    , compilationInterfaceElmModuleNamePrefixes : List String
    , dependencies : List ( CompileFullstackApp.DependencyKey, Bytes.Bytes )
    }


type alias CompilationResponse =
    Result (List CompileFullstackApp.CompilationError) CompileFullstackApp.AppFiles


lowerForSourceFilesSerialized : String -> String
lowerForSourceFilesSerialized argumentsJson =
    (case Json.Decode.decodeString jsonDecodeCompilationArguments argumentsJson of
        Err decodeError ->
            Err ("Failed to decode arguments: " ++ Json.Decode.errorToString decodeError)

        Ok args ->
            CompileFullstackApp.loweredForSourceFiles args.compilationInterfaceElmModuleNamePrefixes args.sourceFiles
    )
        |> Result.mapError (CompileFullstackApp.OtherCompilationError >> List.singleton)
        |> jsonEncodeLowerForSourceFilesResponse
        |> Json.Encode.encode 0


lowerForSourceFilesAndJsonCodersSerialized : String -> String
lowerForSourceFilesAndJsonCodersSerialized argumentsJson =
    (case Json.Decode.decodeString jsonDecodeCompilationArguments argumentsJson of
        Err decodeError ->
            Err ("Failed to decode arguments: " ++ Json.Decode.errorToString decodeError)

        Ok args ->
            CompileFullstackApp.loweredForSourceFilesAndJsonCoders args.compilationInterfaceElmModuleNamePrefixes args.sourceFiles
    )
        |> Result.mapError (CompileFullstackApp.OtherCompilationError >> List.singleton)
        |> jsonEncodeLowerForSourceFilesResponse
        |> Json.Encode.encode 0


lowerForSourceFilesAndJsonCodersAndElmMakeSerialized : String -> String
lowerForSourceFilesAndJsonCodersAndElmMakeSerialized argumentsJson =
    (case Json.Decode.decodeString jsonDecodeCompilationArguments argumentsJson of
        Err decodeError ->
            Err [ CompileFullstackApp.OtherCompilationError ("Failed to decode arguments: " ++ Json.Decode.errorToString decodeError) ]

        Ok args ->
            CompileFullstackApp.loweredForSourceFilesAndJsonCodersAndElmMake
                args.compilationInterfaceElmModuleNamePrefixes
                args.dependencies
                args.sourceFiles
    )
        |> jsonEncodeLowerForSourceFilesResponse
        |> Json.Encode.encode 0


jsonEncodeLowerForSourceFilesResponse : CompilationResponse -> Json.Encode.Value
jsonEncodeLowerForSourceFilesResponse submissionResponse =
    json_encode_Result
        (Json.Encode.list jsonEncodeCompilationError)
        jsonEncodeAppCode
        submissionResponse


jsonDecodeCompilationArguments : Json.Decode.Decoder CompilationArguments
jsonDecodeCompilationArguments =
    Json.Decode.map3 CompilationArguments
        (Json.Decode.field "sourceFiles" jsonDecodeAppCode)
        (Json.Decode.field "compilationInterfaceElmModuleNamePrefixes" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "dependencies" (Json.Decode.list jsonDecodeCompilationArgumentsDependency))


jsonDecodeCompilationArgumentsDependency : Json.Decode.Decoder ( CompileFullstackApp.DependencyKey, Bytes.Bytes )
jsonDecodeCompilationArgumentsDependency =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "key" jsonDecodeDependencyKey)
        (Json.Decode.field "value" json_decode_Bytes)


jsonEncodeCompilationError : CompileFullstackApp.CompilationError -> Json.Encode.Value
jsonEncodeCompilationError compilationError =
    case compilationError of
        CompileFullstackApp.MissingDependencyError missingDependencyKey ->
            [ ( "MissingDependencyError", Json.Encode.list identity [ jsonEncodeDependencyKey missingDependencyKey ] ) ]
                |> Json.Encode.object

        CompileFullstackApp.OtherCompilationError otherError ->
            [ ( "OtherCompilationError", Json.Encode.list identity [ Json.Encode.string otherError ] ) ]
                |> Json.Encode.object


jsonEncodeDependencyKey : CompileFullstackApp.DependencyKey -> Json.Encode.Value
jsonEncodeDependencyKey dependencyKey =
    case dependencyKey of
        CompileFullstackApp.ElmMakeDependency elmMakeRequest ->
            [ ( "ElmMakeDependency", Json.Encode.list identity [ jsonEncodeElmMakeRequest elmMakeRequest ] ) ]
                |> Json.Encode.object


jsonDecodeDependencyKey : Json.Decode.Decoder CompileFullstackApp.DependencyKey
jsonDecodeDependencyKey =
    Json.Decode.oneOf
        [ Json.Decode.field "ElmMakeDependency"
            (Json.Decode.index 0 (jsonDecodeElmMakeRequest |> Json.Decode.map CompileFullstackApp.ElmMakeDependency))
        ]


jsonEncodeElmMakeRequest : CompileFullstackApp.ElmMakeRequestStructure -> Json.Encode.Value
jsonEncodeElmMakeRequest elmMakeRequest =
    [ ( "files", jsonEncodeAppCode elmMakeRequest.files )
    , ( "entryPointFilePath", Json.Encode.list Json.Encode.string elmMakeRequest.entryPointFilePath )
    , ( "outputType", jsonEncodeElmMakeOutputType elmMakeRequest.outputType )
    , ( "enableDebug", Json.Encode.bool elmMakeRequest.enableDebug )
    ]
        |> Json.Encode.object


jsonDecodeElmMakeRequest : Json.Decode.Decoder CompileFullstackApp.ElmMakeRequestStructure
jsonDecodeElmMakeRequest =
    Json.Decode.map4 CompileFullstackApp.ElmMakeRequestStructure
        (Json.Decode.field "files" jsonDecodeAppCode)
        (Json.Decode.field "entryPointFilePath" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "outputType" jsonDecodeElmMakeOutputType)
        (Json.Decode.field "enableDebug" Json.Decode.bool)


jsonEncodeElmMakeOutputType : CompileFullstackApp.ElmMakeOutputType -> Json.Encode.Value
jsonEncodeElmMakeOutputType elmMakeOutputType =
    case elmMakeOutputType of
        CompileFullstackApp.ElmMakeOutputTypeHtml ->
            [ ( "ElmMakeOutputTypeHtml", Json.Encode.list Json.Encode.string [] ) ]
                |> Json.Encode.object

        CompileFullstackApp.ElmMakeOutputTypeJs ->
            [ ( "ElmMakeOutputTypeJs", Json.Encode.list Json.Encode.string [] ) ]
                |> Json.Encode.object


jsonDecodeElmMakeOutputType : Json.Decode.Decoder CompileFullstackApp.ElmMakeOutputType
jsonDecodeElmMakeOutputType =
    Json.Decode.oneOf
        [ Json.Decode.field "ElmMakeOutputTypeHtml" (jsonDecodeSucceedWhenNotNull CompileFullstackApp.ElmMakeOutputTypeHtml)
        , Json.Decode.field "ElmMakeOutputTypeJs" (jsonDecodeSucceedWhenNotNull CompileFullstackApp.ElmMakeOutputTypeJs)
        ]


jsonEncodeAppCode : CompileFullstackApp.AppFiles -> Json.Encode.Value
jsonEncodeAppCode appCode =
    appCode
        |> Dict.toList
        |> Json.Encode.list jsonEncodeAppCodeEntry


jsonEncodeAppCodeEntry : ( List String, Bytes.Bytes ) -> Json.Encode.Value
jsonEncodeAppCodeEntry ( filePath, fileContent ) =
    [ ( "path", filePath |> Json.Encode.list Json.Encode.string )
    , ( "content", fileContent |> json_encode_Bytes )
    ]
        |> Json.Encode.object


jsonDecodeAppCode : Json.Decode.Decoder CompileFullstackApp.AppFiles
jsonDecodeAppCode =
    Json.Decode.list jsonDecodeAppCodeEntry
        |> Json.Decode.map Dict.fromList


jsonDecodeAppCodeEntry : Json.Decode.Decoder ( List String, Bytes.Bytes )
jsonDecodeAppCodeEntry =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "path" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "content" json_decode_Bytes)


json_encode_Result : (err -> Json.Encode.Value) -> (ok -> Json.Encode.Value) -> Result err ok -> Json.Encode.Value
json_encode_Result encodeErr encodeOk valueToEncode =
    case valueToEncode of
        Err valueToEncodeError ->
            [ ( "Err", [ valueToEncodeError ] |> Json.Encode.list encodeErr ) ] |> Json.Encode.object

        Ok valueToEncodeOk ->
            [ ( "Ok", [ valueToEncodeOk ] |> Json.Encode.list encodeOk ) ] |> Json.Encode.object


json_decode_Result : Json.Decode.Decoder err -> Json.Decode.Decoder ok -> Json.Decode.Decoder (Result err ok)
json_decode_Result decodeErr decodeOk =
    Json.Decode.oneOf
        [ Json.Decode.field "Err" (Json.Decode.index 0 decodeErr) |> Json.Decode.map Err
        , Json.Decode.field "Ok" (Json.Decode.index 0 decodeOk) |> Json.Decode.map Ok
        ]


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


{-| Support function-level dead code elimination (<https://elm-lang.org/blog/small-assets-without-the-headache>)
Elm code needed to inform the Elm compiler about our entry points.
-}
main : Program Int () String
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update =
            \_ stateBefore ->
                ( [ lowerForSourceFilesSerialized ""
                  , lowerForSourceFilesAndJsonCodersSerialized ""
                  , lowerForSourceFilesAndJsonCodersAndElmMakeSerialized ""
                  ]
                    |> always stateBefore
                , Cmd.none
                )
        , subscriptions = \_ -> Sub.none
        }
