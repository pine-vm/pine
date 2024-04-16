module Main exposing (..)

import Base64
import Bytes
import CompileBackendApp
import CompileElmApp exposing (CompilationArguments, ElmMakeEntryPointKind(..), EntryPointClass)
import Dict
import Elm.Syntax.Range
import Json.Decode
import Json.Encode
import Platform


type alias CompilationResponse =
    Result String (Result (List CompileElmApp.LocatedCompilationError) CompileElmApp.CompilationIterationSuccess)


lowerSerialized : String -> String
lowerSerialized argumentsJson =
    Json.Decode.decodeString jsonDecodeCompilationArguments argumentsJson
        |> Result.mapError (Json.Decode.errorToString >> (++) "Failed to decode arguments: ")
        |> Result.map (CompileElmApp.asCompletelyLoweredElmApp defaultEntryPoints)
        |> jsonEncodeLowerForSourceFilesResponse
        |> Json.Encode.encode 0


defaultEntryPoints : List EntryPointClass
defaultEntryPoints =
    CompileElmApp.defaultEntryPoints
        ++ CompileBackendApp.entryPoints


jsonEncodeLowerForSourceFilesResponse : CompilationResponse -> Json.Encode.Value
jsonEncodeLowerForSourceFilesResponse =
    json_encode_Result
        Json.Encode.string
        (json_encode_Result (Json.Encode.list jsonEncodeLocatedCompilationError) jsonEncodeCompilationIterationSuccess)


jsonDecodeCompilationArguments : Json.Decode.Decoder CompilationArguments
jsonDecodeCompilationArguments =
    Json.Decode.map5 CompilationArguments
        (Json.Decode.field "sourceFiles" jsonDecodeAppCode)
        (Json.Decode.field "compilationInterfaceElmModuleNamePrefixes" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "dependencies" (Json.Decode.list jsonDecodeCompilationArgumentsDependency))
        (Json.Decode.field "compilationRootFilePath" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "interfaceToHostRootModuleName" (Json.Decode.list Json.Decode.string))


jsonDecodeCompilationArgumentsDependency : Json.Decode.Decoder ( CompileElmApp.DependencyKey, Bytes.Bytes )
jsonDecodeCompilationArgumentsDependency =
    Json.Decode.map2 Tuple.pair
        (Json.Decode.field "key" jsonDecodeDependencyKey)
        (Json.Decode.field "value" json_decode_Bytes)


jsonEncodeLocatedCompilationError : CompileElmApp.LocatedCompilationError -> Json.Encode.Value
jsonEncodeLocatedCompilationError locatedCompilationError =
    case locatedCompilationError of
        CompileElmApp.LocatedInSourceFiles location error ->
            [ ( "location", jsonEncodeLocationInSourceFiles location )
            , ( "error", jsonEncodeCompilationError error )
            ]
                |> Json.Encode.object


jsonEncodeLocationInSourceFiles : CompileElmApp.LocationInSourceFiles -> Json.Encode.Value
jsonEncodeLocationInSourceFiles location =
    [ ( "filePath", location.filePath |> Json.Encode.list Json.Encode.string )
    , ( "locationInModuleText", location.locationInModuleText |> jsonEncodeRangeInText )
    ]
        |> Json.Encode.object


jsonEncodeRangeInText : Elm.Syntax.Range.Range -> Json.Encode.Value
jsonEncodeRangeInText range =
    [ ( "start", range.start |> jsonEncodeLocationInText )
    , ( "end", range.end |> jsonEncodeLocationInText )
    ]
        |> Json.Encode.object


jsonEncodeLocationInText : Elm.Syntax.Range.Location -> Json.Encode.Value
jsonEncodeLocationInText location =
    [ ( "row", location.row |> Json.Encode.int )
    , ( "column", location.column |> Json.Encode.int )
    ]
        |> Json.Encode.object


jsonEncodeCompilationIterationSuccess : CompileElmApp.CompilationIterationSuccess -> Json.Encode.Value
jsonEncodeCompilationIterationSuccess success =
    [ ( "compiledFiles", jsonEncodeAppCode success.compiledFiles )
    , ( "rootModuleEntryPointKind"
      , json_encode_Result Json.Encode.string jsonEncodeElmMakeEntryPointKind success.rootModuleEntryPointKind
      )
    ]
        |> Json.Encode.object


jsonEncodeElmMakeEntryPointKind : CompileElmApp.ElmMakeEntryPointKind -> Json.Encode.Value
jsonEncodeElmMakeEntryPointKind kind =
    case kind of
        CompileElmApp.ClassicMakeEntryPoint entryPoint ->
            Json.Encode.object
                [ ( "ClassicMakeEntryPoint"
                  , [ jsonEncodeElmMakeEntryPointStruct entryPoint ]
                        |> Json.Encode.list identity
                  )
                ]

        CompileElmApp.BlobMakeEntryPoint entryPoint ->
            Json.Encode.object
                [ ( "BlobMakeEntryPoint"
                  , [ jsonEncodeElmMakeEntryPointStruct entryPoint ]
                        |> Json.Encode.list identity
                  )
                ]


jsonEncodeElmMakeEntryPointStruct : CompileElmApp.ElmMakeEntryPointStruct -> Json.Encode.Value
jsonEncodeElmMakeEntryPointStruct struct =
    Json.Encode.object
        [ ( "elmMakeJavaScriptFunctionName"
          , Json.Encode.string struct.elmMakeJavaScriptFunctionName
          )
        ]


jsonEncodeCompilationError : CompileElmApp.CompilationError -> Json.Encode.Value
jsonEncodeCompilationError compilationError =
    case compilationError of
        CompileElmApp.MissingDependencyError missingDependencyKey ->
            [ ( "MissingDependencyError", Json.Encode.list identity [ jsonEncodeDependencyKey missingDependencyKey ] ) ]
                |> Json.Encode.object

        CompileElmApp.OtherCompilationError otherError ->
            [ ( "OtherCompilationError", Json.Encode.list identity [ Json.Encode.string otherError ] ) ]
                |> Json.Encode.object


jsonEncodeDependencyKey : CompileElmApp.DependencyKey -> Json.Encode.Value
jsonEncodeDependencyKey dependencyKey =
    case dependencyKey of
        CompileElmApp.ElmMakeDependency elmMakeRequest ->
            [ ( "ElmMakeDependency", Json.Encode.list identity [ jsonEncodeElmMakeRequest elmMakeRequest ] ) ]
                |> Json.Encode.object


jsonDecodeDependencyKey : Json.Decode.Decoder CompileElmApp.DependencyKey
jsonDecodeDependencyKey =
    Json.Decode.oneOf
        [ Json.Decode.field "ElmMakeDependency"
            (Json.Decode.index 0 (jsonDecodeElmMakeRequest |> Json.Decode.map CompileElmApp.ElmMakeDependency))
        ]


jsonEncodeElmMakeRequest : CompileElmApp.ElmMakeRequestStructure -> Json.Encode.Value
jsonEncodeElmMakeRequest elmMakeRequest =
    [ ( "files", jsonEncodeAppCode elmMakeRequest.files )
    , ( "entryPointFilePath", Json.Encode.list Json.Encode.string elmMakeRequest.entryPointFilePath )
    , ( "outputType", jsonEncodeElmMakeOutputType elmMakeRequest.outputType )
    , ( "enableDebug", Json.Encode.bool elmMakeRequest.enableDebug )
    ]
        |> Json.Encode.object


jsonDecodeElmMakeRequest : Json.Decode.Decoder CompileElmApp.ElmMakeRequestStructure
jsonDecodeElmMakeRequest =
    Json.Decode.map4 CompileElmApp.ElmMakeRequestStructure
        (Json.Decode.field "files" jsonDecodeAppCode)
        (Json.Decode.field "entryPointFilePath" (Json.Decode.list Json.Decode.string))
        (Json.Decode.field "outputType" jsonDecodeElmMakeOutputType)
        (Json.Decode.field "enableDebug" Json.Decode.bool)


jsonEncodeElmMakeOutputType : CompileElmApp.ElmMakeOutputType -> Json.Encode.Value
jsonEncodeElmMakeOutputType elmMakeOutputType =
    case elmMakeOutputType of
        CompileElmApp.ElmMakeOutputTypeHtml ->
            [ ( "ElmMakeOutputTypeHtml", Json.Encode.list Json.Encode.string [] ) ]
                |> Json.Encode.object

        CompileElmApp.ElmMakeOutputTypeJs ->
            [ ( "ElmMakeOutputTypeJs", Json.Encode.list Json.Encode.string [] ) ]
                |> Json.Encode.object


jsonDecodeElmMakeOutputType : Json.Decode.Decoder CompileElmApp.ElmMakeOutputType
jsonDecodeElmMakeOutputType =
    Json.Decode.oneOf
        [ Json.Decode.field "ElmMakeOutputTypeHtml" (jsonDecodeSucceedWhenNotNull CompileElmApp.ElmMakeOutputTypeHtml)
        , Json.Decode.field "ElmMakeOutputTypeJs" (jsonDecodeSucceedWhenNotNull CompileElmApp.ElmMakeOutputTypeJs)
        ]


jsonEncodeAppCode : CompileElmApp.AppFiles -> Json.Encode.Value
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


jsonDecodeAppCode : Json.Decode.Decoder CompileElmApp.AppFiles
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
        { init = always ( (), Cmd.none )
        , update =
            { a = lowerSerialized }
                |> always ( (), Cmd.none )
                |> always
                |> always
        , subscriptions = always Sub.none
        }
