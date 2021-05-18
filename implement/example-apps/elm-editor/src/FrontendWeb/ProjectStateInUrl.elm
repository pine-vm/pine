module FrontendWeb.ProjectStateInUrl exposing (..)

import Base64
import Bytes
import Bytes.Encode
import Common
import CompilationInterface.GenerateJsonCoders
import Flate
import Json.Decode
import Json.Encode
import LZ77
import Maybe.Extra
import ProjectState
import ProjectState_2021_01
import SHA256
import Url
import Url.Builder
import Url.Parser exposing ((<?>))
import Url.Parser.Query


type ProjectStateDescriptionInUrl
    = LiteralProjectState ProjectState.FileTreeNode
    | LinkProjectState String
    | DiffProjectState ProjectState_2021_01.ProjectState


projectStateQueryParameterName : String
projectStateQueryParameterName =
    "project-state"


projectStateDeflateBase64QueryParameterName : String
projectStateDeflateBase64QueryParameterName =
    "project-state-deflate-base64"


projectStateHashQueryParameterName : String
projectStateHashQueryParameterName =
    "project-state-hash"


filePathToOpenQueryParameterName : String
filePathToOpenQueryParameterName =
    "file-path-to-open"


setProjectStateInUrl : ProjectState.FileTreeNode -> Maybe ( String, ProjectState.FileTreeNode ) -> { filePathToOpen : Maybe (List String) } -> Url.Url -> Url.Url
setProjectStateInUrl projectState maybeProjectStateBase optionalParameters url =
    let
        projectStateDescription =
            case maybeProjectStateBase of
                Nothing ->
                    LiteralProjectState projectState

                Just ( projectStateBaseLink, projectStateBaseFileTree ) ->
                    if projectStateCompositionHash projectState == projectStateCompositionHash projectStateBaseFileTree then
                        LinkProjectState projectStateBaseLink

                    else
                        case
                            ProjectState.searchProjectStateDifference_2021_01
                                projectState
                                { baseComposition = projectStateBaseFileTree }
                        of
                            Err _ ->
                                LiteralProjectState projectState

                            Ok diffModel ->
                                DiffProjectState { base = projectStateBaseLink, differenceFromBase = diffModel }

        ( projectStateString, applyCompression ) =
            case projectStateDescription of
                LinkProjectState link ->
                    ( link, False )

                _ ->
                    ( Json.Encode.encode 0 (jsonEncodeProjectStateDescription projectStateDescription), True )

        projectStateQueryParameter =
            if applyCompression then
                let
                    projectStateStringBytes =
                        projectStateString
                            |> Bytes.Encode.string
                            |> Bytes.Encode.encode

                    compression =
                        Flate.WithWindowSize LZ77.maxWindowSize

                    deflatedWithStatic =
                        Flate.deflateWithOptions (Flate.Static compression) projectStateStringBytes

                    deflatedWithDynamic =
                        Flate.deflateWithOptions (Flate.Dynamic compression) projectStateStringBytes

                    deflatedBytes =
                        [ deflatedWithStatic, deflatedWithDynamic ]
                            |> List.sortBy Bytes.width
                            |> List.head
                            |> Maybe.withDefault deflatedWithStatic
                in
                Url.Builder.string
                    projectStateDeflateBase64QueryParameterName
                    (deflatedBytes |> Base64.fromBytes |> Maybe.withDefault "Failed to encode as base64")

            else
                Url.Builder.string projectStateQueryParameterName projectStateString

        optionalQueryParameters =
            case optionalParameters.filePathToOpen of
                Nothing ->
                    []

                Just filePathToOpen ->
                    [ Url.Builder.string filePathToOpenQueryParameterName (String.join "/" filePathToOpen) ]
    in
    Url.Builder.crossOrigin
        (Url.toString { url | path = "", query = Nothing, fragment = Nothing })
        []
        (projectStateQueryParameter
            :: Url.Builder.string projectStateHashQueryParameterName (projectStateCompositionHash projectState)
            :: optionalQueryParameters
        )
        |> Url.fromString
        |> Maybe.withDefault { url | host = "Error: Failed to parse URL from String" }


projectStateDescriptionFromUrl : Url.Url -> Maybe (Result String ProjectStateDescriptionInUrl)
projectStateDescriptionFromUrl url =
    let
        stringFromQueryParameterName paramName =
            { url | path = "" }
                |> Url.Parser.parse (Url.Parser.top <?> Url.Parser.Query.string paramName)
                |> Maybe.Extra.join

        continueWithProjectStateString projectStateString =
            if projectStateStringQualifiesAsLink projectStateString then
                Ok (LinkProjectState projectStateString)

            else
                Json.Decode.decodeString jsonDecodeProjectStateDescription projectStateString
                    |> Result.mapError Json.Decode.errorToString
    in
    case stringFromQueryParameterName projectStateQueryParameterName of
        Just projectStateString ->
            if projectStateStringQualifiesAsLink projectStateString then
                Just (Ok (LinkProjectState projectStateString))

            else
                Just (continueWithProjectStateString projectStateString)

        Nothing ->
            case stringFromQueryParameterName projectStateDeflateBase64QueryParameterName of
                Nothing ->
                    Nothing

                Just projectStateDeflateBase64 ->
                    projectStateDeflateBase64
                        |> Base64.toBytes
                        |> Result.fromMaybe "Failed to decode from base64"
                        |> Result.andThen (Flate.inflate >> Result.fromMaybe "Failed to inflate")
                        |> Result.andThen (Common.decodeBytesToString >> Result.fromMaybe "Failed to decode bytes to string")
                        |> Result.andThen continueWithProjectStateString
                        |> Just


projectStateStringQualifiesAsLink : String -> Bool
projectStateStringQualifiesAsLink projectStateString =
    String.startsWith "https://" projectStateString


filePathToOpenFromUrl : Url.Url -> Maybe String
filePathToOpenFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse (Url.Parser.top <?> Url.Parser.Query.string filePathToOpenQueryParameterName)
        |> Maybe.Extra.join


projectStateHashFromUrl : Url.Url -> Maybe String
projectStateHashFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse (Url.Parser.top <?> Url.Parser.Query.string projectStateHashQueryParameterName)
        |> Maybe.Extra.join


projectStateCompositionHash : ProjectState.FileTreeNode -> String
projectStateCompositionHash =
    ProjectState.compositionHashFromFileTreeNode >> SHA256.toHex


jsonEncodeProjectStateDescription : ProjectStateDescriptionInUrl -> Json.Encode.Value
jsonEncodeProjectStateDescription projectStateDescription =
    case projectStateDescription of
        LiteralProjectState literalProjectState ->
            Json.Encode.object
                [ ( "version_2020_12"
                  , CompilationInterface.GenerateJsonCoders.jsonEncodeProjectState_2020_12 literalProjectState
                  )
                ]

        LinkProjectState link ->
            Json.Encode.string link

        DiffProjectState diffProjectState ->
            Json.Encode.object
                [ ( "version_2021_01"
                  , CompilationInterface.GenerateJsonCoders.jsonEncodeProjectState_2021_01 diffProjectState
                  )
                ]


jsonDecodeProjectStateDescription : Json.Decode.Decoder ProjectStateDescriptionInUrl
jsonDecodeProjectStateDescription =
    Json.Decode.oneOf
        [ Json.Decode.field "version_2020_12"
            CompilationInterface.GenerateJsonCoders.jsonDecodeProjectState_2020_12
            |> Json.Decode.map LiteralProjectState
        , Json.Decode.field "version_2021_01"
            CompilationInterface.GenerateJsonCoders.jsonDecodeProjectState_2021_01
            |> Json.Decode.map DiffProjectState
        ]


projectStateStringFromUrl : Url.Url -> Maybe String
projectStateStringFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse
            (Url.Parser.top <?> Url.Parser.Query.string projectStateQueryParameterName)
        |> Maybe.Extra.join
