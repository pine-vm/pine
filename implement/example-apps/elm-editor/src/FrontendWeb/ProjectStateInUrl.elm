module FrontendWeb.ProjectStateInUrl exposing (..)

import ElmFullstackCompilerInterface.GenerateJsonCoders
import Json.Decode
import Json.Encode
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


projectStateCompositionHashQueryParameterName : String
projectStateCompositionHashQueryParameterName =
    "project-state-composition-hash"


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

        projectStateQueryParameter =
            case projectStateDescription of
                LinkProjectState link ->
                    link

                _ ->
                    Json.Encode.encode 0 (jsonEncodeProjectStateDescription projectStateDescription)

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
        (Url.Builder.string projectStateQueryParameterName projectStateQueryParameter
            :: Url.Builder.string projectStateCompositionHashQueryParameterName (projectStateCompositionHash projectState)
            :: optionalQueryParameters
        )
        |> Url.fromString
        |> Maybe.withDefault { url | host = "Error: Failed to parse URL from String" }


projectStateDescriptionFromUrl : Url.Url -> Maybe (Result Json.Decode.Error ProjectStateDescriptionInUrl)
projectStateDescriptionFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse
            (Url.Parser.top <?> Url.Parser.Query.string projectStateQueryParameterName)
        |> Maybe.Extra.join
        |> Maybe.map
            (\projectStateString ->
                if projectStateStringQualifiesAsLink projectStateString then
                    Ok (LinkProjectState projectStateString)

                else
                    Json.Decode.decodeString jsonDecodeProjectStateDescription projectStateString
            )


projectStateStringQualifiesAsLink : String -> Bool
projectStateStringQualifiesAsLink projectStateString =
    String.startsWith "https://" projectStateString


filePathToOpenFromUrl : Url.Url -> Maybe String
filePathToOpenFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse (Url.Parser.top <?> Url.Parser.Query.string filePathToOpenQueryParameterName)
        |> Maybe.Extra.join


projectStateCompositionHashFromUrl : Url.Url -> Maybe String
projectStateCompositionHashFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse (Url.Parser.top <?> Url.Parser.Query.string projectStateCompositionHashQueryParameterName)
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
                  , ElmFullstackCompilerInterface.GenerateJsonCoders.jsonEncodeProjectState_2020_12 literalProjectState
                  )
                ]

        LinkProjectState link ->
            Json.Encode.string link

        DiffProjectState diffProjectState ->
            Json.Encode.object
                [ ( "version_2021_01"
                  , ElmFullstackCompilerInterface.GenerateJsonCoders.jsonEncodeProjectState_2021_01 diffProjectState
                  )
                ]


jsonDecodeProjectStateDescription : Json.Decode.Decoder ProjectStateDescriptionInUrl
jsonDecodeProjectStateDescription =
    Json.Decode.oneOf
        [ Json.Decode.field "version_2020_12"
            ElmFullstackCompilerInterface.GenerateJsonCoders.jsonDecodeProjectState_2020_12
            |> Json.Decode.map LiteralProjectState
        , Json.Decode.field "version_2021_01"
            ElmFullstackCompilerInterface.GenerateJsonCoders.jsonDecodeProjectState_2021_01
            |> Json.Decode.map DiffProjectState
        ]


projectStateStringFromUrl : Url.Url -> Maybe String
projectStateStringFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse
            (Url.Parser.top <?> Url.Parser.Query.string projectStateQueryParameterName)
        |> Maybe.Extra.join
