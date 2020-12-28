module FrontendWeb.ProjectStateInUrl exposing (..)

import ElmFullstackCompilerInterface.GenerateJsonCoders
import Json.Decode
import Json.Encode
import Maybe.Extra
import ProjectState exposing (ProjectState)
import SHA256
import Url
import Url.Builder
import Url.Parser exposing ((<?>))
import Url.Parser.Query


projectStateQueryParameterName : String
projectStateQueryParameterName =
    "project-state"


projectStateCompositionHashQueryParameterName : String
projectStateCompositionHashQueryParameterName =
    "project-state-composition-hash"


setProjectStateInUrl : ProjectState -> Url.Url -> Url.Url
setProjectStateInUrl projectState url =
    Url.Builder.crossOrigin
        (Url.toString { url | path = "", query = Nothing, fragment = Nothing })
        []
        [ Url.Builder.string projectStateQueryParameterName (Json.Encode.encode 0 (jsonEncodeProjectState projectState))
        , Url.Builder.string projectStateCompositionHashQueryParameterName (projectStateCompositionHash projectState)
        ]
        |> Url.fromString
        |> Maybe.withDefault { url | host = "Error: Failed to parse URL from String" }


projectStateFromUrl : Url.Url -> Maybe (Result Json.Decode.Error ProjectState)
projectStateFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse
            (Url.Parser.top <?> Url.Parser.Query.string projectStateQueryParameterName)
        |> Maybe.Extra.join
        |> Maybe.map (Json.Decode.decodeString jsonDecodeProjectState)


projectStateCompositionHash : ProjectState -> String
projectStateCompositionHash =
    ProjectState.compositionHashFromFileTreeNode >> SHA256.toHex


jsonEncodeProjectState : ProjectState -> Json.Encode.Value
jsonEncodeProjectState project =
    Json.Encode.object
        [ ( "version_2020_12", ElmFullstackCompilerInterface.GenerateJsonCoders.jsonEncodeProjectState_2020_12 project )
        ]


jsonDecodeProjectState : Json.Decode.Decoder ProjectState
jsonDecodeProjectState =
    Json.Decode.oneOf
        [ Json.Decode.field "version_2020_12" ElmFullstackCompilerInterface.GenerateJsonCoders.jsonDecodeProjectState_2020_12
        ]


projectStateStringFromUrl : Url.Url -> Maybe String
projectStateStringFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse
            (Url.Parser.top <?> Url.Parser.Query.string projectStateQueryParameterName)
        |> Maybe.Extra.join
