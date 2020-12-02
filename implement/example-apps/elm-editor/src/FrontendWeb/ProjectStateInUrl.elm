module FrontendWeb.ProjectStateInUrl exposing (..)

import ElmFullstackCompilerInterface.GenerateJsonCoders
import Json.Decode
import Json.Encode
import Maybe.Extra
import ProjectState exposing (ProjectState)
import Url
import Url.Builder
import Url.Parser exposing ((<?>))
import Url.Parser.Query


projectStateQueryParameterName : String
projectStateQueryParameterName =
    "project-state"


setProjectStateInUrl : ProjectState -> Url.Url -> Url.Url
setProjectStateInUrl projectState url =
    Url.Builder.crossOrigin
        (Url.toString { url | path = "", query = Nothing, fragment = Nothing })
        []
        [ Url.Builder.string projectStateQueryParameterName (Json.Encode.encode 0 (encodeProjectState projectState)) ]
        |> Url.fromString
        |> Maybe.withDefault url


projectStateFromUrl : Url.Url -> Maybe (Result Json.Decode.Error ProjectState)
projectStateFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse
            (Url.Parser.top <?> Url.Parser.Query.string projectStateQueryParameterName)
        |> Maybe.Extra.join
        |> Maybe.map (Json.Decode.decodeString decodeProjectState)


encodeProjectState : ProjectState -> Json.Encode.Value
encodeProjectState =
    ElmFullstackCompilerInterface.GenerateJsonCoders.jsonEncodeProjectState_2020_12


decodeProjectState : Json.Decode.Decoder ProjectState
decodeProjectState =
    ElmFullstackCompilerInterface.GenerateJsonCoders.jsonDecodeProjectState_2020_12


projectStateStringFromUrl : Url.Url -> Maybe String
projectStateStringFromUrl url =
    { url | path = "" }
        |> Url.Parser.parse
            (Url.Parser.top <?> Url.Parser.Query.string projectStateQueryParameterName)
        |> Maybe.Extra.join
