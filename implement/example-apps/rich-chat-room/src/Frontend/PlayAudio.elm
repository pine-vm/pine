module Frontend.PlayAudio exposing (PlaybackState, State, init, renderHtml, startPlayback)

import Dict
import Html
import Html.Attributes as HA
import Json.Encode


type alias PlaybackId =
    Int


type alias PlaybackState =
    { sourceUrl : String
    , volume : Float
    }


type alias State =
    Dict.Dict PlaybackId PlaybackState


init : State
init =
    Dict.empty


startPlayback : PlaybackState -> State -> ( State, PlaybackId )
startPlayback playback stateBefore =
    let
        nextId =
            (stateBefore |> Dict.keys |> List.maximum |> Maybe.withDefault 0) + 1
    in
    ( stateBefore |> Dict.insert nextId playback, nextId )


renderHtml : State -> Html.Html a
renderHtml state =
    state
        |> Dict.values
        |> List.map renderPlaybackHtml
        |> Html.div [ HA.style "display" "none" ]


renderPlaybackHtml : PlaybackState -> Html.Html a
renderPlaybackHtml playback =
    Html.audio
        [ HA.controls False, HA.autoplay True, HA.property "volume" (Json.Encode.string (String.fromFloat playback.volume)) ]
        [ Html.source [ HA.src playback.sourceUrl ] [] ]
