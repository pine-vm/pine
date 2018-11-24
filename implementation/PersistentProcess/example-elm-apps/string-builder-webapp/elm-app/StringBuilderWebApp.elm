module StringBuilderWebApp exposing
    ( State
    , deserializeState
    , initState
    , main
    , processEvent
    , processSerializedEvent
    , serializeState
    )

import ElmAppInKalmitProcess
import Json.Decode
import Json.Encode
import Platform


type alias State =
    String


processSerializedEvent : String -> State -> ( State, String )
processSerializedEvent =
    ElmAppInKalmitProcess.wrapUpdateForSerialInterface processEvent


processEvent : ElmAppInKalmitProcess.KalmitProcessEvent -> State -> ( State, List ElmAppInKalmitProcess.KalmitProcessResponse )
processEvent hostEvent stateBefore =
    case hostEvent of
        ElmAppInKalmitProcess.HttpRequest httpRequestEvent ->
            let
                state =
                    case httpRequestEvent.request.method |> String.toLower of
                        "get" ->
                            stateBefore

                        "post" ->
                            stateBefore ++ (httpRequestEvent.request.bodyAsString |> Maybe.withDefault "")

                        _ ->
                            stateBefore

                httpResponse =
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response =
                        { statusCode = 200
                        , bodyAsString = Just state
                        }
                    }
                        |> ElmAppInKalmitProcess.CompleteHttpResponse
            in
            ( state, [ httpResponse ] )


serializeState : State -> String
serializeState =
    identity


deserializeState : String -> State
deserializeState =
    identity


initState : State
initState =
    ""



-- Support function-level dead code elimination (https://elm-lang.org/blog/small-assets-without-the-headache) Elm code needed to inform the Elm compiler about our entry points.


main : Program Int State String
main =
    Platform.worker
        { init = \_ -> ( initState, Cmd.none )
        , update =
            \event stateBefore ->
                processSerializedEvent event (stateBefore |> serializeState |> deserializeState) |> Tuple.mapSecond (always Cmd.none)
        , subscriptions = \_ -> Sub.none
        }
