module Main exposing
    ( State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    , main
    , processEvent
    )

import ElmAppInKalmitProcess
import Json.Decode
import Json.Encode
import Platform


type alias State =
    String


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
                        , bodyAsString =
                            Just
                                ("The request uri was: "
                                    ++ httpRequestEvent.request.uri
                                    ++ "\nThe current state is:"
                                    ++ state
                                )
                        , headersToAdd = []
                        }
                    }
                        |> ElmAppInKalmitProcess.CompleteHttpResponse
            in
            ( state, [ httpResponse ] )


interfaceToHost_serializeState : State -> String
interfaceToHost_serializeState =
    identity


interfaceToHost_deserializeState : String -> State
interfaceToHost_deserializeState =
    identity


interfaceToHost_initState : State
interfaceToHost_initState =
    ""


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    ElmAppInKalmitProcess.wrapUpdateForSerialInterface processEvent



-- Support function-level dead code elimination (https://elm-lang.org/blog/small-assets-without-the-headache) Elm code needed to inform the Elm compiler about our entry points.


main : Program Int State String
main =
    Platform.worker
        { init = \_ -> ( interfaceToHost_initState, Cmd.none )
        , update =
            \event stateBefore ->
                interfaceToHost_processEvent event (stateBefore |> interfaceToHost_serializeState |> interfaceToHost_deserializeState) |> Tuple.mapSecond (always Cmd.none)
        , subscriptions = \_ -> Sub.none
        }
