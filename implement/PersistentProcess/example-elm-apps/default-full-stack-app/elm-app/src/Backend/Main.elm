module Backend.Main exposing
    ( State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    , main
    , processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Json.Decode
import Json.Encode
import Platform


type alias State =
    { httpRequestsCount : Int }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent


processEvent : InterfaceToHost.ProcessEvent -> State -> ( State, List InterfaceToHost.ProcessRequest )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequest httpRequestEvent ->
            let
                state =
                    { stateBefore | httpRequestsCount = stateBefore.httpRequestsCount + 1 }

                httpResponse =
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response =
                        { statusCode = 200
                        , bodyAsString =
                            Just
                                ("You are seeing the default configuration for the web app."
                                    ++ "\nFor information on how to configure the web app, see https://github.com/elm-fullstack/elm-fullstack"
                                    ++ "\nThis web app received "
                                    ++ (state.httpRequestsCount |> String.fromInt)
                                    ++ " HTTP requests."
                                )
                        , headersToAdd = []
                        }
                    }
                        |> InterfaceToHost.CompleteHttpResponse
            in
            ( state, [ httpResponse ] )

        InterfaceToHost.TaskComplete _ ->
            ( stateBefore, [] )


interfaceToHost_serializeState : State -> String
interfaceToHost_serializeState =
    .httpRequestsCount >> String.fromInt


interfaceToHost_deserializeState : String -> State
interfaceToHost_deserializeState serializedState =
    { httpRequestsCount = serializedState |> String.toInt |> Maybe.withDefault 0 }


interfaceToHost_initState : State
interfaceToHost_initState =
    { httpRequestsCount = 0 }



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
