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
import Json.Encode
import Platform


type alias State =
    ()


processEvent : ElmAppInKalmitProcess.KalmitProcessEvent -> State -> ( State, List ElmAppInKalmitProcess.KalmitProcessResponse )
processEvent hostEvent stateBefore =
    case hostEvent of
        ElmAppInKalmitProcess.HttpRequest httpRequestEvent ->
            let
                headerToPropagateBody =
                    { name = "response-header-name"
                    , values = [ httpRequestEvent.request.bodyAsString |> Maybe.withDefault "" ]
                    }

                responseBody =
                    httpRequestEvent.request.headers
                        |> Json.Encode.list
                            (\requestHeader ->
                                [ ( "name", requestHeader.name |> Json.Encode.string )
                                , ( "values", requestHeader.values |> Json.Encode.list Json.Encode.string )
                                ]
                                    |> Json.Encode.object
                            )
                        |> Json.Encode.encode 0

                httpResponse =
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response =
                        { statusCode = 200
                        , bodyAsString = Just responseBody
                        , headersToAdd = [ headerToPropagateBody ]
                        }
                    }
                        |> ElmAppInKalmitProcess.CompleteHttpResponse
            in
            ( stateBefore, [ httpResponse ] )


interfaceToHost_serializeState : State -> String
interfaceToHost_serializeState =
    always ""


interfaceToHost_deserializeState : String -> State
interfaceToHost_deserializeState =
    always ()


interfaceToHost_initState : State
interfaceToHost_initState =
    ()


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
