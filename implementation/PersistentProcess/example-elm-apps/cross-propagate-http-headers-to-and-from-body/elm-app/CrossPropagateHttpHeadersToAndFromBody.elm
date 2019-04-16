module CrossPropagateHttpHeadersToAndFromBody exposing
    ( State
    , deserializeState
    , initState
    , main
    , processEvent
    , processSerializedEvent
    , serializeState
    )

import ElmAppInKalmitProcess
import Json.Encode
import Platform


type alias State =
    ()


processSerializedEvent : String -> State -> ( State, String )
processSerializedEvent =
    ElmAppInKalmitProcess.wrapUpdateForSerialInterface processEvent


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


serializeState : State -> String
serializeState =
    always ""


deserializeState : String -> State
deserializeState =
    always ()


initState : State
initState =
    ()



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
