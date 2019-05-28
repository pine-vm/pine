module Main exposing
    ( State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    , main
    )

import ElmAppInKalmitProcess
import HttpViaVolatileHost
import Json.Decode
import Json.Encode
import Platform


type alias State =
    { volatileHostId : Maybe String
    , httpRequestToForward : Maybe ElmAppInKalmitProcess.HttpRequestEvent
    }


processEvent : ElmAppInKalmitProcess.ProcessEvent -> State -> ( State, List ElmAppInKalmitProcess.ProcessRequest )
processEvent hostEvent stateBefore =
    case hostEvent of
        ElmAppInKalmitProcess.HttpRequest httpRequestEvent ->
            let
                state =
                    { stateBefore | httpRequestToForward = Just httpRequestEvent }
            in
            ( state, state |> httpRequestForwardRequestsFromState )

        ElmAppInKalmitProcess.TaskComplete taskWithIdResult ->
            case taskWithIdResult.taskResult of
                ElmAppInKalmitProcess.CreateVolatileHostResponse createVolatileHostResponse ->
                    case createVolatileHostResponse of
                        Err _ ->
                            ( stateBefore, [] )

                        Ok { hostId } ->
                            let
                                state =
                                    { stateBefore | volatileHostId = Just hostId }
                            in
                            ( state, state |> httpRequestForwardRequestsFromState )

                ElmAppInKalmitProcess.RunInVolatileHostResponse runInVolatileHostResponse ->
                    case runInVolatileHostResponse of
                        Err _ ->
                            ( stateBefore, [] )

                        Ok runInVolatileHostComplete ->
                            let
                                returnValueAsHttpResponseResult =
                                    runInVolatileHostComplete.returnValueToString
                                        |> Maybe.withDefault ""
                                        |> Json.Decode.decodeString HttpViaVolatileHost.decodeVolatileHostHttpResponse
                            in
                            case returnValueAsHttpResponseResult of
                                Err _ ->
                                    ( stateBefore, [] )

                                Ok volatileHostHttpResponse ->
                                    let
                                        requests =
                                            case stateBefore.httpRequestToForward of
                                                Nothing ->
                                                    []

                                                Just httpRequestToForward ->
                                                    [ ElmAppInKalmitProcess.CompleteHttpResponse
                                                        { httpRequestId = httpRequestToForward.httpRequestId
                                                        , response =
                                                            { statusCode = 200
                                                            , bodyAsString = volatileHostHttpResponse.bodyAsString
                                                            , headersToAdd = volatileHostHttpResponse.headers
                                                            }
                                                        }
                                                    ]

                                        state =
                                            { stateBefore | httpRequestToForward = Nothing }
                                    in
                                    ( state, requests )

                ElmAppInKalmitProcess.CompleteWithoutResult ->
                    ( stateBefore, [] )


httpRequestForwardRequestsFromState : State -> List ElmAppInKalmitProcess.ProcessRequest
httpRequestForwardRequestsFromState state =
    case state.httpRequestToForward of
        Nothing ->
            []

        Just httpRequestToForward ->
            case state.volatileHostId of
                Nothing ->
                    [ ElmAppInKalmitProcess.StartTask
                        { taskId = "create-vhost", task = ElmAppInKalmitProcess.CreateVolatileHost }
                    ]

                Just volatileHostId ->
                    let
                        maybeForwardTo : Maybe String
                        maybeForwardTo =
                            httpRequestToForward.request.headers
                                |> List.filter (.name >> (==) "forward-to")
                                |> List.map (.values >> List.head)
                                |> List.head
                                |> Maybe.andThen identity
                    in
                    case maybeForwardTo of
                        Nothing ->
                            [ ElmAppInKalmitProcess.CompleteHttpResponse
                                { httpRequestId = httpRequestToForward.httpRequestId
                                , response =
                                    { statusCode = 400
                                    , bodyAsString = Just "Where to should I forward this HTTP request? Use the 'forward-to' HTTP header to specify a destination."
                                    , headersToAdd = []
                                    }
                                }
                            ]

                        Just forwardTo ->
                            let
                                httpRequest =
                                    { uri = forwardTo
                                    , method = httpRequestToForward.request.method
                                    , headers = httpRequestToForward.request.headers
                                    , bodyAsString = httpRequestToForward.request.bodyAsString
                                    }

                                task =
                                    { hostId = volatileHostId
                                    , script =
                                        HttpViaVolatileHost.scriptToGetResponseFromHttpRequest httpRequest
                                    }
                                        |> ElmAppInKalmitProcess.RunInVolatileHost
                            in
                            [ ElmAppInKalmitProcess.StartTask
                                { taskId = "http-request-forward-" ++ httpRequestToForward.httpRequestId
                                , task = task
                                }
                            ]


interfaceToHost_initState : State
interfaceToHost_initState =
    { volatileHostId = Nothing, httpRequestToForward = Nothing }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    ElmAppInKalmitProcess.wrapForSerialInterface_processEvent processEvent


interfaceToHost_serializeState : State -> String
interfaceToHost_serializeState =
    always ""


interfaceToHost_deserializeState : String -> State
interfaceToHost_deserializeState =
    always interfaceToHost_initState



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
