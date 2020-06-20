module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    )

import Backend.HttpViaVolatileHost as HttpViaVolatileHost
import Backend.InterfaceToHost as InterfaceToHost
import Base64
import Bytes.Encode
import Json.Decode


type alias State =
    { volatileHostId : Maybe String
    , httpRequestToForward : Maybe InterfaceToHost.HttpRequestEventStructure
    }


processEvent : InterfaceToHost.AppEvent -> State -> ( State, InterfaceToHost.AppEventResponse )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.ArrivedAtTimeEvent _ ->
            ( stateBefore
            , { completeHttpResponses = []
              , startTasks = []
              , notifyWhenArrivedAtTime = Nothing
              }
            )

        InterfaceToHost.HttpRequestEvent httpRequestEvent ->
            let
                state =
                    { stateBefore | httpRequestToForward = Just httpRequestEvent }
            in
            ( state, state |> httpRequestForwardRequestsFromState )

        InterfaceToHost.TaskCompleteEvent taskComplete ->
            case taskComplete.taskResult of
                InterfaceToHost.CreateVolatileHostResponse createVolatileHostResponse ->
                    case createVolatileHostResponse of
                        Err _ ->
                            ( stateBefore
                            , { completeHttpResponses = []
                              , startTasks = []
                              , notifyWhenArrivedAtTime = Nothing
                              }
                            )

                        Ok { hostId } ->
                            let
                                state =
                                    { stateBefore | volatileHostId = Just hostId }
                            in
                            ( state, state |> httpRequestForwardRequestsFromState )

                InterfaceToHost.RequestToVolatileHostResponse requestToVolatileHostResponse ->
                    case stateBefore.httpRequestToForward of
                        Nothing ->
                            ( stateBefore
                            , { completeHttpResponses = []
                              , startTasks = []
                              , notifyWhenArrivedAtTime = Nothing
                              }
                            )

                        Just httpRequestToForward ->
                            let
                                bodyFromString =
                                    Bytes.Encode.string >> Bytes.Encode.encode >> Base64.fromBytes

                                httpResponse =
                                    case requestToVolatileHostResponse of
                                        Err _ ->
                                            { statusCode = 500
                                            , bodyAsBase64 = bodyFromString "Error running in volatile host."
                                            , headersToAdd = []
                                            }

                                        Ok requestToVolatileHostComplete ->
                                            case requestToVolatileHostComplete.exceptionToString of
                                                Just exceptionToString ->
                                                    { statusCode = 500
                                                    , bodyAsBase64 = bodyFromString ("Exception in volatile host: " ++ exceptionToString)
                                                    , headersToAdd = []
                                                    }

                                                Nothing ->
                                                    let
                                                        returnValueAsHttpResponseResult =
                                                            requestToVolatileHostComplete.returnValueToString
                                                                |> Maybe.withDefault ""
                                                                |> Json.Decode.decodeString HttpViaVolatileHost.decodeVolatileHostHttpResponse
                                                    in
                                                    case returnValueAsHttpResponseResult of
                                                        Err decodeError ->
                                                            { statusCode = 500
                                                            , bodyAsBase64 =
                                                                bodyFromString ("Error decoding response from volatile host: " ++ (decodeError |> Json.Decode.errorToString))
                                                            , headersToAdd = []
                                                            }

                                                        Ok volatileHostHttpResponse ->
                                                            let
                                                                headersToAdd =
                                                                    volatileHostHttpResponse.headers
                                                                        |> List.filter (.name >> String.toLower >> (/=) "transfer-encoding")
                                                            in
                                                            { statusCode = 200
                                                            , bodyAsBase64 = volatileHostHttpResponse.bodyAsBase64
                                                            , headersToAdd = headersToAdd
                                                            }

                                state =
                                    { stateBefore | httpRequestToForward = Nothing }
                            in
                            ( state
                            , { completeHttpResponses =
                                    [ { httpRequestId = httpRequestToForward.httpRequestId
                                      , response = httpResponse
                                      }
                                    ]
                              , startTasks = []
                              , notifyWhenArrivedAtTime = Nothing
                              }
                            )

                InterfaceToHost.CompleteWithoutResult ->
                    ( stateBefore
                    , { completeHttpResponses = []
                      , startTasks = []
                      , notifyWhenArrivedAtTime = Nothing
                      }
                    )


httpRequestForwardRequestsFromState : State -> InterfaceToHost.AppEventResponse
httpRequestForwardRequestsFromState state =
    case state.httpRequestToForward of
        Nothing ->
            { startTasks = []
            , completeHttpResponses = []
            , notifyWhenArrivedAtTime = Nothing
            }

        Just httpRequestToForward ->
            case state.volatileHostId of
                Nothing ->
                    { startTasks =
                        [ { taskId = "create-vhost"
                          , task = InterfaceToHost.CreateVolatileHost { script = HttpViaVolatileHost.volatileHostScript }
                          }
                        ]
                    , completeHttpResponses = []
                    , notifyWhenArrivedAtTime = Nothing
                    }

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
                            { completeHttpResponses =
                                [ { httpRequestId = httpRequestToForward.httpRequestId
                                  , response =
                                        { statusCode = 400
                                        , bodyAsBase64 =
                                            "Where to should I forward this HTTP request? Use the 'forward-to' HTTP header to specify a destination."
                                                |> Bytes.Encode.string
                                                |> Bytes.Encode.encode
                                                |> Base64.fromBytes
                                        , headersToAdd = []
                                        }
                                  }
                                ]
                            , startTasks = []
                            , notifyWhenArrivedAtTime = Nothing
                            }

                        Just forwardTo ->
                            let
                                httpRequest =
                                    { uri = forwardTo
                                    , method = httpRequestToForward.request.method
                                    , headers = httpRequestToForward.request.headers
                                    , bodyAsBase64 = httpRequestToForward.request.bodyAsBase64
                                    }

                                task =
                                    { hostId = volatileHostId
                                    , request =
                                        HttpViaVolatileHost.requestToVolatileHost httpRequest
                                    }
                                        |> InterfaceToHost.RequestToVolatileHost
                            in
                            { startTasks =
                                [ { taskId = "http-request-forward-" ++ httpRequestToForward.httpRequestId
                                  , task = task
                                  }
                                ]
                            , completeHttpResponses = []
                            , notifyWhenArrivedAtTime = Nothing
                            }


interfaceToHost_initState : State
interfaceToHost_initState =
    { volatileHostId = Nothing, httpRequestToForward = Nothing }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent
