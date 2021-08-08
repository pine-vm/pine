module Backend.Main exposing
    ( State
    , backendMain
    )

import Backend.HttpViaVolatileProcess as HttpViaVolatileProcess
import Base64
import Bytes.Encode
import ElmFullstack
import Json.Decode


type alias State =
    { volatileProcessId : Maybe String
    , httpRequestToForward : Maybe ElmFullstack.HttpRequestEventStructure
    }


backendMain : ElmFullstack.BackendConfiguration State
backendMain =
    { init = { volatileProcessId = Nothing, httpRequestToForward = Nothing }
    , update = processEvent
    }


processEvent : ElmFullstack.BackendEvent -> State -> ( State, ElmFullstack.BackendEventResponse )
processEvent hostEvent stateBefore =
    case hostEvent of
        ElmFullstack.PosixTimeHasArrivedEvent _ ->
            ( stateBefore
            , ElmFullstack.passiveBackendEventResponse
            )

        ElmFullstack.HttpRequestEvent httpRequestEvent ->
            let
                state =
                    { stateBefore | httpRequestToForward = Just httpRequestEvent }
            in
            ( state, state |> httpRequestForwardRequestsFromState )

        ElmFullstack.TaskCompleteEvent taskComplete ->
            case taskComplete.taskResult of
                ElmFullstack.CreateVolatileProcessResponse createVolatileProcessResponse ->
                    case createVolatileProcessResponse of
                        Err _ ->
                            ( stateBefore
                            , ElmFullstack.passiveBackendEventResponse
                            )

                        Ok { processId } ->
                            let
                                state =
                                    { stateBefore | volatileProcessId = Just processId }
                            in
                            ( state, state |> httpRequestForwardRequestsFromState )

                ElmFullstack.RequestToVolatileProcessResponse requestToVolatileProcessResponse ->
                    case stateBefore.httpRequestToForward of
                        Nothing ->
                            ( stateBefore
                            , ElmFullstack.passiveBackendEventResponse
                            )

                        Just httpRequestToForward ->
                            let
                                bodyFromString =
                                    Bytes.Encode.string >> Bytes.Encode.encode >> Base64.fromBytes

                                httpResponse =
                                    case requestToVolatileProcessResponse of
                                        Err _ ->
                                            { statusCode = 500
                                            , bodyAsBase64 = bodyFromString "Error running in volatile process."
                                            , headersToAdd = []
                                            }

                                        Ok requestToVolatileHostComplete ->
                                            case requestToVolatileHostComplete.exceptionToString of
                                                Just exceptionToString ->
                                                    { statusCode = 500
                                                    , bodyAsBase64 = bodyFromString ("Exception in volatile process: " ++ exceptionToString)
                                                    , headersToAdd = []
                                                    }

                                                Nothing ->
                                                    let
                                                        returnValueAsHttpResponseResult =
                                                            requestToVolatileHostComplete.returnValueToString
                                                                |> Maybe.withDefault ""
                                                                |> Json.Decode.decodeString HttpViaVolatileProcess.decodeVolatileProcessHttpResponse
                                                    in
                                                    case returnValueAsHttpResponseResult of
                                                        Err decodeError ->
                                                            { statusCode = 500
                                                            , bodyAsBase64 =
                                                                bodyFromString ("Error decoding response from volatile process: " ++ (decodeError |> Json.Decode.errorToString))
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
                            , ElmFullstack.passiveBackendEventResponse
                                |> ElmFullstack.withCompleteHttpResponsesAdded
                                    [ { httpRequestId = httpRequestToForward.httpRequestId
                                      , response = httpResponse
                                      }
                                    ]
                            )

                ElmFullstack.CompleteWithoutResult ->
                    ( stateBefore
                    , ElmFullstack.passiveBackendEventResponse
                    )


httpRequestForwardRequestsFromState : State -> ElmFullstack.BackendEventResponse
httpRequestForwardRequestsFromState state =
    case state.httpRequestToForward of
        Nothing ->
            ElmFullstack.passiveBackendEventResponse

        Just httpRequestToForward ->
            case state.volatileProcessId of
                Nothing ->
                    ElmFullstack.passiveBackendEventResponse
                        |> ElmFullstack.withStartTasksAdded
                            [ { taskId = "create-volatile-process"
                              , task = ElmFullstack.CreateVolatileProcess { programCode = HttpViaVolatileProcess.programCode }
                              }
                            ]

                Just volatileProcessId ->
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
                            ElmFullstack.passiveBackendEventResponse
                                |> ElmFullstack.withCompleteHttpResponsesAdded
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

                        Just forwardTo ->
                            let
                                httpRequest =
                                    { uri = forwardTo
                                    , method = httpRequestToForward.request.method
                                    , headers = httpRequestToForward.request.headers
                                    , bodyAsBase64 = httpRequestToForward.request.bodyAsBase64
                                    }

                                task =
                                    { processId = volatileProcessId
                                    , request =
                                        HttpViaVolatileProcess.requestToVolatileProcess httpRequest
                                    }
                                        |> ElmFullstack.RequestToVolatileProcess
                            in
                            ElmFullstack.passiveBackendEventResponse
                                |> ElmFullstack.withStartTasksAdded
                                    [ { taskId = "http-request-forward-" ++ httpRequestToForward.httpRequestId
                                      , task = task
                                      }
                                    ]
