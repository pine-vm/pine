module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Backend.VolatileHost as VolatileHost
import Base64
import Bytes.Encode


type alias State =
    { volatileHostId : Maybe String
    , pendingHttpRequest : Maybe InterfaceToHost.HttpRequestEventStructure
    }


processEvent : InterfaceToHost.AppEvent -> State -> ( State, InterfaceToHost.AppEventResponse )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.ArrivedAtTimeEvent _ ->
            ( stateBefore
            , InterfaceToHost.passiveAppEventResponse
            )

        InterfaceToHost.HttpRequestEvent httpRequestEvent ->
            let
                state =
                    { stateBefore | pendingHttpRequest = Just httpRequestEvent }
            in
            ( state, state |> tasksToVolatileHostFromState )

        InterfaceToHost.TaskCompleteEvent taskComplete ->
            let
                bodyFromString =
                    Bytes.Encode.string >> Bytes.Encode.encode >> Base64.fromBytes

                httpResponseInternalServerError errorMessage =
                    { statusCode = 500
                    , bodyAsBase64 = bodyFromString errorMessage
                    , headersToAdd = []
                    }
            in
            case taskComplete.taskResult of
                InterfaceToHost.CreateVolatileHostResponse createVolatileHostResponse ->
                    case createVolatileHostResponse of
                        Err error ->
                            let
                                httpResponses =
                                    case stateBefore.pendingHttpRequest of
                                        Nothing ->
                                            []

                                        Just pendingHttpRequest ->
                                            [ { httpRequestId = pendingHttpRequest.httpRequestId
                                              , response =
                                                    httpResponseInternalServerError
                                                        ("Failed to create volatile host: " ++ error.exceptionToString)
                                              }
                                            ]
                            in
                            ( stateBefore
                            , InterfaceToHost.passiveAppEventResponse |> InterfaceToHost.withCompleteHttpResponsesAdded httpResponses
                            )

                        Ok { hostId } ->
                            let
                                state =
                                    { stateBefore | volatileHostId = Just hostId }
                            in
                            ( state, state |> tasksToVolatileHostFromState )

                InterfaceToHost.RequestToVolatileHostResponse requestToVolatileHostResponse ->
                    case stateBefore.pendingHttpRequest of
                        Nothing ->
                            ( stateBefore
                            , InterfaceToHost.passiveAppEventResponse
                            )

                        Just pendingHttpRequest ->
                            let
                                httpResponse =
                                    case requestToVolatileHostResponse of
                                        Err _ ->
                                            httpResponseInternalServerError "Error running in volatile host."

                                        Ok requestToVolatileHostComplete ->
                                            case requestToVolatileHostComplete.exceptionToString of
                                                Just exceptionToString ->
                                                    httpResponseInternalServerError ("Exception in volatile host: " ++ exceptionToString)

                                                Nothing ->
                                                    { statusCode = 200
                                                    , bodyAsBase64 = Maybe.andThen bodyFromString requestToVolatileHostComplete.returnValueToString
                                                    , headersToAdd = []
                                                    }

                                state =
                                    { stateBefore | pendingHttpRequest = Nothing }
                            in
                            ( state
                            , InterfaceToHost.passiveAppEventResponse
                                |> InterfaceToHost.withCompleteHttpResponsesAdded
                                    [ { httpRequestId = pendingHttpRequest.httpRequestId
                                      , response = httpResponse
                                      }
                                    ]
                            )

                InterfaceToHost.CompleteWithoutResult ->
                    ( stateBefore
                    , InterfaceToHost.passiveAppEventResponse
                    )


tasksToVolatileHostFromState : State -> InterfaceToHost.AppEventResponse
tasksToVolatileHostFromState state =
    case state.pendingHttpRequest of
        Nothing ->
            InterfaceToHost.passiveAppEventResponse

        Just pendingHttpRequest ->
            case state.volatileHostId of
                Nothing ->
                    InterfaceToHost.passiveAppEventResponse
                        |> InterfaceToHost.withStartTasksAdded
                            [ { taskId = "create-vhost"
                              , task = InterfaceToHost.CreateVolatileHost { script = VolatileHost.volatileHostScript }
                              }
                            ]

                Just volatileHostId ->
                    let
                        task =
                            { hostId = volatileHostId
                            , request = ""
                            }
                                |> InterfaceToHost.RequestToVolatileHost
                    in
                    InterfaceToHost.passiveAppEventResponse
                        |> InterfaceToHost.withStartTasksAdded
                            [ { taskId = "http-request-" ++ pendingHttpRequest.httpRequestId
                              , task = task
                              }
                            ]


interfaceToHost_initState : State
interfaceToHost_initState =
    { volatileHostId = Nothing, pendingHttpRequest = Nothing }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent
