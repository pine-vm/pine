module Backend.Main exposing
    ( State
    , backendMain
    )

import Backend.VolatileProcess
import Base64
import Bytes.Encode
import ElmFullstack


type alias State =
    { volatileProcessId : Maybe String
    , pendingHttpRequest : Maybe ElmFullstack.HttpRequestEventStructure
    }


backendMain : ElmFullstack.BackendConfiguration State
backendMain =
    { init = { volatileProcessId = Nothing, pendingHttpRequest = Nothing }
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
                    { stateBefore | pendingHttpRequest = Just httpRequestEvent }
            in
            ( state, state |> tasksToVolatileProcessFromState )

        ElmFullstack.TaskCompleteEvent taskComplete ->
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
                ElmFullstack.CreateVolatileProcessResponse createVolatileProcessResponse ->
                    case createVolatileProcessResponse of
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
                                                        ("Failed to create volatile process: " ++ error.exceptionToString)
                                              }
                                            ]
                            in
                            ( stateBefore
                            , ElmFullstack.passiveBackendEventResponse |> ElmFullstack.withCompleteHttpResponsesAdded httpResponses
                            )

                        Ok { processId } ->
                            let
                                state =
                                    { stateBefore | volatileProcessId = Just processId }
                            in
                            ( state, state |> tasksToVolatileProcessFromState )

                ElmFullstack.RequestToVolatileProcessResponse requestToVolatileProcessResponse ->
                    case stateBefore.pendingHttpRequest of
                        Nothing ->
                            ( stateBefore
                            , ElmFullstack.passiveBackendEventResponse
                            )

                        Just pendingHttpRequest ->
                            let
                                httpResponse =
                                    case requestToVolatileProcessResponse of
                                        Err _ ->
                                            httpResponseInternalServerError "Error running in volatile process."

                                        Ok requestToVolatileProcessComplete ->
                                            case requestToVolatileProcessComplete.exceptionToString of
                                                Just exceptionToString ->
                                                    httpResponseInternalServerError ("Exception in volatile process: " ++ exceptionToString)

                                                Nothing ->
                                                    { statusCode = 200
                                                    , bodyAsBase64 = Maybe.andThen bodyFromString requestToVolatileProcessComplete.returnValueToString
                                                    , headersToAdd = []
                                                    }

                                state =
                                    { stateBefore | pendingHttpRequest = Nothing }
                            in
                            ( state
                            , ElmFullstack.passiveBackendEventResponse
                                |> ElmFullstack.withCompleteHttpResponsesAdded
                                    [ { httpRequestId = pendingHttpRequest.httpRequestId
                                      , response = httpResponse
                                      }
                                    ]
                            )

                ElmFullstack.CompleteWithoutResult ->
                    ( stateBefore
                    , ElmFullstack.passiveBackendEventResponse
                    )


tasksToVolatileProcessFromState : State -> ElmFullstack.BackendEventResponse
tasksToVolatileProcessFromState state =
    case state.pendingHttpRequest of
        Nothing ->
            ElmFullstack.passiveBackendEventResponse

        Just pendingHttpRequest ->
            case state.volatileProcessId of
                Nothing ->
                    ElmFullstack.passiveBackendEventResponse
                        |> ElmFullstack.withStartTasksAdded
                            [ { taskId = "create-volatile-process"
                              , task =
                                    ElmFullstack.CreateVolatileProcess
                                        { programCode = Backend.VolatileProcess.programCode }
                              }
                            ]

                Just volatileProcessId ->
                    let
                        task =
                            { processId = volatileProcessId
                            , request = ""
                            }
                                |> ElmFullstack.RequestToVolatileProcess
                    in
                    ElmFullstack.passiveBackendEventResponse
                        |> ElmFullstack.withStartTasksAdded
                            [ { taskId = "http-request-" ++ pendingHttpRequest.httpRequestId
                              , task = task
                              }
                            ]
