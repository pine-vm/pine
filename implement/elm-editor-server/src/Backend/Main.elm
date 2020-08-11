module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    )

import Backend.ElmMakeVolatileHost as ElmMakeVolatileHost
import Backend.InterfaceToHost as InterfaceToHost
import Base64
import Bytes.Encode
import Common
import ElmFullstackCompilerInterface.ElmMake
import Url


type alias State =
    { volatileHostId : Maybe String
    , httpRequestToAnswer : Maybe InterfaceToHost.HttpRequestEventStructure
    }


processEvent : InterfaceToHost.AppEvent -> State -> ( State, InterfaceToHost.AppEventResponse )
processEvent hostEvent stateBefore =
    let
        ( state, responseBeforeTasks ) =
            processEventBeforeDerivingTasks hostEvent stateBefore

        tasks =
            tasksFromState state
    in
    ( state
    , responseBeforeTasks |> InterfaceToHost.withStartTasksAdded tasks
    )


processEventBeforeDerivingTasks : InterfaceToHost.AppEvent -> State -> ( State, InterfaceToHost.AppEventResponse )
processEventBeforeDerivingTasks hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.ArrivedAtTimeEvent _ ->
            ( stateBefore
            , InterfaceToHost.passiveAppEventResponse
            )

        InterfaceToHost.HttpRequestEvent httpRequestEvent ->
            if
                httpRequestEvent.request.uri
                    |> Url.fromString
                    |> Maybe.map urlLeadsToFrontendHtmlDocument
                    |> Maybe.withDefault False
            then
                ( stateBefore
                , InterfaceToHost.passiveAppEventResponse
                    |> InterfaceToHost.withCompleteHttpResponsesAdded
                        [ { httpRequestId = httpRequestEvent.httpRequestId
                          , response =
                                { statusCode = 200
                                , bodyAsBase64 = Just ElmFullstackCompilerInterface.ElmMake.elm_make__debug__base64____src_FrontendWeb_Main_elm
                                , headersToAdd = []
                                }
                          }
                        ]
                )

            else
                ( { stateBefore | httpRequestToAnswer = Just httpRequestEvent }
                , InterfaceToHost.passiveAppEventResponse
                )

        InterfaceToHost.TaskCompleteEvent taskComplete ->
            case taskComplete.taskResult of
                InterfaceToHost.CreateVolatileHostResponse createVolatileHostResponse ->
                    case createVolatileHostResponse of
                        Err createVolatileHostError ->
                            case stateBefore.httpRequestToAnswer of
                                Nothing ->
                                    ( stateBefore
                                    , InterfaceToHost.passiveAppEventResponse
                                    )

                                Just httpRequestToAnswer ->
                                    let
                                        bodyFromString =
                                            Bytes.Encode.string >> Bytes.Encode.encode >> Base64.fromBytes

                                        httpResponse =
                                            { statusCode = 500
                                            , bodyAsBase64 = bodyFromString ("Failed to create volatile host: " ++ createVolatileHostError.exceptionToString)
                                            , headersToAdd = []
                                            }

                                        state =
                                            { stateBefore | httpRequestToAnswer = Nothing }
                                    in
                                    ( state
                                    , InterfaceToHost.passiveAppEventResponse
                                        |> InterfaceToHost.withCompleteHttpResponsesAdded
                                            [ { httpRequestId = httpRequestToAnswer.httpRequestId
                                              , response = httpResponse
                                              }
                                            ]
                                    )

                        Ok { hostId } ->
                            ( { stateBefore | volatileHostId = Just hostId }, InterfaceToHost.passiveAppEventResponse )

                InterfaceToHost.RequestToVolatileHostResponse requestToVolatileHostResponse ->
                    case stateBefore.httpRequestToAnswer of
                        Nothing ->
                            ( stateBefore
                            , InterfaceToHost.passiveAppEventResponse
                            )

                        Just httpRequestToAnswer ->
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
                                                    , bodyAsBase64 = bodyFromString ("Exception in volatile host:\n" ++ exceptionToString)
                                                    , headersToAdd = []
                                                    }

                                                Nothing ->
                                                    let
                                                        headersToAdd =
                                                            []
                                                    in
                                                    { statusCode = 200
                                                    , bodyAsBase64 =
                                                        bodyFromString
                                                            (requestToVolatileHostComplete.returnValueToString
                                                                |> Maybe.withDefault ""
                                                            )
                                                    , headersToAdd = headersToAdd
                                                    }

                                state =
                                    { stateBefore | httpRequestToAnswer = Nothing }
                            in
                            ( state
                            , InterfaceToHost.passiveAppEventResponse
                                |> InterfaceToHost.withCompleteHttpResponsesAdded
                                    [ { httpRequestId = httpRequestToAnswer.httpRequestId
                                      , response = httpResponse
                                      }
                                    ]
                            )

                InterfaceToHost.CompleteWithoutResult ->
                    ( stateBefore
                    , InterfaceToHost.passiveAppEventResponse
                    )


tasksFromState : State -> List InterfaceToHost.StartTaskStructure
tasksFromState state =
    case state.httpRequestToAnswer of
        Nothing ->
            []

        Just httpRequestToAnswer ->
            case state.volatileHostId of
                Nothing ->
                    [ { taskId = "create-vhost"
                      , task = InterfaceToHost.CreateVolatileHost { script = ElmMakeVolatileHost.volatileHostScript }
                      }
                    ]

                Just volatileHostId ->
                    let
                        task =
                            { hostId = volatileHostId
                            , request =
                                httpRequestToAnswer.request.bodyAsBase64
                                    |> Maybe.andThen Common.decodeBase64ToString
                                    |> Maybe.withDefault "Error decoding base64"
                            }
                                |> InterfaceToHost.RequestToVolatileHost
                    in
                    [ { taskId = "http-request-api-" ++ httpRequestToAnswer.httpRequestId
                      , task = task
                      }
                    ]


urlLeadsToFrontendHtmlDocument : Url.Url -> Bool
urlLeadsToFrontendHtmlDocument url =
    not (url.path == "/api" || (url.path |> String.startsWith "/api/"))


interfaceToHost_initState : State
interfaceToHost_initState =
    { volatileHostId = Nothing, httpRequestToAnswer = Nothing }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent
