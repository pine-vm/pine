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
    { createVolatileProcessResult : Maybe (Result String { processId : String })
    , httpRequestToForward : Maybe ElmFullstack.HttpRequestEventStruct
    }


backendMain : ElmFullstack.BackendConfig State
backendMain =
    { init =
        ( { createVolatileProcessResult = Nothing
          , httpRequestToForward = Nothing
          }
        , []
        )
    , subscriptions = subscriptions
    }


subscriptions : State -> ElmFullstack.BackendSubs State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : ElmFullstack.HttpRequestEventStruct -> State -> ( State, ElmFullstack.BackendCmds State )
updateForHttpRequestEvent event stateBefore =
    let
        state =
            { stateBefore | httpRequestToForward = Just event }
    in
    ( state, state |> httpRequestForwardRequestsFromState )


httpRequestForwardRequestsFromState : State -> ElmFullstack.BackendCmds State
httpRequestForwardRequestsFromState state =
    case state.httpRequestToForward of
        Nothing ->
            []

        Just httpRequestToForward ->
            case state.createVolatileProcessResult of
                Nothing ->
                    [ ElmFullstack.CreateVolatileProcess
                        { programCode = HttpViaVolatileProcess.programCode
                        , update = updateForCreateVolatileProcess
                        }
                    ]

                Just (Err createVolatileProcessErr) ->
                    [ ElmFullstack.RespondToHttpRequest
                        { httpRequestId = httpRequestToForward.httpRequestId
                        , response =
                            { statusCode = 500
                            , bodyAsBase64 =
                                bodyBase64FromString
                                    ("Failed to create volatile process: " ++ createVolatileProcessErr)
                            , headersToAdd = []
                            }
                        }
                    ]

                Just (Ok createVolatileProcessOk) ->
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
                            [ ElmFullstack.RespondToHttpRequest
                                { httpRequestId = httpRequestToForward.httpRequestId
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
                            in
                            [ ElmFullstack.RequestToVolatileProcess
                                { processId = createVolatileProcessOk.processId
                                , request = HttpViaVolatileProcess.requestToVolatileProcess httpRequest
                                , update = updateForRequestToVolatileProcess
                                }
                            ]


updateForCreateVolatileProcess : ElmFullstack.CreateVolatileProcessResult -> State -> ( State, ElmFullstack.BackendCmds State )
updateForCreateVolatileProcess createVolatileProcessResponse stateBefore =
    let
        createVolatileProcessResult =
            createVolatileProcessResponse
                |> Result.mapError .exceptionToString

        state =
            { stateBefore | createVolatileProcessResult = Just createVolatileProcessResult }
    in
    ( state, state |> httpRequestForwardRequestsFromState )


updateForRequestToVolatileProcess : ElmFullstack.RequestToVolatileProcessResult -> State -> ( State, ElmFullstack.BackendCmds State )
updateForRequestToVolatileProcess requestToVolatileProcessResponse stateBefore =
    case stateBefore.httpRequestToForward of
        Nothing ->
            ( stateBefore, [] )

        Just httpRequestToForward ->
            let
                httpResponse =
                    case requestToVolatileProcessResponse of
                        Err ElmFullstack.ProcessNotFound ->
                            { statusCode = 500
                            , bodyAsBase64 = bodyBase64FromString "Error running in volatile process: ProcessNotFound"
                            , headersToAdd = []
                            }

                        Ok requestToVolatileProcessComplete ->
                            case requestToVolatileProcessComplete.exceptionToString of
                                Just exceptionToString ->
                                    { statusCode = 500
                                    , bodyAsBase64 = bodyBase64FromString ("Exception in volatile process: " ++ exceptionToString)
                                    , headersToAdd = []
                                    }

                                Nothing ->
                                    let
                                        returnValueAsHttpResponseResult =
                                            requestToVolatileProcessComplete.returnValueToString
                                                |> Maybe.withDefault ""
                                                |> Json.Decode.decodeString HttpViaVolatileProcess.decodeVolatileProcessHttpResponse
                                    in
                                    case returnValueAsHttpResponseResult of
                                        Err decodeError ->
                                            { statusCode = 500
                                            , bodyAsBase64 =
                                                bodyBase64FromString ("Error decoding response from volatile process: " ++ (decodeError |> Json.Decode.errorToString))
                                            , headersToAdd = []
                                            }

                                        Ok volatileProcessHttpResponse ->
                                            let
                                                headersToAdd =
                                                    volatileProcessHttpResponse.headers
                                                        |> List.filter (.name >> String.toLower >> (/=) "transfer-encoding")
                                            in
                                            { statusCode = 200
                                            , bodyAsBase64 = volatileProcessHttpResponse.bodyAsBase64
                                            , headersToAdd = headersToAdd
                                            }
            in
            ( { stateBefore | httpRequestToForward = Nothing }
            , [ ElmFullstack.RespondToHttpRequest
                    { httpRequestId = httpRequestToForward.httpRequestId
                    , response = httpResponse
                    }
              ]
            )


bodyBase64FromString : String -> Maybe String
bodyBase64FromString =
    Bytes.Encode.string >> Bytes.Encode.encode >> Base64.fromBytes
