module Backend.Main exposing
    ( State
    , webServiceMain
    )

import Backend.HttpViaVolatileProcess as HttpViaVolatileProcess
import Base64
import Bytes
import Bytes.Encode
import Json.Decode
import Platform.WebService


type alias State =
    { createVolatileProcessResult : Maybe (Result String { processId : String })
    , httpRequestToForward : Maybe Platform.WebService.HttpRequestEventStruct
    }


webServiceMain : Platform.WebService.WebServiceConfig State
webServiceMain =
    { init =
        ( { createVolatileProcessResult = Nothing
          , httpRequestToForward = Nothing
          }
        , []
        )
    , subscriptions = subscriptions
    }


subscriptions : State -> Platform.WebService.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : Platform.WebService.HttpRequestEventStruct -> State -> ( State, Platform.WebService.Commands State )
updateForHttpRequestEvent event stateBefore =
    let
        state =
            { stateBefore | httpRequestToForward = Just event }
    in
    ( state, state |> httpRequestForwardRequestsFromState )


httpRequestForwardRequestsFromState : State -> Platform.WebService.Commands State
httpRequestForwardRequestsFromState state =
    case state.httpRequestToForward of
        Nothing ->
            []

        Just httpRequestToForward ->
            case state.createVolatileProcessResult of
                Nothing ->
                    [ Platform.WebService.CreateVolatileProcess
                        { programCode = HttpViaVolatileProcess.programCode
                        , update = updateForCreateVolatileProcess
                        }
                    ]

                Just (Err createVolatileProcessErr) ->
                    [ Platform.WebService.RespondToHttpRequest
                        { httpRequestId = httpRequestToForward.httpRequestId
                        , response =
                            { statusCode = 500
                            , body =
                                Just
                                    (bodyFromString
                                        ("Failed to create volatile process: " ++ createVolatileProcessErr)
                                    )
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
                            [ Platform.WebService.RespondToHttpRequest
                                { httpRequestId = httpRequestToForward.httpRequestId
                                , response =
                                    { statusCode = 400
                                    , body =
                                        Just
                                            (bodyFromString
                                                "Where to should I forward this HTTP request? Use the 'forward-to' HTTP header to specify a destination."
                                            )
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
                                    , bodyAsBase64 =
                                        case httpRequestToForward.request.body of
                                            Nothing ->
                                                Nothing

                                            Just body ->
                                                Base64.fromBytes body
                                    }
                            in
                            [ Platform.WebService.RequestToVolatileProcess
                                { processId = createVolatileProcessOk.processId
                                , request = HttpViaVolatileProcess.requestToVolatileProcess httpRequest
                                , update = updateForRequestToVolatileProcess
                                }
                            ]


updateForCreateVolatileProcess : Platform.WebService.CreateVolatileProcessResult -> State -> ( State, Platform.WebService.Commands State )
updateForCreateVolatileProcess createVolatileProcessResponse stateBefore =
    let
        createVolatileProcessResult =
            createVolatileProcessResponse
                |> Result.mapError .exceptionToString

        state =
            { stateBefore | createVolatileProcessResult = Just createVolatileProcessResult }
    in
    ( state, state |> httpRequestForwardRequestsFromState )


updateForRequestToVolatileProcess : Platform.WebService.RequestToVolatileProcessResult -> State -> ( State, Platform.WebService.Commands State )
updateForRequestToVolatileProcess requestToVolatileProcessResponse stateBefore =
    case stateBefore.httpRequestToForward of
        Nothing ->
            ( stateBefore, [] )

        Just httpRequestToForward ->
            let
                httpResponse : Platform.WebService.HttpResponse
                httpResponse =
                    case requestToVolatileProcessResponse of
                        Err Platform.WebService.ProcessNotFound ->
                            { statusCode = 500
                            , body =
                                Just (bodyFromString "Error running in volatile process: ProcessNotFound")
                            , headersToAdd = []
                            }

                        Err (Platform.WebService.RequestToVolatileProcessOtherError err) ->
                            { statusCode = 500
                            , body =
                                Just (bodyFromString ("Error running in volatile process: " ++ err))
                            , headersToAdd = []
                            }

                        Ok requestToVolatileProcessComplete ->
                            case requestToVolatileProcessComplete.exceptionToString of
                                Just exceptionToString ->
                                    { statusCode = 500
                                    , body =
                                        Just (bodyFromString ("Exception in volatile process: " ++ exceptionToString))
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
                                            , body =
                                                Just
                                                    (bodyFromString
                                                        ("Error decoding response from volatile process: "
                                                            ++ (decodeError |> Json.Decode.errorToString)
                                                        )
                                                    )
                                            , headersToAdd = []
                                            }

                                        Ok volatileProcessHttpResponse ->
                                            let
                                                headersToAdd =
                                                    volatileProcessHttpResponse.headers
                                                        |> List.filter (.name >> String.toLower >> (/=) "transfer-encoding")
                                            in
                                            { statusCode = 200
                                            , body =
                                                case volatileProcessHttpResponse.bodyAsBase64 of
                                                    Nothing ->
                                                        Nothing

                                                    Just bodyAsBase64 ->
                                                        Base64.toBytes bodyAsBase64
                                            , headersToAdd = headersToAdd
                                            }
            in
            ( { stateBefore | httpRequestToForward = Nothing }
            , [ Platform.WebService.RespondToHttpRequest
                    { httpRequestId = httpRequestToForward.httpRequestId
                    , response = httpResponse
                    }
              ]
            )


bodyFromString : String -> Bytes.Bytes
bodyFromString =
    Bytes.Encode.string >> Bytes.Encode.encode
