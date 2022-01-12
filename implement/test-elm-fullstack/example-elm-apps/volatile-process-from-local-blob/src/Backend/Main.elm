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
    , pendingHttpRequest : Maybe ElmFullstack.HttpRequestEventStruct
    }


backendMain : ElmFullstack.BackendConfig State
backendMain =
    { init = ( { volatileProcessId = Nothing, pendingHttpRequest = Nothing }, [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> ElmFullstack.BackendSubs State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : ElmFullstack.HttpRequestEventStruct -> State -> ( State, ElmFullstack.BackendCmds State )
updateForHttpRequestEvent httpRequestEvent stateBefore =
    let
        state =
            { stateBefore | pendingHttpRequest = Just httpRequestEvent }
    in
    ( state, state |> volatileProcessCmdsFromState )


volatileProcessCmdsFromState : State -> ElmFullstack.BackendCmds State
volatileProcessCmdsFromState state =
    case state.pendingHttpRequest of
        Nothing ->
            []

        Just pendingHttpRequest ->
            case state.volatileProcessId of
                Nothing ->
                    [ ElmFullstack.CreateVolatileProcess
                        { programCode = Backend.VolatileProcess.programCode
                        , update = updateForCreateVolatileProcess pendingHttpRequest
                        }
                    ]

                Just volatileProcessId ->
                    [ ElmFullstack.RequestToVolatileProcess
                        { processId = volatileProcessId
                        , request = ""
                        , update = updateForRequestToVolatileProcess pendingHttpRequest
                        }
                    ]


updateForCreateVolatileProcess : ElmFullstack.HttpRequestEventStruct -> ElmFullstack.CreateVolatileProcessResult -> State -> ( State, ElmFullstack.BackendCmds State )
updateForCreateVolatileProcess pendingHttpRequest createVolatileProcessResponse stateBefore =
    case createVolatileProcessResponse of
        Err { exceptionToString } ->
            ( stateBefore
            , [ ElmFullstack.RespondToHttpRequest
                    { httpRequestId = pendingHttpRequest.httpRequestId
                    , response = httpResponseInternalServerError ("Error creating volatile process: " ++ exceptionToString)
                    }
              ]
            )

        Ok { processId } ->
            let
                state =
                    { stateBefore | volatileProcessId = Just processId }
            in
            ( state, state |> volatileProcessCmdsFromState )


updateForRequestToVolatileProcess : ElmFullstack.HttpRequestEventStruct -> ElmFullstack.RequestToVolatileProcessResult -> State -> ( State, ElmFullstack.BackendCmds State )
updateForRequestToVolatileProcess pendingHttpRequest requestToVolatileProcessResponse stateBefore =
    let
        httpResponse =
            case requestToVolatileProcessResponse of
                Err ElmFullstack.ProcessNotFound ->
                    httpResponseInternalServerError "Error running in volatile process: ProcessNotFound"

                Ok requestToVolatileProcessComplete ->
                    case requestToVolatileProcessComplete.exceptionToString of
                        Just exceptionToString ->
                            httpResponseInternalServerError ("Error running in volatile process: Exception: " ++ exceptionToString)

                        Nothing ->
                            { statusCode = 200
                            , bodyAsBase64 = Maybe.andThen bodyFromString requestToVolatileProcessComplete.returnValueToString
                            , headersToAdd = []
                            }
    in
    ( { stateBefore | pendingHttpRequest = Nothing }
    , [ ElmFullstack.RespondToHttpRequest
            { httpRequestId = pendingHttpRequest.httpRequestId
            , response = httpResponse
            }
      ]
    )


httpResponseInternalServerError : String -> ElmFullstack.HttpResponse
httpResponseInternalServerError errorMessage =
    { statusCode = 500
    , bodyAsBase64 = bodyFromString errorMessage
    , headersToAdd = []
    }


bodyFromString : String -> Maybe String
bodyFromString =
    Bytes.Encode.string >> Bytes.Encode.encode >> Base64.fromBytes
