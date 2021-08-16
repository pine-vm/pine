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

        Just _ ->
            case state.volatileProcessId of
                Nothing ->
                    [ ElmFullstack.CreateVolatileProcess
                        { programCode = Backend.VolatileProcess.programCode
                        , update = updateForCreateVolatileProcess
                        }
                    ]

                Just volatileProcessId ->
                    [ ElmFullstack.RequestToVolatileProcess
                        { processId = volatileProcessId
                        , request = ""
                        , update = updateForRequestToVolatileProcess
                        }
                    ]


updateForCreateVolatileProcess : ElmFullstack.CreateVolatileProcessResult -> State -> ( State, ElmFullstack.BackendCmds State )
updateForCreateVolatileProcess createVolatileProcessResponse stateBefore =
    case createVolatileProcessResponse of
        Err _ ->
            ( stateBefore, [] )

        Ok { processId } ->
            let
                state =
                    { stateBefore | volatileProcessId = Just processId }
            in
            ( state, state |> volatileProcessCmdsFromState )


updateForRequestToVolatileProcess : ElmFullstack.RequestToVolatileProcessResult -> State -> ( State, ElmFullstack.BackendCmds State )
updateForRequestToVolatileProcess requestToVolatileProcessResponse stateBefore =
    let
        bodyFromString =
            Bytes.Encode.string >> Bytes.Encode.encode >> Base64.fromBytes

        httpResponseInternalServerError errorMessage =
            { statusCode = 500
            , bodyAsBase64 = bodyFromString errorMessage
            , headersToAdd = []
            }
    in
    case stateBefore.pendingHttpRequest of
        Nothing ->
            ( stateBefore, [] )

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
            in
            ( { stateBefore | pendingHttpRequest = Nothing }
            , [ ElmFullstack.RespondToHttpRequest
                    { httpRequestId = pendingHttpRequest.httpRequestId
                    , response = httpResponse
                    }
              ]
            )
