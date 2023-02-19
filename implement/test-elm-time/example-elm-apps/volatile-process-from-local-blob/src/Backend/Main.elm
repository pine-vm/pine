module Backend.Main exposing
    ( State
    , backendMain
    )

import Base64
import Bytes.Encode
import CompilationInterface.SourceFiles
import ElmWebServer


type alias State =
    { volatileProcessId : Maybe String
    , pendingHttpRequest : Maybe ElmWebServer.HttpRequestEventStruct
    }


backendMain : ElmWebServer.WebServerConfig State
backendMain =
    { init = ( { volatileProcessId = Nothing, pendingHttpRequest = Nothing }, [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> ElmWebServer.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : ElmWebServer.HttpRequestEventStruct -> State -> ( State, ElmWebServer.Commands State )
updateForHttpRequestEvent httpRequestEvent stateBefore =
    let
        state =
            { stateBefore | pendingHttpRequest = Just httpRequestEvent }
    in
    ( state, state |> volatileProcessCmdsFromState )


volatileProcessCmdsFromState : State -> ElmWebServer.Commands State
volatileProcessCmdsFromState state =
    case state.pendingHttpRequest of
        Nothing ->
            []

        Just pendingHttpRequest ->
            case state.volatileProcessId of
                Nothing ->
                    [ ElmWebServer.CreateVolatileProcess
                        { programCode = CompilationInterface.SourceFiles.file____src_Backend_VolatileProcess_csx.utf8
                        , update = updateForCreateVolatileProcess pendingHttpRequest
                        }
                    ]

                Just volatileProcessId ->
                    [ ElmWebServer.RequestToVolatileProcess
                        { processId = volatileProcessId
                        , request = ""
                        , update = updateForRequestToVolatileProcess pendingHttpRequest
                        }
                    ]


updateForCreateVolatileProcess : ElmWebServer.HttpRequestEventStruct -> ElmWebServer.CreateVolatileProcessResult -> State -> ( State, ElmWebServer.Commands State )
updateForCreateVolatileProcess pendingHttpRequest createVolatileProcessResponse stateBefore =
    case createVolatileProcessResponse of
        Err { exceptionToString } ->
            ( stateBefore
            , [ ElmWebServer.RespondToHttpRequest
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


updateForRequestToVolatileProcess : ElmWebServer.HttpRequestEventStruct -> ElmWebServer.RequestToVolatileProcessResult -> State -> ( State, ElmWebServer.Commands State )
updateForRequestToVolatileProcess pendingHttpRequest requestToVolatileProcessResponse stateBefore =
    let
        httpResponse =
            case requestToVolatileProcessResponse of
                Err ElmWebServer.ProcessNotFound ->
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
    , [ ElmWebServer.RespondToHttpRequest
            { httpRequestId = pendingHttpRequest.httpRequestId
            , response = httpResponse
            }
      ]
    )


httpResponseInternalServerError : String -> ElmWebServer.HttpResponse
httpResponseInternalServerError errorMessage =
    { statusCode = 500
    , bodyAsBase64 = bodyFromString errorMessage
    , headersToAdd = []
    }


bodyFromString : String -> Maybe String
bodyFromString =
    Bytes.Encode.string >> Bytes.Encode.encode >> Base64.fromBytes
