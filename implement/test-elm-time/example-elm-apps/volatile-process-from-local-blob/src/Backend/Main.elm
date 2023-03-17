module Backend.Main exposing
    ( State
    , webServerMain
    )

import Base64
import Bytes.Encode
import CompilationInterface.SourceFiles
import Platform.WebServer


type alias State =
    { volatileProcessId : Maybe String
    , pendingHttpRequest : Maybe Platform.WebServer.HttpRequestEventStruct
    }


webServerMain : Platform.WebServer.WebServerConfig State
webServerMain =
    { init = ( { volatileProcessId = Nothing, pendingHttpRequest = Nothing }, [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> Platform.WebServer.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : Platform.WebServer.HttpRequestEventStruct -> State -> ( State, Platform.WebServer.Commands State )
updateForHttpRequestEvent httpRequestEvent stateBefore =
    let
        state =
            { stateBefore | pendingHttpRequest = Just httpRequestEvent }
    in
    ( state, state |> volatileProcessCmdsFromState )


volatileProcessCmdsFromState : State -> Platform.WebServer.Commands State
volatileProcessCmdsFromState state =
    case state.pendingHttpRequest of
        Nothing ->
            []

        Just pendingHttpRequest ->
            case state.volatileProcessId of
                Nothing ->
                    [ Platform.WebServer.CreateVolatileProcess
                        { programCode = CompilationInterface.SourceFiles.file____src_Backend_VolatileProcess_csx.utf8
                        , update = updateForCreateVolatileProcess pendingHttpRequest
                        }
                    ]

                Just volatileProcessId ->
                    [ Platform.WebServer.RequestToVolatileProcess
                        { processId = volatileProcessId
                        , request = ""
                        , update = updateForRequestToVolatileProcess pendingHttpRequest
                        }
                    ]


updateForCreateVolatileProcess : Platform.WebServer.HttpRequestEventStruct -> Platform.WebServer.CreateVolatileProcessResult -> State -> ( State, Platform.WebServer.Commands State )
updateForCreateVolatileProcess pendingHttpRequest createVolatileProcessResponse stateBefore =
    case createVolatileProcessResponse of
        Err { exceptionToString } ->
            ( stateBefore
            , [ Platform.WebServer.RespondToHttpRequest
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


updateForRequestToVolatileProcess : Platform.WebServer.HttpRequestEventStruct -> Platform.WebServer.RequestToVolatileProcessResult -> State -> ( State, Platform.WebServer.Commands State )
updateForRequestToVolatileProcess pendingHttpRequest requestToVolatileProcessResponse stateBefore =
    let
        httpResponse =
            case requestToVolatileProcessResponse of
                Err Platform.WebServer.ProcessNotFound ->
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
    , [ Platform.WebServer.RespondToHttpRequest
            { httpRequestId = pendingHttpRequest.httpRequestId
            , response = httpResponse
            }
      ]
    )


httpResponseInternalServerError : String -> Platform.WebServer.HttpResponse
httpResponseInternalServerError errorMessage =
    { statusCode = 500
    , bodyAsBase64 = bodyFromString errorMessage
    , headersToAdd = []
    }


bodyFromString : String -> Maybe String
bodyFromString =
    Bytes.Encode.string >> Bytes.Encode.encode >> Base64.fromBytes
