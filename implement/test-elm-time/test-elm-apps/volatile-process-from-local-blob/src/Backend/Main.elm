module Backend.Main exposing
    ( State
    , webServiceMain
    )

import Base64
import Bytes.Encode
import CompilationInterface.SourceFiles
import Platform.WebService


type alias State =
    { volatileProcessId : Maybe String
    , pendingHttpRequest : Maybe Platform.WebService.HttpRequestEventStruct
    }


webServiceMain : Platform.WebService.WebServiceConfig State
webServiceMain =
    { init = ( { volatileProcessId = Nothing, pendingHttpRequest = Nothing }, [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> Platform.WebService.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : Platform.WebService.HttpRequestEventStruct -> State -> ( State, Platform.WebService.Commands State )
updateForHttpRequestEvent httpRequestEvent stateBefore =
    let
        state =
            { stateBefore | pendingHttpRequest = Just httpRequestEvent }
    in
    ( state, state |> volatileProcessCmdsFromState )


volatileProcessCmdsFromState : State -> Platform.WebService.Commands State
volatileProcessCmdsFromState state =
    case state.pendingHttpRequest of
        Nothing ->
            []

        Just pendingHttpRequest ->
            case state.volatileProcessId of
                Nothing ->
                    [ Platform.WebService.CreateVolatileProcess
                        { programCode = CompilationInterface.SourceFiles.file____src_Backend_VolatileProcess_csx.utf8
                        , update = updateForCreateVolatileProcess pendingHttpRequest
                        }
                    ]

                Just volatileProcessId ->
                    [ Platform.WebService.RequestToVolatileProcess
                        { processId = volatileProcessId
                        , request = ""
                        , update = updateForRequestToVolatileProcess pendingHttpRequest
                        }
                    ]


updateForCreateVolatileProcess : Platform.WebService.HttpRequestEventStruct -> Platform.WebService.CreateVolatileProcessResult -> State -> ( State, Platform.WebService.Commands State )
updateForCreateVolatileProcess pendingHttpRequest createVolatileProcessResponse stateBefore =
    case createVolatileProcessResponse of
        Err { exceptionToString } ->
            ( stateBefore
            , [ Platform.WebService.RespondToHttpRequest
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


updateForRequestToVolatileProcess : Platform.WebService.HttpRequestEventStruct -> Platform.WebService.RequestToVolatileProcessResult -> State -> ( State, Platform.WebService.Commands State )
updateForRequestToVolatileProcess pendingHttpRequest requestToVolatileProcessResponse stateBefore =
    let
        httpResponse =
            case requestToVolatileProcessResponse of
                Err Platform.WebService.ProcessNotFound ->
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
    , [ Platform.WebService.RespondToHttpRequest
            { httpRequestId = pendingHttpRequest.httpRequestId
            , response = httpResponse
            }
      ]
    )


httpResponseInternalServerError : String -> Platform.WebService.HttpResponse
httpResponseInternalServerError errorMessage =
    { statusCode = 500
    , bodyAsBase64 = bodyFromString errorMessage
    , headersToAdd = []
    }


bodyFromString : String -> Maybe String
bodyFromString =
    Bytes.Encode.string >> Bytes.Encode.encode >> Base64.fromBytes
