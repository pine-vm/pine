module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Backend.StateType
import ElmFullstackCompilerInterface.GenerateJsonCoders
import Json.Decode
import Json.Encode


type alias State =
    { attemptSetMaybeStringOnMigration : Bool
    , maybeString : Maybe String
    , otherState : String
    }


processEvent : InterfaceToHost.ProcessEvent -> State -> ( State, List InterfaceToHost.ProcessRequest )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequest httpRequestEvent ->
            let
                ( state, httpResponseCode, httpResponseBody ) =
                    case httpRequestEvent.request.method |> String.toLower of
                        "get" ->
                            ( stateBefore
                            , 200
                            , stateBefore
                                |> ElmFullstackCompilerInterface.GenerateJsonCoders.encodeBackendState
                                |> Json.Encode.encode 0
                            )

                        "post" ->
                            case
                                httpRequestEvent.request.bodyAsString
                                    |> Maybe.withDefault ""
                                    |> Json.Decode.decodeString ElmFullstackCompilerInterface.GenerateJsonCoders.decodeBackendState
                            of
                                Err decodeErr ->
                                    ( stateBefore
                                    , 400
                                    , "Failed to decode state:\n" ++ (decodeErr |> Json.Decode.errorToString)
                                    )

                                Ok decodedState ->
                                    ( decodedState
                                    , 200
                                    , "Successfully set state"
                                    )

                        _ ->
                            ( stateBefore, 405, "Method not supported" )

                httpResponse =
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response =
                        { statusCode = httpResponseCode
                        , bodyAsString = Just httpResponseBody
                        , headersToAdd = []
                        }
                    }
                        |> InterfaceToHost.CompleteHttpResponse
            in
            ( state, [ httpResponse ] )

        InterfaceToHost.TaskComplete _ ->
            ( stateBefore, [] )


interfaceToHost_initState : Backend.StateType.State
interfaceToHost_initState =
    { maybeString = Nothing
    , attemptSetMaybeStringOnMigration = False
    , otherState = ""
    }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent
