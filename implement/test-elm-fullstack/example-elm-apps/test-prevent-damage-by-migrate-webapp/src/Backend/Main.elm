module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Backend.StateType
import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode
import CompilationInterface.GenerateJsonCoders
import Json.Decode
import Json.Encode


type alias State =
    { attemptSetMaybeStringOnMigration : Bool
    , maybeString : Maybe String
    , otherState : String
    }


processEvent : InterfaceToHost.AppEvent -> State -> ( State, InterfaceToHost.AppEventResponse )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequestEvent httpRequestEvent ->
            let
                ( state, httpResponseCode, httpResponseBodyString ) =
                    case httpRequestEvent.request.method |> String.toLower of
                        "get" ->
                            ( stateBefore
                            , 200
                            , stateBefore
                                |> CompilationInterface.GenerateJsonCoders.encodeBackendState
                                |> Json.Encode.encode 0
                            )

                        "post" ->
                            case
                                httpRequestEvent.request.bodyAsBase64
                                    |> Maybe.map (Base64.toBytes >> Maybe.map (decodeBytesToString >> Maybe.withDefault "Failed to decode bytes to string") >> Maybe.withDefault "Failed to decode from base64")
                                    |> Maybe.withDefault "Missing HTTP body"
                                    |> Json.Decode.decodeString CompilationInterface.GenerateJsonCoders.decodeBackendState
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
                        , bodyAsBase64 = httpResponseBodyString |> Bytes.Encode.string |> Bytes.Encode.encode |> Base64.fromBytes
                        , headersToAdd = []
                        }
                    }
            in
            ( state
            , InterfaceToHost.passiveAppEventResponse
                |> InterfaceToHost.withCompleteHttpResponsesAdded [ httpResponse ]
            )

        InterfaceToHost.TaskCompleteEvent _ ->
            ( stateBefore, InterfaceToHost.passiveAppEventResponse )

        InterfaceToHost.ArrivedAtTimeEvent _ ->
            ( stateBefore, InterfaceToHost.passiveAppEventResponse )


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))


interfaceToHost_initState : Backend.StateType.State
interfaceToHost_initState =
    { maybeString = Nothing
    , attemptSetMaybeStringOnMigration = False
    , otherState = ""
    }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent
