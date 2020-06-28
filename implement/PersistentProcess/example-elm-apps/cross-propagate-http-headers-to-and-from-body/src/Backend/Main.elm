module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode
import Json.Encode


type alias State =
    ()


processEvent : InterfaceToHost.AppEvent -> State -> ( State, InterfaceToHost.AppEventResponse )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequestEvent httpRequestEvent ->
            let
                headerToPropagateBody =
                    { name = "response-header-name"
                    , values =
                        [ httpRequestEvent.request.bodyAsBase64
                            |> Maybe.map (Base64.toBytes >> Maybe.map (decodeBytesToString >> Maybe.withDefault "Failed to decode bytes to string") >> Maybe.withDefault "Failed to decode from base64")
                            |> Maybe.withDefault ""
                        ]
                    }

                httpResponseBodyString =
                    httpRequestEvent.request.headers
                        |> Json.Encode.list
                            (\requestHeader ->
                                [ ( "name", requestHeader.name |> Json.Encode.string )
                                , ( "values", requestHeader.values |> Json.Encode.list Json.Encode.string )
                                ]
                                    |> Json.Encode.object
                            )
                        |> Json.Encode.encode 0

                httpResponse =
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response =
                        { statusCode = 200
                        , bodyAsBase64 = httpResponseBodyString |> Bytes.Encode.string |> Bytes.Encode.encode |> Base64.fromBytes
                        , headersToAdd = [ headerToPropagateBody, { name = "content-type", values = [ "application/json" ] } ]
                        }
                    }
            in
            ( stateBefore
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


interfaceToHost_initState : State
interfaceToHost_initState =
    ()


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent
