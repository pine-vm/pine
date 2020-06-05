module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Bytes
import Bytes.Decode
import Bytes.Encode
import Json.Encode


type alias State =
    ()


processEvent : InterfaceToHost.ProcessEvent -> State -> ( State, List InterfaceToHost.ProcessRequest )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequest httpRequestEvent ->
            let
                headerToPropagateBody =
                    { name = "response-header-name"
                    , values =
                        [ httpRequestEvent.request.body
                            |> Maybe.map (decodeBytesToString >> Maybe.withDefault "Failed to decode bytes to string")
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
                        , body = httpResponseBodyString |> Bytes.Encode.string |> Bytes.Encode.encode |> Just
                        , headersToAdd = [ headerToPropagateBody, { name = "content-type", values = [ "application/json" ] } ]
                        }
                    }
                        |> InterfaceToHost.CompleteHttpResponse
            in
            ( stateBefore, [ httpResponse ] )

        InterfaceToHost.TaskComplete _ ->
            ( stateBefore, [] )


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))


interfaceToHost_initState : State
interfaceToHost_initState =
    ()


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent
