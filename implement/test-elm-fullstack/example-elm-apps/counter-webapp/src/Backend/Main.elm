module Backend.Main exposing
    ( State
    , backendMain
    )

import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode
import ElmFullstack
import Json.Decode


type alias State =
    Int


type alias CounterEvent =
    { addition : Int }


backendMain : ElmFullstack.BackendConfiguration State
backendMain =
    { init = 0
    , update = processEvent
    }


processEvent : ElmFullstack.BackendEvent -> State -> ( State, ElmFullstack.BackendEventResponse )
processEvent hostEvent stateBefore =
    case hostEvent of
        ElmFullstack.HttpRequestEvent httpRequestEvent ->
            let
                ( state, result ) =
                    case
                        httpRequestEvent.request.bodyAsBase64
                            |> Maybe.map (Base64.toBytes >> Maybe.map (decodeBytesToString >> Maybe.withDefault "Failed to decode bytes to string") >> Maybe.withDefault "Failed to decode from base64")
                            |> Maybe.withDefault "Missing HTTP body"
                            |> deserializeCounterEvent
                    of
                        Err error ->
                            ( stateBefore, Err ("Failed to deserialize counter event from HTTP Request content: " ++ error) )

                        Ok counterEvent ->
                            stateBefore |> processCounterEvent counterEvent |> Tuple.mapSecond Ok

                ( httpResponseCode, httpResponseBodyString ) =
                    case result of
                        Err error ->
                            ( 400, error )

                        Ok message ->
                            ( 200, message )

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
            , ElmFullstack.passiveBackendEventResponse
                |> ElmFullstack.withCompleteHttpResponsesAdded [ httpResponse ]
            )

        ElmFullstack.TaskCompleteEvent _ ->
            ( stateBefore, ElmFullstack.passiveBackendEventResponse )

        ElmFullstack.PosixTimeHasArrivedEvent _ ->
            ( stateBefore, ElmFullstack.passiveBackendEventResponse )


processCounterEvent : CounterEvent -> State -> ( State, String )
processCounterEvent counterEvent stateBefore =
    let
        state =
            stateBefore + counterEvent.addition
    in
    ( state, state |> String.fromInt )


deserializeCounterEvent : String -> Result String CounterEvent
deserializeCounterEvent serializedEvent =
    serializedEvent
        |> Json.Decode.decodeString (Json.Decode.field "addition" Json.Decode.int |> Json.Decode.map CounterEvent)
        |> Result.mapError Json.Decode.errorToString


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))
