module Backend.Main exposing
    ( State
    , interfaceToHost_deserializeState
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , interfaceToHost_serializeState
    )

import Backend.InterfaceToHost as InterfaceToHost
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
                    , values = [ httpRequestEvent.request.bodyAsString |> Maybe.withDefault "" ]
                    }

                responseBody =
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
                        , bodyAsString = Just responseBody
                        , headersToAdd = [ headerToPropagateBody, { name = "content-type", values = [ "application/json" ] } ]
                        }
                    }
                        |> InterfaceToHost.CompleteHttpResponse
            in
            ( stateBefore, [ httpResponse ] )

        InterfaceToHost.TaskComplete _ ->
            ( stateBefore, [] )


interfaceToHost_initState : State
interfaceToHost_initState =
    ()


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent


interfaceToHost_serializeState : State -> String
interfaceToHost_serializeState =
    always ""


interfaceToHost_deserializeState : String -> State
interfaceToHost_deserializeState =
    always ()
