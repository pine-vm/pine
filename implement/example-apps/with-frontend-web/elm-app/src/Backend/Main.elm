module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Bytes
import Bytes.Decode
import Bytes.Encode
import ElmFullstackCompilerInterface.ElmMakeFrontendWeb
import Url


type alias State =
    String


processEvent : InterfaceToHost.ProcessEvent -> State -> ( State, List InterfaceToHost.ProcessRequest )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequest httpRequestEvent ->
            let
                ( state, httpResponse ) =
                    if
                        httpRequestEvent.request.uri
                            |> Url.fromString
                            |> Maybe.map urlLeadsToFrontendHtmlDocument
                            |> Maybe.withDefault False
                    then
                        ( stateBefore
                        , { statusCode = 200
                          , body = Just ElmFullstackCompilerInterface.ElmMakeFrontendWeb.elm_make_frontendWeb_html
                          , headersToAdd = []
                          }
                        )

                    else
                        let
                            stateAfterRequest =
                                case httpRequestEvent.request.method |> String.toLower of
                                    "get" ->
                                        stateBefore

                                    "post" ->
                                        stateBefore
                                            ++ (httpRequestEvent.request.body
                                                    |> Maybe.map (\bytes -> bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width)) |> Maybe.withDefault "Failed to decode bytes as string")
                                                    |> Maybe.withDefault ""
                                               )

                                    _ ->
                                        stateBefore
                        in
                        ( stateAfterRequest
                        , { statusCode = 200
                          , body =
                                [ "The request uri was: " ++ httpRequestEvent.request.uri
                                , "The current state is:" ++ stateAfterRequest
                                ]
                                    |> String.join "\n"
                                    |> Bytes.Encode.string
                                    |> Bytes.Encode.encode
                                    |> Just
                          , headersToAdd = []
                          }
                        )
            in
            ( state
            , [ { httpRequestId = httpRequestEvent.httpRequestId, response = httpResponse }
                    |> InterfaceToHost.CompleteHttpResponse
              ]
            )

        InterfaceToHost.TaskComplete _ ->
            ( stateBefore, [] )


urlLeadsToFrontendHtmlDocument : Url.Url -> Bool
urlLeadsToFrontendHtmlDocument url =
    not (url.path == "/api" || (url.path |> String.startsWith "/api/"))


interfaceToHost_initState : State
interfaceToHost_initState =
    ""


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent
