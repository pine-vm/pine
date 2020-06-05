module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Bytes.Encode
import Common
import ElmFullstackCompilerInterface.ElmMakeFrontendWeb
import Url


type alias State =
    { httpRequestsCount : Int
    , lastHttpRequests : List InterfaceToHost.HttpRequestEvent
    }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent


processEvent : InterfaceToHost.ProcessEvent -> State -> ( State, List InterfaceToHost.ProcessRequest )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequest httpRequestEvent ->
            let
                state =
                    { stateBefore
                        | httpRequestsCount = stateBefore.httpRequestsCount + 1
                        , lastHttpRequests = httpRequestEvent :: stateBefore.lastHttpRequests |> List.take 4
                    }

                httpResponse =
                    if
                        httpRequestEvent.request.uri
                            |> Url.fromString
                            |> Maybe.map urlLeadsToFrontendHtmlDocument
                            |> Maybe.withDefault False
                    then
                        { statusCode = 200
                        , body = Just ElmFullstackCompilerInterface.ElmMakeFrontendWeb.elm_make_frontendWeb_html_debug
                        , headersToAdd = []
                        }

                    else
                        { statusCode = 200
                        , body =
                            [ Common.guideMarkdown
                            , ""
                            , "This backend process received " ++ (state.httpRequestsCount |> String.fromInt) ++ " HTTP requests."
                            ]
                                |> String.join "\n"
                                |> Bytes.Encode.string
                                |> Bytes.Encode.encode
                                |> Just
                        , headersToAdd = []
                        }
            in
            ( state
            , [ { httpRequestId = httpRequestEvent.httpRequestId
                , response = httpResponse
                }
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
    { httpRequestsCount = 0
    , lastHttpRequests = []
    }
