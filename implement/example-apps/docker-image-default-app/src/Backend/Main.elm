module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    , processEvent
    )

import Backend.InterfaceToHost as InterfaceToHost
import Base64
import Bytes.Encode
import CompilationInterface.ElmMake
import CompilationInterface.SourceFiles
import Url


type alias State =
    { httpRequestsCount : Int
    , lastHttpRequests : List InterfaceToHost.HttpRequestEventStructure
    }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent


processEvent : InterfaceToHost.AppEvent -> State -> ( State, InterfaceToHost.AppEventResponse )
processEvent hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.HttpRequestEvent httpRequestEvent ->
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
                        , bodyAsBase64 = Just CompilationInterface.ElmMake.elm_make__debug__base64____src_FrontendWeb_Main_elm
                        , headersToAdd = []
                        }

                    else
                        { statusCode = 200
                        , bodyAsBase64 =
                            [ CompilationInterface.SourceFiles.file__utf8____readme_md
                            , ""
                            , "This backend process received " ++ (state.httpRequestsCount |> String.fromInt) ++ " HTTP requests."
                            ]
                                |> String.join "\n"
                                |> Bytes.Encode.string
                                |> Bytes.Encode.encode
                                |> Base64.fromBytes
                        , headersToAdd = []
                        }
            in
            ( state
            , InterfaceToHost.passiveAppEventResponse
                |> InterfaceToHost.withCompleteHttpResponsesAdded
                    [ { httpRequestId = httpRequestEvent.httpRequestId
                      , response = httpResponse
                      }
                    ]
            )

        InterfaceToHost.TaskCompleteEvent _ ->
            ( stateBefore, InterfaceToHost.passiveAppEventResponse )

        InterfaceToHost.ArrivedAtTimeEvent _ ->
            ( stateBefore, InterfaceToHost.passiveAppEventResponse )


urlLeadsToFrontendHtmlDocument : Url.Url -> Bool
urlLeadsToFrontendHtmlDocument url =
    not (url.path == "/api" || (url.path |> String.startsWith "/api/"))


interfaceToHost_initState : State
interfaceToHost_initState =
    { httpRequestsCount = 0
    , lastHttpRequests = []
    }
