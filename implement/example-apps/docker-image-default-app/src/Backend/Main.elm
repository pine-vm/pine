module Backend.Main exposing
    ( State
    , backendMain
    )

import Base64
import Bytes.Encode
import CompilationInterface.ElmMake
import CompilationInterface.SourceFiles
import ElmFullstack
import Url


type alias State =
    { httpRequestsCount : Int
    , lastHttpRequests : List ElmFullstack.HttpRequestEventStruct
    }


backendMain : ElmFullstack.BackendConfig State
backendMain =
    { init = ( initState, [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> ElmFullstack.BackendSubs State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : ElmFullstack.HttpRequestEventStruct -> State -> ( State, ElmFullstack.BackendCmds State )
updateForHttpRequestEvent httpRequestEvent stateBefore =
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
                , bodyAsBase64 = Just CompilationInterface.ElmMake.elm_make____src_Frontend_Main_elm.debug.base64
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
    , [ ElmFullstack.RespondToHttpRequest
            { httpRequestId = httpRequestEvent.httpRequestId
            , response = httpResponse
            }
      ]
    )


urlLeadsToFrontendHtmlDocument : Url.Url -> Bool
urlLeadsToFrontendHtmlDocument url =
    not (url.path == "/api" || (url.path |> String.startsWith "/api/"))


initState : State
initState =
    { httpRequestsCount = 0
    , lastHttpRequests = []
    }
