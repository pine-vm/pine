module Backend.Main exposing
    ( State
    , webServiceMain
    )

import Base64
import Bytes.Encode
import CompilationInterface.ElmMake
import CompilationInterface.SourceFiles
import Platform.WebService
import Url


type alias State =
    { httpRequestsCount : Int
    , lastHttpRequests : List Platform.WebService.HttpRequestEventStruct
    }


webServiceMain : Platform.WebService.WebServiceConfig State
webServiceMain =
    { init = ( initState, [] )
    , subscriptions = subscriptions
    }


subscriptions : State -> Platform.WebService.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent :
    Platform.WebService.HttpRequestEventStruct
    -> State
    -> ( State, Platform.WebService.Commands State )
updateForHttpRequestEvent httpRequestEvent stateBefore =
    let
        state =
            { stateBefore
                | httpRequestsCount = stateBefore.httpRequestsCount + 1
                , lastHttpRequests = httpRequestEvent :: List.take 4 stateBefore.lastHttpRequests
            }

        httpResponse =
            if
                httpRequestEvent.request.uri
                    |> Url.fromString
                    |> Maybe.map urlLeadsToFrontendHtmlDocument
                    |> Maybe.withDefault False
            then
                { statusCode = 200
                , body =
                    CompilationInterface.ElmMake.elm_make____src_Frontend_Main_elm.debug.base64
                        |> Base64.toBytes
                , headersToAdd =
                    [ { name = "Content-Type", values = [ "text/html" ] }
                    ]
                }

            else
                { statusCode = 200
                , body =
                    [ CompilationInterface.SourceFiles.file____README_md.utf8
                    , ""
                    , "This backend process received "
                        ++ String.fromInt state.httpRequestsCount
                        ++ " HTTP requests."
                    ]
                        |> String.join "\n"
                        |> Bytes.Encode.string
                        |> Bytes.Encode.encode
                        |> Just
                , headersToAdd = []
                }
    in
    ( state
    , [ Platform.WebService.RespondToHttpRequest
            { httpRequestId = httpRequestEvent.httpRequestId
            , response = httpResponse
            }
      ]
    )


urlLeadsToFrontendHtmlDocument : Url.Url -> Bool
urlLeadsToFrontendHtmlDocument url =
    not (url.path == "/api" || String.startsWith "/api/" url.path)


initState : State
initState =
    { httpRequestsCount = 0
    , lastHttpRequests = []
    }
