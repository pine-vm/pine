module Backend.Main exposing
    ( State
    , backendMain
    )

import Base64
import Bytes.Encode
import CompilationInterface.SourceFiles
import ElmFullstack


type alias State =
    {}


backendMain : ElmFullstack.BackendConfig State
backendMain =
    { init = ( {}, [] )
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
        response =
            if (httpRequestEvent.request.method |> String.toLower) /= "get" then
                { statusCode = 405
                , bodyAsBase64 = Nothing
                , headersToAdd = []
                }

            else
                case httpRequestEvent.request.uri |> String.split "/" |> List.reverse |> List.head of
                    Just "bytes" ->
                        { statusCode = 200
                        , bodyAsBase64 = CompilationInterface.SourceFiles.file____static_content_demo_file_mp3 |> Base64.fromBytes
                        , headersToAdd =
                            [ { name = "Cache-Control", values = [ "public, max-age=3600" ] }
                            ]
                        }

                    Just "utf8" ->
                        { statusCode = 200
                        , bodyAsBase64 =
                            CompilationInterface.SourceFiles.file__utf8____readme_md
                                |> Bytes.Encode.string
                                |> Bytes.Encode.encode
                                |> Base64.fromBytes
                        , headersToAdd =
                            [ { name = "Cache-Control", values = [ "public, max-age=3600" ] }
                            ]
                        }

                    _ ->
                        { statusCode = 404
                        , bodyAsBase64 = Nothing
                        , headersToAdd = []
                        }

        httpResponse =
            { httpRequestId = httpRequestEvent.httpRequestId
            , response = response
            }
    in
    ( stateBefore
    , [ ElmFullstack.RespondToHttpRequest httpResponse ]
    )
