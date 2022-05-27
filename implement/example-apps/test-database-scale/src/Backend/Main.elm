module Backend.Main exposing
    ( State
    , backendMain
    )

import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode
import CompilationInterface.ElmMake
import CompilationInterface.GenerateJsonCoders
import Dict
import ElmFullstack
import Json.Encode
import Url
import Url.Parser exposing ((</>))


type alias State =
    { store : Dict.Dict Int String }


type Route
    = EntryRoute (Maybe Int)


backendMain : ElmFullstack.BackendConfig State
backendMain =
    { init =
        ( { store = Dict.empty }
        , []
        )
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
        bodyFromString =
            Bytes.Encode.string >> Bytes.Encode.encode >> Base64.fromBytes

        staticContentHttpHeaders { contentType, contentEncoding } =
            { cacheMaxAgeMinutes = Just (60 * 24)
            , contentType = contentType
            , contentEncoding = contentEncoding
            }

        httpResponseOkWithStringContent stringContent httpResponseHeaders =
            httpResponseOkWithBodyAsBase64 (bodyFromString stringContent) httpResponseHeaders

        httpResponseOkWithBodyAsBase64 bodyAsBase64 contentConfig =
            { statusCode = 200
            , bodyAsBase64 = bodyAsBase64
            , headersToAdd =
                [ ( "Cache-Control"
                  , contentConfig.cacheMaxAgeMinutes
                        |> Maybe.map (\maxAgeMinutes -> "public, max-age=" ++ String.fromInt (maxAgeMinutes * 60))
                  )
                , ( "Content-Type", Just contentConfig.contentType )
                , ( "Content-Encoding", contentConfig.contentEncoding )
                ]
                    |> List.concatMap
                        (\( name, maybeValue ) ->
                            maybeValue
                                |> Maybe.map (\value -> [ { name = name, values = [ value ] } ])
                                |> Maybe.withDefault []
                        )
            }

        frontendHtmlDocumentResponse frontendConfig =
            httpResponseOkWithBodyAsBase64
                (Just (frontendHtmlDocumentBase64 frontendConfig))
                (staticContentHttpHeaders { contentType = "text/html", contentEncoding = Nothing })

        continueWithStaticHttpResponse httpResponse =
            ( stateBefore
            , [ ElmFullstack.RespondToHttpRequest
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response = httpResponse
                    }
              ]
            )
    in
    case httpRequestEvent.request.uri |> Url.fromString |> Maybe.andThen routeFromUrl of
        Nothing ->
            frontendHtmlDocumentResponse { debug = False }
                |> continueWithStaticHttpResponse

        Just (EntryRoute maybeEntryId) ->
            case maybeEntryId of
                Nothing ->
                    let
                        responseDict =
                            stateBefore.store
                                |> Dict.map (\_ entry -> { length = String.length entry })
                    in
                    ( stateBefore
                    , [ ElmFullstack.RespondToHttpRequest
                            { httpRequestId = httpRequestEvent.httpRequestId
                            , response =
                                { statusCode = 200
                                , bodyAsBase64 =
                                    responseDict
                                        |> CompilationInterface.GenerateJsonCoders.jsonEncodeGetDirectoryResponse
                                        |> Json.Encode.encode 0
                                        |> Bytes.Encode.string
                                        |> Bytes.Encode.encode
                                        |> Base64.fromBytes
                                , headersToAdd = []
                                }
                            }
                      ]
                    )

                Just entryId ->
                    case String.toLower httpRequestEvent.request.method of
                        "get" ->
                            case stateBefore.store |> Dict.get entryId of
                                Nothing ->
                                    ( stateBefore
                                    , [ ElmFullstack.RespondToHttpRequest
                                            { httpRequestId = httpRequestEvent.httpRequestId
                                            , response =
                                                { statusCode = 404
                                                , bodyAsBase64 = Nothing
                                                , headersToAdd = []
                                                }
                                            }
                                      ]
                                    )

                                Just entry ->
                                    ( stateBefore
                                    , [ ElmFullstack.RespondToHttpRequest
                                            { httpRequestId = httpRequestEvent.httpRequestId
                                            , response =
                                                httpResponseOkWithStringContent entry
                                                    { cacheMaxAgeMinutes = Nothing
                                                    , contentType = "text/html"
                                                    , contentEncoding = Nothing
                                                    }
                                            }
                                      ]
                                    )

                        "post" ->
                            case
                                httpRequestEvent.request.bodyAsBase64
                                    |> Maybe.andThen Base64.toBytes
                                    |> Maybe.andThen decodeBytesToString
                            of
                                Nothing ->
                                    ( stateBefore
                                    , [ ElmFullstack.RespondToHttpRequest
                                            { httpRequestId = httpRequestEvent.httpRequestId
                                            , response =
                                                { statusCode = 400
                                                , bodyAsBase64 = Nothing
                                                , headersToAdd = []
                                                }
                                            }
                                      ]
                                    )

                                Just entry ->
                                    let
                                        store =
                                            stateBefore.store |> Dict.insert entryId entry
                                    in
                                    ( { stateBefore | store = store }
                                    , [ ElmFullstack.RespondToHttpRequest
                                            { httpRequestId = httpRequestEvent.httpRequestId
                                            , response =
                                                { statusCode = 200
                                                , bodyAsBase64 = Nothing
                                                , headersToAdd = []
                                                }
                                            }
                                      ]
                                    )

                        _ ->
                            ( stateBefore
                            , [ ElmFullstack.RespondToHttpRequest
                                    { httpRequestId = httpRequestEvent.httpRequestId
                                    , response =
                                        { statusCode = 405
                                        , bodyAsBase64 = Nothing
                                        , headersToAdd = []
                                        }
                                    }
                              ]
                            )


frontendHtmlDocumentBase64 : { debug : Bool } -> String
frontendHtmlDocumentBase64 { debug } =
    CompilationInterface.ElmMake.elm_make____src_Frontend_Main_elm
        |> (if debug then
                .debug >> .base64

            else
                .base64
           )


routeFromUrl : Url.Url -> Maybe Route
routeFromUrl =
    Url.Parser.parse
        (Url.Parser.oneOf
            [ Url.Parser.map (Just >> EntryRoute) (Url.Parser.s "api" </> Url.Parser.s "entry" </> Url.Parser.int)
            , Url.Parser.map (EntryRoute Nothing) (Url.Parser.s "api" </> Url.Parser.s "entry")
            ]
        )


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))
