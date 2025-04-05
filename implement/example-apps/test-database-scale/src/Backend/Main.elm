module Backend.Main exposing
    ( State
    , webServiceMain
    )

import Backend.State
import Base64
import Bytes
import Bytes.Encode
import CompilationInterface.ElmMake
import CompilationInterface.GenerateJsonConverters
import Dict
import Json.Encode
import Platform.WebService
import Url
import Url.Parser exposing ((</>))


type alias State =
    Backend.State.State


type Route
    = EntryRoute (Maybe Int)


webServiceMain : Platform.WebService.WebServiceConfig State
webServiceMain =
    { init =
        ( { store = Dict.empty }
        , []
        )
    , subscriptions = subscriptions
    }


subscriptions : State -> Platform.WebService.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent : Platform.WebService.HttpRequestEventStruct -> State -> ( State, Platform.WebService.Commands State )
updateForHttpRequestEvent httpRequestEvent stateBefore =
    let
        staticContentHttpHeaders { contentType, contentEncoding } =
            { cacheMaxAgeMinutes = Just (60 * 24)
            , contentType = contentType
            , contentEncoding = contentEncoding
            }

        httpResponseOkWithBody body contentConfig =
            { statusCode = 200
            , body = body
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
            httpResponseOkWithBody
                (Base64.toBytes (frontendHtmlDocumentBase64 frontendConfig))
                (staticContentHttpHeaders { contentType = "text/html", contentEncoding = Nothing })

        continueWithStaticHttpResponse httpResponse =
            ( stateBefore
            , [ Platform.WebService.RespondToHttpRequest
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
                        responseDict : List ( Int, { length : Int } )
                        responseDict =
                            stateBefore.store
                                |> Dict.toList
                                |> List.map
                                    (\( entryId, entry ) ->
                                        ( entryId
                                        , { length = Bytes.width entry.entryBytes }
                                        )
                                    )
                    in
                    ( stateBefore
                    , [ Platform.WebService.RespondToHttpRequest
                            { httpRequestId = httpRequestEvent.httpRequestId
                            , response =
                                { statusCode = 200
                                , body =
                                    responseDict
                                        |> CompilationInterface.GenerateJsonConverters.jsonEncodeGetDirectoryResponse
                                        |> Json.Encode.encode 0
                                        |> Bytes.Encode.string
                                        |> Bytes.Encode.encode
                                        |> Just
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
                                    , [ Platform.WebService.RespondToHttpRequest
                                            { httpRequestId = httpRequestEvent.httpRequestId
                                            , response =
                                                { statusCode = 404
                                                , body = Nothing
                                                , headersToAdd = []
                                                }
                                            }
                                      ]
                                    )

                                Just entry ->
                                    ( stateBefore
                                    , [ Platform.WebService.RespondToHttpRequest
                                            { httpRequestId = httpRequestEvent.httpRequestId
                                            , response =
                                                httpResponseOkWithBody
                                                    (Just entry.entryBytes)
                                                    { cacheMaxAgeMinutes = Nothing
                                                    , contentType = "text/html"
                                                    , contentEncoding = Nothing
                                                    }
                                            }
                                      ]
                                    )

                        "post" ->
                            case httpRequestEvent.request.body of
                                Nothing ->
                                    ( stateBefore
                                    , [ Platform.WebService.RespondToHttpRequest
                                            { httpRequestId = httpRequestEvent.httpRequestId
                                            , response =
                                                { statusCode = 400
                                                , body = Nothing
                                                , headersToAdd = []
                                                }
                                            }
                                      ]
                                    )

                                Just entryBytes ->
                                    let
                                        store =
                                            Dict.insert
                                                entryId
                                                { entryBytes = entryBytes }
                                                stateBefore.store
                                    in
                                    ( { stateBefore | store = store }
                                    , [ Platform.WebService.RespondToHttpRequest
                                            { httpRequestId = httpRequestEvent.httpRequestId
                                            , response =
                                                { statusCode = 200
                                                , body = Nothing
                                                , headersToAdd = []
                                                }
                                            }
                                      ]
                                    )

                        _ ->
                            ( stateBefore
                            , [ Platform.WebService.RespondToHttpRequest
                                    { httpRequestId = httpRequestEvent.httpRequestId
                                    , response =
                                        { statusCode = 405
                                        , body = Nothing
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
