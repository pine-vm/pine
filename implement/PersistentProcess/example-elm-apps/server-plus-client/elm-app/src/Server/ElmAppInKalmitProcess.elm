module Server.ElmAppInKalmitProcess exposing
    ( HttpRequestContext
    , HttpRequestEvent
    , HttpResponse
    , HttpResponseResponse
    , KalmitProcessEvent(..)
    , KalmitProcessResponse(..)
    , wrapUpdateForSerialInterface
    )

import Json.Decode
import Json.Encode


type KalmitProcessEvent
    = HttpRequest HttpRequestEvent


type KalmitProcessResponse
    = CompleteHttpResponse HttpResponseResponse


type alias HttpRequestEvent =
    { httpRequestId : String
    , posixTimeMilli : Int
    , requestContext : HttpRequestContext
    , request : HttpRequestProperties
    }


type alias HttpRequestContext =
    { clientAddress : Maybe String
    }


type alias HttpRequestProperties =
    { method : String
    , uri : String
    , bodyAsString : Maybe String
    , headers : List HttpHeader
    }


type ResponseOverSerialInterface
    = DecodeError String
    | DecodeSuccess (List KalmitProcessResponse)


type alias HttpResponseResponse =
    { httpRequestId : String
    , response : HttpResponse
    }


type alias HttpResponse =
    { statusCode : Int
    , bodyAsString : Maybe String
    , headersToAdd : List HttpHeader
    }


type alias HttpHeader =
    { name : String
    , values : List String
    }


wrapUpdateForSerialInterface : (KalmitProcessEvent -> state -> ( state, List KalmitProcessResponse )) -> String -> state -> ( state, String )
wrapUpdateForSerialInterface update serializedEvent stateBefore =
    let
        ( state, response ) =
            case serializedEvent |> Json.Decode.decodeString decodeKalmitProcessEvent of
                Err error ->
                    ( stateBefore
                    , ("Failed to deserialize event: " ++ (error |> Json.Decode.errorToString))
                        |> DecodeError
                    )

                Ok hostEvent ->
                    stateBefore
                        |> update hostEvent
                        |> Tuple.mapSecond DecodeSuccess
    in
    ( state, response |> encodeResponseOverSerialInterface |> Json.Encode.encode 0 )


decodeKalmitProcessEvent : Json.Decode.Decoder KalmitProcessEvent
decodeKalmitProcessEvent =
    Json.Decode.oneOf
        [ Json.Decode.field "httpRequest" decodeHttpRequestEvent |> Json.Decode.map HttpRequest
        ]


decodeHttpRequestEvent : Json.Decode.Decoder HttpRequestEvent
decodeHttpRequestEvent =
    Json.Decode.map4 HttpRequestEvent
        (Json.Decode.field "httpRequestId" Json.Decode.string)
        (Json.Decode.field "posixTimeMilli" Json.Decode.int)
        (Json.Decode.field "requestContext" decodeHttpRequestContext)
        (Json.Decode.field "request" decodeHttpRequest)


decodeHttpRequestContext : Json.Decode.Decoder HttpRequestContext
decodeHttpRequestContext =
    Json.Decode.map HttpRequestContext
        (decodeOptionalField "clientAddress" Json.Decode.string)


decodeHttpRequest : Json.Decode.Decoder HttpRequestProperties
decodeHttpRequest =
    Json.Decode.map4 HttpRequestProperties
        (Json.Decode.field "method" Json.Decode.string)
        (Json.Decode.field "uri" Json.Decode.string)
        (decodeOptionalField "bodyAsString" Json.Decode.string)
        (Json.Decode.field "headers" (Json.Decode.list decodeHttpHeader))


decodeHttpHeader : Json.Decode.Decoder HttpHeader
decodeHttpHeader =
    Json.Decode.map2 HttpHeader
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "values" (Json.Decode.list Json.Decode.string))


decodeOptionalField : String -> Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe a)
decodeOptionalField fieldName decoder =
    let
        finishDecoding json =
            case Json.Decode.decodeValue (Json.Decode.field fieldName Json.Decode.value) json of
                Ok _ ->
                    Json.Decode.map Just (Json.Decode.field fieldName decoder)

                Err _ ->
                    Json.Decode.succeed Nothing
    in
    Json.Decode.value |> Json.Decode.andThen finishDecoding


encodeResponseOverSerialInterface : ResponseOverSerialInterface -> Json.Encode.Value
encodeResponseOverSerialInterface responseOverSerialInterface =
    (case responseOverSerialInterface of
        DecodeError error ->
            [ ( "decodeError", error |> Json.Encode.string ) ]

        DecodeSuccess response ->
            [ ( "decodeSuccess", response |> Json.Encode.list encodeKalmitProcessResponse ) ]
    )
        |> Json.Encode.object


encodeKalmitProcessResponse : KalmitProcessResponse -> Json.Encode.Value
encodeKalmitProcessResponse response =
    case response of
        CompleteHttpResponse httpResponse ->
            [ ( "completeHttpResponse", httpResponse |> encodeHttpResponseResponse )
            ]
                |> Json.Encode.object


encodeHttpResponseResponse : HttpResponseResponse -> Json.Encode.Value
encodeHttpResponseResponse httpResponseResponse =
    Json.Encode.object
        [ ( "httpRequestId", httpResponseResponse.httpRequestId |> Json.Encode.string )
        , ( "response", httpResponseResponse.response |> encodeHttpResponse )
        ]


encodeHttpResponse : HttpResponse -> Json.Encode.Value
encodeHttpResponse httpResponse =
    [ ( "statusCode", httpResponse.statusCode |> Json.Encode.int |> Just )
    , ( "headersToAdd", httpResponse.headersToAdd |> Json.Encode.list encodeHttpHeader |> Just )
    , ( "bodyAsString", httpResponse.bodyAsString |> Maybe.map Json.Encode.string )
    ]
        |> filterTakeOnlyWhereTupleSecondNotNothing
        |> Json.Encode.object


encodeHttpHeader : HttpHeader -> Json.Encode.Value
encodeHttpHeader httpHeader =
    [ ( "name", httpHeader.name |> Json.Encode.string )
    , ( "values", httpHeader.values |> Json.Encode.list Json.Encode.string )
    ]
        |> Json.Encode.object


filterTakeOnlyWhereTupleSecondNotNothing : List ( first, Maybe second ) -> List ( first, second )
filterTakeOnlyWhereTupleSecondNotNothing =
    List.filterMap
        (\( first, maybeSecond ) ->
            maybeSecond |> Maybe.map (\second -> ( first, second ))
        )
