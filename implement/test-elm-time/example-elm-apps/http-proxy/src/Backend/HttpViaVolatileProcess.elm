module Backend.HttpViaVolatileProcess exposing
    ( HttpRequestProperties
    , HttpResponseProperties
    , decodeVolatileProcessHttpResponse
    , programCode
    , requestToVolatileProcess
    )

import CompilationInterface.SourceFiles
import Json.Decode
import Json.Encode


type alias HttpRequestProperties =
    { uri : String
    , method : String
    , headers : List HttpHeader
    , bodyAsBase64 : Maybe String
    }


type alias HttpResponseProperties =
    { statusCode : Int
    , headers : List HttpHeader
    , bodyAsBase64 : Maybe String
    }


type alias HttpHeader =
    { name : String
    , values : List String
    }


decodeVolatileProcessHttpResponse : Json.Decode.Decoder HttpResponseProperties
decodeVolatileProcessHttpResponse =
    Json.Decode.map3 HttpResponseProperties
        (Json.Decode.field "statusCode" Json.Decode.int)
        (Json.Decode.field "headers" (Json.Decode.list decodeHttpHeader))
        (Json.Decode.field "bodyAsBase64" (jsonDecodeNullAsMaybeNothing Json.Decode.string))


decodeHttpHeader : Json.Decode.Decoder HttpHeader
decodeHttpHeader =
    Json.Decode.map2 HttpHeader
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "values" (Json.Decode.list Json.Decode.string))


encodeHttpRequestProperties : HttpRequestProperties -> Json.Encode.Value
encodeHttpRequestProperties httpRequestProperties =
    Json.Encode.object
        [ ( "uri", httpRequestProperties.uri |> Json.Encode.string )
        , ( "method", httpRequestProperties.method |> Json.Encode.string )
        , ( "headers", httpRequestProperties.headers |> Json.Encode.list encodeHttpHeader )
        , ( "bodyAsBase64", httpRequestProperties.bodyAsBase64 |> jsonEncodeMaybeNothingAsNull Json.Encode.string )
        ]


encodeHttpHeader : HttpHeader -> Json.Encode.Value
encodeHttpHeader httpHeader =
    [ ( "name", httpHeader.name |> Json.Encode.string )
    , ( "values", httpHeader.values |> Json.Encode.list Json.Encode.string )
    ]
        |> Json.Encode.object


jsonEncodeMaybeNothingAsNull : (a -> Json.Encode.Value) -> Maybe a -> Json.Encode.Value
jsonEncodeMaybeNothingAsNull encoder =
    Maybe.map encoder >> Maybe.withDefault Json.Encode.null


jsonDecodeNullAsMaybeNothing : Json.Decode.Decoder a -> Json.Decode.Decoder (Maybe a)
jsonDecodeNullAsMaybeNothing =
    Json.Decode.nullable


requestToVolatileProcess : HttpRequestProperties -> String
requestToVolatileProcess =
    encodeHttpRequestProperties >> Json.Encode.encode 0


programCode : String
programCode =
    CompilationInterface.SourceFiles.file____src_Backend_HttpViaVolatileProcess_csx.utf8
