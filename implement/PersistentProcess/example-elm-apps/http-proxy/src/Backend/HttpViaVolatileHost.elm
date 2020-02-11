module Backend.HttpViaVolatileHost exposing
    ( HttpRequestProperties
    , HttpResponseProperties
    , decodeVolatileHostHttpResponse
    , requestToVolatileHost
    , volatileHostScript
    )

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


decodeVolatileHostHttpResponse : Json.Decode.Decoder HttpResponseProperties
decodeVolatileHostHttpResponse =
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


requestToVolatileHost : HttpRequestProperties -> String
requestToVolatileHost =
    encodeHttpRequestProperties >> Json.Encode.encode 0


volatileHostScript : String
volatileHostScript =
    """
#r "netstandard"
#r "System"
#r "System.Collections.Immutable"
#r "System.Net"
#r "System.Net.Http"
#r "System.Net.Primitives"
#r "System.Private.Uri"
#r "System.Linq"

//  https://www.nuget.org/api/v2/package/Newtonsoft.Json/12.0.2
#r "sha256:B9B4E633EA6C728BAD5F7CBBEF7F8B842F7E10181731DBE5EC3CD995A6F60287"

using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Net.Http;

public class HttpRequest
{
    public string method;

    public string uri;

    public string bodyAsBase64;

    public HttpHeader[] headers;
}

public class HttpResponse
{
    public int statusCode;

    public string bodyAsBase64;

    public HttpHeader[] headers;
}

public class HttpHeader
{
    public string name;

    public string[] values;
}

HttpResponse GetResponseFromHttpRequest(HttpRequest request)
{
    var headerContentType =
        request?.headers?.FirstOrDefault(header => header.name?.ToLowerInvariant() == "content-type")?.values?.FirstOrDefault();

    var client = new HttpClient();

    var httpRequestMessage = new HttpRequestMessage(method: new HttpMethod(request.method), requestUri: request.uri);

    if (request.bodyAsBase64 != null)
        httpRequestMessage.Content = new ByteArrayContent(Convert.FromBase64String(request.bodyAsBase64));

    httpRequestMessage.Headers.Clear();

    if (request.headers != null)
        foreach (var header in request.headers)
            httpRequestMessage.Headers.TryAddWithoutValidation(header.name, header.values);

    /*
    2019-08-30 Work around problem with `httpRequestMessage.Headers.TryAddWithoutValidation` (used to set headers above):
    Also set the content-type header on `httpRequestMessage.Content.Headers.ContentType`.
    */
    if(headerContentType != null && httpRequestMessage.Content != null)
        httpRequestMessage.Content.Headers.ContentType = System.Net.Http.Headers.MediaTypeHeaderValue.Parse(headerContentType);

    var response = client.SendAsync(httpRequestMessage).Result;

    var responseBodyAsByteArray =
        response.Content?.ReadAsByteArrayAsync().Result;

    var responseBodyAsBase64 =
        responseBodyAsByteArray == null ? null :
        Convert.ToBase64String(responseBodyAsByteArray);

    var responseHeaders =
        response.Headers
        .Select(responseHeader => new HttpHeader { name = responseHeader.Key, values = responseHeader.Value.ToArray() })
        .ToArray();

    return new HttpResponse
    {
        statusCode = (int)response.StatusCode,
        headers = responseHeaders,
        bodyAsBase64 = responseBodyAsBase64,
    };
}

string GetResponseFromHttpRequestSerial(string serializedRequest)
{
    var request = Newtonsoft.Json.JsonConvert.DeserializeObject<HttpRequest>(serializedRequest);

    var response = GetResponseFromHttpRequest(request);

    return Newtonsoft.Json.JsonConvert.SerializeObject(response);
}

string InterfaceToHost_Request(string request)
{
    return GetResponseFromHttpRequestSerial(request);
}

"""
