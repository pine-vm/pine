module HttpViaVolatileHost exposing
    ( HttpRequestProperties
    , HttpResponseProperties
    , decodeVolatileHostHttpResponse
    , scriptToGetResponseFromHttpRequest
    )

import ElmAppInKalmitProcess
import Json.Decode
import Json.Encode


type alias HttpRequestProperties =
    { uri : String
    , method : String
    , headers : List HttpHeader
    , bodyAsString : Maybe String
    }


type alias HttpResponseProperties =
    { statusCode : Int
    , headers : List HttpHeader
    , bodyAsString : Maybe String
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
        (ElmAppInKalmitProcess.decodeOptionalField "bodyAsString" Json.Decode.string)


decodeHttpHeader : Json.Decode.Decoder HttpHeader
decodeHttpHeader =
    Json.Decode.map2 HttpHeader
        (Json.Decode.field "name" Json.Decode.string)
        (Json.Decode.field "values" (Json.Decode.list Json.Decode.string))


encodeHttpRequestProperties : HttpRequestProperties -> Json.Encode.Value
encodeHttpRequestProperties httpRequestProperties =
    Json.Encode.object
        ([ ( "uri", httpRequestProperties.uri |> Json.Encode.string )
         , ( "method", httpRequestProperties.method |> Json.Encode.string )
         , ( "headers", httpRequestProperties.headers |> Json.Encode.list encodeHttpHeader )
         ]
            ++ (httpRequestProperties.bodyAsString
                    |> Maybe.map (\bodyAsString -> [ ( "bodyAsString", bodyAsString |> Json.Encode.string ) ])
                    |> Maybe.withDefault []
               )
        )


encodeHttpHeader : HttpHeader -> Json.Encode.Value
encodeHttpHeader httpHeader =
    [ ( "name", httpHeader.name |> Json.Encode.string )
    , ( "values", httpHeader.values |> Json.Encode.list Json.Encode.string )
    ]
        |> Json.Encode.object


scriptToGetResponseFromHttpRequest : HttpRequestProperties -> String
scriptToGetResponseFromHttpRequest httpRequest =
    let
        httpRequestExpression =
            httpRequest |> encodeHttpRequestProperties |> Json.Encode.encode 0

        expression =
            "GetResponseFromHttpRequestSerial("
                ++ (httpRequestExpression |> Json.Encode.string |> Json.Encode.encode 0)
                ++ ")"
    in
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

    public string bodyAsString;

    public HttpHeader[] headers;
}

public class HttpResponse
{
    public int statusCode;

    public string bodyAsString;

    public HttpHeader[] headers;
}

public class HttpHeader
{
    public string name;

    public string[] values;
}

HttpResponse GetResponseFromHttpRequest(HttpRequest request)
{
    var client = new HttpClient();

    var httpRequestMessage = new HttpRequestMessage(method: new HttpMethod(request.method), requestUri: request.uri);

    if (request.bodyAsString != null)
        httpRequestMessage.Content = new StringContent(request.bodyAsString);

    httpRequestMessage.Headers.Clear();

    if (request.headers != null)
        foreach (var header in request.headers)
            httpRequestMessage.Headers.TryAddWithoutValidation(header.name, header.values);

    var response = client.SendAsync(httpRequestMessage).Result;

    var responseBodyAsString =
        response.Content == null ?
        null :
        response.Content.ReadAsStringAsync().Result;

    var responseHeaders =
        response.Headers
        .Select(responseHeader => new HttpHeader { name = responseHeader.Key, values = responseHeader.Value.ToArray() })
        .ToArray();

    return new HttpResponse
    {
        statusCode = (int)response.StatusCode,
        headers = responseHeaders,
        bodyAsString = responseBodyAsString,
    };
}

string GetResponseFromHttpRequestSerial(string serializedRequest)
{
    var request = Newtonsoft.Json.JsonConvert.DeserializeObject<HttpRequest>(serializedRequest);

    var response = GetResponseFromHttpRequest(request);

    return Newtonsoft.Json.JsonConvert.SerializeObject(response);
}
""" ++ expression
