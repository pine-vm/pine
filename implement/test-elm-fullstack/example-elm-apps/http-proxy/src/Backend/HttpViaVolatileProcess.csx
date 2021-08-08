#r "netstandard"
#r "System"
#r "System.Collections.Immutable"
#r "System.Net"
#r "System.Net.Http"
#r "System.Net.Primitives"
#r "System.Private.Uri"
#r "System.Linq"

//  https://www.nuget.org/api/v2/package/Newtonsoft.Json/12.0.2
#r "sha256:b9b4e633ea6c728bad5f7cbbef7f8b842f7e10181731dbe5ec3cd995a6f60287"

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
    if (headerContentType != null && httpRequestMessage.Content != null)
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

