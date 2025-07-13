#r "netstandard"
#r "System"
#r "System.Collections.Immutable"
#r "System.Net"
#r "System.Net.Http"
#r "System.Net.Primitives"
#r "System.Private.Uri"
#r "System.Linq"
#r "System.Text.Json"


using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Net.Http;

public record HttpRequest(
    string method,
    string uri,
    string bodyAsBase64,
    HttpHeader[] headers);

public record HttpResponse(
    int statusCode,
    string bodyAsBase64,
    HttpHeader[] headers);

public record HttpHeader(
    string name,
    string[] values);

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
        .Select(responseHeader => new HttpHeader(name: responseHeader.Key, values: responseHeader.Value.ToArray()))
        .ToArray();

    return new HttpResponse
    (
        statusCode: (int)response.StatusCode,
        headers: responseHeaders,
        bodyAsBase64: responseBodyAsBase64
    );
}

string GetResponseFromHttpRequestSerial(string serializedRequest)
{
    var request = System.Text.Json.JsonSerializer.Deserialize<HttpRequest>(serializedRequest);

    var response = GetResponseFromHttpRequest(request);

    return System.Text.Json.JsonSerializer.Serialize(response);
}

string InterfaceToHost_Request(string request)
{
    return GetResponseFromHttpRequestSerial(request);
}

