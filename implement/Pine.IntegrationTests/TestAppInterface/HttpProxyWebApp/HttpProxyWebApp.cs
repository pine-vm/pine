using Pine.Core;
using System.Collections.Generic;

namespace Pine.IntegrationTests.TestAppInterface.HttpProxyWebApp;


public record HttpRequest(
    string method,
    string uri,
    Maybe<string> bodyAsBase64,
    HttpHeader[] headers);

public record HttpRequestContext(
    Maybe<string> clientAddress);

public record HttpResponseRequest(
    string httpRequestId,
    HttpResponse response);

public record HttpResponse(
    int statusCode,
    Maybe<string> bodyAsBase64,
    IReadOnlyList<HttpHeader> headersToAdd);

public record HttpHeader(
    string name,
    string[] values);
