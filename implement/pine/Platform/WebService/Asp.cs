using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Http.Extensions;
using Microsoft.Extensions.DependencyInjection;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace ElmTime.Platform.WebService;

/// <summary>
/// Integrations for ASP.NET (https://learn.microsoft.com/en-us/aspnet/overview)
/// </summary>
public static class Asp
{
    public static void ConfigureServices(IServiceCollection services)
    {
    }

    public static async Task<Pine.Elm.Platform.WebServiceInterface.HttpRequestProperties>
        AsInterfaceHttpRequestAsync(HttpRequest httpRequest)
    {
        var httpHeaders = new List<Pine.Elm.Platform.WebServiceInterface.HttpHeader>(httpRequest.Headers.Count);

        // Convert the headers to the interface representation.
        for (var i = 0; i < httpRequest.Headers.Count; i++)
        {
            var header = httpRequest.Headers.ElementAt(i);

            var values = new List<string>(header.Value.Count);

            for (var j = 0; j < header.Value.Count; j++)
            {
                if (header.Value[j] is { } value)
                    values.Add(value);
            }

            httpHeaders.Add(
                new Pine.Elm.Platform.WebServiceInterface.HttpHeader(
                    Name: header.Key,
                    Values: values));
        }

        var httpRequestBody = await CopyRequestBodyAsync(httpRequest);

        return new Pine.Elm.Platform.WebServiceInterface.HttpRequestProperties
        (
            Method: httpRequest.Method,
            Uri: httpRequest.GetDisplayUrl(),
            Body: httpRequestBody,
            Headers: httpHeaders
        );
    }

    public static async Task SendHttpResponseAsync(
        HttpContext httpContext,
        Pine.Elm.Platform.WebServiceInterface.HttpResponse httpResponse)
    {
        httpContext.Response.StatusCode = httpResponse.StatusCode;

        string? headerContentType = null;

        if (httpResponse.HeadersToAdd is { } headersToAdd)
        {
            foreach (var headerToAdd in headersToAdd)
            {
                if (headerToAdd.Name.Equals("content-type", StringComparison.OrdinalIgnoreCase))
                {
                    if (headerToAdd.Values is { } values && values.Count > 0)
                    {
                        headerContentType = headerToAdd.Values[0];
                    }
                }
                else
                {
                    httpContext.Response.Headers[headerToAdd.Name] =
                        new Microsoft.Extensions.Primitives.StringValues([.. headerToAdd.Values]);
                }
            }
        }

        if (headerContentType is not null)
            httpContext.Response.ContentType = headerContentType;

        httpContext.Response.Headers.XPoweredBy = "Pine";

        if (httpResponse.Body is { } contentAsByteArray)
        {
            httpContext.Response.ContentLength = contentAsByteArray.Length;

            await httpContext.Response.Body.WriteAsync(contentAsByteArray);
        }
        else
        {
            httpContext.Response.ContentLength = 0;
        }
    }

    public static async Task<ReadOnlyMemory<byte>> CopyRequestBodyAsync(HttpRequest httpRequest)
    {
        httpRequest.EnableBuffering(bufferThreshold: 100_000);
        httpRequest.Body.Position = 0;

        using var memoryStream = new MemoryStream();

        await httpRequest.Body.CopyToAsync(memoryStream);

        httpRequest.Body.Position = 0;

        return memoryStream.ToArray();
    }
}
