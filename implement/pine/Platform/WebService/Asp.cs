using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Http.Extensions;
using Microsoft.Extensions.DependencyInjection;
using Pine;
using System;
using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Threading.Tasks;

namespace ElmTime.Platform.WebService;

public static class Asp
{
    private class ClientsRateLimitStateContainer
    {
        public readonly ConcurrentDictionary<string, IMutableRateLimit> RateLimitFromClientId = new();
    }

    public static void ConfigureServices(IServiceCollection services)
    {
        services.AddSingleton(new ClientsRateLimitStateContainer());
    }

    public static async Task MiddlewareFromWebServiceConfig(
        WebServiceConfigJson? serverConfig, HttpContext context, Func<Task> next) =>
        await RateLimitMiddlewareFromWebServiceConfig(serverConfig, context, next);

    private static async Task RateLimitMiddlewareFromWebServiceConfig(
        WebServiceConfigJson? serverConfig,
        HttpContext context,
        Func<Task> next)
    {
        string clientId()
        {
            const string defaultClientId = "MapToIPv4-failed";
            try
            {
                return context.Connection.RemoteIpAddress?.MapToIPv4().ToString() ?? defaultClientId;
            }
            catch
            {
                return defaultClientId;
            }
        }

        var rateLimitFromClientId =
            context.RequestServices.GetService<ClientsRateLimitStateContainer>()!.RateLimitFromClientId;

        var clientRateLimitState =
            rateLimitFromClientId.GetOrAdd(
                clientId(), _ => BuildRateLimitContainerForClient(serverConfig));

        if (clientRateLimitState?.AttemptPass(Configuration.GetDateTimeOffset(context).ToUnixTimeMilliseconds()) ?? true)
        {
            await next.Invoke();
            return;
        }

        context.Response.StatusCode = 429;
        await context.Response.WriteAsync("");
    }

    private static IMutableRateLimit BuildRateLimitContainerForClient(WebServiceConfigJson? jsonStructure)
    {
        if (jsonStructure?.singleRateLimitWindowPerClientIPv4Address == null)
            return new MutableRateLimitAlwaysPassing();

        return new RateLimitMutableContainer(new RateLimitStateSingleWindow
        (
            limit: jsonStructure.singleRateLimitWindowPerClientIPv4Address.limit,
            windowSize: jsonStructure.singleRateLimitWindowPerClientIPv4Address.windowSizeInMs,
            passes: ImmutableQueue<long>.Empty
        ));
    }

    public static async Task<InterfaceToHost.HttpRequest> AsPersistentProcessInterfaceHttpRequest(
        HttpRequest httpRequest)
    {
        var httpHeaders =
            httpRequest.Headers
            .Select(header => new InterfaceToHost.HttpHeader(name: header.Key, values: header.Value.ToArray().WhereNotNull().ToArray()))
            .ToArray();

        var httpRequestBody = await CopyRequestBody(httpRequest);

        return new InterfaceToHost.HttpRequest
        (
            method: httpRequest.Method,
            uri: httpRequest.GetDisplayUrl(),
            bodyAsBase64: Maybe.NothingFromNull(Convert.ToBase64String(httpRequestBody.Span)),
            headers: httpHeaders
        );
    }

    public static async Task<ReadOnlyMemory<byte>> CopyRequestBody(HttpRequest httpRequest)
    {
        httpRequest.EnableBuffering(bufferThreshold: 100_000);
        httpRequest.Body.Position = 0;

        using var memoryStream = new MemoryStream();

        await httpRequest.Body.CopyToAsync(memoryStream);

        httpRequest.Body.Position = 0;

        return memoryStream.ToArray();
    }
}
