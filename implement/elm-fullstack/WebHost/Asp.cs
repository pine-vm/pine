using System;
using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Http.Extensions;
using Microsoft.Extensions.DependencyInjection;

namespace ElmFullstack.WebHost;

static public class Asp
{
    class ClientsRateLimitStateContainer
    {
        readonly public ConcurrentDictionary<string, IMutableRateLimit> RateLimitFromClientId = new();
    }

    static public void ConfigureServices(IServiceCollection services)
    {
        services.AddSingleton(new ClientsRateLimitStateContainer());
    }

    static public async Task MiddlewareFromWebAppConfig(
        WebAppConfigurationJsonStructure? webAppConfig, HttpContext context, Func<Task> next) =>
        await RateLimitMiddlewareFromWebAppConfig(webAppConfig, context, next);

    static async Task RateLimitMiddlewareFromWebAppConfig(
        WebAppConfigurationJsonStructure? webAppConfig,
        HttpContext context,
        Func<Task> next)
    {
        string ClientId()
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
                ClientId(), _ => BuildRateLimitContainerForClient(webAppConfig));

        if (clientRateLimitState?.AttemptPass(Configuration.GetDateTimeOffset(context).ToUnixTimeMilliseconds()) ?? true)
        {
            await next.Invoke();
            return;
        }

        context.Response.StatusCode = 429;
        await context.Response.WriteAsync("");
        return;
    }

    static IMutableRateLimit BuildRateLimitContainerForClient(WebAppConfigurationJsonStructure? jsonStructure)
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

    static public async Task<InterfaceToHost.HttpRequest> AsPersistentProcessInterfaceHttpRequest(
        HttpRequest httpRequest)
    {
        var httpHeaders =
            httpRequest.Headers
            .Select(header => new InterfaceToHost.HttpHeader(name: header.Key, values: header.Value.ToArray()))
            .ToArray();

        using var stream = new MemoryStream();

        await httpRequest.Body.CopyToAsync(stream);

        var httpRequestBody = stream.ToArray();

        return new InterfaceToHost.HttpRequest
        (
            method: httpRequest.Method,
            uri: httpRequest.GetDisplayUrl(),
            bodyAsBase64: Convert.ToBase64String(httpRequestBody),
            headers: httpHeaders
        );
    }
}
