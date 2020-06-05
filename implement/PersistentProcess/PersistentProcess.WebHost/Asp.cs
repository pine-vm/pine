using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Http.Extensions;
using Microsoft.Extensions.Configuration;
using Microsoft.Extensions.DependencyInjection;
using System;
using System.Collections.Concurrent;
using System.IO;
using System.Linq;
using System.Net.Http.Headers;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading.Tasks;

namespace Kalmit.PersistentProcess.WebHost
{
    static public class Asp
    {
        class ClientsRateLimitStateContainer
        {
            readonly public ConcurrentDictionary<string, RateLimitMutableContainer> RateLimitFromClientId =
                new ConcurrentDictionary<string, RateLimitMutableContainer>();
        }

        static public void ConfigureServices(IServiceCollection services)
        {
            services.AddSingleton(new ClientsRateLimitStateContainer());
        }

        static public async Task MiddlewareFromWebAppConfig(
            WebAppConfiguration webAppConfig, HttpContext context, Func<Task> next) =>
            await StaticFilesMiddlewareFromWebAppConfig(webAppConfig, context,
                () => RateLimitMiddlewareFromWebAppConfig(webAppConfig, context, next));

        static async Task StaticFilesMiddlewareFromWebAppConfig(
            WebAppConfiguration webAppConfig, HttpContext context, Func<Task> next)
        {
            var matchingUrlMapToStaticFile =
                webAppConfig?.JsonStructure?.mapsFromRequestUrlToStaticFileName
                ?.FirstOrDefault(conditionalMap =>
                    Regex.IsMatch(context.Request.GetDisplayUrl(), conditionalMap.matchingRegexPattern));

            if (matchingUrlMapToStaticFile != null)
            {
                if (!string.Equals("get", context.Request.Method, StringComparison.InvariantCultureIgnoreCase))
                {
                    context.Response.StatusCode = 405;
                    await context.Response.WriteAsync("This resource only supports the GET method.");
                    return;
                }

                var staticFilePath = matchingUrlMapToStaticFile.resultString.Split('/');

                var staticFile =
                    webAppConfig?.StaticFiles
                    ?.FirstOrDefault(staticFileNameAndContent =>
                        staticFilePath.SequenceEqual(staticFileNameAndContent.staticFilePath));

                if (staticFile?.staticFileContent == null)
                {
                    context.Response.StatusCode = 404;
                    return;
                }

                context.Response.StatusCode = 200;
                await context.Response.Body.WriteAsync(staticFile?.staticFileContent.ToArray(), 0, staticFile.Value.staticFileContent.Count);
                return;
            }

            await next?.Invoke();
        }

        static async Task RateLimitMiddlewareFromWebAppConfig(
            WebAppConfiguration webAppConfig, HttpContext context, Func<Task> next)
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
                context.RequestServices.GetService<ClientsRateLimitStateContainer>().RateLimitFromClientId;

            var clientRateLimitState =
                rateLimitFromClientId.GetOrAdd(
                    ClientId(), _ => BuildRateLimitContainerForClient(webAppConfig?.JsonStructure));

            if (clientRateLimitState?.AttemptPass(Configuration.GetDateTimeOffset(context).ToUnixTimeMilliseconds()) ?? true)
            {
                await next?.Invoke();
                return;
            }

            context.Response.StatusCode = 429;
            await context.Response.WriteAsync("");
            return;
        }

        static RateLimitMutableContainer BuildRateLimitContainerForClient(WebAppConfigurationJsonStructure jsonStructure)
        {
            if (jsonStructure?.singleRateLimitWindowPerClientIPv4Address == null)
                return null;

            return new RateLimitMutableContainer(new RateLimitStateSingleWindow
            {
                limit = jsonStructure.singleRateLimitWindowPerClientIPv4Address.limit,
                windowSize = jsonStructure.singleRateLimitWindowPerClientIPv4Address.windowSizeInMs,
            });
        }

        static public async Task<InterfaceToHost.HttpRequest> AsPersistentProcessInterfaceHttpRequest(
            HttpRequest httpRequest)
        {
            var httpHeaders =
                httpRequest.Headers
                .Select(header => new InterfaceToHost.HttpHeader { name = header.Key, values = header.Value.ToArray() })
                .ToArray();

            byte[] httpRequestBody = null;

            using (var stream = new MemoryStream())
            {
                await httpRequest.Body.CopyToAsync(stream);

                httpRequestBody = stream.ToArray();
            }

            return new InterfaceToHost.HttpRequest
            {
                method = httpRequest.Method,
                uri = httpRequest.GetDisplayUrl(),
                bodyAsBase64 = httpRequestBody == null ? null : Convert.ToBase64String(httpRequestBody),
                headers = httpHeaders,
            };
        }
    }
}