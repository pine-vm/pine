using System;
using System.Collections.Concurrent;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Http.Extensions;
using Microsoft.Extensions.DependencyInjection;

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
                webAppConfig?.Map?.mapsFromRequestUrlToStaticFileName
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

                var staticFile =
                    webAppConfig?.StaticFiles
                    ?.FirstOrDefault(staticFileNameAndContent =>
                        string.Equals(staticFileNameAndContent.staticFileName, matchingUrlMapToStaticFile.resultString));

                if (staticFile?.staticFileContent == null)
                {
                    context.Response.StatusCode = 404;
                    return;
                }

                context.Response.StatusCode = 200;
                await context.Response.Body.WriteAsync(staticFile?.staticFileContent, 0, staticFile.Value.staticFileContent.Length);
                return;
            }

            await next?.Invoke();
        }

        static async Task RateLimitMiddlewareFromWebAppConfig(
            WebAppConfiguration webAppConfig, HttpContext context, Func<Task> next)
        {
            string ClientId()
            {
                try
                {
                    return context.Connection.RemoteIpAddress.MapToIPv4().ToString();
                }
                catch
                {
                    return "MapToIPv4-failed";
                }
            }

            var rateLimitFromClientId =
                context.RequestServices.GetService<ClientsRateLimitStateContainer>().RateLimitFromClientId;

            var clientRateLimitState =
                rateLimitFromClientId.GetOrAdd(
                    ClientId(), _ => BuildRateLimitContainerForClient(webAppConfig?.Map));

            if (clientRateLimitState?.AttemptPass(Configuration.GetDateTimeOffset(context).ToUnixTimeMilliseconds()) ?? true)
            {
                await next?.Invoke();
                return;
            }

            context.Response.StatusCode = 429;
            await context.Response.WriteAsync("");
            return;
        }

        static RateLimitMutableContainer BuildRateLimitContainerForClient(WebAppConfigurationMap map)
        {
            if (map?.singleRateLimitWindowPerClientIPv4Address == null)
                return null;

            return new RateLimitMutableContainer(new RateLimitStateSingleWindow
            {
                limit = map.singleRateLimitWindowPerClientIPv4Address.limit,
                windowSize = map.singleRateLimitWindowPerClientIPv4Address.windowSizeInMs,
            });
        }
    }
}