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
                () => RateLimitMiddlewareFromWebAppConfig(webAppConfig, context,
                () => AdminPersistentProcessMiddlewareFromWebAppConfig(webAppConfig, context, next)));

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

        static async Task AdminPersistentProcessMiddlewareFromWebAppConfig(
            WebAppConfiguration webAppConfig, HttpContext context, Func<Task> next)
        {
            if (context.Request.Path.StartsWithSegments(
                new PathString(Configuration.AdminPath),
                out var requestPathInAdmin))
            {
                /* TODO:
                Remove special handling for this path from the public HTTP interface when the apps in production have been migrated to the new admin interface.
                */

                var configuration = context.RequestServices.GetService<IConfiguration>();

                var rootPassword = configuration.GetValue<string>(Configuration.AdminRootPasswordSettingKey);

                var expectedAuthorization = Configuration.BasicAuthenticationForAdminRoot(rootPassword);

                context.Request.Headers.TryGetValue("Authorization", out var requestAuthorizationHeaderValue);

                AuthenticationHeaderValue.TryParse(
                    requestAuthorizationHeaderValue.FirstOrDefault(), out var requestAuthorization);

                if (!(0 < rootPassword?.Length))
                {
                    context.Response.StatusCode = 403;
                    await context.Response.WriteAsync("Forbidden");
                    return;
                }

                var buffer = new byte[400];

                var decodedRequestAuthorizationParameter =
                    Convert.TryFromBase64String(requestAuthorization?.Parameter ?? "", buffer, out var bytesWritten) ?
                    Encoding.UTF8.GetString(buffer, 0, bytesWritten) : null;

                if (!(string.Equals(expectedAuthorization, decodedRequestAuthorizationParameter) &&
                    string.Equals("basic", requestAuthorization?.Scheme, StringComparison.OrdinalIgnoreCase)))
                {
                    context.Response.StatusCode = 401;
                    context.Response.Headers.Add(
                        "WWW-Authenticate",
                        @"Basic realm=""" + context.Request.Host + Configuration.AdminPath + @""", charset=""UTF-8""");
                    await context.Response.WriteAsync("Unauthorized");
                    return;
                }

                if (string.Equals(requestPathInAdmin, Configuration.ApiPersistentProcessStatePath))
                {
                    if (string.Equals(context.Request.Method, "post", StringComparison.InvariantCultureIgnoreCase))
                    {
                        var stateToSet = new StreamReader(context.Request.Body, System.Text.Encoding.UTF8).ReadToEndAsync().Result;

                        var processStoreWriter = context.RequestServices.GetService<ProcessStore.IProcessStoreWriter>();

                        var persistentProcess = context.RequestServices.GetService<IPersistentProcess>();

                        var compositionRecord = persistentProcess.SetState(stateToSet);

                        processStoreWriter.AppendSerializedCompositionRecord(compositionRecord.serializedCompositionRecord);

                        context.Response.StatusCode = 200;
                        await context.Response.WriteAsync("Successfully set process state.");
                        return;
                    }

                    if (string.Equals(context.Request.Method, "get", StringComparison.InvariantCultureIgnoreCase))
                    {
                        var persistentProcess = context.RequestServices.GetService<IPersistentProcess>();

                        var reducedValueLiteralString =
                            persistentProcess.ReductionRecordForCurrentState().ReducedValueLiteralString;

                        context.Response.StatusCode = 200;
                        context.Response.ContentType = "application/json";
                        await context.Response.WriteAsync(reducedValueLiteralString);
                        return;
                    }

                    context.Response.StatusCode = 405;
                    await context.Response.WriteAsync("Method not supported.");
                    return;
                }

                context.Response.StatusCode = 404;
                await context.Response.WriteAsync("Not Found");
                return;
            }

            await next?.Invoke();
        }

        static public InterfaceToHost.HttpRequest AsPersistentProcessInterfaceHttpRequest(
            HttpRequest httpRequest)
        {
            var httpHeaders =
                httpRequest.Headers
                .Select(header => new InterfaceToHost.HttpHeader { name = header.Key, values = header.Value.ToArray() })
                .ToArray();

            return new InterfaceToHost.HttpRequest
            {
                method = httpRequest.Method,
                uri = httpRequest.GetDisplayUrl(),
                bodyAsString = new System.IO.StreamReader(httpRequest.Body).ReadToEndAsync().Result,
                headers = httpHeaders,
            };
        }
    }
}