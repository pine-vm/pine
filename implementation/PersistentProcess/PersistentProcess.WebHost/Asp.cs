using System;
using System.Linq;
using System.Text.RegularExpressions;
using System.Threading.Tasks;
using Microsoft.AspNetCore.Http;
using Microsoft.AspNetCore.Http.Extensions;

namespace Kalmit.PersistentProcess.WebHost
{
    static public class Asp
    {
        static public async Task StaticFilesMiddlewareFromWebApp(
            WebAppConfiguration webAppConfig, HttpContext context, Func<Task> next)
        {
            var matchingUrlMapToStaticFile =
                webAppConfig?.Map?.MapsFromRequestUrlToStaticFileName
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
    }
}