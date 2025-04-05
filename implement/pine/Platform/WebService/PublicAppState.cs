using FluffySpoon.AspNet.EncryptWeMust;
using Microsoft.AspNetCore.Builder;
using Microsoft.AspNetCore.Hosting;
using Microsoft.AspNetCore.Http;
using Microsoft.Extensions.DependencyInjection;
using Microsoft.Extensions.Hosting;
using Microsoft.Extensions.Logging;
using Pine.Core;
using Pine.Elm.Platform;
using System;
using System.Collections.Generic;
using System.Linq;

namespace ElmTime.Platform.WebService;

public class PublicAppState(
    ServerAndElmAppConfig serverAndElmAppConfig,
    Func<DateTimeOffset> getDateTimeOffset)
{
    private long nextHttpRequestIndex = 0;

    public readonly System.Threading.CancellationTokenSource applicationStoppingCancellationTokenSource = new();

    private readonly ServerAndElmAppConfig serverAndElmAppConfig = serverAndElmAppConfig;
    private readonly Func<DateTimeOffset> getDateTimeOffset = getDateTimeOffset;

    public WebApplication Build(
        WebApplicationBuilder appBuilder,
        ILogger logger,
        IHostEnvironment env,
        IReadOnlyList<string> publicWebHostUrls,
        bool? disableLetsEncrypt,
        bool disableHttps)
    {
        appBuilder.Services.AddLogging(logging =>
        {
            logging.AddConsole();
            logging.AddDebug();
        });

        var enableUseFluffySpoonLetsEncrypt =
            serverAndElmAppConfig.ServerConfig?.letsEncryptOptions is not null && !(disableLetsEncrypt ?? false);

        var canUseHttps =
            enableUseFluffySpoonLetsEncrypt;

        var publicWebHostUrlsFilteredForHttps =
            canUseHttps && !disableHttps
            ?
            publicWebHostUrls
            :
            [.. publicWebHostUrls.Where(url => !url.StartsWith("https://", StringComparison.OrdinalIgnoreCase))];

        logger.LogInformation(
            "disableLetsEncrypt: {disableLetsEncrypt}", disableLetsEncrypt?.ToString() ?? "null");

        logger.LogInformation(
            "enableUseFluffySpoonLetsEncrypt: {enableUseFluffySpoonLetsEncrypt}", enableUseFluffySpoonLetsEncrypt);

        logger.LogInformation(
            "canUseHttps: {canUseHttps}", canUseHttps);

        logger.LogInformation(
            "disableHttps: {disableHttps}", disableHttps);

        var webHostBuilder =
            appBuilder.WebHost
            .ConfigureKestrel(kestrelOptions =>
            {
            })
            .ConfigureServices(services =>
            {
                ConfigureServices(services, logger);
            })
            .UseUrls([.. publicWebHostUrlsFilteredForHttps])
            .WithSettingDateTimeOffsetDelegate(getDateTimeOffset);

        var app = appBuilder.Build();

        if (env.IsDevelopment())
        {
            app.UseDeveloperExceptionPage();
        }

        if (enableUseFluffySpoonLetsEncrypt)
        {
            app.UseFluffySpoonLetsEncrypt();
        }

        app.UseResponseCompression();

        app.Lifetime.ApplicationStopping.Register(() =>
        {
            applicationStoppingCancellationTokenSource.Cancel();
            app.Logger?.LogInformation("Public app noticed ApplicationStopping.");
        });

        app.Run(async context =>
        {
            await Asp.MiddlewareFromWebServiceConfig(
                serverAndElmAppConfig.ServerConfig,
                context,
                () => HandleRequestAsync(context));
        });

        return app;
    }

    private void ConfigureServices(
        IServiceCollection services,
        ILogger logger)
    {
        services.AddResponseCompression(options =>
        {
            options.EnableForHttps = true;
        });

        var letsEncryptOptions = serverAndElmAppConfig.ServerConfig?.letsEncryptOptions;

        if (letsEncryptOptions is null)
        {
            logger.LogInformation("I did not find 'letsEncryptOptions' in the configuration. I continue without Let's Encrypt.");
        }
        else
        {
            if (serverAndElmAppConfig.DisableLetsEncrypt ?? false)
            {
                logger.LogInformation(
                    "I found 'letsEncryptOptions' in the configuration, but 'disableLetsEncrypt' is set to true. I continue without Let's Encrypt.");
            }
            else
            {
                logger.LogInformation(
                    "I found 'letsEncryptOptions' in the configuration: {letsEncryptOptions}",
                    System.Text.Json.JsonSerializer.Serialize(letsEncryptOptions));
                services.AddFluffySpoonLetsEncrypt(letsEncryptOptions);
                services.AddFluffySpoonLetsEncryptFileCertificatePersistence();
                services.AddFluffySpoonLetsEncryptMemoryChallengePersistence();
            }
        }

        Asp.ConfigureServices(services);
    }

    private async System.Threading.Tasks.Task HandleRequestAsync(HttpContext context)
    {
        var currentDateTime = getDateTimeOffset();
        var timeMilli = currentDateTime.ToUnixTimeMilliseconds();
        var httpRequestIndex = System.Threading.Interlocked.Increment(ref nextHttpRequestIndex);

        var httpRequestId = timeMilli + "-" + httpRequestIndex;

        var httpRequest =
            await Asp.AsInterfaceHttpRequestAsync(context.Request);

        if (serverAndElmAppConfig.ServerConfig?.httpRequestEventSizeLimit is { } httpRequestEventSizeLimit)
        {
            if (httpRequestEventSizeLimit < EstimateHttpRequestEventSize(httpRequest))
            {
                context.Response.StatusCode = StatusCodes.Status413RequestEntityTooLarge;
                await context.Response.WriteAsync("Request is too large.");
                return;
            }
        }

        var httpRequestEvent =
            new WebServiceInterface.HttpRequestEventStruct(
                HttpRequestId: httpRequestId.ToString(),
                PosixTimeMilli: timeMilli,
                new WebServiceInterface.HttpRequestContext(
                    ClientAddress: context.Connection.RemoteIpAddress?.ToString()),
                Request: httpRequest);

        var task =
            serverAndElmAppConfig.ProcessHttpRequestAsync(httpRequestEvent);

        var waitForHttpResponseClock = System.Diagnostics.Stopwatch.StartNew();

        WebServiceInterface.HttpResponse? GetResponseFromAppOrTimeout(TimeSpan timeout)
        {
            if (task.IsCompleted)
            {
                return task.Result;
            }

            if (timeout <= waitForHttpResponseClock.Elapsed)
            {
                return
                    new WebServiceInterface.HttpResponse(
                        StatusCode: 500,
                        Body: System.Text.Encoding.UTF8.GetBytes(
                            "The app did not return an HTTP response within " +
                            (int)waitForHttpResponseClock.Elapsed.TotalSeconds +
                            " seconds."),
                        HeadersToAdd: []);
            }

            return null;
        }

        while (true)
        {
            var httpResponse =
                GetResponseFromAppOrTimeout(TimeSpan.FromSeconds(60));

            if (httpResponse is null)
            {
                await System.Threading.Tasks.Task.Delay(TimeSpan.FromMilliseconds(30));
                continue;
            }

            var headerContentType =
                httpResponse.HeadersToAdd
                ?.FirstOrDefault(header => header.Name?.ToLowerInvariant() == "content-type")
                ?.Values?.FirstOrDefault();

            context.Response.StatusCode = httpResponse.StatusCode;

            foreach (var headerToAdd in httpResponse.HeadersToAdd.EmptyIfNull())
            {
                context.Response.Headers[headerToAdd.Name] =
                    new Microsoft.Extensions.Primitives.StringValues([.. headerToAdd.Values]);
            }

            if (headerContentType != null)
                context.Response.ContentType = headerContentType;

            context.Response.Headers.XPoweredBy = "Pine";

            ReadOnlyMemory<byte>? contentAsByteArray = httpResponse.Body;

            context.Response.ContentLength = contentAsByteArray?.Length ?? 0;

            if (contentAsByteArray != null)
                await context.Response.Body.WriteAsync(contentAsByteArray.Value);

            break;
        }
    }

    public static long EstimateHttpRequestEventSize(
        WebServiceInterface.HttpRequestProperties httpRequest)
    {
        var headersSize =
            httpRequest.Headers
            .Select(header => header.Name.Length + header.Values.Sum(value => value.Length))
            .Sum();

        var bodySize = httpRequest.Body?.Length ?? 0;

        return
            httpRequest.Method.Length +
            httpRequest.Uri.Length +
            headersSize +
            bodySize;
    }
}

public record ServerAndElmAppConfig(
    WebServiceConfigJson? ServerConfig,
    Func<WebServiceInterface.HttpRequestEventStruct, System.Threading.Tasks.Task<WebServiceInterface.HttpResponse>> ProcessHttpRequestAsync,
    PineValue SourceComposition,
    InterfaceToHost.BackendEventResponseStruct? InitOrMigrateCmds,
    bool? DisableLetsEncrypt,
    bool DisableHttps);
