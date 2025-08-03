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
using System.Threading.Tasks;

namespace ElmTime.Platform.WebService;

public class PublicAppState(
    ServerAndElmAppConfig serverAndElmAppConfig,
    Func<DateTimeOffset> getDateTimeOffset)
{
    private long _nextHttpRequestIndex = 0;

    public readonly System.Threading.CancellationTokenSource ApplicationStoppingCancellationTokenSource = new();

    public WebApplication Build(
        WebApplicationBuilder appBuilder,
        ILogger logger,
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
                ConfigureServices(serverAndElmAppConfig, services, logger);
            })
            .UseUrls([.. publicWebHostUrlsFilteredForHttps])
            .WithSettingDateTimeOffsetDelegate(getDateTimeOffset);

        var app = appBuilder.Build();

        if (enableUseFluffySpoonLetsEncrypt)
        {
            app.UseFluffySpoonLetsEncrypt();
        }

        app.UseResponseCompression();

        app.Lifetime.ApplicationStopping.Register(() =>
        {
            ApplicationStoppingCancellationTokenSource.Cancel();
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

    private static void ConfigureServices(
        ServerAndElmAppConfig serverAndElmAppConfig,
        IServiceCollection services,
        ILogger logger)
    {
        services.AddResponseCompression(options =>
        {
            options.EnableForHttps = true;
        });

        if (serverAndElmAppConfig.ServerConfig?.letsEncryptOptions is not { } letsEncryptOptions)
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

    /// <summary>
    /// Handles an HTTP request from an ASP.NET Core HttpContext.
    /// </summary>
    public async Task HandleRequestAsync(HttpContext context)
    {
        var httpRequest =
            await Asp.AsInterfaceHttpRequestAsync(context.Request);

        var httpResponse =
            await HandleRequestAsync(
                httpRequest,
                new WebServiceInterface.HttpRequestContext(
                    ClientAddress: context.Connection.RemoteIpAddress?.ToString()),
                httpRequestEventSizeLimit: serverAndElmAppConfig.ServerConfig?.httpRequestEventSizeLimit,
                ApplicationStoppingCancellationTokenSource.Token);

        await Asp.SendHttpResponseAsync(context, httpResponse);
    }

    /// <summary>
    /// Handles an HTTP request.
    /// </summary>
    public async Task<WebServiceInterface.HttpResponse> HandleRequestAsync(
        WebServiceInterface.HttpRequestProperties httpRequest,
        WebServiceInterface.HttpRequestContext httpRequestContext,
        int? httpRequestEventSizeLimit,
        System.Threading.CancellationToken cancellationToken)
    {
        if (httpRequestEventSizeLimit is { } limit &&
            EstimateHttpRequestEventSize(httpRequest) > limit)
        {
            return
                new WebServiceInterface.HttpResponse(
                    StatusCode: StatusCodes.Status413RequestEntityTooLarge,
                    Body: System.Text.Encoding.UTF8.GetBytes("Request is too large."),
                    HeadersToAdd: []);
        }

        var currentDateTime = getDateTimeOffset();
        var timeMilli = currentDateTime.ToUnixTimeMilliseconds();
        var httpRequestIndex = System.Threading.Interlocked.Increment(ref _nextHttpRequestIndex);

        var httpRequestId = timeMilli + "-" + httpRequestIndex;

        var httpRequestEvent =
            new WebServiceInterface.HttpRequestEventStruct(
                HttpRequestId: httpRequestId.ToString(),
                PosixTimeMilli: timeMilli,
                httpRequestContext,
                Request: httpRequest);

        var task = serverAndElmAppConfig.ProcessHttpRequestAsync(httpRequestEvent);

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
                GetResponseFromAppOrTimeout(TimeSpan.FromMinutes(4));

            if (httpResponse is not null)
                return httpResponse;

            cancellationToken.ThrowIfCancellationRequested();

            await Task.Delay(TimeSpan.FromMilliseconds(30), cancellationToken);
            continue;
        }
    }

    /// <summary>
    /// Estimates the size of an HTTP request event for size limit checks.
    /// </summary>
    public static long EstimateHttpRequestEventSize(
        WebServiceInterface.HttpRequestProperties httpRequest)
    {
        var headersSize = 0;

        foreach (var header in httpRequest.Headers)
        {
            headersSize += header.Name.Length + 10;

            foreach (var value in header.Values)
            {
                headersSize += value.Length + 10;
            }
        }

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
    Func<WebServiceInterface.HttpRequestEventStruct, Task<WebServiceInterface.HttpResponse>> ProcessHttpRequestAsync,
    PineValue SourceComposition,
    InterfaceToHost.BackendEventResponseStruct? InitOrMigrateCmds,
    bool? DisableLetsEncrypt,
    bool DisableHttps);
