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
    public readonly System.Threading.CancellationTokenSource ApplicationStoppingCancellationTokenSource = new();

    private readonly ServerAndElmAppConfig _serverAndElmAppConfig = serverAndElmAppConfig;
    private readonly HttpRequestHandler _httpRequestHandler = HttpRequestHandler.FromServerAndElmAppConfig(serverAndElmAppConfig, getDateTimeOffset);

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
            _serverAndElmAppConfig.ServerConfig?.letsEncryptOptions is not null && !(disableLetsEncrypt ?? false);

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
                _serverAndElmAppConfig.ServerConfig,
                context,
                () => _httpRequestHandler.HandleRequestAsync(context));
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

        var letsEncryptOptions = _serverAndElmAppConfig.ServerConfig?.letsEncryptOptions;

        if (letsEncryptOptions is null)
        {
            logger.LogInformation("I did not find 'letsEncryptOptions' in the configuration. I continue without Let's Encrypt.");
        }
        else
        {
            if (_serverAndElmAppConfig.DisableLetsEncrypt ?? false)
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
}

public record ServerAndElmAppConfig(
    WebServiceConfigJson? ServerConfig,
    Func<WebServiceInterface.HttpRequestEventStruct, System.Threading.Tasks.Task<WebServiceInterface.HttpResponse>> ProcessHttpRequestAsync,
    PineValue SourceComposition,
    InterfaceToHost.BackendEventResponseStruct? InitOrMigrateCmds,
    bool? DisableLetsEncrypt,
    bool DisableHttps);
