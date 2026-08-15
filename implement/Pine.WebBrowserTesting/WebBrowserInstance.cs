using DotNet.Testcontainers.Builders;
using DotNet.Testcontainers.Containers;
using DotNet.Testcontainers.Networks;
using Microsoft.Playwright;
using System;
using System.IO;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.WebBrowserTesting;

/// <summary>
/// Owns one Chromium process that can create isolated browser test contexts.
/// </summary>
public sealed class WebBrowserInstance : IAsyncDisposable
{
    private const ushort PlaywrightPort = 8080;

    private readonly IPlaywright _playwright;

    private readonly IContainer? _container;

    private readonly INetwork? _containerNetwork;

    private readonly TimeSpan _operationTimeout;

    private bool _disposed;

    private WebBrowserInstance(
        IPlaywright playwright,
        IBrowser browser,
        IContainer? container,
        INetwork? containerNetwork,
        TimeSpan operationTimeout)
    {
        _playwright = playwright;
        AdvancedBrowser = browser;
        _container = container;
        _containerNetwork = containerNetwork;
        _operationTimeout = operationTimeout;
    }

    /// <summary>
    /// Exposes the underlying Playwright browser for capabilities not yet represented by this façade.
    /// </summary>
    public IBrowser AdvancedBrowser { get; }

    public static async Task<WebBrowserInstance> StartAsync(
        WebBrowserExecutionMode executionMode,
        WebBrowserProcessOptions? options = null,
        CancellationToken cancellationToken = default)
    {
        options ??= new WebBrowserProcessOptions();
        Validate(options);

        IPlaywright? playwright = null;
        IContainer? container = null;
        INetwork? containerNetwork = null;
        IBrowser? browser = null;

        try
        {
            playwright =
                await Microsoft.Playwright.Playwright.CreateAsync()
                .WaitAsync(options.StartupTimeout, cancellationToken)
                .ConfigureAwait(false);

            switch (executionMode)
            {
                case WebBrowserExecutionMode.Container:
                    var localPlaywrightPackage =
                        new DirectoryInfo(
                            Path.Combine(
                                AppContext.BaseDirectory,
                                ".playwright",
                                "package"));

                    if (!File.Exists(Path.Combine(localPlaywrightPackage.FullName, "cli.js")))
                    {
                        throw new FileNotFoundException(
                            "Could not find the Playwright driver package copied to the application output.",
                            Path.Combine(localPlaywrightPackage.FullName, "cli.js"));
                    }

                    containerNetwork = new NetworkBuilder().Build();

                    using (var networkStartupCancellation =
                           CancellationTokenSource.CreateLinkedTokenSource(cancellationToken))
                    {
                        networkStartupCancellation.CancelAfter(options.StartupTimeout);

                        await containerNetwork.CreateAsync(networkStartupCancellation.Token)
                        .ConfigureAwait(false);
                    }

                    container =
                        new ContainerBuilder(options.ContainerImage)
                        .WithNetwork(containerNetwork)
                        .WithPortBinding(PlaywrightPort, assignRandomHostPort: true)
                        .WithCreateParameterModifier(
                            parameters =>
                            {
                                if (parameters.HostConfig?.PortBindings is null ||
                                    !parameters.HostConfig.PortBindings.TryGetValue(
                                        PlaywrightPort + "/tcp",
                                        out var bindings))
                                {
                                    throw new InvalidOperationException(
                                        "Could not configure the Playwright port binding.");
                                }

                                foreach (var binding in bindings)
                                    binding.HostIP = "127.0.0.1";

                                parameters.User = "pwuser";
                            })
                        .WithEntrypoint("/bin/sh", "-c")
                        .WithResourceMapping(localPlaywrightPackage, "/pine-playwright")
                        .WithCommand(
                            "node /pine-playwright/cli.js run-server --port " +
                            PlaywrightPort +
                            " --host 0.0.0.0")
                        .WithWaitStrategy(
                            Wait.ForUnixContainer()
                            .UntilMessageIsLogged(
                                "Listening on ws://0.0.0.0:" + PlaywrightPort + "/"))
                        .Build();

                    using (var startupCancellation =
                           CancellationTokenSource.CreateLinkedTokenSource(cancellationToken))
                    {
                        startupCancellation.CancelAfter(options.StartupTimeout);
                        await container.StartAsync(startupCancellation.Token).ConfigureAwait(false);
                    }

                    browser =
                        await playwright.Chromium.ConnectAsync(
                            new UriBuilder(
                                "ws",
                                container.Hostname,
                                container.GetMappedPublicPort(PlaywrightPort))
                            .Uri
                            .ToString(),
                            new BrowserTypeConnectOptions
                            {
                                Timeout = (float)options.StartupTimeout.TotalMilliseconds,
                            })
                        .WaitAsync(options.StartupTimeout, cancellationToken)
                        .ConfigureAwait(false);

                    break;

                case WebBrowserExecutionMode.Host:
                    browser =
                        await playwright.Chromium.LaunchAsync(
                            new BrowserTypeLaunchOptions
                            {
                                Channel =
                                string.Equals(
                                    Environment.GetEnvironmentVariable("GITHUB_ACTIONS"),
                                    "true",
                                    StringComparison.OrdinalIgnoreCase)
                                ?
                                "chrome"
                                :
                                null,
                                Headless = options.Headless,
                                Timeout = (float)options.StartupTimeout.TotalMilliseconds,
                            })
                        .WaitAsync(options.StartupTimeout, cancellationToken)
                        .ConfigureAwait(false);

                    break;

                default:
                    throw new ArgumentOutOfRangeException(
                        nameof(executionMode),
                        executionMode,
                        "Unknown browser execution mode.");
            }

            return
                new WebBrowserInstance(
                    playwright,
                    browser,
                    container,
                    containerNetwork,
                    options.OperationTimeout);
        }
        catch (Exception exception)
        {
            var containerLogs = await GetContainerLogsAsync(container).ConfigureAwait(false);

            if (browser is not null)
                await TryCloseBrowserAsync(browser).ConfigureAwait(false);

            playwright?.Dispose();

            if (container is not null)
                await container.DisposeAsync().ConfigureAwait(false);

            if (containerNetwork is not null)
                await containerNetwork.DisposeAsync().ConfigureAwait(false);

            if (container is not null)
            {
                throw new WebBrowserStartupException(
                    "Failed to start or connect to Chromium container." +
                    Environment.NewLine +
                    "Container logs:" +
                    Environment.NewLine +
                    containerLogs,
                    containerLogs,
                    exception);
            }

            throw;
        }
    }

    public async Task<WebBrowserPage> CreatePageAsync(
        WebBrowserContextOptions? options = null,
        CancellationToken cancellationToken = default)
    {
        ObjectDisposedException.ThrowIf(_disposed, this);

        options ??= new WebBrowserContextOptions();
        Validate(options);

        var context =
            await AdvancedBrowser.NewContextAsync(ToPlaywrightOptions(options))
            .WaitForPlaywrightAsync(
                _operationTimeout,
                cancellationToken,
                () => TryCloseBrowserAsync(AdvancedBrowser))
            .ConfigureAwait(false);

        try
        {
            context.SetDefaultTimeout((float)_operationTimeout.TotalMilliseconds);
            context.SetDefaultNavigationTimeout((float)_operationTimeout.TotalMilliseconds);

            await context.Tracing.StartAsync(
                new TracingStartOptions
                {
                    Screenshots = true,
                    Snapshots = true,
                    Sources = true,
                })
            .WaitForPlaywrightAsync(
                _operationTimeout,
                cancellationToken,
                () => TryCloseContextAsync(context))
            .ConfigureAwait(false);

            var page =
                await context.NewPageAsync()
                .WaitForPlaywrightAsync(
                    _operationTimeout,
                    cancellationToken,
                    () => TryCloseContextAsync(context))
                .ConfigureAwait(false);

            return
                await WebBrowserPage.CreateAsync(context, page, _operationTimeout, cancellationToken)
                .ConfigureAwait(false);
        }
        catch
        {
            await TryCloseContextAsync(context).ConfigureAwait(false);
            throw;
        }
    }

    public async ValueTask DisposeAsync()
    {
        if (_disposed)
            return;

        _disposed = true;

        try
        {
            await TryCloseBrowserAsync(AdvancedBrowser).ConfigureAwait(false);
        }
        finally
        {
            _playwright.Dispose();

            if (_container is not null)
                await _container.DisposeAsync().ConfigureAwait(false);

            if (_containerNetwork is not null)
                await _containerNetwork.DisposeAsync().ConfigureAwait(false);
        }
    }

    private static BrowserNewContextOptions ToPlaywrightOptions(WebBrowserContextOptions options) =>
        new()
        {
            ViewportSize =
            new ViewportSize
            {
                Width = options.ViewportWidth,
                Height = options.ViewportHeight,
            },
            ScreenSize =
            options.ScreenWidth is not null && options.ScreenHeight is not null
            ?
            new ScreenSize
            {
                Width = options.ScreenWidth.Value,
                Height = options.ScreenHeight.Value,
            }
            :
            null,
            DeviceScaleFactor = options.DeviceScaleFactor,
            IsMobile = options.IsMobile,
            HasTouch = options.HasTouch,
            JavaScriptEnabled = options.JavaScriptEnabled,
            Locale = options.Locale,
            TimezoneId = options.TimezoneId,
            UserAgent = options.UserAgent,
            ColorScheme =
            options.ColorScheme switch
            {
                WebBrowserColorScheme.NoPreference => Microsoft.Playwright.ColorScheme.NoPreference,
                WebBrowserColorScheme.Light => Microsoft.Playwright.ColorScheme.Light,
                WebBrowserColorScheme.Dark => Microsoft.Playwright.ColorScheme.Dark,

                _ =>
                throw new ArgumentOutOfRangeException(
                    nameof(options.ColorScheme),
                    options.ColorScheme,
                    "Unknown browser color scheme."),
            },
            ReducedMotion =
            options.ReducedMotion
            ?
            Microsoft.Playwright.ReducedMotion.Reduce
            :
            Microsoft.Playwright.ReducedMotion.NoPreference,
            ExtraHTTPHeaders = options.ExtraHttpHeaders,
        };

    private static async Task<string> GetContainerLogsAsync(IContainer? container)
    {
        if (container is null)
            return string.Empty;

        try
        {
            var (standardOutput, standardError) =
                await container.GetLogsAsync(
                    since: DateTime.MinValue,
                    until: DateTime.MaxValue,
                    timestampsEnabled: true,
                    ct: CancellationToken.None)
                .ConfigureAwait(false);

            return standardOutput + standardError;
        }
        catch (Exception logException)
        {
            return "Could not retrieve container logs: " + logException.Message;
        }
    }

    private static async Task TryCloseBrowserAsync(IBrowser browser)
    {
        try
        {
            await browser.CloseAsync().ConfigureAwait(false);
        }
        catch
        {
        }
    }

    private static async Task TryCloseContextAsync(IBrowserContext context)
    {
        try
        {
            await context.CloseAsync().ConfigureAwait(false);
        }
        catch
        {
        }
    }

    private static void Validate(WebBrowserProcessOptions options)
    {
        ArgumentException.ThrowIfNullOrWhiteSpace(options.ContainerImage);

        if (options.StartupTimeout <= TimeSpan.Zero)
            throw new ArgumentOutOfRangeException(nameof(options.StartupTimeout));

        if (options.OperationTimeout <= TimeSpan.Zero)
            throw new ArgumentOutOfRangeException(nameof(options.OperationTimeout));
    }

    private static void Validate(WebBrowserContextOptions options)
    {
        if (options.ViewportWidth <= 0)
            throw new ArgumentOutOfRangeException(nameof(options.ViewportWidth));

        if (options.ViewportHeight <= 0)
            throw new ArgumentOutOfRangeException(nameof(options.ViewportHeight));

        if ((options.ScreenWidth is null) != (options.ScreenHeight is null))
            throw new ArgumentException("Screen width and height must either both be set or both be omitted.");

        if (options.ScreenWidth is <= 0)
            throw new ArgumentOutOfRangeException(nameof(options.ScreenWidth));

        if (options.ScreenHeight is <= 0)
            throw new ArgumentOutOfRangeException(nameof(options.ScreenHeight));

        if (options.DeviceScaleFactor <= 0)
            throw new ArgumentOutOfRangeException(nameof(options.DeviceScaleFactor));
    }
}
