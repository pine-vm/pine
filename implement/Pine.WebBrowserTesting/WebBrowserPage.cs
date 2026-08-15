using Microsoft.Playwright;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.WebBrowserTesting;

/// <summary>
/// Owns one non-persistent browser context and its initial page.
/// </summary>
public sealed class WebBrowserPage : IAsyncDisposable
{
    private readonly IBrowserContext _context;

    private readonly IPage _page;

    private readonly TimeSpan _operationTimeout;

    private readonly ConcurrentDictionary<string, VirtualResponse> _responses =
        new(StringComparer.Ordinal);

    private bool _traceStopped;

    private bool _aborted;

    private bool _disposed;

    private WebBrowserPage(IBrowserContext context, IPage page, TimeSpan operationTimeout)
    {
        _context = context;
        _page = page;
        _operationTimeout = operationTimeout;
    }

    /// <summary>
    /// Exposes the underlying Playwright context for advanced capabilities.
    /// </summary>
    public IBrowserContext AdvancedContext => _context;

    /// <summary>
    /// Exposes the underlying Playwright page for advanced capabilities.
    /// </summary>
    public IPage AdvancedPage => _page;

    internal static async Task<WebBrowserPage> CreateAsync(
        IBrowserContext context,
        IPage page,
        TimeSpan operationTimeout,
        CancellationToken cancellationToken)
    {
        var browserPage = new WebBrowserPage(context, page, operationTimeout);

        await page.RouteAsync("**/*", browserPage.RouteRequestAsync)
        .WaitForPlaywrightAsync(
            operationTimeout,
            cancellationToken,
            browserPage.AbortOperationsAsync)
        .ConfigureAwait(false);

        return browserPage;
    }

    public async Task LoadHtmlAsync(
        ReadOnlyMemory<byte> htmlDocument,
        HtmlDocumentOptions? options = null,
        CancellationToken cancellationToken = default)
    {
        ThrowIfDisposed();

        options ??= new HtmlDocumentOptions();
        Validate(options);

        var headers =
            options.Headers is null
            ?
            new Dictionary<string, string>()
            :
            new Dictionary<string, string>(options.Headers, StringComparer.OrdinalIgnoreCase);

        var responseUrl =
            new UriBuilder(options.Url)
            {
                Fragment = string.Empty,
            }
            .Uri
            .AbsoluteUri;

        _responses[responseUrl] =
            new VirtualResponse(
                htmlDocument.ToArray(),
                options.ContentType,
                options.StatusCode,
                headers);

        await _page.GotoAsync(
            options.Url.AbsoluteUri,
            new PageGotoOptions
            {
                WaitUntil = WaitUntilState.Load,
                Timeout = (float)_operationTimeout.TotalMilliseconds,
            })
        .WaitForPlaywrightAsync(
            _operationTimeout,
            cancellationToken,
            AbortOperationsAsync)
        .ConfigureAwait(false);
    }

    /// <summary>
    /// Waits until a caller-provided JavaScript expression becomes truthy.
    /// </summary>
    public async Task WaitForReadyAsync(
        string expression,
        object? argument = null,
        CancellationToken cancellationToken = default)
    {
        ThrowIfDisposed();
        ArgumentException.ThrowIfNullOrWhiteSpace(expression);

        await _page.WaitForFunctionAsync(
            expression,
            argument,
            new PageWaitForFunctionOptions
            {
                Timeout = (float)_operationTimeout.TotalMilliseconds,
            })
        .WaitForPlaywrightAsync(
            _operationTimeout,
            cancellationToken,
            AbortOperationsAsync)
        .ConfigureAwait(false);
    }

    public WebBrowserLocator GetByCss(string selector)
    {
        ThrowIfDisposed();
        ArgumentException.ThrowIfNullOrWhiteSpace(selector);
        return new WebBrowserLocator(_page.Locator(selector), _operationTimeout, AbortOperationsAsync);
    }

    public WebBrowserLocator GetByRole(string role, string? accessibleName = null, bool exact = false)
    {
        ThrowIfDisposed();
        ArgumentException.ThrowIfNullOrWhiteSpace(role);

        if (!Enum.TryParse<AriaRole>(role.Replace("-", string.Empty), ignoreCase: true, out var ariaRole))
            throw new ArgumentException("Unknown ARIA role: " + role, nameof(role));

        return
            new WebBrowserLocator(
                _page.GetByRole(
                    ariaRole,
                    new PageGetByRoleOptions
                    {
                        Name = accessibleName,
                        Exact = exact,
                    }),
                _operationTimeout,
                AbortOperationsAsync);
    }

    public WebBrowserLocator GetByText(string text, bool exact = false)
    {
        ThrowIfDisposed();
        ArgumentNullException.ThrowIfNull(text);

        return
            new WebBrowserLocator(
                _page.GetByText(text, new PageGetByTextOptions { Exact = exact }),
                _operationTimeout,
                AbortOperationsAsync);
    }

    public WebBrowserLocator GetByLabel(string label, bool exact = false)
    {
        ThrowIfDisposed();
        ArgumentNullException.ThrowIfNull(label);

        return
            new WebBrowserLocator(
                _page.GetByLabel(label, new PageGetByLabelOptions { Exact = exact }),
                _operationTimeout,
                AbortOperationsAsync);
    }

    public WebBrowserLocator GetByPlaceholder(string placeholder, bool exact = false)
    {
        ThrowIfDisposed();
        ArgumentNullException.ThrowIfNull(placeholder);

        return
            new WebBrowserLocator(
                _page.GetByPlaceholder(
                    placeholder,
                    new PageGetByPlaceholderOptions { Exact = exact }),
                _operationTimeout,
                AbortOperationsAsync);
    }

    public WebBrowserLocator GetByAltText(string alternativeText, bool exact = false)
    {
        ThrowIfDisposed();
        ArgumentNullException.ThrowIfNull(alternativeText);

        return
            new WebBrowserLocator(
                _page.GetByAltText(
                    alternativeText,
                    new PageGetByAltTextOptions { Exact = exact }),
                _operationTimeout,
                AbortOperationsAsync);
    }

    public WebBrowserLocator GetByTitle(string title, bool exact = false)
    {
        ThrowIfDisposed();
        ArgumentNullException.ThrowIfNull(title);

        return
            new WebBrowserLocator(
                _page.GetByTitle(title, new PageGetByTitleOptions { Exact = exact }),
                _operationTimeout,
                AbortOperationsAsync);
    }

    public WebBrowserLocator GetByTestId(string testId)
    {
        ThrowIfDisposed();
        ArgumentNullException.ThrowIfNull(testId);
        return new WebBrowserLocator(_page.GetByTestId(testId), _operationTimeout, AbortOperationsAsync);
    }

    public async Task<string> GetDomSnapshotAsync(CancellationToken cancellationToken = default)
    {
        ThrowIfDisposed();

        return
            await _page.ContentAsync()
            .WaitForPlaywrightAsync(
                _operationTimeout,
                cancellationToken,
                AbortOperationsAsync)
            .ConfigureAwait(false);
    }

    public async Task<ReadOnlyMemory<byte>> TakeScreenshotAsync(
        WebBrowserScreenshotOptions? options = null,
        CancellationToken cancellationToken = default)
    {
        ThrowIfDisposed();
        options ??= new WebBrowserScreenshotOptions();

        return
            await _page.ScreenshotAsync(
                new PageScreenshotOptions
                {
                    FullPage = options.FullPage,
                    OmitBackground = options.OmitBackground,
                    Type = ScreenshotType.Png,
                    Timeout = (float)_operationTimeout.TotalMilliseconds,
                })
            .WaitForPlaywrightAsync(
                _operationTimeout,
                cancellationToken,
                AbortOperationsAsync)
            .ConfigureAwait(false);
    }

    public async Task<WebBrowserDiagnostics> GetDiagnosticsAsync(
        CancellationToken cancellationToken = default)
    {
        ThrowIfDisposed();

        var messages =
            await _page.ConsoleMessagesAsync()
            .WaitForPlaywrightAsync(
                _operationTimeout,
                cancellationToken,
                AbortOperationsAsync)
            .ConfigureAwait(false);

        var consoleMessages = new List<WebBrowserConsoleMessage>(messages.Count);

        foreach (var message in messages)
        {
            var arguments = new List<string>(message.Args.Count);

            foreach (var argument in message.Args)
            {
                try
                {
                    var value =
                        await argument.JsonValueAsync<JsonElement>()
                        .WaitForPlaywrightAsync(
                            _operationTimeout,
                            cancellationToken,
                            AbortOperationsAsync)
                        .ConfigureAwait(false);

                    arguments.Add(value.GetRawText());
                }
                catch (PlaywrightException)
                {
                    arguments.Add(argument.ToString() ?? string.Empty);
                }
            }

            consoleMessages.Add(
                new WebBrowserConsoleMessage(
                    message.Type,
                    message.Text,
                    arguments,
                    message.Location,
                    message.Timestamp));
        }

        var pageErrors =
            await _page.PageErrorsAsync()
            .WaitForPlaywrightAsync(
                _operationTimeout,
                cancellationToken,
                AbortOperationsAsync)
            .ConfigureAwait(false);

        return new WebBrowserDiagnostics(consoleMessages, pageErrors);
    }

    /// <summary>
    /// Captures diagnostics intended to be retained when a test fails.
    /// Calling this method stops tracing for this context.
    /// </summary>
    public async Task<WebBrowserFailureArtifacts> CaptureFailureArtifactsAsync(
        CancellationToken cancellationToken = default)
    {
        ThrowIfDisposed();

        var domSnapshot = await GetDomSnapshotAsync(cancellationToken).ConfigureAwait(false);
        var screenshot = await TakeScreenshotAsync(cancellationToken: cancellationToken).ConfigureAwait(false);
        var diagnostics = await GetDiagnosticsAsync(cancellationToken).ConfigureAwait(false);
        var trace = await StopTraceAsync(cancellationToken).ConfigureAwait(false);

        return new WebBrowserFailureArtifacts(domSnapshot, screenshot, trace, diagnostics);
    }

    public async ValueTask DisposeAsync()
    {
        if (_disposed)
            return;

        _disposed = true;

        try
        {
            if (!_traceStopped && !_aborted)
            {
                try
                {
                    await _context.Tracing.StopAsync().ConfigureAwait(false);
                }
                catch (PlaywrightException)
                {
                }
            }
        }
        finally
        {
            try
            {
                await _context.CloseAsync().ConfigureAwait(false);
            }
            catch (PlaywrightException)
            {
            }
        }
    }

    private async Task RouteRequestAsync(IRoute route)
    {
        if (_responses.TryGetValue(route.Request.Url, out var response))
        {
            await route.FulfillAsync(
                new RouteFulfillOptions
                {
                    BodyBytes = response.Body,
                    ContentType = response.ContentType,
                    Status = response.StatusCode,
                    Headers = response.Headers,
                })
            .ConfigureAwait(false);

            return;
        }

        await route.AbortAsync("blockedbyclient").ConfigureAwait(false);
    }

    private async Task<ReadOnlyMemory<byte>> StopTraceAsync(CancellationToken cancellationToken)
    {
        if (_traceStopped)
            return ReadOnlyMemory<byte>.Empty;

        var tracePath =
            Path.Combine(
                Path.GetTempPath(),
                "pine-web-browser-trace-" + Guid.NewGuid().ToString("N") + ".zip");

        try
        {
            await _context.Tracing.StopAsync(new TracingStopOptions { Path = tracePath })
            .WaitForPlaywrightAsync(
                _operationTimeout,
                cancellationToken,
                AbortOperationsAsync)
            .ConfigureAwait(false);

            _traceStopped = true;
            return await File.ReadAllBytesAsync(tracePath, cancellationToken).ConfigureAwait(false);
        }
        finally
        {
            if (File.Exists(tracePath))
                File.Delete(tracePath);
        }
    }

    private static void Validate(HtmlDocumentOptions options)
    {
        if (options.Url.Scheme is not ("http" or "https"))
            throw new ArgumentException("The virtual document URL must use HTTP or HTTPS.", nameof(options));

        if (!options.Url.IsAbsoluteUri)
            throw new ArgumentException("The virtual document URL must be absolute.", nameof(options));

        ArgumentException.ThrowIfNullOrWhiteSpace(options.ContentType);

        if (options.StatusCode is < 100 or > 599)
            throw new ArgumentOutOfRangeException(nameof(options.StatusCode));
    }

    private void ThrowIfDisposed() => ObjectDisposedException.ThrowIf(_disposed, this);

    private async Task AbortOperationsAsync()
    {
        _aborted = true;
        await _context.CloseAsync().ConfigureAwait(false);
    }

    private sealed record VirtualResponse(
        byte[] Body,
        string ContentType,
        int StatusCode,
        IReadOnlyDictionary<string, string> Headers);
}
