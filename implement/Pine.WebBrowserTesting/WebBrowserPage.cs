using Microsoft.Playwright;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics;
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

    private readonly ConcurrentQueue<WebBrowserRequestFailure> _requestFailures = new();

    private bool _traceStopped;

    private bool _aborted;

    private bool _pageCrashed;

    private bool _disposed;

    private WebBrowserPage(IBrowserContext context, IPage page, TimeSpan operationTimeout)
    {
        _context = context;
        _page = page;
        _operationTimeout = operationTimeout;

        page.Crash += (_, _) => _pageCrashed = true;

        page.RequestFailed +=
            (_, request) =>
            _requestFailures.Enqueue(
                new WebBrowserRequestFailure(
                    request.Url,
                    request.Method,
                    request.ResourceType,
                    request.Failure));
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

        try
        {
            await WaitForFunctionAsync(
                expression,
                argument,
                _operationTimeout,
                cancellationToken)
            .ConfigureAwait(false);
        }
        catch (Exception exception) when (
            exception is not OperationCanceledException &&
            exception is not WebBrowserOperationException)
        {
            throw
                new WebBrowserOperationException(
                    "Waiting for the page readiness expression",
                    await GetDiagnosticsAsync(CancellationToken.None).ConfigureAwait(false),
                    exception);
        }
    }

    /// <summary>
    /// Waits for caller-selected resources and presentation frames before a visual operation.
    /// </summary>
    public async Task WaitForRenderReadyAsync(
        WebBrowserRenderWaitOptions? options = null,
        CancellationToken cancellationToken = default)
    {
        ThrowIfDisposed();
        options ??= new WebBrowserRenderWaitOptions();
        Validate(options);

        var timeout = options.Timeout ?? _operationTimeout;
        var stopwatch = Stopwatch.StartNew();

        try
        {
            if (options.ReadyExpression is not null)
            {
                ArgumentException.ThrowIfNullOrWhiteSpace(options.ReadyExpression);

                await WaitForFunctionAsync(
                    options.ReadyExpression,
                    options.ReadyExpressionArgument,
                    RemainingTimeout(timeout, stopwatch),
                    cancellationToken)
                .ConfigureAwait(false);
            }

            if (options.WaitForImages)
            {
                await WaitForFunctionAsync(
                    "() => Array.from(document.images).every(image => image.complete)",
                    argument: null,
                    RemainingTimeout(timeout, stopwatch),
                    cancellationToken)
                .ConfigureAwait(false);

                await _page.EvaluateAsync(
                    """
                    async ({ failOnImageError }) => {
                        const failures = [];

                        await Promise.all(Array.from(document.images).map(async image => {
                            if (image.naturalWidth <= 0 || image.naturalHeight <= 0) {
                                failures.push(image.currentSrc || image.src || "<missing source>");
                                return;
                            }

                            if (typeof image.decode === "function") {
                                try {
                                    await image.decode();
                                }
                                catch {
                                    failures.push(image.currentSrc || image.src || "<missing source>");
                                }
                            }
                        }));

                        if (failOnImageError && failures.length > 0) {
                            throw new Error(
                                "Images failed to load or decode: " +
                                failures.map(source => "'" + source + "'").join(", "));
                        }
                    }
                    """,
                    new { failOnImageError = options.FailOnImageError })
                .WaitForPlaywrightAsync(
                    RemainingTimeout(timeout, stopwatch),
                    cancellationToken,
                    AbortOperationsAsync)
                .ConfigureAwait(false);
            }

            if (options.WaitForFonts)
            {
                await _page.EvaluateAsync(
                    """
                    async () => {
                        if (document.fonts)
                            await document.fonts.ready;
                    }
                    """)
                .WaitForPlaywrightAsync(
                    RemainingTimeout(timeout, stopwatch),
                    cancellationToken,
                    AbortOperationsAsync)
                .ConfigureAwait(false);
            }

            if (options.AnimationFrameCount > 0)
            {
                await _page.EvaluateAsync(
                    """
                    async frameCount => {
                        for (let frame = 0; frame < frameCount; ++frame)
                            await new Promise(resolve => requestAnimationFrame(resolve));
                    }
                    """,
                    options.AnimationFrameCount)
                .WaitForPlaywrightAsync(
                    RemainingTimeout(timeout, stopwatch),
                    cancellationToken,
                    AbortOperationsAsync)
                .ConfigureAwait(false);
            }
        }
        catch (Exception exception) when (
            exception is not OperationCanceledException &&
            exception is not WebBrowserOperationException)
        {
            throw
                new WebBrowserOperationException(
                    "Waiting for renderable page content",
                    await GetDiagnosticsAsync(CancellationToken.None).ConfigureAwait(false),
                    exception);
        }
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
        Validate(options);

        if (options.WaitForRender is not null)
        {
            await WaitForRenderReadyAsync(options.WaitForRender, cancellationToken)
            .ConfigureAwait(false);
        }

        try
        {
            return await TakeScreenshotCoreAsync(options, cancellationToken).ConfigureAwait(false);
        }
        catch (Exception exception) when (
            exception is not OperationCanceledException &&
            exception is not WebBrowserOperationException)
        {
            throw
                new WebBrowserOperationException(
                    "Taking a page screenshot",
                    await GetDiagnosticsAsync(CancellationToken.None).ConfigureAwait(false),
                    exception);
        }
    }

    public async Task<WebBrowserDiagnostics> GetDiagnosticsAsync(
        CancellationToken cancellationToken = default)
    {
        ThrowIfDisposed();

        var collectionErrors = new List<string>();
        var consoleMessages = new List<WebBrowserConsoleMessage>();
        var pageErrors = new List<string>();
        WebBrowserDocumentDiagnostics? document = null;

        try
        {
            var messages =
                await _page.ConsoleMessagesAsync()
                .WaitForPlaywrightAsync(
                    _operationTimeout,
                    cancellationToken,
                    AbortOperationsAsync)
                .ConfigureAwait(false);

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
                    catch (Exception exception) when (
                        exception is not OperationCanceledException ||
                        !cancellationToken.IsCancellationRequested)
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
        }
        catch (Exception exception) when (
            exception is not OperationCanceledException ||
            !cancellationToken.IsCancellationRequested)
        {
            collectionErrors.Add("Console messages: " + exception.Message);
        }

        try
        {
            pageErrors.AddRange(
                await _page.PageErrorsAsync()
                .WaitForPlaywrightAsync(
                    _operationTimeout,
                    cancellationToken,
                    AbortOperationsAsync)
                .ConfigureAwait(false));
        }
        catch (Exception exception) when (
            exception is not OperationCanceledException ||
            !cancellationToken.IsCancellationRequested)
        {
            collectionErrors.Add("Page errors: " + exception.Message);
        }

        try
        {
            var documentJson =
                await _page.EvaluateAsync<JsonElement>(
                    """
                    () => ({
                        Url: location.href,
                        Title: document.title,
                        VisibilityState: document.visibilityState,
                        ReadyState: document.readyState,
                        Images: Array.from(document.images).map(image => ({
                            Source: image.src,
                            CurrentSource: image.currentSrc,
                            Complete: image.complete,
                            NaturalWidth: image.naturalWidth,
                            NaturalHeight: image.naturalHeight
                        }))
                    })
                    """)
                .WaitForPlaywrightAsync(
                    _operationTimeout,
                    cancellationToken,
                    AbortOperationsAsync)
                .ConfigureAwait(false);

            document = documentJson.Deserialize<WebBrowserDocumentDiagnostics>();
        }
        catch (Exception exception) when (
            exception is not OperationCanceledException ||
            !cancellationToken.IsCancellationRequested)
        {
            collectionErrors.Add("Document state: " + exception.Message);
        }

        return
            new WebBrowserDiagnostics(
                consoleMessages,
                pageErrors,
                _requestFailures.ToArray(),
                document,
                _context.Browser?.Version ?? "Unavailable",
                _context.Browser?.IsConnected ?? false,
                _context.IsClosed,
                _page.IsClosed,
                _pageCrashed,
                collectionErrors);
    }

    /// <summary>
    /// Captures diagnostics intended to be retained when a test fails.
    /// Calling this method stops tracing for this context.
    /// </summary>
    public async Task<WebBrowserFailureArtifacts> CaptureFailureArtifactsAsync(
        WebBrowserFailureArtifactOptions? options = null,
        CancellationToken cancellationToken = default)
    {
        ThrowIfDisposed();
        options ??= new WebBrowserFailureArtifactOptions();

        var captureErrors = new List<string>();
        var domSnapshot = string.Empty;
        var screenshot = ReadOnlyMemory<byte>.Empty;
        var trace = ReadOnlyMemory<byte>.Empty;

        if (options.CaptureDomSnapshot)
        {
            try
            {
                domSnapshot = await GetDomSnapshotAsync(cancellationToken).ConfigureAwait(false);
            }
            catch (Exception exception) when (
                exception is not OperationCanceledException ||
                !cancellationToken.IsCancellationRequested)
            {
                captureErrors.Add("DOM snapshot: " + exception.Message);
            }
        }

        if (options.CaptureScreenshot)
        {
            try
            {
                screenshot =
                    await TakeScreenshotCoreAsync(
                        new WebBrowserScreenshotOptions { WaitForRender = null },
                        cancellationToken)
                    .ConfigureAwait(false);
            }
            catch (Exception exception) when (
                exception is not OperationCanceledException ||
                !cancellationToken.IsCancellationRequested)
            {
                captureErrors.Add("Screenshot: " + exception.Message);
            }
        }

        var diagnostics = await GetDiagnosticsAsync(cancellationToken).ConfigureAwait(false);

        if (options.CaptureTrace)
        {
            try
            {
                trace = await StopTraceAsync(cancellationToken).ConfigureAwait(false);
            }
            catch (Exception exception) when (
                exception is not OperationCanceledException ||
                !cancellationToken.IsCancellationRequested)
            {
                captureErrors.Add("Trace: " + exception.Message);
            }
        }

        return
            new WebBrowserFailureArtifacts(
                domSnapshot,
                screenshot,
                trace,
                diagnostics,
                captureErrors);
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

    private static void Validate(WebBrowserScreenshotOptions options)
    {
        if (options.Quality is < 0 or > 100)
            throw new ArgumentOutOfRangeException(nameof(options.Quality));

        if (options.ImageFormat is WebBrowserScreenshotImageFormat.Png &&
            options.Quality is not null)
        {
            throw new ArgumentException(
                "Screenshot quality can only be specified for JPEG encoding.",
                nameof(options));
        }
    }

    private static void Validate(WebBrowserRenderWaitOptions options)
    {
        if (options.AnimationFrameCount < 0)
            throw new ArgumentOutOfRangeException(nameof(options.AnimationFrameCount));

        if (options.Timeout <= TimeSpan.Zero)
            throw new ArgumentOutOfRangeException(nameof(options.Timeout));
    }

    private static TimeSpan RemainingTimeout(TimeSpan timeout, Stopwatch stopwatch)
    {
        var remaining = timeout - stopwatch.Elapsed;

        if (remaining <= TimeSpan.Zero)
            throw new TimeoutException("The render readiness wait timed out after " + timeout + ".");

        return remaining;
    }

    private async Task WaitForFunctionAsync(
        string expression,
        object? argument,
        TimeSpan timeout,
        CancellationToken cancellationToken)
    {
        await _page.WaitForFunctionAsync(
            expression,
            argument,
            new PageWaitForFunctionOptions
            {
                Timeout = (float)timeout.TotalMilliseconds,
            })
        .WaitForPlaywrightAsync(
            timeout + TimeSpan.FromSeconds(1),
            cancellationToken,
            AbortOperationsAsync)
        .ConfigureAwait(false);
    }

    private async Task<ReadOnlyMemory<byte>> TakeScreenshotCoreAsync(
        WebBrowserScreenshotOptions options,
        CancellationToken cancellationToken) =>
        await _page.ScreenshotAsync(
            new PageScreenshotOptions
            {
                FullPage = options.FullPage,
                OmitBackground = options.OmitBackground,
                Type =
                options.ImageFormat switch
                {
                    WebBrowserScreenshotImageFormat.Png => ScreenshotType.Png,
                    WebBrowserScreenshotImageFormat.Jpeg => ScreenshotType.Jpeg,

                    _ =>
                    throw new ArgumentOutOfRangeException(
                        nameof(options.ImageFormat),
                        options.ImageFormat,
                        "Unknown screenshot image format."),
                },
                Quality = options.Quality,
                Timeout = (float)_operationTimeout.TotalMilliseconds,
            })
        .WaitForPlaywrightAsync(
            _operationTimeout,
            cancellationToken,
            AbortOperationsAsync)
        .ConfigureAwait(false);

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
