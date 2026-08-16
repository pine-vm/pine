using System;
using System.Collections.Generic;

namespace Pine.WebBrowserTesting;

/// <summary>
/// Selects where Chromium runs.
/// </summary>
public enum WebBrowserExecutionMode
{
    Container,
    Host,
}

/// <summary>
/// Options for starting a Chromium browser process.
/// </summary>
public sealed record WebBrowserProcessOptions
{
    public const string DefaultContainerImage = "mcr.microsoft.com/playwright:v1.62.0-noble";

    public string ContainerImage { get; init; } = DefaultContainerImage;

    public bool Headless { get; init; } = true;

    public TimeSpan StartupTimeout { get; init; } = TimeSpan.FromMinutes(2);

    public TimeSpan OperationTimeout { get; init; } = TimeSpan.FromSeconds(30);
}

/// <summary>
/// Viewport and browser-environment options for one isolated test context.
/// </summary>
public sealed record WebBrowserContextOptions
{
    public int ViewportWidth { get; init; } = 1280;

    public int ViewportHeight { get; init; } = 720;

    public int? ScreenWidth { get; init; }

    public int? ScreenHeight { get; init; }

    public float DeviceScaleFactor { get; init; } = 1;

    public bool IsMobile { get; init; }

    public bool HasTouch { get; init; }

    public bool JavaScriptEnabled { get; init; } = true;

    public string? Locale { get; init; }

    public string? TimezoneId { get; init; }

    public string? UserAgent { get; init; }

    public WebBrowserColorScheme ColorScheme { get; init; } = WebBrowserColorScheme.NoPreference;

    public bool ReducedMotion { get; init; }

    public IReadOnlyDictionary<string, string>? ExtraHttpHeaders { get; init; }
}

public enum WebBrowserColorScheme
{
    NoPreference,
    Light,
    Dark,
}

/// <summary>
/// Describes an in-memory HTML response served at a virtual HTTP origin.
/// </summary>
public sealed record HtmlDocumentOptions
{
    public Uri Url { get; init; } = new("http://pine.test/");

    public string ContentType { get; init; } = "text/html; charset=utf-8";

    public int StatusCode { get; init; } = 200;

    public IReadOnlyDictionary<string, string>? Headers { get; init; }
}

public enum WebBrowserLocatorState
{
    Attached,
    Detached,
    Visible,
    Hidden,
}

public enum WebBrowserMouseButton
{
    Left,
    Right,
    Middle,
}

public sealed record WebBrowserScreenshotOptions
{
    public WebBrowserScreenshotImageFormat ImageFormat { get; init; } = WebBrowserScreenshotImageFormat.Png;

    public int? Quality { get; init; }

    public bool FullPage { get; init; }

    public bool OmitBackground { get; init; }

    /// <summary>
    /// Optional readiness checks to run immediately before capture.
    /// Set to <see langword="null"/> to capture without an additional wait.
    /// </summary>
    public WebBrowserRenderWaitOptions? WaitForRender { get; init; } = new();
}

public enum WebBrowserScreenshotImageFormat
{
    Png,
    Jpeg,
}

/// <summary>
/// Configures the conditions that must be satisfied before content is considered renderable.
/// </summary>
public sealed record WebBrowserRenderWaitOptions
{
    /// <summary>
    /// Optional JavaScript function or expression that must become truthy before resource checks begin.
    /// </summary>
    public string? ReadyExpression { get; init; }

    /// <summary>
    /// Optional argument passed to <see cref="ReadyExpression"/>.
    /// </summary>
    public object? ReadyExpressionArgument { get; init; }

    /// <summary>
    /// Wait for every image in the document to finish loading and decoding.
    /// </summary>
    public bool WaitForImages { get; init; } = true;

    /// <summary>
    /// Fail when a completed image has no decoded pixels.
    /// </summary>
    public bool FailOnImageError { get; init; } = true;

    /// <summary>
    /// Wait for the document font set, when available.
    /// </summary>
    public bool WaitForFonts { get; init; } = true;

    /// <summary>
    /// Number of animation frames to wait after all other conditions are satisfied.
    /// </summary>
    public int AnimationFrameCount { get; init; } = 2;

    /// <summary>
    /// Overrides the browser operation timeout for the complete render wait.
    /// </summary>
    public TimeSpan? Timeout { get; init; }
}

/// <summary>
/// Selects which potentially expensive failure artifacts to capture.
/// </summary>
public sealed record WebBrowserFailureArtifactOptions
{
    public bool CaptureDomSnapshot { get; init; } = true;

    public bool CaptureScreenshot { get; init; } = true;

    public bool CaptureTrace { get; init; } = true;
}
