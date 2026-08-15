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
}

public enum WebBrowserScreenshotImageFormat
{
    Png,
    Jpeg,
}
