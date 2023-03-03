namespace ElmTime.Platform.WebServer;

/// <summary>
/// Structure of the optional 'web-server.json' file to configure a web server.
/// </summary>
public record WebServerConfigJson(
    RateLimitWindow? singleRateLimitWindowPerClientIPv4Address = null,
    int? httpRequestEventSizeLimit = null,
    FluffySpoon.AspNet.LetsEncrypt.LetsEncryptOptions? letsEncryptOptions = null);

public record RateLimitWindow(
    int windowSizeInMs,
    int limit);
