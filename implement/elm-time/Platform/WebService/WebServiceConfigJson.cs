namespace ElmTime.Platform.WebService;

/// <summary>
/// Structure of the optional 'web-service.json' file to configure a web server.
/// </summary>
public record WebServiceConfigJson(
    RateLimitWindow? singleRateLimitWindowPerClientIPv4Address = null,
    int? httpRequestEventSizeLimit = null,
    FluffySpoon.AspNet.LetsEncrypt.LetsEncryptOptions? letsEncryptOptions = null);

public record RateLimitWindow(
    int windowSizeInMs,
    int limit);
