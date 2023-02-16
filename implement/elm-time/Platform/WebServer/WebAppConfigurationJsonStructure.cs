namespace ElmTime.Platform.WebServer;

public record WebAppConfigurationJsonStructure(
    RateLimitWindow? singleRateLimitWindowPerClientIPv4Address = null,
    int? httpRequestEventSizeLimit = null,
    FluffySpoon.AspNet.LetsEncrypt.LetsEncryptOptions? letsEncryptOptions = null);

public record RateLimitWindow(
    int windowSizeInMs,
    int limit);
