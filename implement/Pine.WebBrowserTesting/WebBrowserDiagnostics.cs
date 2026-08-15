using System;
using System.Collections.Generic;

namespace Pine.WebBrowserTesting;

public sealed record WebBrowserConsoleMessage(
    string Type,
    string Text,
    IReadOnlyList<string> Arguments,
    string Location,
    double Timestamp);

public sealed record WebBrowserDiagnostics(
    IReadOnlyList<WebBrowserConsoleMessage> ConsoleMessages,
    IReadOnlyList<string> PageErrors);

public sealed record WebBrowserFailureArtifacts(
    string DomSnapshot,
    ReadOnlyMemory<byte> Screenshot,
    ReadOnlyMemory<byte> Trace,
    WebBrowserDiagnostics Diagnostics);

/// <summary>
/// Thrown when a browser container cannot be started or reached.
/// </summary>
public sealed class WebBrowserStartupException : Exception
{
    public WebBrowserStartupException(string message, string containerLogs, Exception innerException)
        : base(message, innerException)
    {
        ContainerLogs = containerLogs;
    }

    public string ContainerLogs { get; }
}
