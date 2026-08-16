using System;
using System.Collections.Generic;
using System.Text;

namespace Pine.WebBrowserTesting;

public sealed record WebBrowserConsoleMessage(
    string Type,
    string Text,
    IReadOnlyList<string> Arguments,
    string Location,
    double Timestamp);

public sealed record WebBrowserImageDiagnostics(
    string Source,
    string CurrentSource,
    bool Complete,
    int NaturalWidth,
    int NaturalHeight);

public sealed record WebBrowserRequestFailure(
    string Url,
    string Method,
    string ResourceType,
    string Failure);

public sealed record WebBrowserDocumentDiagnostics(
    string Url,
    string Title,
    string VisibilityState,
    string ReadyState,
    IReadOnlyList<WebBrowserImageDiagnostics> Images);

public sealed record WebBrowserDiagnostics(
    IReadOnlyList<WebBrowserConsoleMessage> ConsoleMessages,
    IReadOnlyList<string> PageErrors,
    IReadOnlyList<WebBrowserRequestFailure> RequestFailures,
    WebBrowserDocumentDiagnostics? Document,
    string BrowserVersion,
    bool BrowserConnected,
    bool ContextClosed,
    bool PageClosed,
    bool PageCrashed,
    IReadOnlyList<string> CollectionErrors)
{
    public override string ToString()
    {
        var builder = new StringBuilder();

        builder.Append("Browser version: ").AppendLine(BrowserVersion);
        builder.Append("Browser connected: ").AppendLine(BrowserConnected.ToString());
        builder.Append("Context closed: ").AppendLine(ContextClosed.ToString());
        builder.Append("Page closed: ").AppendLine(PageClosed.ToString());
        builder.Append("Page crashed: ").AppendLine(PageCrashed.ToString());

        if (Document is not null)
        {
            builder.Append("Page URL: ").AppendLine(Document.Url);
            builder.Append("Document title: ").AppendLine(Document.Title);
            builder.Append("Document visibility: ").AppendLine(Document.VisibilityState);
            builder.Append("Document ready state: ").AppendLine(Document.ReadyState);

            foreach (var image in Document.Images)
            {
                builder
                    .Append("Image: src='")
                    .Append(image.Source)
                    .Append("', currentSrc='")
                    .Append(image.CurrentSource)
                    .Append("', complete=")
                    .Append(image.Complete)
                    .Append(", naturalSize=")
                    .Append(image.NaturalWidth)
                    .Append('x')
                    .Append(image.NaturalHeight)
                    .AppendLine();
            }
        }

        foreach (var requestFailure in RequestFailures)
        {
            builder
                .Append("Request failed: ")
                .Append(requestFailure.Method)
                .Append(' ')
                .Append(requestFailure.Url)
                .Append(" (")
                .Append(requestFailure.ResourceType)
                .Append("): ")
                .AppendLine(requestFailure.Failure);
        }

        foreach (var pageError in PageErrors)
            builder.Append("Page error: ").AppendLine(pageError);

        foreach (var consoleMessage in ConsoleMessages)
        {
            builder
                .Append("Console ")
                .Append(consoleMessage.Type)
                .Append(": ")
                .AppendLine(consoleMessage.Text);
        }

        foreach (var collectionError in CollectionErrors)
            builder.Append("Diagnostic collection error: ").AppendLine(collectionError);

        return builder.ToString().TrimEnd();
    }
}

public sealed record WebBrowserFailureArtifacts(
    string DomSnapshot,
    ReadOnlyMemory<byte> Screenshot,
    ReadOnlyMemory<byte> Trace,
    WebBrowserDiagnostics Diagnostics,
    IReadOnlyList<string> CaptureErrors);

/// <summary>
/// Adds browser and document state to a failed browser operation.
/// </summary>
public sealed class WebBrowserOperationException : Exception
{
    public WebBrowserOperationException(
        string operation,
        WebBrowserDiagnostics diagnostics,
        Exception innerException)
        :
        base(
            operation +
            " failed." +
            Environment.NewLine +
            innerException.Message +
            Environment.NewLine +
            "Browser diagnostics:" +
            Environment.NewLine +
            diagnostics,
            innerException)
    {
        Operation = operation;
        Diagnostics = diagnostics;
    }

    public string Operation { get; }

    public WebBrowserDiagnostics Diagnostics { get; }
}

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
