using Pine.Core;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.LanguageServerProtocol;
using Pine.Elm.CommonBinaries;
using System;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Elm.LanguageServerAdapters;

public delegate ValueTask<string> ElmFormatInvoker(
    string sourceText,
    CancellationToken cancellationToken);

/// <summary>
/// Adapts Pine's in-process Elm formatter to the language-server formatter boundary.
/// </summary>
public sealed class InProcessElmDocumentFormatter(
    Func<string, Result<ElmSyntaxParseError, string>> format) : IDocumentFormatter
{
    private readonly Func<string, Result<ElmSyntaxParseError, string>> _format =
        format
        ??
        throw new ArgumentNullException(nameof(format));

    public InProcessElmDocumentFormatter()
        : this(ElmFormat.FormatModuleText)
    {
    }

    public ValueTask<Result<DocumentFormattingError, string>> FormatAsync(
        string documentUri,
        string sourceText,
        FormattingOptions options,
        CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        if (!IsElmDocumentUri(documentUri))
        {
            return
                ValueTask.FromResult<Result<DocumentFormattingError, string>>(
                    new DocumentFormattingError(
                        DocumentFormattingErrorKind.UnsupportedDocument,
                        "The in-process Elm formatter only supports .elm documents."));
        }

        try
        {
            return
                ValueTask.FromResult(
                    _format(sourceText)
                    .MapError(
                        error =>
                        new DocumentFormattingError(
                            DocumentFormattingErrorKind.SyntaxError,
                            error.ToString())));
        }
        catch (Exception exception)
        {
            return
                ValueTask.FromResult<Result<DocumentFormattingError, string>>(
                    new DocumentFormattingError(
                        DocumentFormattingErrorKind.ProviderFailure,
                        "The in-process Elm formatter failed: " + exception.Message));
        }
    }

    private static bool IsElmDocumentUri(string documentUri) =>
        Uri.TryCreate(documentUri, UriKind.Absolute, out var uri) &&
        uri.AbsolutePath.EndsWith(".elm", StringComparison.OrdinalIgnoreCase);
}

/// <summary>
/// Adapts the AVH4 elm-format executable to the language-server formatter boundary.
/// </summary>
public sealed class Avh4ElmDocumentFormatter(ElmFormatInvoker format) : IDocumentFormatter
{
    private readonly ElmFormatInvoker _format =
        format
        ??
        throw new ArgumentNullException(nameof(format));

    public Avh4ElmDocumentFormatter()
        : this(InvokeElmFormat)
    {
    }

    public async ValueTask<Result<DocumentFormattingError, string>> FormatAsync(
        string documentUri,
        string sourceText,
        FormattingOptions options,
        CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        if (!Uri.TryCreate(documentUri, UriKind.Absolute, out var uri) ||
            !uri.AbsolutePath.EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
        {
            return
                new DocumentFormattingError(
                    DocumentFormattingErrorKind.UnsupportedDocument,
                    "elm-format only supports .elm documents.");
        }

        try
        {
            var formatted = await _format(sourceText, cancellationToken);

            cancellationToken.ThrowIfCancellationRequested();

            return formatted;
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            throw;
        }
        catch (Exception exception)
        {
            return
                new DocumentFormattingError(
                    DocumentFormattingErrorKind.ProviderFailure,
                    "elm-format failed: " + exception.Message);
        }
    }

    private static async ValueTask<string> InvokeElmFormat(
        string sourceText,
        CancellationToken cancellationToken)
    {
        return
            await AVH4ElmFormatBinaries.RunElmFormatAsync(
                sourceText,
                cancellationToken);
    }
}
