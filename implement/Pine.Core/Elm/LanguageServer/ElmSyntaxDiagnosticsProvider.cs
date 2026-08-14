using System;
using System.Collections.Generic;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Core.Elm.LanguageServer;

/// <summary>
/// Reports syntax errors found by the Elm syntax parser as diagnostics for the entry-point
/// document itself.
/// </summary>
/// <param name="documentTextSource">Provides the current text of the document.</param>
public class ElmSyntaxDiagnosticsProvider(
    IDocumentTextSource documentTextSource)
    : IDiagnosticsProvider
{
    /// <inheritdoc/>
    public ValueTask<Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>>
        GetDiagnosticsAsync(
        string entryPointDocumentUri,
        CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        if (!entryPointDocumentUri.EndsWith(".elm", StringComparison.OrdinalIgnoreCase))
        {
            return
                ValueTask.FromResult(
                    Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok([]));
        }

        var documentText = documentTextSource.TryGetDocumentText(entryPointDocumentUri);

        if (documentText is null)
        {
            return
                ValueTask.FromResult(
                    Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.err(
                        new DiagnosticsProviderError(
                            DiagnosticsProviderErrorKind.SourceUnavailable,
                            "No text available for " + entryPointDocumentUri)));
        }

        var diagnostics =
            LanguageServer.ComputeSyntaxErrorDiagnostics(documentText);

        IReadOnlyList<DocumentDiagnostics> documentDiagnostics =
            diagnostics.Count is 0
            ?
            []
            :
            [new DocumentDiagnostics(entryPointDocumentUri, diagnostics)];

        return
            ValueTask.FromResult(
                Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok(
                    documentDiagnostics));
    }
}

/// <summary>
/// Runs <paramref name="first"/> and only falls back to <paramref name="second"/> when the first
/// provider reported no diagnostics.
/// <para>
/// Used to keep the syntax-first behavior: while a module does not even parse, a compiler report
/// would not add value.
/// </para>
/// </summary>
/// <param name="first">Provider consulted first.</param>
/// <param name="second">Provider consulted when the first one reported no diagnostics.</param>
/// <param name="logDelegate">Optional delegate receiving log messages.</param>
public class CompositeDiagnosticsProvider(
    IDiagnosticsProvider first,
    IDiagnosticsProvider second,
    Action<string>? logDelegate = null)
    : IDiagnosticsProvider
{
    /// <inheritdoc/>
    public async ValueTask<Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>>
        GetDiagnosticsAsync(
        string entryPointDocumentUri,
        CancellationToken cancellationToken)
    {
        var firstResult =
            await first.GetDiagnosticsAsync(entryPointDocumentUri, cancellationToken);

        cancellationToken.ThrowIfCancellationRequested();

        if (firstResult.IsErrOrNull() is { } firstError)
        {
            logDelegate?.Invoke(
                "First diagnostics provider failed for " + entryPointDocumentUri + ": " +
                firstError.Kind + ": " + firstError.Message);
        }
        else
        {
            if (firstResult.IsOkOrNull() is not { } firstDiagnostics)
            {
                throw new InvalidOperationException(
                    "Unexpected diagnostics result type: " + firstResult.GetType());
            }

            var anyDiagnostic = false;

            foreach (var documentDiagnostics in firstDiagnostics)
            {
                if (0 < documentDiagnostics.Diagnostics.Count)
                {
                    anyDiagnostic = true;
                    break;
                }
            }

            if (anyDiagnostic)
            {
                return firstResult;
            }
        }

        return await second.GetDiagnosticsAsync(entryPointDocumentUri, cancellationToken);
    }
}
