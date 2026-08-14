using Pine.Core;
using Pine.Core.Elm.Elm019;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.LanguageServerProtocol;
using Pine.Elm019;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

using LspRange = Pine.Core.LanguageServerProtocol.Range;

namespace Pine.Elm.LanguageServerAdapters;

public sealed record ElmMakeInvocationResult(
    int ExitCode,
    string StandardOutput,
    string StandardError);

public delegate ValueTask<ElmMakeInvocationResult> ElmMakeInvoker(
    string workingDirectory,
    string entryPointFilePath,
    CancellationToken cancellationToken);

/// <summary>
/// Adapts <c>elm make --report=json</c> to implementation-neutral language-server diagnostics.
/// </summary>
public sealed class ElmMakeDiagnosticsProvider(
    Func<string, string?> findElmJsonFile,
    ElmMakeInvoker invokeElmMake) : IDiagnosticsProvider
{
    private readonly Func<string, string?> _findElmJsonFile =
            findElmJsonFile
            ??
            throw new ArgumentNullException(nameof(findElmJsonFile));

    private readonly ElmMakeInvoker _invokeElmMake =
            invokeElmMake
            ??
            throw new ArgumentNullException(nameof(invokeElmMake));

    public ElmMakeDiagnosticsProvider()
        : this(FindElmJsonFile, InvokeElmMake)
    {
    }

    public async ValueTask<Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>>
        GetDiagnosticsAsync(
        string entryPointDocumentUri,
        CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        var entryPointPathResult = LocalPathFromDocumentUri(entryPointDocumentUri);

        if (entryPointPathResult.IsErrOrNull() is { } uriError)
            return uriError;

        var entryPointPath =
            entryPointPathResult.IsOkOrNull()
            ??
            throw new InvalidOperationException("Unexpected document URI conversion result.");

        string? elmJsonPath;

        try
        {
            elmJsonPath = _findElmJsonFile(entryPointPath);
        }
        catch (Exception exception)
        {
            return
                Error(
                    DiagnosticsProviderErrorKind.SourceUnavailable,
                    "Failed to locate elm.json: " + exception.Message);
        }

        if (elmJsonPath is null)
        {
            return
                Error(
                    DiagnosticsProviderErrorKind.SourceUnavailable,
                    "Could not find elm.json for " + entryPointDocumentUri);
        }

        var workingDirectory = Path.GetDirectoryName(elmJsonPath);

        if (workingDirectory is null)
        {
            return
                Error(
                    DiagnosticsProviderErrorKind.SourceUnavailable,
                    "Could not determine the Elm project directory for " + entryPointDocumentUri);
        }

        ElmMakeInvocationResult invocation;

        try
        {
            invocation =
                await _invokeElmMake(
                    workingDirectory,
                    entryPointPath,
                    cancellationToken);
        }
        catch (OperationCanceledException) when (cancellationToken.IsCancellationRequested)
        {
            throw;
        }
        catch (Exception exception)
        {
            return
                Error(
                    DiagnosticsProviderErrorKind.ProviderFailure,
                    "Failed to run elm make: " + exception.Message);
        }

        cancellationToken.ThrowIfCancellationRequested();

        if (invocation.ExitCode is 0)
            return Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok([]);

        ElmMakeReport report;

        try
        {
            report = ElmMakeReportConverter.Deserialize(invocation.StandardError);
        }
        catch (Exception exception)
        {
            return
                Error(
                    DiagnosticsProviderErrorKind.InvalidResponse,
                    "Failed to parse elm make report: " + exception.Message);
        }

        if (report is ElmMakeReport.ElmMakeReportError generalError)
        {
            return
                Error(
                    DiagnosticsProviderErrorKind.ProviderFailure,
                    generalError.Title + ": " +
                    string.Concat(generalError.Message.Select(MessageItemToString)));
        }

        if (report is not ElmMakeReport.ElmMakeReportCompileErrors compileErrors)
        {
            return
                Error(
                    DiagnosticsProviderErrorKind.InvalidResponse,
                    "Unexpected elm make report type: " + report.GetType().FullName);
        }

        try
        {
            IReadOnlyList<DocumentDiagnostics> diagnostics =
                [
                .. compileErrors.Errors
                .GroupBy(
                    error => DocumentUriFromReportedPath(workingDirectory, error.Path),
                    StringComparer.Ordinal)
                .OrderBy(group => group.Key, StringComparer.Ordinal)
                .Select(
                    group =>
                    new DocumentDiagnostics(
                        group.Key,
                        [
                        .. group
                        .SelectMany(error => error.Problems)
                        .Select(ToDiagnostic)
                        ]))
                ];

            return
                Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok(
                    diagnostics);
        }
        catch (Exception exception)
        {
            return
                Error(
                    DiagnosticsProviderErrorKind.InvalidResponse,
                    "Failed to map elm make diagnostics: " + exception.Message);
        }
    }

    private static async ValueTask<ElmMakeInvocationResult> InvokeElmMake(
        string workingDirectory,
        string entryPointFilePath,
        CancellationToken cancellationToken)
    {
        var output =
            await ElmMakeRunner.ElmMakeAsync(
                workingDirectory,
                entryPointFilePath,
                cancellationToken);

        return
            new ElmMakeInvocationResult(
                output.ExitCode,
                output.StandardOutput,
                output.StandardError);
    }

    private static Result<DiagnosticsProviderError, string> LocalPathFromDocumentUri(
        string documentUri)
    {
        if (!Uri.TryCreate(documentUri, UriKind.Absolute, out var uri) ||
            !string.Equals(uri.Scheme, Uri.UriSchemeFile, StringComparison.OrdinalIgnoreCase))
        {
            return
                Error(
                    DiagnosticsProviderErrorKind.InvalidRequest,
                    "elm make requires an absolute file document URI: " + documentUri);
        }

        try
        {
            return Path.GetFullPath(uri.LocalPath);
        }
        catch (Exception exception)
        {
            return
                Error(
                    DiagnosticsProviderErrorKind.InvalidRequest,
                    "Failed to convert the document URI to a local path: " + exception.Message);
        }
    }

    private static string? FindElmJsonFile(string entryPointFilePath)
    {
        var directoryPath = Path.GetDirectoryName(entryPointFilePath);

        while (directoryPath is not null)
        {
            var elmJsonPath = Path.Combine(directoryPath, "elm.json");

            if (File.Exists(elmJsonPath))
                return elmJsonPath;

            directoryPath = Path.GetDirectoryName(directoryPath);
        }

        return null;
    }

    private static string DocumentUriFromReportedPath(
        string workingDirectory,
        string reportedPath)
    {
        if (string.IsNullOrWhiteSpace(reportedPath))
            throw new InvalidDataException("elm make returned an empty diagnostic path.");

        var absolutePath =
            Path.IsPathFullyQualified(reportedPath)
            ?
            Path.GetFullPath(reportedPath)
            :
            Path.GetFullPath(reportedPath, workingDirectory);

        return new Uri(absolutePath, UriKind.Absolute).AbsoluteUri;
    }

    private static Diagnostic ToDiagnostic(ElmMakeReportCompileErrorsErrorProblem problem) =>
        new(
            Range: new LspRange(
                Start: new Position(
                    Line: (uint)Math.Max(0, problem.Region.Start.Line - 1),
                    Character: (uint)Math.Max(0, problem.Region.Start.Column - 1)),
                End: new Position(
                    Line: (uint)Math.Max(0, problem.Region.End.Line - 1),
                    Character: (uint)Math.Max(0, problem.Region.End.Column - 1))),
            Severity: DiagnosticSeverity.Error,
            Code: null,
            Source: "elm make",
            Message: string.Concat(problem.Message.Select(MessageItemToString)),
            CodeDescription: null,
            Tags: null,
            RelatedInformation: null);

    private static string MessageItemToString(MessageItem item) =>
        item switch
        {
            MessageItem.StringMessage stringMessage =>
            stringMessage.Value,

            MessageItem.StyledMessage styledMessage =>
            styledMessage.String,

            _ =>
            throw new InvalidOperationException("Unexpected elm make message item: " + item.GetType().FullName)
        };

    private static DiagnosticsProviderError Error(
        DiagnosticsProviderErrorKind kind,
        string message) =>
        new(kind, message);
}
