using Pine.Core.Elm.LanguageServer;
using Pine.Core.Elm.LanguageServer.LanguageServiceInterface;
using Pine.Core.IO;
using System;
using System.Collections.Generic;
using System.Text;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Core.Tests.Elm.LanguageServer;

/// <summary>
/// Language-service session recording the mutations applied by a language server.
/// </summary>
public class RecordingLanguageServiceSession : ILanguageServiceSession
{
    private readonly Dictionary<string, string> _files = new(StringComparer.Ordinal);

    private readonly List<string> _mutations = [];

    private readonly List<(ElmPackageVersion019Identifer PackageVersionId,
        IReadOnlyList<KeyValuePair<IReadOnlyList<string>, string>> Modules)>
        _packages = [];

    public IReadOnlyDictionary<string, string> Files => _files;

    public IReadOnlyList<string> Mutations => _mutations;

    public IReadOnlyList<(ElmPackageVersion019Identifer PackageVersionId,
        IReadOnlyList<KeyValuePair<IReadOnlyList<string>, string>> Modules)> Packages => _packages;

    public string? TryGetFile(string fileUri) =>
        _files.TryGetValue(fileUri, out var content) ? content : null;

    public Result<string, Response.WorkspaceSummaryResponse> AddFile(
        string fileUri,
        string fileContentAsText)
    {
        lock (_files)
        {
            _files[fileUri] = fileContentAsText;
            _mutations.Add("add:" + fileUri);
        }

        return new Response.WorkspaceSummaryResponse();
    }

    public Result<string, Response.WorkspaceSummaryResponse> DeleteFile(
        string fileUri)
    {
        lock (_files)
        {
            _files.Remove(fileUri);
            _mutations.Add("delete:" + fileUri);
        }

        return new Response.WorkspaceSummaryResponse();
    }

    public Result<string, Response.WorkspaceSummaryResponse> AddElmPackage(
        ElmPackageVersion019Identifer packageVersionId,
        IReadOnlyList<KeyValuePair<IReadOnlyList<string>, string>> filesContentsAsText)
    {
        lock (_files)
        {
            _packages.Add((packageVersionId, filesContentsAsText));
            _mutations.Add("package:" + packageVersionId.PackageName + "@" + packageVersionId.VersionTag);
        }

        return new Response.WorkspaceSummaryResponse();
    }

    public Result<string, Response> HandleRequest(Request request) =>
        "Not implemented in this test double";
}

/// <summary>
/// Session factory returning a session prepared by the test.
/// </summary>
public class StubLanguageServiceSessionFactory(
    Result<string, ILanguageServiceSession> result)
    : ILanguageServiceSessionFactory
{
    public static StubLanguageServiceSessionFactory WithSession(ILanguageServiceSession session) =>
        new(Result<string, ILanguageServiceSession>.ok(session));

    public int CreateSessionCount { get; private set; }

    public ValueTask<Result<string, ILanguageServiceSession>> CreateSessionAsync(
        CancellationToken cancellationToken)
    {
        ++CreateSessionCount;

        return ValueTask.FromResult(result);
    }
}

/// <summary>
/// Diagnostics provider returning results prepared by the test, per entry-point document.
/// </summary>
public class StubDiagnosticsProvider : IDiagnosticsProvider
{
    private readonly Dictionary<string, Func<CancellationToken,
        ValueTask<Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>>>>
        _responses =
        new(StringComparer.Ordinal);

    private readonly List<string> _requests = [];

    public IReadOnlyList<string> Requests => _requests;

    public void SetResult(
        string entryPointDocumentUri,
        Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>> result)
    {
        _responses[entryPointDocumentUri] = _ => ValueTask.FromResult(result);
    }

    public void SetHandler(
        string entryPointDocumentUri,
        Func<CancellationToken,
            ValueTask<Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>>> handler)
    {
        _responses[entryPointDocumentUri] = handler;
    }

    public ValueTask<Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>>
        GetDiagnosticsAsync(
        string entryPointDocumentUri,
        CancellationToken cancellationToken)
    {
        lock (_requests)
        {
            _requests.Add(entryPointDocumentUri);
        }

        if (_responses.TryGetValue(entryPointDocumentUri, out var handler))
        {
            return handler(cancellationToken);
        }

        return
            ValueTask.FromResult(
                Result<DiagnosticsProviderError, IReadOnlyList<DocumentDiagnostics>>.ok([]));
    }
}

/// <summary>
/// Formatter applying a function supplied by the test.
/// </summary>
public class StubDocumentFormatter(
    Func<string, Result<DocumentFormattingError, string>> format)
    : IDocumentFormatter
{
    public ValueTask<Result<DocumentFormattingError, string>> FormatAsync(
        string documentUri,
        string sourceText,
        LanguageServerProtocol.FormattingOptions options,
        CancellationToken cancellationToken)
    {
        cancellationToken.ThrowIfCancellationRequested();

        return ValueTask.FromResult(format(sourceText));
    }
}

/// <summary>
/// Helpers to build in-memory workspaces addressed by virtual document URIs.
/// </summary>
public static class VirtualWorkspace
{
    public const string RootUri = "memory://workspace/";

    public static string DocumentUri(params string[] pathComponents) =>
        RootUri + string.Join("/", pathComponents);

    public static (WorkspaceFromFileStoreMounts Workspace, FileStoreFromConcurrentDictionary Store)
        Create(IReadOnlyList<(string[] Path, string Content)>? files = null)
    {
        var store = new FileStoreFromConcurrentDictionary();

        foreach (var (path, content) in files ?? [])
        {
            SetFile(store, path, content);
        }

        var workspace =
            new WorkspaceFromFileStoreMounts(
                [new FileStoreMount(new Uri(RootUri), store)]);

        return (workspace, store);
    }

    public static void SetFile(
        FileStoreFromConcurrentDictionary store,
        IReadOnlyList<string> path,
        string content) =>
        store.SetFileContent(
            [.. path],
            Encoding.UTF8.GetBytes(content));

    public static void DeleteFile(
        FileStoreFromConcurrentDictionary store,
        IReadOnlyList<string> path) =>
        store.DeleteFile([.. path]);
}
