using ElmTime.Elm019;
using Pine.Core;
using Pine.Core.Elm.LanguageServer;
using Pine.Core.IO;
using Pine.Core.PineVM;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;

namespace Pine.Elm.LanguageServerAdapters;

/// <summary>
/// Composition of the implementation-neutral <see cref="LanguageServer"/> with the adapters
/// specific to this host: local file system, Elm package cache, <c>elm make</c> and
/// <c>elm-format</c>.
/// </summary>
public static class LanguageServerComposition
{
    /// <summary>
    /// Name of the environment variable selecting the AVH4 <c>elm-format</c> executable instead of
    /// the formatter running in this process.
    /// </summary>
    public const string FormatViaAvh4EnvironmentVariableName = "ELM_LS_FORMAT_VIA_AVH4";

    /// <summary>
    /// Creates the virtual machine used to run the language service program.
    /// </summary>
    public static IPineVM CreatePineVM() =>
        PineVMResettingCache.Create(resetCacheEntriesThresholdDefault: 10_000);

    /// <summary>
    /// Store caching the compiled language service program between processes.
    /// </summary>
    public static IFileStore CreateDefaultCompilationCache(string pineAppVersionId) =>
        new FileStoreFromSystemIOFile(
            Path.Combine(
                Filesystem.CacheDirectory,
                "lang-service-compile",
                pineAppVersionId));

    /// <summary>
    /// Creates a language-service session using the default compilation cache of this host.
    /// </summary>
    public static Result<string, LanguageServiceState> InitLanguageServiceState(
        string pineAppVersionId,
        Action<string>? logDelegate = null) =>
        LanguageServiceState.InitLanguageServiceState(
            CreatePineVM(),
            CreateDefaultCompilationCache(pineAppVersionId),
            logDelegate);

    /// <summary>
    /// Creates the session factory used by the language server.
    /// </summary>
    public static ILanguageServiceSessionFactory CreateSessionFactory(
        string pineAppVersionId,
        Action<string>? logDelegate = null) =>
        new LanguageServiceSessionFactory(
            CreatePineVM,
            CreateDefaultCompilationCache(pineAppVersionId),
            logDelegate);

    /// <summary>
    /// Directories searched for the Elm packages referenced from <c>elm.json</c> files.
    /// </summary>
    public static IReadOnlyList<string> DefaultElmPackagesSearchDirectories() =>
        [Path.Combine(Elm019Binaries.GetElmHomeDirectory(), "0.19.1", "packages")];

    /// <summary>
    /// Selects the formatter implementation, honoring
    /// <see cref="FormatViaAvh4EnvironmentVariableName"/>.
    /// </summary>
    public static IDocumentFormatter CreateDocumentFormatter(
        Action<string>? logDelegate = null)
    {
        var useAvh4Binary =
            Environment.GetEnvironmentVariable(FormatViaAvh4EnvironmentVariableName);

        if (!string.IsNullOrEmpty(useAvh4Binary))
        {
            logDelegate?.Invoke(
                "Using elm-format via AVH4 binary as configured via environment variable " +
                FormatViaAvh4EnvironmentVariableName);

            return new Avh4ElmDocumentFormatter();
        }

        return new InProcessElmDocumentFormatter();
    }

    /// <summary>
    /// Composes a language server for this host.
    /// </summary>
    /// <param name="pineAppVersionId">Version reported to the client and used to scope the cache.</param>
    /// <param name="logDelegate">Optional delegate receiving log messages.</param>
    /// <param name="elmPackagesSearchDirectories">
    /// Directories searched for Elm packages. Defaults to
    /// <see cref="DefaultElmPackagesSearchDirectories"/>.
    /// </param>
    public static Core.Elm.LanguageServer.LanguageServer CreateLanguageServer(
        string pineAppVersionId,
        Action<string>? logDelegate = null,
        IReadOnlyList<string>? elmPackagesSearchDirectories = null)
    {
        var workspace = new FileSystemWorkspace();

        IReadOnlyList<string> searchRootUris =
            [
            .. (elmPackagesSearchDirectories ?? DefaultElmPackagesSearchDirectories())
            .Select(DirectoryUriFromLocalPath)
            ];

        var elmPackageSource =
            new ElmPackageSourceFromWorkspace(
                workspace,
                searchRootUris,
                logDelegate);

        var documentTextSource = new MutableDocumentTextSource();

        var syntaxDiagnosticsProvider =
            new ElmSyntaxDiagnosticsProvider(documentTextSource);

        var diagnosticsProvider =
            new CompositeDiagnosticsProvider(
                syntaxDiagnosticsProvider,
                new ElmMakeDiagnosticsProvider(),
                logDelegate);

        var languageServer =
            new Core.Elm.LanguageServer.LanguageServer(
                sessionFactory: CreateSessionFactory(pineAppVersionId, logDelegate),
                workspace: workspace,
                elmPackageSource: elmPackageSource,
                diagnosticsProvider: diagnosticsProvider,
                documentFormatter: CreateDocumentFormatter(logDelegate),
                options: new LanguageServerOptions(ServerVersion: pineAppVersionId),
                logDelegate: logDelegate,
                formattingDiagnosticsProvider: syntaxDiagnosticsProvider);

        documentTextSource.Inner = languageServer;

        return languageServer;
    }

    private static string DirectoryUriFromLocalPath(string localPath)
    {
        var pathWithSeparator =
            localPath.EndsWith(Path.DirectorySeparatorChar) || localPath.EndsWith('/')
            ?
            localPath
            :
            localPath + Path.DirectorySeparatorChar;

        return new Uri(pathWithSeparator).AbsoluteUri;
    }
}
