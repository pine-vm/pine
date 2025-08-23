using ElmTime.Platform.WebService;
using Microsoft.AspNetCore.Http;
using Pine.Core;
using Pine.Core.PopularEncodings;
using Pine.Elm.Platform;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Platform.WebService;

/// <summary>
/// A simple web service that persists exclusively via JSON snapshots.
/// If loading of the app state snapshot from the file store fails (as can happen after a schema change),
/// it uses the app state from the init function of the web service app configuration.
/// </summary>
public sealed record StaticAppSnapshottingState : IAsyncDisposable
{
    private readonly ProcessLiveRepresentation _process;

    private readonly PublicAppState _publicAppState;

    private readonly WebServiceConfigJson? _serverConfig;

    private readonly IFileStore _fileStore;

    private PineValue? _lastAppStateSnapshot = null;

    private readonly Lock _processAndStoreLock = new();

    const string AppStateSnapshotFileName = "app-state-snapshot";

    private readonly string _webServiceAppHash;

    private readonly IImmutableList<string> _appStateSnapshotFilePath;

    public static StaticAppSnapshottingState Create(
        TreeNodeWithStringPath webServiceAppSourceFiles,
        WebServiceConfigJson? serverConfig,
        IFileStore fileStore,
        Action<string> logMessage,
        CancellationToken cancellationToken) =>
        Create(
            webServiceAppSourceFiles,
            serverConfig: serverConfig,
            entryFileName: ["src", "Backend", "Main.elm"],
            fileStore: fileStore,
            logMessage: logMessage,
            cancellationToken: cancellationToken);

    public static StaticAppSnapshottingState Create(
        TreeNodeWithStringPath webServiceAppSourceFiles,
        WebServiceConfigJson? serverConfig,
        IReadOnlyList<string> entryFileName,
        IFileStore fileStore,
        Action<string> logMessage,
        CancellationToken cancellationToken)
    {
        var webServiceCompiledModules =
            Elm.Platform.WebServiceInterface.CompiledModulesFromSourceFilesAndEntryFileName(
                webServiceAppSourceFiles,
                entryFileName: entryFileName);

        return
            new StaticAppSnapshottingState(
                webServiceCompiledModules,
                serverConfig,
                fileStore,
                logMessage,
                cancellationToken);
    }

    public static StaticAppSnapshottingState Create(
        PineValue webServiceCompiledModules,
        WebServiceConfigJson? serverConfig,
        IFileStore fileStore,
        Action<string> logMessage,
        CancellationToken cancellationToken)
    {
        return
            new StaticAppSnapshottingState(
                webServiceCompiledModules,
                serverConfig,
                fileStore,
                logMessage,
                cancellationToken);
    }

    public StaticAppSnapshottingState(
        PineValue webServiceCompiledModules,
        WebServiceConfigJson? serverConfig,
        IFileStore fileStore,
        Action<string> logMessage,
        CancellationToken cancellationToken)
    {
        _fileStore = fileStore;
        _serverConfig = serverConfig;

        _webServiceAppHash =
            Convert.ToHexStringLower(PineValueHashTree.ComputeHash(webServiceCompiledModules).Span);

        _appStateSnapshotFilePath =
            ["v-" + _webServiceAppHash[..8], AppStateSnapshotFileName];

        var webServiceConfig =
            Elm.Platform.WebServiceInterface.ConfigFromCompiledModules(
                webServiceCompiledModules,
                entryModuleName: "Backend.Main",
                entryDeclName: "webServiceMain")
            .Extract(err => throw new Exception("Failed to parse WebServiceConfig: " + err));

        var discardingWriter = new DiscardingProgressWriter();

        _process =
            ProcessLiveRepresentation.Create(
                webServiceConfig,
                lastAppState: null,
                progressWriter: discardingWriter,
                getDateTimeOffset: () => DateTimeOffset.UtcNow,
                artifactSourceCompositions: [webServiceCompiledModules],
                cancellationToken: cancellationToken);

        _publicAppState =
            new PublicAppState(
                serverAndElmAppConfig: new ServerAndElmAppConfig(
                    ServerConfig: serverConfig,
                    ProcessHttpRequestAsync: _process.ProcessHttpRequestAsync,
                    InitOrMigrateCmds: null,
                    DisableLetsEncrypt: true,
                    DisableHttps: true),
                getDateTimeOffset: () => DateTimeOffset.UtcNow);

        logMessage(
            "Initializing web service app " + _webServiceAppHash[..8]);

        // Attempt to load the app state snapshot from the file store.

        if (_fileStore.GetFileContent(_appStateSnapshotFilePath) is { } appStateSnapshotBytes)
        {
            logMessage(
                "App state snapshot file found, attempting to deserialize from " +
                CommandLineInterface.FormatIntegerForDisplay(appStateSnapshotBytes.Length) +
                " bytes.");

            try
            {
                var appState =
                    PineValueBinaryEncoding.DecodeRoot(appStateSnapshotBytes);

                var setStateResult =
                    _process.ResetAppStateIgnoringTypeChecking(appState);

                _lastAppStateSnapshot = appState;
            }
            catch (Exception ex)
            {
                // If deserialization fails, we log the error and initialize from the default state.
                logMessage($"Failed to deserialize app state snapshot: {ex.Message}");
            }
        }
        else
        {
            logMessage("App state snapshot file not found, initializing from default.");
        }
    }

    public Task HandleRequestAsync(
        HttpContext context,
        Action<string>? logMessage = null,
        Func<WebServiceInterface.HttpRequestProperties, WebServiceInterface.HttpRequestProperties>? modifyRequest = null)
    {
        return
            Asp.AsInterfaceHttpRequestAsync(context.Request)
            .ContinueWith(httpRequestTask =>
            {
                if (httpRequestTask.IsFaulted)
                {
                    logMessage?.Invoke("Failed to convert HTTP request: " + httpRequestTask.Exception);

                    context.Response.StatusCode = 500;
                }

                var httpRequestProperties = httpRequestTask.Result;

                if (modifyRequest is not null)
                {
                    httpRequestProperties = modifyRequest(httpRequestProperties);
                }

                lock (_processAndStoreLock)
                {
                    var httpResponse =
                        _publicAppState.HandleRequestAsync(
                            httpRequestProperties,
                            new WebServiceInterface.HttpRequestContext(
                                ClientAddress: context.Connection.RemoteIpAddress?.ToString()),
                            httpRequestEventSizeLimit: _serverConfig?.httpRequestEventSizeLimit,
                            cancellationToken: context.RequestAborted).Result;

                    EnsurePersisted(logMessage);

                    return httpResponse;
                }
            })
            .ContinueWith(httpResponseTask =>
            {
                if (httpResponseTask.IsFaulted)
                {
                    logMessage?.Invoke("Failed to handle HTTP request: " + httpResponseTask.Exception);
                    context.Response.StatusCode = 500;

                    return Task.CompletedTask;
                }
                else
                {
                    return Asp.SendHttpResponseAsync(context, httpResponseTask.Result);
                }
            });
    }

    private void EnsurePersisted(
        Action<string>? logMessage)
    {
        var clock = System.Diagnostics.Stopwatch.StartNew();

        lock (_processAndStoreLock)
        {
            var newAppStateSnapshot = _process.GetAppStateOnMainBranch();

            if (!newAppStateSnapshot.Equals(_lastAppStateSnapshot))
            {
                using var stream = new System.IO.MemoryStream();

                PineValueBinaryEncoding.Encode(stream, newAppStateSnapshot);

                stream.Seek(0, System.IO.SeekOrigin.Begin);

                _fileStore.SetFileContent(
                    _appStateSnapshotFilePath,
                    fileContent: stream.ToArray());

                logMessage?.Invoke(
                    "App state snapshot updated in " +
                    CommandLineInterface.FormatIntegerForDisplay(clock.ElapsedMilliseconds) +
                    " ms, new size: " +
                    CommandLineInterface.FormatIntegerForDisplay(stream.Length) +
                    " bytes.");

                _lastAppStateSnapshot = newAppStateSnapshot;
            }
        }
    }

    public ValueTask DisposeAsync()
    {
        async Task DisposeAsyncCore()
        {
            await _process.DisposeAsync();
        }

        return new ValueTask(DisposeAsyncCore());
    }
}
