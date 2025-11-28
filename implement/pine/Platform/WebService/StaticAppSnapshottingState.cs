using ElmTime.Platform.WebService;
using Microsoft.AspNetCore.Http;
using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using Pine.Core.Files;
using Pine.Core.IO;
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
    public const int HttpRequestEventSizeLimitDefault = 1_000_000;

    private readonly ProcessLiveRepresentation _process;

    private readonly PublicAppState _publicAppState;

    private readonly IFileStore _fileStore;

    private PineValue? _lastAppStateSnapshot = null;

    private readonly Lock _processAndStoreLock = new();

    const string AppStateSnapshotFileName = "app-state-snapshot";

    private readonly string _webServiceAppHash;

    private readonly IImmutableList<string> _appStateSnapshotFilePath;

    public static StaticAppSnapshottingState Create(
        FileTree webServiceAppSourceFiles,
        IFileStore fileStore,
        Action<string> logMessage,
        CancellationToken cancellationToken) =>
        Create(
            webServiceAppSourceFiles,
            entryFileName: ["src", "Backend", "Main.elm"],
            fileStore: fileStore,
            logMessage: logMessage,
            cancellationToken: cancellationToken);

    public static StaticAppSnapshottingState Create(
        FileTree webServiceAppSourceFiles,
        IReadOnlyList<string> entryFileName,
        IFileStore fileStore,
        Action<string> logMessage,
        CancellationToken cancellationToken)
    {
        var webServiceCompiledModules =
            WebServiceInterface.CompiledModulesFromSourceFilesAndEntryFileName(
                webServiceAppSourceFiles,
                entryFileName: entryFileName);

        return
            new StaticAppSnapshottingState(
                webServiceCompiledModules,
                fileStore,
                logMessage,
                cancellationToken);
    }

    public static StaticAppSnapshottingState Create(
        PineValue webServiceCompiledModules,
        IFileStore fileStore,
        Action<string> logMessage,
        CancellationToken cancellationToken)
    {
        return
            new StaticAppSnapshottingState(
                webServiceCompiledModules,
                fileStore,
                logMessage,
                cancellationToken);
    }

    public StaticAppSnapshottingState(
        PineValue webServiceCompiledModules,
        IFileStore fileStore,
        Action<string> logMessage,
        CancellationToken cancellationToken)
    {
        _fileStore = fileStore;

        _webServiceAppHash =
            Convert.ToHexStringLower(PineValueHashTree.ComputeHash(webServiceCompiledModules).Span);

        _appStateSnapshotFilePath =
            ["v-" + _webServiceAppHash[..8], AppStateSnapshotFileName];

        var webServiceConfig =
            WebServiceInterface.ConfigFromCompiledModules(
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
                    ProcessHttpRequestAsync: _process.ProcessHttpRequestAsync,
                    InitOrMigrateCmds: []),
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
                    ValueBinaryEncodingClassic.DecodeRoot(appStateSnapshotBytes);

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

    public record HandleRequestReport(
        WebServiceInterface.HttpRequestEventStruct RequestEvent,
        bool StateChanged,
        WebServiceInterface.HttpResponse Response);

    public readonly record struct PersistenceReport(
        bool StateChanged,
        TimeSpan Duration);

    public Task<HandleRequestReport> HandleRequestAsync(
        HttpContext context,
        Action<string>? logMessage = null,
        Func<WebServiceInterface.HttpRequestProperties, WebServiceInterface.HttpRequestProperties>? prepareRequest = null,
        Func<WebServiceInterface.HttpResponse, WebServiceInterface.HttpResponse>? finalizeResponse = null)
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

                if (prepareRequest is not null)
                {
                    httpRequestProperties = prepareRequest(httpRequestProperties);
                }

                lock (_processAndStoreLock)
                {
                    var processReport =
                        _publicAppState.HandleRequestAsync(
                            httpRequestProperties,
                            new WebServiceInterface.HttpRequestContext(
                                ClientAddress: context.Connection.RemoteIpAddress?.ToString()),
                            httpRequestEventSizeLimit: HttpRequestEventSizeLimitDefault,
                            cancellationToken: context.RequestAborted).Result;

                    var httpResponse = processReport.Response;

                    if (finalizeResponse is not null)
                    {
                        httpResponse = finalizeResponse(httpResponse);
                    }

                    var persistReport = EnsurePersisted(logMessage);

                    return new HandleRequestReport(processReport.RequestEvent, StateChanged: persistReport.StateChanged, httpResponse);
                }
            })
            .ContinueWith(reportTask =>
            {
                if (reportTask.IsFaulted)
                {
                    logMessage?.Invoke("Failed to handle HTTP request: " + reportTask.Exception);
                    context.Response.StatusCode = 500;

                    return new HandleRequestReport(
                        new WebServiceInterface.HttpRequestEventStruct(
                            HttpRequestId: "failed",
                            PosixTimeMilli: DateTimeOffset.UtcNow.ToUnixTimeMilliseconds(),
                            new WebServiceInterface.HttpRequestContext(ClientAddress: context.Connection.RemoteIpAddress?.ToString()),
                            Request: new WebServiceInterface.HttpRequestProperties(
                                Method: context.Request.Method,
                                Uri: context.Request.Path,
                                Body: null,
                                Headers: [])),
                        StateChanged: false,
                        Response: new WebServiceInterface.HttpResponse(
                            StatusCode: 500,
                            Body: System.Text.Encoding.UTF8.GetBytes("Failed to handle HTTP request."),
                            HeadersToAdd: []));
                }
                else
                {
                    var report = reportTask.Result;
                    return Asp.SendHttpResponseAsync(context, report.Response)
                        .ContinueWith(_ => report)
                        .Result;
                }
            });
    }

    private PersistenceReport EnsurePersisted(
        Action<string>? logMessage)
    {
        var clock = System.Diagnostics.Stopwatch.StartNew();

        lock (_processAndStoreLock)
        {
            var newAppStateSnapshot = _process.GetAppStateOnMainBranch();

            if (!newAppStateSnapshot.Equals(_lastAppStateSnapshot))
            {
                using var stream = new System.IO.MemoryStream();

                ValueBinaryEncodingClassic.Encode(stream, newAppStateSnapshot);

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
                return new PersistenceReport(StateChanged: true, Duration: clock.Elapsed);
            }
        }

        return new PersistenceReport(StateChanged: false, Duration: clock.Elapsed);
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
