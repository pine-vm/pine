using ElmTime.Platform.WebService;
using ElmTime.Platform.WebService.ProcessStoreSupportingMigrations;
using Microsoft.AspNetCore.Http;
using Pine.Core;
using Pine.PineVM;
using System;
using System.Collections.Immutable;
using System.Text;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Platform.WebService;

/// <summary>
/// A simple web service that persists exclusively via JSON snapshots.
/// If loading of the app state snapshot from the file store fails (as can happen after a schema change),
/// it uses the app state from the init function of the web service app configuration.
/// </summary>
public record StaticAppSnapshottingViaJson
{
    private readonly PersistentProcessLiveRepresentation _process;

    private readonly PublicAppState _publicAppState;

    private readonly IFileStore _fileStore;

    private string? _lastAppStateSnapshot = null;

    private readonly PineVMCache _pineVMCache = new();

    private readonly PineVM.PineVM _pineVM;

    const string AppStateSnapshotFileName = "app-state-snapshot.json";

    static readonly IImmutableList<string> s_appStateSnapshotFilePath = [AppStateSnapshotFileName];

    public StaticAppSnapshottingViaJson(
        TreeNodeWithStringPath webServiceAppSourceFiles,
        IFileStore fileStore,
        Action<string> logMessage,
        CancellationToken cancellationToken)
    {
        _fileStore = fileStore;

        _pineVM = new PineVM.PineVM(evalCache: _pineVMCache.EvalCache);

        var discardingWriter = new DiscardingStoreWriter();

        var sourceComposition =
            PineValueComposition.FromTreeWithStringPath(webServiceAppSourceFiles);

        _process =
            PersistentProcessLiveRepresentation.Create(
                new ProcessAppConfig(sourceComposition),
                lastAppState: null,
                storeWriter: discardingWriter,
                getDateTimeOffset: () => DateTimeOffset.UtcNow,
                overrideElmAppInterfaceConfig: null,
                cancellationToken: cancellationToken);

        _publicAppState =
            new PublicAppState(
                serverAndElmAppConfig: new ServerAndElmAppConfig(
                    ServerConfig: null,
                    ProcessHttpRequestAsync: _process.ProcessHttpRequestAsync,
                    SourceComposition: sourceComposition,
                    InitOrMigrateCmds: null,
                    DisableLetsEncrypt: true,
                    DisableHttps: true),
                getDateTimeOffset: () => DateTimeOffset.UtcNow);

        // Attempt to load the app state snapshot from the file store.

        if (_fileStore.GetFileContent(s_appStateSnapshotFilePath) is { } appStateSnapshotBytes)
        {
            logMessage(
                "App state snapshot file found, attempting to deserialize from " +
                CommandLineInterface.FormatIntegerForDisplay(appStateSnapshotBytes.Length) +
                " bytes.");

            try
            {
                var appStateSnapshotJsonString = Encoding.UTF8.GetString(appStateSnapshotBytes.Span);

                _lastAppStateSnapshot = appStateSnapshotJsonString;

                if (appStateSnapshotJsonString is not null)
                {
                    var setStateResult =
                        _process.SetStateOnMainBranch(appStateSnapshotJsonString, _pineVM);

                    if (setStateResult.IsErrOrNull() is { } error)
                    {
                        logMessage($"Failed to set app state from snapshot: {error}");
                    }
                    else
                    {
                        logMessage("App state snapshot successfully deserialized and applied.");
                    }
                }
            }
            catch (JsonException ex)
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

    public async Task HandleRequestAsync(
        HttpContext context,
        Action<string>? logMessage = null)
    {
        await
            _publicAppState.HandleRequestAsync(context)
            .ContinueWith(
                task =>
                {
                    if (task.IsFaulted)
                    {
                        context.Response.StatusCode = 500;
                        context.Response.WriteAsync("Internal Server Error");
                    }
                    else if (task.IsCompletedSuccessfully)
                    {
                        context.Response.StatusCode = 200;
                    }

                    var newAppStateSnapshotResult = _process.GetAppStateOnMainBranch(_pineVM);

                    _pineVMCache.EvalCache.Clear();

                    if (newAppStateSnapshotResult.IsOkOrNull() is { } snapshotJsonString)
                    {
                        if (snapshotJsonString == _lastAppStateSnapshot)
                        {
                            return; // No change, no need to write to file store.
                        }

                        var snapshotJsonBytes = Encoding.UTF8.GetBytes(snapshotJsonString);

                        logMessage?.Invoke(
                            "App state snapshot updated, new size: " +
                            CommandLineInterface.FormatIntegerForDisplay(snapshotJsonString.Length) +
                            " bytes.");

                        _fileStore.SetFileContent(
                            s_appStateSnapshotFilePath,
                            fileContent: snapshotJsonBytes);

                        _lastAppStateSnapshot = snapshotJsonString;
                    }
                },
                TaskScheduler.Default);
    }
}
