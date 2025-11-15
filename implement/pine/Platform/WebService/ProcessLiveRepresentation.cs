using ElmTime.Platform.WebService.ProcessStoreSupportingMigrations;
using Pine.Core;
using Pine.Elm.Platform;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Text.Json;

namespace ElmTime.Platform.WebService;

public interface IProgressWriter
{
    void FunctionApplications(IReadOnlyList<ApplyUpdateReport<WebServiceInterface.Command>> reports);

    void StoreReduction(PineValue appState);
}

public class DiscardingProgressWriter : IProgressWriter
{
    public void FunctionApplications(IReadOnlyList<ApplyUpdateReport<WebServiceInterface.Command>> reports)
    {
    }

    public void StoreReduction(PineValue appState)
    {
    }
}

record DelegatingProgressWriter(
    Action<IReadOnlyList<ApplyUpdateReport<WebServiceInterface.Command>>> DelegateFunctionApplications,
    Action<PineValue> DelegateStoreReduction)
    : IProgressWriter
{
    public void FunctionApplications(IReadOnlyList<ApplyUpdateReport<WebServiceInterface.Command>> reports) =>
        DelegateFunctionApplications(reports);

    public void StoreReduction(PineValue appState) =>
        DelegateStoreReduction(appState);
}

public sealed class ProcessLiveRepresentation : IAsyncDisposable
{
    private readonly System.Threading.Lock _processLock = new();

    private readonly Func<DateTimeOffset> _getDateTimeOffset;

    private readonly WebServiceInterface.WebServiceConfig _appConfigParsed;

    private readonly MutatingWebServiceApp _mutatingWebServiceApp;

    private readonly VolatileProcessHost _volatileProcessHost;

    private readonly IProgressWriter _progressWriter;

    private readonly System.Threading.Timer _notifyTimeHasArrivedTimer;

    public record struct CompositionLogRecordWithResolvedDependencies(
        CompositionLogRecordInFile CompositionRecord,
        string CompositionRecordHashBase16,
        ReductionWithResolvedDependencies? Reduction,
        CompositionEventWithResolvedDependencies? Composition);

    public record struct ReductionWithResolvedDependencies(
        PineValue ElmAppState,
        PineValue AppConfig,
        BlobTreeWithStringPath AppConfigAsTree);

    public record struct CompositionEventWithResolvedDependencies(
        PineValue? SetElmAppState = null,
        BlobTreeWithStringPath? DeployAppConfigAndInitElmAppState = null,
        BlobTreeWithStringPath? DeployAppConfigAndMigrateElmAppState = null,
        ApplyFunctionOnLiteralsAndStateEvent? ApplyFunctionOnLiteralsAndState = null);


    public record ApplyFunctionOnLiteralsAndStateEvent(
        PineValue Function,
        PineValue Arguments);

    public static ProcessLiveRepresentation Create(
        WebServiceInterface.WebServiceConfig webServiceConfig,
        PineValue? lastAppState,
        IProgressWriter progressWriter,
        Func<DateTimeOffset> getDateTimeOffset,
        IEnumerable<PineValue> artifactSourceCompositions,
        System.Threading.CancellationToken cancellationToken = default)
    {
        return new ProcessLiveRepresentation(
            webServiceConfig,
            lastAppState,
            progressWriter,
            getDateTimeOffset,
            artifactSourceCompositions,
            cancellationToken);
    }

    private ProcessLiveRepresentation(
        WebServiceInterface.WebServiceConfig webServiceConfig,
        PineValue? lastAppState,
        IProgressWriter progressWriter,
        Func<DateTimeOffset> getDateTimeOffset,
        IEnumerable<PineValue> artifactSourceCompositions,
        System.Threading.CancellationToken cancellationToken = default)
    {
        _getDateTimeOffset = getDateTimeOffset;
        _progressWriter = progressWriter;

        _appConfigParsed = webServiceConfig;

        _mutatingWebServiceApp =
            new MutatingWebServiceApp(_appConfigParsed);

        _volatileProcessHost =
            new VolatileProcessHost([.. artifactSourceCompositions]);

        if (lastAppState is not null)
        {
            _mutatingWebServiceApp.ResetAppState(lastAppState);
        }

        _notifyTimeHasArrivedTimer = new System.Threading.Timer(
            callback: _ =>
            {
                if (cancellationToken.IsCancellationRequested)
                {
                    _notifyTimeHasArrivedTimer?.Dispose();
                    return;
                }

                ProcessElmAppCmdsAsync().Wait();

                ProcessEventTimeHasArrived(getDateTimeOffset());
            },
            state: null,
            dueTime: TimeSpan.Zero,
            period: TimeSpan.FromMilliseconds(100));

        ProcessEventTimeHasArrived(getDateTimeOffset());
    }

    public async System.Threading.Tasks.Task<WebServiceInterface.HttpResponse> ProcessHttpRequestAsync(
        WebServiceInterface.HttpRequestEventStruct requestEvent)
    {
        var response =
            await _mutatingWebServiceApp.HttpRequestSendAsync(requestEvent);

        await ProcessElmAppCmdsAsync();

        return response;
    }

    public void ProcessEventTimeHasArrived(DateTimeOffset currentTime)
    {
        _mutatingWebServiceApp.UpdateForPosixTime(
            posixTimeMilli: currentTime.ToUnixTimeMilliseconds());
    }

    private async System.Threading.Tasks.Task ProcessElmAppCmdsAsync()
    {
        EnsurePersisted();

        await _volatileProcessHost.ExchangeAsync(_mutatingWebServiceApp);
    }

    private void EnsurePersisted()
    {
        _progressWriter.FunctionApplications(_mutatingWebServiceApp.DequeueApplyFunctionReports());
    }

    private record struct StoreAppStateResetAndReductionReport(
        DateTimeOffset Time,
        PineValue ElmAppState,
        CompositionLogRecordInFile.CompositionEvent CompositionLogEvent,
        string RecordHashBase16,
        System.Threading.Tasks.Task TaskStoringReduction);

    public int ResetAppStateIgnoringTypeChecking(
        PineValue appState)
    {
        lock (_processLock)
        {
            _mutatingWebServiceApp.ResetAppState(appState);

            return 0;
        }
    }

    public PineValue GetAppStateOnMainBranch()
    {
        return _mutatingWebServiceApp.AppState;
    }

    public Result<string, JsonElement> GetAppStateOnMainBranchAsJson()
    {
        var pineVMCache = new PineVMCache();

        var pineVM = new PineVM(pineVMCache.EvalCache);

        return
            GetAppStateOnMainBranchAsJson(pineVM)
            .Map(jsonString => JsonSerializer.Deserialize<JsonElement>(jsonString));
    }

    public Result<string, string> GetAppStateOnMainBranchAsJson(
        PineVM pineVM)
    {
        var appState = _mutatingWebServiceApp.AppState;

        var jsonStringResult =
            _appConfigParsed.JsonAdapter.EncodeAppStateAsJsonString(appState, pineVM);

        {
            if (jsonStringResult.IsErrOrNull() is { } err)
            {
                return Result<string, string>.err(err);
            }
        }

        if (jsonStringResult.IsOkOrNull() is not { } jsonString)
        {
            throw new Exception("Unexpected result: " + jsonStringResult);
        }

        return Result<string, string>.ok(jsonString);
    }

    public async System.Threading.Tasks.ValueTask DisposeAsync()
    {
        await _notifyTimeHasArrivedTimer.DisposeAsync();

        await _volatileProcessHost.DisposeAsync();
    }
}
