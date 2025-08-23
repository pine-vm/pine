using Pine.Core;
using Pine.Core.Elm;
using Pine.Core.PineVM;
using Pine.PineVM;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;

namespace Pine.Elm.Platform;

/// <summary>
/// Mutating container for a web service app.
/// </summary>
public class MutatingWebServiceApp
{
    private readonly System.Threading.Lock _stateLock = new();

    private record CacheAndVM(
        PineVMCache PineVMCache,
        PineVM.PineVM PineVM)
    {
        public static CacheAndVM Create()
        {
            var pineVMCache = new PineVMCache();
            var pineVM = new PineVM.PineVM(pineVMCache.EvalCache);

            return new CacheAndVM(pineVMCache, pineVM);
        }
    }

    private readonly CacheAndVM _cacheAndVM = CacheAndVM.Create();

    private readonly WebServiceInterface.WebServiceConfig _appConfig;

    private (PineValue appState, WebServiceInterface.Subscriptions subscriptions) _appStateAndSubscriptions;

    public PineValue AppState => _appStateAndSubscriptions.appState;

    private readonly ConcurrentQueue<ApplyUpdateReport<WebServiceInterface.Command>> _applyFunctionReports = new();

    public IReadOnlyList<ApplyUpdateReport<WebServiceInterface.Command>> DequeueApplyFunctionReports() =>
        [.. _applyFunctionReports.DequeueAllEnumerable()];

    public IPineVM PineVM => _cacheAndVM.PineVM;

    public long? PosixTimeSubscriptionMinimumTime =>
        _appStateAndSubscriptions.subscriptions?.PosixTimeIsPast?.MinimumPosixTimeMilli;

    public ElmTime.ElmTimeJsonAdapter.Parsed JsonAdapter => _appConfig.JsonAdapter;

    private readonly ConcurrentDictionary<string, WebServiceInterface.Command.RespondToHttpRequest> _httpResponses = new();

    private readonly ConcurrentQueue<WebServiceInterface.Command> _commands = new();

    public IReadOnlyList<WebServiceInterface.Command.RespondToHttpRequest> CopyHttpResponses() =>
        [.. _httpResponses.Values];

    public IReadOnlyList<WebServiceInterface.Command> DequeueCommands() =>
        [.. _commands.DequeueAllEnumerable()];

    public void ResetAppState(PineValue appState)
    {
        SetAppState(appState);
    }

    private void TrackAppliedFunction(
        ApplyUpdateReport<WebServiceInterface.Command> applyFunctionReport,
        bool discardIfResultingStateSame)
    {
        lock (_stateLock)
        {
            if (discardIfResultingStateSame && applyFunctionReport.ResponseState == AppState)
            {
                return;
            }

            _applyFunctionReports.Enqueue(applyFunctionReport);

            SetAppState(applyFunctionReport.ResponseState);
        }
    }

    private void SetAppState(PineValue appState)
    {
        lock (_stateLock)
        {
            var subscriptions =
                WebServiceInterface.WebServiceConfig.ParseSubscriptions(_appConfig, appState, _cacheAndVM.PineVM);

            _appStateAndSubscriptions = (appState, subscriptions);
        }
    }

    public MutatingWebServiceApp(
        WebServiceInterface.WebServiceConfig appConfig)
    {
        _appConfig = appConfig;

        ResetAppState(appConfig.Init.State);

        foreach (var (_, cmdParsed) in appConfig.Init.Commands)
        {
            _commands.Enqueue(cmdParsed);
        }
    }

    public ApplyUpdateReport<WebServiceInterface.Command> EventHttpRequest(
        WebServiceInterface.HttpRequestEventStruct httpRequest)
    {
        lock (_stateLock)
        {
            var eventConfig =
                WebServiceInterface.WebServiceConfig.EventHttpRequest(
                    _appStateAndSubscriptions.subscriptions,
                    httpRequest);

            return ApplyUpdate(eventConfig);
        }
    }

    public ApplyUpdateReport<WebServiceInterface.Command>? UpdateForPosixTime(long posixTimeMilli)
    {
        if (_appStateAndSubscriptions.subscriptions.PosixTimeIsPast is not { } posixTimeSub)
        {
            return null;
        }

        if (posixTimeMilli < posixTimeSub.MinimumPosixTimeMilli)
        {
            return null;
        }

        var functionRecord = posixTimeSub.Update;

        if (functionRecord.Parsed.ParameterCount is not 2)
        {
            throw new Exception("Expected posixTimeIsPast function to have two parameters.");
        }

        var inputEncoded =
            ElmValueEncoding.ElmValueAsPineValue(
                new ElmValue.ElmRecord(
                    [
                    ("currentPosixTimeMilli",
                    ElmValue.Integer(posixTimeMilli))
                    ]));

        return
            ApplyUpdate(
                functionRecord,
                [inputEncoded]);
    }

    public ApplyUpdateReport<WebServiceInterface.Command> ApplyUpdate(
        ApplyFunctionInput applyFunctionInput)
    {
        return
            ApplyUpdate(
                applyFunctionInput.AppliedFunction,
                applyFunctionInput.ArgsBeforeState);
    }

    public ApplyUpdateReport<WebServiceInterface.Command> ApplyUpdate(
        FunctionRecordValueAndParsed updateFunction,
        IReadOnlyList<PineValue> updateArgsBeforeState)
    {
        lock (_stateLock)
        {
            var eventResponse =
                WebServiceInterface.WebServiceConfig.ApplyUpdate(
                    updateFunction,
                    updateArgsBeforeState,
                    AppState,
                    _cacheAndVM.PineVM);

            TrackAppliedFunction(
                eventResponse,
                discardIfResultingStateSame: true);

            MutateConsolidatingAppResponse(eventResponse);

            {
                // TODO: Use overlap and warmup to reduce response delays.

                if (_cacheAndVM.PineVMCache.EvalCache.Count > 10_000)
                {
                    _cacheAndVM.PineVMCache.EvalCache.Clear();
                }
            }

            return eventResponse;
        }
    }

    private void MutateConsolidatingAppResponse(
        ApplyUpdateReport<WebServiceInterface.Command> eventResponse)
    {
        foreach (var (_, cmdParsed) in eventResponse.ResponseCommands)
        {
            if (cmdParsed is WebServiceInterface.Command.RespondToHttpRequest httpResponseCmd)
            {
                _httpResponses[httpResponseCmd.Respond.HttpRequestId] = httpResponseCmd;
            }
            else
            {
                _commands.Enqueue(cmdParsed);
            }
        }
    }

    public async System.Threading.Tasks.Task<WebServiceInterface.HttpResponse> HttpRequestSendAsync(
        WebServiceInterface.HttpRequestEventStruct httpRequest,
        System.Threading.CancellationToken cancellationToken = default)
    {
        return
            await System.Threading.Tasks.Task.Run(async () =>
            {
                EventHttpRequest(httpRequest);

                while (true)
                {
                    cancellationToken.ThrowIfCancellationRequested();

                    if (_httpResponses.TryRemove(
                        httpRequest.HttpRequestId,
                        out var httpResponse))
                    {
                        return httpResponse.Respond.Response;
                    }

                    await System.Threading.Tasks.Task.Delay(TimeSpan.FromMilliseconds(100));
                }
            });
    }
}
