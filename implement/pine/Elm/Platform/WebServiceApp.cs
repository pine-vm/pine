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
    private readonly System.Threading.Lock stateLock = new();

    private readonly PineVMCache pineVMCache = new();

    private readonly PineVM.PineVM pineVM;

    private readonly WebServiceInterface.WebServiceConfig appConfig;

    private (PineValue appState, WebServiceInterface.Subscriptions subscriptions) appStateAndSubscriptions;

    public PineValue AppState => appStateAndSubscriptions.appState;

    private readonly ConcurrentQueue<ApplyUpdateReport<WebServiceInterface.Command>> applyFunctionReports = new();

    public IReadOnlyList<ApplyUpdateReport<WebServiceInterface.Command>> DequeueApplyFunctionReports() =>
        [.. applyFunctionReports.DequeueAllEnumerable()];

    public IPineVM PineVM => pineVM;

    public long? PosixTimeSubscriptionMinimumTime =>
        appStateAndSubscriptions.subscriptions?.PosixTimeIsPast?.MinimumPosixTimeMilli;

    public ElmTime.ElmTimeJsonAdapter.Parsed JsonAdapter => appConfig.JsonAdapter;

    private readonly ConcurrentDictionary<string, WebServiceInterface.Command.RespondToHttpRequest> httpResponses = new();

    private readonly ConcurrentQueue<WebServiceInterface.Command> commands = new();

    public IReadOnlyList<WebServiceInterface.Command.RespondToHttpRequest> CopyHttpResponses() =>
        [.. httpResponses.Values];

    public IReadOnlyList<WebServiceInterface.Command> DequeueCommands() =>
        [.. commands.DequeueAllEnumerable()];

    public void ResetAppState(PineValue appState)
    {
        SetAppState(appState);
    }

    private void TrackAppliedFunction(
        ApplyUpdateReport<WebServiceInterface.Command> applyFunctionReport,
        bool discardIfResultingStateSame)
    {
        lock (stateLock)
        {
            if (discardIfResultingStateSame && applyFunctionReport.ResponseState == AppState)
            {
                return;
            }

            applyFunctionReports.Enqueue(applyFunctionReport);

            SetAppState(applyFunctionReport.ResponseState);
        }
    }

    private void SetAppState(PineValue appState)
    {
        lock (stateLock)
        {
            var subscriptions =
                WebServiceInterface.WebServiceConfig.ParseSubscriptions(appConfig, appState, pineVM);

            appStateAndSubscriptions = (appState, subscriptions);
        }
    }

    public MutatingWebServiceApp(
        WebServiceInterface.WebServiceConfig appConfig)
    {
        this.appConfig = appConfig;

        pineVM = new PineVM.PineVM(pineVMCache.EvalCache);

        ResetAppState(appConfig.Init.State);

        foreach (var (_, cmdParsed) in appConfig.Init.Commands)
        {
            commands.Enqueue(cmdParsed);
        }
    }

    public ApplyUpdateReport<WebServiceInterface.Command> EventHttpRequest(
        WebServiceInterface.HttpRequestEventStruct httpRequest)
    {
        lock (stateLock)
        {
            var eventConfig =
                WebServiceInterface.WebServiceConfig.EventHttpRequest(
                    appStateAndSubscriptions.subscriptions,
                    httpRequest);

            return ApplyUpdate(eventConfig);
        }
    }

    public ApplyUpdateReport<WebServiceInterface.Command>? UpdateForPosixTime(long posixTimeMilli)
    {
        if (appStateAndSubscriptions.subscriptions.PosixTimeIsPast is not { } posixTimeSub)
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
        lock (stateLock)
        {
            var eventResponse =
                WebServiceInterface.WebServiceConfig.ApplyUpdate(
                    updateFunction,
                    updateArgsBeforeState,
                    AppState,
                    pineVM);

            TrackAppliedFunction(
                eventResponse,
                discardIfResultingStateSame: true);

            MutateConsolidatingAppResponse(eventResponse);

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
                httpResponses[httpResponseCmd.Respond.HttpRequestId] = httpResponseCmd;
            }
            else
            {
                commands.Enqueue(cmdParsed);
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

                    if (httpResponses.TryRemove(
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
