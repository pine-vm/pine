using ElmTime.ElmInteractive;
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

    public IPineVM PineVM => pineVM;

    public long? PosixTimeSubscriptionMinimumTime =>
        appStateAndSubscriptions.subscriptions?.PosixTimeIsPast?.MinimumPosixTimeMilli;

    public ElmTime.ElmTimeJsonAdapter.Parsed JsonAdapter => appConfig.JsonAdapter;

    private readonly ConcurrentQueue<WebServiceInterface.Command.RespondToHttpRequest> httpResponses = new();

    private readonly ConcurrentQueue<WebServiceInterface.Command> commands = new();

    public IReadOnlyList<WebServiceInterface.Command.RespondToHttpRequest> CopyHttpResponses() =>
        [.. httpResponses];

    public IReadOnlyList<WebServiceInterface.Command> DequeueCommands() =>
        [.. commands.DequeueAllEnumerable()];

    public void ResetAppState(PineValue appState)
    {
        SetAppState(appState);
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

        SetAppState(appConfig.Init.State);

        foreach (var command in appConfig.Init.Commands)
        {
            commands.Enqueue(command);
        }
    }

    public WebServiceInterface.WebServiceEventResponse EventHttpRequest(
        WebServiceInterface.HttpRequestEventStruct httpRequest)
    {
        lock (stateLock)
        {
            var eventResponse =
                WebServiceInterface.WebServiceConfig.EventHttpRequest(
                    appStateAndSubscriptions.subscriptions,
                    httpRequest,
                    AppState,
                    pineVM);

            SetAppState(eventResponse.State);

            MutateConsolidatingAppResponse(eventResponse);

            return eventResponse;
        }
    }

    public WebServiceInterface.WebServiceEventResponse? UpdateForPosixTime(
        long posixTimeMilli)
    {
        lock (stateLock)
        {
            var eventResponse =
                WebServiceInterface.WebServiceConfig.EventPosixTime(
                    subscriptions: appStateAndSubscriptions.subscriptions,
                    posixTimeMilli,
                    AppState,
                    pineVM);

            if (eventResponse is not null)
            {
                SetAppState(eventResponse.State);
                MutateConsolidatingAppResponse(eventResponse);
            }

            return eventResponse;
        }
    }

    public WebServiceInterface.WebServiceEventResponse ApplyUpdate(
        ElmInteractiveEnvironment.FunctionRecord updateFunction,
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

            SetAppState(eventResponse.State);

            MutateConsolidatingAppResponse(eventResponse);

            return eventResponse;
        }
    }

    private void MutateConsolidatingAppResponse(WebServiceInterface.WebServiceEventResponse eventResponse)
    {
        foreach (var cmd in eventResponse.Commands)
        {
            if (cmd is WebServiceInterface.Command.RespondToHttpRequest httpResponseCmd)
            {
                httpResponses.Enqueue(httpResponseCmd);
            }
            else
            {
                commands.Enqueue(cmd);
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

                    foreach (var httpResponse in httpResponses)
                    {
                        if (httpResponse.Respond.HttpRequestId == httpRequest.HttpRequestId)
                        {
                            return httpResponse.Respond.Response;
                        }
                    }

                    await System.Threading.Tasks.Task.Delay(TimeSpan.FromMilliseconds(100));
                }
            });
    }
}
