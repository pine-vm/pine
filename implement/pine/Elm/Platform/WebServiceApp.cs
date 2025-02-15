using ElmTime.ElmInteractive;
using Pine.Core;
using Pine.Core.PineVM;
using Pine.PineVM;
using System.Collections.Concurrent;
using System.Collections.Generic;

namespace Pine.Elm.Platform;

/// <summary>
/// Mutating container for a web service app.
/// </summary>
public class MutatingWebServiceApp
{
    private readonly PineVMCache pineVMCache = new();

    private readonly PineVM.PineVM pineVM;

    private readonly WebServiceInterface.WebServiceConfig appConfig;

    private (PineValue appState, WebServiceInterface.Subscriptions subscriptions) appStateAndSubscriptions;

    public PineValue AppState => appStateAndSubscriptions.appState;

    public IPineVM PineVM => pineVM;

    public ElmTime.ElmTimeJsonAdapter.Parsed JsonAdapter => appConfig.JsonAdapter;

    private readonly ConcurrentQueue<WebServiceInterface.Command> commands = new();

    public IReadOnlyList<WebServiceInterface.Command> DequeueCommands() =>
        [.. commands.DequeueAllEnumerable()];

    public void SetAppState(PineValue appState)
    {
        var subscriptions =
            WebServiceInterface.WebServiceConfig.ParseSubscriptions(appConfig, appState, pineVM);

        appStateAndSubscriptions = (appState, subscriptions);
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

    public WebServiceInterface.WebServiceEventResponse ApplyUpdate(
        ElmInteractiveEnvironment.FunctionRecord updateFunction,
        IReadOnlyList<PineValue> updateArgsBeforeState)
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

    private void MutateConsolidatingAppResponse(WebServiceInterface.WebServiceEventResponse eventResponse)
    {
        foreach (var cmd in eventResponse.Commands)
        {
            commands.Enqueue(cmd);
        }
    }
}
