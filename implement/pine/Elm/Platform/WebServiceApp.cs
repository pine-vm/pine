using Pine.Core;
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
    private readonly PineVMCache pineVMCache = new();

    private readonly PineVM.PineVM pineVM;

    private readonly WebServiceInterface.WebServiceConfig appConfig;

    private PineValue appState;

    private readonly ConcurrentQueue<WebServiceInterface.Command> commands = new();

    public IReadOnlyList<WebServiceInterface.Command> DequeueCommands() =>
        [.. commands.DequeueAllEnumerable()];

    public MutatingWebServiceApp(
        WebServiceInterface.WebServiceConfig appConfig)
    {
        this.appConfig = appConfig;

        pineVM = new PineVM.PineVM(pineVMCache.EvalCache);

        appState = appConfig.Init.State;

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
                appConfig,
                httpRequest,
                appState,
                pineVM);

        appState = eventResponse.State;

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
