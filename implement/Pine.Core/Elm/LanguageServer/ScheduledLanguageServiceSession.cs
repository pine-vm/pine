using Pine.Core.Concurrent;
using Pine.Core.Elm.LanguageServer.LanguageServiceInterface;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading.Tasks;

namespace Pine.Core.Elm.LanguageServer;

internal sealed class ScheduledLanguageServiceSession(
    LanguageServiceState.LanguageServiceProgram program,
    int maxConcurrencyCount,
    ScheduledLanguageServiceSession.Worker firstWorker,
    Func<ScheduledLanguageServiceSession.Worker> createWorker) : ILanguageServiceSession
{
    internal sealed record Worker(
        IPineVM PineVM,
        BufferedInvocationCacheAccess InvocationCache);

    private readonly RevisionedOperationScheduler<
        PineValue,
        Request,
        Result<string, Response>,
        Worker>
        _scheduler =
        new(
            initialState: program.InitialState,
            maxConcurrencyCount,
            createWorker:
            index =>
            index is 0
            ?
            firstWorker
            :
            createWorker(),
            execute:
            (worker, request, state, _) =>
            {
                var transition =
                    LanguageServiceState.ApplyRequest(
                        program,
                        worker.PineVM,
                        state,
                        request);

                return
                    ValueTask.FromResult(
                        new RevisionedOperationAttempt<
                            PineValue,
                            Result<string, Response>>(
                            transition.State,
                            transition.Response,
                            CanCompleteSpeculatively: state.Equals(transition.State)));
            },
            statesEqual: (left, right) => left.Equals(right),
            finalizeAttempt:
            worker =>
            {
                worker.InvocationCache.MergeIntoShared();
                return ValueTask.CompletedTask;
            });

    /// <inheritdoc/>
    public Result<string, Response.WorkspaceSummaryResponse> DeleteFile(
        string fileUri) =>
        ExpectResponse<Response.WorkspaceSummaryResponse>(
            HandleRequest(new Request.DeleteWorkspaceFileRequest(fileUri)));

    /// <inheritdoc/>
    public Result<string, Response.WorkspaceSummaryResponse> AddFile(
        string fileUri,
        string fileContentAsText) =>
        AddFileAsync(fileUri, fileContentAsText)
        .GetAwaiter()
        .GetResult();

    /// <inheritdoc/>
    public async Task<Result<string, Response.WorkspaceSummaryResponse>> AddFileAsync(
        string fileUri,
        string fileContentAsText,
        System.Threading.CancellationToken cancellationToken = default)
    {
        var asBase64 =
            Convert.ToBase64String(
                System.Text.Encoding.UTF8.GetBytes(fileContentAsText));

        return
            ExpectResponse<Response.WorkspaceSummaryResponse>(
                await HandleRequestAsync(
                    new Request.AddWorkspaceFileRequest(
                        fileUri,
                        new FileTreeBlobNode(AsBase64: asBase64, AsText: fileContentAsText)),
                    cancellationToken));
    }

    /// <inheritdoc/>
    public Result<string, Response.WorkspaceSummaryResponse> AddElmPackage(
        ElmPackageVersion019Identifer packageVersionId,
        IReadOnlyList<KeyValuePair<IReadOnlyList<string>, string>> filesContentsAsText) =>
        ExpectResponse<Response.WorkspaceSummaryResponse>(
            HandleRequest(
                new Request.AddElmPackageVersionRequest(
                    packageVersionId,
                    [
                    .. filesContentsAsText.Select(
                        entry =>
                        (entry.Key,
                        new FileTreeBlobNode(
                            AsBase64:
                            Convert.ToBase64String(
                                System.Text.Encoding.UTF8.GetBytes(entry.Value)),
                            AsText: entry.Value)))
                    ])));

    /// <inheritdoc/>
    public Result<string, Response> HandleRequest(Request request) =>
        HandleRequestAsync(request)
        .GetAwaiter()
        .GetResult();

    /// <inheritdoc/>
    public async Task<Result<string, Response>> HandleRequestAsync(
        Request request,
        System.Threading.CancellationToken cancellationToken = default) =>
        (await _scheduler
        .SubmitAsync(request, cancellationToken))
        .Result;

    private static Result<string, ResponseT> ExpectResponse<ResponseT>(
        Result<string, Response> responseResult)
        where ResponseT : Response
    {
        if (responseResult.IsErrOrNull() is { } err)
        {
            return err;
        }

        if (responseResult.IsOkOrNull() is not { } response)
        {
            throw new InvalidOperationException(
                "Unexpected language service response result type: " + responseResult.GetType());
        }

        if (response is not ResponseT expectedResponse)
        {
            throw new InvalidOperationException(
                "Unexpected language service response type: " + response.GetType());
        }

        return expectedResponse;
    }
}
