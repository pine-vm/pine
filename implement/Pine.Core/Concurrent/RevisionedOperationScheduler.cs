using System;
using System.Threading;
using System.Threading.Channels;
using System.Threading.Tasks;

namespace Pine.Core.Concurrent;

/// <summary>
/// Result of one application operation attempt.
/// </summary>
public sealed record RevisionedOperationAttempt<TState, TResult>(
    TState State,
    TResult Result,
    bool CanCompleteSpeculatively);

/// <summary>
/// Accepted operation result and the canonical revision against which it was accepted.
/// </summary>
public sealed record RevisionedOperationResult<TResult>(
    TResult Result,
    long Revision,
    bool WasReplayed);

/// <summary>
/// Observable scheduler lifecycle event.
/// </summary>
public readonly record struct RevisionedOperationSchedulerEvent(
    long Sequence,
    long SourceRevision,
    long CurrentRevision,
    RevisionedOperationSchedulerEventKind Kind);

/// <summary>
/// Scheduler lifecycle event kinds.
/// </summary>
public enum RevisionedOperationSchedulerEventKind
{
    /// <summary>
    /// A speculative execution attempt began on a worker.
    /// </summary>
    SpeculationStarted,
    /// <summary>
    /// A speculative execution attempt produced a result.
    /// </summary>
    SpeculationCompleted,
    /// <summary>
    /// The speculative result was accepted without replay because the canonical revision still matched.
    /// </summary>
    SpeculationAccepted,
    /// <summary>
    /// A replay execution began against the current canonical state.
    /// </summary>
    ReplayStarted,
    /// <summary>
    /// A replay execution finished against the current canonical state.
    /// </summary>
    ReplayCompleted,
    /// <summary>
    /// A replay attempt updated the canonical state and advanced the revision.
    /// </summary>
    StateCommitted,
    /// <summary>
    /// The worker finalize hook completed and the attempt was ready to release its worker.
    /// </summary>
    AttemptFinalized,
    /// <summary>
    /// Processing for the scheduled operation ended because its cancellation token was triggered.
    /// </summary>
    Canceled,
    /// <summary>
    /// Processing for the scheduled operation ended with a non-cancellation exception.
    /// </summary>
    Faulted,
}

/// <summary>
/// Runs operations speculatively on a bounded worker pool and serializes acceptance
/// and replay against a canonical immutable state.
/// </summary>
public sealed class RevisionedOperationScheduler<TState, TOperation, TResult, TWorker> :
    IAsyncDisposable
{
    private readonly Lock _stateLock = new();

    private readonly Func<TWorker, TOperation, TState, CancellationToken,
        ValueTask<RevisionedOperationAttempt<TState, TResult>>>
        _execute;

    private readonly Func<TWorker, ValueTask>? _finalizeAttempt;

    private readonly Func<TState, TState, bool> _statesEqual;

    private readonly Action<RevisionedOperationSchedulerEvent>? _reportEvent;

    private readonly Channel<TWorker> _workers;

    private readonly TWorker[] _allWorkers;

    private readonly CancellationTokenSource _lifetimeCancellation = new();

    private TState _canonicalState;

    private long _revision;

    private long _nextSequence;

    private Task _previousOperation = Task.CompletedTask;

    private bool _disposed;

    /// <summary>
    /// Creates a scheduler and eagerly constructs its exclusive worker pool.
    /// </summary>
    public RevisionedOperationScheduler(
        TState initialState,
        int maxConcurrencyCount,
        Func<int, TWorker> createWorker,
        Func<TWorker, TOperation, TState, CancellationToken,
            ValueTask<RevisionedOperationAttempt<TState, TResult>>> execute,
        Func<TState, TState, bool> statesEqual,
        Func<TWorker, ValueTask>? finalizeAttempt = null,
        Action<RevisionedOperationSchedulerEvent>? reportEvent = null)
    {
        ArgumentOutOfRangeException.ThrowIfLessThan(maxConcurrencyCount, 1);
        ArgumentNullException.ThrowIfNull(createWorker);
        ArgumentNullException.ThrowIfNull(execute);
        ArgumentNullException.ThrowIfNull(statesEqual);

        _canonicalState = initialState;
        _execute = execute;
        _statesEqual = statesEqual;
        _finalizeAttempt = finalizeAttempt;
        _reportEvent = reportEvent;

        _workers =
            Channel.CreateBounded<TWorker>(
                new BoundedChannelOptions(maxConcurrencyCount)
                {
                    FullMode = BoundedChannelFullMode.Wait,
                    SingleReader = false,
                    SingleWriter = false,
                });

        var workers = new TWorker[maxConcurrencyCount];

        for (var index = 0; index < workers.Length; index++)
        {
            workers[index] = createWorker(index);

            if (!_workers.Writer.TryWrite(workers[index]))
            {
                throw new InvalidOperationException("Failed to initialize the worker pool.");
            }
        }

        _allWorkers = workers;
    }

    /// <summary>
    /// Gets a snapshot of the current canonical state and revision.
    /// </summary>
    public (TState State, long Revision) GetStateSnapshot()
    {
        lock (_stateLock)
        {
            return (_canonicalState, _revision);
        }
    }

    /// <summary>
    /// Submits an operation for speculative execution and ordered acceptance or replay.
    /// </summary>
    public Task<RevisionedOperationResult<TResult>> SubmitAsync(
        TOperation operation,
        CancellationToken cancellationToken = default)
    {
        lock (_stateLock)
        {
            ObjectDisposedException.ThrowIf(_disposed, this);

            var sequence = _nextSequence++;
            var sourceRevision = _revision;
            var sourceState = _canonicalState;
            var previousOperation = _previousOperation;

            var orderedCompletion =
                new TaskCompletionSource(TaskCreationOptions.RunContinuationsAsynchronously);

            var resultCompletion =
                new TaskCompletionSource<RevisionedOperationResult<TResult>>(
                    TaskCreationOptions.RunContinuationsAsynchronously);

            _previousOperation = orderedCompletion.Task;

            _ =
                Task.Run(
                    () =>
                    ProcessOperationAsync(
                        sequence,
                        sourceRevision,
                        sourceState,
                        operation,
                        previousOperation,
                        orderedCompletion,
                        resultCompletion,
                        cancellationToken));

            return resultCompletion.Task;
        }
    }

    private async Task ProcessOperationAsync(
        long sequence,
        long sourceRevision,
        TState sourceState,
        TOperation operation,
        Task previousOperation,
        TaskCompletionSource orderedCompletion,
        TaskCompletionSource<RevisionedOperationResult<TResult>> resultCompletion,
        CancellationToken cancellationToken)
    {
        using var operationCancellation =
            CancellationTokenSource.CreateLinkedTokenSource(
                cancellationToken,
                _lifetimeCancellation.Token);

        try
        {
            var speculativeAttempt =
                await ExecuteAttemptAsync(
                    sequence,
                    sourceRevision,
                    operation,
                    sourceState,
                    isReplay: false,
                    operationCancellation.Token);

            await previousOperation.ConfigureAwait(false);
            operationCancellation.Token.ThrowIfCancellationRequested();

            lock (_stateLock)
            {
                if (sourceRevision == _revision &&
                    speculativeAttempt.CanCompleteSpeculatively &&
                    _statesEqual(sourceState, speculativeAttempt.State))
                {
                    ReportEvent(
                        sequence,
                        sourceRevision,
                        RevisionedOperationSchedulerEventKind.SpeculationAccepted);

                    resultCompletion.SetResult(
                        new RevisionedOperationResult<TResult>(
                            speculativeAttempt.Result,
                            _revision,
                            WasReplayed: false));

                    return;
                }
            }

            TState replayState;
            long replayRevision;

            lock (_stateLock)
            {
                replayState = _canonicalState;
                replayRevision = _revision;
            }

            var replayAttempt =
                await ExecuteAttemptAsync(
                    sequence,
                    replayRevision,
                    operation,
                    replayState,
                    isReplay: true,
                    operationCancellation.Token);

            operationCancellation.Token.ThrowIfCancellationRequested();

            lock (_stateLock)
            {
                if (!_statesEqual(_canonicalState, replayAttempt.State))
                {
                    _canonicalState = replayAttempt.State;
                    _revision++;

                    ReportEvent(
                        sequence,
                        replayRevision,
                        RevisionedOperationSchedulerEventKind.StateCommitted);
                }

                resultCompletion.SetResult(
                    new RevisionedOperationResult<TResult>(
                        replayAttempt.Result,
                        _revision,
                        WasReplayed: true));
            }
        }
        catch (OperationCanceledException ex)
        {
            await previousOperation.ConfigureAwait(false);

            ReportEvent(
                sequence,
                sourceRevision,
                RevisionedOperationSchedulerEventKind.Canceled);

            resultCompletion.TrySetCanceled(ex.CancellationToken);
        }
        catch (Exception ex)
        {
            await previousOperation.ConfigureAwait(false);

            ReportEvent(
                sequence,
                sourceRevision,
                RevisionedOperationSchedulerEventKind.Faulted);

            resultCompletion.TrySetException(ex);
        }
        finally
        {
            orderedCompletion.TrySetResult();
        }
    }

    private async ValueTask<RevisionedOperationAttempt<TState, TResult>> ExecuteAttemptAsync(
        long sequence,
        long sourceRevision,
        TOperation operation,
        TState state,
        bool isReplay,
        CancellationToken cancellationToken)
    {
        var worker = await _workers.Reader.ReadAsync(cancellationToken).ConfigureAwait(false);

        try
        {
            ReportEvent(
                sequence,
                sourceRevision,
                isReplay
                ?
                RevisionedOperationSchedulerEventKind.ReplayStarted
                :
                RevisionedOperationSchedulerEventKind.SpeculationStarted);

            var attempt =
                await _execute(worker, operation, state, cancellationToken)
                .ConfigureAwait(false);

            ReportEvent(
                sequence,
                sourceRevision,
                isReplay
                ?
                RevisionedOperationSchedulerEventKind.ReplayCompleted
                :
                RevisionedOperationSchedulerEventKind.SpeculationCompleted);

            return attempt;
        }
        finally
        {
            try
            {
                if (_finalizeAttempt is not null)
                {
                    await _finalizeAttempt(worker).ConfigureAwait(false);
                }

                ReportEvent(
                    sequence,
                    sourceRevision,
                    RevisionedOperationSchedulerEventKind.AttemptFinalized);
            }
            finally
            {
                await _workers.Writer.WriteAsync(worker, CancellationToken.None).ConfigureAwait(false);
            }
        }
    }

    private void ReportEvent(
        long sequence,
        long sourceRevision,
        RevisionedOperationSchedulerEventKind kind)
    {
        if (_reportEvent is not { } reportEvent)
        {
            return;
        }

        long currentRevision;

        lock (_stateLock)
        {
            currentRevision = _revision;
        }

        reportEvent(
            new RevisionedOperationSchedulerEvent(
                sequence,
                sourceRevision,
                currentRevision,
                kind));
    }

    /// <inheritdoc/>
    public async ValueTask DisposeAsync()
    {
        Task pendingOperations;

        lock (_stateLock)
        {
            if (_disposed)
            {
                return;
            }

            _disposed = true;
            _lifetimeCancellation.Cancel();
            pendingOperations = _previousOperation;
        }

        await pendingOperations.ConfigureAwait(false);

        _workers.Writer.Complete();

        foreach (var worker in _allWorkers)
        {
            if (worker is IAsyncDisposable asyncDisposable)
            {
                await asyncDisposable.DisposeAsync().ConfigureAwait(false);
            }
            else if (worker is IDisposable disposable)
            {
                disposable.Dispose();
            }
        }

        _lifetimeCancellation.Dispose();
    }
}
