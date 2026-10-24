using Pine.Core.Addressing;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Globalization;
using System.Linq;
using System.Threading;
using System.Threading.Channels;
using System.Threading.Tasks;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Bounds and scheduling for continuous invocation-cache learning.
/// </summary>
public sealed record ContinuousInvocationCacheLearningOptions(
    TimeSpan? MinimumInterval = null,
    int SampleCapacity = 16,
    int PendingCapacity = 32);

/// <summary>
/// A consistent observation of the controller, excluding pending evaluation inputs.
/// </summary>
public sealed record ContinuousInvocationCacheLearningSnapshot(
    InvocationCacheLearningState State,
    int SampleCount,
    int PendingCount,
    long AcceptedCount,
    long DroppedCount,
    long RunCount,
    long SwitchCount,
    bool IsLearning,
    bool IsDisposed);

/// <summary>
/// Samples successful evaluations and serializes bounded, background learning rounds.
/// Existing VM caches are not owned or replaced by this controller.
/// </summary>
public sealed class ContinuousInvocationCacheLearning : IAsyncDisposable, IDisposable
{
    private readonly object _gate = new();

    private readonly Func<InvocationCacheLearningState, IReadOnlyList<InvocationCacheLearningRequest>,
        CancellationToken, Task<InvocationCacheLearningState>>
        _learn;

    private readonly Action<string>? _log;

    private readonly Channel<InvocationCacheLearningRequest> _pending;

    private readonly CancellationTokenSource _stop = new();

    private readonly TimeSpan _interval;

    private readonly int _sampleCapacity;

    private readonly int _pendingCapacity;

    private readonly Task _pump;

    private readonly long _startedAt = Stopwatch.GetTimestamp();

    private readonly IDisposable? _learningResource;

    private readonly TaskCompletionSource _resourceDisposed = new(TaskCreationOptions.RunContinuationsAsynchronously);

    private InvocationCacheLearningState _state;

    private int _resourceDisposalStarted;

    private int _sampleCount;

    private long _acceptedCount;

    private long _droppedCount;

    private long _runCount;

    private long _switchCount;

    private long? _lastSwitchAt;

    private bool _isLearning;

    private bool _disposed;

    /// <summary>
    /// Starts a single background pump. Learning never runs on evaluation callers.
    /// Takes ownership of the optional learning resource, including on initialization failure.
    /// </summary>
    public ContinuousInvocationCacheLearning(
        Func<InvocationCacheLearningState, IReadOnlyList<InvocationCacheLearningRequest>,
            CancellationToken, Task<InvocationCacheLearningState>> learn,
        Action<string>? log = null,
        ContinuousInvocationCacheLearningOptions? options = null,
        InvocationCacheLearningState? initialState = null,
        IDisposable? learningResource = null)
    {
        _log = log;
        _learningResource = learningResource;

        try
        {
            ArgumentNullException.ThrowIfNull(learn);
            options ??= new();
            var interval = options.MinimumInterval ?? TimeSpan.FromSeconds(30);

            if (interval < TimeSpan.FromMilliseconds(1) || interval.TotalMilliseconds > uint.MaxValue - 1)
            {
                throw new ArgumentOutOfRangeException(
                    nameof(options),
                    "The interval must be at least one millisecond and supported by a timer.");
            }

            if (options.SampleCapacity is < 2 or > 16 || options.PendingCapacity is < 1 or > 32)
            {
                throw new ArgumentOutOfRangeException(
                    nameof(options),
                    "Sample capacity must be 2..16 and pending capacity 1..32.");
            }

            _learn = learn;
            _state = initialState ?? InvocationCacheLearningState.Initial;
            _sampleCapacity = options.SampleCapacity;
            _pendingCapacity = options.PendingCapacity;

            _pending =
                Channel.CreateBounded<InvocationCacheLearningRequest>(
                    new BoundedChannelOptions(options.PendingCapacity)
                    {
                        SingleReader = true,
                        SingleWriter = false,
                        FullMode = BoundedChannelFullMode.Wait,
                        AllowSynchronousContinuations = false
                    });

            _interval = interval;
            _pump = Task.Run(RunAsync);
        }
        catch
        {
            DisposeLearningResource();
            _stop.Dispose();
            throw;
        }
    }

    /// <summary>
    /// The atomically published policy. Hosts should capture this once per VM.
    /// </summary>
    public InvocationCachePolicy CurrentPolicy => State.Policy;

    /// <summary>
    /// The last completed learning state.
    /// </summary>
    public InvocationCacheLearningState State => Volatile.Read(ref _state);

    /// <summary>
    /// Returns bounded sample counts and cumulative activity counters.
    /// </summary>
    public ContinuousInvocationCacheLearningSnapshot GetSnapshot()
    {
        lock (_gate)
        {
            return
                new(
                    _state,
                    _sampleCount,
                    _pending.Reader.Count,
                    Interlocked.Read(ref _acceptedCount),
                    Interlocked.Read(ref _droppedCount),
                    _runCount,
                    _switchCount,
                    _isLearning,
                    _disposed);
        }
    }

    /// <summary>
    /// Enqueues without waiting for capacity or learning. Returns false when full or stopped.
    /// </summary>
    public bool Record(InvocationCacheLearningRequest request)
    {
        ArgumentNullException.ThrowIfNull(request);

        if (_pending.Writer.TryWrite(request))
        {
            Interlocked.Increment(ref _acceptedCount);
            return true;
        }

        Interlocked.Increment(ref _droppedCount);
        return false;
    }

    /// <summary>
    /// Records only successful evaluations and preserves a VM's cancellation capability.
    /// </summary>
    public ICancellablePineVM Wrap(IPineVM vm)
    {
        ArgumentNullException.ThrowIfNull(vm);
        return new RecordingVM(this, vm);
    }

    private async Task RunAsync()
    {
        var sample = new Queue<InvocationCacheLearningRequest>(_sampleCapacity);
        var continueLearning = false;
        var remainingCandidates = -1;

        try
        {
            var initialPolicy = State.Policy;

            SafeLog(
                $"Invocation-cache learning configured: policy={initialPolicy.Id}, expressions={initialPolicy.Expressions.Count}, " +
                $"interval-ms={Milliseconds(_interval)}, sample-capacity={_sampleCapacity}, pending-capacity={_pendingCapacity}");

            while (true)
            {
                await Task.Delay(_interval, _stop.Token).ConfigureAwait(false);
                var received = false;

                // A bounded drain also prevents continuous producers from starving learning.
                var pendingCount = _pending.Reader.Count;

                for (var index = 0; index < pendingCount && _pending.Reader.TryRead(out var request); ++index)
                {
                    if (sample.Count == _sampleCapacity)
                        sample.Dequeue();

                    sample.Enqueue(request);
                    received = true;
                }

                if (received)
                    remainingCandidates = -1;

                InvocationCacheLearningState before;
                long run;

                lock (_gate)
                {
                    _sampleCount = sample.Count;

                    if (_disposed)
                        return;

                    if (sample.Count < 2 || (!received && !continueLearning))
                        continue;

                    before = _state;
                    run = ++_runCount;
                    _isLearning = true;
                }

                var startedAt = Stopwatch.GetTimestamp();

                try
                {
                    var next = await _learn(before, [.. sample], _stop.Token).ConfigureAwait(false);
                    ArgumentNullException.ThrowIfNull(next);
                    ArgumentNullException.ThrowIfNull(next.Policy);
                    PolicySwitch? policySwitch = null;

                    lock (_gate)
                    {
                        if (_disposed)
                            return;

                        var changed = !before.Policy.Equals(next.Policy);
                        Volatile.Write(ref _state, next);
                        var candidateCount = next.KnownExpressions.IsDefaultOrEmpty ? 0 : next.KnownExpressions.Length;

                        if (changed)
                        {
                            remainingCandidates = candidateCount;
                            continueLearning = true;
                        }
                        else if (next.LastRun is { } evidence)
                        {
                            // The learner's cursor wraps. Count inspected neighbors instead of
                            // mistaking a wrapped cursor for an unfinished, infinite search.
                            remainingCandidates =
                                Math.Max(
                                    0,
                                    (remainingCandidates < 0 ? candidateCount : remainingCandidates) -
                                    evidence.EvaluatedCandidateCount);

                            continueLearning = remainingCandidates > 0 && evidence.EvaluatedCandidateCount > 0;
                        }
                        else
                        {
                            continueLearning =
                                next.NextCandidateIndex > before.NextCandidateIndex &&
                                next.NextCandidateIndex < candidateCount;
                        }

                        if (changed)
                        {
                            var switchedAt = Stopwatch.GetTimestamp();

                            policySwitch =
                                new(
                                    before,
                                    next,
                                    ++_switchCount,
                                    run,
                                    startedAt,
                                    switchedAt,
                                    _lastSwitchAt,
                                    Interlocked.Read(ref _acceptedCount),
                                    Interlocked.Read(ref _droppedCount));

                            _lastSwitchAt = switchedAt;
                        }
                    }

                    if (policySwitch is { } change)
                        LogSwitch(change);
                }
                catch (OperationCanceledException) when (_stop.IsCancellationRequested)
                {
                    return;
                }
                catch (Exception exception)
                {
                    continueLearning = false;
                    SafeLog($"Invocation-cache learning failed: run={run}, exception={exception.GetType().Name}");
                }
                finally
                {
                    lock (_gate)
                    {
                        _isLearning = false;
                    }
                }
            }
        }
        catch (OperationCanceledException) when (_stop.IsCancellationRequested)
        {
        }
        catch (Exception exception)
        {
            SafeLog($"Invocation-cache learning stopped: exception={exception.GetType().Name}");
        }
        finally
        {
            lock (_gate)
            {
                _disposed = true;
                _pending.Writer.TryComplete();

                while (_pending.Reader.TryRead(out _))
                {
                }

                _stop.Dispose();
            }

            DisposeLearningResource();
        }
    }

    private void LogSwitch(PolicySwitch change)
    {
        var sinceLastSwitch =
            change.PreviousSwitchAt is { } lastSwitch
            ?
            Milliseconds(Stopwatch.GetElapsedTime(lastSwitch, change.SwitchedAt))
            :
            "none";

        var evidence =
            change.After.LastRun is { } runEvidence
            ?
            string.Create(
                CultureInfo.InvariantCulture,
                $", baseline-cost={runEvidence.BaselineCost}, selected-cost={runEvidence.SelectedCost}, " +
                $"requests={runEvidence.RequestCount}, candidates={runEvidence.CandidateCount}, " +
                $"evaluated-candidates={runEvidence.EvaluatedCandidateCount}, " +
                $"evidence-added={runEvidence.AddedExpressionCount}, evidence-removed={runEvidence.RemovedExpressionCount}, " +
                $"evidence-elapsed-ms={Milliseconds(runEvidence.Elapsed)}")
            :
            "";

        SafeLog(
            $"Invocation-cache policy switch: sequence={change.Sequence}, run={change.Run}, state-run={change.After.RunCount}, " +
            $"old={change.Before.Policy.Id}, new={change.After.Policy.Id}, " +
            $"old-expressions={change.Before.Policy.Expressions.Count}, new-expressions={change.After.Policy.Expressions.Count}, " +
            $"added=[{Hashes(change.After.Policy.Expressions.Except(change.Before.Policy.Expressions))}], " +
            $"removed=[{Hashes(change.Before.Policy.Expressions.Except(change.After.Policy.Expressions))}], " +
            $"switches={change.Sequence}, elapsed-ms={Milliseconds(Stopwatch.GetElapsedTime(_startedAt, change.SwitchedAt))}, " +
            $"learning-ms={Milliseconds(Stopwatch.GetElapsedTime(change.StartedAt, change.SwitchedAt))}, " +
            $"accepted={change.AcceptedCount}, dropped={change.DroppedCount}, " +
            $"since-last-switch-ms={sinceLastSwitch}{evidence}");
    }

    private readonly record struct PolicySwitch(
        InvocationCacheLearningState Before,
        InvocationCacheLearningState After,
        long Sequence,
        long Run,
        long StartedAt,
        long SwitchedAt,
        long? PreviousSwitchAt,
        long AcceptedCount,
        long DroppedCount);

    private static string Hashes(IEnumerable<PineValue> values) =>
        string.Join(
            ",",
            values.Select(
                value =>
                Convert.ToHexStringLower(PineValueHashFlat.ComputeHashForValue(value).hashBytes.Span))
            .Order(StringComparer.Ordinal));

    private static string Milliseconds(TimeSpan duration) =>
        duration.TotalMilliseconds.ToString("F3", CultureInfo.InvariantCulture);

    private void SafeLog(string message)
    {
        try
        {
            _log?.Invoke(message);
        }
        catch
        {
            // Diagnostics must never stop learning or a production evaluation.
        }
    }

    private void DisposeLearningResource()
    {
        if (Interlocked.Exchange(ref _resourceDisposalStarted, 1) is not 0)
            return;

        try
        {
            _learningResource?.Dispose();
        }
        catch (Exception exception)
        {
            SafeLog($"Invocation-cache learning resource disposal failed: exception={exception.GetType().Name}");
        }
        finally
        {
            _resourceDisposed.TrySetResult();
        }
    }

    /// <summary>
    /// Cancels learning without waiting for a learner to acknowledge cancellation.
    /// </summary>
    public void Dispose()
    {
        var cancellationCallbackFailed = false;

        lock (_gate)
        {
            if (!_disposed)
            {
                _disposed = true;
                _pending.Writer.TryComplete();

                try
                {
                    _stop.Cancel();
                }
                catch (AggregateException)
                {
                    cancellationCallbackFailed = true;
                }
            }
        }

        if (cancellationCallbackFailed)
            SafeLog("Invocation-cache learning cancellation callback failed.");

        DisposeLearningResource();
    }

    /// <summary>
    /// Cancels and waits until the sole learning pump has stopped.
    /// </summary>
    public async ValueTask DisposeAsync()
    {
        Dispose();
        await _pump.ConfigureAwait(false);
        await _resourceDisposed.Task.ConfigureAwait(false);
    }

    private sealed class RecordingVM(ContinuousInvocationCacheLearning owner, IPineVM vm) : ICancellablePineVM
    {
        public Result<string, PineValue> EvaluateExpression(Expression expression, PineValue environment)
        {
            var result = vm.EvaluateExpression(expression, environment);

            if (result.IsOk())
                owner.Record(new(expression, environment));

            return result;
        }

        public Result<string, PineValue> EvaluateExpression(
            Expression expression,
            PineValue environment,
            CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();

            var result =
                vm is ICancellablePineVM cancellable
                ?
                cancellable.EvaluateExpression(expression, environment, cancellationToken)
                :
                vm.EvaluateExpression(expression, environment);

            cancellationToken.ThrowIfCancellationRequested();

            if (result.IsOk())
                owner.Record(new(expression, environment));

            return result;
        }
    }
}
