using System;
using System.Threading.Tasks;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Runs a request or a group of Evals with a fresh VM and private invocation buffer.
/// Completed computations are merged even on failure or cancellation, then the optional
/// application-owned completion policy runs. Concurrent calls use separate buffers/VMs;
/// the supplied shared cache and factory must support that concurrency.
/// </summary>
/// <typeparam name="TVM">The evaluator supplied to the request callback.</typeparam>
public sealed class InvocationCacheExecutionContext<TVM>
{
    private readonly Func<IInvocationCacheAccess, TVM> _createVM;

    private readonly int _maxBufferedEntries;

    private readonly Action? _onCompleted;

    /// <summary>The shared invocation tier; metadata caches can be captured separately by the factory.</summary>
    public IInvocationCacheAccess SharedCache { get; }

    /// <summary>
    /// Creates an execution context. Callbacks must not retain the VM or buffer after returning.
    /// The optional buffer limit is independent of shared storage. A completion callback may check
    /// shared counts and request background maintenance, but should not perform maintenance inline.
    /// </summary>
    public InvocationCacheExecutionContext(
        IInvocationCacheAccess sharedCache,
        Func<IInvocationCacheAccess, TVM> createVM,
        int maxBufferedEntries = int.MaxValue,
        Action? onCompleted = null)
    {
        ArgumentNullException.ThrowIfNull(sharedCache);
        ArgumentNullException.ThrowIfNull(createVM);
        ArgumentOutOfRangeException.ThrowIfLessThan(maxBufferedEntries, 1);

        SharedCache = sharedCache;
        _createVM = createVM;
        _maxBufferedEntries = maxBufferedEntries;
        _onCompleted = onCompleted;
    }

    /// <summary>Runs a synchronous request, publishing its buffer only after the callback exits.</summary>
    public TResult Run<TResult>(Func<TVM, TResult> execute)
    {
        ArgumentNullException.ThrowIfNull(execute);
        var buffer = new BufferedInvocationCacheAccess(SharedCache, _maxBufferedEntries);

        try
        {
            return execute(_createVM(buffer));
        }
        finally
        {
            Complete(buffer);
        }
    }

    /// <summary>
    /// Awaits the entire request before merging. The callback owns cancellation and can perform multiple Evals.
    /// </summary>
    public async ValueTask<TResult> RunAsync<TResult>(Func<TVM, ValueTask<TResult>> execute)
    {
        ArgumentNullException.ThrowIfNull(execute);
        var buffer = new BufferedInvocationCacheAccess(SharedCache, _maxBufferedEntries);

        try
        {
            return await execute(_createVM(buffer)).ConfigureAwait(false);
        }
        finally
        {
            Complete(buffer);
        }
    }

    private void Complete(BufferedInvocationCacheAccess buffer)
    {
        try
        {
            buffer.MergeIntoShared();
        }
        finally
        {
            _onCompleted?.Invoke();
        }
    }
}
