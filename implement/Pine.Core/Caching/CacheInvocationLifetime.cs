using System;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

namespace Pine.Core.Caching;

/// <summary>
/// Host policy for marker-managed caches: above three working sets, trim toward two.
/// Concurrent invocations get independent increasing markers; active markers are pinned.
/// Each tier counts its own distinct inputs, including configuration variants.
/// </summary>
public sealed class CacheInvocationLifetime
{
    private readonly Lock _gate = new();
    private readonly HashSet<long> _active = [];
    private readonly Dictionary<IMarkerCacheLifetime<long>, Maintenance> _maintenance = [];
    private long _marker;

    private sealed class Maintenance(int budget)
    {
        public int? PendingBudget = budget;
        public TaskCompletionSource Completion { get; } = new(TaskCreationOptions.RunContinuationsAsynchronously);
    }

    /// <summary>
    /// Sets markers in the caller's execution context. Dispose there after preparation, including
    /// failed/cancelled preparation. Async children inherit the marker.
    /// A tier must belong to only one controller, even when it fronts shared persistent storage.
    /// </summary>
    public IDisposable BeginInvocation(params IMarkerCacheLifetime<long>?[] lifetimes)
    {
        var marker = Interlocked.Increment(ref _marker);
        lock (_gate)
            _active.Add(marker);

        try
        {
            var caches =
                lifetimes.OfType<IMarkerCacheLifetime<long>>().Distinct()
                .Select(cache => (Cache: cache, Usage: cache.SetMarker(marker))).ToArray();

            return new Invocation(this, marker, caches);
        }
        catch
        {
            lock (_gate)
                _active.Remove(marker);
            throw;
        }
    }

    private sealed class Invocation(
        CacheInvocationLifetime owner,
        long marker,
        (IMarkerCacheLifetime<long> Cache, ICacheUsage Usage)[] caches) : IDisposable
    {
        private int _disposed;

        public void Dispose()
        {
            if (Interlocked.Exchange(ref _disposed, 1) != 0)
                return;

            lock (owner._gate)
                owner._active.Remove(marker);

            foreach (var (cache, usage) in caches)
            {
                var used = usage.EntryCount;
                if (used == 0)
                    continue;

                var budget = (int)Math.Min(int.MaxValue, 2L * used);
                Maintenance maintenance;
                lock (owner._gate)
                {
                    if (owner._maintenance.TryGetValue(cache, out var existing))
                    {
                        existing.PendingBudget = budget;
                        continue;
                    }

                    if (cache.Count <= 3L * used)
                        continue;

                    maintenance = new(budget);
                    owner._maintenance.Add(cache, maintenance);
                }

                _ = owner.MaintainAsync(cache, maintenance);
            }
        }
    }

    private async Task MaintainAsync(IMarkerCacheLifetime<long> cache, Maintenance maintenance)
    {
        try
        {
            while (true)
            {
                int budget;
                lock (_gate)
                {
                    if (maintenance.PendingBudget is not { } pending)
                    {
                        _maintenance.Remove(cache);
                        maintenance.Completion.SetResult();
                        return;
                    }

                    budget = pending;
                    maintenance.PendingBudget = null;
                }

                await cache.TrimAsync(budget, Retention).ConfigureAwait(false);
            }
        }
        catch (Exception exception)
        {
            lock (_gate)
                _maintenance.Remove(cache);
            maintenance.Completion.SetException(exception);
            _ = maintenance.Completion.Task.Exception;
            Trace.TraceError("Cache invocation maintenance failed: " + exception);
        }

        CacheRetention Retention(long entryMarker)
        {
            lock (_gate)
            {
                return _active.Contains(entryMarker)
                    ? new CacheRetention.Retain()
                    : new CacheRetention.Ranked(entryMarker);
            }
        }
    }

    /// <summary>Awaits currently scheduled cleanup, propagating storage/policy failures.</summary>
    public Task WaitForMaintenanceAsync()
    {
        lock (_gate)
            return Task.WhenAll(_maintenance.Values.Select(item => item.Completion.Task));
    }
}
