using Pine.Core.Caching;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Concrete;
using Pine.Core.Functional;
using Pine.Core.IntermediateVM;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using Pine.Core.Runtime.IntermediateVM;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Diagnostics;
using System.Linq;
using System.Threading;
using System.Threading.Tasks;

using IPineVM = Pine.Core.Runtime.IPineVM;

namespace Pine.Core.Elm.Testing;

/// <summary>
/// Caller-owned caches for repeated Elm test runs. Retain this object, not VMs,
/// instrumentation, cancellation state, or test reports. Native leaves must remain immutable.
/// </summary>
/// <remarks>
/// Creates a session with fixed native leaves and optional generational storage and background learning.
/// Without generation options, invocation storage remains unbounded. The session owns the supplied learner.
/// </remarks>
public sealed class ElmTestRunCacheSession(
    IReadOnlyDictionary<PineValue, PrecompiledLeaf>? precompiledLeaves = null,
    InvocationCacheGenerationOptions? generationOptions = null,
    ContinuousInvocationCacheLearning? cacheLearning = null,
    SourceCompilationCaches? compilationCaches = null) : IDisposable, IAsyncDisposable
{
    /// <summary>
    /// Retained pure source transformations. Supply alternate stores or cascades here;
    /// concurrent hosts require thread-safe implementations.
    /// </summary>
    public SourceCompilationCaches CompilationCaches { get; } =
        compilationCaches ?? SourceCompilationCaches.CreateInMemory();

    private readonly Lock _compilationGate = new();
    private readonly HashSet<long> _activeCompilations = [];
    private readonly Dictionary<IMarkerCacheLifetime<long>, CompilationMaintenance> _compilationMaintenance = [];
    private long _compilationMarker;

    private sealed class CompilationMaintenance(int budget)
    {
        public int? PendingBudget = budget;
        public TaskCompletionSource Completion { get; } = new(TaskCreationOptions.RunContinuationsAsynchronously);
    }

    /// <summary>
    /// Begins a host invocation with an increasing marker. Dispose in the same execution context
    /// after preparation, including failed/cancelled preparation. Async children inherit the marker.
    /// </summary>
    public IDisposable BeginCompilationInvocation()
    {
        var marker = Interlocked.Increment(ref _compilationMarker);
        lock (_compilationGate)
            _activeCompilations.Add(marker);

        try
        {
            var caches =
                new[] { CompilationCaches.OperatorLifetime, CompilationCaches.SemanticLifetime }
                .OfType<IMarkerCacheLifetime<long>>().Distinct()
                .Select(cache => (Cache: cache, Usage: cache.SetMarker(marker))).ToArray();

            return new CompilationInvocation(this, marker, caches);
        }
        catch
        {
            lock (_compilationGate)
                _activeCompilations.Remove(marker);
            throw;
        }
    }

    private sealed class CompilationInvocation(
        ElmTestRunCacheSession owner,
        long marker,
        (IMarkerCacheLifetime<long> Cache, ICacheUsage Usage)[] caches) : IDisposable
    {
        private int _disposed;

        public void Dispose()
        {
            if (Interlocked.Exchange(ref _disposed, 1) != 0)
                return;

            lock (owner._compilationGate)
                owner._activeCompilations.Remove(marker);

            foreach (var (cache, usage) in caches)
            {
                var used = usage.EntryCount;

                if (used == 0)
                    continue;

                var budget = (int)Math.Min(int.MaxValue, 2L * used);
                CompilationMaintenance maintenance;
                lock (owner._compilationGate)
                {
                    if (owner._compilationMaintenance.TryGetValue(cache, out var existing))
                    {
                        existing.PendingBudget = budget;
                        continue;
                    }

                    if (cache.Count <= 3L * used)
                        continue;

                    maintenance = new(budget);
                    owner._compilationMaintenance.Add(cache, maintenance);
                }

                _ = owner.MaintainCompilationCacheAsync(cache, maintenance);
            }
        }
    }

    private async Task MaintainCompilationCacheAsync(
        IMarkerCacheLifetime<long> cache, CompilationMaintenance maintenance)
    {
        try
        {
            while (true)
            {
                int budget;
                lock (_compilationGate)
                {
                    if (maintenance.PendingBudget is not { } pending)
                    {
                        _compilationMaintenance.Remove(cache);
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
            lock (_compilationGate)
                _compilationMaintenance.Remove(cache);
            maintenance.Completion.SetException(exception);
            _ = maintenance.Completion.Task.Exception;
            Trace.TraceError("Source-transformation cache maintenance failed: " + exception);
        }

        CacheRetention Retention(long entryMarker)
        {
            lock (_compilationGate)
            {
                return _activeCompilations.Contains(entryMarker)
                    ? new CacheRetention.Retain()
                    : new CacheRetention.Ranked(entryMarker);
            }
        }
    }

    /// <summary>Awaits currently scheduled transformation cleanup, propagating storage/policy failures.</summary>
    public Task WaitForCompilationMaintenanceAsync()
    {
        lock (_compilationGate)
            return Task.WhenAll(_compilationMaintenance.Values.Select(item => item.Completion.Task));
    }

    /// <summary>Compilation, parsing, encoding and reduction caches shared by preparation and workers.</summary>
    public PineVMSharedCaches VmCaches { get; } = new();

    /// <summary>Successful function invocations, keyed by expression and complete frame input.</summary>
    public ConcurrentInvocationCache InvocationResults { get; } = new();

    /// <summary>Optional bounded-generation storage used instead of <see cref="InvocationResults"/>.</summary>
    public GenerationalInvocationCache? GenerationalInvocations { get; } =
        generationOptions is null ? null : new(generationOptions);

    /// <summary>Shared invocation storage used by preparation and test executions.</summary>
    public IInvocationCacheAccess InvocationCache =>
        (IInvocationCacheAccess?)GenerationalInvocations ?? InvocationResults;

    /// <summary>Cumulative VM-facing cache operations across executions.</summary>
    public InvocationCacheStatistics CacheStatistics { get; } = new();

    /// <summary>Optional session-owned background learner and its current state.</summary>
    public ContinuousInvocationCacheLearning? CacheLearning { get; } = cacheLearning;

    /// <summary>Captures cache operations and, when configured, generational storage statistics.</summary>
    public InvocationCacheStatisticsSnapshot GetCacheStatisticsSnapshot() =>
        CacheStatistics.GetSnapshot() with { SharedCacheGenerations = GenerationalInvocations?.GetStatistics() };

    internal InvocationCacheConfiguration? InvocationCacheConfiguration =>
        CacheLearning is null ? null : new(0, 0, 0, 0);

    internal InvocationCacheExecutionContext<IPineVM> CreateExecutionContext(
        Func<IInvocationCacheAccess, IPineVM> createVm) =>
        new(
            InvocationCache,
            buffer =>
            {
                var vm = createVm(buffer);
                return CacheLearning?.Wrap(vm) ?? vm;
            },
            onCompleted: RequestMaintenance,
            statistics: CacheStatistics,
            getPolicy: CacheLearning is null ? null : () => CacheLearning.CurrentPolicy);

    private void RequestMaintenance()
    {
        if (GenerationalInvocations is not { } cache ||
            cache.NewestGenerationEntryCount < cache.Options.RotationEntryCount)
        {
            return;
        }

        _ = ObserveMaintenance();

        async Task ObserveMaintenance()
        {
            try
            {
                await cache.MaintainAsync().ConfigureAwait(false);
            }
            catch (Exception exception)
            {
                try
                {
                    Trace.TraceError("Invocation-cache maintenance failed: " + exception);
                }
                catch
                {
                    // A diagnostic listener must not fault background maintenance observation.
                }
            }
        }
    }

    /// <summary>Stops session-owned learning without retaining a run's instrumentation.</summary>
    public void Dispose()
    {
        CacheLearning?.Dispose();
        WaitForCompilationMaintenanceAsync().GetAwaiter().GetResult();
    }

    /// <summary>Stops and awaits session-owned background learning.</summary>
    public async ValueTask DisposeAsync()
    {
        if (CacheLearning is not null)
            await CacheLearning.DisposeAsync().ConfigureAwait(false);

        await WaitForCompilationMaintenanceAsync().ConfigureAwait(false);
    }

    /// <summary>Source parsing keyed by complete source text, never by path or timestamp.</summary>
    public IDictionary<string, Result<ParseError, File>> SourceParsing { get; } =
        new ConcurrentDictionary<string, Result<ParseError, File>>(StringComparer.Ordinal);

    /// <summary>Declaration canonicalization memoized over structural syntax and consulted resolution inputs.</summary>
    public Canonicalization.DeclarationCache DeclarationCanonicalization { get; } = new();

    internal Result<ParseError, File> ParseSource(string text)
    {
        if (SourceParsing.TryGetValue(text, out var parsed))
            return parsed;

        var result = ElmSyntaxParser.ParseModuleText(text);

        if (result.IsOkOrNull() is not null)
            SourceParsing[text] = result;

        return result;
    }

    internal IReadOnlyDictionary<PineValue, PrecompiledLeaf> PrecompiledLeaves { get; } =
        precompiledLeaves ?? SetupVM.DefaultPrecompiledLeaves;

    internal void ValidateInstrumentation(ElmTestInstrumentation? instrumentation)
    {
        if (instrumentation is null)
            return;

        if (instrumentation.RecordDiagnostics)
        {
            throw new ArgumentException(
                "Diagnostic profiles require isolated caches; do not supply a cache session.",
                nameof(instrumentation));
        }

        var options = instrumentation.Options;

        if (options.DisableReduction || options.DisableInlining || options.DisableTailRecursion ||
            options.DisablePrecompiledLeaves ||
            options.DisableApplicationFastPaths || options.DisableInvocationCache ||
            !ReferenceEquals(instrumentation.ResolvePrecompiledLeaves(), PrecompiledLeaves))
        {
            throw new ArgumentException(
                "A cache session requires the same default VM compilation controls and native leaves for every run.",
                nameof(instrumentation));
        }
    }
}
