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
    ContinuousInvocationCacheLearning? cacheLearning = null) : IDisposable, IAsyncDisposable
{
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
    public void Dispose() => CacheLearning?.Dispose();

    /// <summary>Stops and awaits session-owned background learning.</summary>
    public ValueTask DisposeAsync() => CacheLearning?.DisposeAsync() ?? ValueTask.CompletedTask;

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
