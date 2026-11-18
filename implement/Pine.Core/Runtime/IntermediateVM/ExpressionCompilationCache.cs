using Pine.Core.PineLanguage;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Threading;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Results retained after compiling an expression for execution in the intermediate VM.
/// </summary>
public readonly record struct ExpressionCompilationCacheEntry(
    ExpressionCompilation Compilation,
    ExpressionIdentity Identity);

/// <summary>
/// Looks up an expression compilation in a caller-owned cache.
/// </summary>
public delegate bool TryGetExpressionCompilation(
    InternalExpression.Expression expression,
    out ExpressionCompilationCacheEntry compilation);

/// <summary>
/// Gets an existing expression compilation or builds and inserts one.
/// </summary>
public delegate ExpressionCompilationCacheEntry GetOrAddExpressionCompilation(
    InternalExpression.Expression expression,
    Func<ExpressionCompilationCacheEntry> buildCompilation);

/// <summary>
/// Thread-safe expression-compilation cache for sharing entries between VM instances.
/// </summary>
public sealed class ConcurrentExpressionCompilationCache
{
    private readonly ConcurrentDictionary<
        InternalExpression.Expression,
        Lazy<ExpressionCompilationCacheEntry>>
        _entries = [];

    /// <summary>Number of compiled or currently compiling expressions.</summary>
    public int Count => _entries.Count;

    /// <summary>
    /// Looks up an expression compilation, waiting for an in-progress compilation of the same expression.
    /// </summary>
    public bool TryGet(
        Expression expression,
        out ExpressionCompilationCacheEntry compilation)
        => TryGet(InternalExpression.ExpressionConversion.FromCanonicalCached(expression), out compilation);

    /// <summary>Looks up retained syntax without constructing a canonical view or content hash.</summary>
    public bool TryGet(
        InternalExpression.Expression expression,
        out ExpressionCompilationCacheEntry compilation)
    {
        if (!_entries.TryGetValue(expression, out var lazyCompilation))
        {
            compilation = default;
            return false;
        }

        compilation = GetValue(expression, lazyCompilation);
        return true;
    }

    /// <summary>
    /// Gets or compiles an expression. Concurrent misses for the same expression share one compilation.
    /// </summary>
    public ExpressionCompilationCacheEntry GetOrAdd(
        Expression expression,
        Func<ExpressionCompilationCacheEntry> buildCompilation)
        => GetOrAdd(InternalExpression.ExpressionConversion.FromCanonicalCached(expression), buildCompilation);

    /// <summary>Compiles retained syntax without requiring canonical metadata.</summary>
    public ExpressionCompilationCacheEntry GetOrAdd(
        InternalExpression.Expression expression,
        Func<ExpressionCompilationCacheEntry> buildCompilation)
    {
        ArgumentNullException.ThrowIfNull(buildCompilation);

        var lazyCompilation =
            _entries.GetOrAdd(
                expression,
                _ =>
                new Lazy<ExpressionCompilationCacheEntry>(
                    buildCompilation,
                    LazyThreadSafetyMode.ExecutionAndPublication));

        return GetValue(expression, lazyCompilation);
    }

    private ExpressionCompilationCacheEntry GetValue(
        InternalExpression.Expression expression,
        Lazy<ExpressionCompilationCacheEntry> lazyCompilation)
    {
        try
        {
            return lazyCompilation.Value;
        }
        catch
        {
            ((ICollection<KeyValuePair<InternalExpression.Expression, Lazy<ExpressionCompilationCacheEntry>>>)_entries)
                .Remove(new(expression, lazyCompilation));

            throw;
        }
    }
}
