using Pine.Core.PineVM;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Threading;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Results retained after compiling an expression for execution in the intermediate VM.
/// </summary>
public readonly record struct ExpressionCompilationCacheEntry(
    ExpressionCompilation Compilation,
    string ExpressionHashBase16,
    OptimizationParametersSerial.ExpressionConfig? OptimizationConfig);

/// <summary>
/// Looks up an expression compilation in a caller-owned cache.
/// </summary>
public delegate bool TryGetExpressionCompilation(
    Expression expression,
    out ExpressionCompilationCacheEntry compilation);

/// <summary>
/// Gets an existing expression compilation or builds and inserts one.
/// </summary>
public delegate ExpressionCompilationCacheEntry GetOrAddExpressionCompilation(
    Expression expression,
    Func<ExpressionCompilationCacheEntry> buildCompilation);

/// <summary>
/// Thread-safe expression-compilation cache for sharing entries between VM instances.
/// </summary>
public sealed class ConcurrentExpressionCompilationCache
{
    private readonly ConcurrentDictionary<
        Expression,
        Lazy<ExpressionCompilationCacheEntry>>
        _entries = [];

    /// <summary>
    /// Looks up an expression compilation, waiting for an in-progress compilation of the same expression.
    /// </summary>
    public bool TryGet(
        Expression expression,
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
        Expression expression,
        Lazy<ExpressionCompilationCacheEntry> lazyCompilation)
    {
        try
        {
            return lazyCompilation.Value;
        }
        catch
        {
            ((ICollection<KeyValuePair<Expression, Lazy<ExpressionCompilationCacheEntry>>>)_entries)
                .Remove(new(expression, lazyCompilation));

            throw;
        }
    }
}
