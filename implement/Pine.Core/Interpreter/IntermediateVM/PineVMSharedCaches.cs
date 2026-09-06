using Pine.Core.CodeAnalysis;
using System.Collections.Concurrent;
using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Thread-safe caches that can be retained while creating short-lived PineVM instances.
/// Share one instance only among VMs with identical compilation configuration.
/// </summary>
public sealed class PineVMSharedCaches
{
    /// <summary>
    /// Compiled sequential IR, shared by compatible VM instances.
    /// </summary>
    public ConcurrentExpressionCompilationCache ExpressionCompilations { get; } = new();

    /// <summary>
    /// Parsed expression values.
    /// </summary>
    public PineVMParseCache ParsedExpressions { get; } = new();

    /// <summary>
    /// Encoded expression values.
    /// </summary>
    public PineVMExpressionEncodingCache EncodedExpressions { get; } = new();

    /// <summary>
    /// Reduced expressions keyed by expression and reduction configuration.
    /// </summary>
    public IDictionary<(Expression, ReductionConfig), Expression> ReducedExpressions { get; } =
        new ConcurrentDictionary<(Expression, ReductionConfig), Expression>();
}
