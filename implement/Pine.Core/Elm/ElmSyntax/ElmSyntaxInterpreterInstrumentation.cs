using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Threading;

namespace Pine.Core.Elm.ElmSyntax;

public partial class ElmSyntaxInterpreter
{
    /// <summary>
    /// Ambient (per-async-flow) <see cref="IInvocationLogger"/> consulted by
    /// <c>RunTrampoline</c> when no explicit logger is passed. This makes it possible to
    /// instrument entry points that don't otherwise thread a logger through their
    /// signature — most importantly the direct-call
    /// <see cref="Interpret(DeclQualifiedName, System.Collections.Generic.IReadOnlyList{PineValueInProcess}, Prepared)"/>
    /// overload used by the Elm-app compilation pipeline — without changing their public
    /// contract. Instrumentation is opt-in: the field is only non-null while a scope opened
    /// by <see cref="BeginInstrumentationScope(IInvocationLogger)"/> is active.
    /// </summary>
    private static readonly AsyncLocal<IInvocationLogger?> t_ambientInvocationLogger = new();

    /// <summary>
    /// The ambient <see cref="IInvocationLogger"/> currently in effect, or <c>null</c> when
    /// no instrumentation scope is active.
    /// </summary>
    public static IInvocationLogger? AmbientInvocationLogger => t_ambientInvocationLogger.Value;

    /// <summary>
    /// Installs <paramref name="logger"/> as the <see cref="AmbientInvocationLogger"/> for
    /// the duration of the returned scope. Any interpretation started on the same async flow
    /// while the scope is open — including the direct-call
    /// <see cref="Interpret(DeclQualifiedName, System.Collections.Generic.IReadOnlyList{PineValueInProcess}, Prepared)"/>
    /// overload — reports its function applications to <paramref name="logger"/>. Disposing
    /// the scope restores the previously-installed logger (scopes nest).
    /// </summary>
    public static IDisposable BeginInstrumentationScope(IInvocationLogger logger)
    {
        ArgumentNullException.ThrowIfNull(logger);

        var previous = t_ambientInvocationLogger.Value;

        t_ambientInvocationLogger.Value = logger;

        return new InstrumentationScope(previous);
    }

    private sealed class InstrumentationScope(IInvocationLogger? previous) : IDisposable
    {
        private bool _disposed;

        public void Dispose()
        {
            if (_disposed)
                return;

            _disposed = true;

            t_ambientInvocationLogger.Value = previous;
        }
    }
}

/// <summary>
/// <see cref="IInvocationLogger"/> that aggregates the number of interpreter steps
/// attributed to each named function, so that the functions that dominate a workload can
/// be identified and, where worthwhile, reimplemented as direct C# builtins in
/// <c>ElmSyntaxInterpreter.BuildBuiltinFunctionResolvers</c>.
/// <para>
/// A "step" here is one direct (name-based) function application
/// (<see cref="IInvocationLogger.OnDirectFunctionApplication(ElmSyntaxInterpreter.Application)"/>):
/// each such application either enters a user-defined function body or is served by a
/// builtin, so the per-function tally is a good proxy for how much interpreter work a
/// function is responsible for. Applications of evaluated function values (closures) are
/// counted separately under a single synthetic bucket because they carry no syntactic
/// name.
/// </para>
/// </summary>
public sealed class FunctionStepCountingInvocationLogger : IInvocationLogger
{
    private readonly Dictionary<DeclQualifiedName, long> _directApplicationCounts = [];

    private long _instructionLoopCount;

    private long _directFunctionApplicationCount;

    private long _functionValueApplicationCount;

    private long _pineBuiltinInvocationCount;

    private int _userCallDepth;

    /// <summary>
    /// Per-function count of direct (name-based) applications observed so far.
    /// </summary>
    public IReadOnlyDictionary<DeclQualifiedName, long> DirectApplicationCounts => _directApplicationCounts;

    /// <summary>
    /// Total number of applications of evaluated function values (closures), which carry no
    /// syntactic name and are therefore not attributed to any individual declaration.
    /// </summary>
    public long FunctionValueApplicationCount => _functionValueApplicationCount;

    /// <summary>
    /// Snapshot of the aggregate performance counters accumulated so far.
    /// </summary>
    public ElmSyntaxInterpreterPerformanceCounters Counters =>
        new(
            InstructionLoopCount: _instructionLoopCount,
            DirectFunctionApplicationCount: _directFunctionApplicationCount,
            FunctionValueApplicationCount: _functionValueApplicationCount,
            PineBuiltinInvocationCount: _pineBuiltinInvocationCount);

    /// <summary>
    /// Returns the functions with the most attributed steps, most expensive first.
    /// </summary>
    /// <param name="limit">Maximum number of entries to return. Non-positive returns all.</param>
    public IReadOnlyList<KeyValuePair<DeclQualifiedName, long>> TopFunctionsByStepCount(int limit = 0)
    {
        var ordered =
            _directApplicationCounts
            .OrderByDescending(kv => kv.Value)
            .ThenBy(kv => kv.Key.FullName, StringComparer.Ordinal);

        if (limit > 0)
            return [.. ordered.Take(limit)];

        return [.. ordered];
    }

    /// <inheritdoc/>
    public void OnInstructionLoop() => _instructionLoopCount++;

    /// <inheritdoc/>
    public void OnDirectFunctionApplication(ElmSyntaxInterpreter.Application application)
    {
        _directFunctionApplicationCount++;

        var functionName = application.FunctionName;

        _directApplicationCounts.TryGetValue(functionName, out var previous);
        _directApplicationCounts[functionName] = previous + 1;
    }

    /// <inheritdoc/>
    public void OnFunctionValueApplication(
        PineValueInProcess functionValue,
        IReadOnlyList<PineValueInProcess> newArguments) =>
        _functionValueApplicationCount++;

    /// <inheritdoc/>
    public void OnPineBuiltinInvocation(ElmSyntaxInterpreter.Application application) =>
        _pineBuiltinInvocationCount++;

    /// <inheritdoc/>
    public int IncrementUserCallDepth() => ++_userCallDepth;
}
