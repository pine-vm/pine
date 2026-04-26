using System.Collections.Generic;

namespace Pine.Core.Elm.ElmSyntax;

/// <summary>
/// One entry in a log of function applications observed by an
/// <see cref="IInvocationLogger"/>. The two variants distinguish
/// <see cref="Direct"/> applications (dispatch through a syntactic
/// <see cref="ElmSyntaxInterpreter.Application"/> identified by
/// <see cref="CodeAnalysis.DeclQualifiedName"/>) from
/// <see cref="FunctionValue"/> applications (apply an already-evaluated
/// <see cref="ElmValue.ElmFunction"/> closure to one or more arguments).
/// </summary>
public abstract record ApplicationLogEntry
{
    /// <summary>
    /// A direct (name-based) application. Corresponds to one increment of
    /// <see cref="ElmSyntaxInterpreterPerformanceCounters.DirectFunctionApplicationCount"/>.
    /// </summary>
    /// <param name="Application">
    /// The <see cref="ElmSyntaxInterpreter.Application"/> that the interpreter was
    /// about to dispatch — its <see cref="ElmSyntaxInterpreter.Application.FunctionName"/>
    /// identifies the call target and its
    /// <see cref="ElmSyntaxInterpreter.Application.Arguments"/> are the (already
    /// evaluated) argument values.
    /// </param>
    public sealed record Direct(ElmSyntaxInterpreter.Application Application)
        : ApplicationLogEntry;

    /// <summary>
    /// An application of a previously-evaluated function value (a closure or a
    /// partially-applied top-level function). Corresponds to one increment of
    /// <see cref="ElmSyntaxInterpreterPerformanceCounters.FunctionValueApplicationCount"/>.
    /// </summary>
    /// <param name="Function">
    /// The function value being applied. For closures originating from a top-level
    /// declaration, this is an <see cref="ElmValue.ElmFunction"/> whose
    /// <see cref="ElmValue.ElmFunction.Source"/> is
    /// <see cref="ElmValue.ElmFunction.SourceRef.Declared"/>.
    /// </param>
    /// <param name="NewArguments">
    /// The arguments newly supplied to the function value (i.e. the arguments at the
    /// application site, not the arguments previously captured by partial application).
    /// </param>
    public sealed record FunctionValue(
        ElmValue Function,
        IReadOnlyList<ElmValue> NewArguments)
        : ApplicationLogEntry;
}

/// <summary>
/// Sink for events emitted by <see cref="ElmSyntaxInterpreter"/> during evaluation. The
/// interpreter calls one of these methods at each high-level event so that callers can
/// either accumulate counters (the default behavior used by
/// <see cref="ElmSyntaxInterpreter.ParseAndInterpretWithCounters(string, IReadOnlyDictionary{CodeAnalysis.DeclQualifiedName, SyntaxModel.Declaration})"/>)
/// or build a richer trace of function applications.
/// </summary>
/// <remarks>
/// Implementations are expected to be cheap and side-effect-free beyond their own state:
/// an event-handler that throws will propagate out of the interpreter and be reported as
/// an evaluation failure.
/// </remarks>
public interface IInvocationLogger
{
    /// <summary>
    /// Called once at the top of every iteration of the trampoline loop. Increments
    /// <see cref="ElmSyntaxInterpreterPerformanceCounters.InstructionLoopCount"/> on
    /// counting implementations.
    /// </summary>
    void OnInstructionLoop();

    /// <summary>
    /// Called when the interpreter dispatches a direct (name-based) function application —
    /// i.e. every <see cref="SyntaxModel.Expression.FunctionOrValue"/> reference that resolves
    /// to a top-level user-defined function, constructor, or built-in identified by name.
    /// </summary>
    void OnDirectFunctionApplication(ElmSyntaxInterpreter.Application application);

    /// <summary>
    /// Called when the interpreter applies an evaluated function value (closure) to one or
    /// more new arguments. <paramref name="newArguments"/> contains only the arguments at the
    /// current application site, not the arguments captured by previous partial applications.
    /// </summary>
    void OnFunctionValueApplication(
        ElmValue functionValue,
        IReadOnlyList<ElmValue> newArguments);

    /// <summary>
    /// Called when an application is resolved to a built-in (<c>Pine_builtin</c> /
    /// <c>Pine_kernel</c>) function and forwarded to <see cref="KernelFunction"/>.
    /// </summary>
    void OnPineBuiltinInvocation(ElmSyntaxInterpreter.Application application);

    /// <summary>
    /// Called when the interpreter is about to enter a user-defined function body
    /// (either via name dispatch or via application of a closure). Returns the new
    /// recursion-depth counter value, used by the interpreter's periodic
    /// infinite-recursion detector.
    /// </summary>
    int IncrementUserCallDepth();
}

/// <summary>
/// <see cref="IInvocationLogger"/> implementation that combines a counting logger
/// with a captured log of every <see cref="ApplicationLogEntry"/>. Used by callers
/// that want both a final
/// <see cref="ElmSyntaxInterpreterPerformanceCounters"/> snapshot and a trace they
/// can search or render.
/// </summary>
public sealed class RecordingInvocationLogger : IInvocationLogger
{
    private readonly List<ApplicationLogEntry> _entries = [];

    private long _instructionLoopCount;

    private long _directFunctionApplicationCount;

    private long _functionValueApplicationCount;

    private long _pineBuiltinInvocationCount;

    private int _userCallDepth;

    /// <summary>
    /// The captured log of function applications, in the order in which the interpreter
    /// dispatched them.
    /// </summary>
    public IReadOnlyList<ApplicationLogEntry> Entries => _entries;

    /// <summary>
    /// Snapshot of the performance counters accumulated so far.
    /// </summary>
    public ElmSyntaxInterpreterPerformanceCounters Counters =>
        new(
            InstructionLoopCount: _instructionLoopCount,
            DirectFunctionApplicationCount: _directFunctionApplicationCount,
            FunctionValueApplicationCount: _functionValueApplicationCount,
            PineBuiltinInvocationCount: _pineBuiltinInvocationCount);

    /// <inheritdoc/>
    public void OnInstructionLoop() => _instructionLoopCount++;

    /// <inheritdoc/>
    public void OnDirectFunctionApplication(ElmSyntaxInterpreter.Application application)
    {
        _directFunctionApplicationCount++;
        _entries.Add(new ApplicationLogEntry.Direct(application));
    }

    /// <inheritdoc/>
    public void OnFunctionValueApplication(
        ElmValue functionValue,
        IReadOnlyList<ElmValue> newArguments)
    {
        _functionValueApplicationCount++;
        _entries.Add(new ApplicationLogEntry.FunctionValue(functionValue, newArguments));
    }

    /// <inheritdoc/>
    public void OnPineBuiltinInvocation(ElmSyntaxInterpreter.Application application) =>
        _pineBuiltinInvocationCount++;

    /// <inheritdoc/>
    public int IncrementUserCallDepth() => ++_userCallDepth;
}
