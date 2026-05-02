namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Delegate for observing tail-loop iterations performed by <see cref="PineVM"/>.
/// <para />
/// Distinct from <c>reportFunctionApplication</c> (which fires only when a stack
/// frame returns), this hook fires once per iteration of a tail loop, where an
/// iteration is either:
/// <list type="bullet">
///   <item>a backward jump within the current stack frame (looping in place), or</item>
///   <item>a tail-call invocation that replaces the current stack frame
///         (under <c>enableTailRecursionOptimization: true</c>).</item>
/// </list>
/// This makes inner loops (such as per-character parser loops) directly observable
/// without having to correlate two evaluations of different input lengths.
/// </summary>
/// <param name="tailLoopIteration">
/// A value-type snapshot describing the tail-loop iteration.
/// </param>
public delegate void ReportTailLoopIteration(
    in TailLoopIteration tailLoopIteration);

/// <summary>
/// Identifies which kind of tail-loop iteration triggered a
/// <see cref="ReportTailLoopIteration"/> notification.
/// </summary>
public enum TailLoopIterationKind
{
    /// <summary>
    /// A backward jump (negative-offset <c>Jump_Const</c> or
    /// <c>Jump_If_Equal_Const</c>) inside a single stack frame.
    /// </summary>
    BackwardJump = 1,

    /// <summary>
    /// A tail-call that replaced the current stack frame instead of growing
    /// the stack.
    /// </summary>
    TailCallReplace = 2,
}

/// <summary>
/// Information about a single tail-loop iteration performed by
/// <see cref="PineVM"/>.
/// </summary>
/// <param name="IterationIndex">
/// Zero-based ordinal of this notification in the overall evaluation, i.e. how
/// many tail-loop iteration callbacks have already been fired before this one.
/// </param>
/// <param name="StackFrameDepth">Number of active stack frames at the moment of the iteration.</param>
/// <param name="Kind">Whether the iteration was a backward jump or a tail-call replacement.</param>
/// <param name="FrameExpression">
/// The expression of the stack frame that is iterating. For
/// <see cref="TailLoopIterationKind.BackwardJump"/> this is the running frame;
/// for <see cref="TailLoopIterationKind.TailCallReplace"/> this is the new
/// (replacing) frame, which represents the next iteration of the tail loop.
/// </param>
/// <param name="FrameInput">
/// The live input (live environment) of the iterating frame at this iteration.
/// For <see cref="TailLoopIterationKind.TailCallReplace"/> this is the input of
/// the replacing frame, i.e. the values forwarded into the next iteration.
/// </param>
public readonly record struct TailLoopIteration(
    long IterationIndex,
    int StackFrameDepth,
    TailLoopIterationKind Kind,
    Expression FrameExpression,
    StackFrameInput FrameInput);
