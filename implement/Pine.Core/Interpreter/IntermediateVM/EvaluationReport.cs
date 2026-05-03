using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Aggregated performance counters collected during expression evaluation.
/// </summary>
/// <param name="InvocationCount">The total number of runtime invocations performed, including parse-and-eval and direct stack-frame invocations.</param>
/// <param name="BuildListCount">The total number of executed <c>Build_List</c> and <c>Build_List_Tagged_Const</c> instructions.</param>
/// <param name="LoopIterationCount">The total number of loop iterations reported by the active stack frames.</param>
/// <param name="InstructionCount">The total number of VM instructions executed.</param>
public readonly record struct PerformanceCounters(
    long InvocationCount,
    long BuildListCount,
    long LoopIterationCount,
    long InstructionCount)
{
    /// <summary>
    /// Returns the element-wise sum of two <see cref="PerformanceCounters"/> instances.
    /// </summary>
    public static PerformanceCounters Add(PerformanceCounters a, PerformanceCounters b) =>
        new(
            InvocationCount: a.InvocationCount + b.InvocationCount,
            BuildListCount: a.BuildListCount + b.BuildListCount,
            LoopIterationCount: a.LoopIterationCount + b.LoopIterationCount,
            InstructionCount: a.InstructionCount + b.InstructionCount);

    /// <summary>
    /// Sums all <see cref="PerformanceCounters"/> in the given sequence.
    /// Returns a zero-valued instance if the sequence is empty.
    /// </summary>
    public static PerformanceCounters Aggregate(IEnumerable<PerformanceCounters> counters)
    {
        long totalInvocations = 0;
        long totalBuildLists = 0;
        long totalLoopIterations = 0;
        long totalInstructions = 0;

        foreach (var c in counters)
        {
            totalInvocations += c.InvocationCount;
            totalBuildLists += c.BuildListCount;
            totalLoopIterations += c.LoopIterationCount;
            totalInstructions += c.InstructionCount;
        }

        return
            new PerformanceCounters(
                InvocationCount: totalInvocations,
                BuildListCount: totalBuildLists,
                LoopIterationCount: totalLoopIterations,
                InstructionCount: totalInstructions);
    }
}

/// <summary>
/// Profiling information and result value produced from evaluating an expression in the intermediate VM.
/// </summary>
/// <param name="ExpressionValue">Encoded representation of the evaluated expression.</param>
/// <param name="Expression">The evaluated expression.</param>
/// <param name="Input">The input values supplied to the expression.</param>
/// <param name="Counters">The aggregated performance counters for this evaluation.</param>
/// <param name="ReturnValue">The returned value in the in-process representation.</param>
/// <param name="StackTrace">The captured stack trace at the time of reporting.</param>
public record EvaluationReport(
    PineValue ExpressionValue,
    Expression Expression,
    StackFrameInput Input,
    PerformanceCounters Counters,
    Internal.PineValueInProcess ReturnValue,
    IReadOnlyList<Expression> StackTrace)
{
    /// <summary>
    /// The total number of VM instructions executed.
    /// </summary>
    public long InstructionCount => Counters.InstructionCount;

    /// <summary>
    /// The total number of runtime invocations performed.
    /// </summary>
    public long InvocationCount => Counters.InvocationCount;

    /// <summary>
    /// The total number of executed Build_List instructions.
    /// </summary>
    public long BuildListCount => Counters.BuildListCount;

    /// <summary>
    /// The total number of loop iterations.
    /// </summary>
    public long LoopIterationCount => Counters.LoopIterationCount;
}
