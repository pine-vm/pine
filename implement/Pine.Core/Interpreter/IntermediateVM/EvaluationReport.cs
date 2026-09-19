using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Aggregated performance counters collected during expression evaluation.
/// </summary>
/// <param name="InvocationCount">The total number of runtime invocations performed, including parse-and-eval and direct stack-frame invocations.</param>
/// <param name="BuildListCount">The total number of executed <c>Build_List</c> and <c>Build_List_With_Prefix</c> instructions.</param>
/// <param name="LoopIterationCount">The total number of loop iterations reported by the active stack frames.</param>
/// <param name="InstructionCount">The total number of VM instructions executed.</param>
/// <param name="CurriedFunctionPlanParseCount">The number of concrete values inspected to build a curried-function plan.</param>
/// <param name="PartialApplicationAllocationCount">The number of in-process partial-application values allocated.</param>
/// <param name="DirectSaturatedApplicationCount">The number of saturated applications entered without an intermediate function value.</param>
/// <param name="PartialApplicationMaterializationCount">The number of partial applications forced to their canonical Pine value.</param>
public readonly record struct PerformanceCounters(
    long InvocationCount,
    long BuildListCount,
    long LoopIterationCount,
    long InstructionCount,
    long CurriedFunctionPlanParseCount = 0,
    long PartialApplicationAllocationCount = 0,
    long DirectSaturatedApplicationCount = 0,
    long PartialApplicationMaterializationCount = 0)
{
    /// <summary>
    /// Returns the element-wise sum of two <see cref="PerformanceCounters"/> instances.
    /// </summary>
    public static PerformanceCounters Add(PerformanceCounters a, PerformanceCounters b) =>
        new(
            InvocationCount: a.InvocationCount + b.InvocationCount,
            BuildListCount: a.BuildListCount + b.BuildListCount,
            LoopIterationCount: a.LoopIterationCount + b.LoopIterationCount,
            InstructionCount: a.InstructionCount + b.InstructionCount,
            CurriedFunctionPlanParseCount:
            a.CurriedFunctionPlanParseCount + b.CurriedFunctionPlanParseCount,
            PartialApplicationAllocationCount:
            a.PartialApplicationAllocationCount + b.PartialApplicationAllocationCount,
            DirectSaturatedApplicationCount:
            a.DirectSaturatedApplicationCount + b.DirectSaturatedApplicationCount,
            PartialApplicationMaterializationCount:
            a.PartialApplicationMaterializationCount + b.PartialApplicationMaterializationCount);

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
        long totalCurriedFunctionPlanParses = 0;
        long totalPartialApplicationAllocations = 0;
        long totalDirectSaturatedApplications = 0;
        long totalPartialApplicationMaterializations = 0;

        foreach (var c in counters)
        {
            totalInvocations += c.InvocationCount;
            totalBuildLists += c.BuildListCount;
            totalLoopIterations += c.LoopIterationCount;
            totalInstructions += c.InstructionCount;
            totalCurriedFunctionPlanParses += c.CurriedFunctionPlanParseCount;
            totalPartialApplicationAllocations += c.PartialApplicationAllocationCount;
            totalDirectSaturatedApplications += c.DirectSaturatedApplicationCount;
            totalPartialApplicationMaterializations += c.PartialApplicationMaterializationCount;
        }

        return
            new PerformanceCounters(
                InvocationCount: totalInvocations,
                BuildListCount: totalBuildLists,
                LoopIterationCount: totalLoopIterations,
                InstructionCount: totalInstructions,
                CurriedFunctionPlanParseCount: totalCurriedFunctionPlanParses,
                PartialApplicationAllocationCount: totalPartialApplicationAllocations,
                DirectSaturatedApplicationCount: totalDirectSaturatedApplications,
                PartialApplicationMaterializationCount: totalPartialApplicationMaterializations);
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

    /// <summary>
    /// The number of concrete values inspected to build a curried-function plan.
    /// </summary>
    public long CurriedFunctionPlanParseCount => Counters.CurriedFunctionPlanParseCount;

    /// <summary>
    /// The number of in-process partial-application values allocated.
    /// </summary>
    public long PartialApplicationAllocationCount => Counters.PartialApplicationAllocationCount;

    /// <summary>
    /// The number of saturated applications entered without an intermediate function value.
    /// </summary>
    public long DirectSaturatedApplicationCount => Counters.DirectSaturatedApplicationCount;

    /// <summary>
    /// The number of partial applications forced to their canonical Pine value.
    /// </summary>
    public long PartialApplicationMaterializationCount => Counters.PartialApplicationMaterializationCount;
}
