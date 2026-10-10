using System.Collections.Generic;
using System.Text.Json.Serialization;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Aggregated performance counters collected during expression evaluation.
/// </summary>
/// <param name="InvocationCount">The total number of runtime invocations performed, including parse-and-eval and direct stack-frame invocations.</param>
/// <param name="BuildListCount">The total number of VM list-building instructions and direct-interpreter list expression evaluations.</param>
/// <param name="LoopIterationCount">The total number of loop iterations reported by the active stack frames.</param>
/// <param name="InstructionCount">The total number of VM instructions executed.</param>
/// <param name="ExpressionTemplatePlanParseCount">The number of concrete values inspected to recognize and validate an expression-template plan.</param>
/// <param name="DeferredTemplateValueAllocationCount">The number of compact intermediate template values allocated.</param>
/// <param name="TemplateDirectInvocationCount">The number of terminal expressions invoked directly, bypassing intermediate encoded-expression construction.</param>
/// <param name="DeferredTemplateValueMaterializationCount">The number of deferred template values reconstructed as concrete Pine values.</param>
/// <param name="BuildListItemCount">The number of immediate item slots allocated by VM list builds and direct list evaluation, including literal prefixes and attempted builds.</param>
/// <param name="DirectInterpreterInvocationCount">The number of external entries into a direct interpreter, excluding recursive evaluation.</param>
/// <param name="DirectInterpreterExpressionCount">The number of expression evaluations performed by direct interpreters, including recursive evaluations.</param>
/// <param name="DirectInterpreterLiteralCount">The number of literal expression evaluations in direct interpreters.</param>
/// <param name="DirectInterpreterListCount">The number of list expression evaluations in direct interpreters.</param>
/// <param name="DirectInterpreterEvalCount">The number of parse-and-eval expression evaluations in direct interpreters, including cache hits.</param>
/// <param name="DirectInterpreterBuiltinCount">The number of builtin expression evaluations in direct interpreters.</param>
/// <param name="DirectInterpreterConditionalCount">The number of conditional expression evaluations in direct interpreters.</param>
/// <param name="DirectInterpreterEnvironmentCount">The number of environment expression evaluations in direct interpreters.</param>
public readonly record struct PerformanceCounters(
    [property: JsonPropertyOrder(0)] long InvocationCount,
    [property: JsonPropertyOrder(3)] long BuildListCount,
    [property: JsonPropertyOrder(1)] long LoopIterationCount,
    [property: JsonPropertyOrder(2)] long InstructionCount,
    [property: JsonPropertyOrder(5)] long ExpressionTemplatePlanParseCount = 0,
    [property: JsonPropertyOrder(6)] long DeferredTemplateValueAllocationCount = 0,
    [property: JsonPropertyOrder(7)] long TemplateDirectInvocationCount = 0,
    [property: JsonPropertyOrder(8)] long DeferredTemplateValueMaterializationCount = 0,
    [property: JsonPropertyOrder(4)] long BuildListItemCount = 0,
    [property: JsonPropertyOrder(9)] long DirectInterpreterInvocationCount = 0,
    [property: JsonPropertyOrder(10)] long DirectInterpreterExpressionCount = 0,
    [property: JsonPropertyOrder(11)] long DirectInterpreterLiteralCount = 0,
    [property: JsonPropertyOrder(12)] long DirectInterpreterListCount = 0,
    [property: JsonPropertyOrder(13)] long DirectInterpreterEvalCount = 0,
    [property: JsonPropertyOrder(14)] long DirectInterpreterBuiltinCount = 0,
    [property: JsonPropertyOrder(15)] long DirectInterpreterConditionalCount = 0,
    [property: JsonPropertyOrder(16)] long DirectInterpreterEnvironmentCount = 0)
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
            ExpressionTemplatePlanParseCount: a.ExpressionTemplatePlanParseCount + b.ExpressionTemplatePlanParseCount,
            DeferredTemplateValueAllocationCount:
            a.DeferredTemplateValueAllocationCount + b.DeferredTemplateValueAllocationCount,
            TemplateDirectInvocationCount: a.TemplateDirectInvocationCount + b.TemplateDirectInvocationCount,
            DeferredTemplateValueMaterializationCount:
            a.DeferredTemplateValueMaterializationCount + b.DeferredTemplateValueMaterializationCount,
            BuildListItemCount: a.BuildListItemCount + b.BuildListItemCount,
            DirectInterpreterInvocationCount: a.DirectInterpreterInvocationCount + b.DirectInterpreterInvocationCount,
            DirectInterpreterExpressionCount: a.DirectInterpreterExpressionCount + b.DirectInterpreterExpressionCount,
            DirectInterpreterLiteralCount: a.DirectInterpreterLiteralCount + b.DirectInterpreterLiteralCount,
            DirectInterpreterListCount: a.DirectInterpreterListCount + b.DirectInterpreterListCount,
            DirectInterpreterEvalCount: a.DirectInterpreterEvalCount + b.DirectInterpreterEvalCount,
            DirectInterpreterBuiltinCount: a.DirectInterpreterBuiltinCount + b.DirectInterpreterBuiltinCount,
            DirectInterpreterConditionalCount: a.DirectInterpreterConditionalCount + b.DirectInterpreterConditionalCount,
            DirectInterpreterEnvironmentCount: a.DirectInterpreterEnvironmentCount + b.DirectInterpreterEnvironmentCount);

    /// <summary>Returns counter deltas between two snapshots of the same evaluation.</summary>
    public static PerformanceCounters Subtract(PerformanceCounters a, PerformanceCounters b) =>
        new(
            InvocationCount: a.InvocationCount - b.InvocationCount,
            BuildListCount: a.BuildListCount - b.BuildListCount,
            LoopIterationCount: a.LoopIterationCount - b.LoopIterationCount,
            InstructionCount: a.InstructionCount - b.InstructionCount,
            ExpressionTemplatePlanParseCount: a.ExpressionTemplatePlanParseCount - b.ExpressionTemplatePlanParseCount,
            DeferredTemplateValueAllocationCount:
            a.DeferredTemplateValueAllocationCount - b.DeferredTemplateValueAllocationCount,
            TemplateDirectInvocationCount: a.TemplateDirectInvocationCount - b.TemplateDirectInvocationCount,
            DeferredTemplateValueMaterializationCount:
            a.DeferredTemplateValueMaterializationCount - b.DeferredTemplateValueMaterializationCount,
            BuildListItemCount: a.BuildListItemCount - b.BuildListItemCount,
            DirectInterpreterInvocationCount: a.DirectInterpreterInvocationCount - b.DirectInterpreterInvocationCount,
            DirectInterpreterExpressionCount: a.DirectInterpreterExpressionCount - b.DirectInterpreterExpressionCount,
            DirectInterpreterLiteralCount: a.DirectInterpreterLiteralCount - b.DirectInterpreterLiteralCount,
            DirectInterpreterListCount: a.DirectInterpreterListCount - b.DirectInterpreterListCount,
            DirectInterpreterEvalCount: a.DirectInterpreterEvalCount - b.DirectInterpreterEvalCount,
            DirectInterpreterBuiltinCount: a.DirectInterpreterBuiltinCount - b.DirectInterpreterBuiltinCount,
            DirectInterpreterConditionalCount: a.DirectInterpreterConditionalCount - b.DirectInterpreterConditionalCount,
            DirectInterpreterEnvironmentCount: a.DirectInterpreterEnvironmentCount - b.DirectInterpreterEnvironmentCount);

    /// <summary>Sums the counters in a sequence; an empty sequence has zero counters.</summary>
    public static PerformanceCounters Aggregate(IEnumerable<PerformanceCounters> counters)
    {
        var total = default(PerformanceCounters);

        foreach (var counter in counters)
            total = Add(total, counter);

        return total;
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
    /// <summary>Disjoint work totals for this evaluation, including work performed by direct interpreters.</summary>
    public PerformanceCountersByOrigin CountersByOrigin { get; init; }

    /// <summary>
    /// The total number of VM instructions executed.
    /// </summary>
    public long InstructionCount => Counters.InstructionCount;

    /// <summary>
    /// The total number of runtime invocations performed.
    /// </summary>
    public long InvocationCount => Counters.InvocationCount;

    /// <summary>
    /// The total number of VM list builds and direct-interpreter list evaluations.
    /// </summary>
    public long BuildListCount => Counters.BuildListCount;

    /// <summary>
    /// The total number of loop iterations.
    /// </summary>
    public long LoopIterationCount => Counters.LoopIterationCount;

    /// <summary>
    /// The number of concrete values inspected to recognize and validate an expression-template plan.
    /// </summary>
    public long ExpressionTemplatePlanParseCount => Counters.ExpressionTemplatePlanParseCount;

    /// <summary>
    /// The number of compact intermediate template values allocated.
    /// </summary>
    public long DeferredTemplateValueAllocationCount => Counters.DeferredTemplateValueAllocationCount;

    /// <summary>
    /// The number of terminal expressions invoked directly, bypassing intermediate encoded-expression construction.
    /// </summary>
    public long TemplateDirectInvocationCount => Counters.TemplateDirectInvocationCount;

    /// <summary>
    /// The number of deferred template values reconstructed as concrete Pine values.
    /// </summary>
    public long DeferredTemplateValueMaterializationCount => Counters.DeferredTemplateValueMaterializationCount;

    /// <summary>The number of immediate item slots populated by VM list builds and direct list evaluation, including literal prefixes.</summary>
    public long BuildListItemCount => Counters.BuildListItemCount;

    /// <summary>The number of external entries into a direct interpreter, excluding recursive evaluation.</summary>
    public long DirectInterpreterInvocationCount => Counters.DirectInterpreterInvocationCount;

    /// <summary>The number of expression evaluations performed by direct interpreters, including recursive evaluations.</summary>
    public long DirectInterpreterExpressionCount => Counters.DirectInterpreterExpressionCount;

    /// <summary>The number of literal expression evaluations in direct interpreters.</summary>
    public long DirectInterpreterLiteralCount => Counters.DirectInterpreterLiteralCount;

    /// <summary>The number of list expression evaluations in direct interpreters.</summary>
    public long DirectInterpreterListCount => Counters.DirectInterpreterListCount;

    /// <summary>The number of parse-and-eval expression evaluations in direct interpreters, including cache hits.</summary>
    public long DirectInterpreterEvalCount => Counters.DirectInterpreterEvalCount;

    /// <summary>The number of builtin expression evaluations in direct interpreters.</summary>
    public long DirectInterpreterBuiltinCount => Counters.DirectInterpreterBuiltinCount;

    /// <summary>The number of conditional expression evaluations in direct interpreters.</summary>
    public long DirectInterpreterConditionalCount => Counters.DirectInterpreterConditionalCount;

    /// <summary>The number of environment expression evaluations in direct interpreters.</summary>
    public long DirectInterpreterEnvironmentCount => Counters.DirectInterpreterEnvironmentCount;
}
