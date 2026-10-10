using Pine.Core.Interpreter.IntermediateVM;

namespace Pine.Core.Interpreter;

/// <summary>Evaluation-local counters shared by direct interpreters performing the same kind of work.</summary>
internal sealed class DirectInterpreterCounters
{
    public long InvocationCount;

    public long ExpressionCount;

    public long LiteralCount;

    public long ListCount;

    public long EvalCount;

    public long BuiltinCount;

    public long ConditionalCount;

    public long EnvironmentCount;

    public long BuildListItemCount;

    public PerformanceCounters Snapshot() =>
        new(
            InvocationCount: 0,
            BuildListCount: ListCount,
            LoopIterationCount: 0,
            InstructionCount: 0,
            BuildListItemCount: BuildListItemCount,
            DirectInterpreterInvocationCount: InvocationCount,
            DirectInterpreterExpressionCount: ExpressionCount,
            DirectInterpreterLiteralCount: LiteralCount,
            DirectInterpreterListCount: ListCount,
            DirectInterpreterEvalCount: EvalCount,
            DirectInterpreterBuiltinCount: BuiltinCount,
            DirectInterpreterConditionalCount: ConditionalCount,
            DirectInterpreterEnvironmentCount: EnvironmentCount);
}
