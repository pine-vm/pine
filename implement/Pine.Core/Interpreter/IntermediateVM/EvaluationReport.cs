using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Profiling information and result value produced from evaluating an expression in the intermediate VM.
/// </summary>
/// <param name="ExpressionValue">Encoded representation of the evaluated expression.</param>
/// <param name="Expression">The evaluated expression.</param>
/// <param name="Input">The input values supplied to the expression.</param>
/// <param name="InstructionCount">The total number of VM instructions executed.</param>
/// <param name="InvocationCount">The total number of runtime invocations performed, including parse-and-eval and direct stack-frame invocations.</param>
/// <param name="BuildListCount">The total number of executed <c>Build_List</c> and <c>Build_List_Tagged_Const</c> instructions.</param>
/// <param name="LoopIterationCount">The total number of loop iterations reported by the active stack frames.</param>
/// <param name="ReturnValue">The returned value in the in-process representation.</param>
/// <param name="StackTrace">The captured stack trace at the time of reporting.</param>
public record EvaluationReport(
    PineValue ExpressionValue,
    Expression Expression,
    StackFrameInput Input,
    long InstructionCount,
    long InvocationCount,
    long BuildListCount,
    long LoopIterationCount,
    Internal.PineValueInProcess ReturnValue,
    IReadOnlyList<Expression> StackTrace);
