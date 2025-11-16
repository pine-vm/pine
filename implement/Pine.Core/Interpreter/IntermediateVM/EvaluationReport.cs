using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

public record EvaluationReport(
    PineValue ExpressionValue,
    Expression Expression,
    StackFrameInput Input,
    long InstructionCount,
    long InvocationCount,
    long LoopIterationCount,
    Internal.PineValueInProcess ReturnValue,
    IReadOnlyList<Expression> StackTrace);

