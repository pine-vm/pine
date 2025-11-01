using Pine.Core;
using System.Collections.Generic;

namespace Pine.PineVM;

public record EvaluationReport(
    PineValue ExpressionValue,
    Expression Expression,
    StackFrameInput Input,
    long InstructionCount,
    long InvocationCount,
    long LoopIterationCount,
    Core.Internal.PineValueInProcess ReturnValue,
    IReadOnlyList<Expression> StackTrace);

