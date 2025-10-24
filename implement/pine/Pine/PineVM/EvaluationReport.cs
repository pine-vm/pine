using Pine.Core;
using System.Collections.Generic;

namespace Pine.PineVM;

public record EvaluationReport(
    PineValue ExpressionValue,
    Expression Expression,
    PineValue Environment,
    long InstructionCount,
    long InvocationCount,
    long LoopIterationCount,
    PineValue ReturnValue,
    IReadOnlyList<Expression> StackTrace);

