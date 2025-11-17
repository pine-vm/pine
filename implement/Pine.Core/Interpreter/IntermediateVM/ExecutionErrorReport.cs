using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

public record ExecutionErrorReport(
    Expression FrameExpression,
    StackFrameInput InputValues,
    StackFrameInstructions Instructions,
    int FrameInstructionPointer);

public static class ExecutionErrorReportExtensions
{

    public static IEnumerable<string> DisplayText(
        this ExecutionErrorReport errorReport,
        ConcurrentPineValueHashCache mutableCacheValueHash)
    {
        var expressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(errorReport.FrameExpression);

        var exprHash =
            mutableCacheValueHash.GetHash(expressionValue);

        var exprHashBase16 = Convert.ToHexStringLower(exprHash.Span);

        yield return
            "Instruction " + errorReport.FrameInstructionPointer +
            " in expression: " + exprHashBase16[..8] + " for environment " +
            errorReport.InputValues.ToString();

        var specializationText =
            errorReport.Instructions.TrackEnvConstraint is { } trackEnvConstraint
            ? "specialized with " + trackEnvConstraint.HashBase16[0..8]
            : "not specialized";

        yield return
            specializationText + " has " +
            errorReport.Instructions.Instructions.Count + " instructions";
    }
}
