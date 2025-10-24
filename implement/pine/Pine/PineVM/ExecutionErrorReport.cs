using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;

namespace Pine.PineVM;

public record ExecutionErrorReport(
    Expression FrameExpression,
    PineValue EnvironmentValue,
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

        var envHash =
            mutableCacheValueHash.GetHash(errorReport.EnvironmentValue);

        var envHashBase16 = Convert.ToHexStringLower(envHash.Span);

        yield return
            "Instruction " + errorReport.FrameInstructionPointer +
            " in expression: " + exprHashBase16[..8] + " for environment " +
            envHashBase16[..8];

        var specializationText =
            errorReport.Instructions.TrackEnvConstraint is { } trackEnvConstraint
            ? "specialized with " + trackEnvConstraint.HashBase16[0..8]
            : "not specialized";

        yield return
            specializationText + " has " +
            errorReport.Instructions.Instructions.Count + " instructions";
    }
}
