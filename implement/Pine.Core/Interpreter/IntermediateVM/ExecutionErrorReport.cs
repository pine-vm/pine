using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using System;
using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Captures the expression, inputs, and instruction pointer of the stack frame where intermediate-VM execution failed.
/// </summary>
public record ExecutionErrorReport(
    Expression FrameExpression,
    StackFrameInput InputValues,
    StackFrameInstructions Instructions,
    int FrameInstructionPointer);

/// <summary>
/// Provides diagnostic formatting helpers for execution error reports.
/// </summary>
public static class ExecutionErrorReportExtensions
{

    /// <summary>
    /// Formats a short textual description of the failing frame, including the instruction pointer, environment, and specialization state.
    /// </summary>
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
            ?
            "specialized with " + trackEnvConstraint.HashBase16[0..8]
            :
            "not specialized";

        yield return
            specializationText + " has " +
            errorReport.Instructions.Instructions.Count + " instructions";
    }
}
