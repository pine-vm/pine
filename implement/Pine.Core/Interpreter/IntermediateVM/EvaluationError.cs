using Pine.Core.Addressing;
using Pine.Core.CommonEncodings;
using Pine.Core.Internal;
using System;
using System.Collections.Generic;
using System.Text;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Identifies a configured evaluation quota.
/// </summary>
public enum EvaluationQuotaKind
{
    /// <summary>
    /// The number of invocations.
    /// </summary>
    InvocationCount,

    /// <summary>
    /// The number of loop iterations, counted as backward jumps.
    /// </summary>
    LoopIterationCount,

    /// <summary>
    /// The number of live stack frames.
    /// </summary>
    StackDepth,
}

/// <summary>
/// Describes why an evaluation stopped without producing a value.
/// </summary>
public abstract record EvaluationErrorReason
{
    /// <summary>
    /// A configured evaluation quota was exhausted.
    /// </summary>
    public sealed record QuotaExhausted(EvaluationQuotaKind QuotaKind, int Limit) : EvaluationErrorReason;

    /// <summary>
    /// The caller requested cancellation.
    /// </summary>
    public sealed record CancellationRequested : EvaluationErrorReason;

    /// <summary>
    /// A value supplied to an eval instruction could not be parsed as a Pine expression.
    /// </summary>
    public sealed record ParseExpressionFailed(
        string ParseError,
        PineValue ExpressionValue,
        PineValueInProcess EnvironmentValue)
        : EvaluationErrorReason;

    /// <summary>
    /// Execution reached the end of a frame without an explicit return instruction.
    /// </summary>
    public sealed record InstructionPointerOutOfBounds : EvaluationErrorReason;
}

/// <summary>
/// A cheap snapshot of a live evaluation frame. Values and instructions are retained
/// by reference so callers can perform deeper analysis only when needed.
/// </summary>
public sealed record EvaluationStackTraceFrame(
    Expression Expression,
    StackFrameInput? Input,
    StackFrameInstructions? Instructions,
    int InstructionPointer);

/// <summary>
/// Structured information returned when intermediate-VM evaluation stops without a value.
/// The stack is ordered from the innermost frame to the outermost frame.
/// </summary>
public sealed record EvaluationError(
    EvaluationErrorReason Reason,
    IReadOnlyList<EvaluationStackTraceFrame> StackTrace,
    PerformanceCounters Counters)
{
    /// <summary>
    /// Renders an evaluation error for human-readable output. Expression hashing and
    /// Pine-value descriptions are intentionally deferred until this function is called.
    /// </summary>
    public static string RenderDisplayString(EvaluationError error)
    {
        var reasonText =
            error.Reason switch
            {
                EvaluationErrorReason.QuotaExhausted quotaExhausted =>
                RenderQuotaExhausted(quotaExhausted),

                EvaluationErrorReason.CancellationRequested =>
                "Evaluation cancellation requested.",

                EvaluationErrorReason.ParseExpressionFailed parseExpressionFailed =>
                "Failed to parse expression from value: " + parseExpressionFailed.ParseError +
                " - expressionValue is " +
                PineVM.DescribeValueForErrorMessage(parseExpressionFailed.ExpressionValue) +
                " - environmentValue is " +
                PineVM.DescribeValueForErrorMessage(parseExpressionFailed.EnvironmentValue.Evaluate()),

                EvaluationErrorReason.InstructionPointerOutOfBounds =>
                "Instruction pointer out of bounds. Missing explicit return instruction.",

                _ =>
                throw new NotImplementedException(
                    nameof(RenderDisplayString) +
                    " does not handle evaluation error reason variant: " +
                    error.Reason.GetType().Name),
            };

        var text = new StringBuilder(reasonText);

        if (error.StackTrace.Count > 0)
        {
            text.AppendLine();
            text.AppendLine("Last stack frames (innermost first):");

            foreach (var frame in error.StackTrace)
            {
                var expressionValue = ExpressionEncoding.EncodeExpressionAsValue(frame.Expression);
                var expressionHash = PineValueHashTree.ComputeHash(expressionValue);

                text.Append("  ");
                text.Append(Convert.ToHexStringLower(expressionHash.Span)[..8]);
                text.Append(" at instruction ");
                text.Append(CommandLineInterface.FormatIntegerForDisplay(frame.InstructionPointer));
                text.AppendLine();
            }
        }

        text.Append("Stack frames: ");
        text.Append(CommandLineInterface.FormatIntegerForDisplay(error.StackTrace.Count));
        text.Append(" - instructions: ");
        text.Append(CommandLineInterface.FormatIntegerForDisplay(error.Counters.InstructionCount));
        text.Append(" - invocations: ");
        text.Append(CommandLineInterface.FormatIntegerForDisplay(error.Counters.InvocationCount));
        text.Append(" - build lists: ");
        text.Append(CommandLineInterface.FormatIntegerForDisplay(error.Counters.BuildListCount));
        text.Append(" - loop iterations: ");
        text.Append(CommandLineInterface.FormatIntegerForDisplay(error.Counters.LoopIterationCount));

        return text.ToString();
    }

    private static string RenderQuotaExhausted(EvaluationErrorReason.QuotaExhausted quotaExhausted)
    {
        var quotaDescription =
            quotaExhausted.QuotaKind switch
            {
                EvaluationQuotaKind.InvocationCount => "Invocation count",
                EvaluationQuotaKind.LoopIterationCount => "Loop iteration count",
                EvaluationQuotaKind.StackDepth => "Stack depth",

                _ =>
                throw new NotImplementedException(
                    nameof(RenderQuotaExhausted) +
                    " does not handle evaluation quota kind: " +
                    quotaExhausted.QuotaKind),
            };

        return
            quotaDescription + " limit exceeded: " +
            CommandLineInterface.FormatIntegerForDisplay(quotaExhausted.Limit);
    }

    /// <inheritdoc/>
    public override string ToString() => RenderDisplayString(this);
}
