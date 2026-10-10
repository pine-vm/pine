using System;
using System.Collections.Generic;

namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>Observes evaluation boundaries and backward jumps, not every instruction.</summary>
public delegate void ReportEvaluationEvent(in EvaluationEvent evaluationEvent);

/// <summary>Evaluation boundaries observable without a callback on every instruction.</summary>
public enum EvaluationEventKind
{
    /// <summary>A new frame was pushed, including a tail-call replacement.</summary>
    FrameEntered,
    /// <summary>A frame is returning or about to be replaced by a tail call.</summary>
    FrameExited,
    /// <summary>A backward jump incremented the current frame's loop counter.</summary>
    BackwardJump,
    /// <summary>Evaluation is stopping without a return value.</summary>
    EvaluationStopped,
}

/// <summary>
/// Cheap live evaluation information. Queries must be consumed during the callback.
/// Stack frames are yielded lazily, starting at the current frame; locals are copied only on request.
/// </summary>
/// <param name="Kind">The observed evaluation boundary.</param>
/// <param name="FrameIndex">Evaluation-local identity of the current frame.</param>
/// <param name="Expression">Expression executed by the current frame.</param>
/// <param name="InstructionPointer">Current instruction offset.</param>
/// <param name="StackDepth">Number of currently active frames.</param>
/// <param name="FrameInstructionCount">Instructions executed by this frame, excluding child frames.</param>
/// <param name="FrameLoopIterationCount">Backward jumps executed by this frame.</param>
/// <param name="LoadCounters">Queries evaluation-wide counters during the callback.</param>
/// <param name="LoadStackTrace">Queries live frames lazily, current frame first, during the callback.</param>
/// <param name="StopReason">Reason for an evaluation-stop event; null for other events.</param>
/// <param name="Instructions">Compiled body containing the reported instruction pointer.</param>
/// <param name="LoadCountersByOrigin">Queries disjoint work totals during the callback.</param>
public readonly record struct EvaluationEvent(
    EvaluationEventKind Kind,
    long FrameIndex,
    Expression Expression,
    int InstructionPointer,
    int StackDepth,
    long FrameInstructionCount,
    long FrameLoopIterationCount,
    Func<PerformanceCounters> LoadCounters,
    Func<IEnumerable<EvaluationStackTraceFrame>> LoadStackTrace,
    EvaluationErrorReason? StopReason = null,
    StackFrameInstructions? Instructions = null,
    Func<PerformanceCountersByOrigin>? LoadCountersByOrigin = null);
