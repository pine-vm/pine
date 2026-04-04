namespace Pine.Core.Interpreter.IntermediateVM;

/// <summary>
/// Represents the result returned by a precompiled function handler.
/// The VM dispatches on the concrete variant to decide whether to use the value directly,
/// continue with another parse-and-eval cycle, or proceed stepwise.
/// </summary>
public abstract record PrecompiledResult
{
    /// <summary>
    /// Indicates that the precompiled function computed a final value directly,
    /// without requiring further evaluation by the VM.
    /// </summary>
    public sealed record FinalValue(
        PineValue Value,
        long StackFrameCount)
        : PrecompiledResult;

    /// <summary>
    /// Indicates that the precompiled function wants to continue by parsing and evaluating
    /// another expression in a new environment.
    /// </summary>
    public sealed record ContinueParseAndEval(
        PineValue EnvironmentValue,
        PineValue ExpressionValue)
        : PrecompiledResult;

    /// <summary>
    /// Indicates that the precompiled function provides a stepwise continuation-based evaluation,
    /// managed by an <see cref="ApplyStepwise"/> instance.
    /// </summary>
    public sealed record StepwiseSpecialization(
        ApplyStepwise Stepwise)
        : PrecompiledResult;
}

