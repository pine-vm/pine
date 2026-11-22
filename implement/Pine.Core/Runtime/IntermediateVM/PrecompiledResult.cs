using Pine.Core.PineLanguage;

namespace Pine.Core.Runtime.IntermediateVM;

/// <summary>
/// Represents the result returned by a precompiled function handler.
/// The VM either uses the value directly or continues with another parse-and-eval cycle.
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
    public sealed record ContinueEval(
        PineValue EnvironmentValue,
        PineValue ExpressionValue)
        : PrecompiledResult;
}

