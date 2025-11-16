namespace Pine.Core.Interpreter.IntermediateVM;

public abstract record PrecompiledResult
{
    public sealed record FinalValue(
        PineValue Value,
        long StackFrameCount)
        : PrecompiledResult;

    public sealed record ContinueParseAndEval(
        PineValue EnvironmentValue,
        PineValue ExpressionValue)
        : PrecompiledResult;

    public sealed record StepwiseSpecialization(
        ApplyStepwise Stepwise)
        : PrecompiledResult;
}

