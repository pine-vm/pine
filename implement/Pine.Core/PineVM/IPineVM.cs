namespace Pine.Core.PineVM;

/// <summary>
/// Common interface for Pine virtual machine implementations.
/// </summary>
public interface IPineVM
{
    /// <summary>
    /// Evaluate the given <paramref name="expression"/> in the context of the provided <paramref name="environment"/>.
    /// </summary>
    Result<string, PineValue> EvaluateExpression(
        Expression expression,
        PineValue environment);
}
