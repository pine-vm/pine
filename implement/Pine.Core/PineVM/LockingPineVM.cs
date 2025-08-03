namespace Pine.Core.PineVM;

/// <summary>
/// Wrapper around an <see cref="IPineVM"/> instance that adds a lock on invocation of the
/// <see cref="IPineVM.EvaluateExpression"/> method to ensure thread safety.
/// </summary>
public class LockingPineVM(IPineVM pineVM) : IPineVM
{
    private readonly System.Threading.Lock _pineVMLock = new();

    /// <summary>
    /// Forwards the evaluation of an expression to the underlying PineVM instance,
    /// while ensuring thread safety.
    /// </summary>
    public Result<string, PineValue> EvaluateExpression(Expression expression, PineValue environment)
    {
        lock (_pineVMLock)
        {
            return pineVM.EvaluateExpression(expression, environment);
        }
    }
}
