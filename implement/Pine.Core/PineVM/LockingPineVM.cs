namespace Pine.Core.PineVM;

public class LockingPineVM : IPineVM
{
    private readonly IPineVM pineVM;

    private readonly object pineVMLock = new();

    public LockingPineVM(IPineVM pineVM)
    {
        this.pineVM = pineVM;
    }

    public Result<string, PineValue> EvaluateExpression(Expression expression, PineValue environment)
    {
        lock (pineVMLock)
        {
            return pineVM.EvaluateExpression(expression, environment);
        }
    }
}
