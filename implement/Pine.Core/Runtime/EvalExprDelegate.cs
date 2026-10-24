using Pine.Core.Functional;
using Pine.Core.PineLanguage;

namespace Pine.Core.Runtime;


/// <summary>
/// Represents a Pine expression evaluator that runs an expression against an environment and returns either an error or the resulting value.
/// </summary>
public delegate Result<string, PineValue> EvalExprDelegate(Expression expression, PineValue environment);


