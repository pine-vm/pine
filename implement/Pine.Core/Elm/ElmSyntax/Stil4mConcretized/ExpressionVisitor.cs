namespace Pine.Core.Elm.ElmSyntax.Stil4mConcretized;

/// <summary>
/// Visitor interface for Elm concretized expression compilation.
/// Forces handling of all expression types at compile time, making it clear which expressions are supported.
/// </summary>
/// <typeparam name="TContext">The type of context passed to visitor methods.</typeparam>
/// <typeparam name="TResult">The result type returned by visitor methods.</typeparam>
public interface IExpressionVisitor<TContext, TResult>
{
    /// <summary>
    /// Visit an integer literal expression.
    /// </summary>
    TResult VisitInteger(Expression.Integer expr, TContext context);

    /// <summary>
    /// Visit a string literal expression.
    /// </summary>
    TResult VisitLiteral(Expression.Literal expr, TContext context);

    /// <summary>
    /// Visit a character literal expression.
    /// </summary>
    TResult VisitCharLiteral(Expression.CharLiteral expr, TContext context);

    /// <summary>
    /// Visit a function or value reference expression.
    /// </summary>
    TResult VisitFunctionOrValue(Expression.FunctionOrValue expr, TContext context);

    /// <summary>
    /// Visit a function application expression.
    /// </summary>
    TResult VisitApplication(Expression.Application expr, TContext context);

    /// <summary>
    /// Visit a list expression.
    /// </summary>
    TResult VisitListExpr(Expression.ListExpr expr, TContext context);

    /// <summary>
    /// Visit an operator application expression.
    /// </summary>
    TResult VisitOperatorApplication(Expression.OperatorApplication expr, TContext context);

    /// <summary>
    /// Visit a parenthesized expression.
    /// </summary>
    TResult VisitParenthesizedExpression(Expression.ParenthesizedExpression expr, TContext context);

    /// <summary>
    /// Visit a negation expression.
    /// </summary>
    TResult VisitNegation(Expression.Negation expr, TContext context);

    /// <summary>
    /// Visit an if-then-else block expression.
    /// </summary>
    TResult VisitIfBlock(Expression.IfBlock expr, TContext context);

    /// <summary>
    /// Visit a case expression.
    /// </summary>
    TResult VisitCaseExpression(Expression.CaseExpression expr, TContext context);

    /// <summary>
    /// Visit a let expression.
    /// </summary>
    TResult VisitLetExpression(Expression.LetExpression expr, TContext context);

    /// <summary>
    /// Visit a tuple expression.
    /// </summary>
    TResult VisitTupledExpression(Expression.TupledExpression expr, TContext context);

    /// <summary>
    /// Visit a record expression.
    /// </summary>
    TResult VisitRecordExpr(Expression.RecordExpr expr, TContext context);

    /// <summary>
    /// Visit a record access expression (e.g., record.field).
    /// </summary>
    TResult VisitRecordAccess(Expression.RecordAccess expr, TContext context);

    /// <summary>
    /// Visit a record access function expression (e.g., .field).
    /// </summary>
    TResult VisitRecordAccessFunction(Expression.RecordAccessFunction expr, TContext context);

    /// <summary>
    /// Visit a record update expression.
    /// </summary>
    TResult VisitRecordUpdateExpression(Expression.RecordUpdateExpression expr, TContext context);

    /// <summary>
    /// Visit a lambda expression.
    /// </summary>
    TResult VisitLambdaExpression(Expression.LambdaExpression expr, TContext context);

    /// <summary>
    /// Visit a hex literal expression.
    /// </summary>
    TResult VisitHex(Expression.Hex expr, TContext context);

    /// <summary>
    /// Visit a prefix operator expression.
    /// </summary>
    TResult VisitPrefixOperator(Expression.PrefixOperator expr, TContext context);

    /// <summary>
    /// Visit a float literal expression.
    /// </summary>
    TResult VisitFloatable(Expression.Floatable expr, TContext context);

    /// <summary>
    /// Visit a unit (empty tuple) expression.
    /// </summary>
    TResult VisitUnitExpr(Expression.UnitExpr expr, TContext context);
}

/// <summary>
/// Base class for expression visitors with default implementations that throw for unsupported expressions.
/// Override specific methods to implement compilation for supported expression types.
/// </summary>
/// <typeparam name="TContext">The type of context passed to visitor methods.</typeparam>
/// <typeparam name="TResult">The result type returned by visitor methods.</typeparam>
public abstract class ExpressionVisitorBase<TContext, TResult> : IExpressionVisitor<TContext, TResult>
{
    /// <summary>
    /// Visit an expression and dispatch to the appropriate visitor method.
    /// </summary>
    public TResult Visit(Expression expression, TContext context) =>
        expression switch
        {
            Expression.Integer e =>
            VisitInteger(e, context),

            Expression.Literal e =>
            VisitLiteral(e, context),

            Expression.CharLiteral e =>
            VisitCharLiteral(e, context),

            Expression.FunctionOrValue e =>
            VisitFunctionOrValue(e, context),

            Expression.Application e =>
            VisitApplication(e, context),

            Expression.ListExpr e =>
            VisitListExpr(e, context),

            Expression.OperatorApplication e =>
            VisitOperatorApplication(e, context),

            Expression.ParenthesizedExpression e =>
            VisitParenthesizedExpression(e, context),

            Expression.Negation e =>
            VisitNegation(e, context),

            Expression.IfBlock e =>
            VisitIfBlock(e, context),

            Expression.CaseExpression e =>
            VisitCaseExpression(e, context),

            Expression.LetExpression e =>
            VisitLetExpression(e, context),

            Expression.TupledExpression e =>
            VisitTupledExpression(e, context),

            Expression.RecordExpr e =>
            VisitRecordExpr(e, context),

            Expression.RecordAccess e =>
            VisitRecordAccess(e, context),

            Expression.RecordAccessFunction e =>
            VisitRecordAccessFunction(e, context),

            Expression.RecordUpdateExpression e =>
            VisitRecordUpdateExpression(e, context),

            Expression.LambdaExpression e =>
            VisitLambdaExpression(e, context),

            Expression.Hex e =>
            VisitHex(e, context),

            Expression.PrefixOperator e =>
            VisitPrefixOperator(e, context),

            Expression.Floatable e =>
            VisitFloatable(e, context),

            Expression.UnitExpr e =>
            VisitUnitExpr(e, context),

            _ =>
            VisitUnknown(expression, context)
        };

    /// <summary>
    /// Default handler for unknown expression types.
    /// </summary>
    protected virtual TResult VisitUnknown(Expression expr, TContext context) =>
        throw new System.NotImplementedException($"Expression type not supported: {expr.GetType().Name}");

    /// <inheritdoc/>
    public virtual TResult VisitInteger(Expression.Integer expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitLiteral(Expression.Literal expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitCharLiteral(Expression.CharLiteral expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitFunctionOrValue(Expression.FunctionOrValue expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitApplication(Expression.Application expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitListExpr(Expression.ListExpr expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitOperatorApplication(Expression.OperatorApplication expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitParenthesizedExpression(Expression.ParenthesizedExpression expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitNegation(Expression.Negation expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitIfBlock(Expression.IfBlock expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitCaseExpression(Expression.CaseExpression expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitLetExpression(Expression.LetExpression expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitTupledExpression(Expression.TupledExpression expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitRecordExpr(Expression.RecordExpr expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitRecordAccess(Expression.RecordAccess expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitRecordAccessFunction(Expression.RecordAccessFunction expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitRecordUpdateExpression(Expression.RecordUpdateExpression expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitLambdaExpression(Expression.LambdaExpression expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitHex(Expression.Hex expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitPrefixOperator(Expression.PrefixOperator expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitFloatable(Expression.Floatable expr, TContext context) =>
        VisitUnknown(expr, context);

    /// <inheritdoc/>
    public virtual TResult VisitUnitExpr(Expression.UnitExpr expr, TContext context) =>
        VisitUnknown(expr, context);
}
