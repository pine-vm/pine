using Pine.Core.CodeGen;
using Pine.Core.CommonEncodings;
using System.Collections.Generic;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Helper methods for creating builtin Pine kernel operations.
/// </summary>
public static class BuiltinHelpers
{
    /// <summary>
    /// Builds an expression that accesses a parameter at the given index.
    /// <para>
    /// The runtime environment is uniformly <c>[envFunctions, arg0, arg1, ...]</c>,
    /// so parameters live at path <c>[1 + parameterIndex]</c>.
    /// </para>
    /// </summary>
    public static Expression BuildPathToParameter(int parameterIndex) =>
        ExpressionBuilder.BuildExpressionForPathInExpression(
            [1 + parameterIndex],
            Expression.EnvironmentInstance);

    /// <summary>
    /// Apply the builtin 'skip' function with a constant count.
    /// Equivalent to: Pine_kernel.skip [ count, expr ]
    /// </summary>
    public static Expression ApplyBuiltinSkip(int count, Expression expr) =>
        Expression.BuiltinInst(
            nameof(BuiltinFunction.skip),
            Expression.ListInst(
                [
                Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(count)),
                expr
                ]));

    /// <summary>
    /// Apply the builtin 'head' function.
    /// Equivalent to: Pine_kernel.head expr
    /// </summary>
    public static Expression ApplyBuiltinHead(Expression expr) =>
        Expression.BuiltinInst(
            nameof(BuiltinFunction.head),
            expr);

    /// <summary>
    /// Apply the builtin 'equal' function for binary comparison.
    /// Equivalent to: Pine_kernel.equal [ left, right ]
    /// </summary>
    public static Expression ApplyBuiltinEqualBinary(Expression left, Expression right) =>
        Expression.BuiltinInst(
            nameof(BuiltinFunction.equal),
            Expression.ListInst([left, right]));

    /// <summary>
    /// Apply the builtin 'length' function.
    /// Equivalent to: Pine_kernel.length expr
    /// </summary>
    public static Expression ApplyBuiltinLength(Expression expr) =>
        Expression.BuiltinInst(
            nameof(BuiltinFunction.length),
            expr);

    /// <summary>
    /// Apply the builtin 'int_add' function.
    /// Equivalent to: Pine_kernel.int_add operands
    /// </summary>
    public static Expression ApplyBuiltinIntAdd(IReadOnlyList<Expression> operands) =>
        Expression.BuiltinInst(
            nameof(BuiltinFunction.int_add),
            Expression.ListInst(operands));

    /// <summary>
    /// Apply the builtin 'int_mul' function.
    /// Equivalent to: Pine_kernel.int_mul operands
    /// </summary>
    public static Expression ApplyBuiltinIntMul(IReadOnlyList<Expression> operands) =>
        Expression.BuiltinInst(
            nameof(BuiltinFunction.int_mul),
            Expression.ListInst(operands));

    /// <summary>
    /// Apply the builtin 'skip' function with a dynamic count expression.
    /// Equivalent to: Pine_kernel.skip [ countExpr, expr ]
    /// </summary>
    public static Expression ApplyBuiltinSkip(Expression countExpr, Expression expr) =>
        Expression.BuiltinInst(
            nameof(BuiltinFunction.skip),
            Expression.ListInst([countExpr, expr]));

    /// <summary>
    /// Apply the builtin 'take' function with a constant count.
    /// Equivalent to: Pine_kernel.take [ count, expr ]
    /// </summary>
    public static Expression ApplyBuiltinTake(int count, Expression expr) =>
        Expression.BuiltinInst(
            nameof(BuiltinFunction.take),
            Expression.ListInst(
                [
                Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(count)),
                expr
                ]));

    /// <summary>
    /// Apply the builtin 'concat' function.
    /// Equivalent to: Pine_kernel.concat items
    /// </summary>
    public static Expression ApplyBuiltinConcat(IReadOnlyList<Expression> items) =>
        Expression.BuiltinInst(
            nameof(BuiltinFunction.concat),
            Expression.ListInst(items));
}
