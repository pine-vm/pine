using Pine.Core.CodeAnalysis;
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
    /// Parameters are placed in the environment at path [1, parameterIndex].
    /// </summary>
    public static Expression BuildPathToParameter(int parameterIndex) =>
        ExpressionBuilder.BuildExpressionForPathInExpression(
            [1, parameterIndex],
            Expression.EnvironmentInstance);

    /// <summary>
    /// Apply the builtin 'skip' function with a constant count.
    /// Equivalent to: Pine_kernel.skip [ count, expr ]
    /// </summary>
    public static Expression ApplyBuiltinSkip(int count, Expression expr) =>
        Expression.KernelApplicationInstance(
            nameof(KernelFunction.skip),
            Expression.ListInstance(
            [
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(count)),
                expr
            ]));

    /// <summary>
    /// Apply the builtin 'head' function.
    /// Equivalent to: Pine_kernel.head expr
    /// </summary>
    public static Expression ApplyBuiltinHead(Expression expr) =>
        Expression.KernelApplicationInstance(
            nameof(KernelFunction.head),
            expr);

    /// <summary>
    /// Apply the builtin 'equal' function for binary comparison.
    /// Equivalent to: Pine_kernel.equal [ left, right ]
    /// </summary>
    public static Expression ApplyBuiltinEqualBinary(Expression left, Expression right) =>
        Expression.KernelApplicationInstance(
            nameof(KernelFunction.equal),
            Expression.ListInstance([left, right]));

    /// <summary>
    /// Apply the builtin 'length' function.
    /// Equivalent to: Pine_kernel.length expr
    /// </summary>
    public static Expression ApplyBuiltinLength(Expression expr) =>
        Expression.KernelApplicationInstance(
            nameof(KernelFunction.length),
            expr);

    /// <summary>
    /// Apply the builtin 'int_add' function.
    /// Equivalent to: Pine_kernel.int_add operands
    /// </summary>
    public static Expression ApplyBuiltinIntAdd(IReadOnlyList<Expression> operands) =>
        Expression.KernelApplicationInstance(
            nameof(KernelFunction.int_add),
            Expression.ListInstance(operands));

    /// <summary>
    /// Apply the builtin 'int_mul' function.
    /// Equivalent to: Pine_kernel.int_mul operands
    /// </summary>
    public static Expression ApplyBuiltinIntMul(IReadOnlyList<Expression> operands) =>
        Expression.KernelApplicationInstance(
            nameof(KernelFunction.int_mul),
            Expression.ListInstance(operands));
}
