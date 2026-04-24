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
    /// <para>
    /// In the historical (<c>WithEnvFunctions</c>) layout the runtime
    /// environment is <c>[envFunctions, arg0, arg1, ...]</c> so parameters
    /// live at path <c>[1 + parameterIndex]</c>.
    /// </para>
    /// <para>
    /// In the §7.7 <c>WithoutEnvFunctions</c> layout (used for non-recursive
    /// single-member SCCs) the runtime environment is
    /// <c>[arg0, arg1, ...]</c>, so parameters live at path
    /// <c>[parameterIndex]</c>. Use the
    /// <see cref="BuildPathToParameter(int, int)"/> overload and pass
    /// <c>envParametersOffset = 0</c> in that case.
    /// </para>
    /// </summary>
    public static Expression BuildPathToParameter(int parameterIndex) =>
        BuildPathToParameter(parameterIndex, envParametersOffset: 1);

    /// <summary>
    /// Builds an expression that accesses a parameter at the given index, with
    /// an explicit env-parameters offset. See
    /// <see cref="BuildPathToParameter(int)"/> for the per-layout meaning of
    /// <paramref name="envParametersOffset"/>.
    /// </summary>
    public static Expression BuildPathToParameter(int parameterIndex, int envParametersOffset) =>
        ExpressionBuilder.BuildExpressionForPathInExpression(
            [envParametersOffset + parameterIndex],
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

    /// <summary>
    /// Apply the builtin 'skip' function with a dynamic count expression.
    /// Equivalent to: Pine_kernel.skip [ countExpr, expr ]
    /// </summary>
    public static Expression ApplyBuiltinSkip(Expression countExpr, Expression expr) =>
        Expression.KernelApplicationInstance(
            nameof(KernelFunction.skip),
            Expression.ListInstance([countExpr, expr]));

    /// <summary>
    /// Apply the builtin 'take' function with a constant count.
    /// Equivalent to: Pine_kernel.take [ count, expr ]
    /// </summary>
    public static Expression ApplyBuiltinTake(int count, Expression expr) =>
        Expression.KernelApplicationInstance(
            nameof(KernelFunction.take),
            Expression.ListInstance(
                [
                Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(count)),
                expr
                ]));

    /// <summary>
    /// Apply the builtin 'concat' function.
    /// Equivalent to: Pine_kernel.concat items
    /// </summary>
    public static Expression ApplyBuiltinConcat(IReadOnlyList<Expression> items) =>
        Expression.KernelApplicationInstance(
            nameof(KernelFunction.concat),
            Expression.ListInstance(items));
}
