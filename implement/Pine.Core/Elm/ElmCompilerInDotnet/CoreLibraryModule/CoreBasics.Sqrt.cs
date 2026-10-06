using Pine.Core.CodeGen;
using Pine.Core.CommonEncodings;
using System;
using System.Numerics;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public partial class CoreBasics
{
    private static readonly Lazy<PineValue> s_roundFunction = new(BuildRoundFunctionValue);

    private static readonly Lazy<PineValue> s_sqrtFunction = new(BuildSqrtFunctionValue);

    private static readonly Lazy<PineValue> s_isNaNFunction = new(() => NonFinitePredicate(true));

    private static readonly Lazy<PineValue> s_isInfiniteFunction = new(() => NonFinitePredicate(false));

    /// <summary>Tests the rational NaN representation, without classifying integers as floating-point tags.</summary>
    public static PineValue IsNaN_FunctionValue() => s_isNaNFunction.Value;

    /// <summary>Tests positive and negative infinity, excluding NaN.</summary>
    public static PineValue IsInfinite_FunctionValue() => s_isInfiniteFunction.Value;

    private static PineValue NonFinitePredicate(bool nan)
    {
        var arg = Expression.EnvironmentInstance;
        var numeratorIsZero = BuiltinHelpers.ApplyBuiltinEqualBinary(ChoiceArgument(arg, 0), LiteralInt(0));

        var result =
            Expression.ConditionalInst(
                condition: numeratorIsZero,
                trueBranch: nan ? s_trueValue : s_falseValue,
                falseBranch: nan ? s_falseValue : s_trueValue);

        return
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.ConditionalInst(
                    condition: BuiltinHelpers.ApplyBuiltinEqualBinary(ChoiceTagName(arg), s_elmFloatTypeTagNameLiteral),
                    falseBranch: s_falseValue,
                    trueBranch: Expression.ConditionalInst(
                        condition: BuiltinHelpers.ApplyBuiltinEqualBinary(ChoiceArgument(arg, 1), LiteralInt(0)),
                        trueBranch: result,
                        falseBranch: s_falseValue)));
    }

    /// <summary>Rounds to the nearest integer, resolving ties toward positive infinity as in Elm.</summary>
    public static PineValue Round_FunctionValue() => s_roundFunction.Value;

    private static PineValue BuildRoundFunctionValue()
    {
        var arg = Expression.EnvironmentInstance;
        var isFloat = BuiltinHelpers.ApplyBuiltinEqualBinary(ChoiceTagName(arg), s_elmFloatTypeTagNameLiteral);
        var nonfinite = BuiltinHelpers.ApplyBuiltinEqualBinary(ChoiceArgument(arg, 1), LiteralInt(0));
        var rounded = Generic_Floor(Internal_Generic_Add(arg, NormalizeFloatResult(LiteralInt(1), LiteralInt(2))));

        return
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.ConditionalInst(
                    condition: isFloat,
                    trueBranch: Expression.ConditionalInst(condition: nonfinite, trueBranch: arg, falseBranch: rounded),
                    falseBranch: rounded));
    }

    /// <summary>Square root normalized by powers of four, with 52 fractional mantissa bits and exact integer Newton iteration.</summary>
    public static PineValue Sqrt_FunctionValue() => s_sqrtFunction.Value;

    private static PineValue BuildSqrtFunctionValue()
    {
        var arg = Expression.EnvironmentInstance;
        var isFloat = BuiltinHelpers.ApplyBuiltinEqualBinary(ChoiceTagName(arg), s_elmFloatTypeTagNameLiteral);

        var inputNumerator =
            Expression.ConditionalInst(condition: isFloat, trueBranch: ChoiceArgument(arg, 0), falseBranch: arg);

        var normalization = NormalizeSquareRootArgument();

        var normalized =
            new Expression.Eval(
                encoded: Expression.LitralInst(normalization),
                environment: Expression.ListInst([Expression.LitralInst(normalization), arg, LiteralInt(0)]));

        var mantissa = BuiltinHelpers.ApplyBuiltinHead(normalized);
        var exponent = BuiltinHelpers.ApplyBuiltinHead(BuiltinHelpers.ApplyBuiltinSkip(1, normalized));

        var mantissaIsFloat =
            BuiltinHelpers.ApplyBuiltinEqualBinary(ChoiceTagName(mantissa), s_elmFloatTypeTagNameLiteral);

        var numerator =
            Expression.ConditionalInst(
                condition: mantissaIsFloat,
                trueBranch: ChoiceArgument(mantissa, 0),
                falseBranch: mantissa);

        var denominator =
            Expression.ConditionalInst(
                condition: isFloat,
                trueBranch: ChoiceArgument(arg, 1),
                falseBranch: LiteralInt(1));

        var mantissaDenominator =
            Expression.ConditionalInst(
                condition: mantissaIsFloat,
                trueBranch: ChoiceArgument(mantissa, 1),
                falseBranch: LiteralInt(1));

        var scale = Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(BigInteger.One << 104));
        var scaled = Internal_Int_div(BuiltinMul(numerator, scale), mantissaDenominator);
        var helper = IntegerSquareRootHelper();

        var root =
            new Expression.Eval(
                encoded: Expression.LitralInst(helper),
                environment: Expression.ListInst([Expression.LitralInst(helper), scaled, scaled]));

        var finite =
            Expression.ConditionalInst(
                condition: BuiltinHelpers.ApplyBuiltinEqualBinary(inputNumerator, LiteralInt(0)),
                trueBranch: LiteralInt(0),
                falseBranch: Internal_Generic_Mul(
                    NormalizeFloatResult(root, LiteralInt(1L << 52)),
                    Internal_Generic_Pow(LiteralInt(2), exponent)));

        var nan = BuildChoice(s_elmFloatTypeTagNameLiteral, [LiteralInt(0), LiteralInt(0)]);

        return
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.ConditionalInst(
                    condition: BuiltinIntIsSortedAsc(inputNumerator, LiteralInt(-1)),
                    trueBranch: nan,
                    falseBranch: Expression.ConditionalInst(
                        condition: BuiltinHelpers.ApplyBuiltinEqualBinary(denominator, LiteralInt(0)),
                        trueBranch: arg,
                        falseBranch: finite)));
    }

    private static PineValue NormalizeSquareRootArgument()
    {
        var self = ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance);
        var value = ExpressionBuilder.BuildExpressionForPathInExpression([1], Expression.EnvironmentInstance);
        var exponent = ExpressionBuilder.BuildExpressionForPathInExpression([2], Expression.EnvironmentInstance);

        var numerator =
            Expression.ConditionalInst(
                condition: BuiltinHelpers.ApplyBuiltinEqualBinary(ChoiceTagName(value), s_elmFloatTypeTagNameLiteral),
                trueBranch: ChoiceArgument(value, 0),
                falseBranch: value);

        var denominator =
            Expression.ConditionalInst(
                condition: BuiltinHelpers.ApplyBuiltinEqualBinary(ChoiceTagName(value), s_elmFloatTypeTagNameLiteral),
                trueBranch: ChoiceArgument(value, 1),
                falseBranch: LiteralInt(1));

        Expression Recurse(Expression next, Expression power) =>
            new Expression.Eval(encoded: self, environment: Expression.ListInst([self, next, power]));

        return
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.ConditionalInst(
                    condition: BuiltinIntIsSortedAsc(numerator, LiteralInt(0)),
                    trueBranch: Expression.ListInst([value, exponent]),
                    falseBranch: Expression.ConditionalInst(
                        condition: BuiltinHelpers.ApplyBuiltinEqualBinary(denominator, LiteralInt(0)),
                        trueBranch: Expression.ListInst([value, exponent]),
                        falseBranch: Expression.ConditionalInst(
                            condition: Generic_Lt(value, LiteralInt(1)),
                            trueBranch: Recurse(
                                Internal_Generic_Mul(value, LiteralInt(4)),
                                BuiltinAdd(exponent, LiteralInt(-1))),
                            falseBranch: Expression.ConditionalInst(
                                condition: Generic_Le(LiteralInt(4), value),
                                trueBranch:
                                Recurse(Internal_Float_div(value, LiteralInt(4)), BuiltinAdd(exponent, LiteralInt(1))),
                                falseBranch: Expression.ListInst([value, exponent]))))));
    }

    private static PineValue IntegerSquareRootHelper()
    {
        var self = ExpressionBuilder.BuildExpressionForPathInExpression([0], Expression.EnvironmentInstance);
        var n = ExpressionBuilder.BuildExpressionForPathInExpression([1], Expression.EnvironmentInstance);
        var current = ExpressionBuilder.BuildExpressionForPathInExpression([2], Expression.EnvironmentInstance);
        var next = Internal_Int_div(BuiltinAdd(current, Internal_Int_div(n, current)), LiteralInt(2));

        return
            ExpressionEncoding.EncodeExpressionAsValue(
                Expression.ConditionalInst(
                    condition: BuiltinIntIsSortedAsc(n, LiteralInt(1)),
                    trueBranch: n,
                    falseBranch: Expression.ConditionalInst(
                        condition: BuiltinIntIsSortedAsc(current, next),
                        trueBranch: current,
                        falseBranch:
                        new Expression.Eval(encoded: self, environment: Expression.ListInst([self, n, next])))));
    }
}
