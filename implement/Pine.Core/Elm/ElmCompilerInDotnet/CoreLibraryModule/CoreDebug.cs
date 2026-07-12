using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CoreLibraryModule;

/// <summary>
/// Minimal native implementation of the Elm <c>Debug</c> module.
/// </summary>
public static class CoreDebug
{
    public static Expression ToString(Expression value) =>
        ApplyUnary(ToString_FunctionValue(), value);

    public static PineValue ToString_FunctionValue() =>
        ExpressionEncoding.EncodeExpressionAsValue(
            Internal_ToString(Expression.EnvironmentInstance));

    private static PineValue IntToStringFunctionValue() =>
        ExpressionEncoding.EncodeExpressionAsValue(
            Internal_IntToString(Expression.EnvironmentInstance));

    private static Expression Internal_IntToString(Expression input)
    {
        var self = Path(0);
        var integer = Path(1);
        var lowerDigits = Path(2);

        var zero = LiteralInt(0);
        var ten = LiteralInt(10);

        var isComplete =
            BuiltinHelpers.ApplyBuiltinEqualBinary(integer, zero);

        var noDigits =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                BuiltinHelpers.ApplyBuiltinLength(lowerDigits),
                zero);

        var completedDigits =
            Expression.ConditionalInstance(
                condition: noDigits,
                falseBranch: lowerDigits,
                trueBranch: LiteralText("0"));

        var upperDigits = CoreBasics.Int_div(integer, ten);

        var digit =
            BuiltinHelpers.ApplyBuiltinIntAdd(
                [
                integer,
                BuiltinHelpers.ApplyBuiltinIntMul([upperDigits, LiteralInt(-10)])
                ]);

        Expression digitText = LiteralText("9");

        for (var digitValue = 8; digitValue >= 0; digitValue--)
        {
            digitText =
                Expression.ConditionalInstance(
                    condition: BuiltinHelpers.ApplyBuiltinEqualBinary(digit, LiteralInt(digitValue)),
                    falseBranch: digitText,
                    trueBranch: LiteralText(digitValue.ToString(System.Globalization.CultureInfo.InvariantCulture)));
        }

        var recurse =
            new Expression.ParseAndEval(
                encoded: self,
                environment:
                Expression.ListInstance(
                    [
                    self,
                    upperDigits,
                    BuiltinHelpers.ApplyBuiltinConcat([digitText, lowerDigits])
                    ]));

        var digitsBody =
            Expression.ConditionalInstance(
                condition: isComplete,
                falseBranch: recurse,
                trueBranch: completedDigits);

        var encodedDigitsBody =
            ExpressionEncoding.EncodeExpressionAsValue(digitsBody);

        var isNonNegative =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                BuiltinHelpers.ApplyBuiltinHead(input),
                LiteralByte(4));

        var absoluteValue =
            Expression.ConditionalInstance(
                condition: isNonNegative,
                falseBranch: BuiltinHelpers.ApplyBuiltinIntMul([LiteralInt(-1), input]),
                trueBranch: input);

        var digits =
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(encodedDigitsBody),
                environment:
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(encodedDigitsBody),
                    absoluteValue,
                    LiteralText("")
                    ]));

        var content =
            Expression.ConditionalInstance(
                condition: isNonNegative,
                falseBranch: BuiltinHelpers.ApplyBuiltinConcat([LiteralText("-"), digits]),
                trueBranch: digits);

        return WrapString(content);
    }

    private static Expression Internal_StringToString(Expression input)
    {
        var stringContent =
            BuiltinHelpers.ApplyBuiltinHead(
                BuiltinHelpers.ApplyBuiltinHead(
                    BuiltinHelpers.ApplyBuiltinSkip(1, input)));

        var content =
            BuiltinHelpers.ApplyBuiltinConcat(
                [LiteralText("\""), stringContent, LiteralText("\"")]);

        return WrapString(content);
    }

    private static Expression Internal_ToString(Expression value)
    {
        // The list helper environment is [self, renderer, remaining].
        var self = Path(0);
        var renderer = Path(1);
        var remaining = Path(2);
        var zero = LiteralInt(0);

        var isEmpty =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                BuiltinHelpers.ApplyBuiltinLength(remaining),
                zero);

        var tail = BuiltinHelpers.ApplyBuiltinSkip(1, remaining);

        var renderedHead =
            new Expression.ParseAndEval(
                encoded: renderer,
                environment:
                Expression.ListInstance(
                    [
                    renderer,
                    BuiltinHelpers.ApplyBuiltinHead(remaining)
                    ]));

        var renderedHeadContent =
            BuiltinHelpers.ApplyBuiltinHead(
                BuiltinHelpers.ApplyBuiltinHead(
                    BuiltinHelpers.ApplyBuiltinSkip(1, renderedHead)));

        var renderedTail =
            new Expression.ParseAndEval(
                encoded: self,
                environment: Expression.ListInstance([self, renderer, tail]));

        var tailIsEmpty =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                BuiltinHelpers.ApplyBuiltinLength(tail),
                zero);

        var nonEmptyContent =
            Expression.ConditionalInstance(
                condition: tailIsEmpty,
                falseBranch:
                BuiltinHelpers.ApplyBuiltinConcat(
                    [renderedHeadContent, LiteralText(","), renderedTail]),
                trueBranch: renderedHeadContent);

        var listBody =
            Expression.ConditionalInstance(
                condition: isEmpty,
                falseBranch: nonEmptyContent,
                trueBranch: LiteralText(""));

        var encodedListHelperBody =
            ExpressionEncoding.EncodeExpressionAsValue(listBody);

        // The renderer environment is [self, value].
        var rendererSelf = Path(0);
        var rendererValue = Path(1);

        var listContent =
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(encodedListHelperBody),
                environment:
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(encodedListHelperBody),
                    rendererSelf,
                    rendererValue
                    ]));

        var listResult =
            WrapString(
                BuiltinHelpers.ApplyBuiltinConcat(
                    [LiteralText("["), listContent, LiteralText("]")]));

        var stringResult =
            Internal_StringToString(rendererValue);

        var hasListItems =
            Expression.KernelApplicationInstance(
                nameof(BuiltinFunction.int_is_sorted_asc),
                Expression.ListInstance(
                    [
                    LiteralInt(1),
                    BuiltinHelpers.ApplyBuiltinLength(rendererValue)
                    ]));

        var isString =
            Expression.ConditionalInstance(
                condition: hasListItems,
                falseBranch: Expression.LiteralInstance(PineVM.PineKernelValues.FalseValue),
                trueBranch:
                BuiltinHelpers.ApplyBuiltinEqualBinary(
                    BuiltinHelpers.ApplyBuiltinHead(rendererValue),
                    Expression.LiteralInstance(ElmValue.ElmStringTypeTagNameAsValue)));

        var nonBlobResult =
            Expression.ConditionalInstance(
                condition: isString,
                falseBranch: listResult,
                trueBranch: stringResult);

        var emptyBlob =
            Expression.LiteralInstance(PineValue.EmptyBlob);

        var isBlob =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                BuiltinHelpers.ApplyBuiltinTake(0, rendererValue),
                emptyBlob);

        var blobLength =
            BuiltinHelpers.ApplyBuiltinLength(rendererValue);

        var blobHasIntegerLength =
            Expression.KernelApplicationInstance(
                nameof(BuiltinFunction.int_is_sorted_asc),
                Expression.ListInstance([LiteralInt(2), blobLength]));

        var blobSign =
            BuiltinHelpers.ApplyBuiltinHead(rendererValue);

        var blobHasIntegerSign =
            Expression.ConditionalInstance(
                condition: BuiltinHelpers.ApplyBuiltinEqualBinary(blobSign, LiteralByte(2)),
                falseBranch: BuiltinHelpers.ApplyBuiltinEqualBinary(blobSign, LiteralByte(4)),
                trueBranch: Expression.LiteralInstance(PineVM.PineKernelValues.TrueValue));

        var blobDecodesAsInteger =
            Expression.ConditionalInstance(
                condition: blobHasIntegerLength,
                falseBranch: Expression.LiteralInstance(PineVM.PineKernelValues.FalseValue),
                trueBranch: blobHasIntegerSign);

        var blobResult =
            WrapString(LiteralText("<blob>"));

        var rendererBody =
            Expression.ConditionalInstance(
                condition: isBlob,
                falseBranch: nonBlobResult,
                trueBranch:
                Expression.ConditionalInstance(
                    condition: blobDecodesAsInteger,
                    falseBranch: blobResult,
                    trueBranch: ApplyUnary(IntToStringFunctionValue(), rendererValue)));

        var encodedRendererBody =
            ExpressionEncoding.EncodeExpressionAsValue(rendererBody);

        return
            new Expression.ParseAndEval(
                encoded: Expression.LiteralInstance(encodedRendererBody),
                environment:
                Expression.ListInstance(
                    [
                    Expression.LiteralInstance(encodedRendererBody),
                    value
                    ]));
    }

    private static Expression WrapString(Expression content) =>
        Expression.ListInstance(
            [
            Expression.LiteralInstance(ElmValue.ElmStringTypeTagNameAsValue),
            Expression.ListInstance([content])
            ]);

    private static Expression Path(int index) =>
        ExpressionBuilder.BuildExpressionForPathInExpression(
            [index],
            Expression.EnvironmentInstance);

    private static Expression.Literal LiteralInt(long value) =>
        Expression.LiteralInstance(IntegerEncoding.EncodeSignedInteger(value));

    private static Expression.Literal LiteralText(string value) =>
        Expression.LiteralInstance(StringEncoding.ValueFromString(value));

    private static Expression.Literal LiteralByte(byte value) =>
        Expression.LiteralInstance(PineValue.BlobSingleByte(value));

    private static Expression ApplyUnary(PineValue functionValue, Expression argument) =>
        new Expression.ParseAndEval(
            encoded: Expression.LiteralInstance(functionValue),
            environment: argument);
}
