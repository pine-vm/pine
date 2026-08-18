using Pine.Core.CodeGen;
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

    private static PineValue FractionDigitsToStringFunctionValue() =>
        ExpressionEncoding.EncodeExpressionAsValue(
            Internal_FractionDigitsToString(Expression.EnvironmentInstance));

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
            Expression.ConditionalInst(
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
                Expression.ConditionalInst(
                    condition: BuiltinHelpers.ApplyBuiltinEqualBinary(digit, LiteralInt(digitValue)),
                    falseBranch: digitText,
                    trueBranch: LiteralText(digitValue.ToString(System.Globalization.CultureInfo.InvariantCulture)));
        }

        var recurse =
            new Expression.Eval(
                encoded: self,
                environment:
                Expression.ListInst(
                    [
                    self,
                    upperDigits,
                    BuiltinHelpers.ApplyBuiltinConcat([digitText, lowerDigits])
                    ]));

        var digitsBody =
            Expression.ConditionalInst(
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
            Expression.ConditionalInst(
                condition: isNonNegative,
                falseBranch: BuiltinHelpers.ApplyBuiltinIntMul([LiteralInt(-1), input]),
                trueBranch: input);

        var digits =
            new Expression.Eval(
                encoded: Expression.LitralInst(encodedDigitsBody),
                environment:
                Expression.ListInst(
                    [
                    Expression.LitralInst(encodedDigitsBody),
                    absoluteValue,
                    LiteralText("")
                    ]));

        var content =
            Expression.ConditionalInst(
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

    private static Expression Internal_FractionDigitsToString(Expression input)
    {
        // The environment is [self, remainingPlaces, divisor, fractionValue, allDigits, trimmedDigits].
        var self = Path(0);
        var remainingPlaces = Path(1);
        var divisor = Path(2);
        var fractionValue = Path(3);
        var allDigits = Path(4);
        var trimmedDigits = Path(5);

        var digit = CoreBasics.Int_div(fractionValue, divisor);
        var remainingFraction = CoreBasics.Int_remainderBy(divisor, fractionValue);
        var digitText = UnwrapString(ApplyUnary(IntToStringFunctionValue(), digit));
        var nextAllDigits = BuiltinHelpers.ApplyBuiltinConcat([allDigits, digitText]);

        var nextTrimmedDigits =
            Expression.ConditionalInst(
                condition: BuiltinHelpers.ApplyBuiltinEqualBinary(digit, LiteralInt(0)),
                falseBranch: nextAllDigits,
                trueBranch: trimmedDigits);

        var recurse =
            new Expression.Eval(
                encoded: self,
                environment:
                Expression.ListInst(
                    [
                    self,
                    BuiltinHelpers.ApplyBuiltinIntAdd([remainingPlaces, LiteralInt(-1)]),
                    CoreBasics.Int_div(divisor, LiteralInt(10)),
                    remainingFraction,
                    nextAllDigits,
                    nextTrimmedDigits
                    ]));

        return
            Expression.ConditionalInst(
                condition: BuiltinHelpers.ApplyBuiltinEqualBinary(remainingPlaces, LiteralInt(0)),
                falseBranch: recurse,
                trueBranch: trimmedDigits);
    }

    private static Expression Internal_FloatToString(Expression input)
    {
        var components =
            BuiltinHelpers.ApplyBuiltinHead(
                BuiltinHelpers.ApplyBuiltinSkip(1, input));

        var numerator = BuiltinHelpers.ApplyBuiltinHead(components);

        var denominator =
            BuiltinHelpers.ApplyBuiltinHead(
                BuiltinHelpers.ApplyBuiltinSkip(1, components));

        var isNegative =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                BuiltinHelpers.ApplyBuiltinHead(numerator),
                LiteralByte(2));

        var absoluteNumerator =
            Expression.ConditionalInst(
                condition: isNegative,
                falseBranch: numerator,
                trueBranch: BuiltinHelpers.ApplyBuiltinIntMul([LiteralInt(-1), numerator]));

        var integerPart = CoreBasics.Int_div(absoluteNumerator, denominator);
        var remainder = CoreBasics.Int_remainderBy(denominator, absoluteNumerator);
        var scale = LiteralInt(10_000_000_000_000_000);
        var scaledValue = BuiltinHelpers.ApplyBuiltinIntMul([remainder, scale]);
        var scaledInteger = CoreBasics.Int_div(scaledValue, denominator);
        var leftover = CoreBasics.Int_remainderBy(denominator, scaledValue);

        var roundUp =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_is_sorted_asc),
                Expression.ListInst(
                    [
                    denominator,
                    BuiltinHelpers.ApplyBuiltinIntMul([leftover, LiteralInt(2)])
                    ]));

        var roundedFraction =
            Expression.ConditionalInst(
                condition: roundUp,
                falseBranch: scaledInteger,
                trueBranch: BuiltinHelpers.ApplyBuiltinIntAdd([scaledInteger, LiteralInt(1)]));

        var overflowed =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_is_sorted_asc),
                Expression.ListInst([scale, roundedFraction]));

        var renderedInteger =
            Expression.ConditionalInst(
                condition: overflowed,
                falseBranch: integerPart,
                trueBranch: BuiltinHelpers.ApplyBuiltinIntAdd([integerPart, LiteralInt(1)]));

        var fractionValue =
            Expression.ConditionalInst(
                condition: overflowed,
                falseBranch: roundedFraction,
                trueBranch: LiteralInt(0));

        var fractionDigits =
            new Expression.Eval(
                encoded: Expression.LitralInst(FractionDigitsToStringFunctionValue()),
                environment:
                Expression.ListInst(
                    [
                    Expression.LitralInst(FractionDigitsToStringFunctionValue()),
                    LiteralInt(16),
                    LiteralInt(1_000_000_000_000_000),
                    fractionValue,
                    LiteralText(""),
                    LiteralText("")
                    ]));

        var sign =
            Expression.ConditionalInst(
                condition: isNegative,
                falseBranch: LiteralText(""),
                trueBranch: LiteralText("-"));

        var integerText =
            UnwrapString(
                ApplyUnary(IntToStringFunctionValue(), renderedInteger));

        var unsignedContent =
            Expression.ConditionalInst(
                condition: BuiltinHelpers.ApplyBuiltinEqualBinary(fractionValue, LiteralInt(0)),
                falseBranch:
                BuiltinHelpers.ApplyBuiltinConcat(
                    [integerText, LiteralText("."), fractionDigits]),
                trueBranch: integerText);

        var finiteContent =
            BuiltinHelpers.ApplyBuiltinConcat([sign, unsignedContent]);

        var infiniteContent =
            Expression.ConditionalInst(
                condition: isNegative,
                falseBranch: LiteralText("Infinity"),
                trueBranch: LiteralText("-Infinity"));

        var nonFiniteContent =
            Expression.ConditionalInst(
                condition: BuiltinHelpers.ApplyBuiltinEqualBinary(numerator, LiteralInt(0)),
                falseBranch: infiniteContent,
                trueBranch: LiteralText("NaN"));

        var content =
            Expression.ConditionalInst(
                condition: BuiltinHelpers.ApplyBuiltinEqualBinary(denominator, LiteralInt(0)),
                falseBranch: finiteContent,
                trueBranch: nonFiniteContent);

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
            new Expression.Eval(
                encoded: renderer,
                environment:
                Expression.ListInst(
                    [
                    renderer,
                    BuiltinHelpers.ApplyBuiltinHead(remaining)
                    ]));

        var renderedHeadContent =
            BuiltinHelpers.ApplyBuiltinHead(
                BuiltinHelpers.ApplyBuiltinHead(
                    BuiltinHelpers.ApplyBuiltinSkip(1, renderedHead)));

        var renderedTail =
            new Expression.Eval(
                encoded: self,
                environment: Expression.ListInst([self, renderer, tail]));

        var tailIsEmpty =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                BuiltinHelpers.ApplyBuiltinLength(tail),
                zero);

        var nonEmptyContent =
            Expression.ConditionalInst(
                condition: tailIsEmpty,
                falseBranch:
                BuiltinHelpers.ApplyBuiltinConcat(
                    [renderedHeadContent, LiteralText(","), renderedTail]),
                trueBranch: renderedHeadContent);

        var listBody =
            Expression.ConditionalInst(
                condition: isEmpty,
                falseBranch: nonEmptyContent,
                trueBranch: LiteralText(""));

        var encodedListHelperBody =
            ExpressionEncoding.EncodeExpressionAsValue(listBody);

        var fieldName =
            BuiltinHelpers.ApplyBuiltinHead(remaining);

        var fieldValue =
            BuiltinHelpers.ApplyBuiltinHead(
                BuiltinHelpers.ApplyBuiltinSkip(1, remaining));

        var renderedFieldValue =
            new Expression.Eval(
                encoded: renderer,
                environment:
                Expression.ListInst(
                    [
                    renderer,
                    fieldValue
                    ]));

        var renderedFieldValueContent =
            UnwrapString(renderedFieldValue);

        var remainingRecordFields =
            BuiltinHelpers.ApplyBuiltinSkip(2, remaining);

        var renderedRemainingRecordFields =
            new Expression.Eval(
                encoded: self,
                environment:
                Expression.ListInst(
                    [
                    self,
                    renderer,
                    remainingRecordFields
                    ]));

        var remainingRecordFieldsIsEmpty =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                BuiltinHelpers.ApplyBuiltinLength(remainingRecordFields),
                zero);

        var nonEmptyRecordContent =
            Expression.ConditionalInst(
                condition: remainingRecordFieldsIsEmpty,
                falseBranch:
                BuiltinHelpers.ApplyBuiltinConcat(
                    [
                    fieldName,
                    LiteralText(" = "),
                    renderedFieldValueContent,
                    LiteralText(", "),
                    renderedRemainingRecordFields
                    ]),
                trueBranch:
                BuiltinHelpers.ApplyBuiltinConcat(
                    [
                    fieldName,
                    LiteralText(" = "),
                    renderedFieldValueContent
                    ]));

        var recordBody =
            Expression.ConditionalInst(
                condition: isEmpty,
                falseBranch: nonEmptyRecordContent,
                trueBranch: LiteralText(""));

        var encodedRecordHelperBody =
            ExpressionEncoding.EncodeExpressionAsValue(recordBody);

        // The renderer environment is [self, value].
        var rendererSelf = Path(0);
        var rendererValue = Path(1);

        var listContent =
            new Expression.Eval(
                encoded: Expression.LitralInst(encodedListHelperBody),
                environment:
                Expression.ListInst(
                    [
                    Expression.LitralInst(encodedListHelperBody),
                    rendererSelf,
                    rendererValue
                    ]));

        var recordContent =
            new Expression.Eval(
                encoded: Expression.LitralInst(encodedRecordHelperBody),
                environment:
                Expression.ListInst(
                    [
                    Expression.LitralInst(encodedRecordHelperBody),
                    rendererSelf,
                    BuiltinHelpers.ApplyBuiltinSkip(1, rendererValue)
                    ]));

        var listResult =
            WrapString(
                BuiltinHelpers.ApplyBuiltinConcat(
                    [LiteralText("["), listContent, LiteralText("]")]));

        var recordResult =
            WrapString(
                BuiltinHelpers.ApplyBuiltinConcat(
                    [LiteralText("{ "), recordContent, LiteralText(" }")]));

        var stringResult =
            Internal_StringToString(rendererValue);

        var floatResult =
            Internal_FloatToString(rendererValue);

        var hasListItems =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_is_sorted_asc),
                Expression.ListInst(
                    [
                    LiteralInt(1),
                    BuiltinHelpers.ApplyBuiltinLength(rendererValue)
                    ]));

        var isString =
            Expression.ConditionalInst(
                condition: hasListItems,
                falseBranch: Expression.LitralInst(PineVM.PineKernelValues.FalseValue),
                trueBranch:
                BuiltinHelpers.ApplyBuiltinEqualBinary(
                    BuiltinHelpers.ApplyBuiltinHead(rendererValue),
                    Expression.LitralInst(ElmValue.ElmStringTypeTagNameAsValue)));

        var isFloat =
            Expression.ConditionalInst(
                condition: hasListItems,
                falseBranch: Expression.LitralInst(PineVM.PineKernelValues.FalseValue),
                trueBranch:
                BuiltinHelpers.ApplyBuiltinEqualBinary(
                    BuiltinHelpers.ApplyBuiltinHead(rendererValue),
                    Expression.LitralInst(ElmValue.ElmFloatTypeTagNameAsValue)));

        var isRecord =
            Expression.ConditionalInst(
                condition: hasListItems,
                falseBranch: Expression.LitralInst(PineVM.PineKernelValues.FalseValue),
                trueBranch:
                BuiltinHelpers.ApplyBuiltinEqualBinary(
                    BuiltinHelpers.ApplyBuiltinHead(rendererValue),
                    Expression.LitralInst(ElmValue.ElmRecordTypeTagNameAsValue)));

        var nonBlobResult =
            Expression.ConditionalInst(
                condition: isFloat,
                falseBranch:
                Expression.ConditionalInst(
                    condition: isString,
                    falseBranch:
                    Expression.ConditionalInst(
                        condition: isRecord,
                        falseBranch: listResult,
                        trueBranch: recordResult),
                    trueBranch: stringResult),
                trueBranch: floatResult);

        var emptyBlob =
            Expression.LitralInst(PineValue.EmptyBlob);

        var isBlob =
            BuiltinHelpers.ApplyBuiltinEqualBinary(
                BuiltinHelpers.ApplyBuiltinTake(0, rendererValue),
                emptyBlob);

        var blobLength =
            BuiltinHelpers.ApplyBuiltinLength(rendererValue);

        var blobHasIntegerLength =
            Expression.BuiltinInst(
                nameof(BuiltinFunction.int_is_sorted_asc),
                Expression.ListInst([LiteralInt(2), blobLength]));

        var blobSign =
            BuiltinHelpers.ApplyBuiltinHead(rendererValue);

        var blobHasIntegerSign =
            Expression.ConditionalInst(
                condition: BuiltinHelpers.ApplyBuiltinEqualBinary(blobSign, LiteralByte(2)),
                falseBranch: BuiltinHelpers.ApplyBuiltinEqualBinary(blobSign, LiteralByte(4)),
                trueBranch: Expression.LitralInst(PineVM.PineKernelValues.TrueValue));

        var blobDecodesAsInteger =
            Expression.ConditionalInst(
                condition: blobHasIntegerLength,
                falseBranch: Expression.LitralInst(PineVM.PineKernelValues.FalseValue),
                trueBranch: blobHasIntegerSign);

        var blobResult =
            WrapString(LiteralText("<blob>"));

        var rendererBody =
            Expression.ConditionalInst(
                condition: isBlob,
                falseBranch: nonBlobResult,
                trueBranch:
                Expression.ConditionalInst(
                    condition: blobDecodesAsInteger,
                    falseBranch: blobResult,
                    trueBranch: ApplyUnary(IntToStringFunctionValue(), rendererValue)));

        var encodedRendererBody =
            ExpressionEncoding.EncodeExpressionAsValue(rendererBody);

        return
            new Expression.Eval(
                encoded: Expression.LitralInst(encodedRendererBody),
                environment:
                Expression.ListInst(
                    [
                    Expression.LitralInst(encodedRendererBody),
                    value
                    ]));
    }

    private static Expression WrapString(Expression content) =>
        Expression.ListInst(
            [
            Expression.LitralInst(ElmValue.ElmStringTypeTagNameAsValue),
            Expression.ListInst([content])
            ]);

    private static Expression UnwrapString(Expression value) =>
        BuiltinHelpers.ApplyBuiltinHead(
            BuiltinHelpers.ApplyBuiltinHead(
                BuiltinHelpers.ApplyBuiltinSkip(1, value)));

    private static Expression Path(int index) =>
        ExpressionBuilder.BuildExpressionForPathInExpression(
            [index],
            Expression.EnvironmentInstance);

    private static Expression.Litral LiteralInt(long value) =>
        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(value));

    private static Expression.Litral LiteralText(string value) =>
        Expression.LitralInst(StringEncoding.ValueFromString(value));

    private static Expression.Litral LiteralByte(byte value) =>
        Expression.LitralInst(PineValue.BlobSingleByte(value));

    private static Expression ApplyUnary(PineValue functionValue, Expression argument) =>
        new Expression.Eval(
            encoded: Expression.LitralInst(functionValue),
            environment: argument);
}
