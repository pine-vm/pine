using System.Collections.Generic;
using System;
using System.Linq;

namespace Pine.Core.PopularEncodings;

/// <summary>
/// The standard encoding of Pine expression as Pine value.
/// This is the encoding used to map from data to code when evaluating a <see cref="Expression.ParseAndEval"/> expression.
/// </summary>
public static class ExpressionEncoding
{
    /// <summary>
    /// The standard encoding of Pine expression as Pine value.
    /// This is the encoding used to map from data to code when evaluating a <see cref="Expression.ParseAndEval"/> expression.
    /// </summary>
    public static PineValue.ListValue EncodeExpressionAsValue(Expression expression) =>
        ReusedInstances.Instance.ExpressionEncodings?.TryGetValue(expression, out var encoded) ?? false && encoded is not null
        ?
        encoded
        :
        expression switch
        {
            Expression.Literal literal =>
            EncodeChoiceTypeVariantAsPineValue(
                "Literal",
                PineValue.List([literal.Value])),

            Expression.Environment =>
            EnvironmentExpressionValue,

            Expression.List list =>
            EncodeListExpressionAsValue(list),

            Expression.Conditional conditional =>
            EncodeConditional(conditional),

            Expression.ParseAndEval parseAndEval =>
            EncodeParseAndEval(parseAndEval),

            Expression.KernelApplication kernelAppl =>
            EncodeKernelApplication(kernelAppl),

            Expression.StringTag stringTag =>
            EncodeChoiceTypeVariantAsPineValue(
                "StringTag",
                PineValue.List(
                    [StringEncoding.BlobValueFromString(stringTag.Tag),
                    EncodeExpressionAsValue(stringTag.Tagged)
                    ])),

            _ =>
            throw new Exception(
                "Unsupported expression type: " + expression.GetType().FullName)
        };

    private static readonly PineValue.ListValue EnvironmentExpressionValue =
        EncodeChoiceTypeVariantAsPineValue("Environment", PineValue.EmptyList);

    private static PineValue.ListValue EncodeListExpressionAsValue(Expression.List list)
    {
        var encodedItems = new PineValue[list.items.Count];

        for (var i = 0; i < list.items.Count; ++i)
        {
            encodedItems[i] = EncodeExpressionAsValue(list.items[i]);
        }

        return
            EncodeChoiceTypeVariantAsPineValue(
                "List",
                PineValue.List([PineValue.List(encodedItems)]));
    }

    /// <summary>
    /// Parse a Pine value as Pine expression.
    /// 
    /// Inverse of <see cref="EncodeExpressionAsValue"/>.
    /// </summary>
    public static Result<string, Expression> ParseExpressionFromValue(
        PineValue value)
    {
        if (value is PineValue.ListValue listValue)
        {
            if (ReusedInstances.Instance.ExpressionDecodings?.TryGetValue(listValue, out var reusedInstance) ?? false)
                return reusedInstance;
        }

        return
            ParseExpressionFromValueDefault(
                value,
                generalParser: ParseExpressionFromValue);
    }

    private static Result<string, Expression> ParseExpressionFromValueDefault(
        PineValue value,
        Func<PineValue, Result<string, Expression>> generalParser)
    {
        if (value is not PineValue.ListValue rootList)
            return "Unexpected value type, not a list: " + value.GetType().FullName;

        if (rootList.Elements.Length is not 2)
            return "Unexpected number of items in list: Not 2 but " + rootList.Elements.Length;

        var parseTagResult = StringEncoding.StringFromValue(rootList.Elements.Span[0]);

        {
            if (parseTagResult.IsErrOrNull() is { } err)
                return "Failed to parse tag as string: " + err;
        }

        if (parseTagResult.IsOkOrNull() is not { } tag)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseTagResult.GetType().FullName);
        }

        if (rootList.Elements.Span[1] is not PineValue.ListValue tagArguments)
        {
            return "Unexpected shape of tag argument value: Not a list";
        }

        return
            tag switch
            {
                "Literal" =>
                tagArguments.Elements.Length < 1
                ?
                "Expected one argument for literal but got zero"
                :
                Expression.LiteralInstance(tagArguments.Elements.Span[0]),

                "List" =>
                tagArguments.Elements.Length is not 1
                ?
                "Unexpected number of arguments for list: Not 1 but " + tagArguments.Elements.Length
                :
                ParseExpressionList(tagArguments.Elements.Span[0]),

                "ParseAndEval" =>
                ParseParseAndEval(generalParser, tagArguments.Elements),

                "KernelApplication" =>
                ParseKernelApplication(generalParser, tagArguments.Elements),

                "Conditional" =>
                ParseConditional(generalParser, tagArguments.Elements),

                "Environment" =>
                Expression.EnvironmentInstance,

                "StringTag" =>
                ParseStringTag(generalParser, tagArguments.Elements),

                var otherTag =>
                "Tag name does not match any known expression variant: " + otherTag
            };
    }

    private static Result<string, Expression> ParseExpressionList(PineValue pineValue)
    {
        if (pineValue is not PineValue.ListValue listValue)
            return "Expected list value";

        var expressions = new Expression[listValue.Elements.Length];

        for (var i = 0; i < listValue.Elements.Length; ++i)
        {
            var itemValue = listValue.Elements.Span[i];

            var parseItemResult = ParseExpressionFromValue(itemValue);

            if (parseItemResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse list item at index " + i + ": " + err;
            }

            if (parseItemResult.IsOkOrNull() is not { } itemExpr)
            {
                throw new NotImplementedException(
                    "Unexpected result type: " + parseItemResult.GetType().FullName);
            }

            expressions[i] = itemExpr;
        }

        return Expression.ListInstance(expressions);
    }

    private static PineValue.ListValue EncodeParseAndEval(Expression.ParseAndEval parseAndEval) =>
        EncodeChoiceTypeVariantAsPineValue("ParseAndEval",
            PineValue.List(
                [
                EncodeExpressionAsValue(parseAndEval.Encoded),
                EncodeExpressionAsValue(parseAndEval.Environment)
                ]));

    private static Result<string, Expression> ParseParseAndEval(
        Func<PineValue, Result<string, Expression>> generalParser,
        ReadOnlyMemory<PineValue> arguments)
    {
        if (arguments.Length is not 2)
        {
            return "Unexpected number of arguments under parse and eval, not two but " + arguments.Length;
        }

        var parseEncodedResult = generalParser(arguments.Span[0]);

        {
            if (parseEncodedResult.IsErrOrNull() is { } err)
                return err;
        }

        if (parseEncodedResult.IsOkOrNull() is not { } encoded)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseEncodedResult.GetType().FullName);
        }

        var parseEnvironmentResult = generalParser(arguments.Span[1]);

        {
            if (parseEnvironmentResult.IsErrOrNull() is { } err)
                return err;
        }

        if (parseEnvironmentResult.IsOkOrNull() is not { } environment)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseEnvironmentResult.GetType().FullName);
        }

        return new Expression.ParseAndEval(encoded: encoded, environment: environment);
    }

    /// <summary>
    /// Encodes a kernel application expression as a Pine value.
    /// </summary>
    private static PineValue.ListValue EncodeKernelApplication(
        Expression.KernelApplication kernelApplicationExpression) =>
        EncodeChoiceTypeVariantAsPineValue(
            "KernelApplication",
            PineValue.List(
                [
                StringEncoding.BlobValueFromString(kernelApplicationExpression.Function),
                EncodeExpressionAsValue(kernelApplicationExpression.Input)
                ]));

    private static Result<string, Expression> ParseKernelApplication(
        Func<PineValue, Result<string, Expression>> generalParser,
        ReadOnlyMemory<PineValue> arguments)
    {
        if (arguments.Length is not 2)
        {
            return "Unexpected number of arguments under kernel application, not two but " + arguments.Length;
        }

        var parseFunctionResult =
            StringEncoding.StringFromValue(arguments.Span[0]);

        {
            if (parseFunctionResult.IsErrOrNull() is { } err)
                return err;
        }

        if (parseFunctionResult.IsOkOrNull() is not { } function)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseFunctionResult.GetType().FullName);
        }

        var parseInputResult = generalParser(arguments.Span[1]);

        {
            if (parseInputResult.IsErrOrNull() is { } err)
                return err;
        }

        if (parseInputResult.IsOkOrNull() is not { } input)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseInputResult.GetType().FullName);
        }

        return new Expression.KernelApplication(function: function, input: input);
    }

    private static PineValue.ListValue EncodeConditional(
        Expression.Conditional conditionalExpression) =>
        EncodeChoiceTypeVariantAsPineValue(
            "Conditional",
            PineValue.List(
                [
                EncodeExpressionAsValue(conditionalExpression.Condition),
                EncodeExpressionAsValue(conditionalExpression.FalseBranch),
                EncodeExpressionAsValue(conditionalExpression.TrueBranch)
                ]));

    private static Result<string, Expression> ParseConditional(
        Func<PineValue, Result<string, Expression>> generalParser,
        ReadOnlyMemory<PineValue> arguments)
    {
        if (arguments.Length is not 3)
        {
            return "Unexpected number of arguments under conditional, not three but " + arguments.Length;
        }

        var parseConditionResult = generalParser(arguments.Span[0]);

        {
            if (parseConditionResult.IsErrOrNull() is { } err)
                return err;
        }

        if (parseConditionResult.IsOkOrNull() is not { } condition)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseConditionResult.GetType().FullName);
        }

        var parseFalseBranchResult = generalParser(arguments.Span[1]);

        {
            if (parseFalseBranchResult.IsErrOrNull() is { } err)
                return err;
        }

        if (parseFalseBranchResult.IsOkOrNull() is not { } falseBranch)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseFalseBranchResult.GetType().FullName);
        }

        var parseTrueBranchResult = generalParser(arguments.Span[2]);

        {
            if (parseTrueBranchResult.IsErrOrNull() is { } err)
                return err;
        }

        if (parseTrueBranchResult.IsOkOrNull() is not { } trueBranch)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseTrueBranchResult.GetType().FullName);
        }

        return Expression.ConditionalInstance(
            condition: condition,
            falseBranch: falseBranch,
            trueBranch: trueBranch);
    }

    private static Result<string, Expression> ParseStringTag(
        Func<PineValue, Result<string, Expression>> generalParser,
        ReadOnlyMemory<PineValue> arguments) =>
        arguments.Length is not 2
        ?
        "Unexpected number of arguments under string tag, not two but " + arguments.Length
        :
        StringEncoding.StringFromValue(arguments.Span[0]) switch
        {
            Result<string, string>.Err err =>
            err.Value,

            Result<string, string>.Ok tag =>
            generalParser(arguments.Span[1]) switch
            {
                Result<string, Expression>.Err err =>
                err.Value,

                Result<string, Expression>.Ok tagged =>
                new Expression.StringTag(tag: tag.Value, tagged: tagged.Value),

                var other =>
                throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
            },

            var other =>
            throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
        };

    private static Result<ErrT, IReadOnlyList<MappedOkT>> ResultListMapCombine<ErrT, OkT, MappedOkT>(
        IReadOnlyList<OkT> list,
        Func<OkT, Result<ErrT, MappedOkT>> mapElement) =>
        list.Select(mapElement).ListCombine();

    private static Result<string, Record> ParseRecord3FromPineValue<FieldA, FieldB, FieldC, Record>(
        PineValue value,
        (string name, Func<PineValue, Result<string, FieldA>> decode) fieldA,
        (string name, Func<PineValue, Result<string, FieldB>> decode) fieldB,
        (string name, Func<PineValue, Result<string, FieldC>> decode) fieldC,
        Func<FieldA, FieldB, FieldC, Record> compose) =>
        ParseRecordFromPineValue(value) switch
        {
            Result<string, IReadOnlyDictionary<string, PineValue>>.Err err =>
            (Result<string, Record>)("Failed to decode as record: " + err.Value),

            Result<string, IReadOnlyDictionary<string, PineValue>>.Ok record =>
            record.Value.TryGetValue(fieldA.name, out var fieldAValue) switch
            {
                false =>
                "Did not find field " + fieldA.name,

                true =>
                record.Value.TryGetValue(fieldB.name, out var fieldBValue) switch
                {
                    false =>
                    "Did not find field " + fieldB.name,

                    true =>
                    record.Value.TryGetValue(fieldC.name, out var fieldCValue) switch
                    {
                        false =>
                        "Did not find field " + fieldC.name,

                        true =>
                        fieldA.decode(fieldAValue) switch
                        {
                            Result<string, FieldA>.Err err =>
                            (Result<string, Record>)err.Value,

                            Result<string, FieldA>.Ok fieldADecoded =>
                            fieldB.decode(fieldBValue) switch
                            {
                                Result<string, FieldB>.Err err =>
                                (Result<string, Record>)err.Value,

                                Result<string, FieldB>.Ok fieldBDecoded =>
                                fieldC.decode(fieldCValue) switch
                                {
                                    Result<string, FieldC>.Err err =>
                                    (Result<string, Record>)err.Value,

                                    Result<string, FieldC>.Ok fieldCDecoded =>
                                    Result<string, Record>.ok(compose(fieldADecoded.Value, fieldBDecoded.Value, fieldCDecoded.Value)),

                                    var other =>
                                    throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                                },

                                var other =>
                                throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                            },

                            var other =>
                            throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                        }
                    }
                }
            },

            var other =>
            throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
        };

    private static Result<string, Record> DecodeRecord2FromPineValue<FieldA, FieldB, Record>(
        PineValue value,
        (string name, Func<PineValue, Result<string, FieldA>> decode) fieldA,
        (string name, Func<PineValue, Result<string, FieldB>> decode) fieldB,
        Func<FieldA, FieldB, Record> compose) =>
        ParseRecordFromPineValue(value) switch
        {
            Result<string, IReadOnlyDictionary<string, PineValue>>.Err err =>
            (Result<string, Record>)("Failed to decode as record: " + err.Value),

            Result<string, IReadOnlyDictionary<string, PineValue>>.Ok record =>
            record.Value.TryGetValue(fieldA.name, out var fieldAValue) switch
            {
                false =>
                "Did not find field " + fieldA.name,

                true =>
                record.Value.TryGetValue(fieldB.name, out var fieldBValue) switch
                {
                    false =>
                    "Did not find field " + fieldB.name,

                    true =>
                    fieldA.decode(fieldAValue) switch
                    {
                        Result<string, FieldA>.Err err =>
                        (Result<string, Record>)err.Value,

                        Result<string, FieldA>.Ok fieldADecoded =>
                        fieldB.decode(fieldBValue) switch
                        {
                            Result<string, FieldB>.Err err =>
                            (Result<string, Record>)err.Value,

                            Result<string, FieldB>.Ok fieldBDecoded =>
                            Result<string, Record>.ok(compose(fieldADecoded.Value, fieldBDecoded.Value)),

                            var other =>
                            throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                        },

                        var other =>
                        throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                    }
                }
            },

            var other =>
            throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
        };

    private static PineValue.ListValue EncodeRecordToPineValue(
        params (string fieldName, PineValue fieldValue)[] fields) =>
        PineValue.List(
            [..fields.Select(field => PineValue.List(
                [
                    StringEncoding.ValueFromString(field.fieldName),
                    field.fieldValue
                ]))]);


    private static Result<string, IReadOnlyDictionary<string, PineValue>> ParseRecordFromPineValue(PineValue value)
    {
        if (ParsePineListValue(value) is not Result<string, ReadOnlyMemory<PineValue>>.Ok listResult)
            return "Failed to parse as list";

        var recordFields = new Dictionary<string, PineValue>(listResult.Value.Length);

        for (var i = 0; i < listResult.Value.Length; ++i)
        {
            var listElement = listResult.Value.Span[i];

            if (ParsePineListValue(listElement).IsOkOrNullable() is not { } listElementResult)
                return "Failed to parse list element as list";

            if (ParseListWithExactlyTwoElements(listElementResult).IsOkOrNullable() is not { } fieldNameValueAndValue)
                return "Failed to parse list element as list with exactly two elements";

            if (StringEncoding.StringFromValue(fieldNameValueAndValue.Item1).IsOkOrNull() is not { } fieldName)
                return "Failed to parse field name as string";

            recordFields[fieldName] = fieldNameValueAndValue.Item2;
        }

        return recordFields;
    }

    private static PineValue.ListValue EncodeChoiceTypeVariantAsPineValue(string tagName, PineValue tagArguments) =>
        PineValue.List(
            [
                StringEncoding.BlobValueFromString(tagName),
                tagArguments,
            ]);

    private static Result<string, T> ParseChoiceFromPineValue<T>(
        Func<PineValue, Result<string, T>> generalParser,
        IReadOnlyDictionary<PineValue, Func<Func<PineValue, Result<string, T>>, PineValue, Result<string, T>>> variants,
        PineValue value) =>
        ParsePineListValue(value) switch
        {
            Result<string, ReadOnlyMemory<PineValue>>.Err err =>
            err.Value,

            Result<string, ReadOnlyMemory<PineValue>>.Ok list =>
            ParseListWithExactlyTwoElements(list.Value) switch
            {
                Result<string, (PineValue, PineValue)>.Err err =>
                err.Value,

                Result<string, (PineValue, PineValue)>.Ok tagNameValueAndValue =>
                StringEncoding.StringFromValue(tagNameValueAndValue.Value.Item1) switch
                {
                    Result<string, string>.Err err =>
                    err.Value,

                    Result<string, string>.Ok tagName =>
                    variants.TryGetValue(tagNameValueAndValue.Value.Item1, out var variant) switch
                    {
                        false =>
                        "Unexpected tag name: " + tagName,

                        true =>
                        variant(generalParser, tagNameValueAndValue.Value.Item2)
                    },

                    var other =>
                    throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                },

                var other =>
                throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
            },

            var other =>
            throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
        };

    private static Result<string, ReadOnlyMemory<PineValue>> ParsePineListValue(PineValue value)
    {
        if (value is not PineValue.ListValue listValue)
            return "Not a list";

        return
            Result<string, ReadOnlyMemory<PineValue>>.ok(listValue.Elements);
    }

    private static Result<string, (T, T)> ParseListWithExactlyTwoElements<T>(ReadOnlyMemory<T> list)
    {
        if (list.Length is not 2)
            return "Unexpected number of items in list: Not 2 but " + list.Length;

        return (list.Span[0], list.Span[1]);
    }
}
