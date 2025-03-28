﻿using System.Collections.Generic;
using System;
using System.Linq;

namespace Pine.Core;

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
            EncodeChoiceTypeVariantAsPineValue("Environment", PineValue.EmptyList),

            Expression.List list =>
            EncodeChoiceTypeVariantAsPineValue(
                "List", PineValue.List([PineValue.List([.. list.items.Select(EncodeExpressionAsValue)])])),

            Expression.Conditional conditional =>
            EncodeConditional(conditional),

            Expression.ParseAndEval parseAndEval =>
            EncodeParseAndEval(parseAndEval),

            Expression.KernelApplication kernelAppl =>
            EncodeKernelApplication(kernelAppl),

            Expression.StringTag stringTag =>
            EncodeChoiceTypeVariantAsPineValue(
                "StringTag",
                PineValue.List([PineValueAsString.ValueFromString(stringTag.Tag), EncodeExpressionAsValue(stringTag.Tagged)])),

            _ =>
            throw new Exception("Unsupported expression type: " + expression.GetType().FullName)
        };

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
        Func<PineValue, Result<string, Expression>> generalParser) =>
        value switch
        {
            PineValue.ListValue rootList =>
            rootList.Elements.Length is not 2
            ?
            "Unexpected number of items in list: Not 2 but " + rootList.Elements.Length
            :
            PineValueAsString.StringFromValue(rootList.Elements.Span[0]) switch
            {
                Result<string, string>.Err err =>
                err.Value,

                Result<string, string>.Ok tag =>
                rootList.Elements.Span[1] is not PineValue.ListValue tagArguments
                ?
                "Unexpected shape of tag argument value: Not a list"
                :
                tag.Value switch
                {
                    "Literal" =>
                    tagArguments.Elements.Length < 1
                    ?
                    "Expected one argument for literal but got zero"
                    :
                    Expression.LiteralInstance(tagArguments.Elements.Span[0]),

                    "List" =>
                    tagArguments.Elements.Length < 1
                    ?
                    "Expected one argument for list but got zero"
                    :
                    ParsePineListValue(tagArguments.Elements.Span[0]) switch
                    {
                        Result<string, ReadOnlyMemory<PineValue>>.Err err =>
                        (Result<string, Expression>)err.Value,

                        Result<string, ReadOnlyMemory<PineValue>>.Ok list =>
                        ResultListMapCombine(list.Value.ToArray(), generalParser) switch
                        {
                            Result<string, IReadOnlyList<Expression>>.Err err =>
                            (Result<string, Expression>)err.Value,

                            Result<string, IReadOnlyList<Expression>>.Ok expressionList =>
                            Result<string, Expression>.ok(Expression.ListInstance(expressionList.Value)),

                            var other =>
                            throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                        },

                        var other =>
                        throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                    },

                    "ParseAndEval" =>
                    ParseParseAndEval(generalParser, tagArguments.Elements) switch
                    {
                        Result<string, Expression.ParseAndEval>.Err err =>
                        (Result<string, Expression>)err.Value,

                        Result<string, Expression.ParseAndEval>.Ok parseAndEval =>
                        (Result<string, Expression>)parseAndEval.Value,

                        var other =>
                        throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                    },

                    "KernelApplication" =>
                    ParseKernelApplication(generalParser, tagArguments.Elements) switch
                    {
                        Result<string, Expression.KernelApplication>.Err err =>
                        (Result<string, Expression>)err.Value,

                        Result<string, Expression.KernelApplication>.Ok kernelApplication =>
                        (Result<string, Expression>)kernelApplication.Value,

                        var other =>
                        throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                    },

                    "Conditional" =>
                    ParseConditional(generalParser, tagArguments.Elements) switch
                    {
                        Result<string, Expression.Conditional>.Err err =>
                        (Result<string, Expression>)err.Value,

                        Result<string, Expression.Conditional>.Ok conditional =>
                        (Result<string, Expression>)conditional.Value,

                        var other =>
                        throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                    },

                    "Environment" =>
                    Expression.EnvironmentInstance,

                    "StringTag" =>
                    ParseStringTag(generalParser, tagArguments.Elements)
                    .Map(stringTag => (Expression)stringTag),

                    var otherTag =>
                    "Tag name does not match any known expression variant: " + otherTag
                },

                var other =>
                "Unexpected result type: " + other.GetType().FullName
            },

            var other =>
            "Unexpected value type, not a list: " + other.GetType().FullName
        };

    private static PineValue.ListValue EncodeParseAndEval(Expression.ParseAndEval parseAndEval) =>
        EncodeChoiceTypeVariantAsPineValue("ParseAndEval",
            PineValue.List(
                [
                EncodeExpressionAsValue(parseAndEval.Encoded),
                EncodeExpressionAsValue(parseAndEval.Environment)
                ]));

    private static Result<string, Expression.ParseAndEval> ParseParseAndEval(
        Func<PineValue, Result<string, Expression>> generalParser,
        ReadOnlyMemory<PineValue> arguments) =>
        arguments.Length is not 2
        ?
        "Expected two arguments under parse and eval, but got " + arguments.Length
        :
        generalParser(arguments.Span[0])
        .AndThen(encoded =>
        generalParser(arguments.Span[1])
        .Map(environment => new Expression.ParseAndEval(encoded: encoded, environment: environment)));

    /// <summary>
    /// Encodes a kernel application expression as a Pine value.
    /// </summary>
    private static PineValue.ListValue EncodeKernelApplication(
        Expression.KernelApplication kernelApplicationExpression) =>
        EncodeChoiceTypeVariantAsPineValue(
            "KernelApplication",
            PineValue.List(
                [
                PineValueAsString.ValueFromString(kernelApplicationExpression.Function),
                EncodeExpressionAsValue(kernelApplicationExpression.Input)
                ]));

    private static Result<string, Expression.KernelApplication> ParseKernelApplication(
        Func<PineValue, Result<string, Expression>> generalParser,
        ReadOnlyMemory<PineValue> arguments) =>
        arguments.Length is not 2
        ?
        "Expected two arguments under kernel application, but got " + arguments.Length
        :
        PineValueAsString.StringFromValue(arguments.Span[0])
        .AndThen(function =>
        generalParser(arguments.Span[1])
        .Map(input => new Expression.KernelApplication(
            function: function,
            input: input)));

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

    private static Result<string, Expression.Conditional> ParseConditional(
        Func<PineValue, Result<string, Expression>> generalParser,
        ReadOnlyMemory<PineValue> arguments) =>
        arguments.Length is not 3
        ?
        "Expected 3 arguments under conditional, but got " + arguments.Length
        :
        generalParser(arguments.Span[0])
        .AndThen(condition =>
        generalParser(arguments.Span[1])
        .AndThen(falseBranch =>
        generalParser(arguments.Span[2])
        .Map(trueBranch =>
            Expression.ConditionalInstance(
                condition: condition,
                falseBranch: falseBranch,
                trueBranch: trueBranch))));

    private static Result<string, Expression.StringTag> ParseStringTag(
        Func<PineValue, Result<string, Expression>> generalParser,
        ReadOnlyMemory<PineValue> arguments) =>
        arguments.Length is not 2
        ?
        "Expected 2 arguments under string tag, but got " + arguments.Length
        :
        PineValueAsString.StringFromValue(arguments.Span[0]) switch
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
                    PineValueAsString.ValueFromString(field.fieldName),
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

            if (PineValueAsString.StringFromValue(fieldNameValueAndValue.Item1).IsOkOrNull() is not { } fieldName)
                return "Failed to parse field name as string";

            recordFields[fieldName] = fieldNameValueAndValue.Item2;
        }

        return recordFields;
    }

    private static PineValue.ListValue EncodeChoiceTypeVariantAsPineValue(string tagName, PineValue tagArguments) =>
        PineValue.List(
            [
                PineValueAsString.ValueFromString(tagName),
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
                PineValueAsString.StringFromValue(tagNameValueAndValue.Value.Item1) switch
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
