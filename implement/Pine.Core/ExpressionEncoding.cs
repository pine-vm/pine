using System.Collections.Generic;
using System;
using System.Linq;

namespace Pine.Core;

/// <summary>
/// The standard encoding of Pine expressions as Pine values.
/// This is the encoding used to map from data to code when evaluating a <see cref="Expression.ParseAndEval"/> expression.
/// </summary>
public static class ExpressionEncoding
{
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
                PineValue.List([PineValueAsString.ValueFromString(stringTag.tag), EncodeExpressionAsValue(stringTag.tagged)])),

            _ =>
            throw new Exception("Unsupported expression type: " + expression.GetType().FullName)
        };

    public static Result<string, Expression> ParseExpressionFromValueDefault(
        PineValue value)
    {
        if (value is PineValue.ListValue listValue)
        {
            if (ReusedInstances.Instance.ExpressionDecodings?.TryGetValue(listValue, out var popularExpression) ?? false)
                return popularExpression;
        }

        return
            ParseExpressionFromValueDefault(
                value,
                generalParser: ParseExpressionFromValueDefault);
    }

    public static Result<string, Expression> ParseExpressionFromValueDefault(
        PineValue value,
        Func<PineValue, Result<string, Expression>> generalParser) =>
        value switch
        {
            PineValue.ListValue rootList =>
            rootList.Elements.Count is not 2
            ?
            "Unexpected number of items in list: Not 2 but " + rootList.Elements.Count
            :
            PineValueAsString.StringFromValue(rootList.Elements[0]) switch
            {
                Result<string, string>.Err err =>
                err.Value,

                Result<string, string>.Ok tag =>
                rootList.Elements[1] is not PineValue.ListValue tagArguments
                ?
                "Unexpected shape of tag argument value: Not a list"
                :
                tag.Value switch
                {
                    "Literal" =>
                    tagArguments.Elements.Count < 1
                    ?
                    "Expected one argument for literal but got zero"
                    :
                    Expression.LiteralInstance(tagArguments.Elements[0]),

                    "List" =>
                    tagArguments.Elements.Count < 1
                    ?
                    "Expected one argument for list but got zero"
                    :
                    ParsePineListValue(tagArguments.Elements[0]) switch
                    {
                        Result<string, IReadOnlyList<PineValue>>.Err err =>
                        (Result<string, Expression>)err.Value,

                        Result<string, IReadOnlyList<PineValue>>.Ok list =>
                        ResultListMapCombine(list.Value, generalParser) switch
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

    public static PineValue.ListValue EncodeParseAndEval(Expression.ParseAndEval parseAndEval) =>
        EncodeChoiceTypeVariantAsPineValue("ParseAndEval",
            PineValue.List(
                [
                EncodeExpressionAsValue(parseAndEval.encoded),
                EncodeExpressionAsValue(parseAndEval.environment)
                ]));

    public static Result<string, Expression.ParseAndEval> ParseParseAndEval(
        Func<PineValue, Result<string, Expression>> generalParser,
        IReadOnlyList<PineValue> arguments) =>
        arguments.Count < 2
        ?
        "Expected two arguments under parse and eval, but got " + arguments.Count
        :
        generalParser(arguments[0])
        .AndThen(encoded =>
        generalParser(arguments[1])
        .Map(environment => new Expression.ParseAndEval(encoded: encoded, environment: environment)));

    public static PineValue.ListValue EncodeKernelApplication(
        Expression.KernelApplication kernelApplicationExpression) =>
        EncodeChoiceTypeVariantAsPineValue(
            "KernelApplication",
            PineValue.List(
                [
                PineValueAsString.ValueFromString(kernelApplicationExpression.function),
                EncodeExpressionAsValue(kernelApplicationExpression.input)
                ]));

    public static Result<string, Expression.KernelApplication> ParseKernelApplication(
        Func<PineValue, Result<string, Expression>> generalParser,
        IReadOnlyList<PineValue> arguments) =>
        arguments.Count < 2
        ?
        "Expected two arguments under kernel application, but got " + arguments.Count
        :
        PineValueAsString.StringFromValue(arguments[0])
        .AndThen(function =>
        generalParser(arguments[1])
        .Map(input => new Expression.KernelApplication(
            function: function,
            input: input)));

    public static PineValue.ListValue EncodeConditional(
        Expression.Conditional conditionalExpression) =>
        EncodeChoiceTypeVariantAsPineValue(
            "Conditional",
            PineValue.List(
                [
                EncodeExpressionAsValue(conditionalExpression.condition),
                EncodeExpressionAsValue(conditionalExpression.falseBranch),
                EncodeExpressionAsValue(conditionalExpression.trueBranch)
                ]));

    public static Result<string, Expression.Conditional> ParseConditional(
        Func<PineValue, Result<string, Expression>> generalParser,
        IReadOnlyList<PineValue> arguments) =>
        arguments.Count < 3
        ?
        "Expected 3 arguments under conditional, but got " + arguments.Count
        :
        generalParser(arguments[0])
        .AndThen(condition =>
        generalParser(arguments[1])
        .AndThen(falseBranch =>
        generalParser(arguments[2])
        .Map(trueBranch =>
            Expression.ConditionalInstance(
                condition: condition,
                falseBranch: falseBranch,
                trueBranch: trueBranch))));

    public static Result<string, Expression.StringTag> ParseStringTag(
        Func<PineValue, Result<string, Expression>> generalParser,
        IReadOnlyList<PineValue> arguments) =>
        arguments.Count < 2
        ?
        "Expected 2 arguments under string tag, but got " + arguments.Count
        :
        PineValueAsString.StringFromValue(arguments[0]) switch
        {
            Result<string, string>.Err err =>
            err.Value,

            Result<string, string>.Ok tag =>
            generalParser(arguments[1]) switch
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

    public static Result<ErrT, IReadOnlyList<MappedOkT>> ResultListMapCombine<ErrT, OkT, MappedOkT>(
        IReadOnlyList<OkT> list,
        Func<OkT, Result<ErrT, MappedOkT>> mapElement) =>
        list.Select(mapElement).ListCombine();

    public static Result<string, Record> ParseRecord3FromPineValue<FieldA, FieldB, FieldC, Record>(
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

    public static Result<string, Record> DecodeRecord2FromPineValue<FieldA, FieldB, Record>(
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

    public static PineValue.ListValue EncodeRecordToPineValue(
        params (string fieldName, PineValue fieldValue)[] fields) =>
        PineValue.List(
            [..fields.Select(field => PineValue.List(
                [
                    PineValueAsString.ValueFromString(field.fieldName),
                    field.fieldValue
                ]))]);


    public static Result<string, IReadOnlyDictionary<string, PineValue>> ParseRecordFromPineValue(PineValue value)
    {
        if (ParsePineListValue(value) is not Result<string, IReadOnlyList<PineValue>>.Ok listResult)
            return "Failed to parse as list";

        var recordFields = new Dictionary<string, PineValue>(listResult.Value.Count);

        foreach (var listElement in listResult.Value)
        {
            if (ParsePineListValue(listElement).IsOkOrNull() is not { } listElementResult)
                return "Failed to parse list element as list";

            if (ParseListWithExactlyTwoElements(listElementResult).IsOkOrNullable() is not { } fieldNameValueAndValue)
                return "Failed to parse list element as list with exactly two elements";

            if (PineValueAsString.StringFromValue(fieldNameValueAndValue.Item1).IsOkOrNull() is not { } fieldName)
                return "Failed to parse field name as string";

            recordFields[fieldName] = fieldNameValueAndValue.Item2;
        }

        return recordFields;
    }

    public static PineValue.ListValue EncodeChoiceTypeVariantAsPineValue(string tagName, PineValue tagArguments) =>
        PineValue.List(
            [
                PineValueAsString.ValueFromString(tagName),
                tagArguments,
            ]);

    public static Result<string, T> ParseChoiceFromPineValue<T>(
        Func<PineValue, Result<string, T>> generalParser,
        IReadOnlyDictionary<PineValue, Func<Func<PineValue, Result<string, T>>, PineValue, Result<string, T>>> variants,
        PineValue value) =>
        ParsePineListValue(value) switch
        {
            Result<string, IReadOnlyList<PineValue>>.Err err =>
            err.Value,

            Result<string, IReadOnlyList<PineValue>>.Ok list =>
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

    public static Result<string, IReadOnlyList<PineValue>> ParsePineListValue(PineValue value)
    {
        if (value is not PineValue.ListValue listValue)
            return "Not a list";

        return
            Result<string, IReadOnlyList<PineValue>>.ok(listValue.Elements);
    }

    public static Result<string, (T, T)> ParseListWithExactlyTwoElements<T>(IReadOnlyList<T> list)
    {
        if (list.Count is not 2)
            return "Unexpected number of items in list: Not 2 but " + list.Count;

        return (list[0], list[1]);
    }
}
