using System.Collections.Generic;
using System;
using System.Linq;
using System.Collections.Immutable;
using System.Collections.Frozen;

namespace Pine.PineVM;

public static class ExpressionEncoding
{
    public static readonly IReadOnlyDictionary<Expression, PineValue> PopularPineExpressionsEncoded =
        PopularPineExpressionsSource()
        .Distinct()
        .ToFrozenDictionary(
            keySelector: expression => expression,
            elementSelector: e => EncodeExpressionAsValue(e).Extract(err => throw new Exception(err)));

    private static readonly IReadOnlyDictionary<string, Func<PineValue, PineValue>> NamedKernelFunctions =
        ImmutableDictionary<string, Func<PineValue, PineValue>>.Empty
        .SetItem(nameof(KernelFunction.equal), KernelFunction.equal)
        .SetItem(nameof(KernelFunction.negate), KernelFunction.negate)
        .SetItem(nameof(KernelFunction.length), KernelFunction.length)
        .SetItem(nameof(KernelFunction.skip), KernelFunction.skip)
        .SetItem(nameof(KernelFunction.take), KernelFunction.take)
        .SetItem(nameof(KernelFunction.reverse), KernelFunction.reverse)
        .SetItem(nameof(KernelFunction.concat), KernelFunction.concat)
        .SetItem(nameof(KernelFunction.list_head), KernelFunction.list_head)
        .SetItem(nameof(KernelFunction.add_int), KernelFunction.add_int)
        .SetItem(nameof(KernelFunction.mul_int), KernelFunction.mul_int)
        .SetItem(nameof(KernelFunction.is_sorted_ascending_int), KernelFunction.is_sorted_ascending_int);

    public static Result<string, PineValue> EncodeExpressionAsValue(Expression expression) =>
        PopularPineExpressionsEncoded?.TryGetValue(expression, out var encoded) ?? false && encoded is not null
        ? encoded
        :
        expression switch
        {
            Expression.LiteralExpression literal =>
            EncodeChoiceTypeVariantAsPineValue("Literal", literal.Value),

            Expression.EnvironmentExpression =>
            EncodeChoiceTypeVariantAsPineValue("Environment", PineValue.EmptyList),

            Expression.ListExpression list =>
            list.List.Select(EncodeExpressionAsValue)
            .ListCombine()
            .Map(listElements => EncodeChoiceTypeVariantAsPineValue("List", PineValue.List(listElements))),

            Expression.ConditionalExpression conditional =>
            EncodeConditionalExpressionAsValue(conditional),

            Expression.ParseAndEvalExpression parseAndEval =>
            EncodeParseAndEvalExpression(parseAndEval),

            Expression.KernelApplicationExpression kernelAppl =>
            EncodeKernelApplicationExpression(kernelAppl),

            Expression.StringTagExpression stringTag =>
            EncodeExpressionAsValue(stringTag.tagged)
            .Map(encodedTagged => EncodeChoiceTypeVariantAsPineValue(
                "StringTag",
                PineValue.List([PineValueAsString.ValueFromString(stringTag.tag), encodedTagged]))),

            _ =>
            "Unsupported expression type: " + expression.GetType().FullName
        };

    public static Result<string, Expression> ParseExpressionFromValue(
        PineValue value,
        IReadOnlyDictionary<PineValue, Expression.DelegatingExpression> parseExpressionOverrides)
    {
        if (parseExpressionOverrides.TryGetValue(value, out var delegatingExpression))
            return delegatingExpression;

        return
            ParseExpressionFromValueDefault(
                value,
                generalParser: value => ParseExpressionFromValue(value, parseExpressionOverrides));
    }

    public static Result<string, Expression> ParseExpressionFromValueDefault(
        PineValue value) =>
        ParseExpressionFromValueDefault(
            value,
            generalParser: ParseExpressionFromValueDefault);

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
                tag.Value switch
                {
                    "Literal" =>
                    new Expression.LiteralExpression(rootList.Elements[1]),

                    "List" =>
                    ParsePineListValue(rootList.Elements[1]) switch
                    {
                        Result<string, IReadOnlyList<PineValue>>.Err err =>
                        (Result<string, Expression>)err.Value,

                        Result<string, IReadOnlyList<PineValue>>.Ok list =>
                        ResultListMapCombine(list.Value, generalParser) switch
                        {
                            Result<string, IReadOnlyList<Expression>>.Err err =>
                            (Result<string, Expression>)err.Value,

                            Result<string, IReadOnlyList<Expression>>.Ok expressionList =>
                            Result<string, Expression>.ok(new Expression.ListExpression(expressionList.Value)),

                            var other =>
                            throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                        },

                        var other =>
                        throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                    },

                    "ParseAndEval" =>
                    ParseParseAndEvalExpression(generalParser, rootList.Elements[1]) switch
                    {
                        Result<string, Expression.ParseAndEvalExpression>.Err err =>
                        (Result<string, Expression>)err.Value,

                        Result<string, Expression.ParseAndEvalExpression>.Ok parseAndEval =>
                        (Result<string, Expression>)parseAndEval.Value,

                        var other =>
                        throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                    },

                    "KernelApplication" =>
                    ParseKernelApplicationExpression(generalParser, rootList.Elements[1]) switch
                    {
                        Result<string, Expression.KernelApplicationExpression>.Err err =>
                        (Result<string, Expression>)err.Value,

                        Result<string, Expression.KernelApplicationExpression>.Ok kernelApplication =>
                        (Result<string, Expression>)kernelApplication.Value,

                        var other =>
                        throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                    },

                    "Conditional" =>
                    ParseConditionalExpression(generalParser, rootList.Elements[1]) switch
                    {
                        Result<string, Expression.ConditionalExpression>.Err err =>
                        (Result<string, Expression>)err.Value,

                        Result<string, Expression.ConditionalExpression>.Ok conditional =>
                        (Result<string, Expression>)conditional.Value,

                        var other =>
                        throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
                    },

                    "Environment" =>
                    Expression.Environment,

                    "StringTag" =>
                    ParseStringTagExpression(generalParser, rootList.Elements[1])
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

    public static Result<string, PineValue> EncodeParseAndEvalExpression(Expression.ParseAndEvalExpression parseAndEval) =>
        EncodeExpressionAsValue(parseAndEval.expression)
        .AndThen(encodedExpression =>
        EncodeExpressionAsValue(parseAndEval.environment)
        .Map(encodedEnvironment =>
        EncodeChoiceTypeVariantAsPineValue("ParseAndEval",
            EncodeRecordToPineValue(
                (nameof(Expression.ParseAndEvalExpression.environment), encodedEnvironment),
                (nameof(Expression.ParseAndEvalExpression.expression), encodedExpression)))));

    public static Result<string, Expression.ParseAndEvalExpression> ParseParseAndEvalExpression(
        Func<PineValue, Result<string, Expression>> generalParser,
        PineValue value) =>
        DecodeRecord2FromPineValue(
            value,
            ("environment", generalParser),
            ("expression", generalParser),
            (environment, expression) => new Expression.ParseAndEvalExpression(expression: expression, environment: environment));

    public static Result<string, PineValue> EncodeKernelApplicationExpression(Expression.KernelApplicationExpression kernelApplicationExpression) =>
        EncodeExpressionAsValue(kernelApplicationExpression.argument)
        .Map(encodedArgument =>
        EncodeChoiceTypeVariantAsPineValue("KernelApplication",
            EncodeRecordToPineValue(
                (nameof(Expression.KernelApplicationExpression.argument), encodedArgument),
                (nameof(Expression.KernelApplicationExpression.functionName), PineValueAsString.ValueFromString(kernelApplicationExpression.functionName)))));

    public static Result<string, Expression.KernelApplicationExpression> ParseKernelApplicationExpression(
        Func<PineValue, Result<string, Expression>> generalParser,
        PineValue value) =>
        DecodeRecord2FromPineValue(
            value,
            (nameof(Expression.KernelApplicationExpression.functionName), decode: PineValueAsString.StringFromValue),
            (nameof(Expression.KernelApplicationExpression.argument), decode: generalParser),
            (functionName, argument) => (functionName, argument))
        switch
        {
            Result<string, (string functionName, Expression argument)>.Err err =>
            err.Value,

            Result<string, (string functionName, Expression argument)>.Ok functionNameAndArgument =>
            Expression.KernelApplication(functionNameAndArgument.Value.argument, functionNameAndArgument.Value.functionName),

            var other =>
            throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
        };

    public static Expression.KernelApplicationExpression ParseKernelApplicationExpressionThrowOnUnknownName(
        string functionName,
        Expression argument) =>
        new(argument: argument, functionName: functionName);

    public static Result<string, PineValue> EncodeConditionalExpressionAsValue(Expression.ConditionalExpression conditionalExpression) =>
        EncodeExpressionAsValue(conditionalExpression.condition)
        .AndThen(encodedCondition =>
        EncodeExpressionAsValue(conditionalExpression.trueBranch)
        .AndThen(encodedIfTrue =>
        EncodeExpressionAsValue(conditionalExpression.falseBranch)
        .Map(encodedIfFalse =>
        EncodeChoiceTypeVariantAsPineValue("Conditional",
            EncodeRecordToPineValue(
                (nameof(Expression.ConditionalExpression.condition), encodedCondition),
                (nameof(Expression.ConditionalExpression.falseBranch), encodedIfFalse),
                (nameof(Expression.ConditionalExpression.trueBranch), encodedIfTrue))))));

    public static Result<string, Expression.ConditionalExpression> ParseConditionalExpression(
        Func<PineValue, Result<string, Expression>> generalParser,
        PineValue value) =>
        ParseRecord3FromPineValue(
            value,
            (nameof(Expression.ConditionalExpression.condition), generalParser),
            (nameof(Expression.ConditionalExpression.falseBranch), generalParser),
            (nameof(Expression.ConditionalExpression.trueBranch), generalParser),
            (condition, falseBranch, trueBranch) =>
            new Expression.ConditionalExpression(
                condition: condition,
                falseBranch: falseBranch,
                trueBranch: trueBranch));

    public static Result<string, Expression.StringTagExpression> ParseStringTagExpression(
        Func<PineValue, Result<string, Expression>> generalParser,
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

                Result<string, (PineValue, PineValue)>.Ok tagValueAndTaggedValue =>
                PineValueAsString.StringFromValue(tagValueAndTaggedValue.Value.Item1) switch
                {
                    Result<string, string>.Err err =>
                    err.Value,

                    Result<string, string>.Ok tag =>
                    generalParser(tagValueAndTaggedValue.Value.Item2) switch
                    {
                        Result<string, Expression>.Err err =>
                        err.Value,

                        Result<string, Expression>.Ok tagged =>
                        new Expression.StringTagExpression(tag: tag.Value, tagged: tagged.Value),

                        var other =>
                        throw new NotImplementedException("Unexpected result type: " + other.GetType().FullName)
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

    public static PineValue EncodeRecordToPineValue(params (string fieldName, PineValue fieldValue)[] fields) =>
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
            if (ParsePineListValue(listElement) is not Result<string, IReadOnlyList<PineValue>>.Ok listElementResult)
                return "Failed to parse list element as list";

            if (ParseListWithExactlyTwoElements(listElementResult.Value) is not Result<string, (PineValue, PineValue)>.Ok fieldNameValueAndValue)
                return "Failed to parse list element as list with exactly two elements";

            if (PineValueAsString.StringFromValue(fieldNameValueAndValue.Value.Item1) is not Result<string, string>.Ok fieldName)
                return "Failed to parse field name as string";

            recordFields[fieldName.Value] = fieldNameValueAndValue.Value.Item2;
        }

        return recordFields;
    }

    public static PineValue EncodeChoiceTypeVariantAsPineValue(string tagName, PineValue tagArguments) =>
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

    private static IEnumerable<Expression> PopularPineExpressionsSource()
    {
        yield return Expression.Environment;

        for (var i = -100; i < 100; ++i)
        {
            yield return new Expression.LiteralExpression(
                PineValueAsInteger.ValueFromSignedInteger(i));
        }

        Expression.KernelApplicationExpression kernelSkipExprFromCount(
            int skipCount,
            Expression argument) =>
            new(
                argument: new Expression.ListExpression(
                    [new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(skipCount)),
                    argument]),
                functionName: nameof(KernelFunction.skip));

        Expression.KernelApplicationExpression kernelListHeadExpr(
            Expression argument) =>
            new(
                argument: argument,
                functionName: nameof(KernelFunction.list_head));

        var envListHeadExpr = kernelListHeadExpr(Expression.Environment);

        yield return envListHeadExpr;

        for (var skipCount = 0; skipCount < 10; skipCount++)
        {
            var justSkip = kernelSkipExprFromCount(skipCount, Expression.Environment);

            yield return justSkip;

            yield return kernelListHeadExpr(justSkip);
        }

        for (var skipCount = 0; skipCount < 10; skipCount++)
        {
            var justSkip = kernelSkipExprFromCount(skipCount, envListHeadExpr);

            yield return justSkip;

            yield return kernelListHeadExpr(justSkip);
        }
    }
}
