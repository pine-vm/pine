using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.CommonEncodings;

/// <summary>
/// The legacy 2024 encoding of Pine expressions as Pine values.
/// </summary>
public static class ExpressionEncoding2024
{
    /// <summary>
    /// The standard encoding of Pine expression as Pine value.
    /// This is the encoding used to map from data to code when evaluating a <see cref="Expression.ParseAndEval"/> expression.
    /// </summary>
    public static PineValue.ListValue EncodeExpressionAsValue(Expression expression) =>
        EncodeExpressionAsValueViaPostOrder(
            expression,
            new ConcurrentDictionary<Expression, PineValue.ListValue>());

    /// <summary>
    /// Encodes <paramref name="rootExpression"/> using an explicit work stack instead of recursion,
    /// so that arbitrarily deep expression trees do not overflow the call stack.
    /// <para>
    /// Subexpressions are encoded in post-order (children before parents) into <paramref name="store"/>,
    /// which also deduplicates shared subtrees so each distinct subexpression is encoded at most once.
    /// Because every direct child is already present in <paramref name="store"/> by the time its parent
    /// is encoded, the per-node call to
    /// <see cref="EncodeExpressionAsValueWithoutTopLevelCacheLookup"/> never recurses more than one level.
    /// </para>
    /// </summary>
    internal static PineValue.ListValue EncodeExpressionAsValueViaPostOrder(
        Expression rootExpression,
        ConcurrentDictionary<Expression, PineValue.ListValue> store)
    {
        if (store.TryGetValue(rootExpression, out var alreadyEncoded))
            return alreadyEncoded;

        var stack = new Stack<Expression>();

        stack.Push(rootExpression);

        var expanded = new HashSet<Expression>();

        while (stack.Count is not 0)
        {
            var current = stack.Peek();

            if (store.ContainsKey(current))
            {
                stack.Pop();
                continue;
            }

            if (expanded.Add(current))
            {
                // First visit: schedule not-yet-encoded direct children to be processed first.
                PushUnencodedChildren(current, store, stack);

                continue;
            }

            // Second visit: all children are encoded and present in 'store'.
            stack.Pop();

            store.GetOrAdd(
                current,
                valueFactory:
                expr =>
                EncodeExpressionAsValueWithoutTopLevelCacheLookup(
                    expr,
                    childExpression => store[childExpression]));
        }

        return store[rootExpression];
    }

    private static void PushUnencodedChildren(
        Expression expression,
        ConcurrentDictionary<Expression, PineValue.ListValue> store,
        Stack<Expression> stack)
    {
        switch (expression)
        {
            case Expression.Environment:
            case Expression.Literal:
                break;

            case Expression.List list:
                for (var i = 0; i < list.Items.Count; i++)
                {
                    if (!store.ContainsKey(list.Items[i]))
                        stack.Push(list.Items[i]);
                }

                break;

            case Expression.ParseAndEval parseAndEval:
                if (!store.ContainsKey(parseAndEval.Encoded))
                    stack.Push(parseAndEval.Encoded);

                if (!store.ContainsKey(parseAndEval.Environment))
                    stack.Push(parseAndEval.Environment);

                break;

            case Expression.KernelApplication kernelApplication:
                if (!store.ContainsKey(kernelApplication.Input))
                    stack.Push(kernelApplication.Input);

                break;

            case Expression.Conditional conditional:
                if (!store.ContainsKey(conditional.Condition))
                    stack.Push(conditional.Condition);

                if (!store.ContainsKey(conditional.FalseBranch))
                    stack.Push(conditional.FalseBranch);

                if (!store.ContainsKey(conditional.TrueBranch))
                    stack.Push(conditional.TrueBranch);

                break;

            case Expression.StringTag stringTag:
                if (!store.ContainsKey(stringTag.Tagged))
                    stack.Push(stringTag.Tagged);

                break;

            default:
                throw new NotImplementedException(
                    "Unknown expression type: " + expression.GetType().FullName);
        }
    }

    /// <summary>
    /// Performs the actual switch over <paramref name="expression"/> kinds without consulting any
    /// top-level cache for <paramref name="expression"/> itself. Recursive sub-encodings are
    /// delegated to <paramref name="encodeSubexpression"/>, which the caller uses to thread a
    /// cache, an instrumentation hook, or any other intermediary.
    /// <para>
    /// <see cref="PineExpressionEncodingCache"/> uses this overload as its factory body, passing
    /// its own <see cref="PineExpressionEncodingCache.EncodeExpressionAsValue(Expression)"/> as
    /// <paramref name="encodeSubexpression"/>, so that recursive sub-encodings hit the same cache
    /// while the entry for the key currently being populated is not re-looked-up.
    /// </para>
    /// <para>
    /// Tests can supply a counting/instrumenting delegate as <paramref name="encodeSubexpression"/>
    /// to verify that every direct subexpression of the input is consulted.
    /// </para>
    /// </summary>
    /// <param name="expression">The expression to encode.</param>
    /// <param name="encodeSubexpression">
    /// Delegate used to encode each direct subexpression of <paramref name="expression"/>.
    /// </param>
    public static PineValue.ListValue EncodeExpressionAsValueWithoutTopLevelCacheLookup(
        Expression expression,
        Func<Expression, PineValue.ListValue> encodeSubexpression)
    {
        if ((ReusedInstances.Instance.ExpressionEncodings?.TryGetValue(expression, out var encoded) ?? false) &&
            encoded is not null)
            return encoded;

        return expression switch
        {
            Expression.Literal literal =>
            EncodeChoiceTypeVariantAsPineValue(
                "Literal",
                PineValue.List([literal.Value])),

            Expression.Environment =>
            s_environmentExpressionValue,

            Expression.List list =>
            EncodeListExpressionAsValue(list, encodeSubexpression),

            Expression.Conditional conditional =>
            EncodeConditional(conditional, encodeSubexpression),

            Expression.ParseAndEval parseAndEval =>
            EncodeParseAndEval(parseAndEval, encodeSubexpression),

            Expression.KernelApplication kernelAppl =>
            EncodeKernelApplication(kernelAppl, encodeSubexpression),

            Expression.StringTag stringTag =>
            EncodeChoiceTypeVariantAsPineValue(
                "StringTag",
                PineValue.List(
                    [
                    StringEncoding.ValueFromString(stringTag.Tag),
                    encodeSubexpression(stringTag.Tagged)
                    ])),

            _ =>
            throw new Exception(
                "Unsupported expression type: " + expression.GetType().FullName)
        };
    }

    private static readonly PineValue.ListValue s_environmentExpressionValue =
        EncodeChoiceTypeVariantAsPineValue("Environment", PineValue.EmptyList);

    private static PineValue.ListValue EncodeListExpressionAsValue(
        Expression.List list,
        Func<Expression, PineValue.ListValue> encodeSubexpression)
    {
        var encodedItems = new PineValue[list.Items.Count];

        for (var i = 0; i < list.Items.Count; ++i)
        {
            encodedItems[i] = encodeSubexpression(list.Items[i]);
        }

        return
            EncodeChoiceTypeVariantAsPineValue(
                "List",
                PineValue.List([PineValue.List(encodedItems)]));
    }

    /// <summary>
    /// Parses a <see cref="PineValue"/> as a Pine <see cref="Expression"/>.
    /// </summary>
    /// <param name="value">The value to decode as an expression.</param>
    /// <returns>
    /// An <see cref="Result{ErrT, OkT}"/> where <c>Ok</c> carries the decoded <see cref="Expression"/>,
    /// and <c>Err</c> contains a diagnostic message if decoding fails.
    /// </returns>
    /// <remarks>
    /// Inverse of <see cref="EncodeExpressionAsValue(Expression)"/>.
    /// Decoding proceeds via an explicit post-order traversal (rather than recursing through
    /// <see cref="ParseExpressionFromValue(PineValue, Func{PineValue, Result{string, Expression}})"/>)
    /// so that deeply nested encodings do not overflow the call stack.
    /// </remarks>
    public static Result<string, Expression> ParseExpressionFromValue(
        PineValue value)
    {
        return
            ParseExpressionFromValueViaPostOrder(
                value,
                new Dictionary<PineValue, Result<string, Expression>>());
    }

    /// <summary>
    /// Parses <paramref name="rootValue"/> using an explicit work stack instead of recursion,
    /// so that arbitrarily deep encodings do not overflow the call stack.
    /// <para>
    /// Subexpressions are decoded in post-order (children before parents) into <paramref name="store"/>,
    /// which also deduplicates shared subtrees so each distinct subvalue is decoded at most once.
    /// Because every direct child is already present in <paramref name="store"/> by the time its parent
    /// is decoded, the per-node call to
    /// <see cref="ParseExpressionFromValue(PineValue, Func{PineValue, Result{string, Expression}})"/>
    /// never recurses more than one level (its <c>generalParser</c> only reads from <paramref name="store"/>).
    /// </para>
    /// </summary>
    internal static Result<string, Expression> ParseExpressionFromValueViaPostOrder(
        PineValue rootValue,
        IDictionary<PineValue, Result<string, Expression>> store)
    {
        if (store.TryGetValue(rootValue, out var alreadyParsed))
            return alreadyParsed;

        Result<string, Expression> lookupParser(PineValue childValue) => store[childValue];

        var stack = new Stack<PineValue>();

        stack.Push(rootValue);

        var expanded = new HashSet<PineValue>();

        while (stack.Count is not 0)
        {
            var current = stack.Peek();

            if (store.ContainsKey(current))
            {
                stack.Pop();
                continue;
            }

            if (expanded.Add(current))
            {
                // First visit: schedule not-yet-parsed direct children to be processed first.
                PushUnparsedChildren(current, store, stack);

                continue;
            }

            // Second visit: all children are parsed and present in 'store'.
            stack.Pop();

            store[current] = ParseExpressionFromValue(current, lookupParser);
        }

        return store[rootValue];
    }

    /// <summary>
    /// Schedules the direct child expression values of <paramref name="value"/> that are not yet
    /// present in <paramref name="store"/> to be parsed before <paramref name="value"/> itself.
    /// <para>
    /// Only values whose shape is that of a nested expression are pushed; malformed shapes are left
    /// for the per-node parse step to report as errors. Values already known to decode to a reused
    /// instance are treated as leaves.
    /// </para>
    /// </summary>
    private static void PushUnparsedChildren(
        PineValue value,
        IDictionary<PineValue, Result<string, Expression>> store,
        Stack<PineValue> stack)
    {
        if (value is not PineValue.ListValue rootList)
            return;

        if (ReusedInstances.Instance.ExpressionDecodings?.ContainsKey(rootList) ?? false)
            return;

        if (rootList.Items.Length is not 2)
            return;

        if (StringEncoding.StringFromValue(rootList.Items.Span[0]).IsOkOrNull() is not { } tag)
            return;

        if (rootList.Items.Span[1] is not PineValue.ListValue tagArguments)
            return;

        var arguments = tagArguments.Items;

        switch (tag)
        {
            case "Literal":
            case "Environment":
                break;

            case "List":
                if (arguments.Length is 1 &&
                    arguments.Span[0] is PineValue.ListValue listItems)
                {
                    for (var i = 0; i < listItems.Items.Length; ++i)
                        PushChildIfUnparsed(listItems.Items.Span[i], store, stack);
                }

                break;

            case "ParseAndEval":
                if (arguments.Length is 2)
                {
                    PushChildIfUnparsed(arguments.Span[0], store, stack);
                    PushChildIfUnparsed(arguments.Span[1], store, stack);
                }

                break;

            case "KernelApplication":
                if (arguments.Length is 2)
                    PushChildIfUnparsed(arguments.Span[1], store, stack);

                break;

            case "Conditional":
                if (arguments.Length is 3)
                {
                    PushChildIfUnparsed(arguments.Span[0], store, stack);
                    PushChildIfUnparsed(arguments.Span[1], store, stack);
                    PushChildIfUnparsed(arguments.Span[2], store, stack);
                }

                break;

            case "StringTag":
                if (arguments.Length is 2)
                    PushChildIfUnparsed(arguments.Span[1], store, stack);

                break;

            default:
                break;
        }
    }

    private static void PushChildIfUnparsed(
        PineValue childValue,
        IDictionary<PineValue, Result<string, Expression>> store,
        Stack<PineValue> stack)
    {
        if (!store.ContainsKey(childValue))
            stack.Push(childValue);
    }

    /// <summary>
    /// Parses a <see cref="PineValue"/> as a Pine <see cref="Expression"/>,
    /// using the supplied <paramref name="generalParser"/> to decode nested expressions.
    /// </summary>
    /// <param name="value">The value to decode as an expression.</param>
    /// <param name="generalParser">
    /// A function used to parse any nested <see cref="PineValue"/> values into <see cref="Expression"/> instances.
    /// Callers can supply a memoizing or instrumented parser; the default overload passes this method itself.
    /// </param>
    /// <returns>
    /// An <see cref="Result{ErrT, OkT}"/> where <c>Ok</c> carries the decoded <see cref="Expression"/>,
    /// and <c>Err</c> contains a diagnostic message if decoding fails.
    /// </returns>
    public static Result<string, Expression> ParseExpressionFromValue(
        PineValue value,
        Func<PineValue, Result<string, Expression>> generalParser)
    {
        if (value is not PineValue.ListValue rootList)
            return "Unexpected value type, not a list: " + value.GetType().FullName;

        if (ReusedInstances.Instance.ExpressionDecodings?.TryGetValue(rootList, out var reusedInstance) ?? false)
            return reusedInstance;

        if (rootList.Items.Length is not 2)
            return "Unexpected number of items in list: Not 2 but " + rootList.Items.Length;

        var parseTagResult = StringEncoding.StringFromValue(rootList.Items.Span[0]);

        {
            if (parseTagResult.IsErrOrNull() is { } err)
                return "Failed to parse tag as string: " + err;
        }

        if (parseTagResult.IsOkOrNull() is not { } tag)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + parseTagResult.GetType().FullName);
        }

        if (rootList.Items.Span[1] is not PineValue.ListValue tagArguments)
        {
            return "Unexpected shape of tag argument value: Not a list";
        }

        return
            tag switch
            {
                "Literal" =>
                tagArguments.Items.Length < 1
                ?
                "Expected one argument for literal but got zero"
                :
                Expression.LiteralInstance(tagArguments.Items.Span[0]),

                "List" =>
                tagArguments.Items.Length is not 1
                ?
                "Unexpected number of arguments for list: Not 1 but " + tagArguments.Items.Length
                :
                ParseExpressionList(generalParser, tagArguments.Items.Span[0]),

                "ParseAndEval" =>
                ParseParseAndEval(generalParser, tagArguments.Items),

                "KernelApplication" =>
                ParseKernelApplication(generalParser, tagArguments.Items),

                "Conditional" =>
                ParseConditional(generalParser, tagArguments.Items),

                "Environment" =>
                Expression.EnvironmentInstance,

                "StringTag" =>
                ParseStringTag(generalParser, tagArguments.Items),

                var otherTag =>
                "Tag name does not match any known expression variant: " + otherTag
            };
    }

    private static Result<string, Expression> ParseExpressionList(
        Func<PineValue, Result<string, Expression>> generalParser,
        PineValue pineValue)
    {
        if (pineValue is not PineValue.ListValue listValue)
            return "Expected list value";

        var expressions = new Expression[listValue.Items.Length];

        for (var i = 0; i < listValue.Items.Length; ++i)
        {
            var itemValue = listValue.Items.Span[i];

            var parseItemResult = generalParser(itemValue);

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

    private static PineValue.ListValue EncodeParseAndEval(
        Expression.ParseAndEval parseAndEval,
        Func<Expression, PineValue.ListValue> encodeSubexpression) =>
        EncodeChoiceTypeVariantAsPineValue(
            "ParseAndEval",
            PineValue.List(
                [
                encodeSubexpression(parseAndEval.Encoded),
                encodeSubexpression(parseAndEval.Environment)
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
        Expression.KernelApplication kernelApplicationExpression,
        Func<Expression, PineValue.ListValue> encodeSubexpression) =>
        EncodeChoiceTypeVariantAsPineValue(
            "KernelApplication",
            PineValue.List(
                [
                StringEncoding.ValueFromString(kernelApplicationExpression.Function),
                encodeSubexpression(kernelApplicationExpression.Input)
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

        return Expression.KernelApplicationInstance(function: function, input: input);
    }

    private static PineValue.ListValue EncodeConditional(
        Expression.Conditional conditionalExpression,
        Func<Expression, PineValue.ListValue> encodeSubexpression) =>
        EncodeChoiceTypeVariantAsPineValue(
            "Conditional",
            PineValue.List(
                [
                encodeSubexpression(conditionalExpression.Condition),
                encodeSubexpression(conditionalExpression.FalseBranch),
                encodeSubexpression(conditionalExpression.TrueBranch)
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

        return
            Expression.ConditionalInstance(
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
                                    Result<string, Record>.ok(
                                        compose(fieldADecoded.Value, fieldBDecoded.Value, fieldCDecoded.Value)),

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
            [
            ..fields.Select(
                field => PineValue.List(
                    [
                    StringEncoding.ValueFromString(field.fieldName),
                    field.fieldValue
                    ]))
            ]);


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
            StringEncoding.ValueFromString(tagName),
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
            Result<string, ReadOnlyMemory<PineValue>>.ok(listValue.Items);
    }

    private static Result<string, (T, T)> ParseListWithExactlyTwoElements<T>(ReadOnlyMemory<T> list)
    {
        if (list.Length is not 2)
            return "Unexpected number of items in list: Not 2 but " + list.Length;

        return (list.Span[0], list.Span[1]);
    }
}
