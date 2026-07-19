using System;
using System.Collections.Concurrent;
using System.Collections.Generic;

namespace Pine.Core.CommonEncodings;

/// <summary>
/// The compact 2026 encoding of Pine expressions as Pine values.
/// </summary>
public static class ExpressionEncoding2026
{
    private static readonly PineValue s_listTag = StringEncoding.ValueFromString("List");

    private static readonly PineValue s_literalTag = StringEncoding.ValueFromString("Litral");

    private static readonly PineValue s_builtinTag = StringEncoding.ValueFromString("Builtin");

    private static readonly PineValue s_conditionalTag = StringEncoding.ValueFromString("Conditional");

    private static readonly PineValue s_environmentTag = StringEncoding.ValueFromString("Environment");

    private static readonly PineValue s_evalTag = StringEncoding.ValueFromString("Eval");

    private static readonly PineValue s_labelTag = StringEncoding.ValueFromString("Label");

    private static readonly PineValue.ListValue s_environmentExpressionValue =
        PineValue.List([s_environmentTag]);

    /// <summary>
    /// Encodes an expression in the compact 2026 format.
    /// </summary>
    public static PineValue.ListValue EncodeExpressionAsValue(Expression expression) =>
        EncodeExpressionAsValueViaPostOrder(
            expression,
            new ConcurrentDictionary<Expression, PineValue.ListValue>());

    internal static PineValue.ListValue EncodeExpressionAsValueViaPostOrder(
        Expression rootExpression,
        ConcurrentDictionary<Expression, PineValue.ListValue> store)
    {
        if (store.TryGetValue(rootExpression, out var alreadyEncoded))
            return alreadyEncoded;

        var stack = new Stack<Expression>();
        var expanded = new HashSet<Expression>();

        stack.Push(rootExpression);

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
                PushUnencodedChildren(current, store, stack);
                continue;
            }

            stack.Pop();

            store.GetOrAdd(
                current,
                expression =>
                EncodeExpressionAsValueWithoutTopLevelCacheLookup(
                    expression,
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
            case Expression.Litral:
            case Expression.Environment:
                break;

            case Expression.List list:
                for (var i = 0; i < list.Items.Count; ++i)
                    PushChildIfUnencoded(list.Items[i], store, stack);

                break;

            case Expression.Builtin kernelApplication:
                PushChildIfUnencoded(kernelApplication.Input, store, stack);
                break;

            case Expression.Conditional conditional:
                PushChildIfUnencoded(conditional.Condition, store, stack);
                PushChildIfUnencoded(conditional.FalseBranch, store, stack);
                PushChildIfUnencoded(conditional.TrueBranch, store, stack);
                break;

            case Expression.Eval parseAndEval:
                PushChildIfUnencoded(parseAndEval.Encoded, store, stack);
                PushChildIfUnencoded(parseAndEval.Environment, store, stack);
                break;

            case Expression.Label stringTag:
                PushChildIfUnencoded(stringTag.Tagged, store, stack);
                break;

            default:
                throw new NotImplementedException(
                    "PushUnencodedChildren does not handle expression variant: " +
                    expression.GetType().Name);
        }
    }

    private static void PushChildIfUnencoded(
        Expression child,
        ConcurrentDictionary<Expression, PineValue.ListValue> store,
        Stack<Expression> stack)
    {
        if (!store.ContainsKey(child))
            stack.Push(child);
    }

    /// <summary>
    /// Encodes one expression node, delegating direct subexpressions to
    /// <paramref name="encodeSubexpression"/>.
    /// </summary>
    public static PineValue.ListValue EncodeExpressionAsValueWithoutTopLevelCacheLookup(
        Expression expression,
        Func<Expression, PineValue.ListValue> encodeSubexpression) =>
        expression switch
        {
            Expression.Litral literal =>
            PineValue.List([s_literalTag, literal.Value]),

            Expression.List list =>
            EncodeList(list, encodeSubexpression),

            Expression.Builtin kernelApplication =>
            PineValue.List(
                [
                s_builtinTag,
                StringEncoding.ValueFromString(kernelApplication.Function),
                encodeSubexpression(kernelApplication.Input)
                ]),

            Expression.Conditional conditional =>
            PineValue.List(
                [
                s_conditionalTag,
                encodeSubexpression(conditional.Condition),
                encodeSubexpression(conditional.FalseBranch),
                encodeSubexpression(conditional.TrueBranch)
                ]),

            Expression.Environment =>
            s_environmentExpressionValue,

            Expression.Eval parseAndEval =>
            PineValue.List(
                [
                s_evalTag,
                encodeSubexpression(parseAndEval.Encoded),
                encodeSubexpression(parseAndEval.Environment)
                ]),

            Expression.Label stringTag =>
            PineValue.List(
                [
                s_labelTag,
                stringTag.LabelValue,
                encodeSubexpression(stringTag.Tagged)
                ]),

            _ =>
            throw new NotImplementedException(
                "EncodeExpressionAsValueWithoutTopLevelCacheLookup does not handle expression variant: " +
                expression.GetType().Name)
        };

    private static PineValue.ListValue EncodeList(
        Expression.List list,
        Func<Expression, PineValue.ListValue> encodeSubexpression)
    {
        var encoded = new PineValue[list.Items.Count + 1];

        encoded[0] = s_listTag;

        for (var i = 0; i < list.Items.Count; ++i)
            encoded[i + 1] = encodeSubexpression(list.Items[i]);

        return PineValue.List(encoded);
    }

    /// <summary>
    /// Parses an expression encoded in the compact 2026 format.
    /// </summary>
    public static Result<string, Expression> ParseExpressionFromValue(PineValue value) =>
        ParseExpressionFromValueViaPostOrder(
            value,
            new Dictionary<PineValue, Result<string, Expression>>());

    internal static Result<string, Expression> ParseExpressionFromValueViaPostOrder(
        PineValue rootValue,
        IDictionary<PineValue, Result<string, Expression>> store)
    {
        if (store.TryGetValue(rootValue, out var alreadyParsed))
            return alreadyParsed;

        Result<string, Expression> LookupParser(PineValue childValue) => store[childValue];

        var stack = new Stack<PineValue>();
        var expanded = new HashSet<PineValue>();

        stack.Push(rootValue);

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
                PushUnparsedChildren(current, store, stack);
                continue;
            }

            stack.Pop();
            store[current] = ParseExpressionFromValue(current, LookupParser);
        }

        return store[rootValue];
    }

    private static void PushUnparsedChildren(
        PineValue value,
        IDictionary<PineValue, Result<string, Expression>> store,
        Stack<PineValue> stack)
    {
        if (value is not PineValue.ListValue rootList || rootList.Items.Length is 0)
            return;

        if (StringEncoding.StringFromValue(rootList.Items.Span[0]).IsOkOrNull() is not { } tag)
            return;

        switch (tag)
        {
            case "Litral":
            case "Environment":
                break;

            case "List":
                for (var i = 1; i < rootList.Items.Length; ++i)
                    PushChildIfUnparsed(rootList.Items.Span[i], store, stack);

                break;

            case "Builtin":
                if (rootList.Items.Length is 3)
                    PushChildIfUnparsed(rootList.Items.Span[2], store, stack);

                break;

            case "Conditional" or "Condition":
                if (rootList.Items.Length is 4)
                {
                    PushChildIfUnparsed(rootList.Items.Span[1], store, stack);
                    PushChildIfUnparsed(rootList.Items.Span[2], store, stack);
                    PushChildIfUnparsed(rootList.Items.Span[3], store, stack);
                }

                break;

            case "Eval":
                if (rootList.Items.Length is 3)
                {
                    PushChildIfUnparsed(rootList.Items.Span[1], store, stack);
                    PushChildIfUnparsed(rootList.Items.Span[2], store, stack);
                }

                break;

            case "Label":
                if (rootList.Items.Length is 3)
                    PushChildIfUnparsed(rootList.Items.Span[2], store, stack);

                break;

            default:
                break;
        }
    }

    private static void PushChildIfUnparsed(
        PineValue child,
        IDictionary<PineValue, Result<string, Expression>> store,
        Stack<PineValue> stack)
    {
        if (!store.ContainsKey(child))
            stack.Push(child);
    }

    /// <summary>
    /// Parses one expression node, delegating direct subexpressions to
    /// <paramref name="generalParser"/>.
    /// </summary>
    public static Result<string, Expression> ParseExpressionFromValue(
        PineValue value,
        Func<PineValue, Result<string, Expression>> generalParser)
    {
        if (value is not PineValue.ListValue rootList)
            return "Unexpected value type, not a list: " + value.GetType().FullName;

        if (rootList.Items.Length is 0)
            return "Unexpected empty list";

        var parseTagResult = StringEncoding.StringFromValue(rootList.Items.Span[0]);

        if (parseTagResult.IsErrOrNull() is { } tagError)
            return "Failed to parse tag as string: " + tagError;

        if (parseTagResult.IsOkOrNull() is not { } tag)
        {
            throw new NotImplementedException(
                "ParseExpressionFromValue received an unexpected tag result variant: " +
                parseTagResult.GetType().Name);
        }

        return tag switch
        {
            "Litral" =>
            rootList.Items.Length is 2
            ?
            Expression.LitralInst(rootList.Items.Span[1])
            :
            UnexpectedItemCount("literal", 2, rootList.Items.Length),

            "List" =>
            ParseList(rootList.Items, generalParser),

            "Builtin" =>
            ParseBuiltin(rootList.Items, generalParser),

            "Conditional" or "Condition" =>
            ParseCondition(rootList.Items, generalParser),

            "Environment" =>
            rootList.Items.Length is 1
            ?
            Expression.EnvironmentInstance
            :
            UnexpectedItemCount("environment", 1, rootList.Items.Length),

            "Eval" =>
            ParseEval(rootList.Items, generalParser),

            "Label" =>
            ParseLabel(rootList.Items, generalParser),

            _ =>
            "Tag name does not match any known 2026 expression variant: " + tag
        };
    }

    private static Result<string, Expression> ParseList(
        ReadOnlyMemory<PineValue> items,
        Func<PineValue, Result<string, Expression>> generalParser)
    {
        var expressions = new Expression[items.Length - 1];

        for (var i = 1; i < items.Length; ++i)
        {
            var parsed = generalParser(items.Span[i]);

            if (parsed.IsErrOrNull() is { } error)
                return "Failed to parse list item at index " + (i - 1) + ": " + error;

            if (parsed.IsOkOrNull() is not { } expression)
            {
                throw new NotImplementedException(
                    "ParseList received an unexpected result variant: " + parsed.GetType().Name);
            }

            expressions[i - 1] = expression;
        }

        return Expression.ListInst(expressions);
    }

    private static Result<string, Expression> ParseBuiltin(
        ReadOnlyMemory<PineValue> items,
        Func<PineValue, Result<string, Expression>> generalParser)
    {
        if (items.Length is not 3)
            return UnexpectedItemCount("builtin", 3, items.Length);

        var functionResult = StringEncoding.StringFromValue(items.Span[1]);

        if (functionResult.IsErrOrNull() is { } functionError)
            return "Failed to parse builtin function name: " + functionError;

        if (functionResult.IsOkOrNull() is not { } function)
        {
            throw new NotImplementedException(
                "ParseBuiltin received an unexpected function result variant: " +
                functionResult.GetType().Name);
        }

        var inputResult = generalParser(items.Span[2]);

        if (inputResult.IsErrOrNull() is { } inputError)
            return inputError;

        if (inputResult.IsOkOrNull() is not { } input)
        {
            throw new NotImplementedException(
                "ParseBuiltin received an unexpected input result variant: " +
                inputResult.GetType().Name);
        }

        return Expression.BuiltinInst(function, input);
    }

    private static Result<string, Expression> ParseCondition(
        ReadOnlyMemory<PineValue> items,
        Func<PineValue, Result<string, Expression>> generalParser)
    {
        if (items.Length is not 4)
            return UnexpectedItemCount("conditional", 4, items.Length);

        var conditionResult = generalParser(items.Span[1]);

        if (conditionResult.IsErrOrNull() is { } conditionError)
            return conditionError;

        var falseResult = generalParser(items.Span[2]);

        if (falseResult.IsErrOrNull() is { } falseError)
            return falseError;

        var trueResult = generalParser(items.Span[3]);

        if (trueResult.IsErrOrNull() is { } trueError)
            return trueError;

        if (conditionResult.IsOkOrNull() is not { } condition ||
            falseResult.IsOkOrNull() is not { } falseBranch ||
            trueResult.IsOkOrNull() is not { } trueBranch)
        {
            throw new NotImplementedException(
                "ParseCondition received an unexpected result variant");
        }

        return Expression.ConditionalInst(condition, falseBranch, trueBranch);
    }

    private static Result<string, Expression> ParseEval(
        ReadOnlyMemory<PineValue> items,
        Func<PineValue, Result<string, Expression>> generalParser)
    {
        if (items.Length is not 3)
            return UnexpectedItemCount("eval", 3, items.Length);

        var encodedResult = generalParser(items.Span[1]);

        if (encodedResult.IsErrOrNull() is { } encodedError)
            return encodedError;

        var environmentResult = generalParser(items.Span[2]);

        if (environmentResult.IsErrOrNull() is { } environmentError)
            return environmentError;

        if (encodedResult.IsOkOrNull() is not { } encoded ||
            environmentResult.IsOkOrNull() is not { } environment)
        {
            throw new NotImplementedException(
                "ParseEval received an unexpected result variant");
        }

        return new Expression.Eval(encoded, environment);
    }

    private static Result<string, Expression> ParseLabel(
        ReadOnlyMemory<PineValue> items,
        Func<PineValue, Result<string, Expression>> generalParser)
    {
        if (items.Length is not 3)
            return UnexpectedItemCount("label", 3, items.Length);

        var labeledResult = generalParser(items.Span[2]);

        if (labeledResult.IsErrOrNull() is { } labeledError)
            return labeledError;

        if (labeledResult.IsOkOrNull() is not { } labeled)
        {
            throw new NotImplementedException(
                "ParseLabel received an unexpected expression result variant: " +
                labeledResult.GetType().Name);
        }

        return new Expression.Label(items.Span[1], labeled);
    }

    private static string UnexpectedItemCount(string variant, int expected, int actual) =>
        "Unexpected number of items for " + variant + ": Expected " + expected + " but got " + actual;
}
