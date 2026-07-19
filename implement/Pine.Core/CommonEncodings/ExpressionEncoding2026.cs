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

    private static readonly PineValue s_conditionTag = StringEncoding.ValueFromString("Condition");

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
        ParseExpressionFromValueWithoutResultAllocation(value).ToPublicResult();

    internal static ParseExpressionResult ParseExpressionFromValueWithoutResultAllocation(
        PineValue value) =>
        ParseExpressionFromValueViaPostOrder(
            value,
            new Dictionary<PineValue, ParseExpressionResult>());

    internal static ParseExpressionResult ParseExpressionFromValueViaPostOrder(
        PineValue rootValue,
        IDictionary<PineValue, ParseExpressionResult> store)
    {
        if (store.TryGetValue(rootValue, out var alreadyParsed))
            return alreadyParsed;

        var stack = new Stack<ParseWorkItem>();

        stack.Push(new ParseWorkItem(rootValue, ParseAfterChildren: false));

        while (stack.Count is not 0)
        {
            var workItem = stack.Pop();
            var current = workItem.Value;

            if (store.ContainsKey(current))
                continue;

            if (!workItem.ParseAfterChildren)
            {
                stack.Push(new ParseWorkItem(current, ParseAfterChildren: true));
                PushUnparsedChildren(current, store, stack);
                continue;
            }

            store[current] =
                ParseExpressionFromValue(
                    current,
                    new StoredChildParser(store));
        }

        return store[rootValue];
    }

    private static void PushUnparsedChildren(
        PineValue value,
        IDictionary<PineValue, ParseExpressionResult> store,
        Stack<ParseWorkItem> stack)
    {
        if (value is not PineValue.ListValue rootList || rootList.Items.Length is 0)
            return;

        switch (ClassifyTag(rootList.Items.Span[0]))
        {
            case EncodedExpressionTag.Litral:
            case EncodedExpressionTag.Environment:
                break;

            case EncodedExpressionTag.List:
                for (var i = 1; i < rootList.Items.Length; ++i)
                    PushChildIfUnparsed(rootList.Items.Span[i], store, stack);

                break;

            case EncodedExpressionTag.Builtin:
                if (rootList.Items.Length is 3)
                    PushChildIfUnparsed(rootList.Items.Span[2], store, stack);

                break;

            case EncodedExpressionTag.Conditional:
                if (rootList.Items.Length is 4)
                {
                    PushChildIfUnparsed(rootList.Items.Span[1], store, stack);
                    PushChildIfUnparsed(rootList.Items.Span[2], store, stack);
                    PushChildIfUnparsed(rootList.Items.Span[3], store, stack);
                }

                break;

            case EncodedExpressionTag.Eval:
                if (rootList.Items.Length is 3)
                {
                    PushChildIfUnparsed(rootList.Items.Span[1], store, stack);
                    PushChildIfUnparsed(rootList.Items.Span[2], store, stack);
                }

                break;

            case EncodedExpressionTag.Label:
                if (rootList.Items.Length is 3)
                    PushChildIfUnparsed(rootList.Items.Span[2], store, stack);

                break;

            case EncodedExpressionTag.Unknown:
                break;

            default:
                throw new NotImplementedException(
                    "PushUnparsedChildren does not handle encoded expression tag: " +
                    ClassifyTag(rootList.Items.Span[0]));
        }
    }

    private static void PushChildIfUnparsed(
        PineValue child,
        IDictionary<PineValue, ParseExpressionResult> store,
        Stack<ParseWorkItem> stack)
    {
        if (!store.ContainsKey(child))
            stack.Push(new ParseWorkItem(child, ParseAfterChildren: false));
    }

    private static EncodedExpressionTag ClassifyTag(PineValue value)
    {
        if (value.Equals(s_literalTag))
            return EncodedExpressionTag.Litral;

        if (value.Equals(s_listTag))
            return EncodedExpressionTag.List;

        if (value.Equals(s_builtinTag))
            return EncodedExpressionTag.Builtin;

        if (value.Equals(s_conditionalTag) || value.Equals(s_conditionTag))
            return EncodedExpressionTag.Conditional;

        if (value.Equals(s_environmentTag))
            return EncodedExpressionTag.Environment;

        if (value.Equals(s_evalTag))
            return EncodedExpressionTag.Eval;

        if (value.Equals(s_labelTag))
            return EncodedExpressionTag.Label;

        return EncodedExpressionTag.Unknown;
    }

    private static ParseExpressionResult ParseExpressionFromValue<ChildParserT>(
        PineValue value,
        ChildParserT childParser)
        where ChildParserT : struct, IChildParser
    {
        if (value is not PineValue.ListValue rootList)
        {
            return
                ParseExpressionResult.Err(
                    "Unexpected value type, not a list: " + value.GetType().FullName);
        }

        if (rootList.Items.Length is 0)
            return ParseExpressionResult.Err("Unexpected empty list");

        return ClassifyTag(rootList.Items.Span[0]) switch
        {
            EncodedExpressionTag.Litral =>
            rootList.Items.Length is 2
            ?
            ParseExpressionResult.Ok(Expression.LitralInst(rootList.Items.Span[1]))
            :
            ParseExpressionResult.Err(UnexpectedItemCount("literal", 2, rootList.Items.Length)),

            EncodedExpressionTag.List =>
            ParseList(rootList.Items, childParser),

            EncodedExpressionTag.Builtin =>
            ParseBuiltin(rootList.Items, childParser),

            EncodedExpressionTag.Conditional =>
            ParseCondition(rootList.Items, childParser),

            EncodedExpressionTag.Environment =>
            rootList.Items.Length is 1
            ?
            ParseExpressionResult.Ok(Expression.EnvironmentInstance)
            :
            ParseExpressionResult.Err(UnexpectedItemCount("environment", 1, rootList.Items.Length)),

            EncodedExpressionTag.Eval =>
            ParseEval(rootList.Items, childParser),

            EncodedExpressionTag.Label =>
            ParseLabel(rootList.Items, childParser),

            EncodedExpressionTag.Unknown =>
            ParseUnknownTag(rootList.Items.Span[0]),

            _ =>
            throw new NotImplementedException(
                "ParseExpressionFromValue does not handle encoded expression tag: " +
                ClassifyTag(rootList.Items.Span[0]))
        };
    }

    private static ParseExpressionResult ParseUnknownTag(PineValue tagValue)
    {
        var parseTagResult =
            StringEncoding.StringFromValueWithoutResultAllocation(tagValue);

        if (!parseTagResult.IsOk)
        {
            return
                ParseExpressionResult.Err(
                    "Failed to parse tag as string: " + parseTagResult.Error);
        }

        return
            ParseExpressionResult.Err(
                "Tag name does not match any known 2026 expression variant: " +
                parseTagResult.Value);
    }

    private static ParseExpressionResult ParseList<ChildParserT>(
        ReadOnlyMemory<PineValue> items,
        ChildParserT childParser)
        where ChildParserT : struct, IChildParser
    {
        var expressions = new Expression[items.Length - 1];

        for (var i = 1; i < items.Length; ++i)
        {
            var parsed = childParser.Parse(items.Span[i]);

            if (!parsed.IsOk)
            {
                return
                    ParseExpressionResult.Err(
                        "Failed to parse list item at index " + (i - 1) + ": " + parsed.Error);
            }

            expressions[i - 1] = parsed.Expression!;
        }

        return ParseExpressionResult.Ok(Expression.ListInst(expressions));
    }

    private static ParseExpressionResult ParseBuiltin<ChildParserT>(
        ReadOnlyMemory<PineValue> items,
        ChildParserT childParser)
        where ChildParserT : struct, IChildParser
    {
        if (items.Length is not 3)
        {
            return
                ParseExpressionResult.Err(
                    UnexpectedItemCount("builtin", 3, items.Length));
        }

        var functionResult =
            StringEncoding.StringFromValueWithoutResultAllocation(items.Span[1]);

        if (!functionResult.IsOk)
        {
            return
                ParseExpressionResult.Err(
                    "Failed to parse builtin function name: " + functionResult.Error);
        }

        var inputResult = childParser.Parse(items.Span[2]);

        if (!inputResult.IsOk)
            return inputResult;

        return
            ParseExpressionResult.Ok(
                Expression.BuiltinInst(functionResult.Value!, inputResult.Expression!));
    }

    private static ParseExpressionResult ParseCondition<ChildParserT>(
        ReadOnlyMemory<PineValue> items,
        ChildParserT childParser)
        where ChildParserT : struct, IChildParser
    {
        if (items.Length is not 4)
        {
            return
                ParseExpressionResult.Err(
                    UnexpectedItemCount("conditional", 4, items.Length));
        }

        var conditionResult = childParser.Parse(items.Span[1]);

        if (!conditionResult.IsOk)
            return conditionResult;

        var falseResult = childParser.Parse(items.Span[2]);

        if (!falseResult.IsOk)
            return falseResult;

        var trueResult = childParser.Parse(items.Span[3]);

        if (!trueResult.IsOk)
            return trueResult;

        return
            ParseExpressionResult.Ok(
                Expression.ConditionalInst(
                    conditionResult.Expression!,
                    falseResult.Expression!,
                    trueResult.Expression!));
    }

    private static ParseExpressionResult ParseEval<ChildParserT>(
        ReadOnlyMemory<PineValue> items,
        ChildParserT childParser)
        where ChildParserT : struct, IChildParser
    {
        if (items.Length is not 3)
            return ParseExpressionResult.Err(UnexpectedItemCount("eval", 3, items.Length));

        var encodedResult = childParser.Parse(items.Span[1]);

        if (!encodedResult.IsOk)
            return encodedResult;

        var environmentResult = childParser.Parse(items.Span[2]);

        if (!environmentResult.IsOk)
            return environmentResult;

        return
            ParseExpressionResult.Ok(
                new Expression.Eval(encodedResult.Expression!, environmentResult.Expression!));
    }

    private static ParseExpressionResult ParseLabel<ChildParserT>(
        ReadOnlyMemory<PineValue> items,
        ChildParserT childParser)
        where ChildParserT : struct, IChildParser
    {
        if (items.Length is not 3)
            return ParseExpressionResult.Err(UnexpectedItemCount("label", 3, items.Length));

        var labeledResult = childParser.Parse(items.Span[2]);

        if (!labeledResult.IsOk)
            return labeledResult;

        return
            ParseExpressionResult.Ok(
                new Expression.Label(items.Span[1], labeledResult.Expression!));
    }

    private readonly record struct ParseWorkItem(
        PineValue Value,
        bool ParseAfterChildren);

    private interface IChildParser
    {
        ParseExpressionResult Parse(PineValue value);
    }

    private readonly struct StoredChildParser(
        IDictionary<PineValue, ParseExpressionResult> store)
        : IChildParser
    {
        public ParseExpressionResult Parse(PineValue value) => store[value];
    }

    private readonly struct PublicResultChildParser(
        Func<PineValue, Result<string, Expression>> parser)
        : IChildParser
    {
        public ParseExpressionResult Parse(PineValue value) =>
            ParseExpressionResult.FromPublicResult(parser(value));
    }

    private enum EncodedExpressionTag : byte
    {
        Unknown,
        Litral,
        List,
        Builtin,
        Conditional,
        Environment,
        Eval,
        Label
    }

    internal readonly struct ParseExpressionResult
    {
        private ParseExpressionResult(Expression? expression, string? error)
        {
            Expression = expression;
            Error = error;
        }

        public Expression? Expression { get; }

        public string? Error { get; }

        public bool IsOk => Expression is not null;

        public static ParseExpressionResult Ok(Expression expression) =>
            new(expression, error: null);

        public static ParseExpressionResult Err(string error) =>
            new(expression: null, error);

        public static ParseExpressionResult FromPublicResult(
            Result<string, Expression> result) =>
            result switch
            {
                Result<string, Expression>.Ok ok => Ok(ok.Value),
                Result<string, Expression>.Err err => Err(err.Value),

                _ =>
                throw new NotImplementedException(
                    "FromPublicResult does not handle result variant: " + result.GetType().Name)
            };

        public Result<string, Expression> ToPublicResult() =>
            Expression is { } expression
            ?
            Result<string, Expression>.ok(expression)
            :
            Result<string, Expression>.err(
                Error ??
                throw new InvalidOperationException("Parse result contains neither an expression nor an error"));
    }

    /// <summary>
    /// Parses one expression node, delegating direct subexpressions to
    /// <paramref name="generalParser"/>.
    /// </summary>
    public static Result<string, Expression> ParseExpressionFromValue(
        PineValue value,
        Func<PineValue, Result<string, Expression>> generalParser) =>
        ParseExpressionFromValue(
            value,
            new PublicResultChildParser(generalParser))
        .ToPublicResult();

    private static string UnexpectedItemCount(string variant, int expected, int actual) =>
        "Unexpected number of items for " + variant + ": Expected " + expected + " but got " + actual;
}
