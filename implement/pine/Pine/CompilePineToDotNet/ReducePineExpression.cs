using Pine.Core;
using Pine.Core.PopularEncodings;
using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.CompilePineToDotNet;

public class ReducePineExpression
{
    static private readonly CompilerMutableCache compilerCache = new();

    static private readonly PineVMCache parseCache = new();

    public static Result<string, PineValue> TryEvaluateExpressionIndependent(Expression expression) =>
        expression switch
        {
            Expression.Environment =>
            "Expression depends on environment",

            Expression.Literal literal =>
            Result<string, PineValue>.ok(literal.Value),

            Expression.List list =>
            TryEvaluateExpressionIndependent(list),

            Expression.KernelApplication kernelApplication =>
            TryEvaluateExpressionIndependent(kernelApplication),

            Expression.ParseAndEval parseAndEvalExpr =>
            TryEvaluateExpressionIndependent(parseAndEvalExpr),

            Expression.Conditional conditional =>
            TryEvaluateExpressionIndependent(conditional),

            Expression.StringTag stringTag =>
            TryEvaluateExpressionIndependent(stringTag.Tagged),

            _ =>
            "Unsupported expression type: " + expression.GetType().FullName
        };

    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.List listExpr)
    {
        var itemsValues = new PineValue[listExpr.items.Count];

        for (var i = 0; i < listExpr.items.Count; i++)
        {
            var itemResult = TryEvaluateExpressionIndependent(listExpr.items[i]);

            if (itemResult.IsOkOrNull() is { } itemValue)
            {
                itemsValues[i] = itemValue;
            }
            else
            {
                return itemResult;
            }
        }

        return PineValue.List(itemsValues);
    }

    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.ParseAndEval parseAndEvalExpr)
    {
        if (false)
        {
            if (TryEvaluateExpressionIndependent(parseAndEvalExpr.Environment).IsOkOrNull() is { } envOk)
            {
                try
                {
                    /*
                     * 2024-10-25: Disabled this approach after observing Stack overflow here.
                     * 
                    return
                        new PineVM.PineVM()
                        .EvaluateExpressionDefaultLessStack(
                            parseAndEvalExpr,
                            PineValue.EmptyList,
                            stackPrevValues: ReadOnlyMemory<PineValue>.Empty);
                    */
                }
                catch (ParseExpressionException)
                {
                    /*
                    * A branch of a conditional expression might always fail to parse.
                    * This does not mean that the code would crash at runtime, since it is conditional.
                    * (The Elm compiler uses constructs like this to encode crashing branches with a custom error message.)
                    * */
                }
            }
        }

        var evalEncodedExprResult =
            TryEvaluateExpressionIndependent(parseAndEvalExpr.Encoded);

        if (evalEncodedExprResult.IsErrOrNull() is { } encodedErr)
        {
            return
                "Failed to evaluate encoded expression: " + encodedErr;
        }

        if (evalEncodedExprResult.IsOkOrNull() is not { } encodedOk)
        {
            throw new NotImplementedException(
                "Unexpected result type from evaluating encoded expression: " + evalEncodedExprResult);
        }

        var parseResult =
            compilerCache.ParseExpressionFromValue(encodedOk);

        if (parseResult.IsErrOrNull() is { } parseErr)
        {
            return
                "Failed to parse encoded expression: " + parseErr;
        }

        if (parseResult.IsOkOrNull() is not { } parseOk)
        {
            throw new NotImplementedException(
                "Unexpected result type from parsing encoded expression: " + parseResult);
        }

        var evalInnerExprResult =
            TryEvaluateExpressionIndependent(parseOk);

        if (evalInnerExprResult.IsErrOrNull() is { } innerErr)
        {
            return
                "Failed to evaluate inner expression: " + innerErr;
        }

        if (evalInnerExprResult.IsOkOrNull() is not { } innerOk)
        {
            throw new NotImplementedException(
                "Unexpected result type from evaluating inner expression: " + evalInnerExprResult);
        }

        return innerOk;
    }

    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.Conditional conditionalExpr)
    {
        var evalConditionResult =
            TryEvaluateExpressionIndependent(conditionalExpr.Condition);

        if (evalConditionResult.IsErrOrNull() is { } conditionErr)
        {
            return
                "Failed to evaluate condition: " + conditionErr;
        }

        if (evalConditionResult.IsOkOrNull() is not { } conditionOk)
        {
            throw new NotImplementedException(
                "Unexpected result type from evaluating condition: " + evalConditionResult);
        }

        if (conditionOk == PineVMValues.TrueValue)
        {
            return TryEvaluateExpressionIndependent(conditionalExpr.TrueBranch);
        }

        return TryEvaluateExpressionIndependent(conditionalExpr.FalseBranch);
    }

    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.KernelApplication kernelApplication)
    {
        var evalInputResult =
            TryEvaluateExpressionIndependent(kernelApplication.Input);

        if (evalInputResult.IsErrOrNull() is { } inputErr)
        {
            return
                "Failed to evaluate kernel application input: " + inputErr;
        }

        if (evalInputResult.IsOkOrNull() is not { } inputOk)
        {
            throw new NotImplementedException(
                "Unexpected result type from evaluating kernel application input: " + evalInputResult);
        }

        return PineVM.PineVM.EvaluateKernelApplicationGeneric(kernelApplication.Function, inputOk);
    }

    public static Expression? SearchForExpressionReduction(
        Expression expression,
        EnvConstraintId? envConstraintId)
    {
        if (expression is Expression.Literal)
            return null;

        if (CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(expression) is { } parsedAsPath)
        {
            if (parsedAsPath is ExprMappedToParentEnv.LiteralInParentEnv asLiteral)
            {
                return Expression.LiteralInstance(asLiteral.Value);
            }

            if (parsedAsPath is ExprMappedToParentEnv.PathInParentEnv asPath && envConstraintId is not null)
            {
                if (envConstraintId.TryGetValue(asPath.Path) is { } fromEnvConstraint)
                {
                    return Expression.LiteralInstance(fromEnvConstraint);
                }
            }
        }

        Expression? AttemptReduceViaEval()
        {
            if (expression.ReferencesEnvironment)
            {
                return null;
            }

            try
            {
                if (TryEvaluateExpressionIndependent(expression).IsOkOrNull() is { } okValue)
                {
                    return Expression.LiteralInstance(okValue);
                }
            }
            catch (ParseExpressionException)
            {
                /*
                 * A branch of a conditional expression might always fail to parse.
                 * This does not mean that the code would crash at runtime, since it is conditional.
                 * (The Elm compiler uses constructs like this to encode crashing branches with a custom error message.)
                 * */
            }

            return null;
        }

        switch (expression)
        {
            case Expression.KernelApplication rootKernelApp:

                Expression.KernelApplication continueWithReducedInput(Expression newInput) =>
                    new(
                        function: rootKernelApp.Function,
                        input: newInput);

                switch (rootKernelApp.Function)
                {
                    case nameof(KernelFunction.equal):
                        {
                            if (rootKernelApp.Input is Expression.List inputList)
                            {
                                if (envConstraintId is not null)
                                {
                                    var reducedArgumentsList =
                                        inputList.items
                                        .Select(origArg => SearchForExpressionReductionRecursive(
                                            maxDepth: 5,
                                            expression: origArg,
                                            envConstraintId: envConstraintId))
                                        .ToImmutableArray();

                                    var listLengthLowerBounds = new List<int>();

                                    var listConcreteValues = new List<PineValue>();

                                    foreach (var item in reducedArgumentsList)
                                    {
                                        listLengthLowerBounds.AddRange(
                                            TryInferListLengthLowerBounds(item, envConstraintId));

                                        if (item is Expression.Literal literal)
                                        {
                                            listConcreteValues.Add(literal.Value);
                                        }
                                    }

                                    if (1 < listConcreteValues.Count)
                                    {
                                        for (var i = 1; i < listConcreteValues.Count; i++)
                                        {
                                            if (listConcreteValues[i] != listConcreteValues[0])
                                            {
                                                return Expression.LiteralInstance(PineVMValues.FalseValue);
                                            }
                                        }
                                    }

                                    var listLengthLowerBound =
                                        listLengthLowerBounds.Count is 0 ?
                                        (int?)null
                                        :
                                        listLengthLowerBounds.Max();

                                    int? prevItemFixedLength = null;

                                    foreach (var item in inputList.items)
                                    {
                                        int? itemFixedLength = null;

                                        if (item is Expression.Literal equalArgLiteral)
                                        {
                                            if (equalArgLiteral.Value is PineValue.ListValue equalArgLiteralList)
                                            {
                                                itemFixedLength = equalArgLiteralList.Elements.Length;
                                            }
                                        }

                                        if (item is Expression.List equalArgList)
                                        {
                                            itemFixedLength = equalArgList.items.Count;
                                        }

                                        if (itemFixedLength.HasValue)
                                        {
                                            if (itemFixedLength < listLengthLowerBound ||
                                                (prevItemFixedLength.HasValue && itemFixedLength.Value != prevItemFixedLength.Value))
                                            {
                                                return Expression.LiteralInstance(PineVMValues.FalseValue);
                                            }

                                            prevItemFixedLength = itemFixedLength;
                                        }
                                    }
                                }
                            }

                            return AttemptReduceViaEval();
                        }

                    case nameof(KernelFunction.head):
                        {
                            if (rootKernelApp.Input is Expression.List inputList)
                            {
                                if (inputList.items.Count is 0)
                                {
                                    return Expression.LiteralInstance(PineValue.EmptyList);
                                }

                                return inputList.items[0];
                            }

                            if (rootKernelApp.Input is Expression.Literal literal)
                            {
                                return Expression.LiteralInstance(KernelFunction.head(literal.Value));
                            }

                            if (rootKernelApp.Input is Expression.KernelApplication headInputKernelApp)
                            {
                                if (headInputKernelApp.Function is nameof(KernelFunction.skip))
                                {
                                    if (headInputKernelApp.Input is Expression.List headSkipInputList &&
                                        headSkipInputList.items.Count is 2)
                                    {
                                        if (TryEvaluateExpressionIndependent(headSkipInputList.items[0]).IsOkOrNull() is { } okSkipCountValue)
                                        {
                                            if (KernelFunction.SignedIntegerFromValueRelaxed(okSkipCountValue) is { } okSkipCount)
                                            {
                                                var skipCountClamped =
                                                    (int)(okSkipCount < 0 ? 0 : okSkipCount);

                                                if (headSkipInputList.items[1] is Expression.List headSkipList)
                                                {
                                                    if (skipCountClamped < headSkipList.items.Count)
                                                    {
                                                        return headSkipList.items[skipCountClamped];
                                                    }

                                                    return Expression.LiteralInstance(PineValue.EmptyList);
                                                }

                                                if (headSkipInputList.items[1] is Expression.Literal headSkipLiteral)
                                                {
                                                    return Expression.LiteralInstance(
                                                        KernelFunction.head(
                                                            KernelFunction.skip(skipCountClamped, headSkipLiteral.Value)));
                                                }
                                            }
                                        }
                                    }
                                }
                            }

                            return AttemptReduceViaEval();
                        }

                    case nameof(KernelFunction.skip):
                        {
                            if (rootKernelApp.Input is Expression.List inputList && inputList.items.Count is 2)
                            {
                                if (TryEvaluateExpressionIndependent(inputList.items[0]).IsOkOrNull() is { } okSkipCountValue)
                                {
                                    if (KernelFunction.SignedIntegerFromValueRelaxed(okSkipCountValue) is { } okSkipCount)
                                    {
                                        if (inputList.items[1] is Expression.List partiallySkippedList)
                                        {
                                            return Expression.ListInstance(
                                                [.. partiallySkippedList.items.Skip((int)okSkipCount)]);
                                        }

                                        if (inputList.items[1] is Expression.Literal literal)
                                        {
                                            return Expression.LiteralInstance(
                                                KernelFunction.skip((int)okSkipCount, literal.Value));
                                        }

                                        if (inputList.items[1] is Expression.KernelApplication innerKernelApp)
                                        {
                                            if (innerKernelApp.Function is nameof(KernelFunction.skip)
                                                && innerKernelApp.Input is Expression.List innerSkipInputList &&
                                                innerSkipInputList.items.Count is 2)
                                            {
                                                if (TryEvaluateExpressionIndependent(innerSkipInputList.items[0]).IsOkOrNull() is { } okInnerSkipCountValue)
                                                {
                                                    if (KernelFunction.SignedIntegerFromValueRelaxed(okInnerSkipCountValue) is { } okInnerSkipCount)
                                                    {
                                                        var outerSkipCountClamped =
                                                            okSkipCount < 0 ? 0 : okSkipCount;

                                                        var innerSkipCountClamped =
                                                            okInnerSkipCount < 0 ? 0 : okInnerSkipCount;

                                                        var aggregateSkipCount = outerSkipCountClamped + innerSkipCountClamped;

                                                        return
                                                            continueWithReducedInput(
                                                                Expression.ListInstance(
                                                                    [
                                                                    Expression.LiteralInstance(
                                                                        IntegerEncoding.EncodeSignedInteger(aggregateSkipCount)),
                                                                    innerSkipInputList.items[1]
                                                                    ]
                                                                ));
                                                    }
                                                }
                                            }
                                        }
                                    }
                                }

                                return AttemptReduceViaEval();
                            }

                            return AttemptReduceViaEval();
                        }

                    case nameof(KernelFunction.concat):
                        {
                            if (rootKernelApp.Input is Expression.List inputList)
                            {
                                if (inputList.items.Count is 0)
                                {
                                    return AttemptReduceViaEval();
                                }

                                if (inputList.items.Count is 1)
                                {
                                    return inputList.items[0];
                                }

                                var firstArgExpr = inputList.items[0];

                                /*
                                if (firstArgExpr is Expression.ListExpression ||
                                    firstArgExpr is Expression.LiteralExpression firstLiteral && firstLiteral.Value is PineValue.ListValue)
                                */
                                {
                                    var nonEmptyItems = new List<Expression>(capacity: inputList.items.Count);

                                    for (var i = 0; i < inputList.items.Count; ++i)
                                    {
                                        var argItem = inputList.items[i];

                                        if (argItem is Expression.List argList && argList.items.Count is 0)
                                            continue;

                                        if (argItem is Expression.Literal argLiteral)
                                        {
                                            if (argLiteral.Value is PineValue.ListValue listValue && listValue.Elements.Length is 0)
                                                continue;
                                        }

                                        nonEmptyItems.Add(argItem);
                                    }

                                    if (nonEmptyItems.Count < inputList.items.Count)
                                    {
                                        if (nonEmptyItems.Count is 0)
                                        {
                                            return Expression.LiteralInstance(PineValue.EmptyList);
                                        }

                                        if (nonEmptyItems.Count is 1)
                                        {
                                            return nonEmptyItems[0];
                                        }

                                        return
                                            continueWithReducedInput(Expression.ListInstance(nonEmptyItems));
                                    }
                                }

                                var items = new List<Expression>();

                                foreach (var argument in inputList.items)
                                {
                                    if (argument is not Expression.List subList)
                                    {
                                        if (argument is Expression.Literal subLiteral &&
                                            subLiteral.Value is PineValue.ListValue subLiteralList)
                                        {
                                            for (var i = 0; i < subLiteralList.Elements.Length; i++)
                                            {
                                                items.Add(Expression.LiteralInstance(subLiteralList.Elements.Span[i]));
                                            }

                                            continue;
                                        }

                                        return AttemptReduceViaEval();
                                    }

                                    items.AddRange(subList.items);
                                }

                                return Expression.ListInstance(items);
                            }

                            return AttemptReduceViaEval();
                        }

                    case nameof(KernelFunction.length):
                        {
                            if (rootKernelApp.Input is Expression.List inputList)
                            {
                                return
                                    Expression.LiteralInstance(
                                        IntegerEncoding.EncodeSignedInteger(inputList.items.Count));
                            }

                            if (rootKernelApp.Input is Expression.KernelApplication lengthInputKernelApp)
                            {
                                if (lengthInputKernelApp.Function is nameof(KernelFunction.concat) &&
                                    lengthInputKernelApp.Input is Expression.List lengthConcatList)
                                {
                                    int? aggregateLength = 0;

                                    for (var i = 0; i < lengthConcatList.items.Count; i++)
                                    {
                                        var lengthConcatListItem = lengthConcatList.items[i];

                                        {
                                            if (lengthConcatListItem is Expression.Literal lengthConcatListItemLiteral &&
                                                lengthConcatListItemLiteral.Value is PineValue.ListValue lengthConcatListItemList)
                                            {
                                                aggregateLength += lengthConcatListItemList.Elements.Length;
                                                continue;
                                            }
                                        }

                                        {
                                            if (lengthConcatListItem is Expression.List lengthConcatListItemList)
                                            {
                                                aggregateLength += lengthConcatListItemList.items.Count;
                                                continue;
                                            }
                                        }

                                        aggregateLength = null;
                                        break;
                                    }

                                    if (aggregateLength.HasValue)
                                    {
                                        return
                                            Expression.LiteralInstance(
                                                IntegerEncoding.EncodeSignedInteger(aggregateLength.Value));
                                    }
                                }
                            }

                            return AttemptReduceViaEval();
                        }

                    default:
                        return AttemptReduceViaEval();
                }

            case Expression.Conditional conditional:
                {
                    if (!conditional.Condition.ReferencesEnvironment)
                    {
                        if (TryEvaluateExpressionIndependent(conditional.Condition).IsOkOrNull() is { } conditionValue)
                        {
                            return
                                conditionValue == PineVMValues.TrueValue
                                ?
                                conditional.TrueBranch
                                :
                                conditional.FalseBranch;
                        }
                    }

                    if (conditional.TrueBranch == conditional.FalseBranch)
                    {
                        return conditional.TrueBranch;
                    }

                    return AttemptReduceViaEval();
                }

            default:
                return AttemptReduceViaEval();
        }
    }

    public static IEnumerable<int> TryInferListLengthLowerBounds(
        Expression expression,
        EnvConstraintId envConstraintId)
    {
        if (expression is Expression.Literal literalExpr)
        {
            if (literalExpr.Value is PineValue.ListValue literalList)
            {
                yield return literalList.Elements.Length;
            }
        }

        if (expression is Expression.List listExpr)
        {
            yield return listExpr.items.Count;
        }

        var asParsedPath = CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(expression);

        if (asParsedPath is ExprMappedToParentEnv.PathInParentEnv itemPath)
        {
            var itemConstraint = envConstraintId.PartUnderPath(itemPath.Path);

            foreach (var itemConstraintItem in itemConstraint.ParsedEnvItems)
            {
                if (itemConstraintItem.Key.Count is 0)
                {
                    if (itemConstraintItem.Value is PineValue.ListValue itemListValue)
                    {
                        yield return itemListValue.Elements.Length;
                    }
                }
                else
                {
                    yield return itemConstraintItem.Key[0] + 1;
                }
            }
        }

        if (asParsedPath is ExprMappedToParentEnv.LiteralInParentEnv literal)
        {
            if (literal.Value is PineValue.ListValue literalList)
            {
                yield return literalList.Elements.Length;
            }
        }

        if (expression is Expression.KernelApplication kernelApp)
        {
            if (kernelApp.Function is nameof(KernelFunction.skip) &&
                kernelApp.Input is Expression.List skipInputList && skipInputList.items.Count is 2)
            {
                if (TryEvaluateExpressionIndependent(skipInputList.items[0]).IsOkOrNull() is { } okSkipCountValue)
                {
                    if (IntegerEncoding.ParseSignedIntegerRelaxed(okSkipCountValue).IsOkOrNullable() is { } okSkipCount)
                    {
                        var skipCountClamped =
                            (int)(okSkipCount < 0 ? 0 : okSkipCount);

                        foreach (var offsetBound in TryInferListLengthLowerBounds(skipInputList.items[1], envConstraintId))
                        {
                            yield return offsetBound - skipCountClamped;
                        }
                    }
                }
            }
        }
    }

    public static (Expression expr, bool referencesOriginalEnv) TransformPineExpressionWithOptionalReplacement(
        Func<Expression, Expression?> findReplacement,
        Expression expression)
    {
        if (findReplacement(expression) is { } fromReplacement)
            return (fromReplacement, false);

        switch (expression)
        {
            case Expression.Literal:
                return (expression, false);

            case Expression.List list:
                {
                    var referencesOriginalEnv = false;

                    var mappedItems = new Expression[list.items.Count];

                    for (var i = 0; i < list.items.Count; i++)
                    {
                        var (mappedItem, itemReferencesOriginalEnv) =
                            TransformPineExpressionWithOptionalReplacement(
                                findReplacement: findReplacement,
                                expression: list.items[i]);

                        mappedItems[i] = mappedItem;
                        referencesOriginalEnv = referencesOriginalEnv || itemReferencesOriginalEnv;
                    }

                    return (Expression.ListInstance(mappedItems), referencesOriginalEnv);
                }

            case Expression.ParseAndEval parseAndEval:
                {
                    var encodedTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            parseAndEval.Encoded);

                    var envTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            parseAndEval.Environment);

                    return
                        (
                        new Expression.ParseAndEval
                        (
                            encoded: encodedTransform.expr,
                            environment: envTransform.expr
                        ),
                        encodedTransform.referencesOriginalEnv || envTransform.referencesOriginalEnv);
                }

            case Expression.KernelApplication kernelApp:
                {
                    var argumentTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            kernelApp.Input);

                    return
                        (
                        new Expression.KernelApplication(
                            function: kernelApp.Function,
                            input: argumentTransform.expr),
                        argumentTransform.referencesOriginalEnv);
                }

            case Expression.Conditional conditional:
                {
                    var conditionTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            conditional.Condition);

                    var trueBranchTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            conditional.TrueBranch);

                    var falseBranchTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            conditional.FalseBranch);

                    return (
                        Expression.ConditionalInstance
                        (
                            condition: conditionTransform.expr,
                            falseBranch: falseBranchTransform.expr,
                            trueBranch: trueBranchTransform.expr
                            ),
                            conditionTransform.referencesOriginalEnv ||
                            falseBranchTransform.referencesOriginalEnv ||
                            trueBranchTransform.referencesOriginalEnv);
                }

            case Expression.Environment:
                return (expression, true);

            case Expression.StringTag stringTagExpr:
                {
                    var taggedTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            stringTagExpr.Tagged);

                    return
                        (new Expression.StringTag
                        (
                            stringTagExpr.Tag,
                            taggedTransform.expr
                        ),
                        taggedTransform.referencesOriginalEnv);
                }
        }

        throw new NotImplementedException(
            "Expression type not implemented: " + expression.GetType().FullName);
    }

    public static Expression SearchForExpressionReductionRecursive(
        int maxDepth,
        Expression expression,
        EnvConstraintId? envConstraintId = null,
        Func<Expression, bool>? dontReduceExpression = null)
    {
        if (maxDepth < 1)
            return expression;

        var transformed =
            TransformPineExpressionWithOptionalReplacement(
                expr =>
                {
                    if (dontReduceExpression?.Invoke(expr) ?? false)
                        return null;

                    return SearchForExpressionReduction(expr, envConstraintId);
                }, expression).expr;

        if (transformed == expression)
            return transformed;

        return
            SearchForExpressionReductionRecursive(
                maxDepth - 1,
                transformed,
                envConstraintId: envConstraintId,
                dontReduceExpression);
    }

    /// <summary>
    /// Performs a single bottom-up reduction pass:
    ///   1. Recursively reduces children first.
    ///   2. Attempts to reduce the resulting node via <see cref="SearchForExpressionReduction"/>.
    /// </summary>
    public static Expression ReduceExpressionBottomUp(
        Expression expression,
        Func<Expression, bool>? dontReduceExpression = null)
    {
        // First, reduce the sub-expressions.
        var expressionWithReducedChildren =
            expression switch
            {
                Expression.Literal => expression,

                Expression.List listExpr =>
                    ReduceListExpression(listExpr, dontReduceExpression),

                Expression.KernelApplication kernelApp =>
                    ReduceKernelApplication(kernelApp, dontReduceExpression),

                Expression.ParseAndEval parseAndEval =>
                    ReduceParseAndEval(parseAndEval, dontReduceExpression),

                Expression.Conditional conditional =>
                    ReduceConditional(conditional, dontReduceExpression),

                Expression.StringTag stringTag =>
                    ReduceStringTag(stringTag, dontReduceExpression),

                // These are direct references to the environment or stack.
                // No further children to reduce.
                Expression.Environment => expression,

                _ =>
                throw new NotImplementedException(
                    $"Expression type not implemented: {expression.GetType().FullName}")
            };

        // Next, try to reduce this node itself�unless the caller forbids it.
        if (!(dontReduceExpression?.Invoke(expressionWithReducedChildren) ?? false))
        {
            // If we can reduce further, return the reduced expression:
            var reduced = SearchForExpressionReduction(expressionWithReducedChildren, envConstraintId: null);

            if (reduced is not null)
                return reduced;
        }

        // If no further reduction, just return our (possibly child-reduced) node.
        return expressionWithReducedChildren;
    }

    public static Expression ReduceListExpression(
        Expression.List listExpr,
        Func<Expression, bool>? dontReduceExpression)
    {
        var items = listExpr.items;
        var changed = false;
        var newItems = new Expression[items.Count];

        for (var i = 0; i < items.Count; i++)
        {
            var reducedChild = ReduceExpressionBottomUp(items[i], dontReduceExpression);

            newItems[i] = reducedChild;

            changed =
                changed || reducedChild != items[i];
        }

        // If none of the subexpressions changed, return the original; else build a new list.
        return
            changed
            ? Expression.ListInstance(newItems)
            : listExpr;
    }

    public static Expression ReduceKernelApplication(
        Expression.KernelApplication kernelApp,
        Func<Expression, bool>? dontReduceExpression)
    {
        var reducedArg =
            ReduceExpressionBottomUp(kernelApp.Input, dontReduceExpression);

        if (reducedArg == kernelApp.Input)
            return kernelApp;

        return new Expression.KernelApplication(kernelApp.Function, reducedArg);
    }

    public static Expression ReduceParseAndEval(
        Expression.ParseAndEval parseAndEval,
        Func<Expression, bool>? dontReduceExpression)
    {
        var reducedEncoded =
            ReduceExpressionBottomUp(parseAndEval.Encoded, dontReduceExpression);

        var reducedEnv =
            ReduceExpressionBottomUp(parseAndEval.Environment, dontReduceExpression);

        if (reducedEncoded == parseAndEval.Encoded &&
            reducedEnv == parseAndEval.Environment)
        {
            return parseAndEval;
        }

        return new Expression.ParseAndEval(reducedEncoded, reducedEnv);
    }

    public static Expression ReduceConditional(
        Expression.Conditional conditional,
        Func<Expression, bool>? dontReduceExpression)
    {
        var reducedCondition =
            ReduceExpressionBottomUp(conditional.Condition, dontReduceExpression);

        var reducedTrue =
            ReduceExpressionBottomUp(conditional.TrueBranch, dontReduceExpression);

        var reducedFalse =
            ReduceExpressionBottomUp(conditional.FalseBranch, dontReduceExpression);

        if (reducedCondition == conditional.Condition &&
            reducedTrue == conditional.TrueBranch &&
            reducedFalse == conditional.FalseBranch)
        {
            return conditional;
        }

        return Expression.ConditionalInstance(
            condition: reducedCondition,
            falseBranch: reducedFalse,
            trueBranch: reducedTrue);
    }

    public static Expression ReduceStringTag(
        Expression.StringTag stringTag,
        Func<Expression, bool>? dontReduceExpression)
    {
        var reducedTagged =
            ReduceExpressionBottomUp(stringTag.Tagged, dontReduceExpression);

        if (reducedTagged == stringTag.Tagged)
            return stringTag;

        return new Expression.StringTag(stringTag.Tag, reducedTagged);
    }
}

