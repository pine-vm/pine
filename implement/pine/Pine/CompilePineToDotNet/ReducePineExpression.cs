using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

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
            list.items.Select(TryEvaluateExpressionIndependent)
            .ListCombine()
            .Map(PineValue.List),

            Expression.KernelApplication kernelApplication =>
            TryEvaluateExpressionIndependent(kernelApplication.input)
            .MapError(err => "Failed to evaluate kernel application input independent: " + err)
            .Map(inputValue =>
            PineVM.PineVM.EvaluateKernelApplicationGeneric(kernelApplication.function, inputValue)),

            Expression.ParseAndEval parseAndEvalExpr =>
            TryEvaluateExpressionIndependent(parseAndEvalExpr),

            Expression.Conditional conditional =>
            TryEvaluateExpressionIndependent(conditional),

            Expression.StringTag stringTag =>
            TryEvaluateExpressionIndependent(stringTag.tagged),

            _ =>
            "Unsupported expression type: " + expression.GetType().FullName
        };

    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.ParseAndEval parseAndEvalExpr)
    {
        if (TryEvaluateExpressionIndependent(parseAndEvalExpr.environment) is Result<string, PineValue>.Ok envOk)
        {
            try
            {
                return
                    new PineVM.PineVM(
                        overrideParseExpression: parseCache.BuildParseExprDelegate)
                    .EvaluateExpressionDefaultLessStack(
                        parseAndEvalExpr,
                        PineValue.EmptyList,
                        stackPrevValues: ReadOnlyMemory<PineValue>.Empty);
            }
            catch (ParseExpressionException)
            {
                /*
                * A branch of a conditional expression might always fail to parse.
                * This does not mean that the code would crash at runtime, since it is conditional.
                * (The Elm compiler uses constructs like this to encoding crashing branches with a custom error message.)
                * */
            }
        }

        return
            TryEvaluateExpressionIndependent(parseAndEvalExpr.encoded)
            .MapError(err => "Expression is not independent: " + err)
            .AndThen(compilerCache.ParseExpressionFromValue)
            .AndThen(innerExpr => TryEvaluateExpressionIndependent(innerExpr)
            .MapError(err => "Inner expression is not independent: " + err));
    }

    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.Conditional conditionalExpr)
    {
        return
            TryEvaluateExpressionIndependent(conditionalExpr.condition)
            .AndThen(conditionValue =>
            {
                if (conditionValue == PineVMValues.FalseValue)
                    return TryEvaluateExpressionIndependent(conditionalExpr.falseBranch);

                if (conditionValue == PineVMValues.TrueValue)
                    return TryEvaluateExpressionIndependent(conditionalExpr.trueBranch);

                return PineValue.EmptyList;
            });
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
                return new Expression.Literal(asLiteral.Value);
            }

            if (parsedAsPath is ExprMappedToParentEnv.PathInParentEnv asPath && envConstraintId is not null)
            {
                if (envConstraintId.TryGetValue(asPath.Path) is { } fromEnvConstraint)
                {
                    return new Expression.Literal(fromEnvConstraint);
                }
            }
        }

        Expression? AttemptReduceViaEval()
        {
            if (Expression.IsIndependent(expression))
            {
                try
                {
                    return
                        TryEvaluateExpressionIndependent(expression)
                        .Unpack(
                            fromErr: _ => null,
                            fromOk: literalValue => new Expression.Literal(literalValue));
                }
                catch (ParseExpressionException)
                {
                    /*
                     * A branch of a conditional expression might always fail to parse.
                     * This does not mean that the code would crash at runtime, since it is conditional.
                     * (The Elm compiler uses constructs like this to encoding crashing branches with a custom error message.)
                     * */
                }
            }

            return null;
        }

        switch (expression)
        {
            case Expression.KernelApplication rootKernelApp:
                switch (rootKernelApp.function)
                {
                    case nameof(KernelFunction.equal):
                        {
                            if (rootKernelApp.input is Expression.List inputList)
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

                                    foreach (var item in reducedArgumentsList)
                                    {
                                        listLengthLowerBounds.AddRange(
                                            TryInferListLengthLowerBounds(item, envConstraintId));
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
                                                itemFixedLength = equalArgLiteralList.Elements.Count;
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
                                                return new Expression.Literal(PineVMValues.FalseValue);
                                            }

                                            prevItemFixedLength = itemFixedLength;
                                        }
                                    }
                                }
                            }

                            return AttemptReduceViaEval();
                        }

                    case "list_head":
                        {
                            if (rootKernelApp.input is Expression.List inputList)
                            {
                                return
                                    inputList.items.FirstOrDefault() ??
                                    new Expression.Literal(PineValue.EmptyList);
                            }

                            if (rootKernelApp.input is Expression.Literal literal)
                                return new Expression.Literal(KernelFunction.list_head(literal.Value));

                            return AttemptReduceViaEval();
                        }

                    case "skip":
                        {
                            if (rootKernelApp.input is Expression.List inputList && inputList.items.Count is 2)
                            {
                                if (TryEvaluateExpressionIndependent(inputList.items[0]) is Result<string, PineValue>.Ok okSkipCountValue)
                                {
                                    if (PineValueAsInteger.SignedIntegerFromValueRelaxed(okSkipCountValue.Value) is Result<string, BigInteger>.Ok okSkipCount)
                                    {
                                        if (inputList.items[1] is Expression.List partiallySkippedList)
                                        {
                                            return new Expression.List(
                                                [.. partiallySkippedList.items.Skip((int)okSkipCount.Value)]);
                                        }

                                        if (inputList.items[1] is Expression.Literal literal)
                                        {
                                            return new Expression.Literal(
                                                KernelFunction.skip((int)okSkipCount.Value, literal.Value));
                                        }

                                        if (inputList.items[1] is Expression.KernelApplication innerKernelApp)
                                        {
                                            if (innerKernelApp.function is "skip"
                                                && innerKernelApp.input is Expression.List innerSkipInputList &&
                                                innerSkipInputList.items.Count is 2)
                                            {
                                                if (TryEvaluateExpressionIndependent(innerSkipInputList.items[0]) is Result<string, PineValue>.Ok okInnerSkipCountValue)
                                                {
                                                    if (PineValueAsInteger.SignedIntegerFromValueRelaxed(okInnerSkipCountValue.Value) is Result<string, BigInteger>.Ok okInnerSkipCount)
                                                    {
                                                        var outerSkipCountClamped =
                                                            okSkipCount.Value < 0 ? 0 : okSkipCount.Value;

                                                        var innerSkipCountClamped =
                                                            okInnerSkipCount.Value < 0 ? 0 : okInnerSkipCount.Value;

                                                        var aggregateSkipCount = outerSkipCountClamped + innerSkipCountClamped;

                                                        return
                                                            rootKernelApp
                                                            with
                                                            {
                                                                input = new Expression.List(
                                                                    [
                                                                    new Expression.Literal(PineValueAsInteger.ValueFromSignedInteger(aggregateSkipCount)),
                                                                    innerSkipInputList.items[1]
                                                                    ]
                                                                )
                                                            };
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
                            if (rootKernelApp.input is Expression.List inputList)
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
                                            if (argLiteral.Value is PineValue.ListValue listValue && listValue.Elements.Count is 0)
                                                continue;
                                        }

                                        nonEmptyItems.Add(argItem);
                                    }

                                    if (nonEmptyItems.Count < inputList.items.Count)
                                    {
                                        if (nonEmptyItems.Count is 0)
                                        {
                                            return new Expression.Literal(PineValue.EmptyList);
                                        }

                                        if (nonEmptyItems.Count is 1)
                                        {
                                            return nonEmptyItems[0];
                                        }

                                        return
                                            rootKernelApp
                                            with
                                            {
                                                input = new Expression.List(nonEmptyItems)
                                            };
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
                                            foreach (var literalItem in subLiteralList.Elements)
                                            {
                                                items.Add(new Expression.Literal(literalItem));
                                            }

                                            continue;
                                        }

                                        return AttemptReduceViaEval();
                                    }

                                    items.AddRange(subList.items);
                                }

                                return new Expression.List(items);
                            }

                            return AttemptReduceViaEval();
                        }

                    case nameof(KernelFunction.length):
                        {
                            if (rootKernelApp.input is Expression.List inputList)
                            {
                                return
                                    new Expression.Literal(
                                        PineValueAsInteger.ValueFromSignedInteger(inputList.items.Count));
                            }

                            if (rootKernelApp.input is Expression.KernelApplication lengthInputKernelApp)
                            {
                                if (lengthInputKernelApp.function is nameof(KernelFunction.concat) &&
                                    lengthInputKernelApp.input is Expression.List lengthConcatList)
                                {
                                    int? aggregateLength = 0;

                                    for (int i = 0; i < lengthConcatList.items.Count; i++)
                                    {
                                        var lengthConcatListItem = lengthConcatList.items[i];

                                        {
                                            if (lengthConcatListItem is Expression.Literal lengthConcatListItemLiteral &&
                                                lengthConcatListItemLiteral.Value is PineValue.ListValue lengthConcatListItemList)
                                            {
                                                aggregateLength += lengthConcatListItemList.Elements.Count;
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
                                            new Expression.Literal(
                                                PineValueAsInteger.ValueFromSignedInteger(aggregateLength.Value));
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
                    var condition =
                        SearchForExpressionReductionRecursive(
                            maxDepth: 5,
                            conditional.condition,
                            envConstraintId: envConstraintId);

                    if (Expression.IsIndependent(condition))
                    {
                        return
                            TryEvaluateExpressionIndependent(condition)
                            .Unpack(
                                fromErr: _ =>
                                AttemptReduceViaEval(),

                                fromOk: conditionValue =>
                                conditionValue == PineVMValues.TrueValue
                                ?
                                conditional.trueBranch
                                :
                                conditionValue == PineVMValues.FalseValue
                                ?
                                conditional.falseBranch
                                :
                                new Expression.Literal(PineValue.EmptyList));
                    }

                    if (conditional.trueBranch == conditional.falseBranch)
                    {
                        return conditional.trueBranch;
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
                yield return literalList.Elements.Count;
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
                        yield return itemListValue.Elements.Count;
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
                yield return literalList.Elements.Count;
            }
        }

        if (expression is Expression.KernelApplication kernelApp)
        {
            if (kernelApp.function is "skip" &&
                kernelApp.input is Expression.List skipInputList && skipInputList.items.Count is 2)
            {
                if (TryEvaluateExpressionIndependent(skipInputList.items[0]) is Result<string, PineValue>.Ok okSkipCountValue)
                {
                    if (PineValueAsInteger.SignedIntegerFromValueRelaxed(okSkipCountValue.Value) is Result<string, BigInteger>.Ok okSkipCount)
                    {
                        var skipCountClamped = (int)(okSkipCount.Value < 0 ? 0 : okSkipCount.Value);

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
            case Expression.Literal _:
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

                    return (new Expression.List(mappedItems), referencesOriginalEnv);
                }

            case Expression.ParseAndEval parseAndEval:
                {
                    var encodedTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            parseAndEval.encoded);

                    var envTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            parseAndEval.environment);

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
                            kernelApp.input);

                    return
                        (
                        kernelApp
                        with
                        {
                            input = argumentTransform.expr
                        },
                        argumentTransform.referencesOriginalEnv);
                }

            case Expression.Conditional conditional:
                {
                    var conditionTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            conditional.condition);

                    var trueBranchTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            conditional.trueBranch);

                    var falseBranchTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            conditional.falseBranch);

                    return (
                        new Expression.Conditional
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
                            stringTagExpr.tagged);

                    return
                        (new Expression.StringTag
                        (
                            stringTagExpr.tag,
                            taggedTransform.expr
                        ),
                        taggedTransform.referencesOriginalEnv);
                }

            case Expression.StackReferenceExpression:
                return (expression, true);

            case Expression.KernelApplications_Skip_ListHead_Path skipListHead:
                {
                    var argumentTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            skipListHead.Argument);

                    return
                        (
                        skipListHead
                        with
                        {
                            Argument = argumentTransform.expr
                        },
                        argumentTransform.referencesOriginalEnv);
                }

            case Expression.KernelApplication_Equal_Two equalTwo:
                {
                    var leftTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            equalTwo.left);

                    var rightTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            equalTwo.right);

                    return (
                        new Expression.KernelApplication_Equal_Two
                        (
                            left: leftTransform.expr,
                            right: rightTransform.expr
                        ),
                        leftTransform.referencesOriginalEnv || rightTransform.referencesOriginalEnv);
                }

            case Expression.DelegatingExpression:
                return (expression, true);
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
}

