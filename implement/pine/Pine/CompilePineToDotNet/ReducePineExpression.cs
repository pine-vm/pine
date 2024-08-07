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
            Expression.EnvironmentExpression =>
            "Expression depends on environment",

            Expression.LiteralExpression literal =>
            Result<string, PineValue>.ok(literal.Value),

            Expression.ListExpression list =>
            list.List.Select(TryEvaluateExpressionIndependent)
            .ListCombine()
            .Map(PineValue.List),

            Expression.KernelApplicationExpression kernelApplication =>
            TryEvaluateExpressionIndependent(kernelApplication.argument)
            .MapError(err => "Failed to evaluate kernel application argument independent: " + err)
            .Map(argumentValue =>
            PineVM.PineVM.EvaluateKernelApplicationGeneric(argumentValue, kernelApplication.functionName)),

            Expression.ParseAndEvalExpression parseAndEvalExpr =>
            TryEvaluateExpressionIndependent(parseAndEvalExpr),

            Expression.ConditionalExpression conditional =>
            TryEvaluateExpressionIndependent(conditional),

            Expression.StringTagExpression stringTag =>
            TryEvaluateExpressionIndependent(stringTag.tagged),

            _ =>
            "Unsupported expression type: " + expression.GetType().FullName
        };

    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.ParseAndEvalExpression parseAndEvalExpr)
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
            TryEvaluateExpressionIndependent(parseAndEvalExpr.expression)
            .MapError(err => "Expression is not independent: " + err)
            .AndThen(compilerCache.ParseExpressionFromValue)
            .AndThen(innerExpr => TryEvaluateExpressionIndependent(innerExpr)
            .MapError(err => "Inner expression is not independent: " + err));
    }

    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.ConditionalExpression conditionalExpr)
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
        if (expression is Expression.LiteralExpression)
            return null;

        if (CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(expression) is { } parsedAsPath)
        {
            if (parsedAsPath is ExprMappedToParentEnv.LiteralInParentEnv asLiteral)
            {
                return new Expression.LiteralExpression(asLiteral.Value);
            }

            if (parsedAsPath is ExprMappedToParentEnv.PathInParentEnv asPath && envConstraintId is not null)
            {
                if (envConstraintId.TryGetValue(asPath.Path) is { } fromEnvConstraint)
                {
                    return new Expression.LiteralExpression(fromEnvConstraint);
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
                            fromOk: literalValue => new Expression.LiteralExpression(literalValue));
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
            case Expression.KernelApplicationExpression rootKernelApp:
                switch (rootKernelApp.functionName)
                {
                    case nameof(KernelFunction.equal):
                        {
                            if (rootKernelApp.argument is Expression.ListExpression argumentList)
                            {
                                if (envConstraintId is not null)
                                {
                                    var reducedArgumentsList =
                                        argumentList.List
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

                                    foreach (var item in argumentList.List)
                                    {
                                        int? itemFixedLength = null;

                                        if (item is Expression.LiteralExpression equalArgLiteral)
                                        {
                                            if (equalArgLiteral.Value is PineValue.ListValue equalArgLiteralList)
                                            {
                                                itemFixedLength = equalArgLiteralList.Elements.Count;
                                            }
                                        }

                                        if (item is Expression.ListExpression equalArgList)
                                        {
                                            itemFixedLength = equalArgList.List.Count;
                                        }

                                        if (itemFixedLength.HasValue)
                                        {
                                            if (itemFixedLength < listLengthLowerBound ||
                                                (prevItemFixedLength.HasValue && itemFixedLength.Value != prevItemFixedLength.Value))
                                            {
                                                return new Expression.LiteralExpression(PineVMValues.FalseValue);
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
                            if (rootKernelApp.argument is Expression.ListExpression argumentList)
                            {
                                return
                                    argumentList.List.FirstOrDefault() ??
                                    new Expression.LiteralExpression(PineValue.EmptyList);
                            }

                            if (rootKernelApp.argument is Expression.LiteralExpression literal)
                                return new Expression.LiteralExpression(KernelFunction.list_head(literal.Value));

                            return AttemptReduceViaEval();
                        }

                    case "skip":
                        {
                            if (rootKernelApp.argument is Expression.ListExpression argumentList && argumentList.List.Count is 2)
                            {
                                if (TryEvaluateExpressionIndependent(argumentList.List[0]) is Result<string, PineValue>.Ok okSkipCountValue)
                                {
                                    if (PineValueAsInteger.SignedIntegerFromValueRelaxed(okSkipCountValue.Value) is Result<string, BigInteger>.Ok okSkipCount)
                                    {
                                        if (argumentList.List[1] is Expression.ListExpression partiallySkippedList)
                                        {
                                            return new Expression.ListExpression(
                                                [.. partiallySkippedList.List.Skip((int)okSkipCount.Value)]);
                                        }

                                        if (argumentList.List[1] is Expression.LiteralExpression literal)
                                        {
                                            return new Expression.LiteralExpression(
                                                KernelFunction.skip((int)okSkipCount.Value, literal.Value));
                                        }

                                        if (argumentList.List[1] is Expression.KernelApplicationExpression innerKernelApp)
                                        {
                                            if (innerKernelApp.functionName is "skip"
                                                && innerKernelApp.argument is Expression.ListExpression innerSkipArgList &&
                                                innerSkipArgList.List.Count is 2)
                                            {
                                                if (TryEvaluateExpressionIndependent(innerSkipArgList.List[0]) is Result<string, PineValue>.Ok okInnerSkipCountValue)
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
                                                                argument = new Expression.ListExpression(
                                                                    [
                                                                    new Expression.LiteralExpression(PineValueAsInteger.ValueFromSignedInteger(aggregateSkipCount)),
                                                                    innerSkipArgList.List[1]
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
                            if (rootKernelApp.argument is Expression.ListExpression argumentList)
                            {
                                if (argumentList.List.Count is 0)
                                {
                                    return AttemptReduceViaEval();
                                }

                                if (argumentList.List.Count is 1)
                                {
                                    return argumentList.List[0];
                                }

                                var firstArgExpr = argumentList.List[0];

                                /*
                                if (firstArgExpr is Expression.ListExpression ||
                                    firstArgExpr is Expression.LiteralExpression firstLiteral && firstLiteral.Value is PineValue.ListValue)
                                */
                                {
                                    var nonEmptyItems = new List<Expression>(capacity: argumentList.List.Count);

                                    for (var i = 0; i < argumentList.List.Count; ++i)
                                    {
                                        var argItem = argumentList.List[i];

                                        if (argItem is Expression.ListExpression argList && argList.List.Count is 0)
                                            continue;

                                        if (argItem is Expression.LiteralExpression argLiteral)
                                        {
                                            if (argLiteral.Value is PineValue.ListValue listValue && listValue.Elements.Count is 0)
                                                continue;
                                        }

                                        nonEmptyItems.Add(argItem);
                                    }

                                    if (nonEmptyItems.Count < argumentList.List.Count)
                                    {
                                        if (nonEmptyItems.Count is 0)
                                        {
                                            return new Expression.LiteralExpression(PineValue.EmptyList);
                                        }

                                        if (nonEmptyItems.Count is 1)
                                        {
                                            return nonEmptyItems[0];
                                        }

                                        return
                                            rootKernelApp
                                            with
                                            {
                                                argument = new Expression.ListExpression(nonEmptyItems)
                                            };
                                    }
                                }

                                var items = new List<Expression>();

                                foreach (var argument in argumentList.List)
                                {
                                    if (argument is not Expression.ListExpression subList)
                                    {
                                        if (argument is Expression.LiteralExpression subLiteral &&
                                            subLiteral.Value is PineValue.ListValue subLiteralList)
                                        {
                                            foreach (var literalItem in subLiteralList.Elements)
                                            {
                                                items.Add(new Expression.LiteralExpression(literalItem));
                                            }

                                            continue;
                                        }

                                        return AttemptReduceViaEval();
                                    }

                                    items.AddRange(subList.List);
                                }

                                return new Expression.ListExpression(items);
                            }

                            return AttemptReduceViaEval();
                        }

                    case nameof(KernelFunction.length):
                        {
                            if (rootKernelApp.argument is Expression.ListExpression argumentList)
                            {
                                return
                                    new Expression.LiteralExpression(
                                        PineValueAsInteger.ValueFromSignedInteger(argumentList.List.Count));
                            }

                            if (rootKernelApp.argument is Expression.KernelApplicationExpression lengthArgKernelApp)
                            {
                                if (lengthArgKernelApp.functionName is nameof(KernelFunction.concat) &&
                                    lengthArgKernelApp.argument is Expression.ListExpression lengthConcatList)
                                {
                                    int? aggregateLength = 0;

                                    for (int i = 0; i < lengthConcatList.List.Count; i++)
                                    {
                                        var lengthConcatListItem = lengthConcatList.List[i];

                                        {
                                            if (lengthConcatListItem is Expression.LiteralExpression lengthConcatListItemLiteral &&
                                                lengthConcatListItemLiteral.Value is PineValue.ListValue lengthConcatListItemList)
                                            {
                                                aggregateLength += lengthConcatListItemList.Elements.Count;
                                                continue;
                                            }
                                        }

                                        {
                                            if (lengthConcatListItem is Expression.ListExpression lengthConcatListItemList)
                                            {
                                                aggregateLength += lengthConcatListItemList.List.Count;
                                                continue;
                                            }
                                        }

                                        aggregateLength = null;
                                        break;
                                    }

                                    if (aggregateLength.HasValue)
                                    {
                                        return
                                            new Expression.LiteralExpression(
                                                PineValueAsInteger.ValueFromSignedInteger(aggregateLength.Value));
                                    }
                                }
                            }

                            return AttemptReduceViaEval();
                        }

                    default:
                        return AttemptReduceViaEval();
                }

            case Expression.ConditionalExpression conditional:
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
                                new Expression.LiteralExpression(PineValue.EmptyList));
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
        if (expression is Expression.LiteralExpression literalExpr)
        {
            if (literalExpr.Value is PineValue.ListValue literalList)
            {
                yield return literalList.Elements.Count;
            }
        }

        if (expression is Expression.ListExpression listExpr)
        {
            yield return listExpr.List.Count;
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

        if (expression is Expression.KernelApplicationExpression kernelApp)
        {
            if (kernelApp.functionName is "skip" &&
                kernelApp.argument is Expression.ListExpression skipArgList && skipArgList.List.Count is 2)
            {
                if (TryEvaluateExpressionIndependent(skipArgList.List[0]) is Result<string, PineValue>.Ok okSkipCountValue)
                {
                    if (PineValueAsInteger.SignedIntegerFromValueRelaxed(okSkipCountValue.Value) is Result<string, BigInteger>.Ok okSkipCount)
                    {
                        var skipCountClamped = (int)(okSkipCount.Value < 0 ? 0 : okSkipCount.Value);

                        foreach (var offsetBound in TryInferListLengthLowerBounds(skipArgList.List[1], envConstraintId))
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
            case Expression.LiteralExpression _:
                return (expression, false);

            case Expression.ListExpression list:
                {
                    var referencesOriginalEnv = false;

                    var mappedItems = new Expression[list.List.Count];

                    for (var i = 0; i < list.List.Count; i++)
                    {
                        var (mappedItem, itemReferencesOriginalEnv) =
                            TransformPineExpressionWithOptionalReplacement(
                                findReplacement: findReplacement,
                                expression: list.List[i]);

                        mappedItems[i] = mappedItem;
                        referencesOriginalEnv = referencesOriginalEnv || itemReferencesOriginalEnv;
                    }

                    return (new Expression.ListExpression(mappedItems), referencesOriginalEnv);
                }

            case Expression.ParseAndEvalExpression parseAndEval:
                {
                    var exprTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            parseAndEval.expression);

                    var envTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            parseAndEval.environment);

                    return
                        (
                        new Expression.ParseAndEvalExpression
                        (
                            expression: exprTransform.expr,
                            environment: envTransform.expr
                        ),
                        exprTransform.referencesOriginalEnv || envTransform.referencesOriginalEnv);
                }

            case Expression.KernelApplicationExpression kernelApp:
                {
                    var argumentTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            kernelApp.argument);

                    return
                        (
                        kernelApp
                        with
                        {
                            argument = argumentTransform.expr
                        },
                        argumentTransform.referencesOriginalEnv);
                }

            case Expression.ConditionalExpression conditional:
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
                        new Expression.ConditionalExpression
                        (
                            condition: conditionTransform.expr,
                            falseBranch: falseBranchTransform.expr,
                            trueBranch: trueBranchTransform.expr
                            ),
                            conditionTransform.referencesOriginalEnv ||
                            falseBranchTransform.referencesOriginalEnv ||
                            trueBranchTransform.referencesOriginalEnv);
                }

            case Expression.EnvironmentExpression:
                return (expression, true);

            case Expression.StringTagExpression stringTagExpr:
                {
                    var taggedTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            stringTagExpr.tagged);

                    return
                        (new Expression.StringTagExpression
                        (
                            stringTagExpr.tag,
                            taggedTransform.expr
                        ),
                        taggedTransform.referencesOriginalEnv);
                }

            case Expression.StackReferenceExpression:
                return (expression, true);

            case Expression.KernelApplications_Skip_ListHead_Path_Expression skipListHead:
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

