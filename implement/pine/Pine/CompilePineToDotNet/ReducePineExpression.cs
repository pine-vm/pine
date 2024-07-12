using Pine.PineVM;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Numerics;

namespace Pine.CompilePineToDotNet;

public class ReducePineExpression
{
    static private readonly CompilerMutableCache compilerCache = new();

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
            .Map(kernelApplication.function),

            Expression.ParseAndEvalExpression parseAndEvalExpr =>
            TryEvaluateExpressionIndependent(parseAndEvalExpr)
            .Map(ok =>
            {
                Console.WriteLine("Successfully evaluated ParseAndEvalExpression independent 🙃");

                return ok;
            }),

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
            return
                new PineVM.PineVM()
                .EvaluateExpressionDefaultLessStack(
                    parseAndEvalExpr,
                    PineValue.EmptyList,
                    stackPrevValues: ReadOnlyMemory<PineValue>.Empty);
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
            TryEvaluateExpressionIndependent(conditionalExpr)
            .AndThen(conditionValue =>
            {
                if (conditionValue == PineVMValues.FalseValue)
                    return TryEvaluateExpressionIndependent(conditionalExpr.ifFalse);

                if (conditionValue == PineVMValues.TrueValue)
                    return TryEvaluateExpressionIndependent(conditionalExpr.ifTrue);

                return PineValue.EmptyList;
            });
    }

    public static Expression? SearchForExpressionReduction(Expression expression)
    {
        if (expression is Expression.LiteralExpression)
            return null;

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

                    case "concat":
                        {
                            if (rootKernelApp.argument is Expression.ListExpression argumentList)
                            {
                                if (argumentList.List.Count is 0)
                                {
                                    return AttemptReduceViaEval();
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

                    default:
                        return AttemptReduceViaEval();
                }

            case Expression.ConditionalExpression conditional:
                {
                    if (Expression.IsIndependent(conditional.condition))
                    {
                        return
                            TryEvaluateExpressionIndependent(conditional.condition)
                            .Unpack(
                                fromErr: _ =>
                                AttemptReduceViaEval(),

                                fromOk: conditionValue =>
                                conditionValue == PineVMValues.TrueValue
                                ?
                                conditional.ifTrue
                                :
                                conditionValue == PineVMValues.FalseValue
                                ?
                                conditional.ifFalse
                                :
                                new Expression.LiteralExpression(PineValue.EmptyList));
                    }

                    return AttemptReduceViaEval();
                }

            default:
                return AttemptReduceViaEval();
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

                    var ifTrueTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            conditional.ifTrue);

                    var ifFalseTransform =
                        TransformPineExpressionWithOptionalReplacement(
                            findReplacement,
                            conditional.ifFalse);

                    return (
                        new Expression.ConditionalExpression
                        (
                            condition: conditionTransform.expr,
                            ifTrue: ifTrueTransform.expr,
                            ifFalse: ifFalseTransform.expr
                            ),
                            conditionTransform.referencesOriginalEnv ||
                            ifTrueTransform.referencesOriginalEnv ||
                            ifFalseTransform.referencesOriginalEnv);
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

                    return SearchForExpressionReduction(expr);
                }, expression).expr;

        if (transformed == expression)
            return transformed;

        return
            SearchForExpressionReductionRecursive(
                maxDepth - 1,
                transformed,
                dontReduceExpression);
    }
}

