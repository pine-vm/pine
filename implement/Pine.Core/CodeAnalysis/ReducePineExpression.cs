using Pine.Core.PineVM;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using KernelFunctionSpecialized = Pine.Core.Internal.KernelFunctionSpecialized;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Helpers for evaluating and reducing Pine expressions without requiring a runtime environment.
/// Provides utilities for constant folding, small-step reductions, and tree transformations
/// used by analyzers and compilers.
/// </summary>
public class ReducePineExpression
{
    /// <summary>
    /// Attempts to evaluate an <see cref="Expression"/> that does not depend on the environment.
    /// Returns a <c>Result</c> whose <c>Ok</c> value is the computed <see cref="PineValue"/>,
    /// or an error message if the expression cannot be evaluated independently.
    /// </summary>
    /// <param name="expression">The expression to evaluate.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <returns>A <see cref="Result{TError, TOk}"/> with an error description or the computed value.</returns>
    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression expression,
        PineVMParseCache parseCache) =>
        expression switch
        {
            Expression.Environment =>
            "Expression depends on environment",

            Expression.Literal literal =>
            Result<string, PineValue>.ok(literal.Value),

            Expression.List list =>
            TryEvaluateExpressionIndependent(list, parseCache),

            Expression.KernelApplication kernelApplication =>
            TryEvaluateExpressionIndependent(kernelApplication, parseCache),

            Expression.ParseAndEval parseAndEvalExpr =>
            TryEvaluateExpressionIndependent(parseAndEvalExpr, parseCache),

            Expression.Conditional conditional =>
            TryEvaluateExpressionIndependent(conditional, parseCache),

            Expression.StringTag stringTag =>
            TryEvaluateExpressionIndependent(stringTag.Tagged, parseCache),

            _ =>
            "Unsupported expression type: " + expression.GetType().FullName
        };

    /// <summary>
    /// Attempts to evaluate a list expression by evaluating each item independently.
    /// On success, returns a concrete <see cref="PineValue.ListValue"/>; otherwise the first error encountered.
    /// </summary>
    /// <param name="listExpr">The list expression to evaluate.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <returns>The evaluated list value or an error message.</returns>
    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.List listExpr,
        PineVMParseCache parseCache)
    {
        var itemsValues = new PineValue[listExpr.Items.Count];

        for (var i = 0; i < listExpr.Items.Count; i++)
        {
            var itemResult = TryEvaluateExpressionIndependent(listExpr.Items[i], parseCache);

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

    /// <summary>
    /// Attempts to evaluate a Parse-and-Eval expression without a runtime environment.
    /// The encoded expression is parsed using <paramref name="parseCache"/>, the provided environment is inlined,
    /// and the inner expression is then evaluated independently.
    /// </summary>
    /// <param name="parseAndEvalExpr">The Parse-and-Eval expression to evaluate.</param>
    /// <param name="parseCache">Cache used for parsing encoded expressions.</param>
    /// <returns>The computed value on success, or an error message if parsing or evaluation fails.</returns>
    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.ParseAndEval parseAndEvalExpr,
        PineVMParseCache parseCache)
    {
        var evalEnvResult =
            TryEvaluateExpressionIndependent(parseAndEvalExpr.Environment, parseCache);

        {
            if (evalEnvResult.IsErrOrNull() is { } err)
            {
                return
                    "Failed evaluating env of parse-and-eval: " + err;
            }
        }

        if (evalEnvResult.IsOkOrNull() is not { } envValue)
        {
            throw new NotImplementedException(
                "Unexpected result type: " + evalEnvResult);
        }

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

        var evalEncodedExprResult =
            TryEvaluateExpressionIndependent(parseAndEvalExpr.Encoded, parseCache);

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

        var parseResult = parseCache.ParseExpression(encodedOk);

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

        if (!parseOk.ReferencesEnvironment)
        {
            return
                TryEvaluateExpressionIndependent(parseOk, parseCache);
        }

        /*
         * 2025-09-16: Disabled after observing Stack overflow here.
         * 

        Expression? Replacement(Expression expr)
        {
            if (expr is Expression.Environment)
            {
                return Expression.LiteralInstance(envValue);
            }

            return null;
        }

        var exprAfterInliningEnv =
            TransformPineExpressionWithOptionalReplacement(
                Replacement,
                parseOk);

        var evalInnerExprResult =
            TryEvaluateExpressionIndependent(exprAfterInliningEnv.expr, parseCache);

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
        */

        return "Not following parse&eval";
    }

    /// <summary>
    /// Attempts to evaluate a conditional expression independently.
    /// Evaluates the condition and returns the value of either the true or false branch accordingly.
    /// </summary>
    /// <param name="conditionalExpr">The conditional expression.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <returns>The result of the selected branch, or an error message if evaluation fails.</returns>
    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.Conditional conditionalExpr,
        PineVMParseCache parseCache)
    {
        var evalConditionResult =
            TryEvaluateExpressionIndependent(conditionalExpr.Condition, parseCache);

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

        if (conditionOk == PineKernelValues.TrueValue)
        {
            return TryEvaluateExpressionIndependent(conditionalExpr.TrueBranch, parseCache);
        }

        return TryEvaluateExpressionIndependent(conditionalExpr.FalseBranch, parseCache);
    }

    /// <summary>
    /// Attempts to evaluate a kernel function application independently.
    /// The input is first evaluated, then dispatched to <see cref="KernelFunction.ApplyKernelFunctionGeneric(string, PineValue)"/>.
    /// </summary>
    /// <param name="kernelApplication">The kernel application expression.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <returns>The computed value, or an error message if evaluation fails.</returns>
    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.KernelApplication kernelApplication,
        PineVMParseCache parseCache)
    {
        var evalInputResult =
            TryEvaluateExpressionIndependent(kernelApplication.Input, parseCache);

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

        return KernelFunction.ApplyKernelFunctionGeneric(kernelApplication.Function, inputOk);
    }

    /// <summary>
    /// Searches for a local reduction opportunity in the given <paramref name="expression"/>.
    /// If a reduction is found, returns the reduced expression; otherwise returns <c>null</c>.
    /// Optional environment constraints may enable more reductions.
    /// </summary>
    /// <param name="expression">The expression to inspect.</param>
    /// <param name="envConstraintId">Optional structural constraints for values taken from the environment.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <returns>A reduced expression or <c>null</c> if no reduction applies.</returns>
    public static Expression? SearchForExpressionReduction(
        Expression expression,
        PineValueClass? envConstraintId,
        PineVMParseCache parseCache)
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
                if (TryEvaluateExpressionIndependent(expression, parseCache).IsOkOrNull() is { } okValue)
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

                Expression.KernelApplication ContinueWithReducedInput(Expression newInput) =>
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
                                        inputList.Items
                                        .Select(origArg => SearchForExpressionReductionRecursive(
                                            maxDepth: 5,
                                            expression: origArg,
                                            parseCache: parseCache,
                                            envConstraintId: envConstraintId))
                                        .ToImmutableArray();

                                    var listLengthLowerBounds = new List<int>();

                                    var listConcreteValues = new List<PineValue>();

                                    foreach (var item in reducedArgumentsList)
                                    {
                                        foreach (var itemBounds in EnumerateInferListLengthBounds(item, envConstraintId, parseCache))
                                        {
                                            if (itemBounds.lower is { } lowerBound)
                                            {
                                                listLengthLowerBounds.Add(lowerBound);
                                            }
                                        }

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
                                                return Expression.LiteralInstance(PineKernelValues.FalseValue);
                                            }
                                        }
                                    }

                                    var listLengthLowerBound =
                                        listLengthLowerBounds.Count is 0 ?
                                        (int?)null
                                        :
                                        listLengthLowerBounds.Max();

                                    int? prevItemFixedLength = null;

                                    foreach (var item in inputList.Items)
                                    {
                                        int? itemFixedLength = null;

                                        if (item is Expression.Literal equalArgLiteral)
                                        {
                                            if (equalArgLiteral.Value is PineValue.ListValue equalArgLiteralList)
                                            {
                                                itemFixedLength = equalArgLiteralList.Items.Length;
                                            }
                                        }

                                        if (item is Expression.List equalArgList)
                                        {
                                            itemFixedLength = equalArgList.Items.Count;
                                        }

                                        if (itemFixedLength.HasValue)
                                        {
                                            if (itemFixedLength < listLengthLowerBound ||
                                                (prevItemFixedLength.HasValue && itemFixedLength.Value != prevItemFixedLength.Value))
                                            {
                                                return Expression.LiteralInstance(PineKernelValues.FalseValue);
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
                            if (ApplyKernelFunctionHeadToAllBranches(rootKernelApp.Input) is { } reducedBranches)
                                return reducedBranches;

                            return AttemptReduceViaEval();
                        }

                    case nameof(KernelFunction.skip):
                        {
                            if (rootKernelApp.Input is Expression.List inputList && inputList.Items.Count is 2)
                            {
                                var countExpr = inputList.Items[0];
                                var seqExpr = inputList.Items[1];

                                if (TryEvaluateExpressionIndependent(countExpr, parseCache).IsOkOrNull() is { } okSkipCountValue &&
                                    KernelFunction.SignedIntegerFromValueRelaxed(okSkipCountValue) is { } okSkipCount)
                                {
                                    if (ApplyKernelFunctionSkipToAllBranches((int)(okSkipCount < 0 ? 0 : okSkipCount), seqExpr) is { } reducedSkip)
                                    {
                                        return reducedSkip;
                                    }
                                }

                                return AttemptReduceViaEval();
                            }

                            return AttemptReduceViaEval();
                        }

                    case nameof(KernelFunction.take):
                        {
                            if (rootKernelApp.Input is Expression.List takeInput && takeInput.Items.Count is 2)
                            {
                                var countExpr = takeInput.Items[0];
                                var srcExpr = takeInput.Items[1];

                                if (TryEvaluateExpressionIndependent(countExpr, parseCache).IsOkOrNull() is { } okTakeCountValue &&
                                    KernelFunction.SignedIntegerFromValueRelaxed(okTakeCountValue) is { } okTakeCount)
                                {
                                    if (ApplyKernelFunctionTakeToAllBranches((int)okTakeCount, srcExpr) is { } reducedTake)
                                    {
                                        return reducedTake;
                                    }
                                }

                                return AttemptReduceViaEval();
                            }

                            return AttemptReduceViaEval();
                        }

                    case nameof(KernelFunction.reverse):
                        {
                            if (ApplyKernelFunctionReverseToAllBranches(rootKernelApp.Input) is { } reducedRev)
                            {
                                return reducedRev;
                            }

                            return AttemptReduceViaEval();
                        }

                    case nameof(KernelFunction.concat):
                        {
                            if (rootKernelApp.Input is Expression.List inputList)
                            {
                                if (inputList.Items.Count is 0)
                                {
                                    return AttemptReduceViaEval();
                                }

                                if (inputList.Items.Count is 1)
                                {
                                    return inputList.Items[0];
                                }

                                var firstArgExpr = inputList.Items[0];

                                /*
                                if (firstArgExpr is Expression.ListExpression ||
                                    firstArgExpr is Expression.LiteralExpression firstLiteral && firstLiteral.Value is PineValue.ListValue)
                                */
                                {
                                    var nonEmptyItems = new List<Expression>(capacity: inputList.Items.Count);

                                    for (var i = 0; i < inputList.Items.Count; ++i)
                                    {
                                        var argItem = inputList.Items[i];

                                        if (argItem is Expression.List argList && argList.Items.Count is 0)
                                            continue;

                                        if (argItem is Expression.Literal argLiteral)
                                        {
                                            if (argLiteral.Value is PineValue.ListValue listValue && listValue.Items.Length is 0)
                                                continue;
                                        }

                                        nonEmptyItems.Add(argItem);
                                    }

                                    if (nonEmptyItems.Count < inputList.Items.Count)
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
                                            ContinueWithReducedInput(Expression.ListInstance(nonEmptyItems));
                                    }
                                }

                                var items = new List<Expression>();

                                foreach (var argument in inputList.Items)
                                {
                                    if (argument is not Expression.List subList)
                                    {
                                        if (argument is Expression.Literal subLiteral &&
                                            subLiteral.Value is PineValue.ListValue subLiteralList)
                                        {
                                            for (var i = 0; i < subLiteralList.Items.Length; i++)
                                            {
                                                items.Add(Expression.LiteralInstance(subLiteralList.Items.Span[i]));
                                            }

                                            continue;
                                        }

                                        return AttemptReduceViaEval();
                                    }

                                    items.AddRange(subList.Items);
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
                                        IntegerEncoding.EncodeSignedInteger(inputList.Items.Count));
                            }

                            if (rootKernelApp.Input is Expression.KernelApplication lengthInputKernelApp)
                            {
                                if (lengthInputKernelApp.Function is nameof(KernelFunction.concat) &&
                                    lengthInputKernelApp.Input is Expression.List lengthConcatList)
                                {
                                    int? aggregateLength = 0;

                                    for (var i = 0; i < lengthConcatList.Items.Count; i++)
                                    {
                                        var lengthConcatListItem = lengthConcatList.Items[i];

                                        {
                                            if (lengthConcatListItem is Expression.Literal lengthConcatListItemLiteral &&
                                                lengthConcatListItemLiteral.Value is PineValue.ListValue lengthConcatListItemList)
                                            {
                                                aggregateLength += lengthConcatListItemList.Items.Length;
                                                continue;
                                            }
                                        }

                                        {
                                            if (lengthConcatListItem is Expression.List lengthConcatListItemList)
                                            {
                                                aggregateLength += lengthConcatListItemList.Items.Count;
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
                        if (TryEvaluateExpressionIndependent(conditional.Condition, parseCache).IsOkOrNull() is { } conditionValue)
                        {
                            return
                                conditionValue == PineKernelValues.TrueValue
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

    /// <summary>
    /// Infers lower and upper bounds for the length of lists produced by the given expression,
    /// using optional environment constraints and local reasoning.
    /// </summary>
    public static (int? lower, int? upper) InferListLengthBounds(
        Expression expression,
        PineValueClass envConstraintId,
        PineVMParseCache parseCache)
    {
        int? lowerBound = null;
        int? upperBound = null;

        if (expression != null)
        {
            foreach (var nextBounds in EnumerateInferListLengthBounds(expression, envConstraintId, parseCache))
            {
                if (lowerBound.HasValue)
                {
                    if (nextBounds.lower > lowerBound.Value)
                    {
                        lowerBound = nextBounds.lower;
                    }
                }
                else
                {
                    lowerBound = nextBounds.lower;
                }

                if (upperBound.HasValue)
                {
                    if (nextBounds.upper < upperBound.Value)
                    {
                        upperBound = nextBounds.upper;
                    }
                }
                else
                {
                    upperBound = nextBounds.upper;
                }
            }
        }

        return (lowerBound, upperBound);
    }

    /// <summary>
    /// Infers lower and upper bounds for the length of lists produced by the given expression,
    /// using optional environment constraints and local reasoning.
    /// </summary>
    /// <param name="expression">The expression to analyze.</param>
    /// <param name="envConstraintId">Constraints for values reachable from the environment.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <returns>An enumeration of lower bound candidates for the list length.</returns>
    public static IEnumerable<(int? lower, int? upper)> EnumerateInferListLengthBounds(
        Expression expression,
        PineValueClass envConstraintId,
        PineVMParseCache parseCache)
    {
        if (expression is Expression.Literal literalExpr)
        {
            if (literalExpr.Value is PineValue.ListValue literalList)
            {
                yield return (literalList.Items.Length, literalList.Items.Length);
            }
        }

        if (expression is Expression.List listExpr)
        {
            yield return (listExpr.Items.Count, listExpr.Items.Count);
        }

        var asParsedPath = CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(expression);

        if (asParsedPath is ExprMappedToParentEnv.PathInParentEnv itemPath)
        {
            var itemConstraint = envConstraintId.PartUnderPath(itemPath.Path);

            foreach (var itemConstraintItem in itemConstraint.ParsedItems)
            {
                if (itemConstraintItem.Key.Count is 0)
                {
                    if (itemConstraintItem.Value is PineValue.ListValue itemListValue)
                    {
                        yield return (itemListValue.Items.Length, null);
                    }
                }
                else
                {
                    yield return (itemConstraintItem.Key[0] + 1, null);
                }
            }
        }

        if (asParsedPath is ExprMappedToParentEnv.LiteralInParentEnv literal)
        {
            if (literal.Value is PineValue.ListValue literalList)
            {
                yield return (literalList.Items.Length, literalList.Items.Length);
            }
        }

        if (expression is Expression.KernelApplication kernelApp)
        {
            if (kernelApp.Function is nameof(KernelFunction.skip) &&
                kernelApp.Input is Expression.List skipInputList && skipInputList.Items.Count is 2)
            {
                if (TryEvaluateExpressionIndependent(skipInputList.Items[0], parseCache).IsOkOrNull() is { } okSkipCountValue)
                {
                    if (IntegerEncoding.ParseSignedIntegerRelaxed(okSkipCountValue).IsOkOrNullable() is { } okSkipCount)
                    {
                        var skipCountClamped =
                            (int)(okSkipCount < 0 ? 0 : okSkipCount);

                        foreach (var offsetBound in EnumerateInferListLengthBounds(skipInputList.Items[1], envConstraintId, parseCache))
                        {
                            yield return (offsetBound.lower - skipCountClamped, offsetBound.upper - skipCountClamped);
                        }
                    }
                }
            }
        }
    }

    /// <summary>
    /// Transforms an expression tree by attempting to replace nodes using a provided function.
    /// Returns the transformed expression along with a flag indicating whether the original
    /// environment is still referenced by any node in the transformed tree.
    /// </summary>
    /// <param name="findReplacement">Function that returns a replacement expression for a node, or <c>null</c> to keep it.</param>
    /// <param name="expression">The root expression to transform.</param>
    /// <returns>The transformed expression and whether it references the original environment.</returns>
    public static (Expression expr, bool referencesOriginalEnv) TransformPineExpressionWithOptionalReplacement(
        Func<Expression, Expression?> findReplacement,
        Expression expression)
    {
        return
            TransformPineExpressionWithOptionalReplacement(
                findReplacement,
                expression,
                cache: []);
    }

    private static (Expression expr, bool referencesOriginalEnv) TransformPineExpressionWithOptionalReplacement(
        Func<Expression, Expression?> findReplacement,
        Expression expression,
        Dictionary<Expression, (Expression expr, bool referencesOriginalEnv)> cache)
    {
        if (cache.TryGetValue(expression, out var cached))
            return cached;

        (Expression expr, bool referencesOriginalEnv) LessCache()
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

                        var mappedItems = new Expression[list.Items.Count];

                        var anyChanged = false;

                        for (var i = 0; i < list.Items.Count; i++)
                        {
                            var (mappedItem, itemReferencesOriginalEnv) =
                                TransformPineExpressionWithOptionalReplacement(
                                    findReplacement: findReplacement,
                                    expression: list.Items[i],
                                    cache);

                            anyChanged = anyChanged || (mappedItem != list.Items[i]);

                            mappedItems[i] = mappedItem;

                            referencesOriginalEnv = referencesOriginalEnv || itemReferencesOriginalEnv;
                        }

                        if (!anyChanged)
                        {
                            return (list, referencesOriginalEnv);
                        }

                        return (Expression.ListInstance(mappedItems), referencesOriginalEnv);
                    }

                case Expression.ParseAndEval parseAndEval:
                    {
                        var encodedTransform =
                            TransformPineExpressionWithOptionalReplacement(
                                findReplacement,
                                parseAndEval.Encoded,
                                cache);

                        var envTransform =
                            TransformPineExpressionWithOptionalReplacement(
                                findReplacement,
                                parseAndEval.Environment,
                                cache);

                        var referencesOriginalEnv =
                            encodedTransform.referencesOriginalEnv ||
                            envTransform.referencesOriginalEnv;

                        if (encodedTransform.expr == parseAndEval.Encoded &&
                            envTransform.expr == parseAndEval.Environment)
                        {
                            return (parseAndEval, referencesOriginalEnv);
                        }

                        return
                            (new Expression.ParseAndEval
                            (
                                encoded: encodedTransform.expr,
                                environment: envTransform.expr
                            ),
                            referencesOriginalEnv);
                    }

                case Expression.KernelApplication kernelApp:
                    {
                        var argumentTransform =
                            TransformPineExpressionWithOptionalReplacement(
                                findReplacement,
                                kernelApp.Input,
                                cache);

                        if (argumentTransform.expr == kernelApp.Input)
                        {
                            return (kernelApp, argumentTransform.referencesOriginalEnv);
                        }

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
                                conditional.Condition,
                                cache);

                        var trueBranchTransform =
                            TransformPineExpressionWithOptionalReplacement(
                                findReplacement,
                                conditional.TrueBranch,
                                cache);

                        var falseBranchTransform =
                            TransformPineExpressionWithOptionalReplacement(
                                findReplacement,
                                conditional.FalseBranch,
                                cache);

                        var referencesOriginalEnv =
                            conditionTransform.referencesOriginalEnv ||
                            falseBranchTransform.referencesOriginalEnv ||
                            trueBranchTransform.referencesOriginalEnv;

                        if (conditionTransform.expr == conditional.Condition &&
                            trueBranchTransform.expr == conditional.TrueBranch &&
                            falseBranchTransform.expr == conditional.FalseBranch)
                        {
                            return (conditional, referencesOriginalEnv);
                        }

                        return
                            (Expression.ConditionalInstance
                            (
                                condition: conditionTransform.expr,
                                falseBranch: falseBranchTransform.expr,
                                trueBranch: trueBranchTransform.expr
                                ),
                            referencesOriginalEnv);
                    }

                case Expression.Environment:
                    return (expression, true);

                case Expression.StringTag stringTagExpr:
                    {
                        var taggedTransform =
                            TransformPineExpressionWithOptionalReplacement(
                                findReplacement,
                                stringTagExpr.Tagged,
                                cache);

                        if (taggedTransform.expr == stringTagExpr.Tagged)
                        {
                            return
                                (stringTagExpr, taggedTransform.referencesOriginalEnv);
                        }

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

        var result = LessCache();

        cache[expression] = result;

        return result;
    }

    /// <summary>
    /// Recursively searches for reductions up to a maximum depth.
    /// Applies <see cref="SearchForExpressionReduction"/> repeatedly until no change occurs or the depth limit is reached.
    /// </summary>
    /// <param name="maxDepth">Maximum number of recursive reduction rounds.</param>
    /// <param name="expression">The expression to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="envConstraintId">Optional environment constraints to enable reductions.</param>
    /// <param name="dontReduceExpression">Predicate to exclude certain expressions from reduction.</param>
    /// <returns>The possibly reduced expression.</returns>
    public static Expression SearchForExpressionReductionRecursive(
        int maxDepth,
        Expression expression,
        PineVMParseCache parseCache,
        PineValueClass? envConstraintId = null,
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

                    return SearchForExpressionReduction(expr, envConstraintId, parseCache);
                }, expression).expr;

        if (transformed == expression)
            return transformed;

        return
            SearchForExpressionReductionRecursive(
                maxDepth - 1,
                transformed,
                parseCache,
                envConstraintId: envConstraintId,
                dontReduceExpression);
    }

    /// <summary>
    /// Performs a single bottom-up reduction pass:
    ///   1. Recursively reduces children first.
    ///   2. Attempts to reduce the resulting node via <see cref="SearchForExpressionReduction"/>.
    /// </summary>
    /// <param name="expression">The expression to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="dontReduceExpression">Predicate to exclude certain expressions from reduction.</param>
    /// <returns>The reduced expression if any reduction was possible; otherwise the original expression.</returns>
    public static Expression ReduceExpressionBottomUp(
        Expression expression,
        PineVMParseCache parseCache,
        Func<Expression, bool>? dontReduceExpression = null)
    {
        // First, reduce the sub-expressions.
        var expressionWithReducedChildren =
            expression switch
            {
                Expression.Literal => expression,

                Expression.List listExpr =>
                    ReduceListExpressionBottomUp(
                        listExpr,
                        parseCache,
                        dontReduceExpression),

                Expression.KernelApplication kernelApp =>
                    ReduceKernelApplicationBottomUp(
                        kernelApp,
                        parseCache,
                        dontReduceExpression),

                Expression.ParseAndEval parseAndEval =>
                    ReduceParseAndEvalBottomUp(
                        parseAndEval,
                        parseCache,
                        dontReduceExpression),

                Expression.Conditional conditional =>
                    ReduceConditionalBottomUp(
                        conditional,
                        parseCache,
                        dontReduceExpression),

                Expression.StringTag stringTag =>
                    ReduceStringTagBottomUp(
                        stringTag,
                        parseCache,
                        dontReduceExpression),

                // These are direct references to the environment or stack.
                // No further children to reduce.
                Expression.Environment => expression,

                _ =>
                throw new NotImplementedException(
                    $"Expression type not implemented: {expression.GetType().FullName}")
            };

        // Next, try to reduce this node itself unless the caller forbids it.
        if (!(dontReduceExpression?.Invoke(expressionWithReducedChildren) ?? false))
        {
            // If we can reduce further, return the reduced expression:
            var reduced = SearchForExpressionReduction(expressionWithReducedChildren, envConstraintId: null, parseCache);

            if (reduced is not null)
                return reduced;
        }

        // If no further reduction, just return our (possibly child-reduced) node.
        return expressionWithReducedChildren;
    }

    /// <summary>
    /// Reduces a list expression bottom-up by first reducing each item, then attempting a local reduction.
    /// </summary>
    /// <param name="listExpr">The list expression to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="dontReduceExpression">Predicate to exclude certain expressions from reduction.</param>
    /// <returns>The reduced list expression or the original if no change occurs.</returns>
    public static Expression ReduceListExpressionBottomUp(
        Expression.List listExpr,
        PineVMParseCache parseCache,
        Func<Expression, bool>? dontReduceExpression)
    {
        var items = listExpr.Items;
        var changed = false;
        var newItems = new Expression[items.Count];

        for (var i = 0; i < items.Count; i++)
        {
            var reducedChild = ReduceExpressionBottomUp(items[i], parseCache, dontReduceExpression);

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

    /// <summary>
    /// Reduces a kernel application bottom-up by first reducing its input, then attempting a local reduction.
    /// </summary>
    /// <param name="kernelApp">The kernel application to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="dontReduceExpression">Predicate to exclude certain expressions from reduction.</param>
    /// <returns>The reduced expression or the original if no change occurs.</returns>
    public static Expression ReduceKernelApplicationBottomUp(
        Expression.KernelApplication kernelApp,
        PineVMParseCache parseCache,
        Func<Expression, bool>? dontReduceExpression)
    {
        var reducedArg =
            ReduceExpressionBottomUp(kernelApp.Input, parseCache, dontReduceExpression);

        if (reducedArg == kernelApp.Input)
            return kernelApp;

        return new Expression.KernelApplication(kernelApp.Function, reducedArg);
    }

    /// <summary>
    /// Reduces a Parse-and-Eval expression bottom-up by reducing both the encoded expression and the environment.
    /// </summary>
    /// <param name="parseAndEval">The Parse-and-Eval expression to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="dontReduceExpression">Predicate to exclude certain expressions from reduction.</param>
    /// <returns>The reduced expression or the original if no change occurs.</returns>
    public static Expression ReduceParseAndEvalBottomUp(
        Expression.ParseAndEval parseAndEval,
        PineVMParseCache parseCache,
        Func<Expression, bool>? dontReduceExpression)
    {
        var reducedEncoded =
            ReduceExpressionBottomUp(parseAndEval.Encoded, parseCache, dontReduceExpression);

        var reducedEnv =
            ReduceExpressionBottomUp(parseAndEval.Environment, parseCache, dontReduceExpression);

        if (reducedEncoded == parseAndEval.Encoded &&
            reducedEnv == parseAndEval.Environment)
        {
            return parseAndEval;
        }

        return new Expression.ParseAndEval(encoded: reducedEncoded, environment: reducedEnv);
    }

    /// <summary>
    /// Reduces a conditional expression bottom-up by reducing its components and then attempting a local reduction.
    /// </summary>
    /// <param name="conditional">The conditional expression to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="dontReduceExpression">Predicate to exclude certain expressions from reduction.</param>
    /// <returns>The reduced expression or the original if no change occurs.</returns>
    public static Expression ReduceConditionalBottomUp(
        Expression.Conditional conditional,
        PineVMParseCache parseCache,
        Func<Expression, bool>? dontReduceExpression)
    {
        var reducedCondition =
            ReduceExpressionBottomUp(
                conditional.Condition,
                parseCache,
                dontReduceExpression);

        var reducedTrue =
            ReduceExpressionBottomUp(
                conditional.TrueBranch,
                parseCache,
                dontReduceExpression);

        var reducedFalse =
            ReduceExpressionBottomUp(
                conditional.FalseBranch,
                parseCache,
                dontReduceExpression);

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

    /// <summary>
    /// Reduces a string-tagged expression bottom-up by reducing the tagged subexpression.
    /// </summary>
    /// <param name="stringTag">The string tag expression to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="dontReduceExpression">Predicate to exclude certain expressions from reduction.</param>
    /// <returns>The reduced expression or the original if no change occurs.</returns>
    public static Expression ReduceStringTagBottomUp(
        Expression.StringTag stringTag,
        PineVMParseCache parseCache,
        Func<Expression, bool>? dontReduceExpression)
    {
        var reducedTagged =
            ReduceExpressionBottomUp(
                stringTag.Tagged,
                parseCache,
                dontReduceExpression);

        if (reducedTagged == stringTag.Tagged)
            return stringTag;

        return new Expression.StringTag(stringTag.Tag, reducedTagged);
    }

    /// <summary>
    /// Attempts to push a reverse operation into the structure of an expression without evaluation.
    /// Supports reversing concrete lists and list-literals, cancels double-reverse kernel applications,
    /// and propagates into both branches of conditionals and into string-tagged subexpressions.
    /// </summary>
    /// <param name="expression">The expression representing a list whose result should be reversed.</param>
    /// <returns>
    /// A structurally transformed expression equivalent to applying <c>reverse</c>,
    /// or <c>null</c> if no structural reduction is applicable.
    /// </returns>
    public static Expression? ApplyKernelFunctionReverseToAllBranches(
        Expression expression)
    {
        switch (expression)
        {
            case Expression.List listExpr:
                {
                    if (listExpr.Items.Count <= 1)
                        return listExpr;

                    var reversed = listExpr.Items.Reverse().ToArray();

                    return Expression.ListInstance(reversed);
                }

            case Expression.Literal literal:
                return Expression.LiteralInstance(KernelFunction.reverse(literal.Value));

            case Expression.KernelApplication innerKernelApp:
                {
                    if (innerKernelApp.Function is nameof(KernelFunction.reverse))
                    {
                        return innerKernelApp.Input;
                    }

                    return null;
                }

            case Expression.Conditional cond:
                {
                    if (ApplyKernelFunctionReverseToAllBranches(cond.FalseBranch) is { } falseOk &&
                       ApplyKernelFunctionReverseToAllBranches(cond.TrueBranch) is { } trueOk)
                    {
                        return
                            Expression.ConditionalInstance(
                                cond.Condition,
                                falseBranch: falseOk,
                                trueBranch: trueOk);
                    }

                    return null;
                }

            case Expression.StringTag tag:
                {
                    if (ApplyKernelFunctionReverseToAllBranches(tag.Tagged) is { } inner)
                    {
                        return new Expression.StringTag(tag.Tag, inner);
                    }

                    return null;
                }
        }

        return null;
    }

    /// <summary>
    /// Attempts to push a skip operation into the expression tree without evaluation.
    /// Supports concrete lists, list-literals, nested <c>skip</c> kernel applications (which are combined),
    /// and propagation into both branches of a conditional and into string-tagged expressions.
    /// </summary>
    /// <param name="count">Number of elements to skip. Negative values are treated as zero.</param>
    /// <param name="expression">The source expression from which to skip elements.</param>
    /// <returns>
    /// A structurally transformed expression that represents the skip, or <c>null</c>
    /// if the transformation cannot be applied safely.
    /// </returns>
    public static Expression? ApplyKernelFunctionSkipToAllBranches(
        int count,
        Expression expression)
    {
        var countClamped =
            count < 0 ? 0 : count;

        if (countClamped is 0)
            return expression;

        switch (expression)
        {
            case Expression.List list:
                {
                    if (countClamped <= 0)
                        return list;

                    if (countClamped >= list.Items.Count)
                        return Expression.LiteralInstance(PineValue.EmptyList);

                    return Expression.ListInstance([.. list.Items.Skip(countClamped)]);
                }

            case Expression.Literal literal:
                return Expression.LiteralInstance(KernelFunctionSpecialized.skip(countClamped, literal.Value));

            case Expression.KernelApplication innerSkip:
                {
                    if (innerSkip.Function is nameof(KernelFunction.skip) &&
                        innerSkip.Input is Expression.List args && args.Items.Count is 2 &&
                        args.Items[0] is Expression.Literal litCount && KernelFunction.SignedIntegerFromValueRelaxed(litCount.Value) is { } innerCount)
                    {
                        var innerCountClamped =
                            innerCount < 0 ? 0 : (int)innerCount;

                        return
                            ApplyKernelFunctionSkipToAllBranches(countClamped + innerCountClamped, args.Items[1]);
                    }

                    return null;
                }

            case Expression.Conditional conditional:
                {
                    if (ApplyKernelFunctionSkipToAllBranches(countClamped, conditional.FalseBranch) is { } falseOk &&
                        ApplyKernelFunctionSkipToAllBranches(countClamped, conditional.TrueBranch) is { } trueOk)
                    {
                        return
                            Expression.ConditionalInstance(
                                conditional.Condition,
                                falseBranch: falseOk,
                                trueBranch: trueOk);
                    }

                    return null;
                }

            case Expression.StringTag tag:
                {
                    if (ApplyKernelFunctionSkipToAllBranches(countClamped, tag.Tagged) is { } taggedOk)
                    {
                        return new Expression.StringTag(tag.Tag, taggedOk);
                    }

                    return null;
                }
        }

        return null;
    }

    /// <summary>
    /// Attempts to push a take operation into the expression tree without evaluation.
    /// Supports concrete lists, list-literals, nested <c>take</c> kernel applications (which are combined),
    /// and propagation into both branches of a conditional and into string-tagged expressions.
    /// </summary>
    /// <param name="count">Number of elements to take. Negative values are treated as zero.</param>
    /// <param name="expression">The source expression from which to take elements.</param>
    /// <returns>
    /// A structurally transformed expression that represents the take, or <c>null</c>
    /// if the transformation cannot be applied safely.
    /// </returns>
    public static Expression? ApplyKernelFunctionTakeToAllBranches(
        int count,
        Expression expression)
    {
        var countClamped =
            count < 0 ? 0 : count;

        switch (expression)
        {
            case Expression.List list:
                {
                    if (countClamped <= 0)
                        return Expression.LiteralInstance(PineValue.EmptyList);

                    if (countClamped >= list.Items.Count)
                        return list;

                    return Expression.ListInstance([.. list.Items.Take(countClamped)]);
                }

            case Expression.Literal literal:
                return Expression.LiteralInstance(KernelFunctionSpecialized.take(countClamped, literal.Value));

            case Expression.KernelApplication innerTake:

                {
                    if (innerTake.Function is nameof(KernelFunction.take) &&
                        innerTake.Input is Expression.List args && args.Items.Count is 2 &&
                        args.Items[0] is Expression.Literal litCount &&
                        KernelFunction.SignedIntegerFromValueRelaxed(litCount.Value) is { } innerCount)
                    {
                        var innerCountClamped =
                            innerCount < 0 ? 0 : (int)innerCount;

                        return
                            ApplyKernelFunctionTakeToAllBranches(
                                countClamped + innerCountClamped,
                                args.Items[1]);
                    }

                    return null;
                }

            case Expression.Conditional conditional:
                {
                    if (ApplyKernelFunctionTakeToAllBranches(countClamped, conditional.FalseBranch) is { } falseOk &&
                        ApplyKernelFunctionTakeToAllBranches(countClamped, conditional.TrueBranch) is { } trueOk)
                    {
                        return
                            Expression.ConditionalInstance(
                                conditional.Condition,
                                falseBranch: falseOk,
                                trueBranch: trueOk);
                    }

                    return null;
                }

            case Expression.StringTag tag:
                {
                    if (ApplyKernelFunctionTakeToAllBranches(countClamped, tag.Tagged) is { } taggedOk)
                    {
                        return new Expression.StringTag(tag.Tag, taggedOk);
                    }

                    return null;
                }
        }

        return null;
    }

    /// <summary>
    /// Attempts to push a head operation (select first element) into the expression tree without evaluation.
    /// If the input is a concrete list or list-literal, returns the first element or an empty list when out of bounds.
    /// Propagates into both branches of a conditional and into string-tagged expressions when possible.
    /// </summary>
    /// <param name="expression">The source expression representing a list from which to select the head.</param>
    /// <returns>
    /// A structurally transformed expression representing the head selection, or <c>null</c>
    /// if no structural reduction is applicable.
    /// </returns>
    public static Expression? ApplyKernelFunctionHeadToAllBranches(Expression expression)
    {
        if (TryReduceSelectListItem(expression, 0) is { } selected)
        {
            return selected;
        }

        switch (expression)
        {
            case Expression.Conditional cond:
                {
                    if (ApplyKernelFunctionHeadToAllBranches(cond.FalseBranch) is { } falseOk &&
                        ApplyKernelFunctionHeadToAllBranches(cond.TrueBranch) is { } trueOk)
                    {
                        return Expression.ConditionalInstance(condition: cond.Condition, falseBranch: falseOk, trueBranch: trueOk);
                    }

                    return null;
                }

            case Expression.StringTag tag:
                {
                    if (ApplyKernelFunctionHeadToAllBranches(tag.Tagged) is { } taggedOk)
                    {
                        return new Expression.StringTag(tag.Tag, taggedOk);
                    }

                    return null;
                }
        }

        return null;
    }

    /// <summary>
    /// Attempts to select the list item at a given index from the provided expression without
    /// evaluating with a runtime environment. Supports:
    /// - Expression.List: returns the item or EmptyList if out of bounds.
    /// - Expression.Literal(list): returns a literal of the item or EmptyList if out of bounds.
    /// - Expression.Conditional: pushes the selection into both branches when possible.
    /// - Expression.StringTag: unwraps and attempts on the tagged expression.
    /// Returns null if no structural reduction can be applied.
    /// </summary>
    public static Expression? TryReduceSelectListItem(Expression inputExpression, int index)
    {
        if (index < 0) index = 0;

        switch (inputExpression)
        {
            case Expression.List list:
                {
                    if (index < list.Items.Count)
                    {
                        return list.Items[index];
                    }

                    return Expression.LiteralInstance(PineValue.EmptyList);
                }

            case Expression.Literal lit:
                {
                    if (lit.Value is PineValue.ListValue lv)
                    {
                        if (index < lv.Items.Length)
                        {
                            return Expression.LiteralInstance(lv.Items.Span[index]);
                        }

                        return Expression.LiteralInstance(PineValue.EmptyList);
                    }

                    // Not a list-literal: no structural reduction.
                    return null;
                }

            case Expression.Conditional cond:
                {
                    var falseOut = TryReduceSelectListItem(cond.FalseBranch, index);
                    var trueOut = TryReduceSelectListItem(cond.TrueBranch, index);

                    if (falseOut is not null && trueOut is not null)
                    {
                        return Expression.ConditionalInstance(
                            condition: cond.Condition,
                            falseBranch: falseOut,
                            trueBranch: trueOut);
                    }

                    return null;
                }

            case Expression.StringTag tag:
                {
                    return TryReduceSelectListItem(tag.Tagged, index);
                }
        }

        return null;
    }
}

