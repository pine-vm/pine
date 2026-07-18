using Pine.Core.CommonEncodings;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;

using BuiltinFunctionSpecialized = Pine.Core.Internal.BuiltinFunctionSpecialized;

namespace Pine.Core.CodeAnalysis;

/// <summary>
/// Configures optional behaviors of the bottom-up expression reducer
/// (<see cref="ReducePineExpression.ReduceExpressionBottomUp(Expression, ReductionConfig, PineVMParseCache, IDictionary{ValueTuple{Expression, ReductionConfig}, Expression}?)"/>).
/// <para>
/// The configuration also participates in the cache key for the optional
/// <c>reducedExpressionCache</c>, ensuring entries reduced under different
/// configurations do not collide.
/// </para>
/// </summary>
/// <param name="DisableGenericApplicationChainConsolidation">When <c>true</c>,
/// suppresses
/// <see cref="ReducePineExpression.TryConsolidateGenericFunctionApplicationChain(Expression.Eval, PineVMParseCache, ReductionConfig)"/>
/// during reduction. Intended for diagnostic / inspection scenarios where the
/// caller wants to observe the pre-consolidation behavior side-by-side with the
/// optimized one.</param>
/// <param name="InlineEvalEnvironmentSizeLimit">Specifies the maximum size of the environment
/// for which <see cref="ReducePineExpression.TryInlineEvalBottomUp(Expression.Eval, ReductionConfig, PineVMParseCache, IDictionary{ValueTuple{Expression, ReductionConfig}, Expression}?)"/>
/// will attempt to inline <see cref="Expression.Eval"/> nodes. Intended for diagnostic /
/// inspection scenarios where the caller wants to observe the
/// pre-inlining shape of the expression tree.</param>
/// during reduction, leaving <see cref="Expression.Eval"/> nodes whose
/// encoded operand is a known constant un-inlined. Intended for diagnostic /
/// inspection scenarios where the caller wants to observe the
/// pre-inlining shape of the expression tree.</param>
public record struct ReductionConfig(
    bool DisableGenericApplicationChainConsolidation,
    int InlineEvalEnvironmentSizeLimit)
{
    /// <summary>
    /// The default configuration: all optional behaviors enabled (no suppression flags set).
    /// </summary>
    public static readonly ReductionConfig Default =
        new(
            DisableGenericApplicationChainConsolidation: false,
            InlineEvalEnvironmentSizeLimit: 10_000);
}

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

            Expression.Litral literal =>
            Result<string, PineValue>.ok(literal.Value),

            Expression.List list =>
            TryEvaluateExpressionIndependent(list, parseCache),

            Expression.Builtin kernelApplication =>
            TryEvaluateExpressionIndependent(kernelApplication, parseCache),

            Expression.Eval parseAndEvalExpr =>
            TryEvaluateExpressionIndependent(parseAndEvalExpr, parseCache),

            Expression.Conditional conditional =>
            TryEvaluateExpressionIndependent(conditional, parseCache),

            Expression.Label stringTag =>
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
        Expression.Eval parseAndEvalExpr,
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
    /// The input is first evaluated, then dispatched to <see cref="BuiltinFunction.ApplyFunctionGeneric(string, PineValue)"/>.
    /// </summary>
    /// <param name="kernelApplication">The kernel application expression.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <returns>The computed value, or an error message if evaluation fails.</returns>
    public static Result<string, PineValue> TryEvaluateExpressionIndependent(
        Expression.Builtin kernelApplication,
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

        return BuiltinFunction.ApplyFunctionGeneric(kernelApplication.Function, inputOk);
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
        if (expression is Expression.Litral)
            return null;

        if (CodeAnalysis.TryParseAsLiteral(expression) is { } literalValue)
        {
            return Expression.LitralInst(literalValue);
        }

        if (envConstraintId is not null &&
            CodeAnalysis.TryParseExprAsPathInEnv(expression) is { } parsedAsPath)
        {
            if (envConstraintId.TryGetValue(parsedAsPath) is { } fromEnvConstraint)
            {
                return Expression.LitralInst(fromEnvConstraint);
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
                    return Expression.LitralInst(okValue);
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
            case Expression.Builtin rootKernelApp:

                Expression.Builtin ContinueWithReducedInput(Expression newInput) =>
                    Expression.BuiltinInst(
                        function: rootKernelApp.Function,
                        input: newInput);

                switch (rootKernelApp.Function)
                {
                    case nameof(BuiltinFunction.equal):
                        {
                            if (rootKernelApp.Input is Expression.List inputList)
                            {
                                if (envConstraintId is not null)
                                {
                                    var reducedArgumentsList =
                                        inputList.Items
                                        .Select(
                                            origArg => SearchForExpressionReductionRecursive(
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

                                        if (item is Expression.Litral literal)
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
                                                return Expression.LitralInst(PineKernelValues.FalseValue);
                                            }
                                        }
                                    }

                                    var listLengthLowerBound =
                                        listLengthLowerBounds.Count is 0
                                        ?
                                        (int?)null
                                        :
                                        listLengthLowerBounds.Max();

                                    int? prevItemFixedLength = null;

                                    foreach (var item in inputList.Items)
                                    {
                                        int? itemFixedLength = null;

                                        if (item is Expression.Litral equalArgLiteral)
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
                                                (prevItemFixedLength.HasValue &&
                                                itemFixedLength.Value != prevItemFixedLength.Value))
                                            {
                                                return Expression.LitralInst(PineKernelValues.FalseValue);
                                            }

                                            prevItemFixedLength = itemFixedLength;
                                        }
                                    }
                                }
                            }

                            return AttemptReduceViaEval();
                        }

                    case nameof(BuiltinFunction.head):
                        {
                            if (ApplyKernelFunctionHeadToAllBranches(rootKernelApp.Input) is { } reducedBranches)
                                return reducedBranches;

                            return AttemptReduceViaEval();
                        }

                    case nameof(BuiltinFunction.skip):
                        {
                            if (rootKernelApp.Input is Expression.List inputList && inputList.Items.Count is 2)
                            {
                                var countExpr = inputList.Items[0];
                                var seqExpr = inputList.Items[1];

                                if (TryEvaluateExpressionIndependent(countExpr, parseCache).IsOkOrNull() is { } okSkipCountValue &&
                                    BuiltinFunction.SignedIntegerFromValueRelaxed(okSkipCountValue) is { } okSkipCount)
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

                    case nameof(BuiltinFunction.take):
                        {
                            if (rootKernelApp.Input is Expression.List takeInput && takeInput.Items.Count is 2)
                            {
                                var countExpr = takeInput.Items[0];
                                var srcExpr = takeInput.Items[1];

                                if (TryEvaluateExpressionIndependent(countExpr, parseCache).IsOkOrNull() is { } okTakeCountValue &&
                                    BuiltinFunction.SignedIntegerFromValueRelaxed(okTakeCountValue) is { } okTakeCount)
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

                    case nameof(BuiltinFunction.reverse):
                        {
                            if (ApplyKernelFunctionReverseToAllBranches(rootKernelApp.Input) is { } reducedRev)
                            {
                                return reducedRev;
                            }

                            return AttemptReduceViaEval();
                        }

                    case nameof(BuiltinFunction.concat):
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

                                        if (argItem is Expression.Litral argLiteral)
                                        {
                                            if (argLiteral.Value is PineValue.ListValue listValue &&
                                                listValue.Items.Length is 0)
                                                continue;
                                        }

                                        nonEmptyItems.Add(argItem);
                                    }

                                    if (nonEmptyItems.Count < inputList.Items.Count)
                                    {
                                        if (nonEmptyItems.Count is 0)
                                        {
                                            return Expression.LitralInst(PineValue.EmptyList);
                                        }

                                        if (nonEmptyItems.Count is 1)
                                        {
                                            return nonEmptyItems[0];
                                        }

                                        return
                                            ContinueWithReducedInput(Expression.ListInst(nonEmptyItems));
                                    }
                                }

                                var items = new List<Expression>();

                                foreach (var argument in inputList.Items)
                                {
                                    if (argument is not Expression.List subList)
                                    {
                                        if (argument is Expression.Litral subLiteral &&
                                            subLiteral.Value is PineValue.ListValue subLiteralList)
                                        {
                                            for (var i = 0; i < subLiteralList.Items.Length; i++)
                                            {
                                                items.Add(Expression.LitralInst(subLiteralList.Items.Span[i]));
                                            }

                                            continue;
                                        }

                                        return AttemptReduceViaEval();
                                    }

                                    items.AddRange(subList.Items);
                                }

                                return Expression.ListInst(items);
                            }

                            return AttemptReduceViaEval();
                        }

                    case nameof(BuiltinFunction.length):
                        {
                            if (rootKernelApp.Input is Expression.List inputList)
                            {
                                return
                                    Expression.LitralInst(
                                        IntegerEncoding.EncodeSignedInteger(inputList.Items.Count));
                            }

                            if (rootKernelApp.Input is Expression.Builtin lengthInputKernelApp)
                            {
                                if (lengthInputKernelApp.Function is nameof(BuiltinFunction.concat) &&
                                    lengthInputKernelApp.Input is Expression.List lengthConcatList)
                                {
                                    int? aggregateLength = 0;

                                    for (var i = 0; i < lengthConcatList.Items.Count; i++)
                                    {
                                        var lengthConcatListItem = lengthConcatList.Items[i];

                                        {
                                            if (lengthConcatListItem is Expression.Litral lengthConcatListItemLiteral &&
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
                                            Expression.LitralInst(
                                                IntegerEncoding.EncodeSignedInteger(aggregateLength.Value));
                                    }
                                }
                            }

                            return AttemptReduceViaEval();
                        }

                    case nameof(BuiltinFunction.int_add):
                        {
                            if (rootKernelApp.Input is Expression.List addInputList)
                            {
                                var reducedKernelApplication =
                                    ReduceFlattenedIntegerKernelApplication(
                                        nameof(BuiltinFunction.int_add),
                                        addInputList.Items,
                                        parseCache);

                                if (reducedKernelApplication is not null)
                                {
                                    return reducedKernelApplication;
                                }
                            }

                            return AttemptReduceViaEval();
                        }

                    case nameof(BuiltinFunction.int_mul):
                        {
                            if (rootKernelApp.Input is Expression.List mulInputList)
                            {
                                var reducedKernelApplication =
                                    ReduceFlattenedIntegerKernelApplication(
                                        nameof(BuiltinFunction.int_mul),
                                        mulInputList.Items,
                                        parseCache);

                                if (reducedKernelApplication is not null)
                                {
                                    return reducedKernelApplication;
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

                    if (conditional.Condition is Expression.Builtin
                        {
                            Function: "equal",
                            Input: Expression.List equalArgsList
                        }
                        && equalArgsList.Items.Count is 2)
                    {
                        if (equalArgsList.Items[1] is Expression.Litral { Value: var val1 })
                        {
                            if (val1 == PineKernelValues.FalseValue &&
                                IsKnownBooleanExpression(equalArgsList.Items[0]))
                            {
                                return
                                    Expression.ConditionalInst(
                                        condition: equalArgsList.Items[0],
                                        trueBranch: conditional.FalseBranch,
                                        falseBranch: conditional.TrueBranch);

                            }

                            if (val1 == PineKernelValues.TrueValue &&
                                IsKnownBooleanExpression(equalArgsList.Items[0]))
                            {
                                return
                                    Expression.ConditionalInst(
                                        condition: equalArgsList.Items[0],
                                        falseBranch: conditional.FalseBranch,
                                        trueBranch: conditional.TrueBranch);
                            }
                        }

                        if (equalArgsList.Items[0] is Expression.Litral { Value: var val0 })
                        {
                            if (val0 == PineKernelValues.FalseValue &&
                                IsKnownBooleanExpression(equalArgsList.Items[1]))
                            {
                                return
                                    Expression.ConditionalInst(
                                        condition: equalArgsList.Items[1],
                                        trueBranch: conditional.FalseBranch,
                                        falseBranch: conditional.TrueBranch);
                            }

                            if (val0 == PineKernelValues.TrueValue &&
                                IsKnownBooleanExpression(equalArgsList.Items[1]))
                            {
                                return
                                    Expression.ConditionalInst(
                                        condition: equalArgsList.Items[1],
                                        falseBranch: conditional.FalseBranch,
                                        trueBranch: conditional.TrueBranch);
                            }
                        }
                    }

                    return AttemptReduceViaEval();
                }

            default:
                return AttemptReduceViaEval();
        }
    }


    /// <summary>
    /// Returns <c>true</c> when <paramref name="expression"/> is statically
    /// known to evaluate to a Pine kernel boolean value
    /// (<see cref="PineKernelValues.TrueValue"/> or
    /// <see cref="PineKernelValues.FalseValue"/>).
    /// <para>
    /// This is conservative: it only returns <c>true</c> when the expression
    /// is one of a small set of forms guaranteed to produce a boolean.
    /// It is used to gate optimizations that would otherwise be unsound for
    /// non-boolean values whose byte representation collides with that of
    /// <see cref="PineKernelValues.TrueValue"/> (<c>Blob([4])</c>) or
    /// <see cref="PineKernelValues.FalseValue"/> (<c>Blob([2])</c>).
    /// In particular, the byte sequence <c>[0x02]</c> arises from
    /// <c>Pine_kernel.skip [ 1, 2 ]</c> and
    /// <c>[0x04]</c> from <c>Pine_kernel.skip [ 1, 4 ]</c>, neither of which
    /// is intended to be interpreted as a boolean.
    /// </para>
    /// </summary>
    /// <param name="expression">The expression to inspect.</param>
    /// <returns><c>true</c> if the expression is provably boolean; otherwise <c>false</c>.</returns>
    public static bool IsKnownBooleanExpression(Expression expression)
    {
        return expression switch
        {
            Expression.Litral literal =>
            literal.Value == PineKernelValues.TrueValue ||
            literal.Value == PineKernelValues.FalseValue,

            Expression.Builtin kernelApp =>
            kernelApp.Function switch
            {
                nameof(BuiltinFunction.equal) => true,
                nameof(BuiltinFunction.int_is_sorted_asc) => true,

                _ =>
                false,
            },

            Expression.Conditional conditional =>
            IsKnownBooleanExpression(conditional.TrueBranch) &&
            IsKnownBooleanExpression(conditional.FalseBranch),

            Expression.Label stringTag =>
            IsKnownBooleanExpression(stringTag.Tagged),

            _ =>
            false,
        };
    }

    /// <summary>
    /// For example, <c>int_add([int_add([a, b]), c])</c> becomes <c>[a, b, c]</c>.
    /// Returns <c>null</c> if no flattening was possible (no nested same-function applications found).
    /// </summary>
    private static IReadOnlyList<Expression>? FlattenNestedKernelApplication(
        string functionName,
        IReadOnlyList<Expression> items)
    {
        var anyFlattened = false;
        var flattened = new List<Expression>(capacity: items.Count);

        for (var i = 0; i < items.Count; i++)
        {
            var item = items[i];

            if (item is Expression.Builtin innerKernel &&
                innerKernel.Function == functionName &&
                innerKernel.Input is Expression.List innerList)
            {
                anyFlattened = true;

                for (var j = 0; j < innerList.Items.Count; j++)
                {
                    flattened.Add(innerList.Items[j]);
                }
            }
            else
            {
                flattened.Add(item);
            }
        }

        return anyFlattened ? flattened : null;
    }

    private static Expression? ReduceFlattenedIntegerKernelApplication(
        string functionName,
        IReadOnlyList<Expression> originalItems,
        PineVMParseCache parseCache)
    {
        var flattenedItems =
            FlattenNestedKernelApplication(functionName, originalItems);

        var reducedItems =
            flattenedItems ?? originalItems;

        var changed =
            flattenedItems is not null;

        var constants =
            CollectConstantIntegers(reducedItems);

        if (1 < constants.constants.Count)
        {
            var foldedConstant =
                functionName switch
                {
                    nameof(BuiltinFunction.int_add) => BigInteger.Zero,
                    nameof(BuiltinFunction.int_mul) => BigInteger.One,

                    _ =>
                    throw new NotSupportedException($"Unsupported integer kernel application: {functionName}")
                };

            foreach (var constant in constants.constants)
            {
                foldedConstant =
                    functionName switch
                    {
                        nameof(BuiltinFunction.int_add) => foldedConstant + constant,
                        nameof(BuiltinFunction.int_mul) => foldedConstant * constant,

                        _ =>
                        throw new NotSupportedException($"Unsupported integer kernel application: {functionName}")
                    };
            }

            reducedItems =
                [
                Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(foldedConstant)),
                .. constants.variables
                ];

            changed = true;
        }

        if (!changed)
        {
            return null;
        }

        var reducedExpr =
            Expression.BuiltinInst(
                functionName,
                Expression.ListInst(reducedItems));

        if (!reducedExpr.ReferencesEnvironment)
        {
            if (TryEvaluateExpressionIndependent(reducedExpr, parseCache).IsOkOrNull() is { } okValue)
            {
                return Expression.LitralInst(okValue);
            }
        }

        return reducedExpr;
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
        if (expression is Expression.Litral literalExpr)
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

        if (CodeAnalysis.TryParseAsLiteral(expression) is { } literal)
        {
            if (literal is PineValue.ListValue literalList)
            {
                yield return (literalList.Items.Length, literalList.Items.Length);
            }
        }

        if (CodeAnalysis.TryParseExprAsPathInEnv(expression) is { } itemPath)
        {
            var itemConstraint = envConstraintId.PartUnderPath(itemPath);

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

        if (expression is Expression.Builtin kernelApp)
        {
            if (kernelApp.Function is nameof(BuiltinFunction.skip) &&
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
                case Expression.Litral:
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

                        return (Expression.ListInst(mappedItems), referencesOriginalEnv);
                    }

                case Expression.Eval parseAndEval:
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
                            (new Expression.Eval(
                                encoded: encodedTransform.expr,
                                environment: envTransform.expr),
                            referencesOriginalEnv);
                    }

                case Expression.Builtin kernelApp:
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
                            (Expression.BuiltinInst(
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
                            (Expression.ConditionalInst
                            (
                                condition: conditionTransform.expr,
                                falseBranch: falseBranchTransform.expr,
                                trueBranch: trueBranchTransform.expr),
                            referencesOriginalEnv);
                    }

                case Expression.Environment:
                    return (expression, true);

                case Expression.Label stringTagExpr:
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
                            (new Expression.Label(
                                stringTagExpr.Tag,
                                taggedTransform.expr),
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
    /// <returns>The possibly reduced expression.</returns>
    public static Expression SearchForExpressionReductionRecursive(
        int maxDepth,
        Expression expression,
        PineVMParseCache parseCache,
        PineValueClass? envConstraintId = null)
    {
        if (maxDepth < 1)
            return expression;

        var transformed =
            TransformPineExpressionWithOptionalReplacement(
                expr => SearchForExpressionReduction(expr, envConstraintId, parseCache),
                expression).expr;

        if (transformed == expression)
            return transformed;

        return
            SearchForExpressionReductionRecursive(
                maxDepth - 1,
                transformed,
                parseCache,
                envConstraintId: envConstraintId);
    }

    /// <summary>
    /// Performs a single bottom-up reduction pass with the default <see cref="ReductionConfig"/>:
    ///   1. Recursively reduces children first.
    ///   2. Attempts to reduce the resulting node via <see cref="SearchForExpressionReduction"/>.
    /// </summary>
    /// <param name="expression">The expression to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="reducedExpressionCache">Optional memoization cache keyed by
    /// <c>(expression, config)</c>. When supplied, lookups and inserts are scoped per
    /// <see cref="ReductionConfig"/> so entries from different configurations cannot collide.</param>
    /// <returns>The reduced expression if any reduction was possible; otherwise the original expression.</returns>
    public static Expression ReduceExpressionBottomUp(
        Expression expression,
        PineVMParseCache parseCache,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache = null)
    {
        return
            ReduceExpressionBottomUp(
                expression,
                ReductionConfig.Default,
                parseCache,
                reducedExpressionCache);
    }

    /// <summary>
    /// Performs a single bottom-up reduction pass:
    ///   1. Recursively reduces children first.
    ///   2. Attempts to reduce the resulting node via <see cref="SearchForExpressionReduction"/>.
    /// </summary>
    /// <param name="expression">The expression to reduce.</param>
    /// <param name="config">Configuration controlling optional reduction behaviors.
    /// Also forms part of the <paramref name="reducedExpressionCache"/> key.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="reducedExpressionCache">Optional memoization cache keyed by
    /// <c>(expression, config)</c>. When supplied, lookups and inserts are scoped per
    /// <see cref="ReductionConfig"/> so entries from different configurations cannot collide.</param>
    /// <returns>The reduced expression if any reduction was possible; otherwise the original expression.</returns>
    public static Expression ReduceExpressionBottomUp(
        Expression expression,
        ReductionConfig config,
        PineVMParseCache parseCache,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache = null)
    {
        if (reducedExpressionCache is not null &&
            reducedExpressionCache.TryGetValue((expression, config), out var cachedReducedExpression))
        {
            return cachedReducedExpression;
        }

        // First, reduce the sub-expressions.
        var expressionWithReducedChildren =
            expression switch
            {
                Expression.Litral => expression,

                Expression.List listExpr =>
                ReduceListExpressionBottomUp(
                    listExpr,
                    parseCache,
                    reducedExpressionCache,
                    config),

                Expression.Builtin kernelApp =>
                ReduceKernelApplicationBottomUp(
                    kernelApp,
                    parseCache,
                    reducedExpressionCache,
                    config),

                Expression.Eval parseAndEval =>
                ReduceParseAndEvalBottomUp(
                    parseAndEval,
                    parseCache,
                    reducedExpressionCache,
                    config),

                Expression.Conditional conditional =>
                ReduceConditionalBottomUp(
                    conditional,
                    parseCache,
                    reducedExpressionCache,
                    config),

                Expression.Label stringTag =>
                ReduceStringTagBottomUp(
                    stringTag,
                    parseCache,
                    reducedExpressionCache,
                    config),

                // These are direct references to the environment or stack.
                // No further children to reduce.
                Expression.Environment => expression,

                _ =>
                throw new NotImplementedException(
                    $"Expression type not implemented: {expression.GetType().FullName}")
            };

        // Next, try to reduce this node itself unless the caller forbids it.
        {
            // If we can reduce further, return the reduced expression:
            var reduced =
                SearchForExpressionReduction(expressionWithReducedChildren, envConstraintId: null, parseCache);

            if (reduced is not null)
            {
                reducedExpressionCache?[(expression, config)] = reduced;

                return reduced;
            }
        }

        // If no further reduction, just return our (possibly child-reduced) node.
        reducedExpressionCache?[(expression, config)] = expressionWithReducedChildren;

        return expressionWithReducedChildren;
    }

    /// <summary>
    /// Reduces a list expression bottom-up by first reducing each item, then attempting a local reduction.
    /// </summary>
    /// <param name="listExpr">The list expression to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="reducedExpressionCache">Optional memoization cache forwarded to recursive reductions.</param>
    /// <param name="config">Configuration controlling optional reduction behaviors.</param>
    /// <returns>The reduced list expression or the original if no change occurs.</returns>
    public static Expression ReduceListExpressionBottomUp(
        Expression.List listExpr,
        PineVMParseCache parseCache,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache,
        ReductionConfig config = default)
    {
        var items = listExpr.Items;
        var changed = false;
        var newItems = new Expression[items.Count];

        for (var i = 0; i < items.Count; i++)
        {
            var reducedChild =
                ReduceExpressionBottomUp(
                    items[i],
                    config,
                    parseCache,
                    reducedExpressionCache);

            newItems[i] = reducedChild;

            changed =
                changed || reducedChild != items[i];
        }

        // If none of the subexpressions changed, return the original; else build a new list.
        return
            changed
            ?
            Expression.ListInst(newItems)
            :
            listExpr;
    }

    /// <summary>
    /// Reduces a kernel application bottom-up by first reducing its input, then attempting a local reduction.
    /// </summary>
    /// <param name="kernelApp">The kernel application to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="reducedExpressionCache">Optional memoization cache forwarded to recursive reductions.</param>
    /// <param name="config">Configuration controlling optional reduction behaviors.</param>
    /// <returns>The reduced expression or the original if no change occurs.</returns>
    public static Expression ReduceKernelApplicationBottomUp(
        Expression.Builtin kernelApp,
        PineVMParseCache parseCache,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache,
        ReductionConfig config = default)
    {
        var reducedArg =
            ReduceExpressionBottomUp(
                kernelApp.Input,
                config,
                parseCache,
                reducedExpressionCache);

        if (kernelApp.Function is nameof(BuiltinFunction.int_mul) &&
            reducedArg is Expression.List operandList)
        {
            var constants = CollectConstantIntegers(operandList.Items);

            if (1 < constants.constants.Count)
            {
                var product = BigInteger.One;

                foreach (var c in constants.constants)
                {
                    product *= c;
                }

                var newItems =
                    new List<Expression>(capacity: constants.variables.Count + 1)
                    {
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(product))
                    };

                newItems.AddRange(constants.variables);

                reducedArg = Expression.ListInst(newItems);
            }
        }

        if (kernelApp.Function is nameof(BuiltinFunction.int_add) &&
            reducedArg is Expression.List addendList)
        {
            var constants = CollectConstantIntegers(addendList.Items);

            if (1 < constants.constants.Count)
            {
                var sum = BigInteger.Zero;

                foreach (var c in constants.constants)
                {
                    sum += c;
                }

                var newItems =
                    new List<Expression>(capacity: constants.variables.Count + 1)
                    {
                        Expression.LitralInst(IntegerEncoding.EncodeSignedInteger(sum))
                    };

                newItems.AddRange(constants.variables);

                reducedArg = Expression.ListInst(newItems);
            }
        }

        if (reducedArg == kernelApp.Input)
            return kernelApp;

        return Expression.BuiltinInst(kernelApp.Function, reducedArg);
    }

    private static (IReadOnlyList<BigInteger> constants, IReadOnlyList<Expression> variables) CollectConstantIntegers(
        IReadOnlyList<Expression> items)
    {
        var constants = new List<BigInteger>();
        var variableExpressions = new List<Expression>();

        for (var i = 0; i < items.Count; i++)
        {
            if (items[i] is Expression.Litral literal &&
                BuiltinFunction.SignedIntegerFromValueRelaxed(literal.Value) is { } intValue)
            {
                constants.Add(intValue);
            }
            else
            {
                variableExpressions.Add(items[i]);
            }
        }

        return (constants, variableExpressions);
    }

    /// <summary>
    /// Reduces a Parse-and-Eval expression bottom-up by reducing both the encoded expression and the environment.
    /// </summary>
    /// <param name="parseAndEval">The Parse-and-Eval expression to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="reducedExpressionCache">Optional memoization cache forwarded to recursive reductions.</param>
    /// <param name="config">Configuration controlling optional reduction behaviors,
    /// including whether
    /// <see cref="TryConsolidateGenericFunctionApplicationChain(Expression.Eval, PineVMParseCache, ReductionConfig)"/>
    /// is applied.</param>
    /// <returns>The reduced expression or the original if no change occurs.</returns>
    public static Expression ReduceParseAndEvalBottomUp(
        Expression.Eval parseAndEval,
        PineVMParseCache parseCache,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache,
        ReductionConfig config = default)
    {
        var reducedEncoded =
            ReduceExpressionBottomUp(
                parseAndEval.Encoded,
                config,
                parseCache,
                reducedExpressionCache);

        var reducedEnv =
            ReduceExpressionBottomUp(
                parseAndEval.Environment,
                config,
                parseCache,
                reducedExpressionCache);

        var reduced =
            (reducedEncoded == parseAndEval.Encoded && reducedEnv == parseAndEval.Environment)
            ?
            parseAndEval
            :
            new Expression.Eval(encoded: reducedEncoded, environment: reducedEnv);

        // Try to consolidate a chain of nested ParseAndEval expressions emitted by frontend
        // compilers as the generic form of a function application. This collapses the chain into
        // a single ParseAndEval when the innermost function expression is a literal and there
        // is more than one argument applied generically.
        if (TryConsolidateGenericFunctionApplicationChain(
                reduced,
                parseCache,
                config) is { } consolidated)
        {
            // The consolidation symbolically inlines the structural-encoding wrapper that
            // FunctionValueBuilder emits, which can leave behind kernel applications
            // (concat/head/skip) over partly-literal lists that are statically reducible.
            // Run a bottom-up reduction pass on the result so those simplifications happen
            // before downstream codegen sees the expression.
            var reducedConsolidated =
                ReduceExpressionBottomUp(
                    consolidated,
                    config,
                    parseCache,
                    reducedExpressionCache);

            return reducedConsolidated;
        }

        if (TryInlineEvalBottomUp(reduced, config, parseCache, reducedExpressionCache) is { } inlined)
        {
            return inlined;
        }

        return reduced;
    }

    /// <summary>
    /// Attempts to inline a <see cref="Expression.Eval"/> whose
    /// <see cref="Expression.Eval.Encoded"/> resolves to a known
    /// constant Pine-encoded expression value, by substituting the parsed inner
    /// expression's <see cref="Expression.Environment"/> nodes with the outer
    /// <see cref="Expression.Eval.Environment"/>.
    /// </summary>
    public static Expression? TryInlineEvalBottomUp(
        Expression.Eval evalExpr,
        ReductionConfig config,
        PineVMParseCache parseCache,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache)
    {
        if (config.InlineEvalEnvironmentSizeLimit <= evalExpr.SubexpressionCount)
            return null;

        if (evalExpr.Encoded.ReferencesEnvironment)
            return null;

        if (TryEvaluateExpressionIndependent(evalExpr.Encoded, parseCache).IsOkOrNull() is not { } encodedExprValue)
            return null;

        if (parseCache.ParseExpression(encodedExprValue).IsOkOrNull() is not { } innerExpr)
            return null;

        var evalCountBefore = evalExpr.Environment.EvalCount;

        if (innerExpr.EvalCount > 0)
        {
            foreach (var innerSubExpr in Expression.EnumerateSelfAndDescendants(innerExpr))
            {
                if (innerSubExpr is Expression.Eval { ReferencesEnvironment: true })
                {
                    return null;
                }
            }
        }

        var innerExprReduced =
            ReduceExpressionBottomUp(
                innerExpr,
                config,
                parseCache,
                reducedExpressionCache);

        var substituted = SubstituteEnvironmentNode(expression: innerExprReduced, replacement: evalExpr.Environment);

        var reducedViaEval =
            ReduceExpressionBottomUp(
                substituted,
                config,
                parseCache,
                reducedExpressionCache);

        var evalCountAfter = reducedViaEval.EvalCount;

        if (evalCountAfter > evalCountBefore)
        {
            return null;
        }

        return reducedViaEval;
    }

    /// <summary>
    /// Attempts to collapse a chain of nested <see cref="Expression.Eval"/> expressions
    /// produced by the generic form of function application
    /// (<see cref="CodeAnalysis.BuildGenericFunctionApplication(Expression, IReadOnlyList{Expression})"/>)
    /// into a single, equivalent expression.
    /// <para>
    /// Triggers only when the recovered argument list contains more than one argument and
    /// the recovered function expression is an <see cref="Expression.Litral"/> whose value
    /// parses successfully as a Pine expression. The transformation symbolically simulates
    /// each application step using the structural encoding emitted by
    /// <c>FunctionValueBuilder</c>: it inlines the literal-encoded function body for the
    /// first argument and then "decodes" the resulting construction expression for each
    /// subsequent argument, producing a single equivalent expression that no longer requires
    /// one <see cref="Expression.Eval"/> per argument.
    /// </para>
    /// </summary>
    /// <param name="parseAndEval">The outermost <see cref="Expression.Eval"/> of the chain.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="config">Configuration controlling optional reduction behaviors. When
    /// <see cref="ReductionConfig.DisableGenericApplicationChainConsolidation"/> is <c>true</c>,
    /// the method returns <c>null</c> regardless of the chain shape. Intended exclusively for
    /// diagnostic / inspection scenarios where the caller wants to observe the
    /// pre-consolidation behavior side-by-side with the optimized one.</param>
    /// <returns>The collapsed expression, or <c>null</c> when no consolidation applies.</returns>
    public static Expression? TryConsolidateGenericFunctionApplicationChain(
        Expression.Eval parseAndEval,
        PineVMParseCache parseCache,
        ReductionConfig config = default)
    {
        if (config.DisableGenericApplicationChainConsolidation)
        {
            return null;
        }

        if (CodeAnalysis.ParseGenericFunctionApplication(parseAndEval) is not { } chain)
        {
            return null;
        }

        var (functionExpr, arguments) = chain;

        if (arguments.Count <= 1)
        {
            return null;
        }

        if (functionExpr is not Expression.Litral functionLiteral)
        {
            return null;
        }

        if (parseCache.ParseExpression(functionLiteral.Value).IsOkOrNull() is not { } functionBody)
        {
            return null;
        }

        // First step: ParseAndEval(Literal(L), arguments[0]) ≡ functionBody[Environment := arguments[0]].
        var currentExpr = SubstituteEnvironmentNode(functionBody, arguments[0]);

        // For each subsequent argument, treat currentExpr as a "construction" expression that
        // builds an encoded Pine expression at runtime, and decode the application of that
        // argument into a single equivalent Pine expression.
        for (var i = 1; i < arguments.Count; ++i)
        {
            if (TryDecodeApplicationOfConstructedEncoding(currentExpr, arguments[i], parseCache) is not { } decoded)
            {
                return null;
            }

            currentExpr = decoded;
        }

        return currentExpr;
    }

    /// <summary>
    /// Symbolically computes an expression equivalent to
    /// <c>ParseAndEval(<paramref name="construction"/>, <paramref name="envArg"/>)</c>,
    /// assuming <paramref name="construction"/> uses the same structural encoding scheme as
    /// <see cref="ExpressionEncoding.EncodeExpressionAsValue(Expression)"/>.
    /// Returns <c>null</c> when the construction does not match a recognized encoded shape.
    /// </summary>
    /// <param name="construction">An expression that, when evaluated, would yield an
    /// encoded Pine expression value (matching the structural encoding scheme).</param>
    /// <param name="envArg">The environment expression that the encoded inner expression
    /// would receive at runtime.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    public static Expression? TryDecodeApplicationOfConstructedEncoding(
        Expression construction,
        Expression envArg,
        PineVMParseCache parseCache)
    {
        // If the construction is itself a ParseAndEval whose Encoded part is a literal,
        // first inline that step (parse the literal as an expression, substitute Environment
        // with the inner ParseAndEval's Environment) and then continue the decoding.
        if (construction is Expression.Eval innerPe &&
            innerPe.Encoded is Expression.Litral innerEncodedLiteral)
        {
            if (parseCache.ParseExpression(innerEncodedLiteral.Value).IsOkOrNull() is { } innerFunctionBody)
            {
                var inlined = SubstituteEnvironmentNode(innerFunctionBody, innerPe.Environment);

                return TryDecodeApplicationOfConstructedEncoding(inlined, envArg, parseCache);
            }

            return null;
        }

        // If the construction itself is a literal, parse it as an expression and substitute
        // Environment with envArg.
        if (construction is Expression.Litral constructionLiteral)
        {
            if (parseCache.ParseExpression(constructionLiteral.Value).IsOkOrNull() is { } parsedInner)
            {
                return SubstituteEnvironmentNode(parsedInner, envArg);
            }

            return null;
        }

        // Otherwise, the construction must be a ListExpr matching the structural encoding shape.
        if (construction is not Expression.List constructionList)
        {
            return null;
        }

        if (constructionList.Items.Count is 0 ||
            constructionList.Items[0] is not Expression.Litral tagLiteral)
        {
            return null;
        }

        if (StringEncoding.StringFromValue(tagLiteral.Value).IsOkOrNull() is not { } tag)
        {
            return null;
        }

        if (TryDecodeConstructedEncoding2026(constructionList, tag, envArg, parseCache) is { } decoded2026)
        {
            return decoded2026;
        }

        if (constructionList.Items.Count is not 2)
        {
            return null;
        }

        if (constructionList.Items[1] is not Expression.List tagArguments)
        {
            return null;
        }

        switch (tag)
        {
            case "Literal":
                {
                    // Encoding of Expression.Literal: ListExpr([Lit("Literal"), ListExpr([X])]).
                    // ParseAndEval(this, envArg) evaluates the parsed Literal expression with
                    // env = envArg; Literal expressions ignore their environment, so the result
                    // is the literal value, which is what X evaluates to in the outer env.
                    if (tagArguments.Items.Count is not 1)
                    {
                        return null;
                    }

                    return tagArguments.Items[0];
                }

            case "Environment":
                {
                    // Encoding of Expression.Environment: ListExpr([Lit("Environment"), ListExpr([])]).
                    // ParseAndEval(this, envArg) evaluates the parsed Environment expression
                    // with env = envArg, producing envArg.
                    if (tagArguments.Items.Count is not 0)
                    {
                        return null;
                    }

                    return envArg;
                }

            case "List":
                {
                    // Encoding of Expression.List: ListExpr([Lit("List"), ListExpr([ListExpr([item_enc...])])]).
                    if (tagArguments.Items.Count is not 1)
                    {
                        return null;
                    }

                    if (tagArguments.Items[0] is not Expression.List innerItemsList)
                    {
                        return null;
                    }

                    var decodedItems = new Expression[innerItemsList.Items.Count];

                    for (var i = 0; i < innerItemsList.Items.Count; ++i)
                    {
                        if (TryDecodeApplicationOfConstructedEncoding(
                                innerItemsList.Items[i],
                                envArg,
                                parseCache) is not { } decodedItem)
                        {
                            return null;
                        }

                        decodedItems[i] = decodedItem;
                    }

                    return Expression.ListInst(decodedItems);
                }

            case "ParseAndEval":
                {
                    // Encoding of Expression.ParseAndEval:
                    // ListExpr([Lit("ParseAndEval"), ListExpr([encInner, envInner])]).
                    if (tagArguments.Items.Count is not 2)
                    {
                        return null;
                    }

                    if (TryDecodeApplicationOfConstructedEncoding(
                            tagArguments.Items[0],
                            envArg,
                            parseCache) is not { } decodedEncoded)
                    {
                        return null;
                    }

                    if (TryDecodeApplicationOfConstructedEncoding(
                            tagArguments.Items[1],
                            envArg,
                            parseCache) is not { } decodedEnv)
                    {
                        return null;
                    }

                    return new Expression.Eval(decodedEncoded, decodedEnv);
                }

            case "KernelApplication":
                {
                    // Encoding of Expression.KernelApplication:
                    // ListExpr([Lit("KernelApplication"), ListExpr([Lit(funcName_str), input_enc])]).
                    if (tagArguments.Items.Count is not 2)
                    {
                        return null;
                    }

                    if (tagArguments.Items[0] is not Expression.Litral funcNameLiteral)
                    {
                        return null;
                    }

                    if (StringEncoding.StringFromValue(funcNameLiteral.Value).IsOkOrNull() is not { } functionName)
                    {
                        return null;
                    }

                    if (TryDecodeApplicationOfConstructedEncoding(
                            tagArguments.Items[1],
                            envArg,
                            parseCache) is not { } decodedInput)
                    {
                        return null;
                    }

                    return Expression.BuiltinInst(functionName, decodedInput);
                }

            default:
                return null;
        }
    }

    private static Expression? TryDecodeConstructedEncoding2026(
        Expression.List construction,
        string tag,
        Expression envArg,
        PineVMParseCache parseCache)
    {
        switch (tag)
        {
            case "Litral":
                return
                    construction.Items.Count is 2
                    ?
                    construction.Items[1]
                    :
                    null;

            case "Environment":
                return
                    construction.Items.Count is 1
                    ?
                    envArg
                    :
                    null;

            case "List":
                {
                    var decodedItems = new Expression[construction.Items.Count - 1];

                    for (var i = 1; i < construction.Items.Count; ++i)
                    {
                        if (TryDecodeApplicationOfConstructedEncoding(
                                construction.Items[i],
                                envArg,
                                parseCache) is not { } decodedItem)
                        {
                            return null;
                        }

                        decodedItems[i - 1] = decodedItem;
                    }

                    return Expression.ListInst(decodedItems);
                }

            case "Builtin":
                {
                    if (construction.Items.Count is not 3 ||
                        construction.Items[1] is not Expression.Litral functionLiteral ||
                        StringEncoding.StringFromValue(functionLiteral.Value).IsOkOrNull() is not { } functionName ||
                        TryDecodeApplicationOfConstructedEncoding(
                            construction.Items[2],
                            envArg,
                            parseCache) is not { } decodedInput)
                    {
                        return null;
                    }

                    return Expression.BuiltinInst(functionName, decodedInput);
                }

            case "Condition":
                {
                    if (construction.Items.Count is not 4 ||
                        TryDecodeApplicationOfConstructedEncoding(
                            construction.Items[1],
                            envArg,
                            parseCache) is not { } decodedCondition ||
                        TryDecodeApplicationOfConstructedEncoding(
                            construction.Items[2],
                            envArg,
                            parseCache) is not { } decodedFalseBranch ||
                        TryDecodeApplicationOfConstructedEncoding(
                            construction.Items[3],
                            envArg,
                            parseCache) is not { } decodedTrueBranch)
                    {
                        return null;
                    }

                    return
                        Expression.ConditionalInst(
                            decodedCondition,
                            decodedFalseBranch,
                            decodedTrueBranch);
                }

            case "Eval":
                {
                    if (construction.Items.Count is not 3 ||
                        TryDecodeApplicationOfConstructedEncoding(
                            construction.Items[1],
                            envArg,
                            parseCache) is not { } decodedEncoded ||
                        TryDecodeApplicationOfConstructedEncoding(
                            construction.Items[2],
                            envArg,
                            parseCache) is not { } decodedEnvironment)
                    {
                        return null;
                    }

                    return new Expression.Eval(decodedEncoded, decodedEnvironment);
                }

            case "Label":
                {
                    if (construction.Items.Count is not 3 ||
                        construction.Items[1] is not Expression.Litral labelLiteral ||
                        TryDecodeApplicationOfConstructedEncoding(
                            construction.Items[2],
                            envArg,
                            parseCache) is not { } decodedLabeled)
                    {
                        return null;
                    }

                    return new Expression.Label(labelLiteral.Value, decodedLabeled);
                }

            default:
                return null;
        }
    }

    private static Expression SubstituteEnvironmentNode(Expression expression, Expression replacement) =>
        TransformPineExpressionWithOptionalReplacement(
            findReplacement: e => e is Expression.Environment ? replacement : null,
            expression: expression).expr;

    /// <summary>
    /// Reduces a conditional expression bottom-up by reducing its components and then attempting a local reduction.
    /// </summary>
    /// <param name="conditional">The conditional expression to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="reducedExpressionCache">Optional memoization cache forwarded to recursive reductions.</param>
    /// <param name="config">Configuration controlling optional reduction behaviors.</param>
    /// <returns>The reduced expression or the original if no change occurs.</returns>
    public static Expression ReduceConditionalBottomUp(
        Expression.Conditional conditional,
        PineVMParseCache parseCache,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache,
        ReductionConfig config = default)
    {
        var reducedCondition =
            ReduceExpressionBottomUp(
                conditional.Condition,
                config,
                parseCache,
                reducedExpressionCache);

        var reducedTrue =
            ReduceExpressionBottomUp(
                conditional.TrueBranch,
                config,
                parseCache,
                reducedExpressionCache);

        var reducedFalse =
            ReduceExpressionBottomUp(
                conditional.FalseBranch,
                config,
                parseCache,
                reducedExpressionCache);

        if (reducedCondition == conditional.Condition &&
            reducedTrue == conditional.TrueBranch &&
            reducedFalse == conditional.FalseBranch)
        {
            return conditional;
        }

        return
            Expression.ConditionalInst(
                condition: reducedCondition,
                falseBranch: reducedFalse,
                trueBranch: reducedTrue);
    }

    /// <summary>
    /// Reduces a string-tagged expression bottom-up by reducing the tagged subexpression.
    /// </summary>
    /// <param name="stringTag">The string tag expression to reduce.</param>
    /// <param name="parseCache">Cache used when parsing encoded expressions.</param>
    /// <param name="reducedExpressionCache">Optional memoization cache forwarded to recursive reductions.</param>
    /// <param name="config">Configuration controlling optional reduction behaviors.</param>
    /// <returns>The reduced expression or the original if no change occurs.</returns>
    public static Expression ReduceStringTagBottomUp(
        Expression.Label stringTag,
        PineVMParseCache parseCache,
        IDictionary<(Expression, ReductionConfig), Expression>? reducedExpressionCache,
        ReductionConfig config = default)
    {
        var reducedTagged =
            ReduceExpressionBottomUp(
                stringTag.Tagged,
                config,
                parseCache,
                reducedExpressionCache);

        if (reducedTagged == stringTag.Tagged)
            return stringTag;

        return new Expression.Label(stringTag.Tag, reducedTagged);
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

                    return Expression.ListInst(reversed);
                }

            case Expression.Litral literal:
                return Expression.LitralInst(BuiltinFunction.reverse(literal.Value));

            case Expression.Builtin innerKernelApp:
                {
                    if (innerKernelApp.Function is nameof(BuiltinFunction.reverse))
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
                            Expression.ConditionalInst(
                                cond.Condition,
                                falseBranch: falseOk,
                                trueBranch: trueOk);
                    }

                    return null;
                }

            case Expression.Label tag:
                {
                    if (ApplyKernelFunctionReverseToAllBranches(tag.Tagged) is { } inner)
                    {
                        return new Expression.Label(tag.Tag, inner);
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
                        return Expression.LitralInst(PineValue.EmptyList);

                    return Expression.ListInst([.. list.Items.Skip(countClamped)]);
                }

            case Expression.Litral literal:
                return Expression.LitralInst(BuiltinFunctionSpecialized.skip(countClamped, literal.Value));

            case Expression.Builtin innerSkip:
                {
                    if (innerSkip.Function is nameof(BuiltinFunction.skip) &&
                        innerSkip.Input is Expression.List args && args.Items.Count is 2 &&
                        args.Items[0] is Expression.Litral litCount &&
                        BuiltinFunction.SignedIntegerFromValueRelaxed(litCount.Value) is { } innerCount)
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
                            Expression.ConditionalInst(
                                conditional.Condition,
                                falseBranch: falseOk,
                                trueBranch: trueOk);
                    }

                    return null;
                }

            case Expression.Label tag:
                {
                    if (ApplyKernelFunctionSkipToAllBranches(countClamped, tag.Tagged) is { } taggedOk)
                    {
                        return new Expression.Label(tag.Tag, taggedOk);
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
                        return Expression.LitralInst(PineValue.EmptyList);

                    if (countClamped >= list.Items.Count)
                        return list;

                    return Expression.ListInst([.. list.Items.Take(countClamped)]);
                }

            case Expression.Litral literal:
                return Expression.LitralInst(BuiltinFunctionSpecialized.take(countClamped, literal.Value));

            case Expression.Builtin innerTake:

                {
                    if (innerTake.Function is nameof(BuiltinFunction.take) &&
                        innerTake.Input is Expression.List args && args.Items.Count is 2 &&
                        args.Items[0] is Expression.Litral litCount &&
                        BuiltinFunction.SignedIntegerFromValueRelaxed(litCount.Value) is { } innerCount)
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
                            Expression.ConditionalInst(
                                conditional.Condition,
                                falseBranch: falseOk,
                                trueBranch: trueOk);
                    }

                    return null;
                }

            case Expression.Label tag:
                {
                    if (ApplyKernelFunctionTakeToAllBranches(countClamped, tag.Tagged) is { } taggedOk)
                    {
                        return new Expression.Label(tag.Tag, taggedOk);
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
                        return
                            Expression.ConditionalInst(
                                condition: cond.Condition,
                                falseBranch: falseOk,
                                trueBranch: trueOk);
                    }

                    return null;
                }

            case Expression.Label tag:
                {
                    if (ApplyKernelFunctionHeadToAllBranches(tag.Tagged) is { } taggedOk)
                    {
                        return new Expression.Label(tag.Tag, taggedOk);
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
        if (index < 0)
            index = 0;

        switch (inputExpression)
        {
            case Expression.List list:
                {
                    if (index < list.Items.Count)
                    {
                        return list.Items[index];
                    }

                    return Expression.LitralInst(PineValue.EmptyList);
                }

            case Expression.Litral lit:
                {
                    if (lit.Value is PineValue.ListValue lv)
                    {
                        if (index < lv.Items.Length)
                        {
                            return Expression.LitralInst(lv.Items.Span[index]);
                        }

                        return Expression.LitralInst(PineValue.EmptyList);
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
                        return
                            Expression.ConditionalInst(
                                condition: cond.Condition,
                                falseBranch: falseOut,
                                trueBranch: trueOut);
                    }

                    return null;
                }

            case Expression.Label tag:
                {
                    return TryReduceSelectListItem(tag.Tagged, index);
                }
        }

        return null;
    }
}
