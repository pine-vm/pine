using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Interpreter.IntermediateVM.Semantic;
using Pine.Core.PineVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Numerics;
using Analysis = Pine.Core.CodeAnalysis.CodeAnalysis;
using BuiltinFunctionSpecialized = Pine.Core.Internal.BuiltinFunctionSpecialized;

namespace Pine.Core.Interpreter.IntermediateVM.Frontend;

/// <summary>
/// Pure migration of the existing preparation policy, independent of instruction production.
/// The local workspace implements the immutable memo fold with private acceleration dictionaries.
/// No legacy mutable-cache compiler or reducer is invoked. All published expressions are owned.
/// </summary>
public static class FunctionPreparation
{
    /// <summary>Parses owned data with an explicit, persistent memo; invalid encodings remain data.</summary>
    public static (ParseMemoEntry Result, CompilerMemo Memo) ParseExpression(
        LiteralValue encoded, CompilerMemo memo)
    {
        if (memo.Parses.TryGetValue(encoded, out var cached))
            return (cached, memo);

        var parsed = ParseWithoutMemo(OwnedExpression.ToValue(encoded));
        var result = parsed switch
        {
            Result<string, Expression>.Ok ok => new ParseMemoEntry(OwnedExpression.Capture(ok.Value), null),
            Result<string, Expression>.Err error => new ParseMemoEntry(null, error.Value),
            _ => throw new NotImplementedException("Unknown parse result variant."),
        };
        return (result, memo with { Parses = memo.Parses.SetItem(encoded, result) });
    }

    private static Result<string, Expression> ParseWithoutMemo(PineValue value)
    {
        var modern = ParseModernWithoutMemo(value);
        return modern.IsOkOrNull() is not null
            ? modern
            : ParseLegacyWithoutMemo(value);
    }

    private static Result<string, Expression> ParseModernWithoutMemo(PineValue value) =>
        ExpressionEncoding2026.ParseExpressionFromValue(value, ParseModernWithoutMemo);

    private static Result<string, Expression> ParseLegacyWithoutMemo(PineValue value) =>
        ExpressionEncoding2024.ParseExpressionFromValue(value, ParseLegacyWithoutMemo);

    private static PineValue.ListValue EncodeWithoutMemo(Expression expression) =>
        ExpressionEncoding.EncodeExpressionAsValueWithoutTopLevelCacheLookup(expression, EncodeWithoutMemo);

    private static Expression.Litral MakeLiteral(PineValue value) =>
        new(OwnedExpression.ToValue(OwnedExpression.CaptureValue(value)));

    private static PineValue EncodeInteger(BigInteger integer) =>
        new PineValue.BlobValue(IntegerEncoding.EncodeSignedIntegerBlob(integer));

    /// <summary>Returns a prepared function and the resulting immutable memo, leaving inputs unchanged.</summary>
    public static (PreparedFunction Function, CompilerMemo Memo) PrepareFunction(
        CompilationRequest request, CompilerMemo memo)
    {
        return memo.Preparations.TryGetValue(request, out var prepared)
            ? (prepared, memo)
            : Prepare();

        (PreparedFunction Function, CompilerMemo Memo) Prepare()
        {
            var parses = memo.Parses.ToDictionary();
            var reducedExpressionCache = memo.Reductions.ToDictionary(
                item => (item.Key.Expression.ToExpression(), item.Key.Config),
                item => item.Value.ToExpression());
            var root = request.Root.ToExpression();
            var constraint = request.Specialization?.ToValueClass();
            var options = request.Options;
            var inlined = options.DisableReduction ? root :
                InlineStaticInvocationsAndReduceRecursive(root, [], 6, 4_000, false,
                    expression => SkipInlining(expression, null),
                    options.PathMaxLowExclusive, options.PathMaxHighInclusive,
                    options.DisableGenericApplicationChainConsolidation);
            var substituted = constraint is null ? inlined :
                SubstituteSubexpressionsForEnvironmentConstraint(inlined, constraint);
            var reduced = options.DisableReduction ? substituted :
                ReduceExpressionAndInlineRecursive(substituted, [], constraint, [root],
                    7, 4_000, false, SkipInlining,
                    options.PathMaxLowExclusive, options.PathMaxHighInclusive,
                    options.DisableGenericApplicationChainConsolidation);
            var signature = new FunctionSignature(
                StaticFunctionInterface.FromExpression(root).ParamsPaths
                    .Select(path => new FunctionParameter(new(path.ToImmutableList()))).ToImmutableList(),
                [Semantic.ValueType.PineValue]);
            var result = new PreparedFunction(request, OwnedExpression.Capture(reduced), signature);
            return (result, memo with
            {
                Parses = parses.ToImmutableDictionary(),
                Reductions = reducedExpressionCache.ToImmutableDictionary(
                    item => new ReductionMemoKey(OwnedExpression.Capture(item.Key.Item1), item.Key.Item2),
                    item => OwnedExpression.Capture(item.Value)),
                Preparations = memo.Preparations.SetItem(request, result),
            });

            bool SkipInlining(Expression expression, PineValueClass? specialization) =>
                request.InlineExclusions.Contains(new(
                    OwnedExpression.Capture(expression),
                    specialization is null ? null : SpecializationFacts.Capture(specialization)));

            Result<string, Expression> ParseExpression(PineValue value)
            {
                var key = OwnedExpression.CaptureValue(value);
                if (parses.TryGetValue(key, out var cached))
                    return cached.Expression is { } expression
                        ? Result<string, Expression>.ok(expression.ToExpression())
                        : Result<string, Expression>.err(cached.Error!);
                var parsed = ParseWithoutMemo(value);
                var entry = parsed switch
                {
                    Result<string, Expression>.Ok ok => new ParseMemoEntry(OwnedExpression.Capture(ok.Value), null),
                    Result<string, Expression>.Err error => new ParseMemoEntry(null, error.Value),
                    _ => throw new NotImplementedException("Unknown parse result variant."),
                };
                parses.Add(key, entry);
                return entry.Expression is { } owned
                    ? Result<string, Expression>.ok(owned.ToExpression())
                    : Result<string, Expression>.err(entry.Error!);
            }

            ExpressionEncoding2026.ParseExpressionResult ParseExpressionWithoutResultAllocation(PineValue value) =>
                ExpressionEncoding2026.ParseExpressionResult.FromPublicResult(ParseExpression(value));

            Expression SubstituteEnvironmentNode(Expression expression, Expression environmentReplacement)
            {
                var substitutions = new Dictionary<Expression, Expression>();
                return Substitute(expression);

                Expression Substitute(Expression node)
                {
                    if (!node.ReferencesEnvironment || environmentReplacement is Expression.Environment)
                        return node;
                    if (substitutions.TryGetValue(node, out var cached))
                        return cached;
                    var result = node switch
                    {
                        Expression.Environment => environmentReplacement,
                        Expression.Litral => node,
                        Expression.List list => new Expression.List(list.Items.Select(Substitute).ToArray()),
                        Expression.Builtin builtin => new Expression.Builtin(builtin.Function, Substitute(builtin.Input)),
                        Expression.Conditional conditional => new Expression.Conditional(
                            Substitute(conditional.Condition), Substitute(conditional.FalseBranch), Substitute(conditional.TrueBranch)),
                        Expression.Eval eval => new Expression.Eval(Substitute(eval.Encoded), Substitute(eval.Environment)),
                        Expression.Label label => new Expression.Label(label.LabelValue, Substitute(label.Tagged)),
                        _ => throw new NotImplementedException("Unknown expression variant: " + node.GetType().Name),
                    };
                    substitutions.Add(node, result);
                    return result;
                }
            }

            ValueEvalResult TryEvalIndependent(
                    Expression expression) =>
                    expression switch
                    {
                        Expression.Environment =>
                        ValueEvalResult.Err("Expression depends on environment"),

                        Expression.Litral literal =>
                        ValueEvalResult.Ok(literal.Value),

                        Expression.List list =>
                        TryEvalList(list),

                        Expression.Builtin builtinExpr =>
                        TryEvalBuiltin(builtinExpr),

                        Expression.Eval evalExpr =>
                        TryEvalEval(evalExpr),

                        Expression.Conditional conditional =>
                        TryEvalConditional(conditional),

                        Expression.Label labelExpr =>
                        TryEvalIndependent(labelExpr.Tagged),

                        _ =>
                        throw new NotImplementedException(
                            "TryEvalIndependent does not handle expression variant: " +
                            expression.GetType().Name)
                    };

            ValueEvalResult TryEvalList(
                    Expression.List listExpr)
            {
                var itemsValues = new PineValue[listExpr.Items.Count];

                for (var i = 0; i < listExpr.Items.Count; i++)
                {
                    var itemResult =
                        TryEvalIndependent(
                            listExpr.Items[i]);

                    if (itemResult.Value is { } itemValue)
                    {
                        itemsValues[i] = itemValue;
                    }
                    else
                    {
                        return itemResult;
                    }
                }

                return ValueEvalResult.Ok(new PineValue.ListValue(itemsValues));
            }

            ValueEvalResult TryEvalEval(
                    Expression.Eval evalExpr)
            {
                var evalEnvResult =
                    TryEvalIndependent(
                        evalExpr.Environment);

                if (evalEnvResult.Error is { } err)
                {
                    return
                        ValueEvalResult.Err(
                            "Failed evaluating env of eval expression: " + err);
                }

                if (evalEnvResult.Value is not { } envValue)
                {
                    throw new InvalidOperationException(
                        "Independent evaluation result contains neither a value nor an error");
                }

                var evalEncodedExprResult =
                    TryEvalIndependent(
                        evalExpr.Encoded);

                if (evalEncodedExprResult.Error is { } encodedErr)
                {
                    return
                        ValueEvalResult.Err(
                            "Failed to evaluate encoded expression: " + encodedErr);
                }

                if (evalEncodedExprResult.Value is not { } encodedOk)
                {
                    throw new InvalidOperationException(
                        "Independent evaluation result contains neither a value nor an error");
                }

                var parseResult =
                    ParseExpressionWithoutResultAllocation(encodedOk);

                if (parseResult.Error is { } parseErr)
                {
                    return
                        ValueEvalResult.Err(
                            "Failed to parse encoded expression: " + parseErr);
                }

                if (parseResult.Expression is not { } parseOk)
                {
                    throw new InvalidOperationException(
                        "Expression parse result contains neither an expression nor an error");
                }

                if (!parseOk.ReferencesEnvironment)
                {
                    return
                        TryEvalIndependent(parseOk);
                }

                return ValueEvalResult.Err("Not following parse&eval");
            }

            ValueEvalResult TryEvalConditional(
                    Expression.Conditional conditionalExpr)
            {
                var evalConditionResult =
                    TryEvalIndependent(
                        conditionalExpr.Condition);

                if (evalConditionResult.Error is { } conditionErr)
                {
                    return
                        ValueEvalResult.Err(
                            "Failed to evaluate condition: " + conditionErr);
                }

                if (evalConditionResult.Value is not { } conditionOk)
                {
                    throw new InvalidOperationException(
                        "Independent evaluation result contains neither a value nor an error");
                }

                if (conditionOk == PineKernelValues.TrueValue)
                {
                    return
                        TryEvalIndependent(
                            conditionalExpr.TrueBranch);
                }

                return
                    TryEvalIndependent(
                        conditionalExpr.FalseBranch);
            }

            ValueEvalResult TryEvalBuiltin(
                    Expression.Builtin builtinExpr)
            {
                var evalInputResult =
                    TryEvalIndependent(
                        builtinExpr.Input);

                if (evalInputResult.Error is { } inputErr)
                {
                    return
                        ValueEvalResult.Err(
                            "Failed to evaluate built-in application input: " + inputErr);
                }

                if (evalInputResult.Value is not { } inputOk)
                {
                    throw new InvalidOperationException(
                        "Independent evaluation result contains neither a value nor an error");
                }

                try
                {
                    return ValueEvalResult.Ok(
                        BuiltinFunction.ApplyFunctionGeneric(builtinExpr.Function, inputOk));
                }
                catch (Exception exception) when (
                    builtinExpr.Function is "bit_shift_left" or "bit_shift_right" &&
                    exception is OverflowException or IndexOutOfRangeException or ArgumentOutOfRangeException)
                {
                    // These are existing primitive failures for out-of-range shifts, not compiler
                    // errors. Keep the operation so an unselected branch remains safe to compile.
                    return ValueEvalResult.Err("Constant evaluation deferred: " + exception.GetType().Name);
                }
            }

            Expression? SearchForExpressionReduction(
                    Expression expression,
                    PineValueClass? envConstraintId)
            {
                if (expression is Expression.Litral)
                    return null;

                if (Analysis.TryParseAsLiteral(expression) is { } literalValue)
                {
                    return MakeLiteral(literalValue);
                }

                if (envConstraintId is not null &&
                    Analysis.TryParseExprAsPathInEnv(expression) is { } parsedAsPath)
                {
                    if (envConstraintId.TryGetValue(parsedAsPath) is { } fromEnvConstraint)
                    {
                        return MakeLiteral(fromEnvConstraint);
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
                        if (TryEvalIndependent(expression).Value is { } okValue)
                        {
                            return MakeLiteral(okValue);
                        }
                    }
                    catch (ParseExpressionException)
                    {

                    }

                    return null;
                }

                switch (expression)
                {
                    case Expression.Builtin rootBuiltinExpr:

                        Expression.Builtin ContinueWithReducedInput(Expression newInput) =>
                            new Expression.Builtin(
                                function: rootBuiltinExpr.Function,
                                input: newInput);

                        switch (rootBuiltinExpr.Function)
                        {
                            case nameof(BuiltinFunction.equal):
                                {
                                    if (rootBuiltinExpr.Input is Expression.List inputList)
                                    {
                                        if (TryReduceEqualityOfSingleFieldTags(inputList) is { } reducedEquality)
                                        {
                                            return reducedEquality;
                                        }

                                        if (envConstraintId is not null)
                                        {
                                            var reducedArgumentsList =
                                                inputList.Items
                                                .Select(
                                                    origArg => SearchForExpressionReductionRecursive(
                                                        maxDepth: 5,
                                                        expression: origArg,
                                                        envConstraintId: envConstraintId))
                                                .ToImmutableArray();

                                            var listLengthLowerBounds = new List<int>();

                                            var listConcreteValues = new List<PineValue>();

                                            foreach (var item in reducedArgumentsList)
                                            {
                                                foreach (var itemBounds in EnumerateInferListLengthBounds(item, envConstraintId))
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
                                                        return MakeLiteral(PineKernelValues.FalseValue);
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
                                                        return MakeLiteral(PineKernelValues.FalseValue);
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
                                    if (ApplyBuiltinFunctionHeadToAllBranches(rootBuiltinExpr.Input) is { } reducedBranches)
                                    {
                                        return
                                            SearchForExpressionReduction(reducedBranches, envConstraintId)
                                            ??
                                            reducedBranches;
                                    }

                                    return AttemptReduceViaEval();
                                }

                            case nameof(BuiltinFunction.skip):
                                {
                                    if (rootBuiltinExpr.Input is Expression.List inputList && inputList.Items.Count is 2)
                                    {
                                        var countExpr = inputList.Items[0];
                                        var seqExpr = inputList.Items[1];

                                        if (TryEvalIndependent(countExpr).Value is
                                            { } okSkipCountValue &&
                                            BuiltinFunction.SignedIntegerFromValueRelaxed(okSkipCountValue) is { } okSkipCount)
                                        {
                                            if (ApplyBuiltinFunctionSkipToAllBranches((int)(okSkipCount < 0 ? 0 : okSkipCount), seqExpr) is { } reducedSkip)
                                            {
                                                return
                                                    SearchForExpressionReduction(reducedSkip, envConstraintId)
                                                    ??
                                                    reducedSkip;
                                            }
                                        }

                                        return AttemptReduceViaEval();
                                    }

                                    return AttemptReduceViaEval();
                                }

                            case nameof(BuiltinFunction.take):
                                {
                                    if (rootBuiltinExpr.Input is Expression.List takeInput && takeInput.Items.Count is 2)
                                    {
                                        var countExpr = takeInput.Items[0];
                                        var srcExpr = takeInput.Items[1];

                                        if (TryEvalIndependent(countExpr).Value is
                                            { } okTakeCountValue &&
                                            BuiltinFunction.SignedIntegerFromValueRelaxed(okTakeCountValue) is { } okTakeCount)
                                        {
                                            if (ApplyBuiltinFunctionTakeToAllBranches((int)okTakeCount, srcExpr) is { } reducedTake)
                                            {
                                                return
                                                    SearchForExpressionReduction(reducedTake, envConstraintId)
                                                    ??
                                                    reducedTake;
                                            }
                                        }

                                        return AttemptReduceViaEval();
                                    }

                                    return AttemptReduceViaEval();
                                }

                            case nameof(BuiltinFunction.reverse):
                                {
                                    if (ApplyBuiltinFunctionReverseToAllBranches(rootBuiltinExpr.Input) is { } reducedRev)
                                    {
                                        return
                                            SearchForExpressionReduction(reducedRev, envConstraintId)
                                            ??
                                            reducedRev;
                                    }

                                    return AttemptReduceViaEval();
                                }

                            case nameof(BuiltinFunction.concat):
                                {
                                    if (rootBuiltinExpr.Input is Expression.List inputList)
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
                                                    return MakeLiteral(PineValue.EmptyList);
                                                }

                                                if (nonEmptyItems.Count is 1)
                                                {
                                                    return nonEmptyItems[0];
                                                }

                                                return
                                                    ContinueWithReducedInput(new Expression.List(nonEmptyItems));
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
                                                        items.Add(MakeLiteral(subLiteralList.Items.Span[i]));
                                                    }

                                                    continue;
                                                }

                                                return AttemptReduceViaEval();
                                            }

                                            items.AddRange(subList.Items);
                                        }

                                        return new Expression.List(items);
                                    }

                                    return AttemptReduceViaEval();
                                }

                            case nameof(BuiltinFunction.length):
                                {
                                    if (rootBuiltinExpr.Input is Expression.List inputList)
                                    {
                                        return
                                            MakeLiteral(
                                                EncodeInteger(inputList.Items.Count));
                                    }

                                    if (rootBuiltinExpr.Input is Expression.Builtin lengthInputBuiltin)
                                    {
                                        if (lengthInputBuiltin.Function is nameof(BuiltinFunction.concat) &&
                                            lengthInputBuiltin.Input is Expression.List lengthConcatList)
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
                                                    MakeLiteral(
                                                        EncodeInteger(aggregateLength.Value));
                                            }
                                        }
                                    }

                                    return AttemptReduceViaEval();
                                }

                            case nameof(BuiltinFunction.int_add):
                                {
                                    if (rootBuiltinExpr.Input is Expression.List addInputList)
                                    {
                                        var reducedBuiltinExpr =
                                            ReduceFlattenedIntegerBuiltinApplication(
                                                nameof(BuiltinFunction.int_add),
                                                addInputList.Items);

                                        if (reducedBuiltinExpr is not null)
                                        {
                                            return reducedBuiltinExpr;
                                        }
                                    }

                                    return AttemptReduceViaEval();
                                }

                            case nameof(BuiltinFunction.int_mul):
                                {
                                    if (rootBuiltinExpr.Input is Expression.List mulInputList)
                                    {
                                        var reducedBuiltinExpr =
                                            ReduceFlattenedIntegerBuiltinApplication(
                                                nameof(BuiltinFunction.int_mul),
                                                mulInputList.Items);

                                        if (reducedBuiltinExpr is not null)
                                        {
                                            return reducedBuiltinExpr;
                                        }
                                    }

                                    return AttemptReduceViaEval();
                                }

                            default:
                                return AttemptReduceViaEval();
                        }

                    case Expression.Conditional conditional:
                        {
                            if (TryReduceHeadMatchWithNonemptyCheck(conditional) is { } reducedHeadMatch)
                            {
                                return reducedHeadMatch;
                            }

                            if (TryMergeAdjacentSliceMatches(conditional) is { } mergedSliceMatch)
                            {
                                return mergedSliceMatch;
                            }

                            if (!conditional.Condition.ReferencesEnvironment)
                            {
                                if (TryEvalIndependent(
                                    conditional.Condition).Value is { } conditionValue)
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
                                            new Expression.Conditional(
                                                condition: equalArgsList.Items[0],
                                                trueBranch: conditional.FalseBranch,
                                                falseBranch: conditional.TrueBranch);

                                    }

                                    if (val1 == PineKernelValues.TrueValue &&
                                        IsKnownBooleanExpression(equalArgsList.Items[0]))
                                    {
                                        return
                                            new Expression.Conditional(
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
                                            new Expression.Conditional(
                                                condition: equalArgsList.Items[1],
                                                trueBranch: conditional.FalseBranch,
                                                falseBranch: conditional.TrueBranch);
                                    }

                                    if (val0 == PineKernelValues.TrueValue &&
                                        IsKnownBooleanExpression(equalArgsList.Items[1]))
                                    {
                                        return
                                            new Expression.Conditional(
                                                condition: equalArgsList.Items[1],
                                                falseBranch: conditional.FalseBranch,
                                                trueBranch: conditional.TrueBranch);
                                    }
                                }
                            }

                            return AttemptReduceViaEval();
                        }

                    case Expression.Litral or Expression.List or Expression.Environment or Expression.Eval or Expression.Label:
                        return AttemptReduceViaEval();
                    default:
                        throw new NotImplementedException("Unknown expression variant: " + expression.GetType().Name);
                }
            }

            Expression? TryReduceEqualityOfSingleFieldTags(
                    Expression.List input)
            {
                if (input.Items.Count is not 2)
                    return null;

                var left = TryUnwrapSingleFieldTagFromAllBranches(input.Items[0]);
                var right = TryUnwrapSingleFieldTagFromAllBranches(input.Items[1]);

                if (left is null ||
                    right is null ||
                    left.Value.marker != right.Value.marker ||
                    left.Value.tag != right.Value.tag)
                    return null;

                return
                    new Expression.Builtin(
                        nameof(BuiltinFunction.equal),
                        new Expression.List([left.Value.inner, right.Value.inner]));
            }

            (PineValue? marker, PineValue tag, Expression inner)? TryUnwrapSingleFieldTagFromAllBranches(
                    Expression expression)
            {
                switch (expression)
                {
                    case Expression.List
                    {
                        Items.Count: 3,
                        Items:
                        [
                            Expression.Litral marker,
                            Expression.Litral tag,
                            var inner
                        ]
                    }:
                        return (marker.Value, tag.Value, inner);

                    case Expression.List
                    {
                        Items.Count: 2,
                        Items:
                        [
                            Expression.Litral tag,
                            Expression.List { Items.Count: 1 } fields
                        ]
                    }:
                        return (null, tag.Value, fields.Items[0]);

                    case Expression.Litral
                    {
                        Value: PineValue.ListValue
                        {
                            Items.Length: 3
                        } taggedValue
                    }:
                        {
                            var items = taggedValue.Items.Span;

                            return
                                (items[0],
                                items[1],
                                MakeLiteral(items[2]));
                        }

                    case Expression.Litral
                    {
                        Value: PineValue.ListValue
                        {
                            Items.Length: 2
                        } taggedValue
                    }:
                        {
                            var items = taggedValue.Items.Span;

                            if (items[1] is not PineValue.ListValue { Items.Length: 1 } fields)
                                return null;

                            return (null, items[0], MakeLiteral(fields.Items.Span[0]));
                        }

                    case Expression.Conditional conditional:
                        {
                            var trueBranch =
                                TryUnwrapSingleFieldTagFromAllBranches(
                                    conditional.TrueBranch);

                            var falseBranch =
                                TryUnwrapSingleFieldTagFromAllBranches(
                                    conditional.FalseBranch);

                            if (trueBranch is null ||
                                falseBranch is null ||
                                trueBranch.Value.marker != falseBranch.Value.marker ||
                                trueBranch.Value.tag != falseBranch.Value.tag)
                            {
                                return null;
                            }

                            return
                                (trueBranch.Value.marker,
                                trueBranch.Value.tag,
                                new Expression.Conditional(
                                    conditional.Condition,
                                    falseBranch: falseBranch.Value.inner,
                                    trueBranch: trueBranch.Value.inner));
                        }

                    case Expression.Eval
                    {
                        Encoded: Expression.Litral encoded
                    } eval:
                        {
                            var parsed =
                                ParseExpressionWithoutResultAllocation(encoded.Value).Expression;

                            if (parsed is null)
                                return null;

                            var unwrapped =
                                TryUnwrapSingleFieldTagFromAllBranches(parsed);

                            if (unwrapped is null)
                                return null;

                            var specializedEncoded =
                                EncodeWithoutMemo(unwrapped.Value.inner);

                            return
                                (unwrapped.Value.marker,
                                unwrapped.Value.tag,
                                new Expression.Eval(
                                    MakeLiteral(specializedEncoded),
                                    eval.Environment));
                        }

                    case Expression.Label label:
                        {
                            var unwrapped =
                                TryUnwrapSingleFieldTagFromAllBranches(label.Tagged);

                            if (unwrapped is null)
                                return null;

                            return
                                (unwrapped.Value.marker,
                                unwrapped.Value.tag,
                                new Expression.Label(label.Tag, unwrapped.Value.inner));
                        }

                    case Expression.Litral or Expression.List or Expression.Environment or Expression.Builtin:
                        return null;
                    default:
                        throw new NotImplementedException("Unknown expression variant: " + expression.GetType().Name);
                }
            }

            Expression? TryReduceHeadMatchWithNonemptyCheck(
                    Expression.Conditional conditional)
            {
                if (!IsFalseLiteral(conditional.FalseBranch))
                    return null;

                if (TryParseHeadEqualsLiteral(conditional.Condition) is { } headMatch &&
                    TryParseNotEmpty(conditional.TrueBranch) is { } notEmptySource &&
                    headMatch.source == notEmptySource)
                {
                    return BuildSliceMatch(headMatch.source, offset: 0, [headMatch.expectedValue]);
                }

                if (TryParseNotEmpty(conditional.Condition) is { } conditionNotEmptySource &&
                    TryParseHeadEqualsLiteral(conditional.TrueBranch) is { } trueHeadMatch &&
                    conditionNotEmptySource == trueHeadMatch.source)
                {
                    return BuildSliceMatch(trueHeadMatch.source, offset: 0, [trueHeadMatch.expectedValue]);
                }

                return null;
            }

            Expression? TryMergeAdjacentSliceMatches(
                    Expression.Conditional conditional)
            {
                if (!IsFalseLiteral(conditional.FalseBranch) ||
                    TryParseSliceMatch(conditional.Condition) is not { } conditionMatch ||
                    TryParseSliceMatch(conditional.TrueBranch) is not { } trueMatch ||
                    conditionMatch.source != trueMatch.source)
                {
                    return null;
                }

                if (conditionMatch.offset > int.MaxValue - conditionMatch.expectedValues.Items.Length ||
                    trueMatch.offset > int.MaxValue - trueMatch.expectedValues.Items.Length)
                {
                    return null;
                }

                var conditionEnd = conditionMatch.offset + conditionMatch.expectedValues.Items.Length;
                var trueEnd = trueMatch.offset + trueMatch.expectedValues.Items.Length;

                if (Math.Min(conditionMatch.offset, trueMatch.offset) is not 0 ||
                    (conditionEnd != trueMatch.offset && trueEnd != conditionMatch.offset))
                {
                    return null;
                }

                var combinedValues = new PineValue[Math.Max(conditionEnd, trueEnd)];

                conditionMatch.expectedValues.Items.Span.CopyTo(combinedValues.AsSpan(conditionMatch.offset));
                trueMatch.expectedValues.Items.Span.CopyTo(combinedValues.AsSpan(trueMatch.offset));

                return BuildSliceMatch(conditionMatch.source, offset: 0, combinedValues);
            }

            (Expression source, PineValue expectedValue)? TryParseHeadEqualsLiteral(
                    Expression expression)
            {
                if (TryParseBinaryEqual(expression) is not { } equalArgs)
                    return null;

                if (equalArgs.left is Expression.Builtin
                    {
                        Function: nameof(BuiltinFunction.head),
                        Input: var leftSource
                    } &&
                    equalArgs.right is Expression.Litral rightLiteral &&
                    CanRepresentHeadMatchAsListSlice(rightLiteral.Value))
                {
                    return (leftSource, rightLiteral.Value);
                }

                if (equalArgs.right is Expression.Builtin
                    {
                        Function: nameof(BuiltinFunction.head),
                        Input: var rightSource
                    } &&
                    equalArgs.left is Expression.Litral leftLiteral &&
                    CanRepresentHeadMatchAsListSlice(leftLiteral.Value))
                {
                    return (rightSource, leftLiteral.Value);
                }

                return null;
            }

            bool CanRepresentHeadMatchAsListSlice(PineValue expectedValue) =>
                    expectedValue is not PineValue.BlobValue { Bytes.Length: 1 };

            Expression? TryParseNotEmpty(Expression expression)
            {
                if (TryParseBinaryEqual(expression) is not { } outerEqual)
                    return null;

                Expression? emptyCheck = null;

                if (IsFalseLiteral(outerEqual.left))
                    emptyCheck = outerEqual.right;

                if (IsFalseLiteral(outerEqual.right))
                    emptyCheck = outerEqual.left;

                if (emptyCheck is null ||
                    TryParseBinaryEqual(emptyCheck) is not { } emptyEqual)
                {
                    return null;
                }

                if (TryParseLengthComparedWithZero(emptyEqual.left, emptyEqual.right) is { } leftSource)
                    return leftSource;

                return TryParseLengthComparedWithZero(emptyEqual.right, emptyEqual.left);
            }

            Expression? TryParseLengthComparedWithZero(
                    Expression lengthExpression,
                    Expression zeroExpression)
            {
                if (lengthExpression is not Expression.Builtin
                    {
                        Function: nameof(BuiltinFunction.length),
                        Input: var source
                    } ||
                    zeroExpression is not Expression.Litral zeroLiteral ||
                    BuiltinFunction.SignedIntegerFromValueRelaxed(zeroLiteral.Value) is not { } zero ||
                    zero != 0)
                {
                    return null;
                }

                return source;
            }

            (
                    Expression source,
                    int offset,
                    PineValue.ListValue expectedValues)? TryParseSliceMatch(
                    Expression expression)
            {
                if (TryParseBinaryEqual(expression) is not { } equalArgs)
                    return null;

                if (TryParseSliceComparedWithLiteral(equalArgs.left, equalArgs.right) is { } leftMatch)
                    return leftMatch;

                return TryParseSliceComparedWithLiteral(equalArgs.right, equalArgs.left);
            }

            (
                    Expression source,
                    int offset,
                    PineValue.ListValue expectedValues)? TryParseSliceComparedWithLiteral(
                    Expression sliceExpression,
                    Expression literalExpression)
            {
                if (sliceExpression is not Expression.Builtin
                    {
                        Function: nameof(BuiltinFunction.take),
                        Input: Expression.List { Items.Count: 2 } takeArguments
                    } ||
                    takeArguments.Items[0] is not Expression.Litral takeCountLiteral ||
                    BuiltinFunction.SignedIntegerFromValueRelaxed(takeCountLiteral.Value) is not { } takeCount ||
                    takeCount < 0 ||
                    literalExpression is not Expression.Litral
                    {
                        Value: PineValue.ListValue expectedValues
                    } ||
                    takeCount != expectedValues.Items.Length)
                {
                    return null;
                }

                var source = takeArguments.Items[1];
                var offset = 0;

                if (source is Expression.Builtin
                    {
                        Function: nameof(BuiltinFunction.skip),
                        Input: Expression.List { Items.Count: 2 } skipArguments
                    } &&
                    skipArguments.Items[0] is Expression.Litral skipCountLiteral &&
                    BuiltinFunction.SignedIntegerFromValueRelaxed(skipCountLiteral.Value) is { } skipCount &&
                    skipCount >= 0 &&
                    skipCount <= int.MaxValue)
                {
                    offset = (int)skipCount;
                    source = skipArguments.Items[1];
                }

                return (source, offset, expectedValues);
            }

            (Expression left, Expression right)? TryParseBinaryEqual(
                    Expression expression)
            {
                if (expression is Expression.Builtin
                    {
                        Function: nameof(BuiltinFunction.equal),
                        Input: Expression.List { Items.Count: 2 } arguments
                    })
                {
                    return (arguments.Items[0], arguments.Items[1]);
                }

                return null;
            }

            bool IsFalseLiteral(Expression expression) =>
                    expression is Expression.Litral { Value: var value } &&
                    value == PineKernelValues.FalseValue;

            Expression BuildSliceMatch(
                    Expression source,
                    int offset,
                    ReadOnlySpan<PineValue> expectedValues)
            {
                var sliceSource = source;

                if (offset is not 0)
                {
                    sliceSource =
                        new Expression.Builtin(
                            nameof(BuiltinFunction.skip),
                            new Expression.List(
                                [
                                MakeLiteral(EncodeInteger(offset)),
                                    source
                                ]));
                }

                var slice =
                    new Expression.Builtin(
                        nameof(BuiltinFunction.take),
                        new Expression.List(
                            [
                            MakeLiteral(EncodeInteger(expectedValues.Length)),
                                sliceSource
                            ]));

                return
                    new Expression.Builtin(
                        nameof(BuiltinFunction.equal),
                        new Expression.List(
                            [
                            slice,
                                MakeLiteral(new PineValue.ListValue(expectedValues.ToArray()))
                            ]));
            }

            bool IsKnownBooleanExpression(Expression expression)
            {
                return expression switch
                {
                    Expression.Litral literal =>
                    literal.Value == PineKernelValues.TrueValue ||
                    literal.Value == PineKernelValues.FalseValue,

                    Expression.Builtin builtinExpr =>
                    builtinExpr.Function switch
                    {
                        nameof(BuiltinFunction.equal) => true,
                        nameof(BuiltinFunction.int_is_sorted_asc) => true,

                        _ => false,
                    },

                    Expression.Conditional conditional =>
                    IsKnownBooleanExpression(conditional.TrueBranch) &&
                    IsKnownBooleanExpression(conditional.FalseBranch),

                    Expression.Label labelExpr =>
                    IsKnownBooleanExpression(labelExpr.Tagged),

                    Expression.List or Expression.Environment or Expression.Eval => false,
                    _ => throw new NotImplementedException("Unknown expression variant: " + expression.GetType().Name),
                };
            }

            IReadOnlyList<Expression>? FlattenNestedBuiltinApplication(
                    string functionName,
                    IReadOnlyList<Expression> items)
            {
                var anyFlattened = false;
                var flattened = new List<Expression>(capacity: items.Count);

                for (var i = 0; i < items.Count; i++)
                {
                    var item = items[i];

                    if (item is Expression.Builtin innerBuiltin &&
                        innerBuiltin.Function == functionName &&
                        innerBuiltin.Input is Expression.List innerList)
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

            Expression? ReduceFlattenedIntegerBuiltinApplication(
                    string functionName,
                    IReadOnlyList<Expression> originalItems)
            {
                var flattenedItems =
                    FlattenNestedBuiltinApplication(functionName, originalItems);

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
                        MakeLiteral(EncodeInteger(foldedConstant)),
                            .. constants.variables
                        ];

                    changed = true;
                }

                if (functionName is nameof(BuiltinFunction.int_add) &&
                    CombineRepeatedLinearIntegerTerms(reducedItems) is { } combinedItems)
                {
                    if (combinedItems.Count is 1)
                        return combinedItems[0];

                    reducedItems = combinedItems;
                    changed = true;
                }

                if (!changed)
                {
                    return null;
                }

                var reducedExpr =
                    new Expression.Builtin(
                        functionName,
                        new Expression.List(reducedItems));

                if (!reducedExpr.ReferencesEnvironment)
                {
                    if (TryEvalIndependent(reducedExpr).Value is { } okValue)
                    {
                        return MakeLiteral(okValue);
                    }
                }

                return reducedExpr;
            }

            IReadOnlyList<Expression>? CombineRepeatedLinearIntegerTerms(
                    IReadOnlyList<Expression> items)
            {
                var groupIndexByBase = new Dictionary<Expression, int>();
                var groups = new List<(Expression Base, BigInteger Coefficient, int Count, Expression FirstTerm)>();
                var changed = false;

                for (var i = 0; i < items.Count; i++)
                {
                    var term = ParseLinearIntegerTerm(items[i]);

                    if (groupIndexByBase.TryGetValue(term.Base, out var groupIndex))
                    {
                        var group = groups[groupIndex];

                        groups[groupIndex] =
                            group with
                            {
                                Coefficient = group.Coefficient + term.Coefficient,
                                Count = group.Count + 1
                            };

                        changed = true;
                    }
                    else
                    {
                        groupIndexByBase.Add(term.Base, groups.Count);
                        groups.Add((term.Base, term.Coefficient, Count: 1, FirstTerm: items[i]));
                    }
                }

                if (!changed)
                    return null;

                var combined = new List<Expression>(capacity: groups.Count);

                foreach (var group in groups)
                {
                    if (group.Count is 1)
                    {
                        combined.Add(group.FirstTerm);
                        continue;
                    }

                    combined.Add(
                        new Expression.Builtin(
                            nameof(BuiltinFunction.int_mul),
                            new Expression.List(
                                [
                                MakeLiteral(EncodeInteger(group.Coefficient)),
                                    group.Base
                                ])));
                }

                return combined;
            }

            (Expression Base, BigInteger Coefficient) ParseLinearIntegerTerm(
                    Expression expression)
            {
                if (expression is not Expression.Builtin
                    {
                        Function: nameof(BuiltinFunction.int_mul),
                        Input: Expression.List factors
                    })
                {
                    return (expression, BigInteger.One);
                }

                var parsedFactors = CollectConstantIntegers(factors.Items);

                if (parsedFactors.variables.Count is not 1)
                    return (expression, BigInteger.One);

                var coefficient = BigInteger.One;

                foreach (var constant in parsedFactors.constants)
                {
                    coefficient *= constant;
                }

                return (parsedFactors.variables[0], coefficient);
            }

            IEnumerable<(int? lower, int? upper)> EnumerateInferListLengthBounds(
                    Expression expression,
                    PineValueClass envConstraintId)
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

                if (Analysis.TryParseAsLiteral(expression) is { } literal)
                {
                    if (literal is PineValue.ListValue literalList)
                    {
                        yield return (literalList.Items.Length, literalList.Items.Length);
                    }
                }

                if (Analysis.TryParseExprAsPathInEnv(expression) is { } itemPath)
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
                        if (TryEvalIndependent(
                            skipInputList.Items[0]).Value is { } okSkipCountValue)
                        {
                            if (IntegerEncoding.ParseSignedIntegerRelaxed(okSkipCountValue).IsOkOrNullable() is { } okSkipCount)
                            {
                                var skipCountClamped =
                                    (int)(okSkipCount < 0 ? 0 : okSkipCount);

                                foreach (var offsetBound in EnumerateInferListLengthBounds(skipInputList.Items[1], envConstraintId))
                                {
                                    yield return (offsetBound.lower - skipCountClamped, offsetBound.upper - skipCountClamped);
                                }
                            }
                        }
                    }
                }
            }

            (Expression expr, bool referencesOriginalEnv) TransformPineExpressionWithOptionalReplacement(
                    Func<Expression, Expression?> findReplacement,
                    Expression expression)
            {
                return
                    TransformCached(
                        findReplacement,
                        expression,
                        cache: []);
            }

            (Expression expr, bool referencesOriginalEnv) TransformCached(
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
                                        TransformCached(
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

                                return (new Expression.List(mappedItems), referencesOriginalEnv);
                            }

                        case Expression.Eval evalExpr:
                            {
                                var encodedTransform =
                                    TransformCached(
                                        findReplacement,
                                        evalExpr.Encoded,
                                        cache);

                                var envTransform =
                                    TransformCached(
                                        findReplacement,
                                        evalExpr.Environment,
                                        cache);

                                var referencesOriginalEnv =
                                    encodedTransform.referencesOriginalEnv ||
                                    envTransform.referencesOriginalEnv;

                                if (encodedTransform.expr == evalExpr.Encoded &&
                                    envTransform.expr == evalExpr.Environment)
                                {
                                    return (evalExpr, referencesOriginalEnv);
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
                                    TransformCached(
                                        findReplacement,
                                        kernelApp.Input,
                                        cache);

                                if (argumentTransform.expr == kernelApp.Input)
                                {
                                    return (kernelApp, argumentTransform.referencesOriginalEnv);
                                }

                                return
                                    (new Expression.Builtin(
                                        function: kernelApp.Function,
                                        input: argumentTransform.expr),
                                    argumentTransform.referencesOriginalEnv);
                            }

                        case Expression.Conditional conditional:
                            {
                                var conditionTransform =
                                    TransformCached(
                                        findReplacement,
                                        conditional.Condition,
                                        cache);

                                var trueBranchTransform =
                                    TransformCached(
                                        findReplacement,
                                        conditional.TrueBranch,
                                        cache);

                                var falseBranchTransform =
                                    TransformCached(
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
                                    (new Expression.Conditional
                                    (
                                        condition: conditionTransform.expr,
                                        falseBranch: falseBranchTransform.expr,
                                        trueBranch: trueBranchTransform.expr),
                                    referencesOriginalEnv);
                            }

                        case Expression.Environment:
                            return (expression, true);

                        case Expression.Label labelExpr:
                            {
                                var taggedTransform =
                                    TransformCached(
                                        findReplacement,
                                        labelExpr.Tagged,
                                        cache);

                                if (taggedTransform.expr == labelExpr.Tagged)
                                {
                                    return
                                        (labelExpr, taggedTransform.referencesOriginalEnv);
                                }

                                return
                                    (new Expression.Label(
                                        labelExpr.Tag,
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

            Expression SearchForExpressionReductionRecursive(
                    int maxDepth,
                    Expression expression,
                    PineValueClass? envConstraintId = null)
            {
                if (maxDepth < 1)
                    return expression;

                var transformed =
                    TransformPineExpressionWithOptionalReplacement(
                        expr => SearchForExpressionReduction(expr, envConstraintId),
                        expression).expr;

                if (transformed == expression)
                    return transformed;

                return
                    SearchForExpressionReductionRecursive(
                        maxDepth - 1,
                        transformed,
                        envConstraintId: envConstraintId);
            }

            Expression ReduceExpressionBottomUp(
                    Expression expression,
                    ReductionConfig config)
            {
                if (reducedExpressionCache is not null &&
                    reducedExpressionCache.TryGetValue((expression, config), out var cachedReducedExpression))
                {
                    return cachedReducedExpression;
                }
                var expressionWithReducedChildren =
                    expression switch
                    {
                        Expression.Litral => expression,

                        Expression.List listExpr =>
                        ReduceListExpressionBottomUp(
                            listExpr,
                            config),

                        Expression.Builtin builtinExpr =>
                        ReduceBuiltinBottomUp(
                            builtinExpr,
                            config),

                        Expression.Eval evalExpr =>
                        ReduceEvalBottomUp(
                            evalExpr,
                            config),

                        Expression.Conditional conditional =>
                        ReduceConditionalBottomUp(
                            conditional,
                            config),

                        Expression.Label labelExpr =>
                        ReduceLabelBottomUp(
                            labelExpr,
                            config),
                        Expression.Environment => expression,

                        _ =>
                        throw new NotImplementedException(
                            $"Expression type not implemented: {expression.GetType().FullName}")
                    };
                {
                    var reduced =
                        SearchForExpressionReduction(expressionWithReducedChildren, envConstraintId: null);

                    if (reduced is not null)
                    {
                        reducedExpressionCache?[(expression, config)] = reduced;

                        return reduced;
                    }
                }
                reducedExpressionCache?[(expression, config)] = expressionWithReducedChildren;

                return expressionWithReducedChildren;
            }

            Expression ReduceListExpressionBottomUp(
                    Expression.List listExpr,
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
                            config);

                    newItems[i] = reducedChild;

                    changed =
                        changed || reducedChild != items[i];
                }
                return
                    changed
                    ?
                    new Expression.List(newItems)
                    :
                    listExpr;
            }

            Expression ReduceBuiltinBottomUp(
                    Expression.Builtin builtinExpr,
                    ReductionConfig config = default)
            {
                var reducedArg =
                    ReduceExpressionBottomUp(
                        builtinExpr.Input,
                        config);

                if (builtinExpr.Function is nameof(BuiltinFunction.int_mul) &&
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
                                    MakeLiteral(EncodeInteger(product))
                            };

                        newItems.AddRange(constants.variables);

                        reducedArg = new Expression.List(newItems);
                    }
                }

                if (builtinExpr.Function is nameof(BuiltinFunction.int_add) &&
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
                                    MakeLiteral(EncodeInteger(sum))
                            };

                        newItems.AddRange(constants.variables);

                        reducedArg = new Expression.List(newItems);
                    }
                }

                if (reducedArg == builtinExpr.Input)
                    return builtinExpr;

                return new Expression.Builtin(builtinExpr.Function, reducedArg);
            }

            (IReadOnlyList<BigInteger> constants, IReadOnlyList<Expression> variables) CollectConstantIntegers(
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

            Expression ReduceEvalBottomUp(
                    Expression.Eval evalExpr,
                    ReductionConfig config = default)
            {
                var reducedEncoded =
                    ReduceExpressionBottomUp(
                        evalExpr.Encoded,
                        config);

                var reducedEnv =
                    ReduceExpressionBottomUp(
                        evalExpr.Environment,
                        config);

                var reduced =
                    (reducedEncoded == evalExpr.Encoded && reducedEnv == evalExpr.Environment)
                    ?
                    evalExpr
                    :
                    new Expression.Eval(encoded: reducedEncoded, environment: reducedEnv);
                if (TryConsolidateGenericFunctionApplicationChain(
                        reduced,
                        config) is { } consolidated)
                {
                    var reducedConsolidated =
                        ReduceExpressionBottomUp(
                            consolidated,
                            config);

                    return reducedConsolidated;
                }

                if (TryInlineEvalBottomUp(reduced, config) is { } inlined)
                {
                    return inlined;
                }

                return reduced;
            }

            Expression? TryInlineEvalBottomUp(
                    Expression.Eval evalExpr,
                    ReductionConfig config)
            {
                if (config.InlineEvalEnvironmentSizeLimit <= evalExpr.SubexpressionCount)
                    return null;

                if (evalExpr.Encoded.ReferencesEnvironment)
                    return null;

                if (TryEvalIndependent(
                    evalExpr.Encoded).Value is not { } encodedExprValue)
                    return null;

                if (ParseExpressionWithoutResultAllocation(encodedExprValue).Expression is not { } innerExpr)
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
                        config);

                var substituted =
                    SubstituteEnvironmentNode(
                        expression: innerExprReduced,
                        environmentReplacement: evalExpr.Environment);

                var reducedViaEval =
                    ReduceExpressionBottomUp(
                        substituted,
                        config);

                var evalCountAfter = reducedViaEval.EvalCount;

                if (evalCountAfter > evalCountBefore)
                {
                    return null;
                }

                return reducedViaEval;
            }

            Expression? TryConsolidateGenericFunctionApplicationChain(
                    Expression.Eval evalExpr,
                    ReductionConfig config = default)
            {
                if (config.DisableGenericApplicationChainConsolidation)
                {
                    return null;
                }

                if (Analysis.ParseGenericFunctionApplication(evalExpr) is not { } chain)
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

                if (ParseExpression(functionLiteral.Value).IsOkOrNull() is not { } functionBody)
                {
                    return null;
                }
                var currentExpr = SubstituteEnvironmentNode(functionBody, arguments[0]);
                for (var i = 1; i < arguments.Count; ++i)
                {
                    if (TryDecodeApplicationOfConstructedEncoding(currentExpr, arguments[i]) is not { } decoded)
                    {
                        return null;
                    }

                    currentExpr = decoded;
                }

                return currentExpr;
            }

            Expression? TryDecodeApplicationOfConstructedEncoding(
                    Expression construction,
                    Expression envArg)
            {
                if (construction is Expression.Eval innerPe &&
                    innerPe.Encoded is Expression.Litral innerEncodedLiteral)
                {
                    if (ParseExpression(innerEncodedLiteral.Value).IsOkOrNull() is { } innerFunctionBody)
                    {
                        var inlined =
                            SubstituteEnvironmentNode(innerFunctionBody, innerPe.Environment);

                        return TryDecodeApplicationOfConstructedEncoding(inlined, envArg);
                    }

                    return null;
                }
                if (construction is Expression.Litral constructionLiteral)
                {
                    if (ParseExpression(constructionLiteral.Value).IsOkOrNull() is { } parsedInner)
                    {
                        return SubstituteEnvironmentNode(parsedInner, envArg);
                    }

                    return null;
                }
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

                if (TryDecodeConstructedEncoding2026(constructionList, tag, envArg) is { } decoded2026)
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
                            if (tagArguments.Items.Count is not 1)
                            {
                                return null;
                            }

                            return tagArguments.Items[0];
                        }

                    case "Environment":
                        {
                            if (tagArguments.Items.Count is not 0)
                            {
                                return null;
                            }

                            return envArg;
                        }

                    case "List":
                        {
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
                                        envArg) is not { } decodedItem)
                                {
                                    return null;
                                }

                                decodedItems[i] = decodedItem;
                            }

                            return new Expression.List(decodedItems);
                        }

                    case "ParseAndEval":
                        {
                            if (tagArguments.Items.Count is not 2)
                            {
                                return null;
                            }

                            if (TryDecodeApplicationOfConstructedEncoding(
                                    tagArguments.Items[0],
                                    envArg) is not { } decodedEncoded)
                            {
                                return null;
                            }

                            if (TryDecodeApplicationOfConstructedEncoding(
                                    tagArguments.Items[1],
                                    envArg) is not { } decodedEnv)
                            {
                                return null;
                            }

                            return new Expression.Eval(decodedEncoded, decodedEnv);
                        }

                    case "KernelApplication":
                        {
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
                                    envArg) is not { } decodedInput)
                            {
                                return null;
                            }

                            return new Expression.Builtin(functionName, decodedInput);
                        }

                    default:
                        return null;
                }
            }

            Expression? TryDecodeConstructedEncoding2026(
                    Expression.List construction,
                    string tag,
                    Expression envArg)
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
                                        envArg) is not { } decodedItem)
                                {
                                    return null;
                                }

                                decodedItems[i - 1] = decodedItem;
                            }

                            return new Expression.List(decodedItems);
                        }

                    case "Builtin":
                        {
                            if (construction.Items.Count is not 3 ||
                                construction.Items[1] is not Expression.Litral functionLiteral ||
                                StringEncoding.StringFromValue(functionLiteral.Value).IsOkOrNull() is not { } functionName ||
                                TryDecodeApplicationOfConstructedEncoding(
                                    construction.Items[2],
                                    envArg) is not { } decodedInput)
                            {
                                return null;
                            }

                            return new Expression.Builtin(functionName, decodedInput);
                        }

                    case "Conditional" or "Condition":
                        {
                            if (construction.Items.Count is not 4 ||
                                TryDecodeApplicationOfConstructedEncoding(
                                    construction.Items[1],
                                    envArg) is not { } decodedCondition ||
                                TryDecodeApplicationOfConstructedEncoding(
                                    construction.Items[2],
                                    envArg) is not { } decodedFalseBranch ||
                                TryDecodeApplicationOfConstructedEncoding(
                                    construction.Items[3],
                                    envArg) is not { } decodedTrueBranch)
                            {
                                return null;
                            }

                            return
                                new Expression.Conditional(
                                    decodedCondition,
                                    decodedFalseBranch,
                                    decodedTrueBranch);
                        }

                    case "Eval":
                        {
                            if (construction.Items.Count is not 3 ||
                                TryDecodeApplicationOfConstructedEncoding(
                                    construction.Items[1],
                                    envArg) is not { } decodedEncoded ||
                                TryDecodeApplicationOfConstructedEncoding(
                                    construction.Items[2],
                                    envArg) is not { } decodedEnvironment)
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
                                    envArg) is not { } decodedLabeled)
                            {
                                return null;
                            }

                            return new Expression.Label(labelLiteral.Value, decodedLabeled);
                        }

                    default:
                        return null;
                }
            }

            Expression ReduceConditionalBottomUp(
                    Expression.Conditional conditional,
                    ReductionConfig config = default)
            {
                var reducedCondition =
                    ReduceExpressionBottomUp(
                        conditional.Condition,
                        config);

                var reducedTrue =
                    ReduceExpressionBottomUp(
                        conditional.TrueBranch,
                        config);

                reducedTrue =
                    ReduceBranchUnderAssumption(
                        reducedTrue,
                        reducedCondition,
                        assumedValue: true,
                        config);

                var reducedFalse =
                    ReduceExpressionBottomUp(
                        conditional.FalseBranch,
                        config);

                reducedFalse =
                    ReduceBranchUnderAssumption(
                        reducedFalse,
                        reducedCondition,
                        assumedValue: false,
                        config);

                if (reducedCondition == conditional.Condition &&
                    reducedTrue == conditional.TrueBranch &&
                    reducedFalse == conditional.FalseBranch)
                {
                    return conditional;
                }

                return
                    new Expression.Conditional(
                        condition: reducedCondition,
                        falseBranch: reducedFalse,
                        trueBranch: reducedTrue);
            }

            Expression ReduceBranchUnderAssumption(
                    Expression branch,
                    Expression condition,
                    bool assumedValue,
                    ReductionConfig config)
            {
                var specialized =
                    TransformPineExpressionWithOptionalReplacement(
                        expression =>
                        TryProveBooleanExpression(
                            expression,
                            condition,
                            assumedValue) is { } provenValue
                        ?
                        MakeLiteral(
                            provenValue
                            ?
                            PineKernelValues.TrueValue
                            :
                            PineKernelValues.FalseValue)
                        :
                        null,
                        branch).expr;

                if (specialized == branch)
                    return branch;

                return
                    ReduceExpressionBottomUp(
                        specialized,
                        config);
            }

            bool? TryProveBooleanExpression(
                    Expression expression,
                    Expression condition,
                    bool assumedValue)
            {
                if (assumedValue && expression == condition)
                    return true;

                if (!assumedValue ||
                    expression is not Expression.Builtin
                    {
                        Function: nameof(BuiltinFunction.int_is_sorted_asc),
                        Input: Expression.List expressionOperands
                    } ||
                    condition is not Expression.Builtin
                    {
                        Function: nameof(BuiltinFunction.int_is_sorted_asc),
                        Input: Expression.List conditionOperands
                    })
                {
                    return null;
                }

                if (expressionOperands.Items.Count is 1 &&
                    !IsKnownIntegerExpression(
                        expressionOperands.Items[0],
                        conditionOperands.Items))
                {
                    return null;
                }

                for (var operandIndex = 1; operandIndex < expressionOperands.Items.Count; operandIndex++)
                {
                    if (!TryProveIntegerLessThanOrEqual(
                        expressionOperands.Items[operandIndex - 1],
                        expressionOperands.Items[operandIndex],
                        conditionOperands.Items))
                    {
                        return null;
                    }
                }

                return true;
            }

            bool TryProveIntegerLessThanOrEqual(
                    Expression left,
                    Expression right,
                    IReadOnlyList<Expression> sortedExpressions)
            {
                if (left == right)
                    return IsKnownIntegerExpression(left, sortedExpressions);

                for (var expressionIndex = 1; expressionIndex < sortedExpressions.Count; expressionIndex++)
                {
                    if (sortedExpressions[expressionIndex - 1] == left &&
                        sortedExpressions[expressionIndex] == right)
                    {
                        return true;
                    }
                }

                if (right is not Expression.Builtin
                    {
                        Function: nameof(BuiltinFunction.int_add),
                        Input: Expression.List addends
                    } ||
                    !IsKnownIntegerExpression(left, sortedExpressions))
                {
                    return false;
                }

                var matchedLeft = false;

                foreach (var addend in addends.Items)
                {
                    if (!matchedLeft && addend == left)
                    {
                        matchedLeft = true;
                        continue;
                    }

                    if (addend is not Expression.Litral literal ||
                        BuiltinFunction.SignedIntegerFromValueRelaxed(literal.Value) is not { } integer ||
                        integer < 0)
                    {
                        return false;
                    }
                }

                return matchedLeft;
            }

            bool IsKnownIntegerExpression(
                    Expression expression,
                    IReadOnlyList<Expression> sortedExpressions) =>
                    expression is Expression.Litral literal &&
                    BuiltinFunction.SignedIntegerFromValueRelaxed(literal.Value) is not null ||
                    sortedExpressions.Contains(expression);

            Expression ReduceLabelBottomUp(
                    Expression.Label labelExpr,
                    ReductionConfig config = default)
            {
                var reducedTagged =
                    ReduceExpressionBottomUp(
                        labelExpr.Tagged,
                        config);

                if (reducedTagged == labelExpr.Tagged)
                    return labelExpr;

                return new Expression.Label(labelExpr.Tag, reducedTagged);
            }

            Expression? ApplyBuiltinFunctionReverseToAllBranches(
                    Expression expression)
            {
                switch (expression)
                {
                    case Expression.List listExpr:
                        {
                            if (listExpr.Items.Count <= 1)
                                return listExpr;

                            var reversed = listExpr.Items.Reverse().ToArray();

                            return new Expression.List(reversed);
                        }

                    case Expression.Litral literal:
                        return MakeLiteral(BuiltinFunction.reverse(literal.Value));

                    case Expression.Builtin innerBuiltinExpr:
                        {
                            if (innerBuiltinExpr.Function is nameof(BuiltinFunction.reverse))
                            {
                                return innerBuiltinExpr.Input;
                            }

                            return null;
                        }

                    case Expression.Conditional cond:
                        {
                            if (ApplyBuiltinFunctionReverseToAllBranches(cond.FalseBranch) is { } falseOk &&
                                ApplyBuiltinFunctionReverseToAllBranches(cond.TrueBranch) is { } trueOk)
                            {
                                return
                                    new Expression.Conditional(
                                        cond.Condition,
                                        falseBranch: falseOk,
                                        trueBranch: trueOk);
                            }

                            return null;
                        }

                    case Expression.Label tag:
                        {
                            if (ApplyBuiltinFunctionReverseToAllBranches(tag.Tagged) is { } inner)
                            {
                                return new Expression.Label(tag.Tag, inner);
                            }

                            return null;
                        }
                }

                return null;
            }

            Expression? ApplyBuiltinFunctionSkipToAllBranches(
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
                                return MakeLiteral(PineValue.EmptyList);

                            return new Expression.List([.. list.Items.Skip(countClamped)]);
                        }

                    case Expression.Litral literal:
                        return MakeLiteral(BuiltinFunctionSpecialized.skip(countClamped, literal.Value));

                    case Expression.Builtin innerSkip:
                        {
                            if (innerSkip.Function is nameof(BuiltinFunction.skip) &&
                                innerSkip.Input is Expression.List args && args.Items.Count is 2 &&
                                args.Items[0] is Expression.Litral litCount &&
                                BuiltinFunction.SignedIntegerFromValueRelaxed(litCount.Value) is { } innerCount)
                            {
                                if (innerCount > int.MaxValue)
                                    return null;

                                var innerCountClamped =
                                    innerCount < 0 ? 0 : (int)innerCount;

                                if (innerCountClamped > int.MaxValue - countClamped)
                                    return null;

                                var combinedCount = countClamped + innerCountClamped;

                                return
                                    ApplyBuiltinFunctionSkipToAllBranches(combinedCount, args.Items[1])
                                    ??
                                    new Expression.Builtin(
                                        nameof(BuiltinFunction.skip),
                                        new Expression.List(
                                            [
                                            MakeLiteral(EncodeInteger(combinedCount)),
                                                args.Items[1]
                                            ]));
                            }

                            return null;
                        }

                    case Expression.Conditional conditional:
                        {
                            if (ApplyBuiltinFunctionSkipToAllBranches(countClamped, conditional.FalseBranch) is { } falseOk &&
                                ApplyBuiltinFunctionSkipToAllBranches(countClamped, conditional.TrueBranch) is { } trueOk)
                            {
                                return
                                    new Expression.Conditional(
                                        conditional.Condition,
                                        falseBranch: falseOk,
                                        trueBranch: trueOk);
                            }

                            return null;
                        }

                    case Expression.Label tag:
                        {
                            if (ApplyBuiltinFunctionSkipToAllBranches(countClamped, tag.Tagged) is { } taggedOk)
                            {
                                return new Expression.Label(tag.Tag, taggedOk);
                            }

                            return null;
                        }
                }

                return null;
            }

            Expression? ApplyBuiltinFunctionTakeToAllBranches(
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
                                return MakeLiteral(PineValue.EmptyList);

                            if (countClamped >= list.Items.Count)
                                return list;

                            return new Expression.List([.. list.Items.Take(countClamped)]);
                        }

                    case Expression.Litral literal:
                        return MakeLiteral(BuiltinFunctionSpecialized.take(countClamped, literal.Value));

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
                                    ApplyBuiltinFunctionTakeToAllBranches(
                                        countClamped + innerCountClamped,
                                        args.Items[1]);
                            }

                            return null;
                        }

                    case Expression.Conditional conditional:
                        {
                            if (ApplyBuiltinFunctionTakeToAllBranches(countClamped, conditional.FalseBranch) is { } falseOk &&
                                ApplyBuiltinFunctionTakeToAllBranches(countClamped, conditional.TrueBranch) is { } trueOk)
                            {
                                return
                                    new Expression.Conditional(
                                        conditional.Condition,
                                        falseBranch: falseOk,
                                        trueBranch: trueOk);
                            }

                            return null;
                        }

                    case Expression.Label tag:
                        {
                            if (ApplyBuiltinFunctionTakeToAllBranches(countClamped, tag.Tagged) is { } taggedOk)
                            {
                                return new Expression.Label(tag.Tag, taggedOk);
                            }

                            return null;
                        }
                }

                return null;
            }

            Expression? ApplyBuiltinFunctionHeadToAllBranches(Expression expression)
            {
                if (TryReduceSelectListItem(expression, 0) is { } selected)
                {
                    return selected;
                }

                switch (expression)
                {
                    case Expression.Conditional cond:
                        {
                            if (ApplyBuiltinFunctionHeadToAllBranches(cond.FalseBranch) is { } falseOk &&
                                ApplyBuiltinFunctionHeadToAllBranches(cond.TrueBranch) is { } trueOk)
                            {
                                return
                                    new Expression.Conditional(
                                        condition: cond.Condition,
                                        falseBranch: falseOk,
                                        trueBranch: trueOk);
                            }

                            return null;
                        }

                    case Expression.Label tag:
                        {
                            if (ApplyBuiltinFunctionHeadToAllBranches(tag.Tagged) is { } taggedOk)
                            {
                                return new Expression.Label(tag.Tag, taggedOk);
                            }

                            return null;
                        }
                }

                return null;
            }

            Expression? TryReduceSelectListItem(Expression inputExpression, int index)
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

                            return MakeLiteral(PineValue.EmptyList);
                        }

                    case Expression.Litral lit:
                        {
                            if (lit.Value is PineValue.ListValue lv)
                            {
                                if (index < lv.Items.Length)
                                {
                                    return MakeLiteral(lv.Items.Span[index]);
                                }

                                return MakeLiteral(PineValue.EmptyList);
                            }
                            return null;
                        }

                    case Expression.Conditional cond:
                        {
                            var falseOut = TryReduceSelectListItem(cond.FalseBranch, index);
                            var trueOut = TryReduceSelectListItem(cond.TrueBranch, index);

                            if (falseOut is not null && trueOut is not null)
                            {
                                return
                                    new Expression.Conditional(
                                        condition: cond.Condition,
                                        falseBranch: falseOut,
                                        trueBranch: trueOut);
                            }

                            return null;
                        }

                    case Expression.Label label:
                        {
                            return TryReduceSelectListItem(label.Tagged, index);
                        }
                }

                return null;
            }

            Expression InlineStaticInvocationsAndReduceRecursive(
                    Expression currentExpression,
                    ImmutableStack<Expression> inlinedParents,
                    int maxDepth,
                    int maxSubexpressionCount,
                    bool disableRecurseAfterInline,
                    Func<Expression, bool> skipInlining,
                    int pathMaxLowExclusive = ExpressionCompilation.DefaultPathMaxLowExclusive,
                    int pathMaxHighInclusive = ExpressionCompilation.DefaultPathMaxHighInclusive,
                    bool disableGenericApplicationChainConsolidation = false)
            {
                var expressionReduced =
                    ReduceExpressionBottomUp(
                        currentExpression,
                        ReductionConfig.Default
                        with
                        {
                            DisableGenericApplicationChainConsolidation = disableGenericApplicationChainConsolidation
                        });

                if (maxDepth <= 0)
                {
                    return expressionReduced;
                }

                if (maxSubexpressionCount < expressionReduced.SubexpressionCount)
                {
                    return expressionReduced;
                }

                Expression? TryInlineEval(
                    Expression.Eval evalExpr)
                {
                    if (evalExpr.Encoded.ReferencesEnvironment)
                    {
                        return null;
                    }

                    if (TryEvalIndependent(
                        evalExpr.Encoded).Value is not { } exprValue)
                    {
                        return null;
                    }

                    if (ParseExpressionWithoutResultAllocation(exprValue).Expression is not { } parseOk)
                    {
                        return null;
                    }

                    if (skipInlining(parseOk))
                    {
                        return null;
                    }

                    if (inlinedParents.Contains(parseOk))
                    {
                        return null;
                    }

                    var inlinedExpr =
                        SubstituteEnvironmentNode(
                            parseOk,
                            evalExpr.Environment);

                    if (disableRecurseAfterInline)
                    {
                        return inlinedExpr;
                    }

                    var inlinedExprReduced =
                        ReduceExpressionBottomUp(
                            inlinedExpr,
                            ReductionConfig.Default
                            with
                            {
                                DisableGenericApplicationChainConsolidation = disableGenericApplicationChainConsolidation
                            });

                    bool IsExpansionCandidateForFirstInliner(Expression.Eval pe)
                    {
                        if (pe.Encoded.ReferencesEnvironment)
                        {
                            return false;
                        }

                        if (TryEvalIndependent(
                            pe.Encoded).Value is not { } peValue)
                        {
                            return false;
                        }

                        if (ParseExpressionWithoutResultAllocation(peValue).Expression is not { } peParsed)
                        {
                            return false;
                        }

                        if (skipInlining(peParsed))
                        {
                            return false;
                        }

                        return true;
                    }

                    {
                        var pmPre = ComputeEvalPathMax(inlinedExprReduced, IsExpansionCandidateForFirstInliner);

                        if (pathMaxLowExclusive < pmPre && pmPre <= pathMaxHighInclusive)
                        {
                            return null;
                        }
                    }

                    var inlinedFinal =
                        InlineStaticInvocationsAndReduceRecursive(
                            currentExpression: inlinedExprReduced,
                            inlinedParents: inlinedParents.Push(parseOk),
                            maxDepth: maxDepth - 1,
                            maxSubexpressionCount: maxSubexpressionCount,
                            skipInlining: skipInlining,
                            disableRecurseAfterInline: disableRecurseAfterInline,
                            pathMaxLowExclusive: pathMaxLowExclusive,
                            pathMaxHighInclusive: pathMaxHighInclusive,
                            disableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation);

                    return inlinedFinal;
                }

                var expressionInlined =
                    InlineEvalRecursive(
                        expressionReduced,
                        conditionCount: 0,
                        (eval, _) => TryInlineEval(eval));

                var expressionInlinedReduced =
                    ReduceExpressionBottomUp(
                        expressionInlined,
                        ReductionConfig.Default
                        with
                        {
                            DisableGenericApplicationChainConsolidation = disableGenericApplicationChainConsolidation
                        });

                return expressionInlinedReduced;
            }

            Expression InlineEvalRecursive(
                    Expression expression,
                    int conditionCount,
                    Func<Expression.Eval, int, Expression?> tryInlineEval)
            {
                if (expression.EvalCount is 0)
                    return expression;

                return InlineEvalRecursiveWithCache(expression, conditionCount, tryInlineEval, cache: []);
            }

            Expression InlineEvalRecursiveWithCache(
                    Expression expression,
                    int conditionCount,
                    Func<Expression.Eval, int, Expression?> tryInlineEval,
                    Dictionary<(Expression expression, int conditionCount), Expression> cache)
            {
                if (expression.EvalCount is 0)
                    return expression;

                var cacheKey = (expression, conditionCount);

                if (cache.TryGetValue(cacheKey, out var cached))
                    return cached;

                Expression inlined;

                if (expression is Expression.Eval evalExpr &&
                    tryInlineEval(evalExpr, conditionCount) is { } inlinedEval)
                {
                    inlined = inlinedEval;
                }
                else
                {
                    inlined =
                        expression switch
                        {
                            Expression.List list =>
                            InlineList(list),

                            Expression.Eval eval =>
                            InlineEvalChildren(eval),

                            Expression.Builtin builtin =>
                            InlineBuiltin(builtin),

                            Expression.Conditional conditional =>
                            InlineConditional(conditional),

                            Expression.Label label =>
                            InlineLabel(label),

                            Expression.Litral or Expression.Environment =>
                            expression,

                            _ =>
                            throw new NotImplementedException(
                                "Expression type not implemented: " + expression.GetType().FullName)
                        };
                }

                cache[cacheKey] = inlined;

                return inlined;

                Expression InlineList(Expression.List list)
                {
                    Expression[]? inlinedItems = null;

                    for (var i = 0; i < list.Items.Count; ++i)
                    {
                        var item = list.Items[i];

                        var inlinedItem =
                            InlineEvalRecursiveWithCache(
                                item,
                                conditionCount,
                                tryInlineEval,
                                cache);

                        if (inlinedItems is null)
                        {
                            if (inlinedItem == item)
                                continue;

                            inlinedItems = new Expression[list.Items.Count];

                            for (var copiedIndex = 0; copiedIndex < i; ++copiedIndex)
                                inlinedItems[copiedIndex] = list.Items[copiedIndex];
                        }

                        inlinedItems[i] = inlinedItem;
                    }

                    return
                        inlinedItems is null
                        ?
                        list
                        :
                        new Expression.List(inlinedItems);
                }

                Expression InlineEvalChildren(Expression.Eval eval)
                {
                    var encodedInlined =
                        InlineEvalRecursiveWithCache(
                            eval.Encoded,
                            conditionCount,
                            tryInlineEval,
                            cache);

                    var environmentInlined =
                        InlineEvalRecursiveWithCache(
                            eval.Environment,
                            conditionCount,
                            tryInlineEval,
                            cache);

                    if (encodedInlined == eval.Encoded &&
                        environmentInlined == eval.Environment)
                    {
                        return eval;
                    }

                    return new Expression.Eval(encodedInlined, environmentInlined);
                }

                Expression InlineBuiltin(Expression.Builtin builtin)
                {
                    var inputInlined =
                        InlineEvalRecursiveWithCache(
                            builtin.Input,
                            conditionCount,
                            tryInlineEval,
                            cache);

                    if (inputInlined == builtin.Input)
                        return builtin;

                    return new Expression.Builtin(builtin.Function, inputInlined);
                }

                Expression InlineConditional(Expression.Conditional conditional)
                {

                    var conditionInlined =
                        InlineEvalRecursiveWithCache(
                            conditional.Condition,
                            conditionCount,
                            tryInlineEval,
                            cache);

                    var falseBranchInlined =
                        InlineEvalRecursiveWithCache(
                            conditional.FalseBranch,
                            conditionCount: conditionCount + 1,
                            tryInlineEval,
                            cache);

                    var trueBranchInlined =
                        InlineEvalRecursiveWithCache(
                            conditional.TrueBranch,
                            conditionCount: conditionCount + 1,
                            tryInlineEval,
                            cache);

                    if (conditionInlined == conditional.Condition &&
                        falseBranchInlined == conditional.FalseBranch &&
                        trueBranchInlined == conditional.TrueBranch)
                    {
                        return conditional;
                    }

                    return
                        new Expression.Conditional(
                            condition: conditionInlined,
                            falseBranch: falseBranchInlined,
                            trueBranch: trueBranchInlined);
                }

                Expression InlineLabel(Expression.Label label)
                {
                    var taggedInlined =
                        InlineEvalRecursiveWithCache(
                            label.Tagged,
                            conditionCount,
                            tryInlineEval,
                            cache);

                    if (taggedInlined == label.Tagged)
                        return label;

                    return new Expression.Label(label.Tag, taggedInlined);
                }
            }

            int ComputeEvalPathMax(
                    Expression expression,
                    Func<Expression.Eval, bool> isExpansionCandidate)
            {
                switch (expression)
                {
                    case Expression.Conditional conditional:
                        {
                            var condP = ComputeEvalPathMax(conditional.Condition, isExpansionCandidate);
                            var trueP = ComputeEvalPathMax(conditional.TrueBranch, isExpansionCandidate);
                            var falseP = ComputeEvalPathMax(conditional.FalseBranch, isExpansionCandidate);

                            return condP + Math.Max(trueP, falseP);
                        }

                    case Expression.Eval evalExpr:
                        {
                            var encP = ComputeEvalPathMax(evalExpr.Encoded, isExpansionCandidate);
                            var envP = ComputeEvalPathMax(evalExpr.Environment, isExpansionCandidate);

                            var selfContribution =
                                isExpansionCandidate(evalExpr) ? 1 : 0;

                            return selfContribution + encP + envP;
                        }

                    case Expression.List list:
                        {
                            var sumP = 0;

                            for (var i = 0; i < list.Items.Count; i++)
                            {
                                sumP += ComputeEvalPathMax(list.Items[i], isExpansionCandidate);
                            }

                            return sumP;
                        }

                    case Expression.Builtin kernelApp:
                        return ComputeEvalPathMax(kernelApp.Input, isExpansionCandidate);

                    case Expression.Label stringTag:
                        return ComputeEvalPathMax(stringTag.Tagged, isExpansionCandidate);

                    case Expression.Litral or Expression.Environment:
                        return 0;
                    default:
                        throw new NotImplementedException("Unknown expression variant: " + expression.GetType().Name);
                }
            }

            Expression ReduceExpressionAndInlineRecursive(
                    Expression currentExpression,
                    ImmutableStack<Expression> inlinedParents,
                    PineValueClass? envConstraintId,
                    ImmutableHashSet<Expression> rootExprForms,
                    int maxDepth,
                    int maxSubexpressionCount,
                    bool disableRecurseAfterInline,
                    Func<Expression, PineValueClass?, bool> skipInlining,
                    int pathMaxLowExclusive = ExpressionCompilation.DefaultPathMaxLowExclusive,
                    int pathMaxHighInclusive = ExpressionCompilation.DefaultPathMaxHighInclusive,
                    bool disableGenericApplicationChainConsolidation = false)
            {
                var expressionSubstituted =
                    envConstraintId is null
                    ?
                    currentExpression
                    :
                    SubstituteSubexpressionsForEnvironmentConstraint(
                        currentExpression,
                        envConstraintId);

                var expressionReduced =
                    ReduceExpressionBottomUp(
                        expressionSubstituted,
                        ReductionConfig.Default
                        with
                        {
                            DisableGenericApplicationChainConsolidation = disableGenericApplicationChainConsolidation
                        });

                if (maxDepth <= 0)
                {
                    return expressionReduced;
                }

                if (envConstraintId is null)
                {

                }

                if (maxSubexpressionCount < expressionReduced.SubexpressionCount)
                {
                    return expressionReduced;
                }

                Expression? TryInlineEval(
                    Expression.Eval evalExpr,
                    bool noRecursion)
                {
                    Expression? ContinueReduceForKnownExprValue(PineValue exprValue)
                    {
                        if (ParseExpression(exprValue).IsOkOrNull() is not { } parseOk)
                        {
                            return null;
                        }

                        if (skipInlining(parseOk, envConstraintId))
                        {
                            return null;
                        }

                        if (noRecursion)
                        {
                            if (inlinedParents.Contains(parseOk))
                            {
                                return null;
                            }
                        }

                        var inlinedExpr =
                            SubstituteEnvironmentNode(
                                expression: parseOk,
                                environmentReplacement: evalExpr.Environment);

                        if (disableRecurseAfterInline)
                        {
                            return inlinedExpr;
                        }

                        var inlinedExprSubstituted =
                            envConstraintId is null
                            ?
                            inlinedExpr
                            :
                            SubstituteSubexpressionsForEnvironmentConstraint(inlinedExpr, envConstraintId);

                        var inlinedExprReduced =
                            ReduceExpressionBottomUp(
                                inlinedExprSubstituted,
                                ReductionConfig.Default
                                with
                                {
                                    DisableGenericApplicationChainConsolidation = disableGenericApplicationChainConsolidation,
                                });

                        bool IsExpansionCandidateForSecondInliner(Expression.Eval pe)
                        {
                            if (pe.Encoded.ReferencesEnvironment)
                            {
                                return false;
                            }

                            if (TryEvalIndependent(
                                pe.Encoded).Value is not { } peValue)
                            {
                                return false;
                            }

                            if (ParseExpressionWithoutResultAllocation(peValue).Expression is not { } peParsed)
                            {
                                return false;
                            }

                            if (skipInlining(peParsed, envConstraintId))
                            {
                                return false;
                            }

                            return true;
                        }

                        {
                            if (500 < inlinedExprReduced.SubexpressionCount)
                            {
                                return null;
                            }

                            var pathInvocations =
                                ComputeEvalPathMax(
                                    inlinedExprReduced,
                                    IsExpansionCandidateForSecondInliner);

                            if (pathMaxLowExclusive < pathInvocations && pathInvocations <= pathMaxHighInclusive)
                            {
                                return null;
                            }
                        }

                        var inlinedFinal =
                            ReduceExpressionAndInlineRecursive(
                                currentExpression: inlinedExprReduced,
                                inlinedParents: inlinedParents.Push(parseOk),
                                rootExprForms: rootExprForms,
                                envConstraintId: envConstraintId,
                                maxDepth: maxDepth - 1,
                                maxSubexpressionCount: maxSubexpressionCount,
                                skipInlining: skipInlining,
                                disableRecurseAfterInline: disableRecurseAfterInline,
                                pathMaxLowExclusive: pathMaxLowExclusive,
                                pathMaxHighInclusive: pathMaxHighInclusive,
                                disableGenericApplicationChainConsolidation: disableGenericApplicationChainConsolidation);

                        {
                            if (500 < inlinedFinal.SubexpressionCount)
                            {
                                return null;
                            }
                        }

                        return inlinedFinal;
                    }

                    if (!evalExpr.Encoded.ReferencesEnvironment)
                    {
                        if (TryEvalIndependent(
                            evalExpr.Encoded).Value is { } evalExprOk)
                        {
                            return ContinueReduceForKnownExprValue(evalExprOk);
                        }
                    }

                    return null;
                }

                var expressionInlined =
                    InlineEvalRecursive(
                        expressionReduced,
                        conditionCount: 0,
                        (eval, conditionCount) =>
                        TryInlineEval(eval, noRecursion: 0 < conditionCount));

                var expressionInlinedReduced =
                    ReduceExpressionBottomUp(
                        expressionInlined,
                        ReductionConfig.Default
                        with
                        {
                            DisableGenericApplicationChainConsolidation = disableGenericApplicationChainConsolidation
                        });

                return expressionInlinedReduced;
            }

            Expression SubstituteSubexpressionsForEnvironmentConstraint(
                    Expression originalExpression,
                    PineValueClass envConstraintId)
            {
                return
                    TransformPineExpressionWithOptionalReplacement(
                        findReplacement:
                        descendant =>
                        {
                            if (descendant is Expression.Litral)
                                return null;

                            if (Analysis.TryParseAsLiteral(descendant) is { } literal)
                            {
                                return MakeLiteral(literal);
                            }

                            if (Analysis.TryParseExprAsPathInEnv(descendant) is { } pathInEnv)
                            {
                                if (envConstraintId.TryGetValue(pathInEnv) is { } value)
                                {
                                    return MakeLiteral(value);
                                }
                            }

                            return null;
                        },
                        originalExpression).expr;
            }
        }
    }
    private readonly struct ValueEvalResult
    {
        private ValueEvalResult(PineValue? value, string? error)
        {
            Value = value;
            Error = error;
        }

        public PineValue? Value { get; }

        public string? Error { get; }

        public bool IsOk => Value is not null;

        public static ValueEvalResult Ok(PineValue value) =>
            new(value, error: null);

        public static ValueEvalResult Err(string error) =>
            new(value: null, error);

        public Result<string, PineValue> ToPublicResult() =>
            Value is { } value
            ?
            value
            :
            Error ??
            throw new InvalidOperationException(
                "Independent evaluation result contains neither a value nor an error");
    }
}
