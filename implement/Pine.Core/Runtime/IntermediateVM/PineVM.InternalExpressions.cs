using Pine.Core.Internal;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime.InternalExpression;
using System.Collections.Generic;

using Retained = Pine.Core.Runtime.InternalExpression.Expression;
using BuiltinId = Pine.Core.Runtime.InternalExpression.BuiltinFunction;
using BuiltinFunction = Pine.Core.PineLanguage.BuiltinFunction;

namespace Pine.Core.Runtime.IntermediateVM;

public partial class PineVM
{
    private static (PineValueInProcess value, EvalWorkCounters evalWorkCounters)? DirectEvalIfSimpleTemplate(
        Retained expression,
        PineValueInProcess environment)
    {
        var profile = PineVMWorkProfile.Current;
        profile?.DirectTemplates.Recognize(expression);

        if (expression.EvalCount > 0 || expression.BuiltinCount > 0)
            return null;

        return
            profile is null
            ?
            EvalDirect(expression, environment)
            :
            profile.DirectTemplates.Apply(expression, environment, profile);
    }

    internal static (PineValueInProcess value, EvalWorkCounters evalWorkCounters)? EvalDirect(
        Retained expression,
        PineValueInProcess environment)
    {
        if (PineVMWorkProfile.Current is { } profile)
        {
            return
                EvalDirectProfiled(
                    expression,
                    environment,
                    profile,
                    preserveConstructorSharing: true,
                    expression.EvalCount is 0 && expression.BuiltinCount is 0,
                    CreateDirectConstructorMemo,
                    EvalDirect);
        }

        var counters = new MutableEvalWorkCounters();
        var memo = CreateDirectConstructorMemo(expression);
        var value = EvalDirect(expression, environment, ref counters, isRoot: true, constructorMemo: memo);
        return value is null ? null : (value, counters.ToImmutable());
    }

    private static DirectContinuation? DirectContinuationIfSimpleEnough(
        Retained expression,
        PineValueInProcess envValue)
    {
        if (expression is not Retained.Eval evalExpr)
            return null;

        if (evalExpr.Encoded.EvalCount > 0 || evalExpr.Environment.EvalCount > 0)
            return null;

        var encodedExprValue = EvalDirect(evalExpr.Encoded, envValue);

        if (encodedExprValue is null)
            return null;

        var innerEnvValue = EvalDirect(evalExpr.Environment, envValue);

        if (innerEnvValue is null)
            return null;

        var aggregateEvalWorkCounters =
            EvalWorkCounters.Add(encodedExprValue.Value.evalWorkCounters, innerEnvValue.Value.evalWorkCounters);

        return
            new DirectContinuation(
                EncodedExprValue: encodedExprValue.Value.value,
                EnvironmentValue: innerEnvValue.Value.value,
                EvalWorkCounters: aggregateEvalWorkCounters);
    }

    private static Dictionary<Retained, DirectConstructorMemoEntry>? CreateDirectConstructorMemo(
        Retained expression)
    {
        if (expression is not Retained.List list ||
            expression.EvalCount is not 0 ||
            expression.BuiltinCount is not 0 ||
            expression.ConditionCount is not 0)
        {
            return null;
        }

        // Flat, small constructors need no tracking unless a child reference actually repeats.
        if (list.MaxDepth <= 1 && list.Items.Length <= 4)
        {
            for (var i = 0; i < list.Items.Length; ++i)
            {
                for (var j = 0; j < i; ++j)
                {
                    if (ReferenceEquals(list.Items[i], list.Items[j]))
                        return new(ReferenceEqualityComparer.Instance);
                }
            }

            return null;
        }

        return new(ReferenceEqualityComparer.Instance);
    }

    private static PineValueInProcess? EvalDirect(
        Retained expression,
        PineValueInProcess envValue,
        ref MutableEvalWorkCounters evalWorkCounters,
        HashSet<Retained>? references = null,
        bool isRoot = false,
        bool underRepeatFrontier = false,
        Dictionary<Retained, DirectConstructorMemoEntry>? constructorMemo = null)
    {
        var profile = PineVMWorkProfile.Current;
        // Prune only the counterfactual estimate's overlapping frontiers, never actual evaluation.
        var pruned =
            profile?.DirectEntry(expression, references, isRoot, underRepeatFrontier) ??
            false;

        if (constructorMemo is not null && constructorMemo.TryGetValue(expression, out var cached))
        {
            evalWorkCounters.InstructionCount += cached.Instructions;
            evalWorkCounters.BuildListCount += cached.Lists;
            evalWorkCounters.BuildListItemCount += cached.ListItems;
            profile?.DirectConstructorMemoHit(expression);
            return cached.Value;
        }

        evalWorkCounters.InstructionCount++;

        if (expression is Retained.Literal literal)
        {
            profile?.Count(PineVMWorkProfile.Metric.LiteralWrapperCalls);
            var wrappersBefore = pruned ? profile![PineVMWorkProfile.Metric.WrapperAllocations] : 0;
            var result = PineValueInProcess.Create(literal.Value);

            if (pruned)
            {
                var wrappers = profile![PineVMWorkProfile.Metric.WrapperAllocations] - wrappersBefore;

                profile.CountConstructorOpportunity(
                    PineVMWorkProfile.Metric.ConstructorFrontierAvoidableWrappers,
                    wrappers);

                profile.CountConstructorOpportunity(
                    PineVMWorkProfile.Metric.ConstructorFrontierAvoidableLiteralWrappers,
                    wrappers);
            }

            constructorMemo?.Add(expression, new(result, 1, 0, 0));
            return result;
        }

        if (expression is Retained.List listExpr)
        {
            var instructionsBefore = evalWorkCounters.InstructionCount - 1;
            var listsBefore = evalWorkCounters.BuildListCount;
            var listItemsBefore = evalWorkCounters.BuildListItemCount;
            var pureConstructor = constructorMemo is not null;
            evalWorkCounters.BuildListCount++;
            evalWorkCounters.BuildListItemCount += listExpr.Items.Length;
            var items = new PineValueInProcess[listExpr.Items.Length];
            profile?.Count(PineVMWorkProfile.Metric.EvalDirectListArrays);
            profile?.Count(PineVMWorkProfile.Metric.EvalDirectListItems, items.Length);
            profile?.ChildArray(items.Length);

            if (pruned)
            {
                profile!.CountConstructorOpportunity(PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListArrays);

                profile.CountConstructorOpportunity(
                    PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListItems,
                    items.Length);
            }

            for (var i = 0; i < items.Length; ++i)
            {
                var itemExpr = listExpr.Items[i];

                var itemValue =
                    EvalDirect(
                        itemExpr,
                        envValue,
                        ref evalWorkCounters,
                        references,
                        underRepeatFrontier: pruned,
                        constructorMemo: constructorMemo);

                if (itemValue is null)
                {
                    return null;
                }

                items[i] = itemValue;
                // Labels (including their ancestors) are deliberately outside the memoized grammar.
                pureConstructor = pureConstructor && constructorMemo!.ContainsKey(itemExpr);
            }

            var wrappersBefore = pruned ? profile![PineVMWorkProfile.Metric.WrapperAllocations] : 0;
            var result = PineValueInProcess.CreateList(items);

            if (pruned)
            {
                var wrappers = profile![PineVMWorkProfile.Metric.WrapperAllocations] - wrappersBefore;

                profile.CountConstructorOpportunity(
                    PineVMWorkProfile.Metric.ConstructorFrontierAvoidableWrappers,
                    wrappers);

                profile.CountConstructorOpportunity(
                    PineVMWorkProfile.Metric.ConstructorFrontierAvoidableListWrappers,
                    wrappers);
            }

            if (pureConstructor)
            {
                constructorMemo!.Add(
                    expression,
                    new(
                        result,
                        evalWorkCounters.InstructionCount - instructionsBefore,
                        evalWorkCounters.BuildListCount - listsBefore,
                        evalWorkCounters.BuildListItemCount - listItemsBefore));
            }

            return result;
        }

        if (expression is Retained.Conditional conditionalExpr)
        {
            var conditionValue =
                EvalDirect(
                    conditionalExpr.Condition,
                    envValue,
                    ref evalWorkCounters,
                    references,
                    underRepeatFrontier: pruned);

            if (conditionValue is null)
            {
                return null;
            }

            if (PineValueInProcess.AreEqual(conditionValue, PineValueInProcess.KernelTrueValue))
            {
                return
                    EvalDirect(
                        conditionalExpr.TrueBranch,
                        envValue,
                        ref evalWorkCounters,
                        references,
                        underRepeatFrontier: pruned);
            }
            else
            {
                return
                    EvalDirect(
                        conditionalExpr.FalseBranch,
                        envValue,
                        ref evalWorkCounters,
                        references,
                        underRepeatFrontier: pruned);
            }
        }

        if (expression is Retained.Switch dispatch)
        {
            var selector =
                EvalDirect(dispatch.Selector, envValue, ref evalWorkCounters, references, underRepeatFrontier: pruned);

            if (selector is null)
                return null;

            var branch = dispatch.SelectBranch(selector.Evaluate());
            return EvalDirect(branch, envValue, ref evalWorkCounters, references, underRepeatFrontier: pruned);
        }

        if (expression is Retained.Environment)
        {
            constructorMemo?.Add(expression, new(envValue, 1, 0, 0));
            return envValue;
        }

        if (expression is Retained.HeadSkip head)
        {
            var source = EvalDirect(head.Input, envValue, ref evalWorkCounters);

            return
                source is null
                ?
                null
                :
                head.Count <= 0 ? PineValueInProcess.Head(source) : source.GetElementAt(head.Count);
        }

        if (expression is Retained.Length_Equal_Binary length)
        {
            var first =
                EvalDirect(length.LengthOnLeft ? length.Sequence : length.Other, envValue, ref evalWorkCounters);

            if (first is null)
                return null;

            var second =
                EvalDirect(length.LengthOnLeft ? length.Other : length.Sequence, envValue, ref evalWorkCounters);

            if (second is null)
                return null;

            var left = length.LengthOnLeft ? PineValueInProcess.CreateInteger(first.GetLength()) : first;
            var right = length.LengthOnLeft ? second : PineValueInProcess.CreateInteger(second.GetLength());

            return
                PineValueInProcess.AreEqual(left, right)
                ?
                PineValueInProcess.KernelTrueValue
                :
                PineValueInProcess.KernelFalseValue;
        }

        if (expression is Retained.BuiltinBinary binary)
        {
            var left = EvalDirect(binary.Left, envValue, ref evalWorkCounters);

            if (left is null)
                return null;

            var right = EvalDirect(binary.Right, envValue, ref evalWorkCounters);

            if (right is null)
                return null;

            if (binary.Function is BuiltinId.Equal)
            {
                return
                    PineValueInProcess.AreEqual(left, right)
                    ?
                    PineValueInProcess.KernelTrueValue
                    :
                    PineValueInProcess.KernelFalseValue;
            }

            if (binary.Function is BuiltinId.Length)
                return PineValueInProcess.CreateInteger(2);

            if (binary.Function is BuiltinId.Head)
                return left;

            if (binary.Function is BuiltinId.Concat)
                return PineValueInProcess.ConcatBinary(left, right);

            return
                PineValueInProcess.Create(
                    Pine.Core.PineLanguage.BuiltinFunction.ApplyFunctionGeneric(
                        BuiltinFunctions.ToName(binary.Function),
                        PineValue.List([left.Evaluate(), right.Evaluate()])));
        }

        if (expression is Retained.Builtin builtinExpr)
        {
            if (builtinExpr.Function is BuiltinId.Length)
            {
                var inputValue = EvalDirect(builtinExpr.Input, envValue, ref evalWorkCounters);

                if (inputValue is null)
                    return null;

                return PineValueInProcess.CreateInteger(inputValue.GetLength());
            }

            if (builtinExpr.Function is BuiltinId.Equal)
            {
                if (builtinExpr.Input is Retained.List equalInputList)
                {
                    if (equalInputList.Items.Length < 2)
                        return PineValueInProcess.KernelTrueValue;

                    if (EvalDirect(equalInputList.Items[0], envValue, ref evalWorkCounters) is not { } firstItemValue)
                        return null;

                    for (var i = 1; i < equalInputList.Items.Length; ++i)
                    {
                        if (EvalDirect(equalInputList.Items[i], envValue, ref evalWorkCounters) is not { } nextItemValue)
                            return null;

                        if (!PineValueInProcess.AreEqual(firstItemValue, nextItemValue))
                            return PineValueInProcess.KernelFalseValue;
                    }

                    return PineValueInProcess.KernelTrueValue;
                }

                {
                    var inputValue = EvalDirect(builtinExpr.Input, envValue, ref evalWorkCounters);

                    if (inputValue is null)
                        return null;

                    if (inputValue.IsBlob())
                        return PineValueInProcess.Create(BuiltinFunction.equal(inputValue.Evaluate()));

                    if (inputValue.GetLength() < 2)
                        return PineValueInProcess.KernelTrueValue;

                    var firstItemValue = inputValue.GetElementAt(0);

                    for (var i = 1; i < inputValue.GetLength(); ++i)
                    {
                        var nextItemValue = inputValue.GetElementAt(i);

                        if (!PineValueInProcess.AreEqual(firstItemValue, nextItemValue))
                            return PineValueInProcess.KernelFalseValue;
                    }

                    return PineValueInProcess.KernelTrueValue;
                }
            }

            if (builtinExpr.Function is BuiltinId.Head)
            {
                if (builtinExpr.Input is Retained.Builtin innerBuiltinExpr &&
                    innerBuiltinExpr.Function is BuiltinId.Skip)
                {
                    if (innerBuiltinExpr.Input is Retained.List skipInputListExpr)
                    {
                        if (skipInputListExpr.Items.Length is not 2)
                            return null;

                        int? skipCount = null;

                        if (skipInputListExpr.Items[0] is Retained.Literal skipCountLiteral)
                        {
                            skipCount = (int?)BuiltinFunction.SignedIntegerFromValueRelaxed(skipCountLiteral.Value);
                        }

                        if (!skipCount.HasValue)
                        {
                            if (EvalDirect(skipInputListExpr.Items[0], envValue, ref evalWorkCounters) is { } skipInputValue)
                            {
                                skipCount = (int?)skipInputValue.AsInteger();
                            }
                        }

                        if (skipCount.HasValue)
                        {
                            if (EvalDirect(skipInputListExpr.Items[1], envValue, ref evalWorkCounters) is { } skipSubjectValue)
                            {
                                if (skipCount <= 0)
                                {
                                    return PineValueInProcess.Head(skipSubjectValue);
                                }

                                return skipSubjectValue.GetElementAt((int)skipCount);
                            }
                        }
                    }
                    else
                    {
                        if (EvalDirect(innerBuiltinExpr.Input, envValue, ref evalWorkCounters) is { } skipInputValue)
                        {
                            var skipInputLength = skipInputValue.GetLength();

                            if (skipInputLength is 2)
                            {
                                var skipCountValue = skipInputValue.GetElementAt(0);

                                if (skipCountValue.AsInteger() is { } skipCount)
                                {
                                    if (skipInputValue.GetElementAt(1) is { } skipSubjectValue)
                                    {
                                        if (skipCount <= 0)
                                        {
                                            return PineValueInProcess.Head(skipSubjectValue);
                                        }

                                        return skipSubjectValue.GetElementAt((int)skipCount);
                                    }
                                }
                            }
                        }
                    }
                }

                var inputValue = EvalDirect(builtinExpr.Input, envValue, ref evalWorkCounters);

                if (inputValue is null)
                    return null;

                return PineValueInProcess.Head(inputValue);
            }

            if (builtinExpr.Function is BuiltinId.Skip)
            {
                var inputValue = EvalDirect(builtinExpr.Input, envValue, ref evalWorkCounters);

                if (inputValue is null)
                    return null;

                if (!inputValue.IsList() || inputValue.GetLength() is not 2)
                    return PineValueInProcess.EmptyList;

                var skipCount = inputValue.GetElementAt(0).AsInteger();

                if (!skipCount.HasValue)
                    return PineValueInProcess.EmptyList;

                var source = inputValue.GetElementAt(1);

                if (skipCount.Value <= 0)
                    return source;

                if (int.MaxValue < skipCount.Value)
                    return null;

                return PineValueInProcess.Skip((int)skipCount.Value, source);
            }

            if (builtinExpr.Function is BuiltinId.Take)
            {
                var inputValue = EvalDirect(builtinExpr.Input, envValue, ref evalWorkCounters);

                if (inputValue is null)
                    return null;

                if (!inputValue.IsList() || inputValue.GetLength() is not 2)
                    return PineValueInProcess.EmptyList;

                var takeCount = inputValue.GetElementAt(0).AsInteger();

                if (!takeCount.HasValue)
                    return PineValueInProcess.EmptyList;

                var source = inputValue.GetElementAt(1);

                if (takeCount.Value <= 0)
                {
                    return
                        source.IsBlob()
                        ?
                        PineValueInProcess.EmptyBlob
                        :
                        PineValueInProcess.EmptyList;
                }

                if (int.MaxValue < takeCount.Value)
                    return source;

                return PineValueInProcess.Take((int)takeCount.Value, source);
            }

            if (builtinExpr.Function is BuiltinId.Concat)
            {
                if (builtinExpr.Input is Retained.List concatList && concatList.Items.Length is 2)
                {
                    if (EvalDirect(concatList.Items[0], envValue, ref evalWorkCounters) is { } leftValue &&
                        EvalDirect(concatList.Items[1], envValue, ref evalWorkCounters) is { } rightValue)
                    {
                        return PineValueInProcess.ConcatBinary(leftValue, rightValue);
                    }
                }

                var inputValue = EvalDirect(builtinExpr.Input, envValue, ref evalWorkCounters);

                if (inputValue is null)
                    return null;

                return PineValueInProcess.Concat(inputValue);
            }

            if (builtinExpr.Function is BuiltinId.IntIsSortedAsc)
            {
                var inputValue = EvalDirect(builtinExpr.Input, envValue, ref evalWorkCounters);

                if (inputValue is null)
                    return null;

                return PineValueInProcess.Create(BuiltinFunction.int_is_sorted_asc(inputValue.Evaluate()));
            }

            if (builtinExpr.Function is BuiltinId.IntAdd)
            {
                var inputValue = EvalDirect(builtinExpr.Input, envValue, ref evalWorkCounters);

                if (inputValue is null)
                    return null;

                return PineValueInProcess.Create(BuiltinFunction.int_add(inputValue.Evaluate()));
            }

            if (builtinExpr.Function is BuiltinId.IntMul)
            {
                var inputValue = EvalDirect(builtinExpr.Input, envValue, ref evalWorkCounters);

                if (inputValue is null)
                    return null;

                return PineValueInProcess.Create(BuiltinFunction.int_mul(inputValue.Evaluate()));
            }

            return null;
        }

        if (expression is Retained.Label stringTagExpr)
        {
            return
                EvalDirect(
                    stringTagExpr.Tagged,
                    envValue,
                    ref evalWorkCounters,
                    references,
                    underRepeatFrontier: pruned);
        }

        return null;
    }
}
