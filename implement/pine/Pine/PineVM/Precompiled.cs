using ElmTime.ElmInteractive;
using Pine.Core;
using Pine.ElmInteractive;
using System;
using System.Collections.Concurrent;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Pine.PineVM;

public class Precompiled
{
    /*
     * 
     * Using the general environment class model in the first stage of dispatch/linking means that we can use that
     * information to optimize the dispatch at compile time.
     * */

    public abstract record PrecompiledResult
    {
        public sealed record FinalValue(
            PineValue Value,
            long StackFrameCount)
            : PrecompiledResult;

        public sealed record ContinueParseAndEval(
            PineValue EnvironmentValue,
            PineValue ExpressionValue)
            : PrecompiledResult;

        public sealed record StepwiseSpecialization(
            PineVM.ApplyStepwise Stepwise)
            : PrecompiledResult;
    }

    record PrecompiledEntry(
        EnvConstraintId EnvConstraint,
        Func<PineValue, PineVMParseCache, Func<PrecompiledResult>?> PrecompiledDelegate)
    {
        public PrecompiledEntry(
            EnvConstraintId EnvConstraint,
            Func<PineValue, PineVMParseCache, PrecompiledResult?> PrecompiledDelegate)
            : this(
                EnvConstraint,
                (env, parseCache) =>
                {
                    var result = PrecompiledDelegate(env, parseCache);

                    if (result is null)
                    {
                        return null;
                    }

                    return () => result;
                })
        {
        }

        public static PrecompiledEntry FinalValueForAnyEnvironment(
            EnvConstraintId EnvConstraint,
            Func<PineValue, PineVMParseCache, PineValue> delegateForAnyEnv) =>
            new(
                EnvConstraint,
                (env, parseCache) =>
                () => new PrecompiledResult.FinalValue(delegateForAnyEnv(env, parseCache), StackFrameCount: 0));
    }

    public static ImmutableHashSet<Expression> PrecompiledExpressions =>
        PrecompiledDict?.Keys.ToImmutableHashSet() ?? [];

    public static bool HasPrecompiledForExpression(Expression expression) =>
        PrecompiledDict?.ContainsKey(expression) ?? false;

    public static Func<PrecompiledResult>? SelectPrecompiled(
        Expression expression,
        PineValue environment,
        PineVMParseCache parseCache)
    {
        if (PrecompiledDict?.TryGetValue(expression, out var envItems) ?? false)
        {
            /*
            {
                // Ensure that the key is the same instance as the expression.
                // This should always be the case when we include the precompiled expression dictionary into the source of reused instances.
                var actualKey = PrecompiledDict.Keys.First(key => key == expression);

                if (!ReferenceEquals(actualKey, expression))
                {
                    throw new InvalidOperationException(
                        "PrecompiledDict key is not the same instance as the expression: " + actualKey);
                }
            }
            */

            foreach (var envItem in envItems)
            {
                if (envItem.EnvConstraint.SatisfiedByValue(environment))
                {
                    return envItem.PrecompiledDelegate.Invoke(environment, parseCache);
                }
            }
        }

        return null;
    }

    private static IEnumerable<KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>> BuildPrecompiled()
    {
        var adaptivePartialApplicationExpression =
            popularExpressionDictionary["adaptivePartialApplication"];

        var adaptivePartialApplicationExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(adaptivePartialApplicationExpression);

        var isPineListExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["isPineList"]);

        var isPineBlobExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["isPineBlob"]);

        var compareStringsExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["compareStrings"]);

        var compareExpression =
            popularExpressionDictionary["compare"];

        var compareExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(compareExpression);

        var compareListsExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["compareLists"]);

        var eqExpression =
            popularExpressionDictionary["eq"];

        var eqExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(eqExpression);

        var dictToListExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["dictToList"]);

        var dictKeysExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["dictKeys"]);

        var listsEqualRecursiveExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["listsEqualRecursive"]);


        var listMemberExpression = popularExpressionDictionary["List.member"];

        var listMemberExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(listMemberExpression);

        var basicsEqExposedValue = popularValueDictionary["Basics.eq.exposed"];


        var dictGetExpression = popularExpressionDictionary["dictGet"];

        var dictGetExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(dictGetExpression);

        var dictGetAfterCompareExpression = popularExpressionDictionary["dictGetAfterCompare"];

        var dictGetAfterCompareExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(dictGetAfterCompareExpression);


        var compareExposedValue = popularValueDictionary["Basics.compare.exposed"];


        var assocListGetExpression = popularExpressionDictionary["assocListGet"];

        var assocListGetExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(assocListGetExpression);

        var dictSizeHelpExpression = popularExpressionDictionary["dictSizeHelp"];

        var dictSizeHelpExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(dictSizeHelpExpression);


        var elmCompiledRecordAccessExpression = popularExpressionDictionary["elmCompiledRecordAccess"];

        var elmCompiledRecordAccessExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(elmCompiledRecordAccessExpression);

        var countPineListValueContentExpression =
            popularExpressionDictionary["countPineListValueContent"];

        var countPineListValueContentExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(countPineListValueContentExpression);

        var countPineValueContentExpression =
            popularExpressionDictionary["countPineValueContent"];

        var countPineValueContentExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(countPineValueContentExpression);


        var listMapExpression =
            popularExpressionDictionary["List.map"];

        var listMapExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(listMapExpression);

        var listConcatMapExpression =
            popularExpressionDictionary["List.concatMap"];

        var listConcatMapExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(listConcatMapExpression);

        var listFoldlExpression =
            popularExpressionDictionary["List.foldl"];

        var listFoldlExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(listFoldlExpression);

        var pineComputeValueFromStringRecursiveMainExpression =
            popularExpressionDictionary["Pine.computeValueFromStringRecursive.main"];

        var pineComputeValueFromStringRecursiveMainExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(pineComputeValueFromStringRecursiveMainExpression);

        var pineComputeValueFromStringRecursiveFromIntExpression =
            popularExpressionDictionary["Pine.computeValueFromStringRecursive.blobValueFromUnsignedInt"];

        var pineComputeValueFromStringRecursiveFromIntExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(pineComputeValueFromStringRecursiveFromIntExpression);

        var pineComputeValueFromStringRecursiveBlobWrapperExpression =
            popularExpressionDictionary["Pine.computeValueFromStringRecursive.BlobValue"];

        var pineComputeValueFromStringRecursiveBlobWrapperExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(pineComputeValueFromStringRecursiveBlobWrapperExpression);


        {
            var compareExpressionEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        compareStringsExpressionValue,
                        isPineListExpressionValue,
                        compareExpressionValue,
                        compareListsExpressionValue
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    compareExpression,
                    [PrecompiledEntry.FinalValueForAnyEnvironment(compareExpressionEnvClass, BasicsCompare)]);
        }

        {
            var eqExpressionEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        dictToListExpressionValue,
                        dictKeysExpressionValue,
                        isPineBlobExpressionValue,
                        eqExpressionValue,
                        listsEqualRecursiveExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    eqExpression,
                    [new PrecompiledEntry(eqExpressionEnvClass, BasicsEq)]);
        }


        {
            var listMemberExpressionEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        basicsEqExposedValue,
                        listMemberExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    listMemberExpression,
                    [new PrecompiledEntry(listMemberExpressionEnvClass, ListMember)]);
        }


        /*
         * 2024-08-31:
         * Disabling since observing difference in results.
         * */
        if (false)
        {
            var listMapEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        listMapExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    listMapExpression,
                    [new PrecompiledEntry(
                        listMapEnvClass,
                        ListMap)]);
        }

        {
            var concatMapEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        adaptivePartialApplicationExpressionValue,
                        listConcatMapExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    listConcatMapExpression,
                    [new PrecompiledEntry(
                        concatMapEnvClass,
                        ListConcatMap)]);
        }

        {
            var listFoldlEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        listFoldlExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    listFoldlExpression,
                    [new PrecompiledEntry(
                        listFoldlEnvClass,
                        ListFoldl)]);
        }

        {
            var assocListGetExpressionEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        basicsEqExposedValue,
                        assocListGetExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    assocListGetExpression,
                    [new PrecompiledEntry(assocListGetExpressionEnvClass, CommonAssocListGet)]);
        }


        {
            var dictGetExpressionEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        compareExposedValue,
                        dictGetExpressionValue,
                        dictGetAfterCompareExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    dictGetExpression,
                    [PrecompiledEntry.FinalValueForAnyEnvironment(dictGetExpressionEnvClass, DictGet)]);
        }


        {
            var dictSizeHelpEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        adaptivePartialApplicationExpressionValue,
                        dictSizeHelpExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    dictSizeHelpExpression,
                    [PrecompiledEntry.FinalValueForAnyEnvironment(dictSizeHelpEnvClass, DictSizeHelp)]);
        }


        {
            var elmCompiledRecordAccessEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    elmCompiledRecordAccessExpressionValue)
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    elmCompiledRecordAccessExpression,
                    [PrecompiledEntry.FinalValueForAnyEnvironment(elmCompiledRecordAccessEnvClass, ElmCompiledRecordAccess)]);
        }


        {
            var adaptivePartialApplicationEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    adaptivePartialApplicationExpressionValue),

                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [1,0],
                    PineValueAsString.ValueFromString("Function")),
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    adaptivePartialApplicationExpression,
                    [new PrecompiledEntry(adaptivePartialApplicationEnvClass, ElmCompiledAdaptivePartialApplication)]);
        }


        {
            var countPineValueContentEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        countPineValueContentExpressionValue,
                        countPineListValueContentExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    countPineValueContentExpression,
                    [new PrecompiledEntry(countPineValueContentEnvClass, CountPineValueContent)]);
        }

        {
            var computeValueFromStringRecursiveEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        pineComputeValueFromStringRecursiveFromIntExpressionValue,
                        pineComputeValueFromStringRecursiveBlobWrapperExpressionValue,
                        pineComputeValueFromStringRecursiveMainExpressionValue
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    pineComputeValueFromStringRecursiveMainExpression,
                    [new PrecompiledEntry(
                        computeValueFromStringRecursiveEnvClass,
                        PineComputeValueFromStringRecursive)]);
        }
    }

    static PineValue BasicsCompare(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var argA = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);
        var argB = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return BasicsCompare(argA, argB);
    }

    static PineValue BasicsCompare(PineValue a, PineValue b)
    {
        if (a == b)
        {
            return Tag_EQ_Value;
        }

        var aTag = PineVM.ValueFromPathInValueOrEmptyList(a, [0]);
        var bTag = PineVM.ValueFromPathInValueOrEmptyList(b, [0]);

        if (aTag == ElmValue.ElmStringTypeTagNameAsValue && bTag == ElmValue.ElmStringTypeTagNameAsValue)
        {
            return
                CompareStrings(
                    PineVM.ValueFromPathInValueOrEmptyList(a, [1, 0]),
                    PineVM.ValueFromPathInValueOrEmptyList(b, [1, 0]));
        }

        if (aTag == ElmValue.ElmFloatTypeTagNameAsValue && bTag == ElmValue.ElmFloatTypeTagNameAsValue)
        {
            var aTagArgs = PineVM.ValueFromPathInValueOrEmptyList(a, [1]);
            var bTagArgs = PineVM.ValueFromPathInValueOrEmptyList(b, [1]);

            var numA = PineVM.ValueFromPathInValueOrEmptyList(aTagArgs, [0]);
            var denomA = PineVM.ValueFromPathInValueOrEmptyList(aTagArgs, [1]);

            var numB = PineVM.ValueFromPathInValueOrEmptyList(bTagArgs, [0]);
            var denomB = PineVM.ValueFromPathInValueOrEmptyList(bTagArgs, [1]);

            var leftProduct = KernelFunction.int_mul(numA, denomB);
            var rightProduct = KernelFunction.int_mul(numB, denomA);

            if (leftProduct == rightProduct)
            {
                return Tag_EQ_Value;
            }

            if (KernelFunction.int_is_sorted_asc(PineValue.List([leftProduct, rightProduct])) == PineVMValues.TrueValue)
            {
                return Tag_LT_Value;
            }

            return Tag_GT_Value;
        }

        if (aTag == ElmValue.ElmFloatTypeTagNameAsValue)
        {
            var aTagArgs = PineVM.ValueFromPathInValueOrEmptyList(a, [1]);

            var numA = PineVM.ValueFromPathInValueOrEmptyList(aTagArgs, [0]);
            var denomA = PineVM.ValueFromPathInValueOrEmptyList(aTagArgs, [1]);

            var rightProduct = KernelFunction.int_mul(denomA, b);

            if (numA == rightProduct)
            {
                return Tag_EQ_Value;
            }

            if (KernelFunction.int_is_sorted_asc(PineValue.List([numA, rightProduct])) == PineVMValues.TrueValue)
            {
                return Tag_LT_Value;
            }

            return Tag_GT_Value;
        }

        if (bTag == ElmValue.ElmFloatTypeTagNameAsValue)
        {
            var bTagArgs = PineVM.ValueFromPathInValueOrEmptyList(b, [1]);

            var numB = PineVM.ValueFromPathInValueOrEmptyList(bTagArgs, [0]);
            var denomB = PineVM.ValueFromPathInValueOrEmptyList(bTagArgs, [1]);

            var leftProduct = KernelFunction.int_mul(a, denomB);

            if (leftProduct == numB)
            {
                return Tag_EQ_Value;
            }

            if (KernelFunction.int_is_sorted_asc(PineValue.List([leftProduct, numB])) == PineVMValues.TrueValue)
            {
                return Tag_LT_Value;
            }

            return Tag_GT_Value;
        }

        if (a is PineValue.ListValue)
        {
            return CompareLists(a, b);
        }

        if (KernelFunction.int_is_sorted_asc(PineValue.List([a, b])) == PineVMValues.TrueValue)
        {
            return Tag_LT_Value;
        }

        return Tag_GT_Value;
    }

    static PineValue CompareLists(PineValue a, PineValue b)
    {
        if (a == PineValue.EmptyList)
        {
            if (b == PineValue.EmptyList)
            {
                return Tag_EQ_Value;
            }

            return Tag_LT_Value;
        }

        if (a is PineValue.ListValue listA && 0 < listA.Elements.Count)
        {
            if (b == PineValue.EmptyList)
            {
                return Tag_GT_Value;
            }

            if (b is PineValue.ListValue listB && 0 < listB.Elements.Count)
            {
                var commonLength =
                    listA.Elements.Count < listB.Elements.Count ?
                    listA.Elements.Count :
                    listB.Elements.Count;

                for (var i = 0; i < commonLength; ++i)
                {
                    var itemA = listA.Elements[i];
                    var itemB = listB.Elements[i];

                    var itemOrder = BasicsCompare(itemA, itemB);

                    if (itemOrder != Tag_EQ_Value)
                    {
                        return itemOrder;
                    }
                }

                if (listA.Elements.Count < listB.Elements.Count)
                {
                    return Tag_LT_Value;
                }

                if (listA.Elements.Count > listB.Elements.Count)
                {
                    return Tag_GT_Value;
                }

                return Tag_EQ_Value;
            }

            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    static PineValue CompareStrings(PineValue environment)
    {
        var argA = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);
        var argB = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return CompareStrings(argA, argB);
    }

    static PineValue CompareStrings(PineValue stringA, PineValue stringB)
    {
        if (stringA is PineValue.ListValue listA && stringB is PineValue.ListValue listB)
        {
            var commonLength =
                listA.Elements.Count < listB.Elements.Count ?
                listA.Elements.Count :
                listB.Elements.Count;

            for (var i = 0; i < commonLength; ++i)
            {
                var itemA = listA.Elements[i];
                var itemB = listB.Elements[i];

                if (itemA is not PineValue.BlobValue blobA)
                {
                    return PineValue.EmptyList;
                }

                if (itemB is not PineValue.BlobValue blobB)
                {
                    return PineValue.EmptyList;
                }

                var charA = PineValueAsInteger.UnsignedIntegerFromBlobValue(blobA.Bytes.Span);
                var charB = PineValueAsInteger.UnsignedIntegerFromBlobValue(blobB.Bytes.Span);

                if (charA < charB)
                {
                    return Tag_LT_Value;
                }

                if (charA > charB)
                {
                    return Tag_GT_Value;
                }
            }

            if (listA.Elements.Count < listB.Elements.Count)
            {
                return Tag_LT_Value;
            }

            if (listA.Elements.Count > listB.Elements.Count)
            {
                return Tag_GT_Value;
            }

            return Tag_EQ_Value;
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    static PrecompiledResult.FinalValue BasicsEq(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var argA = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);
        var argB = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return BasicsEq(argA, argB);
    }

    static PrecompiledResult.FinalValue BasicsEq(PineValue a, PineValue b)
    {
        var (isEq, stackFrameCount) = BasicsEqRecursive(a, b);

        return new PrecompiledResult.FinalValue(
            isEq ? PineVMValues.TrueValue : PineVMValues.FalseValue,
            StackFrameCount: stackFrameCount);
    }

    static (bool, int) BasicsEqRecursive(PineValue a, PineValue b)
    {
        if (a == b)
        {
            return (true, 0);
        }

        var aTag = PineVM.ValueFromPathInValueOrEmptyList(a, [0]);
        var bTag = PineVM.ValueFromPathInValueOrEmptyList(b, [0]);

        var aTagArgs = PineVM.ValueFromPathInValueOrEmptyList(a, [1]);
        var bTagArgs = PineVM.ValueFromPathInValueOrEmptyList(b, [1]);

        if (aTag == ElmValue.ElmFloatTypeTagNameAsValue &&
            aTagArgs is PineValue.ListValue argsList && argsList.Elements.Count is 2)
        {
            var numAValue = argsList.Elements[0];
            var denAValue = argsList.Elements[1];

            return (numAValue == b && denAValue == IntegerOneValue, 0);
        }

        if (bTag == ElmValue.ElmFloatTypeTagNameAsValue &&
            bTagArgs is PineValue.ListValue argsListB && argsListB.Elements.Count is 2)
        {
            var numBValue = argsListB.Elements[0];
            var denBValue = argsListB.Elements[1];

            return (a == numBValue && IntegerOneValue == denBValue, 0);
        }

        if (a is PineValue.BlobValue)
        {
            return (false, 0);
        }

        {
            if (a is PineValue.ListValue listA && b is PineValue.ListValue listB)
            {
                if (listA.Elements.Count != listB.Elements.Count)
                {
                    return (false, 0);
                }

                if (aTag == ElmValue.ElmStringTypeTagNameAsValue)
                {
                    return (false, 0);
                }

                if (aTag == ElmValue.ElmDictNotEmptyTagNameAsValue)
                {
                    var dictAList = DictToListRecursive(a);
                    var dictBList = DictToListRecursive(b);

                    return
                        (PineValue.List(dictAList) == PineValue.List(dictBList), dictAList.Count + dictBList.Count);
                }

                if (aTag == ElmValue.ElmSetTypeTagNameAsValue)
                {
                    var dictA = PineVM.ValueFromPathInValueOrEmptyList(a, [1, 0]);
                    var dictB = PineVM.ValueFromPathInValueOrEmptyList(b, [1, 0]);

                    var dictAKeys = DictKeysRecursive(dictA);
                    var dictBKeys = DictKeysRecursive(dictB);

                    return
                        (PineValue.List(dictAKeys) == PineValue.List(dictBKeys), dictAKeys.Count + dictBKeys.Count);
                }

                return ListsEqualRecursive(listA.Elements, listB.Elements);
            }
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    static PineValue.ListValue DictToList(PineValue dict)
    {
        return PineValue.List(DictToListRecursive(dict));
    }

    static IReadOnlyList<PineValue> DictToListRecursive(PineValue dict)
    {
        var tag = PineVM.ValueFromPathInValueOrEmptyList(dict, [0]);

        if (tag == ElmValue.ElmDictEmptyTagNameAsValue)
        {
            return [];
        }

        if (tag == ElmValue.ElmDictNotEmptyTagNameAsValue)
        {
            var dictNotEmptyArgs = PineVM.ValueFromPathInValueOrEmptyList(dict, [1]);

            var argKey = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [1]);
            var argValue = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [2]);
            var argLeft = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [3]);
            var argRight = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [4]);

            return
                [
                ..DictToListRecursive(argLeft),
                PineValue.List([argKey, argValue]),
                ..DictToListRecursive(argRight)
                ];
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    static IReadOnlyList<PineValue> DictKeysRecursive(PineValue dict)
    {
        var tag = PineVM.ValueFromPathInValueOrEmptyList(dict, [0]);

        if (tag == ElmValue.ElmDictEmptyTagNameAsValue)
        {
            return [];
        }

        if (tag == ElmValue.ElmDictNotEmptyTagNameAsValue)
        {
            var dictNotEmptyArgs = PineVM.ValueFromPathInValueOrEmptyList(dict, [1]);

            var argKey = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [1]);

            var argLeft = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [3]);
            var argRight = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [4]);

            return
                [
                ..DictKeysRecursive(argLeft),
                argKey,
                ..DictKeysRecursive(argRight)
                ];
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    static (bool, int) ListsEqualRecursive(
        IReadOnlyList<PineValue> listA,
        IReadOnlyList<PineValue> listB)
    {
        int totalCount = 0;

        for (int i = 0; i < listA.Count; i++)
        {
            var (itemEq, itemCount) = BasicsEqRecursive(listA[i], listB[i]);

            if (!itemEq)
            {
                return (false, totalCount + itemCount);
            }

            totalCount += itemCount + 1;
        }

        return (true, totalCount);
    }

    static PrecompiledResult.FinalValue ListMember(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var item = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);
        var list = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return ListMember(item, list);
    }

    static PrecompiledResult.FinalValue ListMember(PineValue item, PineValue list)
    {
        if (list is PineValue.ListValue listValue)
        {
            int totalCount = 0;

            for (var i = 0; i < listValue.Elements.Count; ++i)
            {
                var (itemEq, itemEqStackFrameCount) = BasicsEqRecursive(item, listValue.Elements[i]);

                totalCount += itemEqStackFrameCount;

                if (itemEq)
                {
                    return new PrecompiledResult.FinalValue(
                        PineVMValues.TrueValue,
                        StackFrameCount: totalCount);
                }
            }

            return new PrecompiledResult.FinalValue(
                PineVMValues.FalseValue,
                StackFrameCount: totalCount);
        }

        return new PrecompiledResult.FinalValue(
            PineVMValues.FalseValue,
            StackFrameCount: 0);
    }

    static PrecompiledResult.FinalValue CommonAssocListGet(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var key = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);
        var list = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return CommonAssocListGetRecursive(key, list);
    }

    static PrecompiledResult.FinalValue CommonAssocListGetRecursive(
        PineValue key,
        PineValue list)
    {
        if (list is PineValue.ListValue listValue)
        {
            int totalCount = 0;

            for (var i = 0; i < listValue.Elements.Count; ++i)
            {
                if (listValue.Elements[i] is PineValue.ListValue itemList && 1 < itemList.Elements.Count)
                {
                    var (itemEq, itemEqStackFrameCount) = BasicsEqRecursive(key, itemList.Elements[0]);

                    totalCount += itemEqStackFrameCount;

                    if (itemEq)
                    {
                        return new PrecompiledResult.FinalValue(
                            Tag_Just_Value(itemList.Elements[1]),
                            StackFrameCount: totalCount);
                    }
                }
            }
        }

        return new PrecompiledResult.FinalValue(
            Tag_Nothing_Value,
            StackFrameCount: 0);
    }

    static PineValue DictGet(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var targetKey = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);
        var dict = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return DictGet(targetKey, dict);
    }

    static PineValue DictGet(PineValue targetKey, PineValue dict)
    {
        var dictTag = PineVM.ValueFromPathInValueOrEmptyList(dict, [0]);

        if (dictTag == ElmValue.ElmDictEmptyTagNameAsValue)
        {
            return Tag_Nothing_Value;
        }

        if (dictTag == ElmValue.ElmDictNotEmptyTagNameAsValue)
        {
            var dictNotEmptyArgs = PineVM.ValueFromPathInValueOrEmptyList(dict, [1]);

            var key = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [1]);
            var value = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [2]);
            var left = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [3]);
            var right = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [4]);

            var comparison = BasicsCompare(targetKey, key);

            if (comparison == Tag_LT_Value)
            {
                return DictGet(targetKey, left);
            }

            if (comparison == Tag_EQ_Value)
            {
                return Tag_Just_Value(value);
            }

            if (comparison == Tag_GT_Value)
            {
                return DictGet(targetKey, right);
            }
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    static PineValue DictSizeHelp(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        static long sizeHelp(
            long n,
            PineValue dict)
        {
            var dictTag = PineVM.ValueFromPathInValueOrEmptyList(dict, [0]);

            if (dictTag == ElmValue.ElmDictEmptyTagNameAsValue)
            {
                return n;
            }

            if (dictTag == ElmValue.ElmDictNotEmptyTagNameAsValue)
            {
                var dictNotEmptyArgs = PineVM.ValueFromPathInValueOrEmptyList(dict, [1]);

                var left = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [3]);
                var right = PineVM.ValueFromPathInValueOrEmptyList(dictNotEmptyArgs, [4]);

                return sizeHelp(sizeHelp(n + 1, right), left);
            }

            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        var countValue = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);
        var count =
            PineValueAsInteger.SignedIntegerFromValueStrict(countValue)
            .Extract(err => throw new Exception(err));

        var dict = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return PineValueAsInteger.ValueFromSignedInteger(sizeHelp((long)count, dict));
    }


    static PineValue ElmCompiledRecordAccess(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var fieldNameValue = PineVM.ValueFromPathInValueOrEmptyList(environment, [1]);
        var remainingFieldsValue = PineVM.ValueFromPathInValueOrEmptyList(environment, [2]);

        if (remainingFieldsValue is PineValue.ListValue remainingFields)
        {
            for (var i = 0; i < remainingFields.Elements.Count; ++i)
            {
                var field = remainingFields.Elements[i];

                if (field is PineValue.ListValue fieldList && 0 < fieldList.Elements.Count)
                {
                    if (fieldList.Elements[0] == fieldNameValue)
                    {
                        if (1 < fieldList.Elements.Count)
                        {
                            return fieldList.Elements[1];
                        }

                        return PineValue.EmptyList;
                    }
                }
            }
        }

        throw new ParseExpressionException("invalid record access - field name not found");
    }

    static PrecompiledResult.FinalValue CountPineValueContent(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var valueArgument =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var (nodeCount, byteCount) =
            CountEncodedPineValueContentRecursive(valueArgument);

        var returnValue =
            PineValue.List(
                [
                PineValueAsInteger.ValueFromSignedInteger(nodeCount),
                PineValueAsInteger.ValueFromSignedInteger(byteCount)
                ]);

        return
            new PrecompiledResult.FinalValue(
                returnValue,
                StackFrameCount: nodeCount);
    }

    static readonly ConcurrentDictionary<PineValue, (long nodeCount, long byteCount)> countEncodedPineValueContentDict = new();

    static (long nodeCount, long byteCount) CountEncodedPineValueContentRecursive(
        PineValue encodedValue)
    {
        return
            countEncodedPineValueContentDict.GetOrAdd(
            encodedValue,
            valueFactory: ComputeCountEncodedPineValueContentRecursive);
    }

    static (long nodeCount, long byteCount) ComputeCountEncodedPineValueContentRecursive(
        PineValue encodedValue)
    {
        if (encodedValue is not PineValue.ListValue listValue)
        {
            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        if (listValue.Elements.Count is not 2)
        {
            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        if (listValue.Elements[1] is not PineValue.ListValue tagArgumentsList)
        {
            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        if (tagArgumentsList.Elements.Count is not 1)
        {
            throw new ParseExpressionException("Error in case-of block: No matching branch.");
        }

        var tagArgument = tagArgumentsList.Elements[0];

        if (listValue.Elements[0] == Tag_ListValue_Value &&
            tagArgument is PineValue.ListValue listValueListList)
        {
            long nodeCount = 0;
            long byteCount = 0;

            for (var i = 0; i < listValueListList.Elements.Count; ++i)
            {
                var (itemNodeCount, itemByteCount) =
                    CountEncodedPineValueContentRecursive(listValueListList.Elements[i]);

                nodeCount += itemNodeCount + 1;
                byteCount += itemByteCount;
            }

            return (nodeCount, byteCount);
        }

        if (listValue.Elements[0] == Tag_BlobValue_Value &&
            tagArgument is PineValue.ListValue blobValueList)
        {
            return (0, blobValueList.Elements.Count);
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    static PrecompiledResult.FinalValue? PineComputeValueFromStringRecursive(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        if (PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]) is not PineValue.ListValue mappedCharsList)
        {
            return null;
        }

        var stringArgument =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        if (stringArgument is not PineValue.ListValue stringCharsList)
        {
            return null;
        }

        var mappedValues =
            new PineValue[mappedCharsList.Elements.Count + stringCharsList.Elements.Count];

        for (var i = 0; i < mappedCharsList.Elements.Count; ++i)
        {
            mappedValues[i] = mappedCharsList.Elements[i];
        }

        for (var i = 0; i < stringCharsList.Elements.Count; ++i)
        {
            if (stringCharsList.Elements[i] is not PineValue.BlobValue charBlobValue)
            {
                return null;
            }

            var blobValueIntegers = new PineValue[charBlobValue.Bytes.Length];

            for (var j = 0; j < charBlobValue.Bytes.Length; ++j)
            {
                blobValueIntegers[j] = PineValue.Blob([4, charBlobValue.Bytes.Span[j]]);
            }

            var blobValue =
                PineValue.List(
                    [
                    Tag_BlobValue_Value,
                    PineValue.List([PineValue.List(blobValueIntegers)])
                    ]);

            mappedValues[mappedCharsList.Elements.Count + i] = blobValue;
        }

        var finalValue =
            PineValue.List(
                [
                Tag_ListValue_Value,
                PineValue.List([PineValue.List(mappedValues)])
                ]);

        return
            new PrecompiledResult.FinalValue(
                finalValue,
                StackFrameCount: 3 + mappedValues.Length * 1);
    }

    static Func<PrecompiledResult>? ListMap(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var argumentMapFunction =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var argumentItems =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        if (argumentItems is not PineValue.ListValue itemsListValue)
        {
            return null;
        }

        if (itemsListValue.Elements.Count < 1)
        {
            return () => new PrecompiledResult.FinalValue(PineValue.EmptyList, StackFrameCount: 0);
        }

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(argumentMapFunction, parseCache)
            is not Result<string, ElmInteractiveEnvironment.FunctionRecord>.Ok functionRecordOk)
        {
            return null;
        }

        if (functionRecordOk.Value.functionParameterCount is not 1)
        {
            return null;
        }

        if (functionRecordOk.Value.argumentsAlreadyCollected.Count is not 0)
        {
            return null;
        }

        var environmentFunctionsEntry =
            PineValue.List(functionRecordOk.Value.envFunctions);

        PineValue environmentForItem(PineValue itemValue)
        {
            var argumentsList = PineValue.List([itemValue]);

            return
                PineValue.List([environmentFunctionsEntry, argumentsList]);
        }

        var itemsResults = new PineValue[itemsListValue.Elements.Count];
        var itemIndex = 0;

        PineVM.ApplyStepwise.StepResult step(PineValue itemResultValue)
        {
            itemsResults[itemIndex] = itemResultValue;

            ++itemIndex;

            if (itemIndex < itemsListValue.Elements.Count)
            {
                return
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(itemsListValue.Elements[itemIndex]),
                        Callback: step);
            }

            var resultListValue = PineValue.List(itemsResults);

            if (false)
            {
                var resultFromVM =
                    new PineVM(
                        disablePrecompiled: true,
                        disableReductionInCompilation: true)
                    .EvaluateExpression(
                        expression: popularExpressionDictionary["List.map"],
                        environment: environment)
                    .Extract(err => throw new Exception(err));

                if (resultFromVM != resultListValue)
                {
                    throw new Exception("List.map result mismatch");
                }
            }

            return new PineVM.ApplyStepwise.StepResult.Complete(resultListValue);
        }

        return
            () => new PrecompiledResult.StepwiseSpecialization(
                new PineVM.ApplyStepwise(
                    start:
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(itemsListValue.Elements[itemIndex]),
                        Callback: step)));
    }

    static Func<PrecompiledResult>? ListConcatMap(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var argumentMapFunction =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var argumentItems =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        if (argumentItems is not PineValue.ListValue itemsListValue)
        {
            return null;
        }

        if (itemsListValue.Elements.Count < 1)
        {
            return () => new PrecompiledResult.FinalValue(PineValue.EmptyList, StackFrameCount: 0);
        }

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(argumentMapFunction, parseCache)
            is not Result<string, ElmInteractiveEnvironment.FunctionRecord>.Ok functionRecordOk)
        {
            return null;
        }

        if (functionRecordOk.Value.functionParameterCount is not 1)
        {
            return null;
        }

        if (functionRecordOk.Value.argumentsAlreadyCollected.Count is not 0)
        {
            return null;
        }

        var environmentFunctionsEntry =
            PineValue.List(functionRecordOk.Value.envFunctions);

        PineValue environmentForItem(PineValue itemValue)
        {
            var argumentsList = PineValue.List([itemValue]);

            return
                PineValue.List([environmentFunctionsEntry, argumentsList]);
        }

        var itemsResults = new PineValue[itemsListValue.Elements.Count];
        var itemIndex = 0;

        PineVM.ApplyStepwise.StepResult step(PineValue itemResultValue)
        {
            itemsResults[itemIndex] = itemResultValue;

            ++itemIndex;

            if (itemIndex < itemsListValue.Elements.Count)
            {
                return
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(itemsListValue.Elements[itemIndex]),
                        Callback: step);
            }

            var concatResult = KernelFunction.concat(PineValue.List(itemsResults));

            return new PineVM.ApplyStepwise.StepResult.Complete(concatResult);
        }

        return
            () => new PrecompiledResult.StepwiseSpecialization(
                new PineVM.ApplyStepwise(
                    start:
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(itemsListValue.Elements[itemIndex]),
                        Callback: step)));
    }

    static Func<PrecompiledResult>? ListFoldl(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        var argumentMapFunction =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);

        var argumentAggregate =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        var argumentItems =
            PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 2]);

        if (argumentItems is not PineValue.ListValue itemsListValue)
        {
            return null;
        }

        if (itemsListValue.Elements.Count < 1)
        {
            return () => new PrecompiledResult.FinalValue(argumentAggregate, StackFrameCount: 0);
        }

        if (ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(argumentMapFunction, parseCache)
            is not Result<string, ElmInteractiveEnvironment.FunctionRecord>.Ok functionRecordOk)
        {
            return null;
        }

        if (functionRecordOk.Value.functionParameterCount is not 2)
        {
            return null;
        }

        if (functionRecordOk.Value.argumentsAlreadyCollected.Count is not 0)
        {
            return null;
        }

        var environmentFunctionsEntry =
            PineValue.List(functionRecordOk.Value.envFunctions);

        PineValue environmentForItem(PineValue aggregate, PineValue itemValue)
        {
            var argumentsList = PineValue.List([itemValue, aggregate]);

            return
                PineValue.List([environmentFunctionsEntry, argumentsList]);
        }

        var mutatedAggregate = argumentAggregate;
        var itemIndex = 0;

        PineVM.ApplyStepwise.StepResult step(PineValue itemResultValue)
        {
            mutatedAggregate = itemResultValue;

            ++itemIndex;

            if (itemIndex < itemsListValue.Elements.Count)
            {
                return
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(mutatedAggregate, itemsListValue.Elements[itemIndex]),
                        Callback: step);
            }

            if (false)
            {
                var resultFromVM =
                    new PineVM(
                        disablePrecompiled: true,
                        disableReductionInCompilation: true)
                    .EvaluateExpression(
                        expression: popularExpressionDictionary["List.foldl"],
                        environment: environment)
                    .Extract(err => throw new Exception(err));

                if (resultFromVM != mutatedAggregate)
                {
                    throw new Exception("foldl result mismatch");
                }
            }

            return new PineVM.ApplyStepwise.StepResult.Complete(mutatedAggregate);
        }

        return
            () => new PrecompiledResult.StepwiseSpecialization(
                new PineVM.ApplyStepwise(
                    start:
                    new PineVM.ApplyStepwise.StepResult.Continue(
                        Expression: functionRecordOk.Value.innerFunction,
                        EnvironmentValue: environmentForItem(mutatedAggregate, itemsListValue.Elements[itemIndex]),
                        Callback: step)));
    }

    static Func<PrecompiledResult>? ElmCompiledAdaptivePartialApplication(
        PineValue environment,
        PineVMParseCache parseCache)
    {
        if (environment is not PineValue.ListValue envList)
            return null;

        if (envList.Elements.Count < 2)
            return null;

        if (envList.Elements[1] is not PineValue.ListValue taggedFunctionRecordList)
            return null;

        if (envList.Elements[2] is not PineValue.ListValue newArgumentsList)
            return null;

        if (newArgumentsList.Elements.Count is 0)
        {
            return () => new PrecompiledResult.FinalValue(taggedFunctionRecordList, StackFrameCount: 0);
        }

        if (taggedFunctionRecordList.Elements.Count < 2)
            return null;

        /*
         * We already check this condition via the environment value class.
         * 
        if (taggedFunctionRecordList.Elements[0] != ElmCompilerFunctionTagValue)
            return null;
        */

        if (taggedFunctionRecordList.Elements[1] is not PineValue.ListValue functionRecord)
            return null;

        if (functionRecord.Elements.Count < 4)
            return null;

        if (functionRecord.Elements[3] is not PineValue.ListValue argsCollectedPreviouslyList)
            return null;

        if (functionRecord.Elements[1] is not PineValue.BlobValue paramCountBlob)
            return null;

        if (paramCountBlob.Bytes.Length is not 2)
            return null;

        if (paramCountBlob.Bytes.Span[0] is not 4)
            return null;

        var paramCount = paramCountBlob.Bytes.Span[1];

        var envFunctions = functionRecord.Elements[2];

        if (paramCount != argsCollectedPreviouslyList.Elements.Count + newArgumentsList.Elements.Count)
            return null;

        PineValue? combinedArgumentsValue = null;

        if (argsCollectedPreviouslyList.Elements.Count is 0)
        {
            combinedArgumentsValue = newArgumentsList;
        }
        else
        {
            var combinedArgsArray = new PineValue[argsCollectedPreviouslyList.Elements.Count + newArgumentsList.Elements.Count];

            for (var i = 0; i < argsCollectedPreviouslyList.Elements.Count; ++i)
            {
                combinedArgsArray[i] = argsCollectedPreviouslyList.Elements[i];
            }

            for (var i = 0; i < newArgumentsList.Elements.Count; ++i)
            {
                combinedArgsArray[i + argsCollectedPreviouslyList.Elements.Count] = newArgumentsList.Elements[i];
            }

            combinedArgumentsValue = PineValue.List(combinedArgsArray);
        }

        var newEnvironment =
            PineValue.List(
                [
                envFunctions,
                combinedArgumentsValue
                ]);

        return
            () => new PrecompiledResult.ContinueParseAndEval(
                EnvironmentValue: newEnvironment,
                ExpressionValue: functionRecord.Elements[0]);
    }

    private static readonly PineValue IntegerOneValue =
        PineValueAsInteger.ValueFromSignedInteger(1);

    private static readonly PineValue Tag_BlobValue_Value =
        PineValueAsString.ValueFromString("BlobValue");

    private static readonly PineValue Tag_ListValue_Value =
        PineValueAsString.ValueFromString("ListValue");

    private static readonly PineValue ElmCompilerFunctionTagValue =
        PineValueAsString.ValueFromString("Function");

    private static readonly PineValue Tag_EQ_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("EQ", []));

    private static readonly PineValue Tag_LT_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("LT", []));

    private static readonly PineValue Tag_GT_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("GT", []));

    private static readonly PineValue Tag_Nothing_Value =
        ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance("Nothing", []));

    private static PineValue.ListValue Tag_Just_Value(PineValue justValue) =>
        PineValue.List(
            [
            PineValueAsString.ValueFromString("Just"),
            PineValue.List([justValue])
            ]
        );

    private static readonly IImmutableDictionary<string, Expression> popularExpressionDictionary =
        PopularExpression.BuildPopularExpressionDictionary();

    private static readonly IImmutableDictionary<string, PineValue> popularValueDictionary =
        PopularExpression.BuildPopularValueDictionary();

    private static readonly FrozenDictionary<Expression, IReadOnlyList<PrecompiledEntry>> PrecompiledDict =
        BuildPrecompiled()
        .ToFrozenDictionary();

}
