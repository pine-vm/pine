using Pine.ElmInteractive;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;

namespace Pine.PineVM;

public class Precompiled
{
    record PrecompiledEntry(
        EnvConstraintId EnvConstraint,
        Func<PineValue, PineValue> PrecompiledDelegate);

    public static ImmutableHashSet<Expression> PrecompiledExpressions =>
        PrecompiledDict?.Keys.ToImmutableHashSet() ?? [];

    public static bool HasPrecompiledForExpression(Expression expression) =>
        PrecompiledDict?.ContainsKey(expression) ?? false;

    public static Func<PineValue, PineValue>? SelectPrecompiled(
        Expression expression,
        PineValue environment)
    {
        if (PrecompiledDict?.TryGetValue(expression, out var envItems) ?? false)
        {
            foreach (var envItem in envItems)
            {
                if (envItem.EnvConstraint.SatisfiedByValue(environment))
                {
                    return envItem.PrecompiledDelegate;
                }
            }
        }

        return null;
    }

    private static IEnumerable<KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>> BuildPrecompiled()
    {
        var genericPartialApplicationExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["adaptivePartialApplication"])
            .Extract(err => throw new Exception(err));

        var isPineListExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["isPineList"])
            .Extract(err => throw new Exception(err));

        var isPineBlobExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["isPineBlob"])
            .Extract(err => throw new Exception(err));

        var compareStringsExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["compareStrings"])
            .Extract(err => throw new Exception(err));

        var compareExpression =
            popularExpressionDictionary["compare"];

        var compareExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(compareExpression)
            .Extract(err => throw new Exception(err));

        var compareListsExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["compareLists"])
            .Extract(err => throw new Exception(err));

        var eqExpression =
            popularExpressionDictionary["eq"];

        var eqExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(eqExpression)
            .Extract(err => throw new Exception(err));

        var dictToListExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["dictToList"])
            .Extract(err => throw new Exception(err));

        var dictKeysExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["dictKeys"])
            .Extract(err => throw new Exception(err));

        var listsEqualRecursiveExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(popularExpressionDictionary["listsEqualRecursive"])
            .Extract(err => throw new Exception(err));


        var listMemberExpression = popularExpressionDictionary["listMember"];

        var listMemberExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(listMemberExpression)
            .Extract(err => throw new Exception(err));

        var eqExposedValue = popularValueDictionary["eq.exposed"];


        var dictGetExpression = popularExpressionDictionary["dictGet"];

        var dictGetExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(dictGetExpression)
            .Extract(err => throw new Exception(err));

        var dictGetAfterCompareExpression = popularExpressionDictionary["dictGetAfterCompare"];

        var dictGetAfterCompareExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(dictGetAfterCompareExpression)
            .Extract(err => throw new Exception(err));


        var compareExposedValue = popularValueDictionary["compare.exposed"];


        var assocListGetExpression = popularExpressionDictionary["assocListGet"];

        var assocListGetExpressionValue =
            ExpressionEncoding.EncodeExpressionAsValue(assocListGetExpression)
            .Extract(err => throw new Exception(err));


        {
            var compareExpressionEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        genericPartialApplicationExpressionValue,
                        isPineListExpressionValue,
                        compareStringsExpressionValue,
                        compareExpressionValue,
                        compareListsExpressionValue
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    compareExpression,
                    [new PrecompiledEntry(compareExpressionEnvClass, Compare)]);
        }

        {
            var eqExpressionEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        genericPartialApplicationExpressionValue,
                        isPineBlobExpressionValue,
                        dictToListExpressionValue,
                        dictKeysExpressionValue,
                        eqExpressionValue,
                        listsEqualRecursiveExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    eqExpression,
                    [new PrecompiledEntry(eqExpressionEnvClass, Eq)]);
        }


        {
            var listMemberExpressionEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        genericPartialApplicationExpressionValue,
                        eqExposedValue,
                        listMemberExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    listMemberExpression,
                    [new PrecompiledEntry(listMemberExpressionEnvClass, ListMember)]);
        }


        {
            var assocListGetExpressionEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        genericPartialApplicationExpressionValue,
                        eqExposedValue,
                        assocListGetExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    assocListGetExpression,
                    [new PrecompiledEntry(assocListGetExpressionEnvClass, AssocListGet)]);
        }

        
        {
            var dictGetExpressionEnvClass =
                EnvConstraintId.Create(
                    [
                    new KeyValuePair<IReadOnlyList<int>, PineValue>(
                    [0],
                    PineValue.List(
                        [
                        genericPartialApplicationExpressionValue,
                        compareExposedValue,
                        dictGetExpressionValue,
                        dictGetAfterCompareExpressionValue,
                        ]))
                    ]);

            yield return
                new KeyValuePair<Expression, IReadOnlyList<PrecompiledEntry>>(
                    dictGetExpression,
                    [new PrecompiledEntry(dictGetExpressionEnvClass, DictGet)]);
        }
    }

    static PineValue Compare(PineValue environment)
    {
        var argA = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);
        var argB = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return Compare(argA, argB);
    }

    static PineValue Compare(PineValue a, PineValue b)
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

        if (a is PineValue.ListValue)
        {
            return CompareLists(a, b);
        }

        if (KernelFunction.is_sorted_ascending_int(PineValue.List([a, b])) == PineVMValues.TrueValue)
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

                    var itemOrder = Compare(itemA, itemB);

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

    static PineValue Eq(PineValue environment)
    {
        var argA = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);
        var argB = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return Eq(argA, argB);
    }

    static PineValue Eq(PineValue a, PineValue b)
    {
        if (a == b)
        {
            return PineVMValues.TrueValue;
        }

        if (a is PineValue.BlobValue)
        {
            return PineVMValues.FalseValue;
        }

        if (a is PineValue.ListValue listA && b is PineValue.ListValue listB)
        {
            if (listA.Elements.Count != listB.Elements.Count)
            {
                return PineVMValues.FalseValue;
            }

            var aTag = PineVM.ValueFromPathInValueOrEmptyList(a, [0]);

            if (aTag == ElmValue.ElmStringTypeTagNameAsValue)
            {
                return PineVMValues.FalseValue;
            }

            if (aTag == ElmValue.ElmDictNotEmptyTagNameAsValue)
            {
                return KernelFunction.equal(DictToList(a), DictToList(b));
            }

            if (aTag == ElmValue.ElmSetTypeTagNameAsValue)
            {
                var dictA = PineVM.ValueFromPathInValueOrEmptyList(a, [1, 0]);
                var dictB = PineVM.ValueFromPathInValueOrEmptyList(b, [1, 0]);

                return KernelFunction.equal(DictKeys(dictA), DictKeys(dictB));
            }

            return ListsEqualRecursive(listA.Elements, listB.Elements);
        }

        throw new ParseExpressionException("Error in case-of block: No matching branch.");
    }

    static PineValue DictToList(PineValue dict)
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

    static PineValue DictKeys(PineValue dict)
    {
        return PineValue.List(DictKeysRecursive(dict));
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

    static PineValue ListsEqualRecursive(IReadOnlyList<PineValue> listA, IReadOnlyList<PineValue> listB)
    {
        for (int i = 0; i < listA.Count; i++)
        {
            if (Eq(listA[i], listB[i]) != PineVMValues.TrueValue)
            {
                return PineVMValues.FalseValue;
            }
        }

        return PineVMValues.TrueValue;
    }

    static PineValue ListMember(PineValue environment)
    {
        var item = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);
        var list = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return ListMember(item, list);
    }

    static PineValue ListMember(PineValue item, PineValue list)
    {
        if (list is PineValue.ListValue listValue)
        {
            for (var i = 0; i < listValue.Elements.Count; ++i)
            {
                if (Eq(listValue.Elements[i], item) == PineVMValues.TrueValue)
                {
                    return PineVMValues.TrueValue;
                }
            }
        }

        return PineVMValues.FalseValue;
    }

    static PineValue AssocListGet(PineValue environment)
    {
        var key = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 0]);
        var list = PineVM.ValueFromPathInValueOrEmptyList(environment, [1, 1]);

        return AssocListGet(key, list);
    }

    static PineValue AssocListGet(PineValue key, PineValue list)
    {
        if (list is PineValue.ListValue listValue)
        {
            for (var i = 0; i < listValue.Elements.Count; ++i)
            {
                if (listValue.Elements[i] is PineValue.ListValue itemList && 1 < itemList.Elements.Count)
                {
                    if (Eq(key, itemList.Elements[0]) == PineVMValues.TrueValue)
                    {
                        return Tag_Just_Value(itemList.Elements[1]);
                    }
                }
            }
        }

        return Tag_Nothing_Value;
    }

    static PineValue DictGet(PineValue environment)
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

            var comparison = Compare(targetKey, key);

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

    static readonly PineValue Tag_EQ_Value =
        ElmValueEncoding.ElmValueAsPineValue(new ElmValue.ElmTag("EQ", []));

    static readonly PineValue Tag_LT_Value =
        ElmValueEncoding.ElmValueAsPineValue(new ElmValue.ElmTag("LT", []));

    static readonly PineValue Tag_GT_Value =
        ElmValueEncoding.ElmValueAsPineValue(new ElmValue.ElmTag("GT", []));

    static readonly PineValue Tag_Nothing_Value =
        ElmValueEncoding.ElmValueAsPineValue(new ElmValue.ElmTag("Nothing", []));

    static PineValue Tag_Just_Value(PineValue justValue) =>
        PineValue.List(
            [
            PineValueAsString.ValueFromString("Just"),
            PineValue.List([justValue])
            ]
        );

    static readonly IImmutableDictionary<string, Expression> popularExpressionDictionary =
        PopularExpression.BuildPopularExpressionDictionary();

    static readonly IImmutableDictionary<string, PineValue> popularValueDictionary =
        PopularExpression.BuildPopularValueDictionary();

    private static readonly FrozenDictionary<Expression, IReadOnlyList<PrecompiledEntry>> PrecompiledDict =
        BuildPrecompiled()
        .ToFrozenDictionary();

}
