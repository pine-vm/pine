using Pine.ElmInteractive;
using Pine.PineVM;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Linq;

namespace Pine;

public record ReusedInstances(
    IEnumerable<Expression> expressionRootsSource)
{
    public static readonly ReusedInstances Instance =
        new(
            expressionRootsSource: ExpressionsSource());

    public FrozenDictionary<PineValue.ListValue.ListValueStruct, PineValue.ListValue>? ListValues { private set; get; }

    public FrozenSet<Expression>? Expressions { private set; get; }

    public FrozenDictionary<PineValue, Expression.Literal>? LiteralExpressions { private set; get; }

    internal FrozenDictionary<Expression.List.ListStruct, Expression.List>? ListExpressions { private set; get; }

    internal FrozenDictionary<Expression.Conditional.ConditionalStruct, Expression.Conditional>? ConditionalExpressions { private set; get; }

    public FrozenDictionary<Expression, PineValue.ListValue>? ExpressionEncodings { private set; get; }

    public FrozenDictionary<PineValue.ListValue, Expression>? ExpressionDecodings { private set; get; }

    public FrozenSet<ElmValue>? ElmValues { private set; get; }

    public FrozenDictionary<ElmValue, PineValue>? ElmValueDecodedAsInElmCompiler { private set; get; }

    public FrozenDictionary<PineValue, ElmValue>? ElmValueEncodedAsInElmCompiler { private set; get; }

    internal FrozenDictionary<ElmValue.ElmTag.ElmTagStruct, ElmValue.ElmTag>? ElmTagValues { private set; get; }

    public FrozenDictionary<PineValue, ElmValue>? ElmValueDecoding { private set; get; }

    public FrozenDictionary<ElmValue, PineValue>? ElmValueEncoding { private set; get; }


    static ReusedInstances()
    {
        Instance.Build();
    }

    static IEnumerable<Expression> ExpressionsSource()
    {
        foreach (var namedExpression in PopularExpression.BuildPopularExpressionDictionary())
        {
            yield return namedExpression.Value;
        }
    }

    public void Build()
    {
        var valueRootsFromProgramsSorted =
            expressionRootsSource
            .Select(ExpressionEncoding.EncodeExpressionAsValue)
            .Concat(PopularExpression.BuildPopularValueDictionary().Values)
            .OrderBy(pineValue => pineValue is PineValue.ListValue listValue ? listValue.NodesCount : 0)
            .ToList();

        var (valuesExpectedInCompilerLists, valuesExpectedInCompilerBlobs) =
            CollectAllComponentsFromRoots(valueRootsFromProgramsSorted);

        var valuesExpectedInCompilerSorted =
            PineValue.ReusedBlobs
            .Cast<PineValue>()
            .Concat(valuesExpectedInCompilerBlobs)
            .Concat(valuesExpectedInCompilerLists.OrderBy(listValue => listValue.NodesCount))
            .ToList();

        {
            var tempEncodingDict = new Dictionary<PineValue, ElmValue>();

            foreach (var valueInCompiler in valueRootsFromProgramsSorted)
            {
                var encodedInCompilerElm =
                    ElmValueInterop.PineValueEncodedAsInElmCompiler(
                        valueInCompiler,
                        tempEncodingDict);

                tempEncodingDict[valueInCompiler] = encodedInCompilerElm;
            }

            var tempElmValueEncodingDict = new Dictionary<ElmValue, PineValue>();

            foreach (var elmValue in tempEncodingDict.Values.OrderBy(ev => ev.ContainedNodesCount))
            {
                tempElmValueEncodingDict[elmValue] =
                    ElmInteractive.ElmValueEncoding.ElmValueAsPineValue(
                        elmValue,
                        tempElmValueEncodingDict);
            }

            var (allListsComponents, _) =
                CollectAllComponentsFromRoots(
                    valuesExpectedInCompilerSorted
                    .Concat(tempElmValueEncodingDict.Values)
                    .Concat(PopularValues.PopularElmValuesSource().Select(ElmInteractive.ElmValueEncoding.ElmValueAsPineValue)));

            var allListsComponentsSorted =
                allListsComponents
                .OrderBy(listValue => listValue.NodesCount)
                .ToList();

            var reusedListsDictInConstruction =
                new Dictionary<PineValue.ListValue, PineValue.ListValue>();

            foreach (var oldInstance in allListsComponentsSorted)
            {
                /*
                 * Since we are iterating in order of increasing size and all components,
                 * all the elements of the current list have already been rebuilt.
                 * Thus, we only need to rebuild shallowly the current list.
                 * */

                var rebuiltItems = new PineValue[oldInstance.Elements.Count];

                for (var i = 0; i < oldInstance.Elements.Count; i++)
                {
                    var listItem = oldInstance.Elements[i];

                    var rebuiltItem =
                        listItem is PineValue.ListValue oldItemAsList
                        ?
                        reusedListsDictInConstruction[oldItemAsList]
                        :
                        listItem;

                    rebuiltItems[i] = rebuiltItem;
                }

                var rebuilt =
                    PineValue.List(rebuiltItems);

                reusedListsDictInConstruction[rebuilt] = rebuilt;
            }

            ListValues =
                reusedListsDictInConstruction
                .ToFrozenDictionary(
                    keySelector: asRef => new PineValue.ListValue.ListValueStruct(asRef.Key.Elements),
                    elementSelector: asRef => asRef.Value);
        }

        {
            /*
             * For the expression instances, we rebuild the collection again, because the literal expressions should
             * reused the value instances we just built.
             * */

            var expressionsRoots =
                expressionRootsSource.ToList();


            var allExpressionDescendants = new HashSet<Expression>();

            /*
             * TODO: Optimize collecting all components and sorting by size.
             * */

            foreach (var expressionRoot in expressionsRoots)
            {
                foreach (var descendant in Expression.EnumerateSelfAndDescendants(expressionRoot))
                {
                    allExpressionDescendants.Add(descendant);
                }
            }

            var allDescendantsOrdered =
                allExpressionDescendants
                .OrderBy(expression => Expression.EnumerateSelfAndDescendants(expression).Count())
                .ToList();

            var reusedInstancesInConstruction = new Dictionary<Expression, Expression>();

            Expression reuseInstance(Expression expression) =>
                reusedInstancesInConstruction[expression];

            foreach (var descendant in allDescendantsOrdered)
            {
                var instanceToReuse =
                    descendant switch
                    {
                        Expression.List listExpression =>
                        Expression.ListInstance([.. listExpression.items.Select(reuseInstance)]),

                        Expression.Conditional conditionalExpression =>
                        Expression.ConditionalInstance(
                            condition: reuseInstance(conditionalExpression.condition),
                            falseBranch: reuseInstance(conditionalExpression.falseBranch),
                            trueBranch: reuseInstance(conditionalExpression.trueBranch)),

                        _ =>
                        descendant
                    };

                reusedInstancesInConstruction[instanceToReuse] = instanceToReuse;
            }

            Expressions =
                reusedInstancesInConstruction
                .Select(kvp => kvp.Value)
                .ToFrozenSet();

            LiteralExpressions =
                Expressions
                .OfType<Expression.Literal>()
                .ToFrozenDictionary(
                    keySelector: literal => literal.Value,
                    elementSelector: literal => literal);

            ListExpressions =
                Expressions
                .OfType<Expression.List>()
                .ToFrozenDictionary(
                    keySelector: list => new Expression.List.ListStruct(list.items),
                    elementSelector: list => list);

            ConditionalExpressions =
                Expressions
                .OfType<Expression.Conditional>()
                .ToFrozenDictionary(
                    keySelector: conditional => new Expression.Conditional.ConditionalStruct(
                        condition: conditional.condition,
                        falseBranch: conditional.falseBranch,
                        trueBranch: conditional.trueBranch),
                    elementSelector: conditional => conditional);
        }

        {
            var encodedDict = new Dictionary<Expression, PineValue.ListValue>();
            var decodedDict = new Dictionary<PineValue.ListValue, Expression>();

            foreach (var expression in Expressions)
            {
                var encoded = ExpressionEncoding.EncodeExpressionAsValue(expression);

                if (!encodedDict.ContainsKey(expression))
                {
                    encodedDict[expression] = encoded;
                }

                if (!decodedDict.ContainsKey(encoded))
                {
                    decodedDict[encoded] = expression;
                }
            }

            ExpressionEncodings =
                encodedDict.ToFrozenDictionary();

            ExpressionDecodings =
                decodedDict.ToFrozenDictionary();
        }

        {
            var coveredElmValues =
                CollectAllComponentsFromRoots(
                    ProduceElmValuesEncodedInCompiler(valuesExpectedInCompilerSorted));

            var coveredElmValuesOrdered =
                coveredElmValues
                .OrderBy(elmValue => elmValue.ContainedNodesCount)
                .ToList();

            var reusedInstances = new Dictionary<ElmValue, ElmValue>();

            foreach (var elmValue in coveredElmValuesOrdered)
            {
                var rebuiltInstance =
                    elmValue switch
                    {
                        ElmValue.ElmChar elmChar =>
                        elmValue,

                        ElmValue.ElmInteger elmInteger =>
                        ElmValue.Integer(elmInteger.Value),

                        ElmValue.ElmString elmString =>
                        ElmValue.String(elmString.Value),

                        ElmValue.ElmList elmList =>
                        new ElmValue.ElmList(
                            [.. elmList.Elements.Select(item => reusedInstances[item])]),

                        ElmValue.ElmRecord elmRecord =>
                        new ElmValue.ElmRecord(
                            [.. elmRecord.Fields.Select(kvp => (kvp.FieldName, reusedInstances[kvp.Value]))]),

                        ElmValue.ElmTag elmTag =>
                        ElmValue.TagInstance(
                            TagName: elmTag.TagName,
                            Arguments: [.. elmTag.Arguments.Select(arg => reusedInstances[arg])]),

                        ElmValue.ElmInternal elmInternal =>
                        elmValue,

                        _ =>
                        throw new System.NotImplementedException(
                            "Unexpected ElmValue type: " + elmValue.GetType())
                    };

                reusedInstances[rebuiltInstance] = rebuiltInstance;
            }

            ElmValues =
                reusedInstances.Values.ToFrozenSet();

            ElmTagValues =
                reusedInstances.Values
                .OfType<ElmValue.ElmTag>()
                .ToFrozenDictionary(
                    keySelector: tag => new ElmValue.ElmTag.ElmTagStruct(tag.TagName, tag.Arguments),
                    elementSelector: tag => tag);

            var encodedForCompilerDict = new Dictionary<PineValue, ElmValue>();

            foreach (var pineValueBeforeRef in valuesExpectedInCompilerSorted)
            {
                var pineValue =
                    ReusedInstance(pineValueBeforeRef) ??
                    throw new System.Exception("Expected reusable instance already set up for " + pineValueBeforeRef);

                var reusedElmValue =
                    ElmValueInterop.PineValueEncodedAsInElmCompiler(
                        pineValue,
                        encodedForCompilerDict);

                {
                    ElmValues.TryGetValue(reusedElmValue, out var refFromDict);

                    if (!ReferenceEquals(refFromDict, reusedElmValue))
                    {
                        throw new System.Exception(
                            "Unexpected difference in references: " + refFromDict + " /= " + reusedElmValue);
                    }
                }

                encodedForCompilerDict[pineValue] = reusedElmValue;
            }

            ElmValueEncodedAsInElmCompiler =
                encodedForCompilerDict.ToFrozenDictionary();

            ElmValueDecodedAsInElmCompiler =
                encodedForCompilerDict
                .ToFrozenDictionary(
                    keySelector: kvp => kvp.Value,
                    elementSelector: kvp => kvp.Key);
        }

        {
            var encodedDict = new Dictionary<ElmValue, PineValue>();

            foreach (var item in ElmValues)
            {
                encodedDict[item] = ElmInteractive.ElmValueEncoding.ElmValueAsPineValue(item);
            }

            ElmValueEncoding =
                encodedDict.ToFrozenDictionary();

            ElmValueDecoding =
                encodedDict
                .ToFrozenDictionary(
                    keySelector: kvp => kvp.Value,
                    elementSelector: kvp => kvp.Key);
        }
    }

    public void AssertReferenceEquality()
    {
        static void AssertReferenceEqual<T>(T? refA, T? refB)
            where T : class
        {
            if (ReferenceEquals(refA, refB))
                return;

            throw new System.Exception(
                "Unexpected difference in references: " +
                (refA is null ? "null" : refA) +
                " /= " +
                (refB is null ? "null" : refB));
        }

        foreach (var expressionEncoded in ExpressionEncodings)
        {
            Expressions.TryGetValue(
                expressionEncoded.Key,
                out var fromReusedExpressions);

            AssertReferenceEqual(
                fromReusedExpressions,
                expressionEncoded.Key);

            ListValues.TryGetValue(
                new PineValue.ListValue.ListValueStruct(expressionEncoded.Value.Elements),
                out var fromReusedListValues);

            AssertReferenceEqual(
                fromReusedListValues,
                expressionEncoded.Value);
        }

        foreach (var expressionDecoded in ExpressionDecodings)
        {
            Expressions.TryGetValue(
                expressionDecoded.Value,
                out var fromReusedExpr);

            AssertReferenceEqual(
                fromReusedExpr,
                expressionDecoded.Value);

            ListValues.TryGetValue(
                new PineValue.ListValue.ListValueStruct(expressionDecoded.Key.Elements),
                out var fromReusedListValues);

            AssertReferenceEqual(
                fromReusedListValues,
                expressionDecoded.Key);
        }

        foreach (var elmValueDecoded in ElmValueDecodedAsInElmCompiler)
        {
            ElmValues.TryGetValue(elmValueDecoded.Key, out var fromElmValues);

            AssertReferenceEqual(fromElmValues, elmValueDecoded.Key);

            if (elmValueDecoded.Value is PineValue.ListValue listValue)
            {
                ListValues.TryGetValue(
                    new PineValue.ListValue.ListValueStruct(listValue.Elements),
                    out var fromPineValues);

                AssertReferenceEqual(fromPineValues, elmValueDecoded.Value);
            }
        }

        foreach (var elmValueEncoded in ElmValueEncodedAsInElmCompiler)
        {
            ElmValues.TryGetValue(elmValueEncoded.Value, out var fromElmValues);

            AssertReferenceEqual(fromElmValues, elmValueEncoded.Value);

            if (elmValueEncoded.Key is PineValue.ListValue elmList)
            {
                ListValues.TryGetValue(
                    new PineValue.ListValue.ListValueStruct(elmList.Elements),
                    out var fromPineValues);

                AssertReferenceEqual(fromPineValues, elmValueEncoded.Key);
            }
        }
    }

    public PineValue? ReusedInstance(PineValue pineValue)
    {
        if (pineValue is PineValue.BlobValue blobValue)
        {
            if (PineValue.ReusedBlobs.TryGetValue(blobValue, out var reused))
                return reused;

            return null;
        }

        if (pineValue is PineValue.ListValue listValue)
        {
            if (ListValues.TryGetValue(new PineValue.ListValue.ListValueStruct(listValue.Elements), out var reused))
                return reused;

            return null;
        }

        throw new System.NotImplementedException(
            "Unexpected type: " + pineValue.GetType());
    }

    public ElmValue? ReusedInstance(ElmValue elmValue)
    {
        if (ElmValues?.TryGetValue(elmValue, out var reusedInstance) ?? false)
        {
            return reusedInstance;
        }

        return elmValue;
    }

    public static (IReadOnlySet<PineValue.ListValue>, IReadOnlySet<PineValue.BlobValue>) CollectAllComponentsFromRoots(
        IEnumerable<PineValue> roots)
    {
        var collectedBlobs = new HashSet<PineValue.BlobValue>(roots.OfType<PineValue.BlobValue>());
        var collectedLists = new HashSet<PineValue.ListValue>();

        var stack = new Stack<PineValue.ListValue>(roots.OfType<PineValue.ListValue>());

        while (stack.TryPop(out var listValue))
        {
            if (collectedLists.Contains(listValue))
                continue;

            collectedLists.Add(listValue);

            for (var i = 0; i < listValue.Elements.Count; i++)
            {
                if (listValue.Elements[i] is PineValue.ListValue childList)
                    stack.Push(childList);

                if (listValue.Elements[i] is PineValue.BlobValue childBlob)
                    collectedBlobs.Add(childBlob);
            }
        }

        return (collectedLists, collectedBlobs);
    }

    public static IReadOnlySet<ElmValue> CollectAllComponentsFromRoots(
        IEnumerable<ElmValue> roots)
    {
        var componentValues = new HashSet<ElmValue>();

        var stack = new Stack<ElmValue>(roots);

        while (stack.TryPop(out var elmValue))
        {
            if (componentValues.Contains(elmValue))
                continue;

            componentValues.Add(elmValue);

            IReadOnlyList<ElmValue> childItems =
                elmValue switch
                {
                    ElmValue.ElmInteger =>
                    [],

                    ElmValue.ElmChar =>
                    [],

                    ElmValue.ElmString =>
                    [],

                    ElmValue.ElmList elmList =>
                    elmList.Elements,

                    ElmValue.ElmRecord elmRecord =>
                    elmRecord.Fields.Select(kvp => kvp.Value).ToList(),

                    ElmValue.ElmTag elmTag =>
                    elmTag.Arguments,

                    ElmValue.ElmInternal =>
                    []
                };

            foreach (var item in childItems)
            {
                stack.Push(item);
            }
        }

        return componentValues;
    }

    static IEnumerable<ElmValue> ProduceElmValuesEncodedInCompiler(
        IEnumerable<PineValue> pineValues)
    {
        var encodedForCompilerDict = new Dictionary<PineValue, ElmValue>();

        /*
         * Encode the smaller values first, so that the larger values can reuse the instances of smaller values.
         * */
        var pineValuesSorted =
            pineValues
            .OrderBy(pineValue => pineValue is PineValue.ListValue listValue ? listValue.NodesCount : 0)
            .ToList();

        foreach (var pineValue in pineValuesSorted)
        {
            var pineValueEncoded =
                ElmValueInterop.PineValueEncodedAsInElmCompiler(
                    pineValue,
                    reusableEncodings: encodedForCompilerDict);

            encodedForCompilerDict[pineValue] = pineValueEncoded;
        }

        return encodedForCompilerDict.Values;
    }
}

