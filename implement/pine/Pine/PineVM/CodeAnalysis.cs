using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using Pine.Core;
using Pine.Core.PopularEncodings;

namespace Pine.PineVM;

public record EnvConstraintId
{
    readonly static CompilePineToDotNet.CompilerMutableCache compilerCache = new();

    public IReadOnlyDictionary<IReadOnlyList<int>, PineValue> ParsedEnvItems { get; }

    readonly public string HashBase16;

    readonly private FastRepresentation fastRepresentation;

    /// <summary>
    /// Representation optimized for fast check at runtime.
    /// Independent of the identity of the constraint, this internal representation could
    /// use different ordering to check the most distinctive items first.
    /// </summary>
    private record FastRepresentation(
        ReadOnlyMemory<(ReadOnlyMemory<int>, PineValue)> Constraints)
    {
        public bool SatisfiedByValue(PineValue envValue)
        {
            for (int i = 0; i < Constraints.Length; ++i)
            {
                var (path, expectedValue) = Constraints.Span[i];

                if (CodeAnalysis.ValueFromPathInValue(envValue, path.Span) is not { } pathValue)
                    return false;

                if (!pathValue.Equals(expectedValue))
                    return false;
            }

            return true;
        }
    }

    private EnvConstraintId(
        IReadOnlyDictionary<IReadOnlyList<int>, PineValue> parsedEnvItems,
        string hashBase16)
    {
        ParsedEnvItems = parsedEnvItems;

        HashBase16 = hashBase16;

        var constraintsList = new (ReadOnlyMemory<int>, PineValue)[parsedEnvItems.Count];

        for (var i = 0; i < parsedEnvItems.Count; ++i)
        {
            var kv = parsedEnvItems.ElementAt(i);

            constraintsList[i] = (new ReadOnlyMemory<int>([.. kv.Key]), kv.Value);
        }

        var constraintsMemory = new ReadOnlyMemory<(ReadOnlyMemory<int>, PineValue)>(constraintsList);

        fastRepresentation = new FastRepresentation(constraintsMemory);
    }

    public PineValue? TryGetValue(IReadOnlyList<int> path)
    {
        if (ParsedEnvItems.TryGetValue(path, out var value))
            return value;

        if (path.Count is 0)
            return null;

        if (TryGetValue([.. path.SkipLast(1)]) is not { } parentValue)
            return null;

        return CodeAnalysis.ValueFromPathInValue(parentValue, [.. path.TakeLast(1)]);
    }

    public override int GetHashCode()
    {
        return HashBase16.GetHashCode();
    }

    public static bool Equal(EnvConstraintId? id0, EnvConstraintId? id1)
    {
        if (ReferenceEquals(id0, id1))
            return true;

        if (id0 is null || id1 is null)
            return false;

        return id0.HashBase16 == id1.HashBase16;
    }

    public static EnvConstraintId CreateEquals(PineValue pineValue) =>
        Create([new KeyValuePair<IReadOnlyList<int>, PineValue>([], pineValue)]);

    public static EnvConstraintId Create(
        ExpressionEnvClass.ConstrainedEnv envClass,
        PineValue envValue,
        bool skipUnavailableItems)
    {
        var parsedEnvItems =
            envClass.ParsedEnvItems
            .SelectMany(path =>
            {
                var itemValue = CodeAnalysis.ValueFromPathInValue(envValue, [.. path]);

                if (itemValue is null)
                {
                    if (skipUnavailableItems)
                        return Enumerable.Empty<KeyValuePair<IReadOnlyList<int>, PineValue>>();

                    throw new Exception("Item value null for path " + string.Join(", ", path));
                }

                return [new KeyValuePair<IReadOnlyList<int>, PineValue>(path, itemValue)];
            })
            .OrderBy(kv => kv.Key, IntPathComparer.Instance)
            .ToImmutableArray();

        return Create(parsedEnvItems);
    }

    public static EnvConstraintId Create(
        IReadOnlyList<KeyValuePair<IReadOnlyList<int>, PineValue>> parsedEnvItems)
    {
        PineValue[] parsedEnvItemsPineValues =
            [.. parsedEnvItems
            .OrderBy(kv => kv.Key, IntPathComparer.Instance)
            .Select(envItem =>
            (PineValue)
            PineValue.List(
                [PineValue.List([.. envItem.Key.Select(pathItem => IntegerEncoding.EncodeSignedInteger(pathItem))]),
                envItem.Value]))];

        var hashBase16 =
            Convert.ToHexStringLower(compilerCache.ComputeHash(PineValue.List(parsedEnvItemsPineValues)).Span);

        return new EnvConstraintId(
            parsedEnvItems.ToImmutableSortedDictionary(keyComparer: IntPathComparer.Instance),
            hashBase16: hashBase16);
    }

    public virtual bool Equals(EnvConstraintId? other) =>
        Equal(this, other);

    public bool SatisfiedByValue(PineValue envValue)
    {
        return fastRepresentation.SatisfiedByValue(envValue);
    }

    public bool SatisfiedByConstraint(EnvConstraintId otherEnvConstraintId)
    {
        foreach (var envItem in ParsedEnvItems)
        {
            if (envItem.Value is not { } expectedValue)
                return false;

            if (otherEnvConstraintId.TryGetValue(envItem.Key) is not { } pathValue)
                return false;

            if (!pathValue.Equals(expectedValue))
                return false;
        }

        return true;
    }

    public static EnvConstraintId CreateIntersection(
        PineValue valueA,
        PineValue valueB,
        int depthLimit) =>
        Create(Intersection(valueA, valueB, depthLimit));


    public static IReadOnlyList<KeyValuePair<IReadOnlyList<int>, PineValue>> Intersection(
        PineValue valueA,
        PineValue valueB,
        int depthLimit)
    {
        var intersectionTree = IntersectionTree(valueA, valueB, depthLimit);

        var mutatedCollection = new List<KeyValuePair<IReadOnlyList<int>, PineValue>>();

        void collectLeafesRecursively(ImmutableQueue<int> stack, IntersectionNode node)
        {
            if (node is IntersectionNode.IntersectionLeaf leaf)
            {
                mutatedCollection.Add(new KeyValuePair<IReadOnlyList<int>, PineValue>([.. stack], leaf.Value));
            }

            if (node is IntersectionNode.IntersectionBranch branch)
            {
                foreach (var (offset, childNode) in branch.Children)
                {
                    collectLeafesRecursively(stack.Enqueue(offset), childNode);
                }
            }
        }

        collectLeafesRecursively([], intersectionTree);

        return mutatedCollection;
    }

    private abstract record IntersectionNode
    {
        public sealed record IntersectionLeaf(PineValue Value)
            : IntersectionNode;

        public sealed record IntersectionBranch(IReadOnlyList<(int offset, IntersectionNode childNode)> Children)
            : IntersectionNode;
    }

    private static IntersectionNode IntersectionTree(
        PineValue valueA,
        PineValue valueB,
        int depthLimit)
    {
        if (valueA == valueB)
        {
            return new IntersectionNode.IntersectionLeaf(valueA);
        }

        if (depthLimit < 1)
        {
            return new IntersectionNode.IntersectionBranch([]);
        }

        if (valueA is not PineValue.ListValue listA || valueB is not PineValue.ListValue listB)
        {
            return new IntersectionNode.IntersectionBranch([]);
        }

        var commonLength =
            listA.Elements.Length < listB.Elements.Length ?
            listA.Elements.Length :
            listB.Elements.Length;

        var children = new List<(int, IntersectionNode)>();

        for (var i = 0; i < commonLength; ++i)
        {
            var childNode = IntersectionTree(listA.Elements.Span[i], listB.Elements.Span[i], depthLimit - 1);

            if (childNode is IntersectionNode.IntersectionBranch branch && branch.Children.Count is 0)
                continue;

            children.Add((i, childNode));
        }

        return new IntersectionNode.IntersectionBranch(children);
    }

    public static EnvConstraintId CreateIntersection(EnvConstraintId constraint, PineValue value)
    {
        if (constraint.SatisfiedByValue(value))
            return constraint;

        var intersectionEnvItems = new Dictionary<IReadOnlyList<int>, PineValue>();

        foreach (var envItem in constraint.ParsedEnvItems)
        {
            if (CodeAnalysis.ValueFromPathInValue(value, [.. envItem.Key]) is not { } pathValue)
                continue;

            if (pathValue.Equals(envItem.Value))
            {
                intersectionEnvItems[envItem.Key] = envItem.Value;
                continue;
            }

            if (envItem.Value is not PineValue.ListValue constraintItemList || pathValue is not PineValue.ListValue valueItemList)
            {
                continue;
            }

            for (var i = 0; i < constraintItemList.Elements.Length && i < valueItemList.Elements.Length; ++i)
            {
                var constraintChildEnvItem = constraintItemList.Elements.Span[i];
                var foundChildEnvItem = valueItemList.Elements.Span[i];

                var childConstraint = CreateEquals(constraintChildEnvItem);

                var childConstraintIntersection = CreateIntersection(childConstraint, foundChildEnvItem);

                foreach (var childEnvConstraintItem in childConstraintIntersection.ParsedEnvItems)
                {
                    intersectionEnvItems[[.. envItem.Key, i, .. childEnvConstraintItem.Key]] = childEnvConstraintItem.Value;
                }
            }
        }

        return Create([.. intersectionEnvItems]);
    }

    public EnvConstraintId PartUnderPath(IReadOnlyList<int> path)
    {
        return Create(
            [..
            ParsedEnvItems
            .SelectMany(item =>
            {
                if (!item.Key.Take(path.Count).SequenceEqual(path))
                {
                    return (IReadOnlyList<KeyValuePair<IReadOnlyList<int>, PineValue>>)[];
                }

                return [new KeyValuePair<IReadOnlyList<int>, PineValue>([.. item.Key.Skip(path.Count)], item.Value)];
            })]);
    }
}

public class EnvironmentClassSpecificityComparer : IComparer<EnvConstraintId>
{
    public readonly static EnvironmentClassSpecificityComparer Instance = new();

    public int Compare(EnvConstraintId? x, EnvConstraintId? y)
    {
        if (x is null && y is null)
            return 0;

        if (x is null)
            return -1;

        if (y is null)
            return 1;

        if (x.SatisfiedByConstraint(y))
        {
            if (!y.SatisfiedByConstraint(x))
            {
                return -1;
            }
        }

        if (y.SatisfiedByConstraint(x))
        {
            if (!x.SatisfiedByConstraint(y))
            {
                return 1;
            }
        }

        return x.ParsedEnvItems.Count - y.ParsedEnvItems.Count;
    }
}



public abstract record ExprMappedToParentEnv
{
    public record PathInParentEnv(IReadOnlyList<int> Path)
        : ExprMappedToParentEnv
    {
        public virtual bool Equals(PathInParentEnv? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return Path.SequenceEqual(other.Path);
        }

        public override int GetHashCode()
        {
            return base.GetHashCode();
        }
    }

    public record LiteralInParentEnv(PineValue Value)
        : ExprMappedToParentEnv;
}

public class IntPathEqualityComparer : IEqualityComparer<IReadOnlyList<int>?>
{
    public static readonly IntPathEqualityComparer Instance = new();

    public bool Equals(IReadOnlyList<int>? x, IReadOnlyList<int>? y) =>
        ReferenceEquals(x, y) ||
        (x is not null && y is not null && x.SequenceEqual(y));

    public int GetHashCode(IReadOnlyList<int> obj) =>
        obj.Aggregate(0, (acc, next) => acc ^ next.GetHashCode());
}

public class IntPathComparer : IComparer<IReadOnlyList<int>>
{
    public static readonly IntPathComparer Instance = new();

    public int Compare(IReadOnlyList<int>? x, IReadOnlyList<int>? y)
    {
        if (ReferenceEquals(x, y))
            return 0;

        if (x is null)
            return -1;

        if (y is null)
            return 1;

        if (x.Count != y.Count)
            return x.Count.CompareTo(y.Count);

        for (var i = 0; i < x.Count; i++)
        {
            var comparison = x[i].CompareTo(y[i]);

            if (comparison != 0)
                return comparison;
        }

        return 0;
    }
}


public class IntPathMemoryComparer : IComparer<ReadOnlyMemory<int>>
{
    public static readonly IntPathMemoryComparer Instance = new();

    public int Compare(ReadOnlyMemory<int> x, ReadOnlyMemory<int> y)
    {
        if (x.Equals(y))
            return 0;

        if (x.Length != y.Length)
            return x.Length.CompareTo(y.Length);

        for (var i = 0; i < x.Length; i++)
        {
            var comparison = x.Span[i].CompareTo(y.Span[i]);

            if (comparison != 0)
                return comparison;
        }

        return 0;
    }
}

public record RecursiveAnalysisResult(
    ExpressionEnvClass RootEnvClass,
    ImmutableHashSet<ExprOnRecursionPathEntry> ExprOnRecursionPath,
    ImmutableHashSet<(Expression expr, EnvConstraintId expandedConstraint)> UsagesCompleteForRecursion);

public record ExprOnRecursionPathEntry(
    EnvConstraintId RootConstraint,
    Expression RootExpr,
    EnvConstraintId Constraint,
    Expression Expr);

public abstract record ExpressionEnvClass
{
    public record UnconstrainedEnv
        : ExpressionEnvClass;

    public record ConstrainedEnv
        : ExpressionEnvClass
    {
        public ImmutableHashSet<IReadOnlyList<int>> ParsedEnvItems { get; }

        public ConstrainedEnv(
            IEnumerable<IReadOnlyList<int>> parsedEnvItems)
        {
            ParsedEnvItems =
                parsedEnvItems.ToImmutableHashSet(equalityComparer: IntPathEqualityComparer.Instance);
        }

        public override int GetHashCode() =>
            ParsedEnvItems.Aggregate(0, (acc, next) => acc ^ next.GetHashCode());

        public virtual bool Equals(ConstrainedEnv? other) =>
            other is not null &&
            other.ParsedEnvItems.SetEquals(ParsedEnvItems);
    }

    public static bool Equal(ExpressionEnvClass? env1, ExpressionEnvClass? env2)
    {
        if (ReferenceEquals(env1, env2))
            return true;

        if (env1 is null || env2 is null)
            return false;

        if (env1 is UnconstrainedEnv || env2 is UnconstrainedEnv)
            return true;

        if (env1 is ConstrainedEnv constrainedEnv1 && env2 is ConstrainedEnv constrainedEnv2)
            return constrainedEnv1.Equals(constrainedEnv2);

        throw new NotImplementedException();
    }

    public static ExprMappedToParentEnv? TryMapPathToParentEnvironment(
        Expression envExpr,
        IReadOnlyList<int> path)
    {
        if (path.Count == 0)
            return CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(envExpr);

        var currentIndex = path[0];

        if (envExpr is not Expression.List listExpr)
            return null;

        if (currentIndex < 0)
            return null;

        if (currentIndex >= listExpr.items.Count)
            return null;

        return TryMapPathToParentEnvironment(listExpr.items[currentIndex], [.. path.Skip(1)]);
    }
}

public class DelegatingEqualityComparer<T>(
    Func<T?, T?, bool> equals,
    Func<T, int> getHashCode)
    : IEqualityComparer<T>
{
    private readonly Func<T?, T?, bool> equals = equals;
    private readonly Func<T, int> getHashCode = getHashCode;

    public bool Equals(T? x, T? y) =>
        equals(x, y);

    public int GetHashCode(T obj) => getHashCode(obj);
}

public class CodeAnalysis
{
    public record ExprAnalysis(
        IImmutableDictionary<PineValue, RecursiveAnalysisResult> EnvDict);

    public record ExprUsageAnalysisStackEntry(
        Expression Expression,
        CompilePineToDotNet.CompiledExpressionId ExpressionId,
        EnvConstraintId EnvConstraintId,
        PineValue Environment);

    public record ParseSubExpression(
        Expression.ParseAndEval ParseAndEvalExpr,
        IReadOnlyList<int>? ExpressionPath,
        PineValue? ExpressionValue,
        Expression? ParsedExpr);

    public static RecursiveAnalysisResult AnalyzeExpressionUsageRecursive(
        IReadOnlyList<ExprUsageAnalysisStackEntry> stack,
        Expression expression,
        PineValue environment,
        ConcurrentDictionary<Expression, ExprAnalysis> mutatedCache,
        PineVMParseCache parseCache,
        PineVM evalVM)
    {
        var expressionId =
            CompilePineToDotNet.CompileToCSharp.CompiledExpressionId(expression)
            .Extract(err => throw new Exception(err));

        var parseSubexpressions =
            Expression.EnumerateSelfAndDescendants(expression)
            .OfType<Expression.ParseAndEval>()
            .Select(parseAndEvalExpr =>
            {
                var encodedMapped = TryParseExpressionAsIndexPathFromEnv(parseAndEvalExpr.Encoded);

                var expressionValue =
                encodedMapped switch
                {
                    ExprMappedToParentEnv.LiteralInParentEnv literalInParentEnv =>
                    literalInParentEnv.Value,

                    ExprMappedToParentEnv.PathInParentEnv pathInParentEnv =>
                    ValueFromPathInValue(environment, [.. pathInParentEnv.Path]),

                    null =>
                    /*
                     * The parsing is not smart enough to understand this case.
                     * */
                    null,

                    _ =>
                    throw new NotImplementedException(encodedMapped.ToString())
                };

                if (expressionValue is null)
                {
                    /*
                    if (Expression.IsIndependent(parseAndEvalExpr.expression))
                    {
                        expressionValue =
                        new PineVM().EvaluateExpressionDefault(parseAndEvalExpr.environment, environment)
                        .WithDefault(null);
                    }
                    */
                }

                var parsedExpr =
                    expressionValue is null
                    ?
                    null
                    :
                    parseCache.ParseExpression(expressionValue)
                    .WithDefault(null);

                return
                    new ParseSubExpression(
                        parseAndEvalExpr,
                        ExpressionPath: (encodedMapped as ExprMappedToParentEnv.PathInParentEnv)?.Path,
                        expressionValue,
                        ParsedExpr: parsedExpr);
            })
            .ToImmutableArray();

        var parseSubexpressionsToIntegrate =
            parseSubexpressions
            /*
             * Functions making use of the metaprogramming functionality in Pine can contain parse expressions which
             * do not in all cases contain a valid expression.
             * (These will be behind a branch not taken at runtime in that case.)
             * An example is the generic function implementing partial application for Elm functions (8cde1f66c7 in 2024-03-28).
             * Instead of defaulting to the unconstrained environment case, we filter out these cases here, to allow optimizing
             * the non-dynamic parts of the function.
             * */
            .Where(parseSubExpr => parseSubExpr.ParsedExpr is not null)
            .ToImmutableArray();

        var selfParsedEnvItems =
            parseSubexpressionsToIntegrate
            .Select(parseSubExpr => parseSubExpr.ExpressionPath)
            /*
             * ExpressionPath can be null if the expression is a literal.
             * */
            .WhereNotNull()
            .Distinct(IntPathEqualityComparer.Instance)
            .ToImmutableArray();

        var selfStackFrameEnv =
            new ExpressionEnvClass.ConstrainedEnv(selfParsedEnvItems);

        var selfEnvConstraintId =
            EnvConstraintId.Create(
                selfStackFrameEnv,
                environment,
                skipUnavailableItems: true);

        if (mutatedCache.TryGetValue(expression, out var cachedAnalysis))
        {
            if (cachedAnalysis.EnvDict.TryGetValue(environment, out var cachedEnvClass) && cachedEnvClass is not null)
            {
                return cachedEnvClass;
            }
        }

        RecursiveAnalysisResult insertInCache(RecursiveAnalysisResult recursiveAnalysisResult)
        {
            mutatedCache.AddOrUpdate(
                expression,
                addValueFactory: _ =>
                new ExprAnalysis(
                    EnvDict: ImmutableDictionary.CreateRange(
                        [new KeyValuePair<PineValue, RecursiveAnalysisResult>(environment, recursiveAnalysisResult)])),
                updateValueFactory: (expr, oldAnalysis) =>
                new ExprAnalysis(oldAnalysis.EnvDict.SetItem(environment, recursiveAnalysisResult)));

            return recursiveAnalysisResult;
        }

        var currentStackFrame =
            new ExprUsageAnalysisStackEntry(
                expression,
                ExpressionId: expressionId,
                selfEnvConstraintId,
                environment);

        if (stack.FirstOrDefault(
            prevStackItem =>
            prevStackItem.ExpressionId == currentStackFrame.ExpressionId &&
            prevStackItem.EnvConstraintId == currentStackFrame.EnvConstraintId) is { } recursionRoot)
        {
            var exprOnRecursionPath =
                stack
                .SkipWhile(stackItem => stackItem != recursionRoot)
                .Select(stackItem => new ExprOnRecursionPathEntry(
                    RootConstraint: recursionRoot.EnvConstraintId,
                    RootExpr: recursionRoot.Expression,
                    Constraint: stackItem.EnvConstraintId,
                    Expr: stackItem.Expression))
                .ToImmutableHashSet();

            return
                new RecursiveAnalysisResult(
                    new ExpressionEnvClass.ConstrainedEnv(selfStackFrameEnv.ParsedEnvItems),
                    ExprOnRecursionPath: exprOnRecursionPath,
                    UsagesCompleteForRecursion: []);
        }

        var nextStack = stack.Append(currentStackFrame).ToImmutableArray();

        RecursiveAnalysisResult computeChildClass(ParseSubExpression parseSubExpr)
        {
            if (parseSubExpr.ExpressionValue is not { } parsedExprValue)
            {
                throw new Exception("Unexpected null value");
            }

            if (parseSubExpr.ParsedExpr is not { } parsedChildExpr)
            {
                return
                    new RecursiveAnalysisResult(
                        new ExpressionEnvClass.UnconstrainedEnv(),
                        ExprOnRecursionPath: [],
                        UsagesCompleteForRecursion: []);
            }

            PineValue? childEnvValue = null;

            try
            {
                childEnvValue =
                evalVM
                /*
                 * Evaluation of the environment expression can fail here, since we are looking into all branches,
                 * including ones that are not reachable in the actual execution.
                 * In this case, classify the environment as unconstrained.
                 * */
                .EvaluateExpressionOnCustomStack(
                    parseSubExpr.ParseAndEvalExpr.Environment,
                    environment,
                    config: new PineVM.EvaluationConfig(ParseAndEvalCountLimit: 100))
                .Unpack(
                    fromErr: _ => null,
                    fromOk: ok => ok.ReturnValue);
            }
            catch
            {
                return
                    new RecursiveAnalysisResult(
                        new ExpressionEnvClass.UnconstrainedEnv(),
                        ExprOnRecursionPath: [],
                        UsagesCompleteForRecursion: []);
            }

            if (childEnvValue is null)
            {
                return
                    new RecursiveAnalysisResult(
                        new ExpressionEnvClass.UnconstrainedEnv(),
                        ExprOnRecursionPath: [],
                        UsagesCompleteForRecursion: []);
            }

            var childEnvBeforeMapping =
                AnalyzeExpressionUsageRecursive(
                    nextStack,
                    parsedChildExpr,
                    childEnvValue,
                    mutatedCache: mutatedCache,
                    parseCache: parseCache,
                    evalVM: evalVM);

            if (childEnvBeforeMapping.RootEnvClass is not ExpressionEnvClass.ConstrainedEnv childConstrainedEnv)
            {
                return childEnvBeforeMapping;
            }

            var childPathEnvMap = BuildPathMapFromChildToParentEnv(parseSubExpr.ParseAndEvalExpr.Environment);

            var parsedEnvItemsMapped =
                childConstrainedEnv.ParsedEnvItems
                .Select(path => childPathEnvMap(path))
                .ToImmutableArray();

            if (parsedEnvItemsMapped.Contains(null))
            {
                return
                    childEnvBeforeMapping
                    with
                    {
                        RootEnvClass = new ExpressionEnvClass.UnconstrainedEnv()
                    };
            }

            var parsedEnvItemsMappedPaths =
                parsedEnvItemsMapped
                .SelectMany(parsedEnvItemMapped =>
                parsedEnvItemMapped switch
                {
                    ExprMappedToParentEnv.LiteralInParentEnv _ =>
                    (IReadOnlyList<IReadOnlyList<int>>)[],

                    ExprMappedToParentEnv.PathInParentEnv pathInParentEnv =>
                    [pathInParentEnv.Path],

                    null =>
                    throw new NullReferenceException(),

                    _ =>
                    throw new NotImplementedException(parsedEnvItemMapped.ToString())
                })
                .ToImmutableArray();

            var childConstraintId =
                EnvConstraintId.Create(
                    childConstrainedEnv,
                    childEnvValue,
                    skipUnavailableItems: true);

            return
                childEnvBeforeMapping
                with
                {
                    RootEnvClass = new ExpressionEnvClass.ConstrainedEnv(parsedEnvItemsMappedPaths)
                };
        }

        var descendantsEnvUsages =
            parseSubexpressionsToIntegrate
            .Select(parseSubExpr => (parseSubExpr, childAnalysis: computeChildClass(parseSubExpr)))
            .ToImmutableArray();

        var descendantsConstrainedEnv =
            descendantsEnvUsages
            .Select(descendantEnvUsage => descendantEnvUsage.childAnalysis.RootEnvClass as ExpressionEnvClass.ConstrainedEnv)
            .WhereNotNull()
            .ToImmutableArray();

        var mergedParsedEnvItems =
            selfParsedEnvItems.Concat(
                descendantsConstrainedEnv
                .SelectMany(env => env.ParsedEnvItems))
            .ToImmutableHashSet();

        var mergedConstraintId =
            EnvConstraintId.Create(
                new ExpressionEnvClass.ConstrainedEnv(mergedParsedEnvItems),
                environment,
                skipUnavailableItems: true);

        var mergedExprOnRecursionPath =
            descendantsEnvUsages
            .SelectMany(entry => entry.childAnalysis.ExprOnRecursionPath)
            .ToImmutableHashSet();

        var usagesCompleteForRecursion =
            mergedExprOnRecursionPath
            .SelectMany(entry =>
            {
                if (entry.RootExpr == expression &&
                    entry.RootConstraint.SatisfiedByConstraint(mergedConstraintId) &&
                    entry.Constraint.SatisfiedByConstraint(mergedConstraintId))
                {
                    return (IEnumerable<(Expression, EnvConstraintId)>)[(entry.Expr, mergedConstraintId)];
                }

                return [];
            })
            .ToImmutableHashSet();

        var mergedEnvClass =
            new ExpressionEnvClass.ConstrainedEnv(mergedParsedEnvItems);

        return
            insertInCache(
                new RecursiveAnalysisResult(
                    mergedEnvClass,
                    ExprOnRecursionPath: mergedExprOnRecursionPath,
                    usagesCompleteForRecursion));
    }

    private class TooManyParsingStepsException(string message) : Exception(message)
    {
    }

    public static Func<IReadOnlyList<int>, ExprMappedToParentEnv?> BuildPathMapFromChildToParentEnv(
        Expression environment)
    {
        var envMappings = EnvItemsMappingsFromChildToParent(environment);

        ExprMappedToParentEnv? TryMapPathToParentEnvironment(IReadOnlyList<int> path)
        {
            var matchingEnvMappings =
            envMappings
            .Where(envMapping => envMapping.Key.SequenceEqual(path.Take(envMapping.Key.Count)))
            .ToImmutableArray();

            if (matchingEnvMappings.Length is 0)
                return null;

            var firstMatchingEnvMapping = matchingEnvMappings[0];

            if (firstMatchingEnvMapping.Value is ExprMappedToParentEnv.PathInParentEnv pathInParentEnv)
            {
                var pathRemainder = path.Skip(firstMatchingEnvMapping.Key.Count).ToImmutableArray();

                return
                    new ExprMappedToParentEnv.PathInParentEnv(
                        Path: [.. pathInParentEnv.Path, .. pathRemainder]);
            }

            if (firstMatchingEnvMapping.Value is ExprMappedToParentEnv.LiteralInParentEnv literalInParentEnv)
            {
                return literalInParentEnv;
            }

            throw new NotImplementedException(firstMatchingEnvMapping.Value.ToString());
        }

        return TryMapPathToParentEnvironment;
    }

    public static IReadOnlyList<KeyValuePair<IReadOnlyList<int>, ExprMappedToParentEnv>>
        EnvItemsMappingsFromChildToParent(Expression envExpression)
    {
        /*
         * 
        if (envExpression is Expression.EnvironmentExpression)
            return [new KeyValuePair<IReadOnlyList<int>, IReadOnlyList<int>>([], [])];
        */

        if (TryParseExpressionAsIndexPathFromEnv(envExpression) is { } path)
        {
            return [new KeyValuePair<IReadOnlyList<int>, ExprMappedToParentEnv>([], path)];
        }

        if (envExpression is Expression.List envListExpr)
        {
            return
                [
                ..envListExpr.items
                .SelectMany((childExpr, childIndex) =>
                EnvItemsMappingsFromChildToParent(childExpr)
                .Select(childMapping => new KeyValuePair<IReadOnlyList<int>, ExprMappedToParentEnv>(
                    [childIndex, .. childMapping.Key],
                    childMapping.Value)))
                ];
        }

        /*
         * TODO: Add a more general mapping from the child environment to the parent environment.
         * The special case we check here works for the typical form found in recursive calls.
         * 
         * 2024-03-07: One limitation of the current model becoming apparent now:
         * The Elm compiler uses kernel functions "take" and "concat" to forward items from the parent environment to the child environment.
         * Since this part of code analysis does not handle these cases, mapping fails for these kinds of invocations.
         * Finding an application of "take" here means we can only predict positions of following items (within the "concat")
         * if our environment model constrains the number of items in that list to at least the count given to "take".
         * The current model of individual items in 'EnvConstraintId.ParsedEnvItems' already enables derivation
         * of lower bounds, however, it turned out that the items we already collect (because they are parsed
         * at the current level) do not always cover the whole count given to "take".
         * One way to constrain the number of items we will get from the "take" invocation to an exact number
         * would be to expand the 'EnvConstraintId' adding lower bounds for subpaths of the environment.
         * 
         * 2024-03-08 The solution was to change the Elm compiler to not use "take" and "concat" for composing the child environment,
         * but instead refer to each forwarded item of the environment individually.
         * */

        return [];
    }

    public static PineValue? ValueFromPathInValue(
        PineValue environment,
        ReadOnlySpan<int> path)
    {
        if (path.Length is 0)
            return environment;

        if (environment is PineValue.BlobValue blobValue)
        {
            if (1 < path.Length)
                return null;

            return KernelFunction.skip(path[0], blobValue);
        }

        if (environment is not PineValue.ListValue listValue)
            return null;

        if (path[0] < 0)
            return null;

        if (path[0] >= listValue.Elements.Length)
            return null;

        return ValueFromPathInValue(listValue.Elements.Span[path[0]], path[1..]);
    }

    /// <summary>
    /// Returns the path of list items starting from the environment to the expression given as the argument.
    /// </summary>
    public static ExprMappedToParentEnv? TryParseExpressionAsIndexPathFromEnv(Expression expression)
    {
        return
            TryParseExpressionAsIndexPath(
                pathExpression: expression,
                rootExpression: Expression.EnvironmentInstance);
    }

    public static ExprMappedToParentEnv? TryParseExpressionAsIndexPath(
        Expression pathExpression,
        Expression rootExpression)
    {
        if (pathExpression == rootExpression)
            return new ExprMappedToParentEnv.PathInParentEnv([]);

        if (pathExpression is Expression.Literal literal)
            return new ExprMappedToParentEnv.LiteralInParentEnv(literal.Value);

        if (pathExpression is Expression.StringTag stringTagExpr)
            return TryParseExpressionAsIndexPath(stringTagExpr.Tagged, rootExpression);

        if (pathExpression is not Expression.KernelApplication kernelApplication)
            return null;

        if (kernelApplication.Function is not nameof(KernelFunction.head))
            return null;

        if (kernelApplication.Input is Expression.KernelApplication inputKernelApplication &&
            inputKernelApplication.Function is nameof(KernelFunction.skip))
        {
            if (inputKernelApplication.Input is not Expression.List skipInputList)
                return null;

            if (skipInputList.items.Count is not 2)
                return null;

            if (skipInputList.items[0] is not Expression.Literal skipCountLiteral)
                return null;

            if (TryParseExpressionAsIndexPath(skipInputList.items[1], rootExpression) is not ExprMappedToParentEnv.PathInParentEnv pathPrefix)
                return null;

            return
                KernelFunction.SignedIntegerFromValueRelaxed(skipCountLiteral.Value) is { } skipValue
                ?
                new ExprMappedToParentEnv.PathInParentEnv([.. pathPrefix.Path, (int)skipValue])
                :
                null;
        }

        {
            if (TryParseExpressionAsIndexPath(kernelApplication.Input, rootExpression) is not ExprMappedToParentEnv.PathInParentEnv pathPrefix)
                return null;

            return new ExprMappedToParentEnv.PathInParentEnv([.. pathPrefix.Path, 0]);
        }
    }

    public static IReadOnlyDictionary<Expression, IReadOnlyList<EnvConstraintId>> EnvironmentClassesFromInvocationReports(
        IReadOnlyList<PineVM.EvaluationReport> invocationReports,
        int limitInvocationSampleCount,
        int limitSampleCountPerSample,
        int classUsageCountMin,
        int limitClassesPerExpression) =>
        EnvironmentClassesFromInvocationReports(
            invocationReports: invocationReports,
            expressionsToIgnore: Precompiled.PrecompiledExpressions,
            limitInvocationSampleCount: limitInvocationSampleCount,
            limitSampleCountPerSample: limitSampleCountPerSample,
            classUsageCountMin: classUsageCountMin,
            limitClassesPerExpression: limitClassesPerExpression);

    public static IReadOnlyDictionary<Expression, IReadOnlyList<EnvConstraintId>> EnvironmentClassesFromInvocationReports(
        IReadOnlyList<PineVM.EvaluationReport> invocationReports,
        IReadOnlySet<Expression> expressionsToIgnore,
        int limitInvocationSampleCount,
        int limitSampleCountPerSample,
        int classUsageCountMin,
        int limitClassesPerExpression)
    {
        var invocationReportsFiltered =
            invocationReports
            .Where(r => !expressionsToIgnore.Contains(r.Expression))
            .ToImmutableArray();

        var invocationReportsByExpr =
            SubsequenceWithEvenDistribution(
                invocationReportsFiltered,
                limitInvocationSampleCount)
            .GroupBy(report => report.Expression)
            .ToImmutableArray();

        var parseCache = new PineVMParseCache();

        var expressionsEnvClasses =
            invocationReportsByExpr
            .Where(exprGroup => classUsageCountMin <= exprGroup.Count())
            .ToDictionary(
                keySelector: exprGroup => exprGroup.Key,
                elementSelector:
                exprGroup =>
                EnvironmentClassesFromExpressionInvocationReports(
                    expression: exprGroup.Key,
                    invocationsEnvironments: [.. exprGroup.Select(report => report.Environment)],
                    limitSampleCountPerSample: limitSampleCountPerSample,
                    classUsageCountMin: classUsageCountMin,
                    limitClassesCount: limitClassesPerExpression,
                    parseCache: parseCache));

        return expressionsEnvClasses;
    }


    public static IReadOnlyList<EnvConstraintId> EnvironmentClassesFromExpressionInvocationReports(
        Expression expression,
        IReadOnlyList<PineValue> invocationsEnvironments,
        int limitSampleCountPerSample,
        int classUsageCountMin,
        int limitClassesCount,
        PineVMParseCache parseCache)
    {
        var environmentClasses =
            GenerateEnvironmentClasses(
                invocationsEnvironments,
                limitIntersectionCountPerValue: limitSampleCountPerSample,
                classDepthLimit: 6);

        var environmentClassesAboveThreshold =
            environmentClasses
            .Where(envClass => classUsageCountMin / 4 <= envClass.matchCount)
            .OrderByDescending(envClass => envClass.matchCount)
            .ToImmutableArray();

        var environmentClassesToSimplify =
            environmentClassesAboveThreshold
            .Take(limitClassesCount * 10)
            .ToImmutableArray();

        var classSimplifications = new Dictionary<EnvConstraintId, EnvConstraintId>();

        var simplifiedClasses = new HashSet<EnvConstraintId>();

        foreach (var (envClass, _) in environmentClassesToSimplify)
        {
            /*

            var (envClassSimplified, simplicationAdditions) =
                SimplifyEnvClassRecursive(
                    expression: expression,
                    envClass,
                    simplifications: classSimplifications,
                    parseCache: parseCache);

            foreach (var mapAddition in simplicationAdditions)
            {
                classSimplifications[mapAddition.Key] = mapAddition.Value;
            }
            */

            var envClassSimplified =
                SimplifyEnvClass(
                    expression,
                    envClass,
                    depthMax: 6,
                    parseCache: parseCache);

            if (0 < envClassSimplified.ParsedEnvItems.Count)
            {
                simplifiedClasses.Add(envClassSimplified);
            }
        }

        var simplifiedClassesRanked =
            simplifiedClasses
            .Select(envClass => (envClass, matchCount: invocationsEnvironments.Count(envClass.SatisfiedByValue)))
            .Where(envClassAndMatchCount => classUsageCountMin <= envClassAndMatchCount.matchCount)
            .OrderByDescending(envClassAndMatchCount => envClassAndMatchCount.matchCount)
            .Select(envClassAndMatchCount => envClassAndMatchCount.envClass)
            .ToImmutableArray();

        return [.. simplifiedClassesRanked.Take(limitClassesCount)];
    }

    static EnvConstraintId SimplifyEnvClassShallow(
        Expression expression,
        EnvConstraintId envClass)
    {
        var shallowObservedPaths =
            Expression.EnumerateSelfAndDescendants(expression)
            .SelectWhereNotNull(
                expr =>
                TryParseExpressionAsIndexPathFromEnv(expr) is ExprMappedToParentEnv.PathInParentEnv path
                ?
                path.Path
                :
                null)
            .ToHashSet(IntPathEqualityComparer.Instance);

        bool keepClassItemPath(IReadOnlyList<int> classItemPath)
        {
            foreach (var observedPath in shallowObservedPaths)
            {
                if (observedPath.Count < classItemPath.Count)
                    continue;

                bool mismatch = false;

                for (var i = 0; i < classItemPath.Count; i++)
                {
                    if (observedPath[i] != classItemPath[i])
                    {
                        mismatch = true;
                        break;
                    }
                }

                if (mismatch)
                    continue;

                return true;
            }

            return false;
        }

        var itemsToKeep =
            envClass.ParsedEnvItems
            .Where(classItem => keepClassItemPath(classItem.Key))
            .ToList();

        if (itemsToKeep.Count == envClass.ParsedEnvItems.Count)
            return envClass;

        return EnvConstraintId.Create(itemsToKeep);
    }


    static public long SimplifyEnvClassCount { private set; get; } = 0;


    static EnvConstraintId SimplifyEnvClass(
        Expression rootExpression,
        EnvConstraintId envClass,
        int depthMax,
        PineVMParseCache parseCache)
    {
        ++SimplifyEnvClassCount;


        static ImmutableHashSet<IReadOnlyList<int>> observedPathsFromExpression(Expression expression) =>
            Expression.EnumerateSelfAndDescendants(expression)
            .SelectWhereNotNull(
                expr =>
                TryParseExpressionAsIndexPathFromEnv(expr) is ExprMappedToParentEnv.PathInParentEnv path
                ?
                path.Path
                :
                null)
            .ToImmutableHashSet(IntPathEqualityComparer.Instance);

        var rootObservedPaths = observedPathsFromExpression(rootExpression);

        var levelsObservedPaths = new List<ImmutableHashSet<IReadOnlyList<int>>>();

        var currentExpression = rootExpression;

        for (var i = 0; i < depthMax; ++i)
        {
            var lastExpression = currentExpression;

            currentExpression =
                PineVM.ReduceExpressionAndInlineRecursive(
                    currentExpression: currentExpression,
                    rootExprForms: [currentExpression],
                    inlinedParents: [],
                    envConstraintId: envClass,
                    maxDepth: 2,
                    maxSubexpressionCount: 4_000,
                    parseCache: parseCache,
                    disableRecurseAfterInline: true,
                    skipInlining: (_, _) => false);

            if (currentExpression == lastExpression)
                break;

            levelsObservedPaths.Add(observedPathsFromExpression(currentExpression));
        }

        var aggregateObservedPaths =
            levelsObservedPaths.Aggregate(
                func: (a, b) => a.Union(b),
                seed: rootObservedPaths);

        bool keepClassItemPath(IReadOnlyList<int> classItemPath)
        {
            foreach (var observedPath in aggregateObservedPaths)
            {
                if (observedPath.Count < classItemPath.Count)
                    continue;

                bool mismatch = false;

                for (var i = 0; i < classItemPath.Count; i++)
                {
                    if (observedPath[i] != classItemPath[i])
                    {
                        mismatch = true;
                        break;
                    }
                }

                if (mismatch)
                    continue;

                return true;
            }

            return false;
        }

        var itemsToKeep =
            envClass.ParsedEnvItems
            .Where(classItem => keepClassItemPath(classItem.Key))
            .ToList();

        if (itemsToKeep.Count == envClass.ParsedEnvItems.Count)
            return envClass;

        return EnvConstraintId.Create(itemsToKeep);
    }


    static (EnvConstraintId, ImmutableDictionary<EnvConstraintId, EnvConstraintId>) SimplifyEnvClassRecursive(
        Expression expression,
        EnvConstraintId envClass,
        IReadOnlyDictionary<EnvConstraintId, EnvConstraintId> simplifications,
        PineVMParseCache parseCache)
    {
        if (simplifications.TryGetValue(envClass, out var simplification))
        {
            return SimplifyEnvClassRecursive(expression, simplification, simplifications, parseCache);
        }

        var itemsToTestRemove =
            envClass.ParsedEnvItems
            .Where(item => 1 < item.Key.Count)
            .OrderByDescending(item => item.Key.Count)
            .ToArray();

        foreach (var itemToTestRemove in itemsToTestRemove)
        {
            var envClassSimplifiedItems =
                envClass.ParsedEnvItems
                .Except([itemToTestRemove]);

            var envClassSimplified =
                EnvConstraintId.Create([.. envClassSimplifiedItems]);

            Expression projectCompilation(EnvConstraintId envClass) =>
                PineVM.ReduceExpressionAndInlineRecursive(
                    rootExpression: expression,
                    rootExprAlternativeForms: [],
                    envConstraintId: envClass,
                    maxDepth: 7,
                    maxSubexpressionCount: 4_000,
                    parseCache: parseCache,
                    disableRecurseAfterInline: false,
                    skipInlining: (_, _) => false);

            var origReducedExpr =
                projectCompilation(envClass);

            var simplifiedReducedExprBeforeSubstitute =
                projectCompilation(envClassSimplified);

            var simplifiedReducedExpr =
                PineVM.SubstituteSubexpressionsForEnvironmentConstraint(
                    simplifiedReducedExprBeforeSubstitute,
                    envConstraintId: envClass);

            if (origReducedExpr.Equals(simplifiedReducedExpr))
            {
                var (envClassSimplifiedLeaf, addedSimplifications) =
                    SimplifyEnvClassRecursive(expression, envClassSimplified, simplifications, parseCache);

                return (envClassSimplifiedLeaf, addedSimplifications.SetItem(envClass, envClassSimplified));
            }
        }

        return (envClass, ImmutableDictionary<EnvConstraintId, EnvConstraintId>.Empty);
    }

    public static IReadOnlyList<(EnvConstraintId envClass, int matchCount)> GenerateEnvironmentClasses(
        IReadOnlyList<PineValue> values,
        int limitIntersectionCountPerValue,
        int classDepthLimit)
    {
        var distinctValues = new HashSet<PineValue>(values);

        var classes =
            distinctValues
            .SelectMany(value =>
            {
                return
                SubsequenceWithEvenDistribution(
                    [.. values.Where(otherValue => otherValue != value)],
                    limitIntersectionCountPerValue)
                .Select(otherValue => EnvConstraintId.CreateIntersection(value, otherValue, depthLimit: classDepthLimit));
            })
            .Where(envClass => envClass.ParsedEnvItems.Any())
            .ToImmutableArray();

        return
            [..classes
            .Distinct()
            .Select(envClass => (envClass, values.Count(value => envClass.SatisfiedByValue(value))))];
    }

    public static IReadOnlyList<T> SubsequenceWithEvenDistribution<T>(
        IReadOnlyList<T> source,
        int limitSampleCount) =>
        source.Count <= limitSampleCount
        ?
        source
        :
        [..source
        .Chunk(source.Count / limitSampleCount)
        .Select(chunk => chunk.First())
        .Take(limitSampleCount)];

    /// <summary>
    /// Filtering out some expressions known to lead to pathological cases with the current combination of interpreter and compiler.
    /// </summary>
    public static bool ExpressionCausesProblemsCompiled(CompilePineToDotNet.CompiledExpressionId expressionId)
    {
        if (expressionId.ExpressionHashBase16.Contains("085ab238"))
            return true;

        if (expressionId.ExpressionHashBase16.Contains("74c00c04"))
            return true;

        if (expressionId.ExpressionHashBase16.Contains("8cde1f66"))
            return true;

        if (expressionId.ExpressionHashBase16.Contains("f04e388a"))
            return true;

        return false;
    }
}
