using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.PineVM;

public record EnvConstraintId
{
    readonly static CompilePineToDotNet.CompilerMutableCache compilerCache = new();

    public IReadOnlyDictionary<IReadOnlyList<int>, PineValue> ParsedEnvItems { get; }

    readonly public string HashBase16;

    private EnvConstraintId(
        IReadOnlyDictionary<IReadOnlyList<int>, PineValue> parsedEnvItems,
        string hashBase16)
    {
        ParsedEnvItems = parsedEnvItems;

        HashBase16 = hashBase16;
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

    public static EnvConstraintId Create(
        ExpressionEnvClass.ConstrainedEnv envClass,
        PineValue envValue)
    {
        var parsedEnvItems =
            envClass.ParsedEnvItems
            .Select(path => new KeyValuePair<IReadOnlyList<int>, PineValue>(
                path,
                CodeAnalysis.ValueFromPathInValue(envValue, path) ?? throw new NullReferenceException()))
            .OrderBy(kv => kv.Key, IntPathComparer.Instance)
            .ToImmutableArray();

        var parsedEnvItemsPineValues =
            parsedEnvItems
            .OrderBy(kv => kv.Key, IntPathComparer.Instance)
            .Select(envItem =>
            PineValue.List(
                [PineValue.List([.. envItem.Key.Select(pathItem => PineValueAsInteger.ValueFromSignedInteger(pathItem))]),
                envItem.Value]))
            .ToImmutableArray();

        var hashBase16 =
            CommonConversion.StringBase16(compilerCache.ComputeHash(PineValue.List(parsedEnvItemsPineValues)));

        return new EnvConstraintId(
            parsedEnvItems.ToImmutableDictionary(keyComparer: IntPathEqualityComparer.Instance),
            hashBase16: hashBase16);
    }

    public virtual bool Equals(EnvConstraintId? other) =>
        Equal(this, other);
}

public class IntPathEqualityComparer : IEqualityComparer<IReadOnlyList<int>>
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

public abstract record ExpressionEnvClass
{
    public record UnconstrainedEnv
        : ExpressionEnvClass;

    public record ConstrainedEnv
        : ExpressionEnvClass
    {
        public ImmutableHashSet<IReadOnlyList<int>> ParsedEnvItems { get; }

        public ConstrainedEnv(IEnumerable<IReadOnlyList<int>> parsedEnvItems)
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

    public static ExpressionEnvClass Merge(
        ExpressionEnvClass env1,
        ExpressionEnvClass env2)
    {
        if (env1 is UnconstrainedEnv || env2 is UnconstrainedEnv)
            return new UnconstrainedEnv();

        if (env1 is ConstrainedEnv constrainedEnv1 && env2 is ConstrainedEnv constrainedEnv2)
        {
            var mergedParsedEnvItems =
                constrainedEnv1.ParsedEnvItems
                .Concat(constrainedEnv2.ParsedEnvItems)
                .ToImmutableHashSet();

            return new ConstrainedEnv(mergedParsedEnvItems);
        }

        throw new NotImplementedException();
    }

    public static ExpressionEnvClass MapToParentEnvironment(
        ExpressionEnvClass currentEnv,
        Expression envExpression)
    {
        if (currentEnv is UnconstrainedEnv)
            return new UnconstrainedEnv();

        if (currentEnv is ConstrainedEnv constrainedEnv)
        {
            var newParsedEnvItems =
                constrainedEnv.ParsedEnvItems
                .Select(path => TryMapPathToParentEnvironment(envExpression, path))
                .ToImmutableArray();

            return new ConstrainedEnv(newParsedEnvItems);
        }

        throw new NotImplementedException();
    }

    public static IReadOnlyList<int>? TryMapPathToParentEnvironment(
        Expression envExpr,
        IReadOnlyList<int> path)
    {
        if (path.Count == 0)
            return CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(envExpr);

        var currentIndex = path[0];

        if (envExpr is not Expression.ListExpression listExpr)
            return null;

        if (currentIndex < 0)
            return null;

        if (currentIndex >= listExpr.List.Length)
            return null;

        return TryMapPathToParentEnvironment(listExpr.List[currentIndex], [.. path.Skip(1)]);
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
        IImmutableDictionary<EnvConstraintId, ExpressionEnvClass> EnvDict);

    public record ExpressionUsageStackEntry(
        Expression Expression,
        EnvConstraintId EnvConstraintId)
    {
        public static bool Equal(
            ExpressionUsageStackEntry entry1,
            ExpressionUsageStackEntry entry2) =>
            ReferenceEquals(entry1, entry2) ||
            (entry1 is not null && entry2 is not null &&
            entry1.Expression.Equals(entry2.Expression) &&
            entry1.EnvConstraintId.Equals(entry2.EnvConstraintId));
    }

    public record ParseSubExpression(
        Expression.ParseAndEvalExpression ParseAndEvalExpr,
        IReadOnlyList<int>? ExpressionPath,
        PineValue? ExpressionValue);

    public static ExpressionEnvClass ComputeExpressionUsageRecordRecursive(
        IReadOnlyList<ExpressionUsageStackEntry> stack,
        Expression expression,
        PineValue environment,
        ConcurrentDictionary<Expression, ExprAnalysis> mutatedCache)
    {
        var parseSubexpressions =
            Expression.EnumerateSelfAndDescendants(expression)
            .OfType<Expression.ParseAndEvalExpression>()
            .Select(parseAndEvalExpr =>
            {
                var expressionPath =
                TryParseExpressionAsIndexPathFromEnv(parseAndEvalExpr.expression);

                var expressionValue =
                expressionPath is null ?
                null :
                ValueFromPathInValue(environment, expressionPath);

                return
                    new ParseSubExpression(
                        parseAndEvalExpr,
                        expressionPath,
                        expressionValue);
            })
            .ToImmutableArray();

        var currentParsedEnvItems =
            parseSubexpressions
            .Select(p =>
            {
                if (p.ExpressionPath is not { } expressionPath)
                    return null;

                if (p.ExpressionValue is not { } parsedExprValue)
                    return null;

                return expressionPath;
            })
            .ToImmutableArray();

        if (currentParsedEnvItems.Contains(null))
        {
            return new ExpressionEnvClass.UnconstrainedEnv();
        }

        var currentStackFrameEnv =
            new ExpressionEnvClass.ConstrainedEnv(currentParsedEnvItems);

        var currentEnvConstraintId = EnvConstraintId.Create(currentStackFrameEnv, environment);

        if (mutatedCache.TryGetValue(expression, out var cachedAnalysis))
        {
            if (cachedAnalysis.EnvDict.TryGetValue(currentEnvConstraintId, out var cachedEnvClass))
            {
                return cachedEnvClass;
            }
        }

        ExpressionEnvClass insertInCache(ExpressionEnvClass envClass)
        {
            mutatedCache.AddOrUpdate(
                expression,
                new ExprAnalysis(
                    EnvDict: ImmutableDictionary.CreateRange(
                        [new KeyValuePair<EnvConstraintId, ExpressionEnvClass>(currentEnvConstraintId, envClass)])),
                (expr, oldAnalysis) => new ExprAnalysis(oldAnalysis.EnvDict.SetItem(currentEnvConstraintId, envClass)));

            return envClass;
        }

        var currentStackFrame =
            new ExpressionUsageStackEntry(expression, currentEnvConstraintId);

        if (stack.Any(prevStackItem => ExpressionUsageStackEntry.Equal(prevStackItem, currentStackFrame)))
        {
            return insertInCache(currentStackFrameEnv);
        }

        var nextStack = stack.Append(currentStackFrame).ToImmutableArray();

        var descendantsEnvUsages =
            parseSubexpressions
            .Select(parseSubExpr =>
            {
                if (parseSubExpr.ExpressionValue is not { } parsedExprValue)
                {
                    throw new Exception("Unexpected null value");
                }

                Expression? parsedInnerExpr = null;

                try
                {
                    parsedInnerExpr =
                        PineVM.ParseExpressionFromValueDefault(parsedExprValue)
                        .WithDefault(null);
                }
                catch { }

                if (parsedInnerExpr is null)
                {
                    return new ExpressionEnvClass.UnconstrainedEnv();
                }

                PineValue? envValue = null;

                try
                {
                    envValue =
                    new PineVM()
                    /*
                     * Evaluation of the environment expression can fail here, since we are looking into all branches,
                     * including ones that are not reachable in the actual execution.
                     * In this case, classify the environment as unconstrained.
                     * */
                    .EvaluateExpressionDefault(parseSubExpr.ParseAndEvalExpr.environment, environment)
                    .WithDefault(null);
                }
                catch
                { }

                if (envValue is null)
                {
                    return new ExpressionEnvClass.UnconstrainedEnv();
                }

                var childEnvBeforeMapping =
                ComputeExpressionUsageRecordRecursive(
                    nextStack,
                    parsedInnerExpr,
                    envValue,
                    mutatedCache: mutatedCache);

                if (childEnvBeforeMapping is not ExpressionEnvClass.ConstrainedEnv constrainedEnv)
                {
                    return new ExpressionEnvClass.UnconstrainedEnv();
                }

                var envMappings = EnvItemsMappingsFromChildToParent(parseSubExpr.ParseAndEvalExpr.environment);

                IReadOnlyList<int>? TryMapPathToParentEnvironment(IReadOnlyList<int> path)
                {
                    var matchingEnvMappings =
                    envMappings
                    .Where(envMapping => envMapping.Key.SequenceEqual(path.Take(envMapping.Key.Count)))
                    .ToImmutableArray();

                    if (matchingEnvMappings.Length is 0)
                        return null;

                    var firstMatchingEnvMapping = matchingEnvMappings[0];

                    var pathRemainder = path.Skip(firstMatchingEnvMapping.Key.Count).ToImmutableArray();

                    return
                    [.. firstMatchingEnvMapping.Value, .. pathRemainder];
                }

                var parsedEnvItemsMapped =
                    constrainedEnv.ParsedEnvItems
                    .Select(path => TryMapPathToParentEnvironment(path))
                    .ToImmutableArray();

                if (parsedEnvItemsMapped.Contains(null))
                    return new ExpressionEnvClass.UnconstrainedEnv();

                return (ExpressionEnvClass)new ExpressionEnvClass.ConstrainedEnv(parsedEnvItemsMapped);
            })
            .ToImmutableArray();

        if (descendantsEnvUsages.Any(u => u is ExpressionEnvClass.UnconstrainedEnv))
        {
            return new ExpressionEnvClass.UnconstrainedEnv();
        }

        var mergedEnvClass =
            descendantsEnvUsages
            .Aggregate(
                seed: (ExpressionEnvClass)currentStackFrameEnv,
                ExpressionEnvClass.Merge);

        return insertInCache(mergedEnvClass);
    }

    public static IReadOnlyList<KeyValuePair<IReadOnlyList<int>, IReadOnlyList<int>>>
        EnvItemsMappingsFromChildToParent(Expression envExpression)
    {
        /*
         * 
        if (envExpression is Expression.EnvironmentExpression)
            return [new KeyValuePair<IReadOnlyList<int>, IReadOnlyList<int>>([], [])];
        */

        if (TryParseExpressionAsIndexPathFromEnv(envExpression) is { } path)
        {
            return [new KeyValuePair<IReadOnlyList<int>, IReadOnlyList<int>>([], path)];
        }

        if (envExpression is Expression.ListExpression envListExpr)
        {
            return
                [
                ..envListExpr.List
                .SelectMany((childExpr, childIndex) =>
                EnvItemsMappingsFromChildToParent(childExpr)
                .Select(childMapping => new KeyValuePair<IReadOnlyList<int>, IReadOnlyList<int>>(
                    [childIndex, .. childMapping.Key],
                    childMapping.Value)))
                ];
        }

        /*
         * TODO: Add a more general mapping from the child environment to the parent environment.
         * The special case we check here works for the typical form found in recursive calls.
         * */

        return [];
    }

    public static PineValue? ValueFromPathInValue(
        PineValue environment,
        IReadOnlyList<int> path)
    {
        if (path.Count is 0)
            return environment;

        if (environment is not PineValue.ListValue listValue)
            return null;

        if (path[0] < 0)
            return null;

        if (path[0] >= listValue.Elements.Count)
            return null;

        return ValueFromPathInValue(listValue.Elements[path[0]], [.. path.Skip(1)]);
    }

    /// <summary>
    /// Returns the path of list items starting from the environment to the expression given as the argument.
    /// </summary>
    public static IReadOnlyList<int>? TryParseExpressionAsIndexPathFromEnv(Expression expression)
    {
        if (expression is Expression.EnvironmentExpression)
            return [];

        if (expression is not Expression.KernelApplicationExpression kernelApplication)
            return null;

        if (kernelApplication.functionName is not nameof(KernelFunction.list_head))
            return null;

        if (kernelApplication.argument is Expression.KernelApplicationExpression argumentKernelApplication &&
            argumentKernelApplication.functionName is nameof(KernelFunction.skip))
        {
            if (argumentKernelApplication.argument is not Expression.ListExpression skipArgumentList)
                return null;

            if (skipArgumentList.List.Length is not 2)
                return null;

            if (skipArgumentList.List[0] is not Expression.LiteralExpression skipCountLiteral)
                return null;

            if (TryParseExpressionAsIndexPathFromEnv(skipArgumentList.List[1]) is not { } pathPrefix)
                return null;

            return
                PineValueAsInteger.SignedIntegerFromValue(skipCountLiteral.Value)
                .Unpack<IReadOnlyList<int>?>(
                    fromErr: _ => null,
                    fromOk: skipValue => [.. pathPrefix, (int)skipValue]);
        }

        {
            if (TryParseExpressionAsIndexPathFromEnv(kernelApplication.argument) is not { } pathPrefix)
                return null;

            return [.. pathPrefix, 0];
        }
    }
}
