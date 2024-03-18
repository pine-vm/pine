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
                CodeAnalysis.ValueFromPathInValue(envValue, [.. path]) ?? throw new NullReferenceException()))
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

    public ExpressionEnvClass AddConstraints(ExpressionEnvClass otherEnv)
    {
        if (this is UnconstrainedEnv || otherEnv is UnconstrainedEnv)
            return this;

        if (this is ConstrainedEnv selfConstrained && otherEnv is ConstrainedEnv otherConstrained)
        {
            var mergedParsedEnvItems =
                selfConstrained.ParsedEnvItems
                .Concat(otherConstrained.ParsedEnvItems)
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
                .SelectMany(path =>
                TryMapPathToParentEnvironment(envExpression, path) switch
                {
                    ExprMappedToParentEnv.LiteralInParentEnv _ =>
                    (IReadOnlyList<IReadOnlyList<int>>)[],

                    ExprMappedToParentEnv.PathInParentEnv pathInParentEnv =>
                    [pathInParentEnv.Path],

                    null =>
                    throw new NullReferenceException(),

                    { } other =>
                    throw new NotImplementedException(other.ToString())
                })
                .ToImmutableArray();

            return new ConstrainedEnv(newParsedEnvItems);
        }

        throw new NotImplementedException();
    }

    public static ExprMappedToParentEnv? TryMapPathToParentEnvironment(
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
                var expressionMapped = TryParseExpressionAsIndexPathFromEnv(parseAndEvalExpr.expression);

                var expressionValue =
                expressionMapped switch
                {
                    ExprMappedToParentEnv.LiteralInParentEnv literalInParentEnv =>
                    literalInParentEnv.Value,

                    ExprMappedToParentEnv.PathInParentEnv pathInParentEnv =>
                    ValueFromPathInValue(environment, [.. pathInParentEnv.Path]),

                    null =>
                    null,

                    _ =>
                    throw new NotImplementedException(expressionMapped.ToString())
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

                return
                    new ParseSubExpression(
                        parseAndEvalExpr,
                        ExpressionPath: (expressionMapped as ExprMappedToParentEnv.PathInParentEnv)?.Path,
                        expressionValue);
            })
            .ToImmutableArray();

        if (parseSubexpressions.Any(parseSubExpr => parseSubExpr.ExpressionValue is null))
        {
            return new ExpressionEnvClass.UnconstrainedEnv();
        }

        var currentParsedEnvItems =
            parseSubexpressions
            .Select(parseSubExpr => parseSubExpr.ExpressionPath)
            /*
             * ExpressionPath can be null if the expression is a literal.
             * */
            .WhereNotNull()
            .Distinct(IntPathEqualityComparer.Instance)
            .ToImmutableArray();

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
            return currentStackFrameEnv;
        }

        var nextStack = stack.Append(currentStackFrame).ToImmutableArray();

        ExpressionEnvClass computeChildClass(ParseSubExpression parseSubExpr)
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
                int parseCount = 0;

                envValue =
                new PineVM(overrideEvaluateExpression: defaultHandler =>
                new PineVM.EvalExprDelegate((expr, env) =>
                {
                    if (expr is Expression.ParseAndEvalExpression)
                    {
                        ++parseCount;
                    }

                    if (100 < parseCount)
                    {
                        throw new Exception("Too many parsing steps: " + parseCount);
                    }

                    return defaultHandler.Invoke(expr, env);
                }))
                /*
                 * Evaluation of the environment expression can fail here, since we are looking into all branches,
                 * including ones that are not reachable in the actual execution.
                 * In this case, classify the environment as unconstrained.
                 * */
                .EvaluateExpressionDefault(parseSubExpr.ParseAndEvalExpr.environment, environment)
                .WithDefault(null);
            }
            catch
            {
                return new ExpressionEnvClass.UnconstrainedEnv();
            }

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

            if (childEnvBeforeMapping is not ExpressionEnvClass.ConstrainedEnv childConstrainedEnv)
            {
                return new ExpressionEnvClass.UnconstrainedEnv();
            }

            var childPathEnvMap = BuildPathMapFromChildToParentEnv(parseSubExpr.ParseAndEvalExpr.environment);

            var parsedEnvItemsMapped =
                childConstrainedEnv.ParsedEnvItems
                .Select(path => childPathEnvMap(path))
                .ToImmutableArray();

            if (parsedEnvItemsMapped.Contains(null))
            {
                return new ExpressionEnvClass.UnconstrainedEnv();
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

            return new ExpressionEnvClass.ConstrainedEnv(parsedEnvItemsMappedPaths);
        }

        var descendantsEnvUsages =
            parseSubexpressions
            .Select(parseSubExpr => (parseSubExpr, childClass: computeChildClass(parseSubExpr)))
            .ToImmutableArray();

        var unconstrainedDescendants =
            descendantsEnvUsages
            .Where(u => u.childClass is ExpressionEnvClass.UnconstrainedEnv)
            .ToImmutableArray();

        if (0 < unconstrainedDescendants.Length)
        {
            /*
             * 2024-03-08: Observed later compilation stage failing when returning the currentStackFrameEnv here.

            return insertInCache(currentStackFrameEnv);
            */

            /*
             * Even better than returning the currentStackFrameEnv seems to be merging the ones we get in any case.
             * However, also with that variant, the later compilation stage failed with errors like these:
             * 
             * Compilation failed with 8 errors:
             * (413,21): error CS0103: The name 'bind_11465cd7d6' does not exist in the current context
             * (423,27): error CS0103: The name 'bind_3c95507845' does not exist in the current context
             * [...]
             * 
             * Looks like propagation of let-bindings dependencies failed.
             * 
             * So returning UnconstrainedEnv here seems more like a temporary workaround.
             * */

            return insertInCache(new ExpressionEnvClass.UnconstrainedEnv());
        }

        var mergedEnvClass =
            descendantsEnvUsages
            .Aggregate(
                seed: (ExpressionEnvClass)currentStackFrameEnv,
                (aggr, next) => aggr.AddConstraints(next.childClass));

        return insertInCache(mergedEnvClass);
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

        if (envExpression is Expression.ListExpression envListExpr)
        {
            return
                [
                ..envListExpr.List
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

        if (environment is not PineValue.ListValue listValue)
            return null;

        if (path[0] < 0)
            return null;

        if (path[0] >= listValue.Elements.Count)
            return null;

        return ValueFromPathInValue(listValue.Elements[path[0]], path[1..]);
    }

    /// <summary>
    /// Returns the path of list items starting from the environment to the expression given as the argument.
    /// </summary>
    public static ExprMappedToParentEnv? TryParseExpressionAsIndexPathFromEnv(Expression expression)
    {
        if (expression is Expression.EnvironmentExpression)
            return new ExprMappedToParentEnv.PathInParentEnv([]);

        if (expression is Expression.LiteralExpression literal)
            return new ExprMappedToParentEnv.LiteralInParentEnv(literal.Value);

        if (expression is Expression.StringTagExpression stringTagExpr)
            return TryParseExpressionAsIndexPathFromEnv(stringTagExpr.tagged);

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

            if (TryParseExpressionAsIndexPathFromEnv(skipArgumentList.List[1]) is not ExprMappedToParentEnv.PathInParentEnv pathPrefix)
                return null;

            return
                PineValueAsInteger.SignedIntegerFromValue(skipCountLiteral.Value)
                    .Unpack<ExprMappedToParentEnv?>(
                        fromErr: _ => null,
                        fromOk: skipValue => new ExprMappedToParentEnv.PathInParentEnv([.. pathPrefix.Path, (int)skipValue]));
        }

        {
            if (TryParseExpressionAsIndexPathFromEnv(kernelApplication.argument) is not ExprMappedToParentEnv.PathInParentEnv pathPrefix)
                return null;

            return new ExprMappedToParentEnv.PathInParentEnv([.. pathPrefix.Path, 0]);
        }
    }
}
