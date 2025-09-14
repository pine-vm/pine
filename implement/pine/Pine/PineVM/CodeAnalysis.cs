using Pine.Core;
using Pine.Core.Addressing;
using Pine.Core.CodeAnalysis;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Concurrent;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

using StaticExpressionGen = Pine.Core.CodeAnalysis.StaticExpression<Pine.Core.CodeAnalysis.StaticFunctionIdentifier>;

namespace Pine.PineVM;



public record RecursiveAnalysisResult(
    ExpressionEnvClass RootEnvClass,
    ImmutableHashSet<ExprOnRecursionPathEntry> ExprOnRecursionPath,
    ImmutableHashSet<(Expression expr, PineValueClass expandedConstraint)> UsagesCompleteForRecursion);

public record ExprOnRecursionPathEntry(
    PineValueClass RootConstraint,
    Expression RootExpr,
    PineValueClass Constraint,
    Expression Expr);

public class DelegatingEqualityComparer<T>(
    Func<T?, T?, bool> equals,
    Func<T, int> getHashCode)
    : IEqualityComparer<T>
{
    private readonly Func<T?, T?, bool> _equals = equals;
    private readonly Func<T, int> _getHashCode = getHashCode;

    public bool Equals(T? x, T? y) =>
        _equals(x, y);

    public int GetHashCode(T obj) => _getHashCode(obj);
}

public class CodeAnalysis
{
    public record ExprAnalysis(
        IImmutableDictionary<PineValue, RecursiveAnalysisResult> EnvDict);

    public record ExprUsageAnalysisStackEntry(
        Expression Expression,
        CompilePineToDotNet.CompiledExpressionId ExpressionId,
        PineValueClass EnvConstraintId,
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
                var encodedMapped =
                Core.CodeAnalysis.CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(parseAndEvalExpr.Encoded);

                var expressionValue =
                encodedMapped switch
                {
                    ExprMappedToParentEnv.LiteralInParentEnv literalInParentEnv =>
                    literalInParentEnv.Value,

                    ExprMappedToParentEnv.PathInParentEnv pathInParentEnv =>
                    Core.CodeAnalysis.CodeAnalysis.ValueFromPathInValue(environment, [.. pathInParentEnv.Path]),

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
            PineValueClass.Create(
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

        RecursiveAnalysisResult InsertInCache(RecursiveAnalysisResult recursiveAnalysisResult)
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

        RecursiveAnalysisResult ComputeChildClass(ParseSubExpression parseSubExpr)
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
                    ExprMappedToParentEnv.LiteralInParentEnv =>
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
                PineValueClass.Create(
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
            .Select(parseSubExpr => (parseSubExpr, childAnalysis: ComputeChildClass(parseSubExpr)))
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
            PineValueClass.Create(
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
                    return (IEnumerable<(Expression, PineValueClass)>)[(entry.Expr, mergedConstraintId)];
                }

                return [];
            })
            .ToImmutableHashSet();

        var mergedEnvClass =
            new ExpressionEnvClass.ConstrainedEnv(mergedParsedEnvItems);

        return
            InsertInCache(
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

        if (Core.CodeAnalysis.CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(envExpression) is { } path)
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


    public static IReadOnlyDictionary<Expression, IReadOnlyList<PineValueClass>> EnvironmentClassesFromInvocationReports(
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

    public static IReadOnlyDictionary<Expression, IReadOnlyList<PineValueClass>> EnvironmentClassesFromInvocationReports(
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


    public static IReadOnlyList<PineValueClass> EnvironmentClassesFromExpressionInvocationReports(
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

        var classSimplifications = new Dictionary<PineValueClass, PineValueClass>();

        var simplifiedClasses = new HashSet<PineValueClass>();

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

            if (0 < envClassSimplified.ParsedItems.Count)
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

    static PineValueClass SimplifyEnvClassShallow(
        Expression expression,
        PineValueClass envClass)
    {
        var shallowObservedPaths =
            Expression.EnumerateSelfAndDescendants(expression)
            .SelectWhereNotNull(
                expr =>
                Core.CodeAnalysis.CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(expr) is ExprMappedToParentEnv.PathInParentEnv path
                ?
                path.Path
                :
                null)
            .ToHashSet(IntPathEqualityComparer.Instance);

        bool KeepClassItemPath(IReadOnlyList<int> classItemPath)
        {
            foreach (var observedPath in shallowObservedPaths)
            {
                if (observedPath.Count < classItemPath.Count)
                    continue;

                var mismatch = false;

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
            envClass.ParsedItems
            .Where(classItem => KeepClassItemPath(classItem.Key))
            .ToList();

        if (itemsToKeep.Count == envClass.ParsedItems.Count)
            return envClass;

        return PineValueClass.Create(itemsToKeep);
    }


    static public long SimplifyEnvClassCount { private set; get; } = 0;


    static PineValueClass SimplifyEnvClass(
        Expression rootExpression,
        PineValueClass envClass,
        int depthMax,
        PineVMParseCache parseCache)
    {
        ++SimplifyEnvClassCount;


        static ImmutableHashSet<IReadOnlyList<int>> ObservedPathsFromExpression(Expression expression) =>
            Expression.EnumerateSelfAndDescendants(expression)
            .SelectWhereNotNull(
                expr =>
                Core.CodeAnalysis.CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(expr) is ExprMappedToParentEnv.PathInParentEnv path
                ?
                path.Path
                :
                null)
            .ToImmutableHashSet(IntPathEqualityComparer.Instance);

        var rootObservedPaths = ObservedPathsFromExpression(rootExpression);

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

            levelsObservedPaths.Add(ObservedPathsFromExpression(currentExpression));
        }

        var aggregateObservedPaths =
            levelsObservedPaths.Aggregate(
                func: (a, b) => a.Union(b),
                seed: rootObservedPaths);

        bool KeepClassItemPath(IReadOnlyList<int> classItemPath)
        {
            foreach (var observedPath in aggregateObservedPaths)
            {
                if (observedPath.Count < classItemPath.Count)
                    continue;

                var mismatch = false;

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
            envClass.ParsedItems
            .Where(classItem => KeepClassItemPath(classItem.Key))
            .ToList();

        if (itemsToKeep.Count == envClass.ParsedItems.Count)
            return envClass;

        return PineValueClass.Create(itemsToKeep);
    }


    static (PineValueClass, ImmutableDictionary<PineValueClass, PineValueClass>) SimplifyEnvClassRecursive(
        Expression expression,
        PineValueClass envClass,
        IReadOnlyDictionary<PineValueClass, PineValueClass> simplifications,
        PineVMParseCache parseCache)
    {
        if (simplifications.TryGetValue(envClass, out var simplification))
        {
            return SimplifyEnvClassRecursive(expression, simplification, simplifications, parseCache);
        }

        var itemsToTestRemove =
            envClass.ParsedItems
            .Where(item => 1 < item.Key.Count)
            .OrderByDescending(item => item.Key.Count)
            .ToArray();

        foreach (var itemToTestRemove in itemsToTestRemove)
        {
            var envClassSimplifiedItems =
                envClass.ParsedItems
                .Except([itemToTestRemove]);

            var envClassSimplified =
                PineValueClass.Create([.. envClassSimplifiedItems]);

            Expression ProjectCompilation(PineValueClass envClass) =>
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
                ProjectCompilation(envClass);

            var simplifiedReducedExprBeforeSubstitute =
                ProjectCompilation(envClassSimplified);

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

        return (envClass, ImmutableDictionary<PineValueClass, PineValueClass>.Empty);
    }

    public static IReadOnlyList<(PineValueClass envClass, int matchCount)> GenerateEnvironmentClasses(
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
                .Select(otherValue => PineValueClass.CreateIntersection(value, otherValue, depthLimit: classDepthLimit));
            })
            .Where(envClass => envClass.ParsedItems.Any())
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

    public static Result<string, (StaticProgram staticProgram, StaticExpression<string>.FunctionApplication entryPoint)>
        ParseAsStaticMonomorphicProgramAssigningNames(
        Expression rootExpression,
        PineValue rootEnvironment,
        Func<PineValue, PineValueClass, string?> nameForDecl,
        PineVMParseCache parseCache)
    {
        var lessSpecializedInterfacesResult =
            ParseAsStaticMonomorphicProgram(
                rootExpression,
                rootEnvironment,
                parseCache);

        {
            if (lessSpecializedInterfacesResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse as static monomorphic program: " + err;
            }
        }

        if (lessSpecializedInterfacesResult.IsOkOrNull() is not { } lessSpecializedInterfaces)
        {
            throw new Exception(
                "Unexpected return type: " +
                lessSpecializedInterfacesResult.GetType().Name);
        }

        StaticFunctionInterface InterfaceForFunction<T>(
            StaticExpression<T> functionBody)
        {
            var allImplicitParam = StaticExpression<T>.ImplicitFunctionParameterList(functionBody);

            /*
             * TODO: Replace stupid heuristic with an adaptive approach:
             * Goal is to reduce the need for callers to compose list instances to hand over arguments.
             * Perhaps we will switch to a separate stage for determining the specialized function interfaces.
             * (We might want to look at all the call sites to determine the optimal shape of the parameter list)
             * */

            var shortenedParams =
                allImplicitParam
                .Select(p => (IReadOnlyList<int>)[.. p.Take(2)]) // Take at most 2 elements from each implicit param path
                .Distinct(IntPathEqualityComparer.Instance)
                .ToArray();

            bool PathIsPrefixOfOtherPath(IReadOnlyList<int> path, IReadOnlyList<int> otherPath)
            {
                if (path.Count >= otherPath.Count)
                    return false;

                for (var i = 0; i < path.Count; ++i)
                {
                    if (path[i] != otherPath[i])
                        return false;
                }

                return true;
            }

            var inludedParams =
                shortenedParams
                .Where(p => !shortenedParams.Any(other => PathIsPrefixOfOtherPath(p, other)))
                .ToArray();

            return StaticFunctionInterface.FromPathsSorted(inludedParams);
        }

        var hashCache = new ConcurrentPineValueHashCache();

        string DeclNameCombined(StaticFunctionIdentifier functionIdentifier)
        {
            return
                nameForDecl(
                    functionIdentifier.EncodedExpr,
                    functionIdentifier.EnvClass)
                ??
                AnonymousFunctionName(
                    functionIdentifier.EncodedExpr,
                    functionIdentifier.EnvClass,
                    hashCache);
        }

        var namedFunctions =
            lessSpecializedInterfaces
            .ToFrozenDictionary(
                keySelector:
                entry => DeclNameCombined(entry.Key),
                elementSelector:
                entry =>
                {
                    var bodyMapped = StaticExpressionGen.MapFunctionIdentifier(entry.Value.body, DeclNameCombined);

                    return
                        (entry.Value.origExpr, interf: InterfaceForFunction(entry.Value.body), bodyMapped, entry.Key.EnvClass);
                });


        /*
         * For now, specialize in cases where root is a simple invocation.
         * */

        if (rootExpression is not Expression.ParseAndEval rootParseAndEval)
        {
            throw new NotImplementedException();
        }

        if (Core.CodeAnalysis.CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(rootParseAndEval.Encoded) is not
            ExprMappedToParentEnv.PathInParentEnv rootInvocationEncodedPath)
        {
            throw new NotImplementedException();
        }

        if (Core.CodeAnalysis.CodeAnalysis.ValueFromPathInValue(rootEnvironment, rootInvocationEncodedPath.Path.ToArray()) is not { } rootInvocationExprValue)
        {
            throw new NotImplementedException();
        }

        if (ExpressionEncoding.ParseExpressionFromValue(rootInvocationExprValue).IsOkOrNull() is not { } rootInvocationExpr)
        {
            throw new NotImplementedException();
        }

        if (rootParseAndEval.Environment is not Expression.Environment)
        {
            throw new NotImplementedException();
        }

        var entryFunctionMatchingExprs =
            namedFunctions
            .Where(c => c.Value.origExpr == rootInvocationExpr)
            .ToImmutableArray();

        var entryFunction =
            entryFunctionMatchingExprs
            .Single(c => c.Value.EnvClass.SatisfiedByValue(rootEnvironment));

        return
            (new StaticProgram(namedFunctions),
            StaticExpression<string>.FunctionApplicationInstance(
                functionName: entryFunction.Key,
                arguments: StaticExpression<string>.ListInstance([])));
    }

    public static Result<string, IReadOnlyDictionary<StaticFunctionIdentifier, (Expression origExpr, StaticExpressionGen body)>>
        ParseAsStaticMonomorphicProgram(
        Expression rootExpression,
        PineValue rootEnvironment,
        PineVMParseCache parseCache)
    {
        var observedSetInitial =
            new List<(Expression origExpr, Expression inlinedExpr, PineValueClass constraint)>();

        var queue = new Queue<(Expression expr, PineValueClass envValueClass)>();

        var rootEnvValueClass =
            PineValueClass.CreateEquals(rootEnvironment);

        queue.Enqueue((rootExpression, rootEnvValueClass));

        while (queue.Count > 0)
        {
            var current = queue.Dequeue();

            if (observedSetInitial.Any(entry => entry.origExpr == current.expr && entry.constraint.Equals(current.envValueClass)))
                continue;

            var currentExprInlined =
                InlineParseAndEvalUsingLiteralFunction(current.expr, parseCache);

            /*
             * Do not record root in observed set, as we will use it as the entry point anyway.
             * */

            if (!(current.expr == rootExpression &&
                current.envValueClass == rootEnvValueClass))
            {
                observedSetInitial.Add((current.expr, currentExprInlined, current.envValueClass));
            }

            var allParseAndEvalExpressions =
                Expression.EnumerateSelfAndDescendants(currentExprInlined)
                .OfType<Expression.ParseAndEval>()
                .ToImmutableArray();

            foreach (var parseAndEvalExpr in allParseAndEvalExpressions)
            {
                if (ParseAndEvalCrashingAlways(parseAndEvalExpr, parseCache))
                {
                    /*
                     * Some compilers create crashing branches like these; it's an allowed pattern.
                     * For static analysis, this means we can ignore the parse and eval expression.
                     * */

                    continue;
                }

                /*
                 * Limited support for scenarios:
                 * As a constraint for the current implementation, we only forward parts that do not depend on
                 * any parse&eval expressions themselves.
                 * */


                /*
                 * For now, try an even simpler subset:
                 * Only forward items modeled in the popular environment composition style:
                 * */

                var childEnvClass = MapValueClass(current.envValueClass, parseAndEvalExpr.Environment);

                if (childEnvClass is null)
                {
                    return "Could not map environment value class through parseAndEval.Environment";
                }

                /*
                 * Special case also for the encoded part of parse&eval:
                 * */

                if (Core.CodeAnalysis.CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(parseAndEvalExpr.Encoded) is not ExprMappedToParentEnv.PathInParentEnv pathInParentEnv)
                {
                    // Cannot interpret the encoded part as a path from the environment.
                    // This means we cannot statically determine what part of the environment
                    // is parsed by this parse&eval expression.
                    // Therefore, we have to give up on static analysis of this program.

                    return "Could not interpret parseAndEval.Encoded as path from environment";
                }

                if (current.envValueClass.TryGetValue(pathInParentEnv.Path) is not { } parsedValue)
                {
                    return "Could not find value for parseAndEval.Encoded in the given environment";
                }

                var childParseResult = parseCache.ParseExpression(parsedValue);

                {
                    if (childParseResult.IsErrOrNull() is { } err)
                    {
                        return "Failed to parse parseAndEval.Encoded value: " + err;
                    }
                }

                if (childParseResult.IsOkOrNull() is not { } childExpr)
                {
                    throw new Exception(
                        "Unexpected return type: " +
                        childParseResult.GetType().Name);
                }

                queue.Enqueue((childExpr, childEnvClass));
            }
        }

        var observedSet =
            observedSetInitial
            .Select(entry =>
            {

                /*
                * 2025-09-14:
                * So far, the mapped value class can contain items that are not used to decode program code.
                * Therefore we filter out all items that do not encode epressions.
                * This is not precise, since there could be superfluous expression items as well.
                * For proper filtering, we probably need to track which items are actually used in the inlined expression.
                * TODO: Find a more precise way to determine which items are relevant.
                * */

                var constraint =
                    FilterClassRemovingAllNonExpressions(entry.constraint, parseCache);

                return
                (entry.origExpr, entry.inlinedExpr, constraint);
            })
            .DistinctBy(entry => (entry.inlinedExpr, entry.constraint))
            .ToArray();

        /*
         * Filter observed set, to remove redundant entries which are already covered by smaller environments.
         * 
         * Here, we assume that every entry in the 'observed' set is completely static, as we would have exited
         * already if we had encountered something dynamic in the exploration above.
         * */

        bool ObservedIsRedundant(
            (Expression origExpr, Expression inlinedExpr, PineValueClass constraint) observedCombo)
        {
            foreach (var other in observedSet)
            {
                if (observedCombo == other)
                    continue;

                if (other.origExpr == observedCombo.origExpr &&
                    other.constraint.SatisfiedByConstraint(observedCombo.constraint))
                {
                    return true;
                }
            }

            return false;
        }

        var filteredObservedSet =
            observedSet
            .Where(observedCombo => !ObservedIsRedundant(observedCombo))
            .ToImmutableArray();

        var genFunctions =
            new Dictionary<StaticFunctionIdentifier, (Expression origExpr, StaticExpressionGen body)>();

        foreach (var observedCombo in filteredObservedSet)
        {
            var parseExprResult =
                ParseAsStaticExpression(
                    observedCombo.inlinedExpr,
                    observedCombo.constraint,
                    parseCache);

            if (parseExprResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse expression as static expression: " + err;
            }

            if (parseExprResult.IsOkOrNull() is not { } staticExpr)
            {
                throw new Exception(
                    "Unexpected return type: " +
                    parseExprResult.GetType().Name);
            }

            var exprValue =
                ExpressionEncoding.EncodeExpressionAsValue(observedCombo.origExpr);

            var functionIdent =
                new StaticFunctionIdentifier(exprValue, observedCombo.constraint);

            if (genFunctions.ContainsKey(functionIdent))
            {
                return "Hash collision detected for function name: " + functionIdent;
            }

            genFunctions[functionIdent] = (observedCombo.origExpr, staticExpr);
        }

        return genFunctions;
    }

    static PineValueClass FilterClassRemovingAllNonExpressions(
        PineValueClass valueClass,
        PineVMParseCache parseCache)
    {
        bool KeepEntry(PineValue value)
        {
            var parseResult = parseCache.ParseExpression(value);

            if (parseResult.IsOkOrNull() is { })
            {
                return true;
            }

            if (value is PineValue.ListValue listValue)
            {
                for (var i = 0; i < listValue.Items.Length; ++i)
                {
                    if (KeepEntry(listValue.Items.Span[i]))
                    {
                        return true;
                    }
                }
            }

            return false;
        }

        var entriesRemaining =
            valueClass.ParsedItems
            .Where(parsedItem => KeepEntry(parsedItem.Value))
            .ToArray();

        return PineValueClass.Create(entriesRemaining);
    }

    static Expression InlineParseAndEvalUsingLiteralFunction(
        Expression expression,
        PineVMParseCache parseCache)
    {
        return
            CompilePineToDotNet.ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
            findReplacement: expr =>
            {
                if (expr is not Expression.ParseAndEval parseAndEval)
                {
                    return null;
                }

                if (parseAndEval.Encoded.ReferencesEnvironment)
                {
                    // Cannot inline parse&eval expressions that reference the environment.
                    return null;
                }

                var parseIndependentResult = ParseIndependentParseAndEvalExpression(parseAndEval, parseCache);

                if (parseIndependentResult.IsOkOrNull() is not { } staticExpr)
                {
                    // Cannot inline parse&eval expressions that cannot be parsed independently.
                    return null;
                }

                var inlinedExpr =
                    CompilePineToDotNet.ReducePineExpression.TransformPineExpressionWithOptionalReplacement(
                        findReplacement:
                        descendant =>
                        {
                            if (descendant is Expression.Environment)
                            {
                                return parseAndEval.Environment;
                            }

                            return null;
                        },
                        staticExpr).expr;

                var inlinedExprReduced =
                    CompilePineToDotNet.ReducePineExpression.ReduceExpressionBottomUp(inlinedExpr);

                return inlinedExprReduced;
            },
            expression).expr;
    }

    static Result<string, StaticExpressionGen> ParseAsStaticExpression(
        Expression expression,
        PineValueClass envValueClass,
        PineVMParseCache parseCache)
    {
        if (Core.CodeAnalysis.CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(expression) is
            ExprMappedToParentEnv.PathInParentEnv asPath)
        {
            if (envValueClass.TryGetValue(asPath.Path) is { } valueFromEnvClass)
            {
                return
                    StaticExpressionGen.LiteralInstance(valueFromEnvClass);
            }
        }

        if (expression is Expression.Literal literal)
        {
            return StaticExpressionGen.LiteralInstance(literal.Value);
        }

        if (expression is Expression.Environment)
        {
            return StaticExpressionGen.EnvironmentInstance;
        }

        if (expression is Expression.ParseAndEval parseAndEval)
        {
            var parseEncodedResult =
                ParseAsStaticExpression(
                    parseAndEval.Encoded,
                    envValueClass,
                    parseCache);

            {
                if (parseEncodedResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse parseAndEval.Encoded as static expression: " + err;
                }
            }

            if (parseEncodedResult.IsOkOrNull() is not { } encodedExpr)
            {
                throw new Exception(
                    "Unexpected return type: " +
                    parseEncodedResult.GetType().Name);
            }

            var parseEnvResult =
                ParseAsStaticExpression(
                    parseAndEval.Environment,
                    envValueClass,
                    parseCache);

            {
                if (parseEnvResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse parseAndEval.Environment as static expression: " + err;
                }
            }

            if (parseEnvResult.IsOkOrNull() is not { } envExpr)
            {
                throw new Exception(
                    "Unexpected return type: " +
                    parseEnvResult.GetType().Name);
            }

            if (ParseAndEvalCrashingAlways(parseAndEval, parseCache))
            {
                return new StaticExpressionGen.CrashingParseAndEval(
                    encodedExpr,
                    envExpr);
            }

            // Try get function value from the environment class:

            if (Core.CodeAnalysis.CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(parseAndEval.Encoded) is not
                ExprMappedToParentEnv.PathInParentEnv pathInParentEnv)
            {
                return "Could not interpret parseAndEval.Encoded as path from environment";
            }

            if (envValueClass.TryGetValue(pathInParentEnv.Path) is not { } parsedValue)
            {
                return "Could not find value for parseAndEval.Encoded in the given environment";
            }

            var childEnvClass = MapValueClass(envValueClass, parseAndEval.Environment);

            if (childEnvClass is null)
            {
                return "Could not map environment value class through parseAndEval.Environment";
            }

            var parseExprResult = parseCache.ParseExpression(parsedValue);

            {
                if (parseExprResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse parseAndEval.Encoded value: " + err;
                }
            }

            if (parseExprResult.IsOkOrNull() is not { } childExpr)
            {
                throw new Exception(
                    "Unexpected return type: " +
                    parseExprResult.GetType().Name);
            }

            var childFunctionName =
                new StaticFunctionIdentifier(
                    parsedValue,
                    childEnvClass);

            return
                StaticExpressionGen.FunctionApplicationInstance(
                    functionName: childFunctionName,
                    arguments: envExpr);
        }

        if (expression is Expression.List listExpr)
        {
            var parsedItems = new StaticExpressionGen[listExpr.items.Count];

            for (var itemIndex = 0; itemIndex < parsedItems.Length; ++itemIndex)
            {
                var item = listExpr.items[itemIndex];

                var parseItemResult =
                    ParseAsStaticExpression(
                        item,
                        envValueClass,
                        parseCache);

                if (parseItemResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse list item: " + err;
                }

                if (parseItemResult.IsOkOrNull() is not { } parsedItem)
                {
                    throw new Exception(
                        "Unexpected return type: " +
                        parseItemResult.GetType().Name);
                }

                parsedItems[itemIndex] = parsedItem;
            }

            return StaticExpressionGen.ListInstance(parsedItems);
        }

        if (expression is Expression.KernelApplication kernelApp)
        {
            var parseInputResult =
                ParseAsStaticExpression(
                    kernelApp.Input,
                    envValueClass,
                    parseCache);

            if (parseInputResult.IsErrOrNull() is { } err)
            {
                return "Failed to parse input of kernel application: " + err;
            }

            if (parseInputResult.IsOkOrNull() is not { } inputExpr)
            {
                throw new Exception(
                    "Unexpected return type: " +
                    parseInputResult.GetType().Name);
            }

            return
                StaticExpressionGen.KernelApplicationInstance(
                    function: kernelApp.Function,
                    input: inputExpr);
        }

        if (expression is Expression.Conditional conditional)
        {
            var parseConditionResult =
                ParseAsStaticExpression(
                    conditional.Condition,
                    envValueClass,
                    parseCache);

            {
                if (parseConditionResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse condition of conditional: " + err;
                }
            }

            if (parseConditionResult.IsOkOrNull() is not { } conditionExpr)
            {
                throw new Exception(
                    "Unexpected return type: " +
                    parseConditionResult.GetType().Name);
            }

            var falseBranchResult =
                ParseAsStaticExpression(
                    conditional.FalseBranch,
                    envValueClass,
                    parseCache);

            {
                if (falseBranchResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse false branch of conditional: " + err;
                }
            }

            if (falseBranchResult.IsOkOrNull() is not { } falseBranchExpr)
            {
                throw new Exception(
                    "Unexpected return type: " +
                    falseBranchResult.GetType().Name);
            }

            var trueBranchResult =
                ParseAsStaticExpression(
                    conditional.TrueBranch,
                    envValueClass,
                    parseCache);

            {
                if (trueBranchResult.IsErrOrNull() is { } err)
                {
                    return "Failed to parse true branch of conditional: " + err;
                }
            }

            if (trueBranchResult.IsOkOrNull() is not { } trueBranchExpr)
            {
                throw new Exception(
                    "Unexpected return type: " +
                    trueBranchResult.GetType().Name);
            }

            return
                StaticExpressionGen.ConditionalInstance(
                    condition: conditionExpr,
                    trueBranch: trueBranchExpr,
                    falseBranch: falseBranchExpr);
        }

        throw new NotImplementedException(
            "Unexpected expression type: " +
            expression.GetType().Name);
    }

    private static bool ParseAndEvalCrashingAlways(
        Expression.ParseAndEval parseAndEval,
        PineVMParseCache parseCache)
    {
        if (parseAndEval.Encoded.ReferencesEnvironment)
        {
            return false;
        }

        var parseIndependentResult =
            ParseIndependentParseAndEvalExpression(parseAndEval, parseCache);

        if (parseIndependentResult.IsErrOrNull() is { })
        {
            return true;
        }

        return false;
    }

    private static Result<object, Expression>
        ParseIndependentParseAndEvalExpression(
        Expression.ParseAndEval parseAndEval,
        PineVMParseCache parseCache)
    {
        if (parseAndEval.Encoded.ReferencesEnvironment)
        {
            throw new InvalidOperationException(
                "Cannot parse independently if parseAndEval.Encoded references the environment");
        }

        var evalIndependentResult =
            CompilePineToDotNet.ReducePineExpression.TryEvaluateExpressionIndependent(parseAndEval.Encoded);

        {
            if (evalIndependentResult.IsErrOrNull() is { } err)
            {
                throw new Exception("Failed to evaluate parseAndEval.Encoded independently: " + err);
            }
        }

        if (evalIndependentResult.IsOkOrNull() is not { } evalIndependentValue)
        {
            throw new Exception(
                "Unexpected return type: " +
                evalIndependentResult.GetType().Name);
        }

        var parseExprResult = parseCache.ParseExpression(evalIndependentValue);

        if (parseExprResult.IsOkOrNull() is not { } parsedExpr)
        {
            /*
             * If parsing of a literal fails, it means the program will crash at runtime if the containing branch is taken.
             * */

            return Result<object, Expression>.err("");
        }

        return Result<object, Expression>.ok(parsedExpr);
    }

    private static string AnonymousFunctionName(
        PineValue exprValue,
        PineValueClass pineValueClass,
        ConcurrentPineValueHashCache hashCache)
    {
        var exprHash = hashCache.GetHash(exprValue);

        var exprHashBase16 = Convert.ToHexStringLower(exprHash.Span);


        return $"anon_{exprHashBase16[..8]}_{pineValueClass.HashBase16[..8]}";
    }

    private static PineValueClass? MapValueClass(
        PineValueClass envClass,
        Expression expression)
    {
        if (expression is Expression.Environment)
        {
            return envClass;
        }

        if (expression is Expression.Literal literal)
        {
            return PineValueClass.CreateEquals(literal.Value);
        }

        if (Core.CodeAnalysis.CodeAnalysis.TryParseExpressionAsIndexPathFromEnv(expression) is
            ExprMappedToParentEnv.PathInParentEnv pathInParentEnv)
        {
            if (envClass.TryGetValue(pathInParentEnv.Path) is not { } valueAtPath)
            {
                return null;
            }

            return PineValueClass.CreateEquals(valueAtPath);
        }

        if (expression is Expression.List listExpr)
        {
            var itemClasses = new PineValueClass?[listExpr.items.Count];

            for (var itemIndex = 0; itemIndex < listExpr.items.Count; ++itemIndex)
            {
                var item = listExpr.items[itemIndex];

                var itemClass = MapValueClass(envClass, item);

                itemClasses[itemIndex] = itemClass;
            }

            var itemsFlattened = new List<KeyValuePair<IReadOnlyList<int>, PineValue>>();

            for (var itemIndex = 0; itemIndex < itemClasses.Length; ++itemIndex)
            {
                if (itemClasses[itemIndex] is not { } itemClass)
                {
                    continue;
                }

                foreach (var classItem in itemClass.ParsedItems)
                {
                    itemsFlattened.Add(
                        new KeyValuePair<IReadOnlyList<int>, PineValue>(
                            [itemIndex, .. classItem.Key],
                            classItem.Value));
                }
            }

            if (itemsFlattened.Count is 0)
                return null;

            return
                PineValueClass.Create(itemsFlattened);
        }

        return null;
    }
}
