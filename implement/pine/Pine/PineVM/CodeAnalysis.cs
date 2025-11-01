using Pine.Core;
using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

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
        IImmutableDictionary<PineValueClass, RecursiveAnalysisResult> EnvDict);

    public record ExprUsageAnalysisStackEntry(
        Expression Expression,
        CompilePineToDotNet.CompiledExpressionId ExpressionId,
        PineValueClass EnvConstraintId,
        PineValueClass Environment);

    public record ParseSubExpression(
        Expression.ParseAndEval ParseAndEvalExpr,
        IReadOnlyList<int>? ExpressionPath,
        PineValue? ExpressionValue,
        Expression? ParsedExpr);

    public static RecursiveAnalysisResult AnalyzeExpressionUsageRecursive(
        IReadOnlyList<ExprUsageAnalysisStackEntry> stack,
        Expression expression,
        PineValueClass environment,
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
                    environment.TryGetValue(pathInParentEnv.Path),

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
                        [new KeyValuePair<PineValueClass, RecursiveAnalysisResult>(environment, recursiveAnalysisResult)])),
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
                    PineValueClassExtensions.CreateMinimalValue(environment),
                    config: new PineVM.EvaluationConfig(ParseAndEvalCountLimit: 100))
                .Unpack(
                    fromErr: _ => null,
                    fromOk: ok => ok.ReturnValue.Evaluate());
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
                    PineValueClass.CreateEquals(childEnvValue),
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
                ..envListExpr.Items
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
        IReadOnlyList<EvaluationReport> invocationReports,
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
        IReadOnlyList<EvaluationReport> invocationReports,
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
                    invocationsInputs: [.. exprGroup.Select(report => report.Input)],
                    limitSampleCountPerSample: limitSampleCountPerSample,
                    classUsageCountMin: classUsageCountMin,
                    limitClassesCount: limitClassesPerExpression,
                    parseCache: parseCache));

        return expressionsEnvClasses;
    }


    public static IReadOnlyList<PineValueClass> EnvironmentClassesFromExpressionInvocationReports(
        Expression expression,
        IReadOnlyList<StackFrameInput> invocationsInputs,
        int limitSampleCountPerSample,
        int classUsageCountMin,
        int limitClassesCount,
        PineVMParseCache parseCache)
    {
        var environmentClasses =
            GenerateEnvironmentClasses(
                invocationsInputs,
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
            .Select(envClass => (envClass, matchCount: invocationsInputs.Count(input => envClass.SatisfiedByConstraint(input.ToValueClass()))))
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
                ExpressionCompilation.ReduceExpressionAndInlineRecursive(
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
                ExpressionCompilation.ReduceExpressionAndInlineRecursive(
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
                ExpressionCompilation.SubstituteSubexpressionsForEnvironmentConstraint(
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
        IReadOnlyList<StackFrameInput> invocationsInputs,
        int limitIntersectionCountPerValue,
        int classDepthLimit)
    {
        var values =
            invocationsInputs
            .Select(input => input.CreateMinimalValue())
            .ToImmutableArray();

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
}
