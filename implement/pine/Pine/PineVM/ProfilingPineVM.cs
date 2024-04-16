using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.PineVM;


public record ExpressionUsageAnalysis(
    Expression Expression,
    EnvConstraintId? EnvId)
{
    public override int GetHashCode()
    {
        return Expression.GetHashCode();
    }

    public virtual bool Equals(ExpressionUsageAnalysis? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        if (!Expression.Equals(other.Expression))
        {
            return false;
        }

        return EnvConstraintId.Equal(EnvId, other.EnvId);
    }
}

public record ExpressionUsageSample(
    Expression.ParseAndEvalExpression ParseAndEvalExpr,
    PineValue Environment,
    System.TimeSpan OrigEvalDuration,
    System.Lazy<Result<string, IReadOnlyList<ExpressionUsageAnalysis>>> Analysis);

public record ExpressionUsageProfile(int UsageCount);

public class ProfilingPineVM
{
    public static readonly ConcurrentQueue<System.TimeSpan> computeExpressionUsageTimes = new();

    public IPineVM PineVM { init; get; }

    private readonly ConcurrentQueue<ExpressionUsageSample> expressionUsages = new();

    public IReadOnlyList<ExpressionUsageSample> DequeueAllSamples()
    {
        return [.. expressionUsages.DequeueAllEnumerable()];
    }

    public ProfilingPineVM(
        PineVM.OverrideParseExprDelegate? overrideParseExpression = null,
        PineVM.OverrideEvalExprDelegate? overrideEvaluateExpression = null)
    {
        ConcurrentDictionary<PineValue, Result<string, Expression>> parseExpressionFromValueCache = new();

        ConcurrentDictionary<Expression, CodeAnalysis.ExprAnalysis> exprAnalysisMutatedCache = new();

        Result<string, Expression> ParseExpressionFromValue(PineValue value) =>
            parseExpressionFromValueCache.GetOrAdd(
                value,
                valueFactory: Pine.PineVM.PineVM.ParseExpressionFromValueDefault);

        PineVM =
            new PineVM(
                overrideParseExpression: overrideParseExpression,
                overrideEvaluateExpression:
                defaultHandler => new PineVM.EvalExprDelegate((expression, environment) =>
                {
                    var origEvalDuration = System.Diagnostics.Stopwatch.StartNew();

                    var evalResult =
                        (overrideEvaluateExpression?.Invoke(defaultHandler) ?? defaultHandler)
                        .Invoke(expression, environment);

                    origEvalDuration.Stop();

                    // if (DynamicPGOShare.ShouldIncludeExpressionInCompilation(expression))
                    if (expression is Expression.ParseAndEvalExpression parseAndEval)
                    {
                        Result<string, IReadOnlyList<ExpressionUsageAnalysis>> runAnalysis()
                        {
                            return
                            defaultHandler(parseAndEval.expression, environment)
                            .AndThen(innerExprValue => ParseExpressionFromValue(innerExprValue))
                            .AndThen(parsedInnerExpr => defaultHandler(parseAndEval.environment, environment)
                            .Map(innerEnvValue =>
                            {
                                var stopwatch = System.Diagnostics.Stopwatch.StartNew();

                                try
                                {
                                    return
                                        AnalyzeExpressionUsage(
                                            parsedInnerExpr,
                                            innerEnvValue,
                                            exprAnalysisMutatedCache);

                                }
                                finally
                                {
                                    stopwatch.Stop();
                                    computeExpressionUsageTimes.Enqueue(stopwatch.Elapsed);
                                }
                            }));
                        }

                        expressionUsages.Enqueue(
                            new ExpressionUsageSample(
                                ParseAndEvalExpr: parseAndEval,
                                Environment: environment,
                                OrigEvalDuration: origEvalDuration.Elapsed,
                                Analysis: new System.Lazy<Result<string, IReadOnlyList<ExpressionUsageAnalysis>>>(
                                    valueFactory: runAnalysis)));
                    }

                    return evalResult;
                }));
    }

    public static IReadOnlyList<ExpressionUsageAnalysis> AnalyzeExpressionUsage(
        Expression expression,
        PineValue environment,
        ConcurrentDictionary<Expression, CodeAnalysis.ExprAnalysis> exprAnalysisMutatedCache)
    {
        var analysisResult =
            CodeAnalysis.AnalyzeExpressionUsageRecursive(
                [],
                expression,
                environment,
                mutatedCache: exprAnalysisMutatedCache);

        var rootConstraintId =
            analysisResult.RootEnvClass is not ExpressionEnvClass.ConstrainedEnv constrained
            ?
            null
            :
            EnvConstraintId.Create(
                constrained,
                environment,
                skipUnavailableItems: true);

        var otherExprAnalysis =
            analysisResult.UsagesCompleteForRecursion
            .Select(exprInRecursion => new ExpressionUsageAnalysis(exprInRecursion.expr, exprInRecursion.expandedConstraint))
            .ToImmutableList();

        var allExprReported =
            (IReadOnlyList<ExpressionUsageAnalysis>)
            [new ExpressionUsageAnalysis(expression, rootConstraintId),
            ..otherExprAnalysis];

        return allExprReported;
    }

    public static IReadOnlyDictionary<ExpressionUsageAnalysis, ExpressionUsageProfile> UsageProfileDictionaryFromListOfUsages(
        IReadOnlyCollection<ExpressionUsageAnalysis> usages)
    {
        var counts = new Dictionary<ExpressionUsageAnalysis, int>();

        foreach (var usage in usages)
            counts[usage] = counts.GetValueOrDefault(usage, 0) + 1;

        return
            counts
            .ToDictionary(
                keySelector: p => p.Key,
                elementSelector: p => new ExpressionUsageProfile(UsageCount: p.Value));
    }

    static public IReadOnlyDictionary<T, ExpressionUsageProfile> AggregateExpressionUsageProfiles<T>(
        IReadOnlyCollection<IReadOnlyDictionary<T, ExpressionUsageProfile>> dictionaries)
        where T : notnull
        =>
        dictionaries
        .SelectMany(dict => dict.Keys)
        .Distinct()
        .ToImmutableDictionary(
            keySelector:
            expr => expr,
            elementSelector:
            expr =>
            {
                var profiles =
                    dictionaries.SelectMany(dict =>
                    {
                        if (!dict.TryGetValue(expr, out var result))
                            return [];

                        return ImmutableList.Create(result);
                    })
                    .ToImmutableArray();

                return
                AggregateExpressionUsageProfiles(profiles);
            });

    static public ExpressionUsageProfile AggregateExpressionUsageProfiles(
        IReadOnlyList<ExpressionUsageProfile> profiles)
    {
        return new ExpressionUsageProfile(
            UsageCount: profiles.Sum(p => p.UsageCount));
    }
}
