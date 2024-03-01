using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.PineVM;


public record ExpressionUsage(
    Expression Expression,
    EnvConstraintId? EnvId)
{
    public override int GetHashCode()
    {
        return Expression.GetHashCode();
    }

    public virtual bool Equals(ExpressionUsage? other)
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

public record ExpressionUsageProfile(
    int UsageCount);

public record ProfilingPineVM(
    PineVM PineVM,
    IReadOnlyCollection<ExpressionUsage> ExpressionEvaluations)
{
    public static ProfilingPineVM BuildProfilingVM(
        PineVM.OverrideParseExprDelegate? overrideParseExpression = null,
        PineVM.OverrideEvalExprDelegate? overrideEvaluateExpression = null)
    {
        var expressionEvaluations = new ConcurrentQueue<ExpressionUsage>();

        ConcurrentDictionary<PineValue, Result<string, Expression>> parseExpressionFromValueCache = new();

        ConcurrentDictionary<Expression, CodeAnalysis.ExprAnalysis> exprAnalysisMutatedCache = new();

        Result<string, Expression> ParseExpressionFromValue(PineValue value) =>
            parseExpressionFromValueCache.GetOrAdd(
                value,
                valueFactory: PineVM.ParseExpressionFromValueDefault);

        var profilingPineVM =
            new PineVM(
                overrideParseExpression: overrideParseExpression,
                overrideEvaluateExpression:
                defaultHandler => new PineVM.EvalExprDelegate((expression, environment) =>
                {
                    var evalResult =
                        (overrideEvaluateExpression?.Invoke(defaultHandler) ?? defaultHandler)
                        .Invoke(expression, environment);

                    // if (DynamicPGOShare.ShouldIncludeExpressionInCompilation(expression))
                    if (expression is Expression.ParseAndEvalExpression parseAndEval)
                    {
                        defaultHandler(parseAndEval.expression, environment)
                        .AndThen(innerExprValue => ParseExpressionFromValue(innerExprValue))
                        .AndThen(parsedInnerExpr => defaultHandler(parseAndEval.environment, environment)
                        .Map(innerEnvValue =>
                        {
                            expressionEvaluations.Enqueue(
                                ComputeExpressionUsageRecord(
                                    parsedInnerExpr,
                                    innerEnvValue,
                                    exprAnalysisMutatedCache));

                            return 0;
                        }));
                    }

                    return evalResult;
                }));

        return new ProfilingPineVM(profilingPineVM, expressionEvaluations);
    }

    public static ExpressionUsage ComputeExpressionUsageRecord(
        Expression expression,
        PineValue environment,
        ConcurrentDictionary<Expression, CodeAnalysis.ExprAnalysis> exprAnalysisMutatedCache)
    {
        var envClass =
            CodeAnalysis.ComputeExpressionUsageRecordRecursive(
                [],
                expression,
                environment,
                mutatedCache: exprAnalysisMutatedCache);

        return new ExpressionUsage(
            expression,
            envClass is ExpressionEnvClass.ConstrainedEnv constrained && constrained.ParsedEnvItems.Count > 0
            ?
            EnvConstraintId.Create(constrained, environment)
            :
            null);
    }

    public static IReadOnlyDictionary<ExpressionUsage, ExpressionUsageProfile> UsageProfileDictionaryFromListOfUsages(
        IReadOnlyCollection<ExpressionUsage> usages)
    {
        var counts = new Dictionary<ExpressionUsage, int>();

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
