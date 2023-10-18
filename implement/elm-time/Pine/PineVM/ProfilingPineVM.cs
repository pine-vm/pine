using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.PineVM;

public record ExpressionUsageProfile(int UsageCount);

public record ProfilingPineVM(
    PineVM PineVM,
    IReadOnlyCollection<Expression> ExpressionEvaluations)
{
    public static ProfilingPineVM BuildProfilingVM(
        PineVM.OverrideDecodeExprDelegate? overrideDecodeExpression = null,
        PineVM.OverrideEvalExprDelegate? overrideEvaluateExpression = null)
    {
        var expressionEvaluations = new ConcurrentQueue<Expression>();

        var profilingPineVM =
            new PineVM(
                overrideDecodeExpression:
                defaultHandler => value =>
                {
                    return
                    (overrideDecodeExpression?.Invoke(defaultHandler) ?? defaultHandler)
                    .Invoke(value)
                    .Map(decodedExpr =>
                    {
                        if (decodedExpr is not Expression.DelegatingExpression)
                        {
                            expressionEvaluations.Enqueue(decodedExpr);
                        }

                        return decodedExpr;
                    });
                },
                overrideEvaluateExpression: overrideEvaluateExpression);

        return new ProfilingPineVM(profilingPineVM, expressionEvaluations);
    }

    public static IReadOnlyDictionary<Expression, ExpressionUsageProfile> UsageProfileDictionaryFromListOfUsages(
        IReadOnlyCollection<Expression> usages)
    {
        var counts = new Dictionary<Expression, int>();

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
