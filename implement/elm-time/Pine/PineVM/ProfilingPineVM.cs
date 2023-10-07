using System;
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
    public static ProfilingPineVM BuildProfilingVM()
    {
        var expressionEvaluations = new ConcurrentQueue<Expression>();

        var profilingPineVM =
            new PineVM(
                overrideDecodeExpression:
                defaultHandler => value =>
                {
                    return
                    defaultHandler(value)
                    .Map(decodedExpr =>
                    {
                        if (decodedExpr is not Expression.DelegatingExpression)
                        {
                            expressionEvaluations.Enqueue(decodedExpr);
                        }

                        return decodedExpr;
                    });
                },
                overrideEvaluateExpression: null);

        return new ProfilingPineVM(profilingPineVM, expressionEvaluations);
    }

    public static IReadOnlyDictionary<Expression, ExpressionUsageProfile> UsageProfileDictionaryFromListOfUsages(
        IReadOnlyCollection<Expression> usages) =>
        usages
        .Distinct()
        .ToImmutableDictionary(
            keySelector: expression => expression,
            elementSelector: expression =>
            {
                var usageCount = usages.Count(e => e == expression);

                return new ExpressionUsageProfile(usageCount);
            });

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
