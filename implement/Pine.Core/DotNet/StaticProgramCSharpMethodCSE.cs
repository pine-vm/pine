using Pine.Core.CodeAnalysis;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.DotNet;

/// <summary>
/// Provides common subexpression elimination (CSE) functionality for StaticProgramCSharpClass.
/// This class contains methods for collecting and analyzing subexpressions to optimize
/// code generation by identifying expressions that should be separated into local variables.
/// </summary>
public static class StaticProgramCSharpMethodCSE
{
    /// <summary>
    /// Collects subexpressions that should be separated into local variable declarations
    /// for common subexpression elimination (CSE).
    /// </summary>
    /// <typeparam name="FuncId">The type used to identify functions in the expression tree.</typeparam>
    /// <param name="expression">The root expression to analyze.</param>
    /// <param name="ignoreExpr">A predicate to determine which expressions should be ignored during collection.</param>
    /// <returns>An enumerable of subexpressions that should be separated into local declarations.</returns>
    public static IEnumerable<StaticExpression<FuncId>> CollectSubexpressionsToSeparate<FuncId>(
        StaticExpression<FuncId> expression,
        System.Func<StaticExpression<FuncId>, bool> ignoreExpr)
    {
        IReadOnlyList<StaticExpression<FuncId>> currentRoots = [expression];

        while (true)
        {
            var collectedThisRound =
                CollectSubexpressionsToSeparateStep(currentRoots, ignoreExpr)
                .ToImmutableArray();

            if (collectedThisRound.Length is 0)
            {
                return currentRoots.Except([expression]);
            }

            currentRoots = [.. currentRoots, .. collectedThisRound];
        }
    }

    /// <summary>
    /// Performs one step of subexpression collection for CSE.
    /// Identifies expressions that appear multiple times (either unconditionally or in all branches)
    /// and should be extracted into local variables.
    /// </summary>
    /// <typeparam name="FuncId">The type used to identify functions in the expression tree.</typeparam>
    /// <param name="rootExpressions">The root expressions to analyze.</param>
    /// <param name="ignoreExpr">A predicate to determine which expressions should be ignored during collection.</param>
    /// <returns>An enumerable of subexpressions identified in this step.</returns>
    public static IEnumerable<StaticExpression<FuncId>> CollectSubexpressionsToSeparateStep<FuncId>(
        IReadOnlyList<StaticExpression<FuncId>> rootExpressions,
        System.Func<StaticExpression<FuncId>, bool> ignoreExpr)
    {
        /*
         * Primary reason to separate a subexpression into a declaration is CSE (prevent repeated evaluation).
         * 
         * For any subexpression that occurs at least once unconditional and a second time, emit to CSE.
         * 
         * Additionally, expressions that appear in ALL branches of a conditional should be treated as
         * effectively unconditional relative to that conditional.
         * */

        var stats = CollectForCSE(rootExpressions, ignoreExpr);

        foreach (var expr in stats.Unconditional)
        {
            if (rootExpressions.Contains(expr))
            {
                continue;
            }

            if (stats.SeenAtLeastTwice.Contains(expr))
            {
                yield return expr;
            }
        }
    }

    private record ExpressionStatsForCSE<FuncId>(
        ImmutableHashSet<StaticExpression<FuncId>> Unconditional,
        ImmutableHashSet<StaticExpression<FuncId>> Conditional,
        ImmutableHashSet<StaticExpression<FuncId>> SeenAtLeastTwice);

    private static ExpressionStatsForCSE<FuncId> CollectForCSE<FuncId>(
        IReadOnlyList<StaticExpression<FuncId>> rootExpressions,
        System.Func<StaticExpression<FuncId>, bool> ignoreExpr)
    {
        var unconditional = new HashSet<StaticExpression<FuncId>>();
        var seenConditional = new HashSet<StaticExpression<FuncId>>();
        var seenAtLeastTwice = new HashSet<StaticExpression<FuncId>>();

        var queue = new Queue<StaticExpression<FuncId>>(rootExpressions);

        void EnqueueIfNoRoot(StaticExpression<FuncId> expr)
        {
            if (rootExpressions.Contains(expr))
            {
                return;
            }

            queue.Enqueue(expr);
        }

        while (queue.Count > 0)
        {
            var current = queue.Dequeue();

            if (ignoreExpr(current))
            {
                continue;
            }

            if (unconditional.Contains(current) ||
                seenConditional.Contains(current))
            {
                seenAtLeastTwice.Add(current);
            }

            unconditional.Add(current);

            if (seenAtLeastTwice.Contains(current))
            {
                continue;
            }

            if (current is StaticExpression<FuncId>.Conditional conditional)
            {
                /* For subexpressions that appear in both branches of a conditional,
                 * we treat them as unconditional relative to the current roots.
                 * */

                EnqueueIfNoRoot(conditional.Condition);

                bool IgnoreExprInBranches(StaticExpression<FuncId> expr)
                {
                    return ignoreExpr(expr) || rootExpressions.Contains(expr);
                }

                var statsInTrueBranch =
                    CollectForCSE(
                        [conditional.TrueBranch],
                        IgnoreExprInBranches);

                var statsInFalseBranch =
                    CollectForCSE(
                        [conditional.FalseBranch],
                        IgnoreExprInBranches);

                var unconditionalFromBothBranches =
                    statsInTrueBranch.Unconditional
                    .Intersect(statsInFalseBranch.Unconditional);

                unconditional.UnionWith(unconditionalFromBothBranches);
                seenAtLeastTwice.UnionWith(unconditionalFromBothBranches);

                var unconditionalFromOneBranch =
                    statsInTrueBranch.Unconditional
                    .Union(statsInFalseBranch.Unconditional)
                    .Except(unconditionalFromBothBranches);

                var conditionalOnceFromBranches =
                    statsInTrueBranch.Conditional
                    .Union(statsInFalseBranch.Conditional)
                    .Union(unconditionalFromOneBranch);

                seenConditional.UnionWith(conditionalOnceFromBranches);

                var seenAtLeastTwiceFromBranches =
                    statsInTrueBranch.SeenAtLeastTwice
                    .Union(statsInFalseBranch.SeenAtLeastTwice)
                    .Union(
                        statsInTrueBranch.Conditional
                        .Intersect(statsInFalseBranch.Conditional));

                seenAtLeastTwice.UnionWith(seenAtLeastTwiceFromBranches);

                continue;
            }


            if (current is StaticExpression<FuncId>.Literal)
            {
                continue;
            }

            if (current is StaticExpression<FuncId>.Environment)
            {
                continue;
            }

            if (current is StaticExpression<FuncId>.List list)
            {
                foreach (var item in list.Items)
                {
                    EnqueueIfNoRoot(item);
                }

                continue;
            }

            if (current is StaticExpression<FuncId>.KernelApplication kernelApp)
            {
                EnqueueIfNoRoot(kernelApp.Input);
                continue;
            }

            if (current is StaticExpression<FuncId>.FunctionApplication funcApp)
            {
                EnqueueIfNoRoot(funcApp.Arguments);
                continue;
            }

            if (current is StaticExpression<FuncId>.CrashingParseAndEval parseAndEval)
            {
                EnqueueIfNoRoot(parseAndEval.Encoded);
                EnqueueIfNoRoot(parseAndEval.EnvironmentExpr);
                continue;
            }

            throw new System.NotImplementedException(
                "CSE collection for expression type " + current.GetType() + " is not implemented.");
        }

        return new ExpressionStatsForCSE<FuncId>(
            Unconditional: [.. unconditional],
            Conditional: [.. seenConditional],
            SeenAtLeastTwice: [.. seenAtLeastTwice]);
    }
}
