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

        // First pass: identify expressions that appear in all branches of conditionals
        var expressionsCommonToAllBranches = FindExpressionsCommonToAllBranches(rootExpressions, ignoreExpr);

        var seenOnceUnconditional = new HashSet<StaticExpression<FuncId>>();

        var seenOnceConditional = new HashSet<StaticExpression<FuncId>>();

        var collected = new HashSet<StaticExpression<FuncId>>();

        var queue = new Queue<(StaticExpression<FuncId> expr, bool conditional)>(capacity: rootExpressions.Count);

        foreach (var rootExpr in rootExpressions)
        {
            queue.Enqueue((rootExpr, false));
        }

        void EnqueueIfNoRoot(StaticExpression<FuncId> expr, bool conditional)
        {
            if (rootExpressions.Contains(expr))
            {
                return;
            }

            queue.Enqueue((expr, conditional));
        }

        while (queue.Count > 0)
        {
            var current = queue.Dequeue();

            if (ignoreExpr(current.expr))
            {
                continue;
            }

            if (collected.Contains(current.expr))
            {
                continue;
            }

            // Treat expressions common to all branches as unconditional
            var effectivelyConditional = current.conditional && !expressionsCommonToAllBranches.Contains(current.expr);

            if (seenOnceUnconditional.Contains(current.expr) ||
                (seenOnceConditional.Contains(current.expr) && !effectivelyConditional))
            {
                yield return current.expr;

                collected.Add(current.expr);

                continue;
            }

            if (effectivelyConditional)
            {
                seenOnceConditional.Add(current.expr);
            }
            else
            {
                seenOnceUnconditional.Add(current.expr);
            }

            ProcessSubexpressions(
                current.expr,
                EnqueueIfNoRoot,
                current.conditional);
        }
    }

    /// <summary>
    /// Finds expressions that appear in all branches of conditionals within the given root expressions.
    /// </summary>
    private static HashSet<StaticExpression<FuncId>> FindExpressionsCommonToAllBranches<FuncId>(
        IReadOnlyList<StaticExpression<FuncId>> rootExpressions,
        System.Func<StaticExpression<FuncId>, bool> ignoreExpr)
    {
        var result = new HashSet<StaticExpression<FuncId>>();

        foreach (var rootExpr in rootExpressions)
        {
            CollectCommonSubexpressionsRecursive(
                rootExpr,
                ignoreExpr,
                commonExprs: result,
                visited: []);
        }

        return result;
    }

    /// <summary>
    /// Processes all direct subexpressions of the given expression.
    /// </summary>
    private static void ProcessSubexpressions<FuncId>(
        StaticExpression<FuncId> expr,
        System.Action<StaticExpression<FuncId>, bool> processSubexpression,
        bool parentIsConditional)
    {
        if (expr is StaticExpression<FuncId>.Conditional conditional)
        {
            processSubexpression(conditional.Condition, parentIsConditional);
            processSubexpression(conditional.TrueBranch, true);
            processSubexpression(conditional.FalseBranch, true);
            return;
        }

        if (expr is StaticExpression<FuncId>.Literal)
        {
            return;
        }

        if (expr is StaticExpression<FuncId>.Environment)
        {
            return;
        }

        if (expr is StaticExpression<FuncId>.List list)
        {
            foreach (var item in list.Items)
            {
                processSubexpression(item, parentIsConditional);
            }
            return;
        }

        if (expr is StaticExpression<FuncId>.KernelApplication kernelApp)
        {
            processSubexpression(kernelApp.Input, parentIsConditional);
            return;
        }

        if (expr is StaticExpression<FuncId>.FunctionApplication funcApp)
        {
            processSubexpression(funcApp.Arguments, parentIsConditional);
            return;
        }

        if (expr is StaticExpression<FuncId>.CrashingParseAndEval parseAndEval)
        {
            processSubexpression(parseAndEval.Encoded, parentIsConditional);
            processSubexpression(parseAndEval.EnvironmentExpr, parentIsConditional);
            return;
        }

        throw new System.NotImplementedException(
            "CSE collection for expression type " + expr.GetType() + " is not implemented.");
    }

    /// <summary>
    /// Recursively collects subexpressions that are common to all branches of conditionals.
    /// </summary>
    private static void CollectCommonSubexpressionsRecursive<FuncId>(
        StaticExpression<FuncId> expr,
        System.Func<StaticExpression<FuncId>, bool> ignoreExpr,
        HashSet<StaticExpression<FuncId>> commonExprs,
        HashSet<StaticExpression<FuncId>> visited)
    {
        if (!visited.Add(expr) || ignoreExpr(expr))
        {
            return;
        }

        if (expr is StaticExpression<FuncId>.Conditional conditional)
        {
            // Recursively process the condition
            CollectCommonSubexpressionsRecursive(conditional.Condition, ignoreExpr, commonExprs, visited);

            // Collect all subexpressions from both branches
            var trueBranchExprs = new HashSet<StaticExpression<FuncId>>();
            CollectAllSubexpressionsRecursive(conditional.TrueBranch, ignoreExpr, trueBranchExprs, []);

            var falseBranchExprs = new HashSet<StaticExpression<FuncId>>();
            CollectAllSubexpressionsRecursive(conditional.FalseBranch, ignoreExpr, falseBranchExprs, []);

            // Find expressions common to both branches and add them to the result
            var branchCommonExprs = trueBranchExprs.Intersect(falseBranchExprs);

            commonExprs.UnionWith(branchCommonExprs);

            // Continue recursing into both branches to find nested conditionals
            CollectCommonSubexpressionsRecursive(conditional.TrueBranch, ignoreExpr, commonExprs, visited);
            CollectCommonSubexpressionsRecursive(conditional.FalseBranch, ignoreExpr, commonExprs, visited);

            return;
        }

        // For non-conditional expressions, recurse using the unified traversal
        ProcessSubexpressions(
            expr,
            (subExpr, _) => CollectCommonSubexpressionsRecursive(subExpr, ignoreExpr, commonExprs, visited),
            parentIsConditional: false);
    }

    /// <summary>
    /// Recursively collects all subexpressions from the given expression.
    /// </summary>
    private static void CollectAllSubexpressionsRecursive<FuncId>(
        StaticExpression<FuncId> expr,
        System.Func<StaticExpression<FuncId>, bool> ignoreExpr,
        HashSet<StaticExpression<FuncId>> allExprs,
        HashSet<StaticExpression<FuncId>> visited)
    {
        if (!visited.Add(expr) || ignoreExpr(expr))
        {
            return;
        }

        allExprs.Add(expr);

        // Use the unified traversal to process subexpressions
        ProcessSubexpressions(
            expr,
            (subExpr, _) => CollectAllSubexpressionsRecursive(subExpr, ignoreExpr, allExprs, visited),
            parentIsConditional: false);
    }
}
