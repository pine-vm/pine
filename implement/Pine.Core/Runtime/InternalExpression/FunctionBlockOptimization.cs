using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Runtime.InternalExpression;

public static class FunctionBlockOptimization
{
    public sealed record Policy(
        int MaximumAnalysisSteps = 4096,
        int MaximumExpressionDepth = 128,
        bool PruneReturnPrefixes = true);

    /// <summary>
    /// Witness for one consistent representation change across a connected set of
    /// forwarding functions. Each concrete return was a list with a literal first item.
    /// </summary>
    public sealed record ReturnPrefixProof(ImmutableArray<int> Functions, int Producers, int Projections);

    public sealed record Optimization(
        Expression.LetFunctions Block, ImmutableArray<ReturnPrefixProof> Proofs, bool BudgetExhausted);

    private sealed class Summary
    {
        public HashSet<int> Forwarding { get; } = [];

        public bool Blocked { get; set; }

        public int Producers { get; set; }

        public int Projections { get; set; }
    }

    /// <param name="block">An expression with same-block calls.</param>
    /// <param name="policy">Deterministic budget. On exhaustion no partial proof is applied.</param>
    /// <param name="publicFunctions">Functions whose original result is observable by callers outside this expression.</param>
    public static Optimization Optimize(
        Expression.LetFunctions block, Policy? policy = null, IEnumerable<int>? publicFunctions = null)
    {
        policy ??= new();
        var budget = new Budget(policy.MaximumAnalysisSteps, policy.MaximumExpressionDepth);

        if (!budget.Spend(0, block.Functions.Length))
            return new(block, [], true);

        var summaries = block.Functions.ToDictionary(function => function.Id, _ => new Summary());

        if (publicFunctions is not null)
        {
            foreach (var id in publicFunctions)
                summaries[id].Blocked = true;
        }

        if (!policy.PruneReturnPrefixes)
        {
            foreach (var summary in summaries.Values)
                summary.Blocked = true;
        }

        var pendingChildren = new Stack<Expression>();

        foreach (var function in block.Functions)
        {
            if (!Inspect(function.Body, function.Id, 0))
                return new(block, [], true);
        }

        if (!Inspect(block.Entry, null, 0))
            return new(block, [], true);

        var proven = new HashSet<int>();
        var visited = new HashSet<int>();
        var proofs = ImmutableArray.CreateBuilder<ReturnPrefixProof>();

        // Calls in result position require caller and callee to use the same layout.
        // Solve each connected component, including cycles, before applying any rewrite.
        foreach (var function in block.Functions)
        {
            if (!visited.Add(function.Id))
                continue;

            var pending = new Stack<int>();
            pending.Push(function.Id);
            var component = new List<int>();
            var producers = 0;
            var projections = 0;
            var blocked = false;

            while (pending.TryPop(out var id))
            {
                var summary = summaries[id];

                if (!budget.Spend(0, summary.Forwarding.Count + 1))
                    return new(block, [], true);

                component.Add(id);
                producers += summary.Producers;
                projections += summary.Projections;
                blocked |= summary.Blocked;

                foreach (var other in summary.Forwarding)
                {
                    if (visited.Add(other))
                        pending.Push(other);
                }
            }

            if (!blocked && producers > 0 && projections > 0)
            {
                proven.UnionWith(component);
                proofs.Add(new([.. component.Order()], producers, projections));
            }
        }

        var functions =
            block.Functions
            .Select(
                function => function with { Body = Rewrite(function.Body, proven.Contains(function.Id)) })
            .ToImmutableArray();

        var entry = Rewrite(block.Entry, false);

        return
            new(
                ReferenceEquals(block.Entry, entry) &&
                functions.Zip(block.Functions).All(pair => ReferenceEquals(pair.First.Body, pair.Second.Body))
                ?
                block
                :
                new Expression.LetFunctions(functions, entry),
                proofs.ToImmutable(),
                false);

        bool Inspect(Expression node, int? resultOf, int depth, bool projected = false)
        {
            if (!budget.Spend(depth))
                return false;

            switch (node)
            {
                case Expression.Call call:
                    if (resultOf is { } owner)
                    {
                        summaries[owner].Forwarding.Add(call.FunctionId);
                        summaries[call.FunctionId].Forwarding.Add(owner);
                    }
                    else if (projected)
                        summaries[call.FunctionId].Projections++;

                    else
                        summaries[call.FunctionId].Blocked = true;

                    return Inspect(call.Environment, null, depth + 1);

                case Expression.LetFunctions:

                    // The nested block has no references to this block, but its result is opaque.
                    if (resultOf is { } nestedOwner)
                        summaries[nestedOwner].Blocked = true;

                    return true;

                case Expression.Literal:
                case Expression.Environment:
                case Expression.List:
                case Expression.Eval:
                case Expression.Builtin:
                case Expression.BuiltinBinary:
                case Expression.Conditional:
                case Expression.Switch:
                case Expression.Label:
                case Expression.Length_Equal_Binary:
                case Expression.HeadSkip:
                    if (resultOf is { } function)
                    {
                        if (HasLiteralPrefix(node))
                            summaries[function].Producers++;

                        else if (node is not (Expression.Conditional or Expression.Switch or Expression.Label))
                            summaries[function].Blocked = true;
                    }

                    var remaining = pendingChildren.Count;
                    Expression.PushChildren(node, pendingChildren);

                    while (pendingChildren.Count > remaining)
                    {
                        var child = pendingChildren.Pop();
                        var isResult = IsResultChild(node, pendingChildren.Count - remaining);
                        var isProjection = node is Expression.HeadSkip { Count: > 0 } && child is Expression.Call;

                        if (!Inspect(child, isResult ? resultOf : null, depth + 1, projected: isProjection))
                            return false;
                    }

                    return true;

                default:
                    throw new NotImplementedException(
                        nameof(Inspect) + " does not handle expression variant: " + node.GetType().Name);
            }
        }

        Expression Rewrite(Expression node, bool trim)
        {
            switch (node)
            {
                case Expression.LetFunctions:
                    return node;

                case Expression.Call:
                case Expression.Literal:
                case Expression.Environment:
                case Expression.List:
                case Expression.Eval:
                case Expression.Builtin:
                case Expression.BuiltinBinary:
                case Expression.Conditional:
                case Expression.Switch:
                case Expression.Label:
                case Expression.Length_Equal_Binary:
                case Expression.HeadSkip:
                    var childIndex = 0;

                    var rewritten =
                        ExpressionFusion.Rebuild(
                            node,
                            child => Rewrite(child, trim && IsResultChild(node, childIndex++)));

                    if (node.ContainsFunctionBindings && rewritten is Expression.Switch dispatch &&
                        ForwardSwitch(dispatch) is { } selected)
                    {
                        return selected;
                    }

                    if (trim && HasLiteralPrefix(node))
                    {
                        return
                            rewritten is Expression.Literal { Value: PineValue.ListValue list }
                            ?
                            new Expression.Literal(PineValue.List(list.Items[1..]))
                            :
                            new Expression.List(((Expression.List)rewritten).Items[1..]);
                    }

                    if (FunctionBlockLowering.Projection(node) is { Index: >= 0 } &&
                        FunctionBlockLowering.Projection(rewritten) is { Index: >= 0 } projection)
                    {
                        var (index, source) = projection;

                        if (index > 0 && source is Expression.Call target && proven.Contains(target.FunctionId))
                        {
                            return FunctionBlockLowering.At(index - 1, source);
                        }

                        // Only erase siblings whose evaluation cannot fail or diverge.
                        if (source is Expression.List { Items: var items } && index < items.Length &&
                            items.Where((_, itemIndex) => itemIndex != index)
                            .All(item => item is Expression.Literal or Expression.Environment))
                        {
                            return items[index];
                        }
                    }

                    return rewritten;

                default:
                    throw new NotImplementedException(
                        nameof(Rewrite) + " does not handle expression variant: " + node.GetType().Name);
            }
        }
    }

    private static Expression? ForwardSwitch(Expression.Switch dispatch)
    {
        int BranchIndex(PineValue key)
        {
            var index = 0;

            foreach (var arm in dispatch.Arms)
            {
                if (arm.Key == key)
                {
                    return index;
                }

                index++;
            }

            return -1;
        }

        if (dispatch.Selector is Expression.Literal literal)
        {
            return dispatch.SelectBranch(literal.Value);
        }

        if (dispatch.Selector is not Expression.Conditional
            {
                FalseBranch: Expression.Literal whenFalse,
                TrueBranch: Expression.Literal whenTrue
            } selector)
        {
            return null;
        }

        var falseIndex = BranchIndex(whenFalse.Value);
        var trueIndex = BranchIndex(whenTrue.Value);

        // Even with a single reachable branch, the predicate may fail or diverge.
        return
            falseIndex == trueIndex
            ?
            null
            :
            new Expression.Conditional(selector.Condition, BranchAt(falseIndex), BranchAt(trueIndex));

        Expression BranchAt(int index) => index < 0 ? dispatch.Default : dispatch.Arms[index].Branch;
    }

    private static bool HasLiteralPrefix(Expression node) =>
        node is Expression.Literal { Value: PineValue.ListValue { Items.Length: > 0 } } or
            Expression.List { Items: [Expression.Literal, ..] };

    private static bool IsResultChild(Expression shape, int index) =>
        shape switch
        {
            Expression.Conditional or Expression.Switch => index > 0,
            Expression.Label => index == 0,

            Expression.Literal or Expression.Environment or Expression.List or Expression.Eval or Expression.Builtin or
            Expression.BuiltinBinary or Expression.Length_Equal_Binary or Expression.HeadSkip or
            Expression.Call or Expression.LetFunctions =>
            false,

            _ =>
            throw new NotImplementedException(
                nameof(IsResultChild) + " does not handle expression variant: " + shape.GetType().Name)
        };

    // Consume the Boolean result at its producer instead of materializing it and branching at
    // a join. Operates on the retained fragments shared by the function-block model.
    internal static Expression ForwardBranchPredicates(Expression expression)
    {
        if (expression.ConditionCount < 2 || expression.MaxDepth > 128 || expression.SubexpressionCount > 4096)
            return expression;

        return Rewrite(expression);

        static Expression Rewrite(Expression node)
        {
            if (node.ConditionCount < 2)
                return node;

            var rebuilt = ExpressionFusion.Rebuild(node, Rewrite);

            while (rebuilt is Expression.Conditional outer)
            {
                var condition = outer.Condition;
                var compared = PineKernelValues.TrueValue;

                if (condition is Expression.BuiltinBinary { Function: BuiltinFunction.Equal } equal)
                {
                    if (equal.Left is Expression.Literal left && equal.Right is Expression.Conditional)
                    {
                        condition = equal.Right;
                        compared = left.Value;
                    }
                    else if (equal.Right is Expression.Literal right && equal.Left is Expression.Conditional)
                    {
                        condition = equal.Left;
                        compared = right.Value;
                    }
                }

                if (condition is not Expression.Conditional
                    {
                        TrueBranch: Expression.Literal yes,
                        FalseBranch: Expression.Literal no
                    } inner)
                {
                    break;
                }

                var whenTrue = yes.Value == compared;
                var whenFalse = no.Value == compared;

                if (whenTrue == whenFalse)
                    break;

                rebuilt =
                    new Expression.Conditional(
                        inner.Condition,
                        whenFalse ? outer.TrueBranch : outer.FalseBranch,
                        whenTrue ? outer.TrueBranch : outer.FalseBranch);
            }

            return rebuilt;
        }
    }

    internal sealed class Budget(int remaining, int maximumDepth)
    {
        public bool Spend(int depth, int count = 1)
        {
            if (depth > maximumDepth || count > remaining)
                return false;

            remaining -= count;
            return true;
        }
    }
}
