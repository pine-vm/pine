using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Runtime.InteropServices;

namespace Pine.Core.Runtime.InternalExpression;

public abstract partial record Expression
{
    private readonly Statistics _statistics;

    /// <summary>Canonical-expanded size for retained nodes; scoped nodes count their syntax before lowering.</summary>
    public long SubexpressionCount => _statistics.Subexpressions;

    /// <summary>Whether evaluation reads its environment.</summary>
    public bool ReferencesEnvironment => _statistics.Environment;

    /// <summary>Canonical Eval count, including one per symbolic call before lowering.</summary>
    public long EvalCount => _statistics.Evals;

    /// <summary>Canonical conditional count.</summary>
    public long ConditionCount => _statistics.Conditions;

    /// <summary>Canonical builtin count, including fused builtins.</summary>
    public long BuiltinCount => _statistics.Builtins;

    /// <summary>Canonical-expanded maximum depth for retained nodes, before lowering scoped nodes.</summary>
    public int MaxDepth => _statistics.Depth;

    private protected readonly record struct Statistics(
        long Subexpressions = 0, long Evals = 0, long Conditions = 0,
        long Builtins = 0, int Depth = 0, bool Environment = false, bool FunctionBindings = false)
    {
        public static Statistics Unary(Expression child, long builtins = 0, long subexpressions = 1, int depth = 1)
        {
            ArgumentNullException.ThrowIfNull(child);

            return
                child._statistics with
                {
                    Subexpressions = child.SubexpressionCount + subexpressions,
                    Builtins = child.BuiltinCount + builtins,
                    Depth = child.MaxDepth + depth
                };
        }

        public static Statistics Binary(
            Expression first,
            Expression second,
            long subexpressions = 2,
            long evals = 0,
            long builtins = 0,
            int depth = 1)
        {
            ArgumentNullException.ThrowIfNull(first);
            ArgumentNullException.ThrowIfNull(second);

            return
                new(
                    Subexpressions: first.SubexpressionCount + second.SubexpressionCount + subexpressions,
                    Environment: first.ReferencesEnvironment || second.ReferencesEnvironment,
                    Evals: first.EvalCount + second.EvalCount + evals,
                    Conditions: first.ConditionCount + second.ConditionCount,
                    Builtins: first.BuiltinCount + second.BuiltinCount + builtins,
                    Depth: Math.Max(first.MaxDepth, second.MaxDepth) + depth,
                    FunctionBindings: first.ContainsFunctionBindings || second.ContainsFunctionBindings);
        }

        public static Statistics Conditional(Expression condition, Expression whenFalse, Expression whenTrue)
        {
            ArgumentNullException.ThrowIfNull(condition);
            var pair = Binary(whenFalse, whenTrue);

            return
                new(
                    Subexpressions: condition.SubexpressionCount + pair.Subexpressions + 1,
                    Environment: condition.ReferencesEnvironment || pair.Environment,
                    Evals: condition.EvalCount + pair.Evals,
                    Conditions: condition.ConditionCount + pair.Conditions + 1,
                    Builtins: condition.BuiltinCount + pair.Builtins,
                    Depth: Math.Max(condition.MaxDepth + 1, pair.Depth),
                    FunctionBindings: condition.ContainsFunctionBindings || pair.FunctionBindings);
        }

        public static Statistics List(ImmutableArray<Expression> items)
        {
            var result = new Statistics(Subexpressions: items.Length, Depth: items.IsEmpty ? 0 : 1);

            foreach (var item in items)
            {
                result = result.IncludeChild(item);
            }

            return result;
        }

        public static Statistics LengthEqual(Expression sequence, Expression other) =>
            Binary(sequence, other, subexpressions: 4, builtins: 2, depth: 2) with
            { Depth = Math.Max(sequence.MaxDepth + 1, other.MaxDepth) + 2 };

        public Statistics IncludeChild(Expression child)
        {
            ArgumentNullException.ThrowIfNull(child);

            return
                new(
                    Subexpressions: Subexpressions + child.SubexpressionCount,
                    Environment: Environment || child.ReferencesEnvironment,
                    Evals: Evals + child.EvalCount,
                    Conditions: Conditions + child.ConditionCount,
                    Builtins: Builtins + child.BuiltinCount,
                    Depth: Math.Max(Depth, child.MaxDepth + 1),
                    FunctionBindings: FunctionBindings || child.ContainsFunctionBindings);
        }
    }

    /// <summary>Shared environment leaf.</summary>
    public static Expression EnvironmentInstance => Environment.Instance;

    /// <summary>Shared empty expression list.</summary>
    public static List EmptyList { get; } = new([]);

    /// <summary>Retains a literal without a canonical expression.</summary>
    public static Literal LiteralInstance(PineValue value) => new(value);

    /// <summary>Creates normalized retained builtin syntax without changing its canonical expansion.</summary>
    public static Expression BuiltinInst(BuiltinFunction function, Expression input) =>
        input is List { Items: [var left, var right] }
        ?
        BuiltinBinaryInst(function, left, right)
        :
        function is BuiltinFunction.Head &&
        TryGetBuiltinInput(input, BuiltinFunction.Skip) is { IsList: true, Length: 2 } skip &&
        skip[0] is Literal { Value: PineValue.BlobValue count } &&
        ExpressionBinary.TryCanonicalInt32(count.Bytes.Span, out var integer)
        ?
        new HeadSkip(integer, skip[1])
        :
        new Builtin(function, input);

    /// <summary>Creates a static pair call, giving more specific fusions priority.</summary>
    public static Expression BuiltinBinaryInst(BuiltinFunction function, Expression left, Expression right)
    {
        if (function is BuiltinFunction.Equal)
        {
            if (TryGetBuiltinInput(left, BuiltinFunction.Length) is { } leftLength)
                return new Length_Equal_Binary(leftLength.ToExpression(), right);

            if (TryGetBuiltinInput(right, BuiltinFunction.Length) is { } rightLength)
                return new Length_Equal_Binary(rightLength.ToExpression(), left, false);
        }

        return new BuiltinBinary(function, left, right);
    }

    internal static (BuiltinFunction Function, ExpressionInput Input)? TryGetBuiltinApplication(Expression? expression) =>
        expression switch
        {
            Builtin builtin => (builtin.Function, (ExpressionInput)builtin.Input),
            BuiltinBinary binary => (binary.Function, ExpressionInput.FromBinary(binary)),

            Literal or List or Eval or Conditional or Switch or Environment or Label or Length_Equal_Binary or HeadSkip or
            Call or LetFunctions =>
            null,

            null => null,

            _ =>
            throw new NotImplementedException(
                nameof(TryGetBuiltinApplication) + " does not handle expression variant: " + expression.GetType().Name)
        };

    internal static ExpressionInput? TryGetBuiltinInput(Expression? expression, BuiltinFunction function) =>
        TryGetBuiltinApplication(expression) is { } builtin && builtin.Function == function
        ?
        builtin.Input
        :
        (ExpressionInput?)null;

    internal static Expression BuiltinListInst(BuiltinFunction function, IReadOnlyList<Expression> operands) =>
        operands.Count is 2
        ?
        BuiltinBinaryInst(function, operands[0], operands[1])
        :
        BuiltinInst(function, ListInst(operands));

    /// <summary>Copies caller-owned storage, but does not copy an immutable array.</summary>
    public static List ListInst(IReadOnlyList<Expression> items) =>
        items.Count == 0
        ?
        EmptyList
        :
        items is ImmutableArray<Expression> immutable
        ?
        new(immutable)
        :
        new(ImmutableCollectionsMarshal.AsImmutableArray(Copy(items)));

    private static Expression[] Copy(IReadOnlyList<Expression> items)
    {
        var copy = new Expression[items.Count];

        for (var index = 0; index < copy.Length; index++)
            copy[index] = items[index];

        return copy;
    }

    /// <summary>Creates a conditional preserving branch order.</summary>
    public static Conditional ConditionalInst(Expression condition, Expression falseBranch, Expression trueBranch) =>
        new(condition, falseBranch, trueBranch);

    /// <summary>Enumerates the retained tree without canonical expansion.</summary>
    public static IEnumerable<Expression> EnumerateSelfAndDescendants(
        Expression expression, Func<Expression, bool>? skipDescendants = null, bool skipConditionalBranches = false)
    {
        var pending = new Stack<Expression>();
        pending.Push(expression);

        while (pending.TryPop(out var node))
        {
            yield return node;

            if (skipDescendants?.Invoke(node) is true)
                continue;

            PushChildren(node, pending, skipConditionalBranches);
        }
    }

    internal static void PushChildren(Expression node, Stack<Expression> pending, bool skipConditionalBranches = false)
    {
        switch (node)
        {
            case Literal:
            case Environment:
                break;

            case List list:
                for (var index = 0; index < list.Items.Length; index++)
                    pending.Push(list.Items[index]);

                break;

            case Eval eval:
                pending.Push(eval.Encoded);
                pending.Push(eval.Environment);
                break;

            case Builtin builtin:
                pending.Push(builtin.Input);
                break;

            case Conditional conditional:
                pending.Push(conditional.Condition);

                if (!skipConditionalBranches)
                {
                    pending.Push(conditional.FalseBranch);
                    pending.Push(conditional.TrueBranch);
                }

                break;

            case Switch dispatch:
                pending.Push(dispatch.Selector);

                if (!skipConditionalBranches)
                {
                    pending.Push(dispatch.Default);

                    foreach (var arm in dispatch.Arms)
                        pending.Push(arm.Branch);
                }

                break;

            case Label label:
                pending.Push(label.Tagged);
                break;

            case BuiltinBinary equal:
                pending.Push(equal.Left);
                pending.Push(equal.Right);
                break;

            case Length_Equal_Binary length:
                pending.Push(length.Sequence);
                pending.Push(length.Other);
                break;

            case HeadSkip head:
                pending.Push(head.Input);
                break;

            case Call call:
                pending.Push(call.Environment);
                break;

            case LetFunctions block:
                pending.Push(block.Entry);

                if (!skipConditionalBranches)
                {
                    foreach (var function in block.Functions)
                        pending.Push(function.Body);
                }

                break;

            default:
                throw new NotImplementedException(
                    nameof(PushChildren) + " does not handle expression variant: " + node.GetType().Name);
        }
    }
}
