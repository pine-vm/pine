using Pine.Core.PineLanguage;
using System;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Runtime.InternalExpression;

public abstract partial record Expression
{
    /// <summary>A chain of literal equalities against the same selector.</summary>
    public sealed record Switch : Expression
    {
        /// <summary>A literal test with its original operand placement and branch.</summary>
        public readonly record struct Arm(PineValue Key, bool LiteralOnLeft, Expression Branch);

        /// <summary>The expression compared against the arm keys.</summary>
        public Expression Selector { get; }

        /// <summary>The tests in canonical order, including any shadowed by an earlier key.</summary>
        public ImmutableList<Arm> Arms { get; }

        /// <summary>The branch taken when no key matches.</summary>
        public Expression Default { get; }

        internal Switch(
            Expression selector,
            ImmutableList<Arm> arms,
            Expression defaultBranch)
            : this(selector, arms, defaultBranch, Measure(selector, arms, defaultBranch))
        {
        }

        private Switch(
            Expression selector,
            ImmutableList<Arm> arms,
            Expression defaultBranch,
            (int Hash, Statistics Stats) measures)
            : base(measures.Hash, measures.Stats)
        {
            Selector = selector;
            Arms = arms;
            Default = defaultBranch;
        }

        private static (int Hash, Statistics Stats) Measure(
            Expression selector,
            ImmutableList<Arm> arms,
            Expression defaultBranch)
        {
            var measures = (HashCode.Combine(typeof(Switch), selector, defaultBranch), defaultBranch._statistics);

            foreach (var arm in Enumerable.Reverse(arms))
                measures = PrependMeasures(selector, arm, measures);

            return measures;
        }

        private static (int Hash, Statistics Stats) PrependMeasures(
            Expression selector,
            Arm arm,
            (int Hash, Statistics Stats) tail)
        {
            var test = selector._statistics;
            var hit = arm.Branch._statistics;
            var miss = tail.Stats;

            return
                (HashCode.Combine(arm, tail.Hash),
                new Statistics(
                    Subexpressions: test.Subexpressions + hit.Subexpressions + miss.Subexpressions + 6,
                    Evals: test.Evals + hit.Evals + miss.Evals,
                    Conditions: test.Conditions + hit.Conditions + miss.Conditions + 1,
                    Builtins: test.Builtins + hit.Builtins + miss.Builtins + 1,
                    Depth: 1 + Math.Max(test.Depth + 2, Math.Max(hit.Depth, miss.Depth)),
                    Environment: test.Environment || hit.Environment || miss.Environment));
        }

        internal Switch Prepend(Arm arm) =>
            new(
                Selector,
                Arms.Insert(0, arm),
                Default,
                PrependMeasures(Selector, arm, (GetHashCode(), _statistics)));

        internal Expression SelectBranch(PineValue key)
        {
            foreach (var arm in Arms)
            {
                if (key == arm.Key)
                    return arm.Branch;
            }

            return Default;
        }

        internal Switch MapChildren(Func<Expression, Expression> map)
        {
            var selector = map(Selector);
            var defaultBranch = map(Default);
            var arms = Arms;

            var index = 0;

            foreach (var arm in Arms)
            {
                var branch = map(arm.Branch);

                if (!ReferenceEquals(branch, arm.Branch))
                    arms = arms.SetItem(index, arm with { Branch = branch });

                index++;
            }

            return
                ReferenceEquals(selector, Selector) && ReferenceEquals(defaultBranch, Default) && arms == Arms
                ?
                this
                :
                new Switch(selector, arms, defaultBranch);
        }

        internal Conditional Expand()
        {
            Expression tail = Default;

            foreach (var (key, literalOnLeft, branch) in Enumerable.Reverse(Arms))
            {
                var literal = new Literal(key);

                var condition =
                    new BuiltinBinary(
                        BuiltinFunction.Equal,
                        literalOnLeft ? literal : Selector,
                        literalOnLeft ? Selector : literal);

                tail = new Conditional(condition, tail, branch);
            }

            return (Conditional)tail;
        }

        /// <inheritdoc/>
        public bool Equals(Switch? other) =>
            ReferenceEquals(this, other) ||
            (other is not null &&
            GetHashCode() == other.GetHashCode() &&
            Selector == other.Selector &&
            Default == other.Default &&
            Arms.SequenceEqual(other.Arms));

        /// <inheritdoc/>
        public override int GetHashCode() => base.GetHashCode();
    }

    internal static Expression FuseConditional(Expression condition, Expression falseBranch, Expression trueBranch)
    {
        if (TryMatchSwitchArm(condition) is { } arm)
        {
            if (falseBranch is Switch next &&
                arm.Selector == next.Selector)
            {
                return next.Prepend(new Switch.Arm(arm.Key, arm.LiteralOnLeft, trueBranch));
            }

            if (falseBranch is Conditional tail &&
                TryMatchSwitchArm(tail.Condition) is { } nextArm &&
                arm.Selector == nextArm.Selector)
            {
                return
                    new Switch(
                        arm.Selector,
                        [
                        new(arm.Key, arm.LiteralOnLeft, trueBranch),
                        new(nextArm.Key, nextArm.LiteralOnLeft, tail.TrueBranch)
                        ],
                        tail.FalseBranch);
            }
        }

        return new Conditional(condition, falseBranch, trueBranch);
    }

    private static (Expression Selector, PineValue Key, bool LiteralOnLeft)? TryMatchSwitchArm(Expression condition) =>
        condition switch
        {
            BuiltinBinary { Function: BuiltinFunction.Equal, Left: Literal literal, Right: not Literal } binary =>
            (binary.Right, literal.Value, true),

            BuiltinBinary { Function: BuiltinFunction.Equal, Right: Literal literal, Left: not Literal } binary =>
            (binary.Left, literal.Value, false),

            Literal or List or Eval or Builtin or Conditional or Switch or Environment or Label or
            BuiltinBinary or Length_Equal_Binary or HeadSkip =>
            null,

            _ =>
            throw new NotImplementedException(
                nameof(TryMatchSwitchArm) + " does not handle expression variant: " + condition.GetType().Name)
        };
}
