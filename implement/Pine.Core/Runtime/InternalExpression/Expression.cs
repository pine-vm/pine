using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Runtime.InternalExpression;

/// <summary>
/// Immutable syntax private to a Pine release. Fused nodes preserve the canonical syntax;
/// scoped function bindings are compiler-only and must be lowered before encoding or execution.
/// </summary>
public abstract partial record Expression
{
    private readonly int _hashCode;

    private protected Expression(int hashCode, Statistics statistics = default)
    {
        _hashCode = hashCode;
        _statistics = statistics;
    }

    /// <inheritdoc/>
    public override int GetHashCode() => _hashCode;

    /// <summary>An arbitrary Pine literal, including noncanonical integer encodings.</summary>
    public sealed record Literal : Expression
    {
        /// <summary>The retained immutable value.</summary>
        public PineValue Value { get; }

        /// <summary>Retains the value without copying its tree.</summary>
        public Literal(PineValue value) : base(HashCode.Combine(typeof(Literal), value)) =>
            Value = value ?? throw new ArgumentNullException(nameof(value));

        /// <inheritdoc/>
        public override int GetHashCode() => base.GetHashCode();
    }

    /// <summary>An ordered list of expressions.</summary>
    public sealed record List : Expression
    {
        /// <summary>Immutable, owned child storage.</summary>
        public ImmutableArray<Expression> Items { get; }

        /// <summary>Copies mutable source collections.</summary>
        public List(IEnumerable<Expression> items) : this([.. items]) { }

        /// <summary>Retains an immutable collection.</summary>
        public List(ImmutableArray<Expression> items) : base(ListHash(items), Statistics.List(items)) => Items = items;

        private static int ListHash(ImmutableArray<Expression> items)
        {
            if (items.IsDefault)
                throw new ArgumentException("A list must have initialized immutable storage.", nameof(items));

            var hash = new HashCode();
            hash.Add(typeof(List));

            foreach (var item in items)
                hash.Add(item ?? throw new ArgumentException("A list cannot contain null expressions.", nameof(items)));

            return hash.ToHashCode();
        }

        /// <inheritdoc/>
        public bool Equals(List? other) =>
            ReferenceEquals(this, other) ||
            (other is not null && GetHashCode() == other.GetHashCode() && Items.SequenceEqual(other.Items));

        /// <inheritdoc/>
        public override int GetHashCode() => base.GetHashCode();
    }

    /// <summary>Evaluation of an encoded expression in a computed environment.</summary>
    public sealed record Eval : Expression
    {
        /// <summary>The expression producing the encoded callee.</summary>
        public Expression Encoded { get; }

        /// <summary>The expression producing its environment.</summary>
        public new Expression Environment { get; }

        /// <summary>Retains both children.</summary>
        public Eval(Expression encoded, Expression environment) : base(HashCode.Combine(typeof(Eval), encoded, environment), Statistics.Binary(encoded, environment, evals: 1))
        {
            Encoded = encoded ?? throw new ArgumentNullException(nameof(encoded));
            Environment = environment ?? throw new ArgumentNullException(nameof(environment));
        }

        /// <inheritdoc/>
        public override int GetHashCode() => base.GetHashCode();
    }

    /// <summary>A general builtin call with a checked enum identifier.</summary>
    public sealed record Builtin : Expression
    {
        /// <summary>The release-private function identifier.</summary>
        public BuiltinFunction Function { get; }

        /// <summary>The arbitrary input expression.</summary>
        public Expression Input { get; }

        /// <summary>Rejects undefined identifiers.</summary>
        public Builtin(BuiltinFunction function, Expression input) : base(HashCode.Combine(typeof(Builtin), function, input), Statistics.Unary(input, builtins: 1))
        {
            _ = BuiltinFunctions.ToName(function);
            Function = function;
            Input = input ?? throw new ArgumentNullException(nameof(input));
        }

        /// <inheritdoc/>
        public override int GetHashCode() => base.GetHashCode();
    }

    /// <summary>Only the exact Pine true value chooses the true branch.</summary>
    public sealed record Conditional : Expression
    {
        /// <summary>The tested expression.</summary>
        public Expression Condition { get; }

        /// <summary>The nontrue branch.</summary>
        public Expression FalseBranch { get; }

        /// <summary>The true branch.</summary>
        public Expression TrueBranch { get; }

        /// <summary>Retains the original branch order.</summary>
        public Conditional(Expression condition, Expression falseBranch, Expression trueBranch)
            : base(HashCode.Combine(typeof(Conditional), condition, falseBranch, trueBranch), Statistics.Conditional(condition, falseBranch, trueBranch))
        {
            Condition = condition ?? throw new ArgumentNullException(nameof(condition));
            FalseBranch = falseBranch ?? throw new ArgumentNullException(nameof(falseBranch));
            TrueBranch = trueBranch ?? throw new ArgumentNullException(nameof(trueBranch));
        }

        /// <inheritdoc/>
        public override int GetHashCode() => base.GetHashCode();
    }

    /// <summary>The current environment.</summary>
    public sealed record Environment : Expression
    {
        /// <summary>The shared leaf instance.</summary>
        public static Environment Instance { get; } = new();

        private Environment() : base(typeof(Environment).GetHashCode(), new(Environment: true)) { }

        /// <inheritdoc/>
        public override int GetHashCode() => base.GetHashCode();
    }

    /// <summary>A retained arbitrary label, without eagerly deriving a display string.</summary>
    public sealed record Label : Expression
    {
        /// <summary>The exact canonical label value.</summary>
        public PineValue Value { get; }

        /// <summary>The labeled expression.</summary>
        public Expression Tagged { get; }

        /// <summary>Retains payload and child.</summary>
        public Label(PineValue value, Expression tagged) : base(HashCode.Combine(typeof(Label), value, tagged), Statistics.Unary(tagged))
        {
            Value = value ?? throw new ArgumentNullException(nameof(value));
            Tagged = tagged ?? throw new ArgumentNullException(nameof(tagged));
        }

        /// <inheritdoc/>
        public override int GetHashCode() => base.GetHashCode();
    }

    /// <summary>A builtin with a statically two-item expression list, without retaining that list.</summary>
    public sealed record BuiltinBinary : Expression
    {
        /// <summary>The checked release-private function identifier.</summary>
        public BuiltinFunction Function { get; }

        /// <summary>The first list item.</summary>
        public Expression Left { get; }

        /// <summary>The second list item.</summary>
        public Expression Right { get; }

        /// <summary>Retains operands in canonical evaluation order.</summary>
        public BuiltinBinary(BuiltinFunction function, Expression left, Expression right)
            : base(HashCode.Combine(typeof(BuiltinBinary), function, left, right), Statistics.Binary(left, right, subexpressions: 3, builtins: 1, depth: 2))
        {
            _ = BuiltinFunctions.ToName(function);
            Function = function;
            Left = left ?? throw new ArgumentNullException(nameof(left));
            Right = right ?? throw new ArgumentNullException(nameof(right));
        }

        /// <inheritdoc/>
        public override int GetHashCode() => base.GetHashCode();
    }

    /// <summary>Binary equality with one operand's length, without builtin or input-list wrappers.</summary>
    public sealed record Length_Equal_Binary : Expression
    {
        /// <summary>The general expression whose result contributes its length.</summary>
        public Expression Sequence { get; }

        /// <summary>The other operand, not necessarily a constant or an integer.</summary>
        public Expression Other { get; }

        /// <summary>Preserves the original operand evaluation order.</summary>
        public bool LengthOnLeft { get; }

        /// <summary>Retains exactly two expression references and an inline ordering flag.</summary>
        public Length_Equal_Binary(Expression sequence, Expression other, bool lengthOnLeft = true)
            : base(HashCode.Combine(typeof(Length_Equal_Binary), sequence, other, lengthOnLeft), Statistics.LengthEqual(sequence, other))
        {
            Sequence = sequence ?? throw new ArgumentNullException(nameof(sequence));
            Other = other ?? throw new ArgumentNullException(nameof(other));
            LengthOnLeft = lengthOnLeft;
        }

        /// <inheritdoc/>
        public override int GetHashCode() => base.GetHashCode();
    }

    /// <summary>head(skip([canonical Int32 literal, input])), with the count stored inline.</summary>
    public sealed record HeadSkip : Expression
    {
        /// <summary>The exact signed literal count, including negative values.</summary>
        public int Count { get; }

        /// <summary>The skipped sequence expression.</summary>
        public Expression Input { get; }

        /// <summary>Retains no count literal, skip node or input-list wrapper.</summary>
        public HeadSkip(int count, Expression input) : base(HashCode.Combine(typeof(HeadSkip), count, input), Statistics.Unary(input, builtins: 2, subexpressions: 4, depth: 3))
        {
            Count = count;
            Input = input ?? throw new ArgumentNullException(nameof(input));
        }

        /// <inheritdoc/>
        public override int GetHashCode() => base.GetHashCode();
    }
}
