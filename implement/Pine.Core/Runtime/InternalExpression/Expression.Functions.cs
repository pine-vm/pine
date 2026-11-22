using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Runtime.InternalExpression;

public abstract partial record Expression
{
    /// <summary>Calls a function in the innermost block with a single logical Pine environment.</summary>
    public sealed record Call : Expression
    {
        /// <summary>The function identifier, local to its block.</summary>
        public int FunctionId { get; }

        /// <summary>The expression that produces the callee's logical environment.</summary>
        public new Expression Environment { get; }

        /// <summary>Creates a symbolic call. The enclosing block validates the target.</summary>
        public Call(int functionId, Expression environment)
            : base(
                HashCode.Combine(typeof(Call), functionId, environment),
                Statistics.Unary(environment) with { FunctionBindings = true, Evals = environment.EvalCount + 1 })
        {
            FunctionId = functionId;
            Environment = environment;
        }

        /// <inheritdoc/>
        public override int GetHashCode() => base.GetHashCode();
    }

    /// <summary>A block-local function and its body, evaluated in the environment supplied by a call.</summary>
    public readonly record struct FunctionDefinition(int Id, Expression Body);

    /// <summary>A group of mutually visible functions and an entry expression in their scope.</summary>
    public sealed record LetFunctions : Expression
    {
        /// <summary>The functions in table order. IDs need not be contiguous.</summary>
        public ImmutableArray<FunctionDefinition> Functions { get; }

        /// <summary>The body evaluated in the enclosing expression's environment.</summary>
        public Expression Entry { get; }

        /// <summary>Creates a block, rejecting duplicate IDs and calls to functions outside the block.</summary>
        public LetFunctions(IEnumerable<FunctionDefinition> functions, Expression entry)
            : this([.. functions], entry)
        {
        }

        private LetFunctions(ImmutableArray<FunctionDefinition> functions, Expression entry)
            : this(functions, entry, Measure(functions, entry))
        {
        }

        private LetFunctions(
            ImmutableArray<FunctionDefinition> functions,
            Expression entry,
            (int Hash, Statistics Stats) measures)
            : base(measures.Hash, measures.Stats)
        {
            var ids = new HashSet<int>();
            var pending = new Stack<Expression>();

            foreach (var function in functions)
            {
                if (!ids.Add(function.Id))
                    throw new ArgumentException($"Duplicate function ID: {function.Id}.", nameof(functions));

                pending.Push(function.Body);
            }

            pending.Push(entry);

            while (pending.TryPop(out var node))
            {
                if (!node.ContainsFunctionBindings || node is LetFunctions)
                    continue;

                if (node is Call call && !ids.Contains(call.FunctionId))
                    throw new ArgumentException($"Unknown function ID in block: {call.FunctionId}.", nameof(functions));

                PushChildren(node, pending);
            }

            Functions = functions;
            Entry = entry;
        }

        private static (int Hash, Statistics Stats) Measure(
            ImmutableArray<FunctionDefinition> functions,
            Expression entry)
        {
            var hash = new HashCode();
            hash.Add(typeof(LetFunctions));
            hash.Add(entry);
            var stats = new Statistics(Subexpressions: 1, FunctionBindings: true).IncludeChild(entry);

            foreach (var function in functions)
            {
                hash.Add(function);
                stats = stats.IncludeChild(function.Body);
            }

            // A function body reads its call's environment, not this block's ambient environment.
            return (hash.ToHashCode(), stats with { Environment = entry.ReferencesEnvironment });
        }

        internal LetFunctions MapChildren(Func<Expression, Expression> map)
        {
            ImmutableArray<FunctionDefinition>.Builder? functions = null;

            for (var index = 0; index < Functions.Length; index++)
            {
                var function = Functions[index];
                var body = map(function.Body);

                if (ReferenceEquals(body, function.Body))
                    continue;

                functions ??= Functions.ToBuilder();
                functions[index] = function with { Body = body };
            }

            var entry = map(Entry);

            return
                functions is null && ReferenceEquals(entry, Entry)
                ?
                this
                :
                new(functions?.MoveToImmutable() ?? Functions, entry);
        }

        /// <inheritdoc/>
        public bool Equals(LetFunctions? other) =>
            ReferenceEquals(this, other) ||
            (other is not null && GetHashCode() == other.GetHashCode() &&
            Entry == other.Entry && Functions.SequenceEqual(other.Functions));

        /// <inheritdoc/>
        public override int GetHashCode() => base.GetHashCode();
    }

    /// <summary>Whether the tree must be lowered before encoding or execution.</summary>
    public bool ContainsFunctionBindings => _statistics.FunctionBindings;

    internal void RequireLoweredFunctionBindings()
    {
        if (ContainsFunctionBindings)
            throw UnloweredFunctionBindings();
    }

    internal static InvalidOperationException UnloweredFunctionBindings() =>
        new("Lower LetFunctions and Call with an environment layout before encoding or executing the expression.");
}
