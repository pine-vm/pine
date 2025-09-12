using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.CodeAnalysis;


/// <summary>
/// Analog to the <see cref="Expression"/>, but without ability to parse-and-eval arbitrary code.
/// This means its expressiveness is limited to not allow for the dynamic/metaprogramming as is possible in the general Pine expression.
/// </summary>
public abstract record StaticExpression
{
    /// <summary>
    /// Number of subexpressions contained in this expression.
    /// </summary>
    public abstract int SubexpressionCount { get; }

    /// <summary>
    /// Create a list expression from the given items.
    /// Analog to <see cref="Expression.List"/>.
    /// A list expression contains a list of subexpressions.
    /// </summary>
    /// <param name="items">The items contained in the list.</param>
    /// <returns>A <see cref="List"/> instance containing <paramref name="items"/>.</returns>
    public static StaticExpression ListInstance(IReadOnlyList<StaticExpression> items) =>
        new List(items);

    /// <summary>
    /// Create a literal expression that wraps a concrete <see cref="PineValue"/>.
    /// Analog to <see cref="Expression.Literal"/>.
    /// </summary>
    /// <param name="value">The value to wrap in a literal expression.</param>
    public static StaticExpression LiteralInstance(PineValue value) =>
        new Literal(value);

    /// <summary>
    /// Create a conditional expression with a condition and two branches.
    /// Analog to <see cref="Expression.Conditional"/>.
    /// </summary>
    /// <param name="condition">Expression evaluated to determine which branch to take.</param>
    /// <param name="falseBranch">Expression evaluated if the <paramref name="condition"/> does not evaluate to true.</param>
    /// <param name="trueBranch">Expression evaluated if the <paramref name="condition"/> evaluates to true.</param>
    public static StaticExpression ConditionalInstance(
        StaticExpression condition,
        StaticExpression falseBranch,
        StaticExpression trueBranch) =>
        new Conditional(condition, falseBranch, trueBranch);

    /// <summary>
    /// Create an application of a Pine kernel function to an input expression.
    /// See <see cref="KernelApplication"/> for details.
    /// </summary>
    /// <param name="function">Name of the kernel function to apply.</param>
    /// <param name="input">Input expression for the kernel function.</param>
    public static StaticExpression KernelApplicationInstance(
        string function,
        StaticExpression input) =>
        new KernelApplication(function, input);

    /// <summary>
    /// Create a parameter reference expression identified by a path inside the original evaluation environment.
    /// </summary>
    /// <param name="path">Path of indices identifying the parameter in the environment.</param>
    public static StaticExpression ParameterReferenceInstance(IReadOnlyList<int> path) =>
        new ParameterReferenceExpression(path);

    /// <summary>
    /// Create an application of a user-defined (named) function to a list of argument expressions.
    /// </summary>
    /// <param name="functionName">The function name.</param>
    /// <param name="arguments">Argument expressions in evaluation order.</param>
    public static StaticExpression FunctionApplicationInstance(
        string functionName,
        IReadOnlyList<StaticExpression> arguments) =>
        new FunctionApplication(functionName, arguments);

    /// <summary>
    /// Analog to <see cref="Expression.List"/>.
    /// A list expression contains a list of subexpressions.
    /// </summary>
    public record List
        : StaticExpression
    {
        private readonly int _slimHashCode;

        /// <inheritdoc/>
        public override int SubexpressionCount { get; }

        /// <summary>
        /// The list of subexpressions.
        /// </summary>
        public IReadOnlyList<StaticExpression> Items { get; }

        internal List(IReadOnlyList<StaticExpression> items)
            :
            this(new ListStruct(items))
        {
        }

        internal List(ListStruct listKey)
        {
            Items = listKey.Items;

            _slimHashCode = listKey.SlimHashCode;

            SubexpressionCount = Items.Count;

            for (var i = 0; i < Items.Count; ++i)
            {
                SubexpressionCount += Items[i].SubexpressionCount;
            }
        }

        /// <inheritdoc/>
        public virtual bool Equals(List? other)
        {
            if (other is not { } notNull)
                return false;

            if (ReferenceEquals(this, notNull))
                return true;

            if (!(_slimHashCode == notNull._slimHashCode))
                return false;

            if (Items.Count != notNull.Items.Count)
                return false;

            for (var i = 0; i < Items.Count; ++i)
            {
                if (!Items[i].Equals(notNull.Items[i]))
                    return false;
            }

            return true;
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            _slimHashCode;

        /// <summary>
        /// Lightweight key structure used to cache the list hash code while keeping item identity.
        /// </summary>
        internal readonly record struct ListStruct
        {
            /// <summary>
            /// Items in the list.
            /// </summary>
            public IReadOnlyList<StaticExpression> Items { get; }

            internal readonly int SlimHashCode;

            public ListStruct(IReadOnlyList<StaticExpression> items)
            {
                Items = items;

                SlimHashCode = ComputeHashCode(items);
            }

            public override int GetHashCode()
            {
                return SlimHashCode;
            }

            public static int ComputeHashCode(IReadOnlyList<StaticExpression> items)
            {
                var hashCode = new HashCode();

                for (var i = 0; i < items.Count; ++i)
                {
                    hashCode.Add(items[i].GetHashCode());
                }

                return hashCode.ToHashCode();
            }
        }
    }

    /// <summary>
    /// Analog to <see cref="Expression.Literal"/>.
    /// A literal expression only contains a concrete value.
    /// </summary>
    public record Literal(
        PineValue Value)
        : StaticExpression
    {
        /// <summary>
        /// Always returns zero, as a <see cref="Literal"/> expression does not contain any subexpressions.
        /// </summary>
        public override int SubexpressionCount { get; } = 0;
    }

    /// <summary>
    /// Analog to <see cref="Expression.Conditional"/>.
    /// A conditional expression contains a condition and two branches.
    /// </summary>
    public record Conditional
        : StaticExpression
    {
        private readonly int _slimHashCode;

        /// <summary>
        /// The expression evaluated to determine which branch to take.
        /// </summary>
        public StaticExpression Condition { get; }

        /// <summary>
        /// The expression evaluated if the condition does not evaluate to true.
        /// </summary>
        public StaticExpression FalseBranch { get; }

        /// <summary>
        /// The expression evaluated if the condition evaluates to true.
        /// </summary>
        public StaticExpression TrueBranch { get; }

        /// <inheritdoc/>
        public override int SubexpressionCount { get; }

        internal Conditional(
            StaticExpression condition,
            StaticExpression falseBranch,
            StaticExpression trueBranch)
            :
            this(new ConditionalStruct(condition, falseBranch, trueBranch))
        {
        }

        internal Conditional(
            ConditionalStruct conditionalStruct)
        {
            Condition = conditionalStruct.Condition;
            FalseBranch = conditionalStruct.FalseBranch;
            TrueBranch = conditionalStruct.TrueBranch;

            _slimHashCode = conditionalStruct.SlimHashCode;

            SubexpressionCount =
                Condition.SubexpressionCount +
                FalseBranch.SubexpressionCount +
                TrueBranch.SubexpressionCount +
                3;
        }

        /// <inheritdoc/>
        public virtual bool Equals(Conditional? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is not { } notNull)
                return false;

            return
                _slimHashCode == notNull._slimHashCode &&
                Condition.Equals(notNull.Condition) &&
                FalseBranch.Equals(notNull.FalseBranch) &&
                TrueBranch.Equals(notNull.TrueBranch);
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            _slimHashCode;

        /// <summary>
        /// Lightweight key structure used to cache the conditional hash while keeping child identity.
        /// </summary>
        internal readonly record struct ConditionalStruct
        {
            public StaticExpression Condition { get; }

            public StaticExpression FalseBranch { get; }

            public StaticExpression TrueBranch { get; }

            internal readonly int SlimHashCode;

            public ConditionalStruct(
                StaticExpression condition,
                StaticExpression falseBranch,
                StaticExpression trueBranch)
            {
                Condition = condition;
                FalseBranch = falseBranch;
                TrueBranch = trueBranch;

                SlimHashCode = ComputeHashCode(condition, falseBranch, trueBranch);
            }

            /// <inheritdoc/>
            public override readonly int GetHashCode() =>
                SlimHashCode;

            /// <inheritdoc/>
            public readonly bool Equals(ConditionalStruct other)
            {
                return
                    other.SlimHashCode == SlimHashCode &&
                    other.Condition.Equals(Condition) &&
                    other.FalseBranch.Equals(FalseBranch) &&
                    other.TrueBranch.Equals(TrueBranch);
            }

            public static int ComputeHashCode(
                StaticExpression condition,
                StaticExpression falseBranch,
                StaticExpression trueBranch)
            {
                return HashCode.Combine(condition, falseBranch, trueBranch);
            }
        }
    }

    /// <summary>
    /// Analog to <see cref="Expression.KernelApplication"/>.
    /// Application of a kernel function to an input expression.
    /// 
    /// Kernel functions are the built-in functions of the Pine language.
    /// 
    /// Kernel functions never crash the program, but may return default values in case of nonsensical input.
    /// Therefore it remains the responsibility of the caller to add branches for error messages as needed.
    /// </summary>
    public record KernelApplication
        : StaticExpression
    {
        /// <summary>
        /// The name of the kernel function to be applied.
        /// </summary>
        public string Function { get; }

        /// <summary>
        /// Input for the kernel function.
        /// </summary>
        public StaticExpression Input { get; }

        /// <inheritdoc/>
        public override int SubexpressionCount { get; }

        /// <summary>
        /// Creates a new instance of a kernel application.
        /// </summary>
        /// <param name="function">Name of the kernel function.</param>
        /// <param name="input">Input expression.</param>
        internal KernelApplication(
            string function,
            StaticExpression input)
        {
            Function = function;
            Input = input;

            SubexpressionCount = input.SubexpressionCount + 1;
        }

        /// <inheritdoc/>
        public virtual bool Equals(KernelApplication? other)
        {
            if (other is not { } notNull)
                return false;

            return
                notNull.Function == Function &&
                (ReferenceEquals(notNull.Input, Input) || notNull.Input.Equals(Input));
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hash = new HashCode();

            hash.Add(Function);
            hash.Add(Input);

            return hash.ToHashCode();
        }
    }

    /// <summary>
    /// Parameters are identified by the path inside the environment of the original Pine expression.
    /// </summary>
    public record ParameterReferenceExpression : StaticExpression
    {
        /// <summary>
        /// Create a parameter reference identified by <paramref name="path"/> within the original environment.
        /// </summary>
        /// <param name="path">Path of indices that identify the parameter.</param>
        public ParameterReferenceExpression(IReadOnlyList<int> path)
        {
            Path = path;
        }

        /// <summary>
        /// Gets the path inside the environment of the original Pine expression that identifies the parameter.
        /// </summary>
        public IReadOnlyList<int> Path { get; } = [];

        /// <inheritdoc/>
        public override int SubexpressionCount { get; } = 0;

        /// <inheritdoc/>
        public override string ToString()
        {
            return $"param_{string.Join("_", Path)}";
        }

        /// <inheritdoc/>
        public virtual bool Equals(ParameterReferenceExpression? other)
        {
            if (other is not { } notNull)
                return false;

            if (notNull.Path.Count != Path.Count)
                return false;

            for (var i = 0; i < Path.Count; i++)
            {
                if (other.Path[i] != Path[i])
                    return false;
            }

            return true;
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new HashCode();

            for (var i = 0; i != Path.Count; i++)
                hashCode.Add(Path[i].GetHashCode());

            return hashCode.ToHashCode();
        }
    }

    /// <summary>
    /// Application of a user-defined function to a list of argument expressions.
    /// </summary>
    public record FunctionApplication
        : StaticExpression
    {
        private readonly int _slimHashCode;

        /// <summary>
        /// The function name.
        /// </summary>
        public string FunctionName { get; }

        /// <summary>
        /// Argument expressions passed to the function in evaluation order.
        /// </summary>
        public IReadOnlyList<StaticExpression> Arguments { get; }

        /// <inheritdoc/>
        public override int SubexpressionCount { get; } = 0;

        /// <summary>
        /// Create a new <see cref="FunctionApplication"/>.
        /// </summary>
        /// <param name="functionName">The function name.</param>
        /// <param name="arguments">Argument expressions.</param>
        public FunctionApplication(
            string functionName,
            IReadOnlyList<StaticExpression> arguments)
        {
            FunctionName = functionName;
            Arguments = arguments;

            _slimHashCode = HashCode.Combine(functionName, arguments);

            foreach (var arg in arguments)
            {
                SubexpressionCount += arg.SubexpressionCount;

                _slimHashCode = HashCode.Combine(_slimHashCode, arg);
            }
        }

        /// <inheritdoc/>
        public override string ToString()
        {
            return
                $"({FunctionName} "
                + string.Join(" ", Arguments.Select(arg => arg.ToString()))
                + ")";
        }

        /// <inheritdoc/>
        public virtual bool Equals(FunctionApplication? other)
        {
            if (other is not FunctionApplication notNull)
                return false;

            if (ReferenceEquals(this, notNull))
                return true;

            if (_slimHashCode != notNull._slimHashCode)
                return false;

            if (FunctionName != notNull.FunctionName)
                return false;

            if (Arguments.Count != notNull.Arguments.Count)
                return false;

            for (var i = 0; i < Arguments.Count; ++i)
            {
                if (!Arguments[i].Equals(notNull.Arguments[i]))
                    return false;
            }

            return true;
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            return _slimHashCode;
        }
    }

    /// <summary>
    /// If a parse-and-eval expression uses a literal as the encoded expression,
    /// we can statically check whether it will crash at runtime.
    /// Compilers sometimes emit crashing branches, and we explicitly mark these in code analysis.
    /// </summary>
    public sealed record AlwaysCrash
        : StaticExpression
    {
        /// <summary>
        /// Always zero; this node has no children.
        /// </summary>
        public override int SubexpressionCount => 0;
    }

    /// <summary>
    /// Enumerate the expression and all of its descendants in depth-first order.
    /// The sequence starts with <paramref name="expression"/> itself.
    /// </summary>
    /// <param name="expression">Root expression to traverse.</param>
    /// <returns>A depth-first traversal sequence over the expression tree.</returns>
    public static IEnumerable<StaticExpression> EnumerateAllDescendants(
        StaticExpression expression)
    {
        var stack = new Stack<StaticExpression>();

        stack.Push(expression);

        while (stack.Count > 0)
        {
            var current = stack.Pop();

            yield return current;

            switch (current)
            {
                case List list:

                    for (var i = 0; i < list.Items.Count; i++)
                    {
                        stack.Push(list.Items[i]);
                    }

                    break;

                case Conditional conditional:
                    stack.Push(conditional.Condition);
                    stack.Push(conditional.FalseBranch);
                    stack.Push(conditional.TrueBranch);
                    break;

                case KernelApplication kernelApp:
                    stack.Push(kernelApp.Input);
                    break;

                case ParameterReferenceExpression:
                    break;

                case Literal:
                    break;

                case FunctionApplication namedFunctionApp:
                    for (var i = 0; i < namedFunctionApp.Arguments.Count; i++)
                    {
                        stack.Push(namedFunctionApp.Arguments[i]);
                    }
                    break;

                case AlwaysCrash:
                    break;

                default:
                    throw new NotSupportedException($"Unknown static expression type: {current.GetType()}");
            }
        }
    }
}
