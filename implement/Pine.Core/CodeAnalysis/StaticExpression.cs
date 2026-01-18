using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.CodeAnalysis;


/// <summary>
/// Analog to the <see cref="Expression"/>, but without ability to parse-and-eval arbitrary code.
/// This means its expressiveness is limited to not allow for the dynamic/metaprogramming as is possible in the general Pine expression.
/// Generic parameter <typeparamref name="IdentifierT"/> allows callers to use richer representations for user-defined identifiers.
/// </summary>
public abstract record StaticExpression<IdentifierT>
{
    /// <summary>
    /// Number of subexpressions contained in this expression.
    /// </summary>
    public abstract int SubexpressionCount { get; }

    /// <summary>
    /// Represents the <see cref="Expression.Environment"/>.
    /// </summary>
    public static readonly StaticExpression<IdentifierT> EnvironmentInstance =
        new Environment();

    /// <summary>
    /// Reference to a parameter by its index within a containing context.
    /// <para>
    /// Note: Since this expression model does not allow lambdas or local functions, there is only one containing function.
    /// </para>
    /// </summary>
    public static StaticExpression<IdentifierT> ParameterReference(int parameterIndex) =>
        new ParameterReferenceExpr(parameterIndex);

    /// <summary>
    /// Create a list expression from the given items.
    /// Analog to <see cref="Expression.List"/>.
    /// A list expression contains a list of subexpressions.
    /// </summary>
    /// <param name="items">The items contained in the list.</param>
    /// <returns>A <see cref="List"/> instance containing <paramref name="items"/>.</returns>
    public static StaticExpression<IdentifierT> ListInstance(IReadOnlyList<StaticExpression<IdentifierT>> items) =>
        new List(items);

    /// <summary>
    /// Create a literal expression that wraps a concrete <see cref="PineValue"/>.
    /// Analog to <see cref="Expression.Literal"/>.
    /// </summary>
    /// <param name="value">The value to wrap in a literal expression.</param>
    public static StaticExpression<IdentifierT> LiteralInstance(PineValue value) =>
        new Literal(value);

    /// <summary>
    /// Create a conditional expression with a condition and two branches.
    /// Analog to <see cref="Expression.Conditional"/>.
    /// </summary>
    /// <param name="condition">Expression evaluated to determine which branch to take.</param>
    /// <param name="falseBranch">Expression evaluated if the <paramref name="condition"/> does not evaluate to true.</param>
    /// <param name="trueBranch">Expression evaluated if the <paramref name="condition"/> evaluates to true.</param>
    public static StaticExpression<IdentifierT> ConditionalInstance(
        StaticExpression<IdentifierT> condition,
        StaticExpression<IdentifierT> falseBranch,
        StaticExpression<IdentifierT> trueBranch) =>
        new Conditional(condition, falseBranch, trueBranch);

    /// <summary>
    /// Create an application of a Pine kernel function to an input expression.
    /// See <see cref="KernelApplication"/> for details.
    /// </summary>
    /// <param name="function">Name of the kernel function to apply.</param>
    /// <param name="input">Input expression for the kernel function.</param>
    public static StaticExpression<IdentifierT> KernelApplicationInstance(
        string function,
        StaticExpression<IdentifierT> input) =>
        new KernelApplication(function, input);

    /// <summary>
    /// Create an application of a user-defined (named) function to a list of argument expressions.
    /// </summary>
    /// <param name="functionName">The function name.</param>
    /// <param name="arguments">Arguments passed to the function.</param>
    public static FunctionApplication FunctionApplicationInstance(
        IdentifierT functionName,
        StaticExpression<IdentifierT> arguments) =>
        new(functionName, arguments);

    /// <summary>
    /// Analog to <see cref="Expression.List"/>.
    /// A list expression contains a list of subexpressions.
    /// </summary>
    public record List
        : StaticExpression<IdentifierT>
    {
        private readonly int _slimHashCode;

        /// <inheritdoc/>
        public override int SubexpressionCount { get; }

        /// <summary>
        /// The list of subexpressions.
        /// </summary>
        public IReadOnlyList<StaticExpression<IdentifierT>> Items { get; }

        internal List(IReadOnlyList<StaticExpression<IdentifierT>> items)
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
            public IReadOnlyList<StaticExpression<IdentifierT>> Items { get; }

            internal readonly int SlimHashCode;

            public ListStruct(IReadOnlyList<StaticExpression<IdentifierT>> items)
            {
                Items = items;

                SlimHashCode = ComputeHashCode(items);
            }

            public override int GetHashCode()
            {
                return SlimHashCode;
            }

            public static int ComputeHashCode(IReadOnlyList<StaticExpression<IdentifierT>> items)
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
        : StaticExpression<IdentifierT>
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
        : StaticExpression<IdentifierT>
    {
        private readonly int _slimHashCode;

        /// <summary>
        /// The expression evaluated to determine which branch to take.
        /// </summary>
        public StaticExpression<IdentifierT> Condition { get; }

        /// <summary>
        /// The expression evaluated if the condition does not evaluate to true.
        /// </summary>
        public StaticExpression<IdentifierT> FalseBranch { get; }

        /// <summary>
        /// The expression evaluated if the condition evaluates to true.
        /// </summary>
        public StaticExpression<IdentifierT> TrueBranch { get; }

        /// <inheritdoc/>
        public override int SubexpressionCount { get; }

        internal Conditional(
            StaticExpression<IdentifierT> condition,
            StaticExpression<IdentifierT> falseBranch,
            StaticExpression<IdentifierT> trueBranch)
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
            public StaticExpression<IdentifierT> Condition { get; }

            public StaticExpression<IdentifierT> FalseBranch { get; }

            public StaticExpression<IdentifierT> TrueBranch { get; }

            internal readonly int SlimHashCode;

            public ConditionalStruct(
                StaticExpression<IdentifierT> condition,
                StaticExpression<IdentifierT> falseBranch,
                StaticExpression<IdentifierT> trueBranch)
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
                StaticExpression<IdentifierT> condition,
                StaticExpression<IdentifierT> falseBranch,
                StaticExpression<IdentifierT> trueBranch)
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
        : StaticExpression<IdentifierT>
    {
        /// <summary>
        /// The name of the kernel function to be applied.
        /// </summary>
        public string Function { get; }

        /// <summary>
        /// Input for the kernel function.
        /// </summary>
        public StaticExpression<IdentifierT> Input { get; }

        /// <inheritdoc/>
        public override int SubexpressionCount { get; }

        /// <summary>
        /// Creates a new instance of a kernel application.
        /// </summary>
        /// <param name="function">Name of the kernel function.</param>
        /// <param name="input">Input expression.</param>
        internal KernelApplication(
            string function,
            StaticExpression<IdentifierT> input)
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

    /*
     * TODO: Probably remove the 'Environment' variant after phasing out usages across project.
     * */
    /// <summary>
    /// Corresponds to the <see cref="Expression.Environment"/>.
    /// </summary>
    public sealed record Environment :
        StaticExpression<IdentifierT>
    {
        /// <summary>
        /// Always returns zero, as an <see cref="Environment"/> expression does not contain any subexpressions.
        /// </summary>
        public override int SubexpressionCount { get; } = 0;
    }

    /// <summary>
    /// Reference to a parameter by its index within a containing context.
    /// <para>
    /// Note: Since this expression model does not allow lambdas or local functions, there is only one containing function.
    /// </para>
    /// </summary>
    public sealed record ParameterReferenceExpr(
        int ParameterIndex)
        : StaticExpression<IdentifierT>
    {
        /// <summary>
        /// Always returns zero, as a <see cref="ParameterReferenceExpr"/> expression does not contain any subexpressions.
        /// </summary>
        public override int SubexpressionCount { get; } = 0;
    }

    /// <summary>
    /// Application of a user-defined function to a list of argument expressions.
    /// </summary>
    public record FunctionApplication
        : StaticExpression<IdentifierT>
    {
        private readonly int _slimHashCode;

        /// <summary>
        /// The function name.
        /// </summary>
        public IdentifierT FunctionName { get; }

        /// <summary>
        /// Arguments passed to the function.
        /// </summary>
        public StaticExpression<IdentifierT> Arguments { get; }

        /// <inheritdoc/>
        public override int SubexpressionCount { get; } = 0;

        /// <summary>
        /// Create a new <see cref="FunctionApplication"/>.
        /// </summary>
        /// <param name="functionName">The function name.</param>
        /// <param name="arguments">Argument expressions.</param>
        public FunctionApplication(
            IdentifierT functionName,
            StaticExpression<IdentifierT> arguments)
        {
            FunctionName = functionName;
            Arguments = arguments;

            _slimHashCode = HashCode.Combine(functionName, arguments);

            SubexpressionCount += arguments.SubexpressionCount;

            _slimHashCode = HashCode.Combine(_slimHashCode, arguments);
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

            if (FunctionName is null)
            {
                if (notNull.FunctionName is not null)
                    return false;
            }
            else
            {
                if (!FunctionName.Equals(notNull.FunctionName))
                    return false;
            }

            if (!Arguments.Equals(notNull.Arguments))
                return false;

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
    public sealed record CrashingParseAndEval(
        StaticExpression<IdentifierT> Encoded,
        StaticExpression<IdentifierT> EnvironmentExpr)
        : StaticExpression<IdentifierT>
    {
        /// <inheritdoc/>
        public override int SubexpressionCount =>
            Encoded.SubexpressionCount +
            EnvironmentExpr.SubexpressionCount;
    }

    /// <summary>
    /// Recursively maps each user-defined function identifier (<see cref="FunctionApplication"/>) from
    /// <typeparamref name="IdentifierT"/> to <typeparamref name="TOut"/> while preserving the
    /// structure of the expression tree and leaving all other node kinds unchanged.
    /// </summary>
    /// <typeparam name="TOut">The target type of the mapped function identifiers.</typeparam>
    /// <param name="expression">Root static expression to transform.</param>
    /// <param name="mapIdentifier">Mapping function applied to every function identifier encountered.</param>
    /// <returns>A new <see cref="StaticExpression{TOut}"/> tree with mapped identifiers.</returns>
    /// <remarks>
    /// Traversal is depth-first. Kernel function applications, literals, parameter references, conditionals,
    /// list nodes, and <see cref="CrashingParseAndEval"/> nodes are recreated only as needed. Argument subtrees are
    /// transformed recursively. This operation is pure and does not mutate the original tree.
    /// </remarks>
    public static StaticExpression<TOut> MapFunctionIdentifier<TOut>(
        StaticExpression<IdentifierT> expression,
        Func<IdentifierT, TOut> mapIdentifier)
    {
        return expression switch
        {
            List list =>
                StaticExpression<TOut>.ListInstance(
                    [.. list.Items.Select(item => MapFunctionIdentifier(item, mapIdentifier))]),

            Literal literal =>
                StaticExpression<TOut>.LiteralInstance(literal.Value),

            Environment =>
            StaticExpression<TOut>.EnvironmentInstance,

            Conditional conditional =>
                StaticExpression<TOut>.ConditionalInstance(
                    MapFunctionIdentifier(conditional.Condition, mapIdentifier),
                    MapFunctionIdentifier(conditional.FalseBranch, mapIdentifier),
                    MapFunctionIdentifier(conditional.TrueBranch, mapIdentifier)),

            KernelApplication kernel =>
                StaticExpression<TOut>.KernelApplicationInstance(
                    kernel.Function,
                    MapFunctionIdentifier(kernel.Input, mapIdentifier)),

            FunctionApplication funApp =>
                StaticExpression<TOut>.FunctionApplicationInstance(
                    mapIdentifier(funApp.FunctionName),
                    MapFunctionIdentifier(funApp.Arguments, mapIdentifier)),

            CrashingParseAndEval crashing =>
            new StaticExpression<TOut>.CrashingParseAndEval(
                MapFunctionIdentifier(crashing.Encoded, mapIdentifier),
                MapFunctionIdentifier(crashing.EnvironmentExpr, mapIdentifier)),

            _ =>
            throw new NotSupportedException($"Unknown static expression type: {expression.GetType()}")
        };
    }
}
