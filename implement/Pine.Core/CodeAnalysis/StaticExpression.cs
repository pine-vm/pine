using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Linq;

namespace Pine.Core.CodeAnalysis;


/// <summary>
/// Analog to the <see cref="Expression"/>, but without ability to parse-and-eval arbitrary code.
/// This means its expressiveness is limited to not allow for the dynamic/metaprogramming as is possible in the general Pine expression.
/// Generic parameter <typeparamref name="TFunctionName"/> allows callers to use richer representations for user-defined function identifiers.
/// </summary>
public abstract record StaticExpression<TFunctionName>
{
    /// <summary>
    /// Number of subexpressions contained in this expression.
    /// </summary>
    public abstract int SubexpressionCount { get; }

    /// <summary>
    /// Represents the <see cref="Expression.Environment"/>.
    /// </summary>
    public static readonly StaticExpression<TFunctionName> EnvironmentInstance =
        new Environment();

    /// <summary>
    /// Create a list expression from the given items.
    /// Analog to <see cref="Expression.List"/>.
    /// A list expression contains a list of subexpressions.
    /// </summary>
    /// <param name="items">The items contained in the list.</param>
    /// <returns>A <see cref="List"/> instance containing <paramref name="items"/>.</returns>
    public static StaticExpression<TFunctionName> ListInstance(IReadOnlyList<StaticExpression<TFunctionName>> items) =>
        new List(items);

    /// <summary>
    /// Create a literal expression that wraps a concrete <see cref="PineValue"/>.
    /// Analog to <see cref="Expression.Literal"/>.
    /// </summary>
    /// <param name="value">The value to wrap in a literal expression.</param>
    public static StaticExpression<TFunctionName> LiteralInstance(PineValue value) =>
        new Literal(value);

    /// <summary>
    /// Create a conditional expression with a condition and two branches.
    /// Analog to <see cref="Expression.Conditional"/>.
    /// </summary>
    /// <param name="condition">Expression evaluated to determine which branch to take.</param>
    /// <param name="falseBranch">Expression evaluated if the <paramref name="condition"/> does not evaluate to true.</param>
    /// <param name="trueBranch">Expression evaluated if the <paramref name="condition"/> evaluates to true.</param>
    public static StaticExpression<TFunctionName> ConditionalInstance(
        StaticExpression<TFunctionName> condition,
        StaticExpression<TFunctionName> falseBranch,
        StaticExpression<TFunctionName> trueBranch) =>
        new Conditional(condition, falseBranch, trueBranch);

    /// <summary>
    /// Create an application of a Pine kernel function to an input expression.
    /// See <see cref="KernelApplication"/> for details.
    /// </summary>
    /// <param name="function">Name of the kernel function to apply.</param>
    /// <param name="input">Input expression for the kernel function.</param>
    public static StaticExpression<TFunctionName> KernelApplicationInstance(
        string function,
        StaticExpression<TFunctionName> input) =>
        new KernelApplication(function, input);

    /// <summary>
    /// Create an application of a user-defined (named) function to a list of argument expressions.
    /// </summary>
    /// <param name="functionName">The function name.</param>
    /// <param name="arguments">Arguments passed to the function.</param>
    public static FunctionApplication FunctionApplicationInstance(
        TFunctionName functionName,
        StaticExpression<TFunctionName> arguments) =>
        new(functionName, arguments);

    /// <summary>
    /// Analog to <see cref="Expression.List"/>.
    /// A list expression contains a list of subexpressions.
    /// </summary>
    public record List
        : StaticExpression<TFunctionName>
    {
        private readonly int _slimHashCode;

        /// <inheritdoc/>
        public override int SubexpressionCount { get; }

        /// <summary>
        /// The list of subexpressions.
        /// </summary>
        public IReadOnlyList<StaticExpression<TFunctionName>> Items { get; }

        internal List(IReadOnlyList<StaticExpression<TFunctionName>> items)
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
            public IReadOnlyList<StaticExpression<TFunctionName>> Items { get; }

            internal readonly int SlimHashCode;

            public ListStruct(IReadOnlyList<StaticExpression<TFunctionName>> items)
            {
                Items = items;

                SlimHashCode = ComputeHashCode(items);
            }

            public override int GetHashCode()
            {
                return SlimHashCode;
            }

            public static int ComputeHashCode(IReadOnlyList<StaticExpression<TFunctionName>> items)
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
        : StaticExpression<TFunctionName>
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
        : StaticExpression<TFunctionName>
    {
        private readonly int _slimHashCode;

        /// <summary>
        /// The expression evaluated to determine which branch to take.
        /// </summary>
        public StaticExpression<TFunctionName> Condition { get; }

        /// <summary>
        /// The expression evaluated if the condition does not evaluate to true.
        /// </summary>
        public StaticExpression<TFunctionName> FalseBranch { get; }

        /// <summary>
        /// The expression evaluated if the condition evaluates to true.
        /// </summary>
        public StaticExpression<TFunctionName> TrueBranch { get; }

        /// <inheritdoc/>
        public override int SubexpressionCount { get; }

        internal Conditional(
            StaticExpression<TFunctionName> condition,
            StaticExpression<TFunctionName> falseBranch,
            StaticExpression<TFunctionName> trueBranch)
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
            public StaticExpression<TFunctionName> Condition { get; }

            public StaticExpression<TFunctionName> FalseBranch { get; }

            public StaticExpression<TFunctionName> TrueBranch { get; }

            internal readonly int SlimHashCode;

            public ConditionalStruct(
                StaticExpression<TFunctionName> condition,
                StaticExpression<TFunctionName> falseBranch,
                StaticExpression<TFunctionName> trueBranch)
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
                StaticExpression<TFunctionName> condition,
                StaticExpression<TFunctionName> falseBranch,
                StaticExpression<TFunctionName> trueBranch)
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
        : StaticExpression<TFunctionName>
    {
        /// <summary>
        /// The name of the kernel function to be applied.
        /// </summary>
        public string Function { get; }

        /// <summary>
        /// Input for the kernel function.
        /// </summary>
        public StaticExpression<TFunctionName> Input { get; }

        /// <inheritdoc/>
        public override int SubexpressionCount { get; }

        /// <summary>
        /// Creates a new instance of a kernel application.
        /// </summary>
        /// <param name="function">Name of the kernel function.</param>
        /// <param name="input">Input expression.</param>
        internal KernelApplication(
            string function,
            StaticExpression<TFunctionName> input)
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
    /// Corresponds to the <see cref="Expression.Environment"/>.
    /// </summary>
    public sealed record Environment :
        StaticExpression<TFunctionName>
    {
        /// <summary>
        /// Always returns zero, as an <see cref="Environment"/> expression does not contain any subexpressions.
        /// </summary>
        public override int SubexpressionCount { get; } = 0;
    }

    /// <summary>
    /// Application of a user-defined function to a list of argument expressions.
    /// </summary>
    public record FunctionApplication
        : StaticExpression<TFunctionName>
    {
        private readonly int _slimHashCode;

        /// <summary>
        /// The function name.
        /// </summary>
        public TFunctionName FunctionName { get; }

        /// <summary>
        /// Arguments passed to the function.
        /// </summary>
        public StaticExpression<TFunctionName> Arguments { get; }

        /// <inheritdoc/>
        public override int SubexpressionCount { get; } = 0;

        /// <summary>
        /// Create a new <see cref="FunctionApplication"/>.
        /// </summary>
        /// <param name="functionName">The function name.</param>
        /// <param name="arguments">Argument expressions.</param>
        public FunctionApplication(
            TFunctionName functionName,
            StaticExpression<TFunctionName> arguments)
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
    public sealed record AlwaysCrash
        : StaticExpression<TFunctionName>
    {
        /// <summary>
        /// Always zero; this node has no children.
        /// </summary>
        public override int SubexpressionCount => 0;
    }

    /// <summary>
    /// Derives the implicit parameter list for a function from its body.
    /// </summary>
    /// <param name="functionBody">The static expression representing the function body.</param>
    /// <returns>
    /// A list of distinct paths to the environment expression used anywhere in <paramref name="functionBody"/>.
    /// </returns>
    /// <remarks>
    /// This helper walks the expression tree, collects all parameter reference nodes, removes duplicates while preserving
    /// structural equality, and produces a stable ordering by comparing the integer path components.
    /// The resulting sequence is suitable for generating a canonical function header such as
    ///   f param_1_0 param_1_2 = ...
    /// ensuring deterministic naming and ordering across builds and platforms.
    /// </remarks>
    public static IReadOnlyList<IReadOnlyList<int>> ImplicitFunctionParameterList(
        StaticExpression<TFunctionName> functionBody)
    {
        var collectedPaths = new HashSet<IReadOnlyList<int>>(IntPathEqualityComparer.Instance);

        bool ShouldSkipDescendants(StaticExpression<TFunctionName> expr)
        {
            if (StaticExpression<TFunctionName>.TryParseAsPathToExpression(expr, pathEndExpression: EnvironmentInstance) is { } path)
            {
                collectedPaths.Add(path);

                return true;
            }

            return false;
        }

        foreach (var _ in EnumerateAllDescendants(functionBody, skipDescendants: ShouldSkipDescendants))
        {
            // Intentionally empty loop body; we only care about the side effect of collecting paths.
        }

        return
            [.. collectedPaths.Order(IntPathComparer.Instance)];
    }

    /// <summary>
    /// Enumerate the expression and all of its descendants in depth-first order.
    /// The sequence starts with <paramref name="expression"/> itself.
    /// </summary>
    /// <param name="expression">Root expression to traverse.</param>
    /// <param name="skipDescendants">
    /// Optional predicate to determine whether to skip traversing the descendants of a given node.
    /// If the function returns true for a node, its descendants are not enumerated.
    /// </param>
    /// <returns>A depth-first traversal sequence over the expression tree.</returns>
    public static IEnumerable<StaticExpression<TFunctionName>> EnumerateAllDescendants(
        StaticExpression<TFunctionName> expression,
        Func<StaticExpression<TFunctionName>, bool>? skipDescendants)
    {
        var stack = new Stack<StaticExpression<TFunctionName>>();

        stack.Push(expression);

        while (stack.Count > 0)
        {
            var current = stack.Pop();

            yield return current;

            if (skipDescendants?.Invoke(current) is true)
            {
                continue;
            }

            switch (current)
            {
                case Literal:
                    break;

                case Environment:
                    break;

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

                case FunctionApplication functionApp:
                    stack.Push(functionApp.Arguments);
                    break;

                case AlwaysCrash:
                    break;

                default:
                    throw new NotSupportedException($"Unknown static expression type: {current.GetType()}");
            }
        }
    }

    /// <summary>
    /// Recursively maps each user-defined function identifier (<see cref="FunctionApplication"/>) from
    /// <typeparamref name="TFunctionName"/> to <typeparamref name="TOut"/> while preserving the
    /// structure of the expression tree and leaving all other node kinds unchanged.
    /// </summary>
    /// <typeparam name="TOut">The target type of the mapped function identifiers.</typeparam>
    /// <param name="expression">Root static expression to transform.</param>
    /// <param name="mapIdentifier">Mapping function applied to every function identifier encountered.</param>
    /// <returns>A new <see cref="StaticExpression{TOut}"/> tree with mapped identifiers.</returns>
    /// <remarks>
    /// Traversal is depth-first. Kernel function applications, literals, parameter references, conditionals,
    /// list nodes, and <see cref="AlwaysCrash"/> nodes are recreated only as needed. Argument subtrees are
    /// transformed recursively. This operation is pure and does not mutate the original tree.
    /// </remarks>
    public static StaticExpression<TOut> MapFunctionIdentifier<TOut>(
        StaticExpression<TFunctionName> expression,
        Func<TFunctionName, TOut> mapIdentifier)
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

            AlwaysCrash =>
            new StaticExpression<TOut>.AlwaysCrash(),

            _ =>
            throw new NotSupportedException($"Unknown static expression type: {expression.GetType()}")
        };
    }

    /// <summary>
    /// Attempts to parse a chain of nested <c>head</c> (and optional <c>skip</c>) kernel applications
    /// representing a path from an arbitrary starting expression back to a designated <paramref name="pathEndExpression"/>.
    /// </summary>
    /// <param name="expression">The root expression that potentially encodes a path.</param>
    /// <param name="pathEndExpression">The terminal expression that marks the end (origin) of the path (typically <see cref="EnvironmentInstance"/>).</param>
    /// <returns>
    /// A list of integer offsets describing the path if <paramref name="expression"/> is a valid encoding; otherwise <c>null</c>.
    /// Offsets are ordered from the outermost application (closest to <paramref name="expression"/>) to the innermost
    /// (closest to <paramref name="pathEndExpression"/>). Zero denotes a direct <c>head</c> without an intervening <c>skip</c>.
    /// </returns>
    /// <remarks>
    /// The recognized shape is a (possibly empty) sequence of kernel applications with the outermost node being
    /// a <c>head</c>. A non-zero offset step is represented as <c>head (skip N NEXT)</c>, while a zero offset is
    /// simply <c>head NEXT</c>. Parsing stops successfully when <paramref name="pathEndExpression"/> is reached.
    /// Any deviation from this pattern causes the method to return <c>null</c>.
    /// </remarks>
    public static IReadOnlyList<int>? TryParseAsPathToExpression(
        StaticExpression<TFunctionName> expression,
        StaticExpression<TFunctionName> pathEndExpression)
    {
        var path = new List<int>();

        var current = expression;

        while (true)
        {
            if (current == pathEndExpression)
            {
                path.Reverse();

                return path;
            }

            if (current is not KernelApplication outerKernelApp ||
                outerKernelApp.Function is not nameof(KernelFunction.head))
            {
                return null;
            }

            var next = outerKernelApp.Input;
            var offset = 0;

            if (next is KernelApplication innerKernelApp &&
                innerKernelApp.Function is nameof(KernelFunction.skip) &&
                innerKernelApp.Input is List skipList &&
                skipList.Items.Count is 2 &&
                skipList.Items[0] is Literal skipLiteral &&
                KernelFunction.SignedIntegerFromValueRelaxed(skipLiteral.Value) is { } skipInteger)
            {
                offset = (int)skipInteger;
                next = skipList.Items[1];
            }

            path.Add(offset);
            current = next;
        }
    }

    /// <summary>
    /// Attempts to partially resolve a path (as used by <see cref="BuildPathToExpression"/>) against an already materialized
    /// static expression tree. Leading offsets that index directly into existing <see cref="List"/> nodes are consumed and
    /// the traversal advances into the corresponding child. Once the traversal can no longer continue (because the current
    /// node is not a <see cref="List"/> or an offset is out of range), the remaining suffix of the path is re-encoded as
    /// kernel applications (using <c>head</c> / <c>skip</c>) starting from the last resolved expression.
    /// </summary>
    /// <param name="path">Full sequence of integer offsets describing a path toward <paramref name="pathEndExpression"/>.</param>
    /// <param name="pathEndExpression">The starting (deepest) expression the path ultimately points to; usually the function environment.</param>
    /// <returns>
    /// A static expression equivalent to one built from <paramref name="path"/> by <see cref="BuildPathToExpression"/>, but
    /// with an initial segment already resolved into existing list nodes where possible. This shortens the resulting encoded path
    /// and avoids redundant kernel applications.
    /// </returns>
    /// <remarks>
    /// Example: If <paramref name="pathEndExpression"/> is already a list <c>[A, B, C]</c> and <paramref name="path"/> is <c>[2, 0, 1]</c>,
    /// the function first advances to item index 2 (<c>C</c>) and then encodes the remaining <c>[0,1]</c> relative to <c>C</c>.
    /// </remarks>
    public static StaticExpression<TFunctionName> BuildReducedPathToExpression(
        IReadOnlyList<int> path,
        StaticExpression<TFunctionName> pathEndExpression)
    {
        var current = pathEndExpression;

        var reducedPathIndex = 0;

        while (reducedPathIndex < path.Count)
        {
            var nextOffset = path[reducedPathIndex];

            if (current is not List listExpr)
            {
                break;
            }

            if (nextOffset < 0 || nextOffset >= listExpr.Items.Count)
            {
                break;
            }

            current = listExpr.Items[nextOffset];
            ++reducedPathIndex;
        }

        return
            BuildPathToExpression([.. path.Skip(reducedPathIndex)], current);
    }

    /// <summary>
    /// Builds an expression that encodes a path as a chain of <c>head</c> (and optional <c>skip</c>) kernel applications
    /// terminating at <paramref name="pathEndExpression"/>.
    /// </summary>
    /// <param name="path">Sequence of integer offsets describing steps from the start toward the end expression. Order must match that returned by <see cref="TryParseAsPathToExpression"/>.</param>
    /// <param name="pathEndExpression">The terminal expression that the constructed path should reference (typically <see cref="EnvironmentInstance"/>).</param>
    /// <returns>An expression representing the encoded path.</returns>
    /// <remarks>
    /// Each offset <c>n</c> produces either <c>head CURRENT</c> (when <c>n == 0</c>) or
    /// <c>head (skip n CURRENT)</c> (when <c>n != 0</c>). Offsets are applied sequentially in the order they appear
    /// in <paramref name="path"/>. Passing the resulting expression to <see cref="TryParseAsPathToExpression"/>
    /// with the same <paramref name="pathEndExpression"/> will yield the original <paramref name="path"/>.
    /// </remarks>
    public static StaticExpression<TFunctionName> BuildPathToExpression(
        IReadOnlyList<int> path,
        StaticExpression<TFunctionName> pathEndExpression)
    {
        var current = pathEndExpression;

        for (var i = 0; i < path.Count; ++i)
        {
            var offset = path[i];

            if (offset is 0)
            {
                current =
                    StaticExpression<TFunctionName>.KernelApplicationInstance(
                        nameof(KernelFunction.head),
                        current);
            }
            else
            {
                current =
                    StaticExpression<TFunctionName>.KernelApplicationInstance(
                        nameof(KernelFunction.head),
                        StaticExpression<TFunctionName>.KernelApplicationInstance(
                            nameof(KernelFunction.skip),
                            StaticExpression<TFunctionName>.ListInstance(
                                [StaticExpression<TFunctionName>.LiteralInstance(IntegerEncoding.EncodeSignedInteger(offset)),
                                 current])));
            }
        }

        return current;
    }
}
