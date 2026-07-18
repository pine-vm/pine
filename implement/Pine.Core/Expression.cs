using Pine.Core.CodeGen;
using Pine.Core.CommonEncodings;
using Pine.Core.Json;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json.Serialization;

namespace Pine.Core;

public delegate Result<string, PineValue> EvalExprDelegate(Expression expression, PineValue environment);


/// <summary>
/// An expression in the Pine language.
/// 
/// For a listing of expression types in the Pine language,
/// see <see href="https://github.com/pine-vm/pine/blob/1f6e378ff376d809e7029a376b3a562d990fde7f/implement/pine/Elm/elm-compiler/src/Pine.elm#L53-L70"/>>
/// </summary>
[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record Expression
{
    /// <summary>
    /// Number of subexpressions contained in this expression.
    /// </summary>
    public abstract long SubexpressionCount { get; }

    /// <summary>
    /// True if the expression itself or any its subexpressions is of type <see cref="Environment"/>.
    /// </summary>
    public abstract bool ReferencesEnvironment { get; }

    /// <summary>
    /// Number of subexpressions contained in this expression that are of type <see cref="Eval"/>, including the expression itself if it is an eval expression.
    /// </summary>
    public abstract long EvalCount { get; }

    /// <summary>
    /// Number of condition expressions (<see cref="Conditional"/>) contained in this expression, including the expression itself if it is a conditional.
    /// </summary>
    public abstract long ConditionCount { get; }

    /// <summary>
    /// Number of built-in expressions (<see cref="Builtin"/>) contained in this expression, including the expression itself if it is a built-in.
    /// </summary>
    public abstract long BuiltinCount { get; }

    /// <summary>
    /// Maximum depth found in the tree.
    /// Zero for leaf nodes like literals or environment reference.
    /// </summary>
    public abstract int MaxDepth { get; }

    /// <summary>
    /// Instance of the <see cref="Environment"/> expression type.
    /// </summary>
    public static readonly Expression EnvironmentInstance = new Environment();

    /// <summary>
    /// A <see cref="List"/> expression containing zero items.
    /// </summary>
    public static readonly List EmptyList = new([]);

    private static readonly Litral s_literalEmptyList =
        LitralInst(PineValue.EmptyList);

    private static readonly Litral s_literalEmptyBlob =
        LitralInst(PineValue.EmptyBlob);

    private static readonly IReadOnlyList<Litral> s_literalsBlobSingleByte =
        [.. Enumerable.Range(0, 0x100).Select(i => LitralInst(PineValue.BlobSingleByte((byte)i)))];

    private static readonly FrozenDictionary<PineValue, Litral> s_literalOtherInstances =
        ReusedLiteralOtherInstancesSource()
        .ToFrozenDictionary(
            literalValue => literalValue,
            LitralInst);

    private static readonly FrozenDictionary<(string function, Expression input), Builtin> s_builtinInstances =
        ReusedBuiltinInstancesSource()
        .ToFrozenDictionary(
            keySelector: instance => (instance.Function, instance.Input),
            elementSelector: instance => instance);


    /// <summary>
    /// For a given expression, checks if an equivalent instance is available in the cache of reused instances.
    /// Returns the reused instance if available, otherwise returns the input expression.
    /// </summary>
    public static Expression EnsureReuseInstanceGeneral(Expression expression)
    {
        if (expression is Builtin builtinExpression)
        {
            if (s_builtinInstances is { } reusedBuiltinInstances)
            {
                if (reusedBuiltinInstances.TryGetValue(
                    (builtinExpression.Function, builtinExpression.Input),
                    out var reusedInstance))
                {
                    return reusedInstance;
                }
            }
        }

        if (ReusedInstances.Instance?.Expressions?.TryGetValue(expression, out var reused) ?? false)
        {
            return reused;
        }

        return expression;
    }

    /// <summary>
    /// Returns an instance of the <see cref="Litral"/> expression type for the given <see cref="PineValue"/>.
    /// 
    /// Checks the cache of reused instances to avoid allocating a new instance if an equivalent instance is already available.
    /// </summary>
    public static Litral LitralInst(PineValue literalValue)
    {
        if (literalValue is PineValue.BlobValue blobValue)
        {
            if (blobValue.Bytes.Length is 0 && s_literalEmptyBlob is { } reusedEmptyBlobLiteral)
            {
                return reusedEmptyBlobLiteral;
            }

            if (blobValue.Bytes.Length is 1 && s_literalsBlobSingleByte is { } reusedSingleByteLiterals)
            {
                return reusedSingleByteLiterals[blobValue.Bytes.Span[0]];
            }
        }

        if (literalValue is PineValue.ListValue listValue)
        {
            if (listValue.Items.Length is 0 && s_literalEmptyList is { } reusedEmptyListLiteral)
            {
                return reusedEmptyListLiteral;
            }
        }

        if (s_literalOtherInstances is { } reusedLiterals)
        {
            if (reusedLiterals.TryGetValue(literalValue, out var reusedInstance))
            {
                return reusedInstance;
            }
        }

        if (ReusedInstances.Instance.LiteralExpressions?.TryGetValue(literalValue, out var literal) ?? false)
            return literal;

        return new Litral(literalValue);
    }

    /// <summary>
    /// Returns an instance of the <see cref="List"/> expression type for the given list of subexpressions.
    /// 
    /// Checks the cache of reused instances to avoid allocating a new instance if an equivalent instance is already available.
    /// </summary>
    public static List ListInst(IReadOnlyList<Expression> items)
    {
        if (items.Count is 0)
            return EmptyList;

        var listKey = new List.ListStruct(items);

        if (ReusedInstances.Instance.ListExpressions is { } reusedListExpressions)
        {
            if (reusedListExpressions.TryGetValue(listKey, out var list))
                return list;
        }

        return new List(listKey);
    }

    /// <summary>
    /// Returns an instance of the <see cref="Conditional"/> expression type for the given condition and branches.
    /// 
    /// Checks the cache of reused instances to avoid allocating a new instance if an equivalent instance is already available.
    /// </summary>
    public static Conditional ConditionalInst(
        Expression condition,
        Expression falseBranch,
        Expression trueBranch)
    {
        var conditionalStruct =
            new Conditional.ConditionalStruct(condition, falseBranch, trueBranch);

        if (ReusedInstances.Instance.ConditionalExpressions is { } reusedConditionalExpressions)
        {
            if (reusedConditionalExpressions.TryGetValue(conditionalStruct, out var conditional))
                return conditional;
        }

        return new Conditional(conditionalStruct);
    }

    /// <summary>
    /// Instance of the <see cref="Builtin"/> variant, applying a built-in function.
    /// </summary>
    public static Builtin BuiltinInst(
        string function,
        Expression input)
    {
        if (s_builtinInstances is { } reusedBuiltinInstances)
        {
            if (reusedBuiltinInstances.TryGetValue((function, input), out var reusedInstance))
                return reusedInstance;
        }

        var newInstance = new Builtin(function, input);

        return newInstance;
    }

    /// <summary>
    /// A literal expression only contains a concrete value.
    /// </summary>
    public record Litral
        : Expression
    {
        internal Litral(PineValue value)
        {
            Value = value;
        }

        /// <summary>
        /// The concrete value of the literal expression.
        /// </summary>
        public PineValue Value { get; }

        /// <summary>
        /// Always returns zero, as a <see cref="Litral"/> expression does not contain any subexpressions.
        /// </summary>
        public override long SubexpressionCount { get; } = 0;

        /// <summary>
        /// Always returns false, as a <see cref="Litral"/> expression does not contain any subexpressions.
        /// </summary>
        public override bool ReferencesEnvironment { get; } = false;

        /// <summary>
        /// Always returns zero, as a <see cref="Litral"/> expression does not contain any subexpressions.
        /// </summary>
        public override long EvalCount { get; } = 0;

        /// <inheritdoc/>
        public override long ConditionCount { get; } = 0;

        /// <inheritdoc/>
        public override long BuiltinCount { get; } = 0;

        /// <inheritdoc/>
        public override int MaxDepth { get; } = 0;

        /// <inheritdoc/>
        public override string ToString()
        {
            string? valueInterpretationString = null;

            if (Value is PineValue.BlobValue blobValue && 0 < blobValue.Bytes.Length)
            {
                if (BuiltinFunction.SignedIntegerFromValueRelaxed(Value) is { } intValue)
                {
                    valueInterpretationString = "int " + intValue;
                }

                if (StringEncoding.StringFromBlobValue(blobValue.Bytes).IsOkOrNull() is { } strValue)
                {
                    valueInterpretationString = "string \"" + strValue + "\"";
                }
            }

            return
                nameof(Litral) +
                " { Value = " + Value.ToString() +
                (valueInterpretationString is not null ? " (" + valueInterpretationString + ")" : "") +
                " }";
        }
    }

    /// <summary>
    /// A list expression contains a list of subexpressions.
    /// </summary>
    public record List
        : Expression
    {
        private readonly int _slimHashCode;

        /// <ihneritdoc/>
        public override long SubexpressionCount { get; }

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; } = false;

        /// <inheritdoc/>
        public override long EvalCount { get; } = 0;

        /// <inheritdoc/>
        public override long ConditionCount { get; } = 0;

        /// <inheritdoc/>
        public override long BuiltinCount { get; } = 0;

        /// <inheritdoc/>
        public override int MaxDepth { get; } = 0;

        private int _subtreeListItemsCount = 1;

        /// <summary>
        /// The list of subexpressions.
        /// </summary>
        public IReadOnlyList<Expression> Items { get; }

        internal List(IReadOnlyList<Expression> items)
            :
            this(new ListStruct(items))
        {
        }

        internal List(ListStruct listKey)
        {
            Items = listKey.Items;

            _slimHashCode = listKey.SlimHashCode;

            SubexpressionCount = Items.Count;

            var maxDepth = 0;

            for (var i = 0; i < Items.Count; ++i)
            {
                var item = Items[i];

                SubexpressionCount += item.SubexpressionCount;

                if (item.ReferencesEnvironment)
                    ReferencesEnvironment = true;

                EvalCount += item.EvalCount;

                ConditionCount += item.ConditionCount;

                BuiltinCount += item.BuiltinCount;

                var itemDepth = item.MaxDepth + 1;

                if (itemDepth > maxDepth)
                    maxDepth = itemDepth;

                if (item is List itemList)
                {
                    _subtreeListItemsCount += itemList._subtreeListItemsCount;
                }
            }

            MaxDepth = maxDepth;
        }

        /// <inheritdoc/>
        public virtual bool Equals(List? other)
        {
            return Equal(this, other);
        }

        private static bool Equal(List left, List? right)
        {
            if (ReferenceEquals(left, right))
                return true;

            if (right is null)
                return false;

            if (10 < left._subtreeListItemsCount)
            {
                /*
                 * Compare nested lists iteratively using an explicit stack instead of recursion.
                 *
                 * Deeply nested list expressions would otherwise recurse once per nesting level and
                 * overflow the call stack (observed in practice for thousands of levels). Pushing the
                 * pairs of nested lists onto an explicit work stack keeps the call stack depth constant.
                 *
                 * Note: nested-list items are dispatched directly to this comparison (via the stack)
                 * instead of calling the virtual Equals method. This must be an 'else' relative to the
                 * generic 'Equals' below. Otherwise a nested list item would be compared twice (once here
                 * and once via 'leftItem.Equals'), doubling the work at every nesting level and yielding
                 * exponential (O(2^depth)) comparisons for deeply nested, structurally-equal lists.
                 * */

                var stack = new Stack<(List left, List right)>();

                stack.Push((left, right));

                while (stack.Count > 0)
                {
                    var (currentLeft, currentRight) = stack.Pop();

                    if (ReferenceEquals(currentLeft, currentRight))
                        continue;

                    if (!(currentLeft._slimHashCode == currentRight._slimHashCode))
                        return false;

                    if (!(currentLeft.SubexpressionCount == currentRight.SubexpressionCount))
                        return false;

                    if (currentLeft.Items.Count != currentRight.Items.Count)
                        return false;

                    for (var i = 0; i < currentLeft.Items.Count; ++i)
                    {
                        var leftItem = currentLeft.Items[i];
                        var rightItem = currentRight.Items[i];

                        if (leftItem is List leftList)
                        {
                            if (rightItem is not List rightList)
                                return false;

                            stack.Push((leftList, rightList));
                        }
                        else if (!leftItem.Equals(rightItem))
                        {
                            return false;
                        }
                    }
                }

                return true;
            }

            if (left._slimHashCode != right._slimHashCode)
                return false;

            if (left.SubexpressionCount != right.SubexpressionCount)
                return false;

            if (left.Items.Count != right.Items.Count)
                return false;

            for (var i = 0; i < left.Items.Count; ++i)
            {
                var leftItem = left.Items[i];
                var rightItem = right.Items[i];

                if (leftItem is List leftList)
                {
                    if (rightItem is not List rightList)
                        return false;

                    if (!Equal(leftList, rightList))
                        return false;

                }
                else if (!leftItem.Equals(rightItem))
                {
                    return false;
                }
            }

            return true;
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            _slimHashCode;

        /// <inheritdoc/>
        public override string ToString()
        {
            return
                ToShortString(
                    itemsCount: Items.Count,
                    maxDepth: MaxDepth,
                    subexpressionCount: SubexpressionCount,
                    referencesEnvironment: ReferencesEnvironment);
        }

        internal static string ToShortString(
            int itemsCount,
            int maxDepth,
            long subexpressionCount,
            bool referencesEnvironment)
        {
            if (itemsCount is 0)
                return nameof(EmptyList);

            return
                nameof(List) +
                " { ItemsCount = " + CommandLineInterface.FormatIntegerForDisplay(itemsCount) +
                ", MaxDepth = " + CommandLineInterface.FormatIntegerForDisplay(maxDepth) +
                ", SubexpressionCount = " + CommandLineInterface.FormatIntegerForDisplay(subexpressionCount) +
                ", ReferencesEnvironment = " + referencesEnvironment +
                " }";
        }

        internal readonly record struct ListStruct
        {
            public IReadOnlyList<Expression> Items { get; }

            internal readonly int SlimHashCode;

            public ListStruct(IReadOnlyList<Expression> items)
            {
                Items = items;

                SlimHashCode = ComputeHashCode(items);
            }

            /// <inheritdoc/>
            public override int GetHashCode()
            {
                return SlimHashCode;
            }

            public static int ComputeHashCode(IReadOnlyList<Expression> items)
            {
                var hashCode = new HashCode();

                for (var i = 0; i < items.Count; ++i)
                {
                    hashCode.Add(items[i].GetHashCode());
                }

                return hashCode.ToHashCode();
            }

            /// <inheritdoc/>
            public bool Equals(ListStruct other)
            {
                if (other.SlimHashCode != SlimHashCode)
                    return false;

                if (other.Items.Count != Items.Count)
                    return false;

                for (var i = 0; i < Items.Count; ++i)
                {
                    if (!other.Items[i].Equals(Items[i]))
                        return false;
                }

                return true;
            }
        }
    }

    /// <summary>
    /// An Eval expression allows for the instantiation of a new environment and the evaluation of an encoded expression.
    /// 
    /// Similar to 'eval' features in other languages, this expression enables meta programming by treating data as a program.
    /// 
    /// Program execution crashes if the value obtained via the <see cref="Encoded"/> expression part
    /// is not a valid encoding of a Pine expression.
    /// </summary>
    public record Eval
        : Expression
    {
        private readonly int _slimHashCode;

        /// <summary>
        /// Subexpression that is evaluated to obtain the value to be parsed as a Pine expression.
        /// </summary>
        public Expression Encoded { get; }

        /// <summary>
        /// Subexpression that is evaluated to obtain the environment value for the evaluation of the encoded expression.
        /// </summary>
        new public Expression Environment { get; }

        /// <inheritdoc/>
        public override long SubexpressionCount { get; }

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; }

        /// <summary>
        /// Always returns true, as this expression is itself a <see cref="Eval"/>.
        /// </summary>
        public override long EvalCount { get; }

        /// <inheritdoc/>
        public override long ConditionCount { get; } = 0;

        /// <inheritdoc/>
        public override long BuiltinCount { get; } = 0;

        /// <inheritdoc/>
        public override int MaxDepth { get; } = 0;

        /// <summary>
        /// Creates a new instance of a Parse-and-Eval expression.
        /// </summary>
        public Eval(
            Expression encoded,
            Expression environment)
        {
            _slimHashCode = HashCode.Combine(encoded, environment);

            Encoded = encoded;
            Environment = environment;

            SubexpressionCount =
                encoded.SubexpressionCount + environment.SubexpressionCount + 2;

            ReferencesEnvironment =
                encoded.ReferencesEnvironment || environment.ReferencesEnvironment;

            ConditionCount =
                encoded.ConditionCount + environment.ConditionCount;

            BuiltinCount =
                encoded.BuiltinCount + environment.BuiltinCount;

            EvalCount =
                encoded.EvalCount + environment.EvalCount + 1;

            MaxDepth =
                Math.Max(encoded.MaxDepth, environment.MaxDepth) + 1;
        }

        /// <inheritdoc/>
        override public int GetHashCode() =>
            _slimHashCode;

        /// <inheritdoc/>
        public virtual bool Equals(Eval? other)
        {
            if (other is not { } notNull)
                return false;

            if (ReferenceEquals(this, notNull))
                return true;

            return
                _slimHashCode == notNull._slimHashCode &&
                Encoded.Equals(notNull.Encoded) &&
                Environment.Equals(notNull.Environment);
        }
    }

    /// <summary>
    /// Application of a built-in function to an input expression.
    /// 
    /// Built-in functions never crash the program, but may return default values in case of nonsensical input.
    /// Therefore it remains the responsibility of the caller to add branches for error messages as needed.
    /// </summary>
    public record Builtin
        : Expression
    {
        private readonly int _slimHashCode;

        /// <summary>
        /// The name of the built-in function to be applied.
        /// </summary>
        public string Function { get; }

        /// <summary>
        /// Input for the built-in function.
        /// </summary>
        public Expression Input { get; }

        /// <inheritdoc/>
        public override long SubexpressionCount { get; }

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; }

        /// <inheritdoc/>
        public override long EvalCount { get; }

        /// <inheritdoc/>
        public override long ConditionCount { get; } = 0;

        /// <inheritdoc/>
        public override long BuiltinCount { get; } = 0;

        /// <inheritdoc/>
        public override int MaxDepth { get; } = 0;

        /// <summary>
        /// Creates a new instance of a built-in application.
        /// </summary>
        internal Builtin(
            string function,
            Expression input)
        {
            function = PopularValues.InternIfKnown(function);

            _slimHashCode = HashCode.Combine(function, input);

            Function = function;
            Input = input;

            SubexpressionCount = input.SubexpressionCount + 1;
            ReferencesEnvironment = input.ReferencesEnvironment;
            EvalCount = input.EvalCount;
            ConditionCount = input.ConditionCount;
            BuiltinCount = input.BuiltinCount + 1;
            MaxDepth = input.MaxDepth + 1;
        }

        /// <inheritdoc/>
        public virtual bool Equals(Builtin? other)
        {
            if (other is not { } notNull)
                return false;

            if (ReferenceEquals(this, notNull))
                return true;

            return
                _slimHashCode == notNull._slimHashCode &&
                notNull.Function == Function &&
                (ReferenceEquals(notNull.Input, Input) || notNull.Input.Equals(Input));
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            return _slimHashCode;
        }
    }

    /// <summary>
    /// A conditional expression contains a condition and two branches.
    /// </summary>
    public record Conditional
        : Expression
    {
        private readonly int _slimHashCode;

        /// <summary>
        /// The expression evaluated to determine which branch to take.
        /// </summary>
        public Expression Condition { get; }

        /// <summary>
        /// The expression evaluated if the condition does not evaluate to true.
        /// </summary>
        public Expression FalseBranch { get; }

        /// <summary>
        /// The expression evaluated if the condition evaluates to true.
        /// </summary>
        public Expression TrueBranch { get; }

        /// <inheritdoc/>
        public override long SubexpressionCount { get; }

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; }

        /// <inheritdoc/>
        public override long EvalCount { get; }

        /// <inheritdoc/>
        public override long ConditionCount { get; } = 0;

        /// <inheritdoc/>
        public override long BuiltinCount { get; } = 0;

        /// <inheritdoc/>
        public override int MaxDepth { get; } = 0;

        internal Conditional(
            Expression condition,
            Expression falseBranch,
            Expression trueBranch)
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

            ReferencesEnvironment =
                Condition.ReferencesEnvironment ||
                FalseBranch.ReferencesEnvironment ||
                TrueBranch.ReferencesEnvironment;

            EvalCount =
                Condition.EvalCount +
                FalseBranch.EvalCount +
                TrueBranch.EvalCount;

            ConditionCount =
                Condition.ConditionCount +
                FalseBranch.ConditionCount +
                TrueBranch.ConditionCount + 1;

            BuiltinCount =
                Condition.BuiltinCount +
                FalseBranch.BuiltinCount +
                TrueBranch.BuiltinCount;

            MaxDepth =
                Math.Max(
                    Math.Max(Condition.MaxDepth, FalseBranch.MaxDepth),
                    TrueBranch.MaxDepth) + 1;
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

        internal readonly record struct ConditionalStruct
        {
            public Expression Condition { get; }

            public Expression FalseBranch { get; }

            public Expression TrueBranch { get; }

            internal readonly int SlimHashCode;

            public ConditionalStruct(
                Expression condition,
                Expression falseBranch,
                Expression trueBranch)
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
                Expression condition,
                Expression falseBranch,
                Expression trueBranch)
            {
                return HashCode.Combine(condition, falseBranch, trueBranch);
            }
        }
    }

    /// <summary>
    /// The environment expression is the only way to parameterize an expression with an environment.
    /// 
    /// Since the environment is a <see cref="PineValue"/>,
    /// it can be used to pass any data to an expression and package any number of values in lists.
    /// </summary>
    public record Environment : Expression
    {
        /// <inheritdoc/>
        public override long SubexpressionCount { get; } = 0;

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; } = true;

        /// <inheritdoc/>
        public override long EvalCount { get; } = 0;

        /// <inheritdoc/>
        public override long ConditionCount { get; } = 0;

        /// <inheritdoc/>
        public override long BuiltinCount { get; } = 0;

        /// <inheritdoc/>
        public override int MaxDepth { get; } = 0;
    }

    /// <summary>
    /// A label expression attaches a label to an expression.
    /// These labels are not meant to be observable by the program, but to help with inspection, tracing and profiling.
    /// </summary>
    public record Label
        : Expression
    {
        /// <summary>
        /// The tag string.
        /// This tag is not meant to be observable by the program, but to help with inspection, tracing and profiling.
        /// </summary>
        public string Tag { get; }

        /// <summary>
        /// The Pine value representing the label.
        /// </summary>
        [JsonIgnore]
        public PineValue LabelValue { get; }

        /// <summary>
        /// Fully determines the value returned by this expression.
        /// </summary>
        public Expression Tagged { get; }

        /// <inheritdoc/>
        public override long SubexpressionCount { get; }

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; }

        /// <inheritdoc/>
        public override long EvalCount { get; }

        /// <inheritdoc/>
        public override long ConditionCount { get; }

        /// <inheritdoc/>
        public override long BuiltinCount { get; }

        /// <inheritdoc/>
        public override int MaxDepth { get; } = 0;

        private readonly int _slimHashCode;

        /// <summary>
        /// Creates a new instance of a string tag expression.
        /// </summary>
        public Label(
            string tag,
            Expression tagged)
            : this(StringEncoding.ValueFromString(tag), tagged)
        {
        }

        /// <summary>
        /// Creates a new tag expression with an arbitrary Pine value as its label.
        /// </summary>
        public Label(
            PineValue labelValue,
            Expression tagged)
        {
            LabelValue = labelValue;

            Tag =
                StringEncoding.StringFromValue(labelValue).IsOkOrNull() ??
                labelValue.ToString();

            Tagged = tagged;

            SubexpressionCount = tagged.SubexpressionCount + 1;
            ReferencesEnvironment = tagged.ReferencesEnvironment;
            EvalCount = tagged.EvalCount;
            ConditionCount = tagged.ConditionCount;
            BuiltinCount = tagged.BuiltinCount;
            MaxDepth = tagged.MaxDepth + 1;

            _slimHashCode = HashCode.Combine(labelValue, tagged);
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            _slimHashCode;

        /// <inheritdoc/>
        public virtual bool Equals(Label? other)
        {
            if (ReferenceEquals(other, this))
                return true;

            if (other is null)
                return false;

            return
                other._slimHashCode == _slimHashCode &&
                other.LabelValue == LabelValue &&
                other.Tagged == Tagged;
        }
    }

    /// <summary>
    /// Collects all unique expressions contained in the given roots and their descendants.
    /// </summary>
    public static IReadOnlySet<Expression> CollectAllComponentsFromRoots(
        IEnumerable<Expression> roots)
    {
        var components = new HashSet<Expression>();

        var stack = new Stack<Expression>(roots);

        while (stack.TryPop(out var expression))
        {
            if (components.Contains(expression))
                continue;

            components.Add(expression);

            var childItems =
                expression switch
                {
                    List listExpression =>
                    listExpression.Items,

                    Conditional conditionalExpression =>
                    [
                    conditionalExpression.Condition,
                    conditionalExpression.FalseBranch,
                    conditionalExpression.TrueBranch
                    ],

                    Builtin builtinExpression =>
                    [builtinExpression.Input],

                    Eval evalExpression =>
                    [evalExpression.Environment, evalExpression.Encoded],

                    Label stringTagExpression =>
                    [stringTagExpression.Tagged],

                    Litral =>
                    [],

                    Environment =>
                    [],

                    _ =>
                    throw new NotImplementedException(
                        "Unexpected expression type: " + expression.GetType())
                };

            foreach (var item in childItems)
            {
                stack.Push(item);
            }
        }

        return components;
    }

    /// <summary>
    /// Enumerates the given expression and all its descendants.
    /// </summary>
    public static IEnumerable<Expression> EnumerateSelfAndDescendants(
        Expression expression) =>
        EnumerateSelfAndDescendants(
            expression,
            skipDescendants: null,
            skipConditionalBranches: false);

    /// <summary>
    /// Enumerates the given expression and all its descendants.
    /// </summary>
    public static IEnumerable<Expression> EnumerateSelfAndDescendants(
        Expression rootExpression,
        Func<Expression, bool>? skipDescendants,
        bool skipConditionalBranches)
    {
        var stack = new Stack<Expression>([rootExpression]);

        while (stack.TryPop(out var expression))
        {
            yield return expression;

            if (skipDescendants?.Invoke(expression) ?? false)
                continue;

            switch (expression)
            {
                case Environment:
                case Litral:
                    break;

                case List list:
                    for (var i = 0; i < list.Items.Count; i++)
                    {
                        stack.Push(list.Items[i]);
                    }

                    break;

                case Eval evalExpression:

                    stack.Push(evalExpression.Encoded);
                    stack.Push(evalExpression.Environment);

                    break;

                case Builtin builtin:

                    stack.Push(builtin.Input);

                    break;

                case Conditional conditional:

                    stack.Push(conditional.Condition);

                    if (!skipConditionalBranches)
                    {
                        stack.Push(conditional.FalseBranch);
                        stack.Push(conditional.TrueBranch);
                    }

                    break;

                case Label stringTag:

                    stack.Push(stringTag.Tagged);

                    break;

                default:
                    throw new NotImplementedException(
                        "Unknown expression type: " + expression.GetType().FullName);
            }
        }
    }

    private static IEnumerable<PineValue> ReusedLiteralOtherInstancesSource()
    {
        foreach (var popularString in PopularValues.PopularStrings)
        {
            yield return StringEncoding.ValueFromString(popularString);
        }

        for (var i = -100; i <= 400; i++)
        {
            yield return IntegerEncoding.EncodeSignedInteger(i);
        }
    }

    private static IEnumerable<Builtin> ReusedBuiltinInstancesSource()
    {
        for (var i = 0; i < 16; ++i)
        {
            var level0Skip =
                i is 0
                ?
                EnvironmentInstance
                :
                BuiltinInst(
                    nameof(BuiltinFunction.skip),
                    ListInst(
                        [
                        LitralInst(IntegerEncoding.EncodeSignedInteger(i)),
                        EnvironmentInstance
                        ]));

            if (level0Skip is Builtin skipZeroBuiltin)
                yield return skipZeroBuiltin;

            var level0SkipHead =
                BuiltinInst(
                    nameof(BuiltinFunction.head),
                    level0Skip);

            yield return level0SkipHead;

            for (var j = 0; j < 4; ++j)
            {
                var level1Skip =
                    j is 0
                    ?
                    level0SkipHead
                    :
                    BuiltinInst(
                        nameof(BuiltinFunction.skip),
                        ListInst(
                            [
                            LitralInst(IntegerEncoding.EncodeSignedInteger(j)),
                            level0SkipHead
                            ]));

                if (j is not 0)
                    yield return level1Skip;

                var level1SkipHead =
                    BuiltinInst(
                        nameof(BuiltinFunction.head),
                        level1Skip);

                yield return level1SkipHead;

                for (var k = 0; k < 4; ++k)
                {
                    var level2Skip =
                        k is 0
                        ?
                        level1SkipHead
                        :
                        BuiltinInst(
                            nameof(BuiltinFunction.skip),
                            ListInst(
                                [
                                LitralInst(IntegerEncoding.EncodeSignedInteger(k)),
                                level1SkipHead
                                ]));

                    if (k is not 0)
                        yield return level2Skip;

                    var level2SkipHead =
                        BuiltinInst(
                            nameof(BuiltinFunction.head),
                            level2Skip);

                    yield return level2SkipHead;
                }
            }
        }

        for (var i = 0; i < 2; ++i)
        {
            {
                var concatPrependPlusSign =
                    BuiltinInst(
                        nameof(BuiltinFunction.concat),
                        ListInst(
                            [
                            LitralInst(PineValue.BlobSingleByte(4)),
                            ExpressionBuilder.BuildExpressionForPathInExpression([i], EnvironmentInstance)
                            ]));

                yield return concatPrependPlusSign;
            }

            for (var j = 0; j < 2; ++j)
            {
                var concatPrependPlusSign =
                    BuiltinInst(
                        nameof(BuiltinFunction.concat),
                        ListInst(
                            [
                            LitralInst(PineValue.BlobSingleByte(4)),
                            ExpressionBuilder.BuildExpressionForPathInExpression([i, j], EnvironmentInstance)
                            ]));

                yield return concatPrependPlusSign;
            }
        }
    }
}
