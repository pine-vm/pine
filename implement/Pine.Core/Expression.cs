using Pine.Core.CommonEncodings;
using Pine.Core.Json;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
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
    public abstract int SubexpressionCount { get; }

    /// <summary>
    /// True if the expression itself or any its subexpressions is of type <see cref="Environment"/>.
    /// </summary>
    public abstract bool ReferencesEnvironment { get; }

    /// <summary>
    /// Instance of the <see cref="Environment"/> expression type.
    /// </summary>
    public static readonly Expression EnvironmentInstance = new Environment();

    /// <summary>
    /// A <see cref="List"/> expression containing zero items.
    /// </summary>
    public static readonly List EmptyList = new([]);

    private static readonly FrozenDictionary<(string function, Expression input), KernelApplication> s_kernelApplicationInstances =
        ReusedKernelApplicationInstancesSource()
        .ToFrozenDictionary(
            keySelector: instance => (instance.Function, instance.Input),
            elementSelector: instance => instance);


    /// <summary>
    /// For a given expression, checks if an equivalent instance is available in the cache of reused instances.
    /// Returns the reused instance if available, otherwise returns the input expression.
    /// </summary>
    public static Expression EnsureReuseInstanceGeneral(Expression expression)
    {
        if (expression is KernelApplication kernelApplicationExpression)
        {
            if (s_kernelApplicationInstances is { } reusedKernelApplications)
            {
                if (reusedKernelApplications.TryGetValue(
                    (kernelApplicationExpression.Function, kernelApplicationExpression.Input),
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
    /// Returns an instance of the <see cref="Literal"/> expression type for the given <see cref="PineValue"/>.
    /// 
    /// Checks the cache of reused instances to avoid allocating a new instance if an equivalent instance is already available.
    /// </summary>
    public static Literal LiteralInstance(PineValue pineValue)
    {
        if (ReusedInstances.Instance.LiteralExpressions?.TryGetValue(pineValue, out var literal) ?? false)
            return literal;

        return new Literal(pineValue);
    }

    /// <summary>
    /// Returns an instance of the <see cref="List"/> expression type for the given list of subexpressions.
    /// 
    /// Checks the cache of reused instances to avoid allocating a new instance if an equivalent instance is already available.
    /// </summary>
    public static List ListInstance(IReadOnlyList<Expression> items)
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
    public static Conditional ConditionalInstance(
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
    /// Instance of the <see cref="KernelApplication"/> variant, applying a kernel function.
    /// </summary>
    public static KernelApplication KernelApplicationInstance(
        string function,
        Expression input)
    {
        if (s_kernelApplicationInstances is { } reusedKernelApplications)
        {
            if (reusedKernelApplications.TryGetValue((function, input), out var reusedInstance))
                return reusedInstance;
        }

        var newInstance = new KernelApplication(function, input);

        return newInstance;
    }

    /// <summary>
    /// A literal expression only contains a concrete value.
    /// </summary>
    public record Literal(
        PineValue Value)
        : Expression
    {
        /// <summary>
        /// Always returns zero, as a <see cref="Literal"/> expression does not contain any subexpressions.
        /// </summary>
        public override int SubexpressionCount { get; } = 0;

        /// <summary>
        /// Always returns false, as a <see cref="Literal"/> expression does not contain any subexpressions.
        /// </summary>
        public override bool ReferencesEnvironment { get; } = false;

        /// <inheritdoc/>
        public override string ToString()
        {
            string? valueInterpretationString = null;

            if (Value is PineValue.BlobValue blobValue && 0 < blobValue.Bytes.Length)
            {
                if (KernelFunction.SignedIntegerFromValueRelaxed(Value) is { } intValue)
                {
                    valueInterpretationString = "int " + intValue;
                }

                if (StringEncoding.StringFromBlobValue(blobValue.Bytes).IsOkOrNull() is { } strValue)
                {
                    valueInterpretationString = "string \"" + strValue + "\"";
                }
            }

            return
                nameof(Literal) +
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
        public override int SubexpressionCount { get; }

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; } = false;

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

            for (var i = 0; i < Items.Count; ++i)
            {
                SubexpressionCount += Items[i].SubexpressionCount;

                if (Items[i].ReferencesEnvironment)
                    ReferencesEnvironment = true;
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

        /// <inheritdoc/>
        public override string ToString()
        {
            return
                ToShortString(
                    itemsCount: Items.Count,
                    subexpressionCount: SubexpressionCount,
                    referencesEnvironment: ReferencesEnvironment);
        }

        internal static string ToShortString(
            int itemsCount,
            int subexpressionCount,
            bool referencesEnvironment)
        {
            if (itemsCount is 0)
                return nameof(EmptyList);

            return
                nameof(List) +
                " { ItemsCount = " + CommandLineInterface.FormatIntegerForDisplay(itemsCount) +
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
    /// A Parse-and-Eval expression allows for the instantiation of a new environment and the evaluation of an encoded expression.
    /// 
    /// Similar to 'eval' features in other languages, this expression enables meta programming by treating data as a program.
    /// 
    /// Program execution crashes if the value obtained via the <see cref="Encoded"/> expression part
    /// is not a valid encoding of a Pine expression.
    /// </summary>
    public record ParseAndEval
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
        public override int SubexpressionCount { get; }

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; }

        /// <summary>
        /// Creates a new instance of a Parse-and-Eval expression.
        /// </summary>
        public ParseAndEval(
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
        }

        /// <inheritdoc/>
        override public int GetHashCode() =>
            _slimHashCode;

        /// <inheritdoc/>
        public virtual bool Equals(ParseAndEval? other)
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
    /// Application of a kernel function to an input expression.
    /// 
    /// Kernel functions are the built-in functions of the Pine language.
    /// 
    /// Kernel functions never crash the program, but may return default values in case of nonsensical input.
    /// Therefore it remains the responsibility of the caller to add branches for error messages as needed.
    /// </summary>
    public record KernelApplication
        : Expression
    {
        private readonly int _slimHashCode;

        /// <summary>
        /// The name of the kernel function to be applied.
        /// </summary>
        public string Function { get; }

        /// <summary>
        /// Input for the kernel function.
        /// </summary>
        public Expression Input { get; }

        /// <inheritdoc/>
        public override int SubexpressionCount { get; }

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; }

        /// <summary>
        /// Creates a new instance of a kernel application.
        /// </summary>
        internal KernelApplication(
            string function,
            Expression input)
        {
            function = PopularValues.InternIfKnown(function);

            _slimHashCode = HashCode.Combine(function, input);

            Function = function;
            Input = input;

            SubexpressionCount = input.SubexpressionCount + 1;
            ReferencesEnvironment = input.ReferencesEnvironment;
        }

        /// <inheritdoc/>
        public virtual bool Equals(KernelApplication? other)
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
        public override int SubexpressionCount { get; }

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; }

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
        public override int SubexpressionCount { get; } = 0;

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; } = true;
    }

    /// <summary>
    /// A string tag expression attaches a string tag to an expression.
    /// These tags are not meant to be observable by the program, but to help with inspection, tracing and profiling.
    /// </summary>
    public record StringTag
        : Expression
    {
        /// <summary>
        /// The tag string.
        /// This tag is not meant to be observable by the program, but to help with inspection, tracing and profiling.
        /// </summary>
        public string Tag { get; }

        /// <summary>
        /// Fully determines the value returned by this expression.
        /// </summary>
        public Expression Tagged { get; }

        /// <inheritdoc/>
        public override int SubexpressionCount { get; }

        /// <inheritdoc/>
        public override bool ReferencesEnvironment { get; }

        internal readonly int SlimHashCode;

        /// <summary>
        /// Creates a new instance of a string tag expression.
        /// </summary>
        public StringTag(
            string tag,
            Expression tagged)
        {
            Tag = tag;
            Tagged = tagged;

            SubexpressionCount = tagged.SubexpressionCount + 1;
            ReferencesEnvironment = tagged.ReferencesEnvironment;

            SlimHashCode = HashCode.Combine(tag, tagged);
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            SlimHashCode;

        /// <inheritdoc/>
        public virtual bool Equals(StringTag? other)
        {
            if (ReferenceEquals(other, this))
                return true;

            if (other is null)
                return false;

            return
                other.SlimHashCode == SlimHashCode &&
                other.Tag == Tag &&
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

                    KernelApplication kernelApplicationExpression =>
                    [kernelApplicationExpression.Input],

                    ParseAndEval parseAndEval =>
                    [parseAndEval.Environment, parseAndEval.Encoded],

                    StringTag stringTagExpression =>
                    [stringTagExpression.Tagged],

                    Literal =>
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
                case Literal:
                    break;

                case List list:
                    for (var i = 0; i < list.Items.Count; i++)
                    {
                        stack.Push(list.Items[i]);
                    }
                    break;

                case ParseAndEval parseAndEvaluate:

                    stack.Push(parseAndEvaluate.Encoded);
                    stack.Push(parseAndEvaluate.Environment);

                    break;

                case KernelApplication kernelApplication:

                    stack.Push(kernelApplication.Input);

                    break;

                case Conditional conditional:

                    stack.Push(conditional.Condition);

                    if (!skipConditionalBranches)
                    {
                        stack.Push(conditional.FalseBranch);
                        stack.Push(conditional.TrueBranch);
                    }

                    break;

                case StringTag stringTag:

                    stack.Push(stringTag.Tagged);

                    break;

                default:
                    throw new NotImplementedException(
                        "Unknown expression type: " + expression.GetType().FullName);
            }
        }
    }

    private static IEnumerable<KernelApplication> ReusedKernelApplicationInstancesSource()
    {
        yield return KernelApplicationInstance(
            nameof(KernelFunction.head),
            EnvironmentInstance);

        for (var i = 0; i <= 16; ++i)
        {
            var skipZero =
                KernelApplicationInstance(
                    nameof(KernelFunction.skip),
                    ListInstance(
                        [
                        LiteralInstance(IntegerEncoding.EncodeSignedInteger(i)),
                        KernelApplicationInstance(
                            nameof(KernelFunction.head),
                            EnvironmentInstance)
                        ]));

            yield return skipZero;

            yield return KernelApplicationInstance(
                nameof(KernelFunction.head),
                skipZero);

            var skipOne =
                KernelApplicationInstance(
                    nameof(KernelFunction.skip),
                    ListInstance(
                        [
                        LiteralInstance(IntegerEncoding.EncodeSignedInteger(i)),
                        KernelApplicationInstance(
                            nameof(KernelFunction.skip),
                            ListInstance(
                                [
                                LiteralInstance(IntegerEncoding.EncodeSignedInteger(1)),
                                KernelApplicationInstance(
                                    nameof(KernelFunction.head),
                                    EnvironmentInstance)
                                ]))
                        ]));

            yield return skipOne;

            yield return KernelApplicationInstance(
                nameof(KernelFunction.head),
                skipOne);
        }
    }
}
