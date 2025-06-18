using Pine.Core.PopularEncodings;
using System;
using System.Collections.Frozen;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;

namespace Pine.Core.Elm;

/// <summary>
/// Corresponding to the type ElmValue in ElmInteractive.elm
/// </summary>
public abstract record ElmValue
{
    /*

    type ElmValue
        = ElmList (List ElmValue)
        | ElmChar Char
        | ElmInteger BigInt.BigInt
        | ElmString String
        | ElmTag String (List ElmValue)
        | ElmRecord (List ( String, ElmValue ))
        | ElmBytes Bytes
        | ElmFloat BigInt.BigInt BigInt.BigInt
        | ElmInternal String

     * */

    /// <summary>
    /// The number of ElmValue nodes contained within this ElmValue.
    /// </summary>
    abstract public int ContainedNodesCount { get; }

    /// <summary>
    /// Corresponding to the Elm expression 'True'.
    /// </summary>
    public static readonly ElmValue TrueValue = TagInstance("True", []);

    /// <summary>
    /// Corresponding to the Elm expression 'False'.
    /// </summary>
    public static readonly ElmValue FalseValue = TagInstance("False", []);

    /// <inheritdoc/>
    override public string ToString() =>
        GetType().Name + " : " + RenderAsElmExpression(this).expressionString;

    /// <summary>
    /// Tag name used to represent Elm records when encoding as <see cref="PineValue"/>.
    /// </summary>
    public const string ElmRecordTypeTagName = "Elm_Record";

    /// <summary>
    /// Tag name used to represent instances of `Bytes.Bytes` from Elm when encoding as <see cref="PineValue"/>.
    /// </summary>
    public const string ElmBytesTypeTagName = "Elm_Bytes";

    /// <summary>
    /// Tag name used to represent Elm strings when encoding as <see cref="PineValue"/>.
    /// This corresponds to the `String` type in Elm.
    /// </summary>
    public const string ElmStringTypeTagName = "String";

    /// <summary>
    /// Tag name used to represent Elm sets when encoding as <see cref="PineValue"/>.
    /// This corresponds to the `Set.Set` type from the `elm/core` library.
    /// </summary>
    public const string ElmSetTypeTagName = "Set_elm_builtin";

    /// <summary>
    /// Tag name used to represent an empty Elm dictionary when encoding as <see cref="PineValue"/>.
    /// This corresponds to an empty `Dict.Dict` from the `elm/core` library, which is represented by `RBEmpty_elm_builtin` internally.
    /// </summary>
    public const string ElmDictEmptyTagName = "RBEmpty_elm_builtin";

    /// <summary>
    /// Tag name used to represent a non-empty Elm dictionary (a node in a red-black tree) when encoding as <see cref="PineValue"/>.
    /// This corresponds to a non-empty `Dict.Dict` from the `elm/core` library, which is represented by `RBNode_elm_builtin` internally.
    /// </summary>
    public const string ElmDictNotEmptyTagName = "RBNode_elm_builtin";

    /// <summary>
    /// Tag name used to represent Elm floating-point numbers when encoding as <see cref="PineValue"/>.
    /// This corresponds to the `Float` type in Elm.
    /// </summary>
    public const string ElmFloatTypeTagName = "Elm_Float";

    /// <summary>
    /// Represents the 'Elm_Record' tag name as a <see cref="PineValue"/> using the default string encoding.
    /// </summary>
    public static readonly PineValue ElmRecordTypeTagNameAsValue =
        StringEncoding.ValueFromString(ElmRecordTypeTagName);

    /// <summary>
    /// Represents the 'Elm_Record' tag name as a <see cref="PineValue"/> using the 2024 string encoding.
    /// </summary>
    public static readonly PineValue ElmRecordTypeTagNameAsValue_2024 =
        StringEncoding.ValueFromString_2024(ElmRecordTypeTagName);

    /// <summary>
    /// Represents the 'Elm_Bytes' tag name as a <see cref="PineValue"/> using the default string encoding.
    /// </summary>
    public static readonly PineValue ElmBytesTypeTagNameAsValue =
        StringEncoding.ValueFromString(ElmBytesTypeTagName);

    /// <summary>
    /// Represents the 'Elm_Bytes' tag name as a <see cref="PineValue"/> using the 2024 string encoding.
    /// </summary>
    public static readonly PineValue ElmBytesTypeTagNameAsValue_2024 =
        StringEncoding.ValueFromString_2024(ElmBytesTypeTagName);

    /// <summary>
    /// Represents the 'String' tag name as a <see cref="PineValue"/> using the default string encoding.
    /// </summary>
    public static readonly PineValue ElmStringTypeTagNameAsValue =
        StringEncoding.ValueFromString(ElmStringTypeTagName);

    /// <summary>
    /// Represents the 'String' tag name as a <see cref="PineValue"/> using the 2024 string encoding.
    /// </summary>
    public static readonly PineValue ElmStringTypeTagNameAsValue_2024 =
        StringEncoding.ValueFromString_2024(ElmStringTypeTagName);

    /// <summary>
    /// Represents the 'Set_elm_builtin' tag name as a <see cref="PineValue"/> using the default string encoding.
    /// </summary>
    public static readonly PineValue ElmSetTypeTagNameAsValue =
        StringEncoding.ValueFromString(ElmSetTypeTagName);

    /// <summary>
    /// Represents the 'Set_elm_builtin' tag name as a <see cref="PineValue"/> using the 2024 string encoding.
    /// </summary>
    public static readonly PineValue ElmSetTypeTagNameAsValue_2024 =
        StringEncoding.ValueFromString_2024(ElmSetTypeTagName);

    /// <summary>
    /// Represents the 'RBEmpty_elm_builtin' tag name as a <see cref="PineValue"/> using the default string encoding.
    /// This corresponds to an empty dictionary of the `Dict.Dict` type from the Elm core library.
    /// </summary>
    public static readonly PineValue ElmDictEmptyTagNameAsValue =
        StringEncoding.ValueFromString(ElmDictEmptyTagName);

    /// <summary>
    /// Represents the 'RBEmpty_elm_builtin' tag name as a <see cref="PineValue"/> using the 2024 string encoding.
    /// This corresponds to an empty dictionary of the `Dict.Dict` type from the Elm core library.
    /// </summary>
    public static readonly PineValue ElmDictEmptyTagNameAsValue_2024 =
        StringEncoding.ValueFromString_2024(ElmDictEmptyTagName);

    /// <summary>
    /// Represents the 'RBNode_elm_builtin' tag name as a <see cref="PineValue"/> using the default string encoding.
    /// This corresponds to a non-empty dictionary of the `Dict.Dict` type from the Elm core library.
    /// </summary>
    public static readonly PineValue ElmDictNotEmptyTagNameAsValue =
        StringEncoding.ValueFromString(ElmDictNotEmptyTagName);

    /// <summary>
    /// Represents the 'RBNode_elm_builtin' tag name as a <see cref="PineValue"/> using the 2024 string encoding.
    /// This corresponds to a non-empty dictionary of the `Dict.Dict` type from the Elm core library.
    /// </summary>
    public static readonly PineValue ElmDictNotEmptyTagNameAsValue_2024 =
        StringEncoding.ValueFromString_2024(ElmDictNotEmptyTagName);

    /// <summary>
    /// Represents the 'Elm_Float' tag name as a <see cref="PineValue"/> using the default string encoding.
    /// </summary>
    public static readonly PineValue ElmFloatTypeTagNameAsValue =
        StringEncoding.ValueFromString(ElmFloatTypeTagName);

    /// <summary>
    /// Represents the 'Elm_Float' tag name as a <see cref="PineValue"/> using the 2024 string encoding.
    /// </summary>
    public static readonly PineValue ElmFloatTypeTagNameAsValue_2024 =
        StringEncoding.ValueFromString_2024(ElmFloatTypeTagName);

    /// <summary>
    /// Represents an empty Elm dictionary.
    /// </summary>
    public static readonly ElmValue EmptyDict =
        TagInstance(ElmDictEmptyTagName, []);

    /// <summary>
    /// Creates an <see cref="ElmList"/> instance from a read-only list of <see cref="ElmValue"/> elements.
    /// </summary>
    /// <param name="Elements">The elements of the list.</param>
    /// <returns>A new <see cref="ElmList"/> instance.</returns>
    public static ElmValue ListInstance(IReadOnlyList<ElmValue> Elements) =>
        new ElmList(Elements);

    /// <summary>
    /// Creates an Elm tuple (represented as an <see cref="ElmList"/>) from two <see cref="ElmValue"/> items.
    /// </summary>
    /// <param name="Item1">The first item of the tuple.</param>
    /// <param name="Item2">The second item of the tuple.</param>
    /// <returns>An <see cref="ElmList"/> representing the tuple.</returns>
    public static ElmValue TupleInstance(ElmValue Item1, ElmValue Item2) =>
        ListInstance([Item1, Item2]);

    /// <summary>
    /// Creates an <see cref="ElmInteger"/> instance from a <see cref="System.Numerics.BigInteger"/>.
    /// This method may return a cached instance for frequently used integer values.
    /// </summary>
    /// <param name="Value">The integer value.</param>
    /// <returns>An <see cref="ElmInteger"/> instance.</returns>
    public static ElmValue Integer(System.Numerics.BigInteger Value)
    {
        if (Value < 0)
        {
            if (-Value < ReusedIntegerInstancesNegative.Span.Length)
            {
                return ReusedIntegerInstancesNegative.Span[(int)-Value];
            }
        }
        else if (Value < ReusedIntegerInstancesPositive.Length)
        {
            return ReusedIntegerInstancesPositive.Span[(int)Value];
        }

        return new ElmInteger(Value);
    }

    /// <summary>
    /// Creates an <see cref="ElmString"/> instance from a <see cref="string"/>.
    /// This method may return a cached instance for frequently used string values.
    /// </summary>
    /// <param name="Value">The string value.</param>
    /// <returns>An <see cref="ElmString"/> instance.</returns>
    public static ElmValue StringInstance(string Value) =>
        ReusedStringInstances?.TryGetValue(Value, out var reusedInstance) ?? false && reusedInstance is not null ?
        reusedInstance
        :
        new ElmString(Value);

    /// <summary>
    /// Creates an <see cref="ElmTag"/> instance.
    /// This method may return a cached instance for frequently used tags.
    /// </summary>
    /// <param name="TagName">The name of the tag.</param>
    /// <param name="Arguments">The arguments associated with the tag.</param>
    /// <returns>An <see cref="ElmTag"/> instance.</returns>
    public static ElmTag TagInstance(string TagName, IReadOnlyList<ElmValue> Arguments)
    {
        var tagStruct =
            new ElmTag.ElmTagStruct(TagName, Arguments);

        if (ReusedInstances.Instance.ElmTagValues?.TryGetValue(tagStruct, out var reusedInstance) ?? false)
        {
            return reusedInstance;
        }

        return new ElmTag(tagStruct);
    }

    /// <summary>
    /// Creates an <see cref="ElmChar"/> instance from an integer representing a Unicode code point.
    /// This method may return a cached instance for frequently used character values.
    /// </summary>
    /// <param name="Value">The Unicode code point of the character.</param>
    /// <returns>An <see cref="ElmChar"/> instance.</returns>
    public static ElmValue CharInstance(int Value) =>
        Value < ReusedCharInstances.Length && 0 <= Value ?
        ReusedCharInstances.Span[Value]
        :
        new ElmChar(Value);

    private static readonly ReadOnlyMemory<ElmValue> ReusedIntegerInstancesNegative =
        Enumerable.Range(0, 10_000)
        .Select(i => Integer(-i))
        .ToArray();

    private static readonly ReadOnlyMemory<ElmValue> ReusedIntegerInstancesPositive =
        Enumerable.Range(0, 10_000)
        .Select(i => Integer(i))
        .ToArray();

    private static readonly FrozenDictionary<string, ElmValue> ReusedStringInstances =
        PopularValues.PopularStrings
        .ToFrozenDictionary(
            keySelector: s => s,
            elementSelector: StringInstance);

    private static readonly ReadOnlyMemory<ElmValue> ReusedCharInstances =
        Enumerable.Range(0, 10_000)
        .Select(CharInstance)
        .ToArray();

    /// <summary>
    /// An integer with unlimited precision, represented as a <see cref="System.Numerics.BigInteger"/>.
    /// </summary>
    /// <param name="Value"></param>
    public record ElmInteger(System.Numerics.BigInteger Value)
        : ElmValue
    {
        /// <inheritdoc/>
        override public int ContainedNodesCount { get; } = 0;

        /// <inheritdoc/>
        public override string ToString()
        {
            return GetType().Name + " : " + Value.ToString();
        }
    }

    /// <summary>
    /// Represents an Elm tagged union value, that is, a variant of a choice type.
    /// </summary>
    public record ElmTag
        : ElmValue
    {
        /// <summary>
        /// Gets the name of the tag.
        /// </summary>
        public string TagName { get; }

        /// <summary>
        /// Gets the list of arguments associated with the tag.
        /// </summary>
        public IReadOnlyList<ElmValue> Arguments { get; }

        readonly int slimHashCode;

        /// <inheritdoc/>
        override public int ContainedNodesCount { get; }

        internal ElmTag(
            string TagName,
            IReadOnlyList<ElmValue> Arguments)
            :
            this(new ElmTagStruct(TagName, Arguments))
        {
        }

        internal ElmTag(ElmTagStruct elmTagStruct)
        {
            TagName = elmTagStruct.TagName;
            Arguments = elmTagStruct.Arguments;

            slimHashCode = elmTagStruct.slimHashCode;

            ContainedNodesCount = 0;

            for (int i = 0; i < Arguments.Count; i++)
            {
                ContainedNodesCount += Arguments[i].ContainedNodesCount + 1;
            }
        }

        /// <inheritdoc/>
        public virtual bool Equals(ElmTag? otherTag)
        {
            if (ReferenceEquals(this, otherTag))
                return true;

            if (otherTag is null)
                return false;

            if (otherTag.slimHashCode != slimHashCode)
                return false;

            if (TagName != otherTag.TagName)
                return false;

            if (Arguments.Count != otherTag.Arguments.Count)
                return false;

            for (int i = 0; i < Arguments.Count; i++)
            {
                if (!Arguments[i].Equals(otherTag.Arguments[i]))
                    return false;
            }

            return true;
        }

        /// <inheritdoc/>
        override public int GetHashCode() =>
            slimHashCode;

        /// <summary>
        /// A struct representing the essential data of an <see cref="ElmTag"/>, used for efficient hashing and equality comparison.
        /// </summary>
        internal readonly record struct ElmTagStruct
        {
            /// <summary>
            /// Gets the name of the tag.
            /// </summary>
            public string TagName { get; }

            /// <summary>
            /// Gets the list of arguments associated with the tag.
            /// </summary>
            public IReadOnlyList<ElmValue> Arguments { get; }

            /// <summary>
            /// Gets the precomputed hash code.
            /// </summary>
            public readonly int slimHashCode;

            /// <summary>
            /// Initializes a new instance of the <see cref="ElmTagStruct"/> struct.
            /// </summary>
            /// <param name="TagName">The name of the tag.</param>
            /// <param name="Arguments">The arguments associated with the tag.</param>
            public ElmTagStruct(string TagName, IReadOnlyList<ElmValue> Arguments)
            {
                this.TagName = TagName;
                this.Arguments = Arguments;

                slimHashCode = ComputeHashCode(TagName, Arguments);
            }

            /// <inheritdoc/>
            public override int GetHashCode() =>
                slimHashCode;

            /// <summary>
            /// Determines whether the current <see cref="ElmTagStruct"/> is equal to another <see cref="ElmTagStruct"/>.
            /// </summary>
            /// <param name="otherTag">The other tag struct to compare with.</param>
            /// <returns><c>true</c> if the two tag structs are equal; otherwise, <c>false</c>.</returns>
            public bool Equals(ElmTagStruct otherTag)
            {
                if (otherTag.slimHashCode != slimHashCode)
                    return false;

                if (TagName != otherTag.TagName)
                    return false;

                if (Arguments.Count != otherTag.Arguments.Count)
                    return false;

                for (int i = 0; i < Arguments.Count; i++)
                {
                    if (!Arguments[i].Equals(otherTag.Arguments[i]))
                        return false;
                }

                return true;
            }

            /// <summary>
            /// Computes the hash code for an Elm tag given its name and arguments.
            /// </summary>
            /// <param name="TagName">The name of the tag.</param>
            /// <param name="Arguments">The arguments of the tag.</param>
            /// <returns>The computed hash code.</returns>
            public static int ComputeHashCode(
                string TagName,
                IReadOnlyList<ElmValue> Arguments)
            {
                var hashCode = new HashCode();

                hashCode.Add(TagName);

                for (int i = 0; i < Arguments.Count; i++)
                {
                    hashCode.Add(Arguments[i]);
                }

                return hashCode.ToHashCode();
            }

            public override string ToString() =>
                GetType().Name + " : " + ElmTagAsExpression(TagName, Arguments).expressionString;
        }

        /// <inheritdoc/>
        public override string ToString() =>
            GetType().Name + " : " + RenderAsElmExpression(this).expressionString;
    }

    /// <summary>
    /// Represents an Elm list value.
    /// </summary>
    public record ElmList
        : ElmValue
    {
        /// <summary>
        /// Gets the elements of the list.
        /// </summary>
        public IReadOnlyList<ElmValue> Elements { init; get; }


        internal readonly int slimHashCode;

        /// <inheritdoc/>
        public override int ContainedNodesCount { get; }

        /// <summary>
        /// Initializes a new instance of the <see cref="ElmList"/> class.
        /// </summary>
        /// <param name="Elements">The elements of the list.</param>
        public ElmList(IReadOnlyList<ElmValue> Elements)
        {
            this.Elements = Elements;

            slimHashCode = ComputeSlimHashCode(Elements);

            ContainedNodesCount = 0;

            for (int i = 0; i < Elements.Count; i++)
            {
                ContainedNodesCount += Elements[i].ContainedNodesCount + 1;
            }
        }

        /// <inheritdoc/>
        public virtual bool Equals(ElmList? otherList)
        {
            if (otherList is null)
                return false;

            if (Elements.Count != otherList.Elements.Count)
                return false;

            for (int i = 0; i < Elements.Count; i++)
            {
                if (!Elements[i].Equals(otherList.Elements[i]))
                    return false;
            }

            return true;
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            return slimHashCode;
        }

        private static int ComputeSlimHashCode(IReadOnlyList<ElmValue> elements)
        {
            var hashCode = new HashCode();

            for (int i = 0; i < elements.Count; i++)
            {
                hashCode.Add(elements[i].GetHashCode());
            }

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public override string ToString() =>
            GetType().Name + " : " + RenderAsElmExpression(this).expressionString;
    }

    /// <summary>
    /// Represents an Elm string value.
    /// </summary>
    /// <param name="Value">The string value.</param>
    public record ElmString(string Value)
        : ElmValue
    {
        /// <inheritdoc/>
        public override int ContainedNodesCount { get; } = 0;
    }

    /// <summary>
    /// Represents an Elm character value.
    /// </summary>
    /// <param name="Value">The Unicode code point of the character.</param>
    public record ElmChar(int Value)
        : ElmValue
    {
        /// <inheritdoc/>
        public override int ContainedNodesCount { get; } = 0;
    }

    /// <summary>
    /// Represents an Elm record value.
    /// </summary>
    /// <param name="Fields">The fields of the record, as a list of name-value pairs.</param>
    public record ElmRecord(IReadOnlyList<(string FieldName, ElmValue Value)> Fields)
        : ElmValue
    {
        /// <inheritdoc/>
        public override int ContainedNodesCount { get; } =
            Fields.Sum(field => field.Value.ContainedNodesCount) + Fields.Count;

        /// <inheritdoc/>
        public virtual bool Equals(ElmRecord? otherRecord)
        {
            if (otherRecord is null)
                return false;

            if (Fields.Count != otherRecord.Fields.Count)
                return false;

            for (int i = 0; i < Fields.Count; i++)
            {
                if (Fields[i].FieldName != otherRecord.Fields[i].FieldName)
                    return false;

                if (!Fields[i].Value.Equals(otherRecord.Fields[i].Value))
                    return false;
            }

            return true;
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            var hashCode = new HashCode();

            for (int i = 0; i < Fields.Count; i++)
            {
                hashCode.Add(Fields[i].FieldName);
                hashCode.Add(Fields[i].Value);
            }

            return hashCode.ToHashCode();
        }

        /// <inheritdoc/>
        public override string ToString() =>
            GetType().Name + " : " + RenderAsElmExpression(this).expressionString;

        /// <summary>
        /// Get the value of a field by name.
        /// Returns null if the record does not contain a field with the given name.
        /// </summary>
        public ElmValue? this[string fieldName] =>
            Fields.FirstOrDefault(field => field.FieldName == fieldName).Value;
    }

    /// <summary>
    /// Elm Bytes type from https://package.elm-lang.org/packages/elm/bytes/latest/
    /// </summary>
    public record ElmBytes(ReadOnlyMemory<byte> Value)
        : ElmValue
    {
        /// <summary>
        /// The number of contained nodes is always zero for 'Bytes' values.
        /// </summary>
        public override int ContainedNodesCount { get; } = 0;


        /// <inheritdoc/>
        public virtual bool Equals(ElmBytes? otherBytes)
        {
            if (otherBytes is null)
                return false;

            if (Value.Length != otherBytes.Value.Length)
                return false;

            return Value.Span.SequenceEqual(otherBytes.Value.Span);
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            Value.GetHashCode();
    }

    /// <summary>
    /// The Elm compiler included with Pine models the 'Float' type as a rational number,
    /// expressed as the quotient or fraction ⁠of two integers, a numerator and a denominator.
    /// <see href="https://en.wikipedia.org/wiki/Rational_number" />
    /// </summary>
    public record ElmFloat
        : ElmValue
    {
        /// <summary>
        /// The numerator part of the rational number.
        /// </summary>
        public System.Numerics.BigInteger Numerator { get; }

        /// <summary>
        /// The denominator part of the rational number.
        /// </summary>
        public System.Numerics.BigInteger Denominator { get; }

        private ElmFloat(
            System.Numerics.BigInteger Numerator,
            System.Numerics.BigInteger Denominator)
        {
            this.Numerator = Numerator;
            this.Denominator = Denominator;
        }

        /// <summary>
        /// Store components verbatim without normalization.
        /// </summary>
        public static ElmFloat NotNormalized(
            System.Numerics.BigInteger Numerator,
            System.Numerics.BigInteger Denominator) =>
            new(Numerator, Denominator);

        /// <summary>
        /// Normalize the numerator and denominator to a canonical representation:
        /// <list>
        /// <item>Shrink numerator and denominator if possible to the most compact representation.</item>
        /// <item>Ensure sign is always on the numerator part.</item>
        /// </list>
        /// </summary>
        public static ElmFloat Normalized(
            System.Numerics.BigInteger Numerator,
            System.Numerics.BigInteger Denominator)
        {
            var sign = Numerator.Sign * Denominator.Sign;

            var divisor =
                Numerator == 0 || Denominator == 0
                ?
                1
                :
                System.Numerics.BigInteger.GreatestCommonDivisor(Numerator, Denominator);

            return new ElmFloat(
                Numerator: System.Numerics.BigInteger.Abs(Numerator) / divisor * sign,
                Denominator: System.Numerics.BigInteger.Abs(Denominator) / divisor);
        }

        /// <summary>
        /// Convert from a .NET <see cref="double"/> to an Elm Float.
        /// </summary>
        public static ElmFloat Convert(double fromDouble)
        {
            if (Math.Floor(fromDouble) == fromDouble)
                return Normalized(new System.Numerics.BigInteger(fromDouble), 1);

            var abs = Math.Abs(fromDouble);

            var absString = abs.ToString(System.Globalization.CultureInfo.InvariantCulture);

            var separatorIndex = absString.IndexOf('.');

            var fractionalDigits = absString[(separatorIndex + 1)..];

            var denominator = Math.Pow(10, fractionalDigits.Length);

            var numeratorAbs = new System.Numerics.BigInteger(abs * denominator);

            return Normalized(
                Numerator: fromDouble < 0 ? -numeratorAbs : numeratorAbs,
                Denominator: new System.Numerics.BigInteger(denominator));
        }

        /// <inheritdoc/>
        override public int ContainedNodesCount { get; } = 0;
    }

    /// <summary>
    /// Represents an Elm internal value, which is not directly representable in Elm source code
    /// but used for internal purposes or to represent values from external systems.
    /// </summary>
    /// <param name="Value">The string representation of the internal value.</param>
    public record ElmInternal(string Value)
        : ElmValue
    {
        /// <inheritdoc/>
        public override int ContainedNodesCount { get; } = 0;
    }

    /// <summary>
    /// Build a text which is a valid Elm expression and evaluates to the given Elm value.
    /// <para />
    /// Besides the expression string, the returned tuple also contains a boolean value indicating
    /// if the given string needs to be enclosed in parentheses when used as an argument to a function application.
    /// For example, an expression like 'Just 4' does not need parentheses by itself,
    /// but needs to be enclosed in parentheses when used as an argument to a function application like 'f (Just 4)'.
    /// </summary>
    public static (string expressionString, bool needsParens) RenderAsElmExpression(
        ElmValue elmValue)
    {
        return
            elmValue switch
            {
                ElmInteger integer =>
                (integer.Value.ToString(), needsParens: false),

                ElmChar charValue =>
                ("'" + RenderCharAsElmExpression(charValue.Value) + "'", needsParens: false),

                ElmList list =>
                ElmListItemsLookLikeTupleItems(list.Elements) ?? false
                ?
                ("(" + string.Join(",", list.Elements.Select(item => RenderAsElmExpression(item).expressionString)) + ")",
                needsParens: false)
                :
                ("[" + string.Join(",", list.Elements.Select(item => RenderAsElmExpression(item).expressionString)) + "]",
                needsParens: false),

                ElmString stringValue =>
                ("\"" + stringValue.Value.Replace("\\", "\\\\").Replace("\"", "\\\"") + "\"", needsParens: false),

                ElmRecord record =>
                record.Fields.Count < 1
                ?
                ("{}", needsParens: false)
                :
                ("{ " + string.Join(", ", record.Fields.Select(field =>
                field.FieldName + " = " + RenderAsElmExpression(field.Value).expressionString)) + " }",
                needsParens: false),

                ElmTag tag =>
                ElmTagAsExpression(tag.TagName, tag.Arguments),

                ElmBytes bytes =>
                ("<" + bytes.Value.Length + " bytes>", needsParens: false),

                ElmFloat elmFloat =>
                (Convert.ToString(
                    (double)elmFloat.Numerator / (double)elmFloat.Denominator,
                    System.Globalization.CultureInfo.InvariantCulture),
                needsParens: false),

                ElmInternal internalValue =>
                ("<" + internalValue.Value + ">", needsParens: false),

                _ =>
                throw new NotImplementedException(
                    "Not implemented for value type: " + elmValue.GetType().FullName)
            };
    }

    /// <summary>
    /// Renders a character's Unicode code point as a string suitable for an Elm character literal.
    /// Handles special characters like newline, carriage return, and tab.
    /// </summary>
    /// <param name="charValue">The Unicode code point of the character.</param>
    /// <returns>A string representation of the character for use in an Elm expression.</returns>
    public static string RenderCharAsElmExpression(int charValue)
    {
        if (charValue is 10)
            return "\\n";

        if (charValue is 13)
            return "\\r";

        if (charValue is 9)
            return "\\t";

        return char.ConvertFromUtf32(charValue);
    }

    /// <summary>
    /// Renders an Elm tag as an Elm expression string.
    /// Handles special cases for Elm's built-in Set and Dict types to render them in a more readable format (e.g., Set.fromList [...], Dict.fromList [...]).
    /// </summary>
    /// <param name="tagName">The name of the tag.</param>
    /// <param name="arguments">The arguments of the tag.</param>
    /// <returns>A tuple containing the Elm expression string and a boolean indicating if parentheses are needed for function application.</returns>
    public static (string expressionString, bool needsParens) ElmTagAsExpression(
        string tagName,
        IReadOnlyList<ElmValue> arguments)
    {
        static string applyNeedsParens((string expressionString, bool needsParens) tuple) =>
            tuple.needsParens ? "(" + tuple.expressionString + ")" : tuple.expressionString;

        if (tagName is "Set_elm_builtin")
        {
            if (arguments.Count is 1)
            {
                var singleArgument = arguments[0];

                var singleArgumentDictToList = ElmValueDictToList(singleArgument);

                if (singleArgumentDictToList.Count is 0)
                    return ("Set.empty", needsParens: false);

                var setElements = singleArgumentDictToList.Select(field => field.key).ToList();

                return
                    ("Set.fromList [" + string.Join(",", setElements.Select(RenderAsElmExpression).Select(applyNeedsParens)) + "]",
                    needsParens: true);
            }
        }

        if (tagName is "RBEmpty_elm_builtin")
            return ("Dict.empty", needsParens: false);

        var dictToList =
            ElmValueDictToList(new ElmTag(tagName, arguments));

        if (dictToList.Count is 0)
        {
            var (needsParens, argumentsString) =
                arguments.Count switch
                {
                    0 =>
                    (false, ""),

                    _ =>
                    (true, " " + string.Join(" ", arguments.Select(RenderAsElmExpression).Select(applyNeedsParens)))
                };

            return (tagName + argumentsString, needsParens);
        }

        return
            ("Dict.fromList [" +
            string.Join(",",
            dictToList
            .Select(field => "(" + RenderAsElmExpression(field.key).expressionString + "," + RenderAsElmExpression(field.value).expressionString + ")")) + "]",
            needsParens: true);
    }

    /// <summary>
    /// Converts an Elm dictionary (represented as an <see cref="ElmValue"/>, typically an <see cref="ElmTag"/> with name "RBNode_elm_builtin" or "RBEmpty_elm_builtin")
    /// into a list of key-value pairs.
    /// </summary>
    /// <param name="dict">The Elm dictionary value.</param>
    /// <returns>A read-only list of key-value pairs from the dictionary.</returns>
    public static IReadOnlyList<(ElmValue key, ElmValue value)> ElmValueDictToList(ElmValue dict) =>
        ElmValueDictFoldr(
            (key, value, acc) => acc.Insert(0, (key, value)),
            ImmutableList<(ElmValue key, ElmValue value)>.Empty,
            dict);

    /// <summary>
    /// Performs a right fold (foldr) operation on a dictionary instance of the `Dict.Dict` type from the Elm core library.
    /// <para />
    /// Elm dictionaries are represented as red-black trees (<c>RBNode_elm_builtin</c> for non-empty, <c>RBEmpty_elm_builtin</c> for empty).
    /// This function recursively traverses the tree, applying the provided function to each key-value pair and an accumulator.
    /// </summary>
    /// <typeparam name="T">The type of the accumulator and the result.</typeparam>
    /// <param name="func">The function to apply to each key-value pair and the accumulator. It takes the key, value, and current accumulator value, and returns the new accumulator value.</param>
    /// <param name="aggregate">The initial value of the accumulator.</param>
    /// <param name="elmValue">The Elm dictionary value to fold over.</param>
    /// <returns>The final accumulated value after traversing the entire dictionary.</returns>
    public static T ElmValueDictFoldr<T>(Func<ElmValue, ElmValue, T, T> func, T aggregate, ElmValue elmValue)
    {
        if (elmValue is ElmTag elmTag && elmTag.TagName is "RBNode_elm_builtin" && elmTag.Arguments.Count is 5)
        {
            var key = elmTag.Arguments[1];
            var value = elmTag.Arguments[2];
            var left = elmTag.Arguments[3];
            var right = elmTag.Arguments[4];

            return ElmValueDictFoldr(func, func(key, value, ElmValueDictFoldr(func, aggregate, right)), left);
        }

        return aggregate;
    }

    /// <summary>
    /// Determines if the items in an Elm list look like they could form a tuple.
    /// <para />
    /// Since tuples from Elm are lowered to plain lists by the Elm compiler, we use heuristics to guess if a Pine list
    /// represents an Elm tuple.
    /// </summary>
    /// <param name="list">The list of <see cref="ElmValue"/> items.</param>
    /// <returns>
    /// <c>true</c> if the list items suggest a tuple (short list with differing item types).
    /// <c>false</c> if the list is too long or all items are definitely of the same type.
    /// <c>null</c> if it's ambiguous (e.g., types are not definitively same or different, like two lists).
    /// </returns>
    public static bool? ElmListItemsLookLikeTupleItems(IReadOnlyList<ElmValue> list)
    {
        if (3 < list.Count)
        {
            return false;
        }

        var areAllItemsEqual = AreElmValueListItemTypesEqual(list);

        if (areAllItemsEqual.HasValue)
            return !areAllItemsEqual.Value;

        return null;
    }

    /// <summary>
    /// Checks if all items in a list of <see cref="ElmValue"/> are of the same Elm type.
    /// </summary>
    /// <param name="list">The list of <see cref="ElmValue"/> items.</param>
    /// <returns>
    /// <c>true</c> if all items are determined to be of the same type.
    /// <c>false</c> if at least one pair of items is determined to be of different types.
    /// <c>null</c> if the type equality is ambiguous for any pair (e.g., comparing two lists or two records with the same field names but potentially different field types).
    /// </returns>
    public static bool? AreElmValueListItemTypesEqual(IReadOnlyList<ElmValue> list)
    {
        if (list.Count <= 1)
            return true;

        var firstElementType = list[0];

        for (int i = 1; i < list.Count; i++)
        {
            var comparisonResult = AreElmValueTypesEqual(firstElementType, list[i]);

            if (comparisonResult is false)
                return false;

            if (comparisonResult is null)
                return null;
        }

        return true;
    }

    /// <summary>
    /// Compares two <see cref="ElmValue"/> instances to determine if they are of the same Elm type.
    /// </summary>
    /// <param name="valueA">The first Elm value.</param>
    /// <param name="valueB">The second Elm value.</param>
    /// <returns>
    /// Returns <c>true</c> if both values are of the same concrete Elm type (e.g., both are ElmInteger, both ElmChar, both ElmString).
    /// Returns <c>false</c> if the types are definitively different (e.g., ElmInteger and ElmString), or if they are records with different field names.
    /// Returns <c>null</c> if the type equality is ambiguous. This occurs for:
    /// - Two <see cref="ElmList"/> instances (as their element types would need further inspection).
    /// - Two <see cref="ElmRecord"/> instances with the same field names (as their field value types would need further inspection).
    /// - Two <see cref="ElmTag"/> instances (as their constructor names and argument types would need further inspection).
    /// - Two <see cref="ElmInternal"/> instances.
    /// </returns>
    public static bool? AreElmValueTypesEqual(
        ElmValue valueA,
        ElmValue valueB)
    {
        if (valueA is ElmInteger && valueB is ElmInteger)
            return true;

        if (valueA is ElmChar && valueB is ElmChar)
            return true;

        if (valueA is ElmString && valueB is ElmString)
            return true;

        if (valueA is ElmList && valueB is ElmList)
            return null;

        if (valueA is ElmRecord recordA && valueB is ElmRecord recordB)
        {
            var recordAFieldNames =
                recordA.Fields.Select(field => field.FieldName).ToList();

            var recordBFieldNames =
                recordB.Fields.Select(field => field.FieldName).ToList();

            if (!recordAFieldNames.OrderBy(name => name).SequenceEqual(recordBFieldNames))
                return false;

            return null;
        }

        if (valueA is ElmTag && valueB is ElmTag)
            return null;

        if (valueA is ElmInternal && valueB is ElmInternal)
            return null;

        return false;
    }
}
