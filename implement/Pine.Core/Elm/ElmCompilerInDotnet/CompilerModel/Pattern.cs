using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Json;
using System.Numerics;
using System.Text.Json.Serialization;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

/// <summary>Phase-neutral resolved patterns shared by optimization and emission. Names are never binding identities.</summary>
[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record Pattern
{
    private protected long _cachedHashCode;

    private Pattern() { }

    /// <summary>Copies a pattern without its memoized hash.</summary>
    protected Pattern(Pattern original) { }

    /// <inheritdoc/>
    public virtual bool Equals(Pattern? other) => other is not null && EqualityContract == other.EqualityContract;

    /// <inheritdoc/>
    public override int GetHashCode() => EqualityContract.GetHashCode();

    /// <summary>Matches any value.</summary>
    public sealed record AllPattern : Pattern
    {
        /// <inheritdoc/>
        public override int GetHashCode() => typeof(AllPattern).GetHashCode();
    }

    /// <summary>Binds a value to one mandatory lexical identity.</summary>
    public sealed record VarPattern : Pattern
    {
        /// <summary>The only binding identity.</summary>
        public BindingId Binding { get; }

        /// <summary>Constructs a resolved binding pattern.</summary>
        public VarPattern(BindingId binding)
        {
            System.ArgumentNullException.ThrowIfNull(binding);
            Binding = binding;
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(VarPattern), Binding);
    }

    /// <summary>Matches the unit value.</summary>
    public sealed record UnitPattern : Pattern
    {
        /// <inheritdoc/>
        public override int GetHashCode() => typeof(UnitPattern).GetHashCode();
    }

    /// <summary>Matches a character code point.</summary>
    public sealed record CharPattern(int Value) : Pattern
    {
        /// <summary>Derived runtime encoding.</summary>
        [JsonIgnore]
        public PineValue ValueAsPineValue => ElmValueEncoding.ElmCharAsPineValue(Value);

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(CharPattern), Value);
    }

    /// <summary>Matches a string.</summary>
    public sealed record StringPattern(string Value) : Pattern
    {
        /// <summary>Derived runtime encoding.</summary>
        [JsonIgnore]
        public PineValue ValueAsPineValue => ElmValueEncoding.StringAsPineValue(Value);

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(StringPattern), Value);
    }

    /// <summary>Matches an integer.</summary>
    public sealed record IntPattern(BigInteger Value) : Pattern
    {
        /// <summary>Derived runtime encoding.</summary>
        [JsonIgnore]
        public PineValue ValueAsPineValue => IntegerEncoding.EncodeSignedInteger(Value);

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(IntPattern), Value);
    }

    /// <summary>Floating pattern retained for explicit unsupported-pattern diagnostics.</summary>
    public sealed record FloatPattern(double Value) : Pattern
    {
        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(FloatPattern), Value);
    }

    /// <summary>Matches a tuple.</summary>
    public sealed record TuplePattern(ValueList<Pattern> Elements) : Pattern
    {
        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(TuplePattern), Elements);
    }

    /// <summary>Matches a list.</summary>
    public sealed record ListPattern(ValueList<Pattern> Elements) : Pattern
    {
        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(ListPattern), Elements);
    }

    /// <summary>Matches the head and tail of a nonempty list.</summary>
    public sealed record UnConsPattern(Pattern Head, Pattern Tail) : Pattern
    {
        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(UnConsPattern), Head, Tail);
    }

    /// <summary>Matches a qualified constructor.</summary>
    public sealed record NamedPattern(DeclQualifiedName Name, ValueList<Pattern> Arguments) : Pattern
    {
        /// <summary>An immutable copy of the constructor's qualified source name.</summary>
        public DeclQualifiedName Name { get; } = FunctionId.CopySourceName(Name);

        /// <summary>Derived runtime tag encoding.</summary>
        [JsonIgnore]
        public PineValue TagNameAsPineValue => StringEncoding.ValueFromString(Name.DeclName);

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(NamedPattern), Name, Arguments);
    }

    /// <summary>Also binds the complete matched value.</summary>
    public sealed record AsPattern : Pattern
    {
        /// <summary>The inner pattern.</summary>
        public Pattern Pattern { get; }

        /// <summary>The alias identity.</summary>
        public BindingId Binding { get; }

        /// <summary>Constructs a resolved alias pattern.</summary>
        public AsPattern(Pattern pattern, BindingId binding)
        {
            System.ArgumentNullException.ThrowIfNull(pattern);
            System.ArgumentNullException.ThrowIfNull(binding);
            Pattern = pattern;
            Binding = binding;
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(AsPattern), Pattern, Binding);
    }
}
