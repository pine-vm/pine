using Pine.Core.CodeAnalysis;
using Pine.Core.Json;
using System;
using System.Collections.Generic;
using System.Linq;
using System.Text.Json.Serialization;

namespace Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

/// <summary>Function identity, independent of generated source names, bodies, and diagnostic provenance.</summary>
[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record FunctionId
{
    private protected long _cachedHashCode;

    private FunctionId() { }

    /// <summary>Copies an identity without its memoized hash.</summary>
    protected FunctionId(FunctionId original) { }

    /// <inheritdoc/>
    public virtual bool Equals(FunctionId? other) => other is not null && EqualityContract == other.EqualityContract;

    /// <inheritdoc/>
    public override int GetHashCode() => EqualityContract.GetHashCode();

    /// <summary>An original, canonical source declaration.</summary>
    public sealed record Source : FunctionId
    {
        /// <summary>The canonical source name.</summary>
        public DeclQualifiedName Name { get; }

        /// <summary>Constructs an original source identity.</summary>
        public Source(DeclQualifiedName name)
        {
            ArgumentNullException.ThrowIfNull(name);
            Name = CopySourceName(name);
        }

        /// <inheritdoc/>
        public override int GetHashCode() => StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Source), Name);
    }

    /// <summary>A local function site in an owning function.</summary>
    public sealed record Lifted : FunctionId
    {
        /// <summary>The containing function.</summary>
        public FunctionId Owner { get; }

        /// <summary>A nonnegative owner-local site.</summary>
        public int Site { get; }

        /// <summary>Constructs a local function-site identity.</summary>
        public Lifted(FunctionId owner, int site)
        {
            ArgumentNullException.ThrowIfNull(owner);
            ArgumentOutOfRangeException.ThrowIfNegative(site);
            Owner = owner;
            Site = site;
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Lifted), Owner, Site);
    }

    /// <summary>A specialization, expressed relative to the original function's parameter slots.</summary>
    public sealed record Specialized : FunctionId
    {
        /// <summary>The unspecialized function.</summary>
        public FunctionId Original { get; }

        /// <summary>Normalized assumptions on original parameter slots.</summary>
        public ValueList<ParameterConstraint> Constraints { get; }

        private Specialized(FunctionId original, ValueList<ParameterConstraint> constraints)
        {
            ArgumentNullException.ThrowIfNull(original);
            ArgumentNullException.ThrowIfNull(constraints);

            if (original is Specialized || constraints.Count is 0 ||
                constraints.Zip(constraints.Skip(1)).Any(pair => pair.First.Parameter >= pair.Second.Parameter))
            {
                throw new ArgumentException(
                    "Specialization identities must use an original function and nonempty, sorted, distinct parameter slots.");
            }

            Original = original;
            Constraints = constraints;
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Specialized), Original, Constraints);

        /// <summary>
        /// Combines compatible assumptions, removing duplicates and history-dependent nesting.
        /// Empty requests return the original identity; conflicting requests are rejected.
        /// </summary>
        public static FunctionId Create(FunctionId original, IEnumerable<ParameterConstraint> constraints)
        {
            ArgumentNullException.ThrowIfNull(original);
            ArgumentNullException.ThrowIfNull(constraints);

            var combined =
                original is Specialized specialized
                ?
                specialized.Constraints.Concat(constraints)
                :
                constraints;

            var normalized = new SortedDictionary<int, ParameterConstraint>();

            foreach (var constraint in combined)
            {
                if (constraint.Parameter < 0)
                    throw new ArgumentOutOfRangeException(nameof(constraints), "Parameter slots must be nonnegative.");

                if (normalized.TryGetValue(constraint.Parameter, out var previous) && previous != constraint)
                {
                    throw new ArgumentException(
                        "Conflicting assumptions for an original parameter slot.",
                        nameof(constraints));
                }

                normalized[constraint.Parameter] = constraint;
            }

            var basis = original is Specialized existing ? existing.Original : original;
            return normalized.Count is 0 ? basis : new Specialized(basis, new(normalized.Values));
        }
    }

    /// <summary>The originating source declaration. Used for diagnostics, not generated-function lookup.</summary>
    [JsonIgnore]
    public DeclQualifiedName SourceDeclaration =>
        this switch
        {
            Source source => source.Name,
            Lifted lifted => lifted.Owner.SourceDeclaration,
            Specialized specialized => specialized.Original.SourceDeclaration,

            _ =>
            throw new NotImplementedException(
                $"{nameof(SourceDeclaration)} does not handle identity variant: {GetType().Name}")
        };

    internal static DeclQualifiedName CopySourceName(DeclQualifiedName name)
    {
        ArgumentNullException.ThrowIfNull(name);
        ArgumentException.ThrowIfNullOrEmpty(name.DeclName);
        var copy = DeclQualifiedName.Create(new ValueList<string>(name.Namespaces), name.DeclName);

        if (copy.FullName != name.FullName)
        {
            throw new ArgumentException(
                "A source name must agree with its namespace and declaration components.",
                nameof(name));
        }

        return copy;
    }
}

/// <summary>Assumptions distinguish specialized functions without synthetic names or caller identities.</summary>
[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record ParameterConstraint
{
    private protected long _cachedHashCode;

    private ParameterConstraint(int parameter)
    {
        ArgumentOutOfRangeException.ThrowIfNegative(parameter);
        Parameter = parameter;
    }

    /// <summary>Copies a constraint without its memoized hash.</summary>
    protected ParameterConstraint(ParameterConstraint original)
    {
        Parameter = original.Parameter;
    }

    /// <inheritdoc/>
    public virtual bool Equals(ParameterConstraint? other) =>
        other is not null && EqualityContract == other.EqualityContract && Parameter == other.Parameter;

    /// <inheritdoc/>
    public override int GetHashCode() => HashCode.Combine(EqualityContract, Parameter);

    /// <summary>The parameter slot in the original, unspecialized function.</summary>
    public int Parameter { get; }

    /// <summary>Fixes callable code; captured values remain explicit runtime arguments.</summary>
    public sealed record KnownFunction(int Parameter, FunctionId Function) : ParameterConstraint(Parameter)
    {
        /// <summary>The fixed code identity.</summary>
        public FunctionId Function { get; } = Function ?? throw new ArgumentNullException(nameof(Function));

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(KnownFunction), Parameter, Function);
    }

    /// <summary>Constrains a parameter to a complete structural record type.</summary>
    public sealed record ClosedRecord(int Parameter, StructuralType.ClosedRecord Type) : ParameterConstraint(Parameter)
    {
        /// <summary>The complete structural record constraint.</summary>
        public StructuralType.ClosedRecord Type { get; } = Type ?? throw new ArgumentNullException(nameof(Type));

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(ClosedRecord), Parameter, Type);
    }
}

/// <summary>Lexical binding identity. Source spellings are not used to identify resolved bindings.</summary>
[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record BindingId
{
    private protected long _cachedHashCode;

    private BindingId() { }

    /// <summary>Copies an identity without its memoized hash.</summary>
    protected BindingId(BindingId original) { }

    /// <inheritdoc/>
    public virtual bool Equals(BindingId? other) => other is not null && EqualityContract == other.EqualityContract;

    /// <inheritdoc/>
    public override int GetHashCode() => EqualityContract.GetHashCode();

    /// <summary>A deterministic declaration-local binding slot.</summary>
    public sealed record Local : BindingId
    {
        /// <summary>The allocating function.</summary>
        public FunctionId Owner { get; }

        /// <summary>A nonnegative lexical slot.</summary>
        public int Slot { get; }

        /// <summary>Constructs a lexical identity.</summary>
        public Local(FunctionId owner, int slot)
        {
            ArgumentNullException.ThrowIfNull(owner);
            ArgumentOutOfRangeException.ThrowIfNegative(slot);
            Owner = owner;
            Slot = slot;
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Local), Owner, Slot);
    }

    /// <summary>A compiler-introduced record receiver, disjoint from every source binding.</summary>
    public sealed record Temporary : BindingId
    {
        /// <summary>The allocating function.</summary>
        public FunctionId Owner { get; }

        /// <summary>A nonnegative temporary slot.</summary>
        public int Slot { get; }

        /// <summary>Constructs a temporary identity disjoint from source bindings.</summary>
        public Temporary(FunctionId owner, int slot)
        {
            ArgumentNullException.ThrowIfNull(owner);
            ArgumentOutOfRangeException.ThrowIfNegative(slot);
            Owner = owner;
            Slot = slot;
        }

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Temporary), Owner, Slot);
    }

}

/// <summary>Resolved expression references have disjoint namespaces.</summary>
[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record Reference
{
    private protected long _cachedHashCode;

    private Reference() { }

    /// <summary>Copies a reference without its memoized hash.</summary>
    protected Reference(Reference original) { }

    /// <inheritdoc/>
    public virtual bool Equals(Reference? other) => other is not null && EqualityContract == other.EqualityContract;

    /// <inheritdoc/>
    public override int GetHashCode() => EqualityContract.GetHashCode();

    /// <summary>A lexical binding.</summary>
    public sealed record Local(BindingId Binding) : Reference
    {
        /// <summary>The only lexical identity.</summary>
        public BindingId Binding { get; } = Binding ?? throw new ArgumentNullException(nameof(Binding));

        /// <inheritdoc/>
        public override int GetHashCode() => StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Local), Binding);
    }

    /// <summary>A source or generated function.</summary>
    public sealed record Function(FunctionId Id) : Reference
    {
        /// <summary>The only function identity.</summary>
        public FunctionId Id { get; } = Id ?? throw new ArgumentNullException(nameof(Id));

        /// <inheritdoc/>
        public override int GetHashCode() => StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Function), Id);
    }

    /// <summary>A choice or record-alias constructor, retaining its runtime source tag.</summary>
    public sealed record Constructor(DeclQualifiedName Name) : Reference
    {
        /// <summary>The source constructor label.</summary>
        public DeclQualifiedName Name { get; } = FunctionId.CopySourceName(Name);

        /// <inheritdoc/>
        public override int GetHashCode() =>
            StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Constructor), Name);
    }

    /// <summary>A natively implemented reference.</summary>
    public sealed record Native(DeclQualifiedName Name) : Reference
    {
        /// <summary>The native implementation name.</summary>
        public DeclQualifiedName Name { get; } = FunctionId.CopySourceName(Name);

        /// <inheritdoc/>
        public override int GetHashCode() => StructuralHash.GetOrCompute(ref _cachedHashCode, typeof(Native), Name);
    }
}
