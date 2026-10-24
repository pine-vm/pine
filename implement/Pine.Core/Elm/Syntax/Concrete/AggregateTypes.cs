using System.Collections;
using System.Collections.Generic;
using System.Linq;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

namespace Pine.Core.Elm.Syntax.Concrete;

/// <summary>
/// A list of syntax nodes separated by delimiters, such as commas in a parameter list.
/// This model stores the locations of delimiter tokens alongside the nodes they separate.
/// It does not store the actual delimiter tokens, therefore only fit for contexts where the delimiter type is known and fixed.
/// </summary>
public abstract record SeparatedSyntaxList<TNode>
    : IEnumerable<TNode>
{
    private protected long _cachedHashCode;

    /// <summary>Creates a syntax list.</summary>
    protected SeparatedSyntaxList() { }

    /// <summary>Copies a list without its memoized hash.</summary>
    protected SeparatedSyntaxList(SeparatedSyntaxList<TNode> original) { }

    /// <inheritdoc/>
    public virtual bool Equals(SeparatedSyntaxList<TNode>? other) =>
        other is not null && EqualityContract == other.EqualityContract;

    /// <inheritdoc/>
    public override int GetHashCode() => EqualityContract.GetHashCode();

    /// <summary>
    /// The case of an empty list.
    /// </summary>
    public sealed record Empty
        : SeparatedSyntaxList<TNode>;

    /// <summary>
    /// Represents a separated syntax list that is guaranteed to contain at least one node.
    /// </summary>
    public sealed record NonEmpty(
        TNode First,
        IReadOnlyList<(Location SeparatorLocation, TNode Node)> Rest)
        : SeparatedSyntaxList<TNode>
    {
        /// <inheritdoc/>
        public bool Equals(NonEmpty? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                EqualityComparer<TNode>.Default.Equals(First, other.First) &&
                Enumerable.SequenceEqual(Rest, other.Rest);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hashCode = new System.HashCode();

            hashCode.Add(First);

            foreach (var item in Rest)
                hashCode.Add(item);

            return SyntaxHashCache.Store(ref _cachedHashCode, hashCode.ToHashCode());
        }
    }

    /// <summary>
    /// Gets the count of elements in the list.
    /// </summary>
    public int Count =>
        this switch
        {
            Empty =>
            0,

            NonEmpty nonEmpty =>
            1 + nonEmpty.Rest.Count,

            _ =>
            throw new System.NotImplementedException(
                "Unexpected type: " + GetType().FullName)
        };

    /// <summary>
    /// Copies the nodes to an indexed list, discarding separator locations without enumeration.
    /// </summary>
    public IReadOnlyList<TNode> WithoutSeparatorTokens()
    {
        if (Count is 0)
            return [];

        var result = new TNode[Count];

        for (var i = 0; i < result.Length; i++)
            result[i] = this[i];

        return result;
    }

    /// <summary>
    /// Gets all nodes in the list as an enumerable.
    /// </summary>
    public IEnumerable<TNode> Nodes =>
        this switch
        {
            Empty =>
            [],

            NonEmpty nonEmpty =>
            new[] { nonEmpty.First }.Concat(nonEmpty.Rest.Select(r => r.Node)),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected type: " + GetType().FullName)
        };

    /// <summary>
    /// Maps the nodes while preserving separator locations.
    /// </summary>
    public SeparatedSyntaxList<TResult> Map<TResult>(System.Func<TNode, TResult> map) =>
        this switch
        {
            Empty =>
            new SeparatedSyntaxList<TResult>.Empty(),

            NonEmpty nonEmpty =>
            new SeparatedSyntaxList<TResult>.NonEmpty(
                map(nonEmpty.First),
                [.. nonEmpty.Rest.Select(item => (item.SeparatorLocation, map(item.Node)))]),

            _ =>
            throw new System.NotImplementedException(
                "Unexpected type: " + GetType().FullName)
        };

    /// <summary>
    /// Gets the element at the specified index.
    /// </summary>
    public TNode this[int index] => this switch
    {
        NonEmpty nonEmpty when index is 0 =>
        nonEmpty.First,

        NonEmpty nonEmpty when index > 0 && index <= nonEmpty.Rest.Count =>
        nonEmpty.Rest[index - 1].Node,

        _ =>
        throw new System.ArgumentOutOfRangeException(nameof(index))
    };

    /// <inheritdoc/>
    public IEnumerator<TNode> GetEnumerator()
    {
        switch (this)
        {
            case Empty:
                yield break;

            case NonEmpty nonEmpty:

                yield return nonEmpty.First;

                foreach (var item in nonEmpty.Rest)
                {
                    yield return item.Node;
                }

                break;

            default:
                throw new System.NotImplementedException(
                    "Unexpected type: " + GetType().FullName);
        }
    }

    IEnumerator IEnumerable.GetEnumerator()
    {
        return GetEnumerator();
    }
}

/// <summary>
/// Represents an incomplete declaration that could not be fully parsed.
/// Contains the original text of the declaration for preservation during roundtrip,
/// along with error information to help diagnose the parsing failure.
/// </summary>
/// <param name="OriginalText">The original text of the incomplete declaration.</param>
/// <param name="ParseError">The parsing error information.</param>
public record IncompleteDeclaration(
    string OriginalText,
    ParseError ParseError)
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected IncompleteDeclaration(IncompleteDeclaration original)
    {
        OriginalText = original.OriginalText;
        ParseError = original.ParseError;
    }

    /// <inheritdoc/>
    public virtual bool Equals(IncompleteDeclaration? other) =>
        other is not null && EqualityContract == other.EqualityContract &&
        OriginalText == other.OriginalText && ParseError.Equals(other.ParseError);

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        var hash = SyntaxHashCache.Combine(EqualityContract.GetHashCode(), OriginalText);
        return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, ParseError));
    }
}

/// <summary>
/// Elm import statement containing target module name, optional alias and exposing list.
/// </summary>
public record Import(
    Location ImportTokenLocation,
    Node<ModuleName> ModuleName,
    (Location AsTokenLocation, Node<ModuleName> Alias)? ModuleAlias,
    (Location ExposingTokenLocation, Node<Exposing> ExposingList)? ExposingList)
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected Import(Import original)
    {
        ImportTokenLocation = original.ImportTokenLocation;
        ModuleName = original.ModuleName;
        ModuleAlias = original.ModuleAlias;
        ExposingList = original.ExposingList;
    }

    /// <inheritdoc/>
    public virtual bool Equals(Import? other) =>
        other is not null && EqualityContract == other.EqualityContract &&
        ImportTokenLocation.Equals(other.ImportTokenLocation) &&
        EqualityComparer<Node<ModuleName>>.Default.Equals(ModuleName, other.ModuleName) &&
        ModuleAlias.Equals(other.ModuleAlias) && ExposingList.Equals(other.ExposingList);

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        var hash = SyntaxHashCache.Combine(EqualityContract.GetHashCode(), ImportTokenLocation);
        hash = SyntaxHashCache.Combine(hash, ModuleName);
        hash = SyntaxHashCache.Combine(hash, ModuleAlias);
        return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, ExposingList));
    }
}

/// <summary>
/// Elm module kinds: normal, port or effect (with possible commands/subscriptions).
/// </summary>
public abstract record Module
{
    private protected long _cachedHashCode;

    /// <summary>Creates a module.</summary>
    protected Module() { }

    /// <summary>Copies a module without its memoized hash.</summary>
    protected Module(Module original) { }

    /// <inheritdoc/>
    public virtual bool Equals(Module? other) =>
        other is not null && EqualityContract == other.EqualityContract;

    /// <inheritdoc/>
    public override int GetHashCode() => EqualityContract.GetHashCode();

    /// <summary>Standard module.</summary>
    public sealed record NormalModule(
        Location ModuleTokenLocation,
        DefaultModuleData ModuleData)
        : Module
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hash = SyntaxHashCache.Combine(base.GetHashCode(), ModuleTokenLocation);
            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, ModuleData));
        }
    }

    /// <summary>Port module exposing native interop.</summary>
    public sealed record PortModule(
        Location PortTokenLocation,
        Location ModuleTokenLocation,
        DefaultModuleData ModuleData)
        : Module
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hash = SyntaxHashCache.Combine(base.GetHashCode(), PortTokenLocation);
            hash = SyntaxHashCache.Combine(hash, ModuleTokenLocation);
            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, ModuleData));
        }
    }

    /// <summary>Effect module with commands and subscriptions.</summary>
    public sealed record EffectModule(
        Location EffectTokenLocation,
        Location ModuleTokenLocation,
        EffectModuleData ModuleData)
        : Module
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hash = SyntaxHashCache.Combine(base.GetHashCode(), EffectTokenLocation);
            hash = SyntaxHashCache.Combine(hash, ModuleTokenLocation);
            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, ModuleData));
        }
    }

    /// <summary>
    /// Retrieve the module name syntax node for the specified module instance.
    /// </summary>
    public static Node<ModuleName> GetModuleName(Module module) =>
        module switch
        {
            NormalModule normalModule =>
            normalModule.ModuleData.ModuleName,

            PortModule portModule =>
            portModule.ModuleData.ModuleName,

            EffectModule effectModule =>
            effectModule.ModuleData.ModuleName,

            _ =>
            throw new System.NotImplementedException(
                "Unexpected module type: " + module.GetType().Name),
        };
}

/// <summary>
/// Shared data for normal/port modules: name and exposing list.
/// </summary>
public record DefaultModuleData(
    Node<ModuleName> ModuleName,
    Location ExposingTokenLocation,
    Node<Exposing> ExposingList)
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected DefaultModuleData(DefaultModuleData original)
    {
        ModuleName = original.ModuleName;
        ExposingTokenLocation = original.ExposingTokenLocation;
        ExposingList = original.ExposingList;
    }

    /// <inheritdoc/>
    public virtual bool Equals(DefaultModuleData? other) =>
        other is not null && EqualityContract == other.EqualityContract &&
        EqualityComparer<Node<ModuleName>>.Default.Equals(ModuleName, other.ModuleName) &&
        ExposingTokenLocation.Equals(other.ExposingTokenLocation) &&
        EqualityComparer<Node<Exposing>>.Default.Equals(ExposingList, other.ExposingList);

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        var hash = SyntaxHashCache.Combine(EqualityContract.GetHashCode(), ModuleName);
        hash = SyntaxHashCache.Combine(hash, ExposingTokenLocation);
        return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, ExposingList));
    }
}

/// <summary>
/// Data for effect modules including optional command and subscription identifiers.
/// </summary>
public record EffectModuleData(
    Node<ModuleName> ModuleName,
    Location ExposingTokenLocation,
    Node<Exposing> ExposingList,
    Node<string>? Command,
    Node<string>? Subscription)
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected EffectModuleData(EffectModuleData original)
    {
        ModuleName = original.ModuleName;
        ExposingTokenLocation = original.ExposingTokenLocation;
        ExposingList = original.ExposingList;
        Command = original.Command;
        Subscription = original.Subscription;
    }

    /// <inheritdoc/>
    public virtual bool Equals(EffectModuleData? other) =>
        other is not null && EqualityContract == other.EqualityContract &&
        EqualityComparer<Node<ModuleName>>.Default.Equals(ModuleName, other.ModuleName) &&
        ExposingTokenLocation.Equals(other.ExposingTokenLocation) &&
        EqualityComparer<Node<Exposing>>.Default.Equals(ExposingList, other.ExposingList) &&
        EqualityComparer<Node<string>?>.Default.Equals(Command, other.Command) &&
        EqualityComparer<Node<string>?>.Default.Equals(Subscription, other.Subscription);

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        var hash = SyntaxHashCache.Combine(EqualityContract.GetHashCode(), ModuleName);
        hash = SyntaxHashCache.Combine(hash, ExposingTokenLocation);
        hash = SyntaxHashCache.Combine(hash, ExposingList);
        hash = SyntaxHashCache.Combine(hash, Command);
        return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, Subscription));
    }
}

/// <summary>
/// How a module exposes its definitions: all or explicit list.
/// </summary>
public abstract record Exposing
{
    private protected long _cachedHashCode;

    /// <summary>Creates an exposing list.</summary>
    protected Exposing() { }

    /// <summary>Copies an exposing list without its memoized hash.</summary>
    protected Exposing(Exposing original) { }

    /// <inheritdoc/>
    public virtual bool Equals(Exposing? other) =>
        other is not null && EqualityContract == other.EqualityContract;

    /// <inheritdoc/>
    public override int GetHashCode() => EqualityContract.GetHashCode();

    /// <summary>Expose everything (represented by .. in Elm).</summary>
    public sealed record All(
        Range Range)
        : Exposing;

    /// <summary>Explicit list of exposed top-level items.</summary>
    public sealed record Explicit(
        Location OpenParenLocation,
        SeparatedSyntaxList<Node<TopLevelExpose>> Nodes,
        Location CloseParenLocation)
        : Exposing
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hash = SyntaxHashCache.Combine(base.GetHashCode(), OpenParenLocation);
            hash = SyntaxHashCache.Combine(hash, Nodes);
            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, CloseParenLocation));
        }
    }
}

/// <summary>
/// Individual item exposed from a module: operators, functions, types, or type with constructors.
/// </summary>
public abstract record TopLevelExpose
{
    /// <summary>Exposed operator (infix).</summary>
    public sealed record InfixExpose(
        string Name)
        : TopLevelExpose;

    /// <summary>Exposed function or value.</summary>
    public sealed record FunctionExpose(
        string Name)
        : TopLevelExpose;

    /// <summary>Exposed type or type alias without constructors.</summary>
    public sealed record TypeOrAliasExpose(
        string Name)
        : TopLevelExpose;

    /// <summary>Exposed choice type including maybe constructors via exposed range.</summary>
    public sealed record TypeExpose(
        ExposedType ExposedType)
        : TopLevelExpose;
}

/// <summary>
/// Choice type exposure specification: name and optional opening range for constructors.
/// </summary>
public record ExposedType(
    string Name,
    Range? Open);

/// <summary>
/// Top-level declarations in a module (functions, types, aliases, ports, infix directives).
/// </summary>
public abstract record Declaration
{
    private protected long _cachedHashCode;

    /// <summary>Creates a declaration.</summary>
    protected Declaration() { }

    /// <summary>Copies a declaration without its memoized hash.</summary>
    protected Declaration(Declaration original) { }

    /// <inheritdoc/>
    public virtual bool Equals(Declaration? other) =>
        other is not null && EqualityContract == other.EqualityContract;

    /// <inheritdoc/>
    public override int GetHashCode() => EqualityContract.GetHashCode();

    /// <summary>Function declaration including implementation.</summary>
    public sealed record FunctionDeclaration(
        FunctionStruct Function)
        : Declaration
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(base.GetHashCode(), Function));
        }
    }

    /// <summary>Choice type declaration.</summary>
    public sealed record ChoiceTypeDeclaration(
        ChoiceTypeStruct TypeDeclaration)
        : Declaration
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return
                SyntaxHashCache.Store(
                    ref _cachedHashCode,
                    SyntaxHashCache.Combine(base.GetHashCode(), TypeDeclaration));
        }
    }

    /// <summary>Type alias declaration.</summary>
    public sealed record AliasDeclaration(
        TypeAlias TypeAlias)
        : Declaration
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(base.GetHashCode(), TypeAlias));
        }
    }

    /// <summary>Port declaration with signature.</summary>
    public sealed record PortDeclaration(
        Location PortTokenLocation,
        Signature Signature)
        : Declaration
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hash = SyntaxHashCache.Combine(base.GetHashCode(), PortTokenLocation);
            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, Signature));
        }
    }

    /// <summary>Infix operator metadata declaration.</summary>
    public sealed record InfixDeclaration(
        Infix Infix)
        : Declaration
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(base.GetHashCode(), Infix));
        }
    }
}

/// <summary>
/// Infix operator properties: direction, precedence, operator symbol and implementing function.
/// </summary>
public record Infix(
    Location InfixTokenLocation,
    Node<InfixDirection> Direction,
    Node<int> Precedence,
    Node<string> Operator,
    Location EqualsTokenLocation,
    Node<string> FunctionName)
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected Infix(Infix original)
    {
        InfixTokenLocation = original.InfixTokenLocation;
        Direction = original.Direction;
        Precedence = original.Precedence;
        Operator = original.Operator;
        EqualsTokenLocation = original.EqualsTokenLocation;
        FunctionName = original.FunctionName;
    }

    /// <inheritdoc/>
    public virtual bool Equals(Infix? other) =>
        other is not null && EqualityContract == other.EqualityContract &&
        InfixTokenLocation.Equals(other.InfixTokenLocation) &&
        EqualityComparer<Node<InfixDirection>>.Default.Equals(Direction, other.Direction) &&
        EqualityComparer<Node<int>>.Default.Equals(Precedence, other.Precedence) &&
        EqualityComparer<Node<string>>.Default.Equals(Operator, other.Operator) &&
        EqualsTokenLocation.Equals(other.EqualsTokenLocation) &&
        EqualityComparer<Node<string>>.Default.Equals(FunctionName, other.FunctionName);

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        var hash = SyntaxHashCache.Combine(EqualityContract.GetHashCode(), InfixTokenLocation);
        hash = SyntaxHashCache.Combine(hash, Direction);
        hash = SyntaxHashCache.Combine(hash, Precedence);
        hash = SyntaxHashCache.Combine(hash, Operator);
        hash = SyntaxHashCache.Combine(hash, EqualsTokenLocation);
        return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, FunctionName));
    }
}

/// <summary>
/// Type alias with optional documentation, generics and underlying type annotation.
/// </summary>
public record TypeAlias(
    Node<string>? Documentation,
    Location TypeTokenLocation,
    Location AliasTokenLocation,
    Node<string> Name,
    IReadOnlyList<Node<string>> Generics,
    Location EqualsTokenLocation,
    Node<TypeAnnotation> TypeAnnotation)
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected TypeAlias(TypeAlias original)
    {
        Documentation = original.Documentation;
        TypeTokenLocation = original.TypeTokenLocation;
        AliasTokenLocation = original.AliasTokenLocation;
        Name = original.Name;
        Generics = original.Generics;
        EqualsTokenLocation = original.EqualsTokenLocation;
        TypeAnnotation = original.TypeAnnotation;
    }

    /// <inheritdoc/>
    public virtual bool Equals(TypeAlias? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            EqualityComparer<Node<string>?>.Default.Equals(Documentation, other.Documentation) &&
            TypeTokenLocation.Equals(other.TypeTokenLocation) &&
            AliasTokenLocation.Equals(other.AliasTokenLocation) &&
            Name.Equals(other.Name) &&
            Enumerable.SequenceEqual(Generics, other.Generics) &&
            EqualsTokenLocation.Equals(other.EqualsTokenLocation) &&
            TypeAnnotation.Equals(other.TypeAnnotation);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        var hashCode = new System.HashCode();

        hashCode.Add(Documentation);
        hashCode.Add(TypeTokenLocation);
        hashCode.Add(AliasTokenLocation);
        hashCode.Add(Name);

        foreach (var item in Generics)
            hashCode.Add(item);

        hashCode.Add(EqualsTokenLocation);
        hashCode.Add(TypeAnnotation);

        return SyntaxHashCache.Store(ref _cachedHashCode, hashCode.ToHashCode());
    }
}

/// <summary>
/// Choice type declaration including constructors.
/// </summary>
public record ChoiceTypeStruct(
    Node<string>? Documentation,
    Location TypeTokenLocation,
    Node<string> Name,
    IReadOnlyList<Node<string>> Generics,
    Location EqualsTokenLocation,
    SeparatedSyntaxList<Node<ValueConstructor>> Constructors)
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected ChoiceTypeStruct(ChoiceTypeStruct original)
    {
        Documentation = original.Documentation;
        TypeTokenLocation = original.TypeTokenLocation;
        Name = original.Name;
        Generics = original.Generics;
        EqualsTokenLocation = original.EqualsTokenLocation;
        Constructors = original.Constructors;
    }

    /// <inheritdoc/>
    public virtual bool Equals(ChoiceTypeStruct? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            EqualityComparer<Node<string>?>.Default.Equals(Documentation, other.Documentation) &&
            TypeTokenLocation.Equals(other.TypeTokenLocation) &&
            Name.Equals(other.Name) &&
            Enumerable.SequenceEqual(Generics, other.Generics) &&
            EqualsTokenLocation.Equals(other.EqualsTokenLocation) &&
            Constructors.Equals(other.Constructors);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        var hashCode = new System.HashCode();

        hashCode.Add(Documentation);
        hashCode.Add(TypeTokenLocation);
        hashCode.Add(Name);

        foreach (var item in Generics)
            hashCode.Add(item);

        hashCode.Add(EqualsTokenLocation);

        hashCode.Add(Constructors);

        return SyntaxHashCache.Store(ref _cachedHashCode, hashCode.ToHashCode());
    }
}

/// <summary>
/// Constructor for a choice type with argument type annotations.
/// </summary>
public record ValueConstructor(
    Node<string> Name,
    IReadOnlyList<Node<TypeAnnotation>> Arguments)
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected ValueConstructor(ValueConstructor original)
    {
        Name = original.Name;
        Arguments = original.Arguments;
    }

    /// <inheritdoc/>
    public virtual bool Equals(ValueConstructor? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            Name.Equals(other.Name) &&
            Enumerable.SequenceEqual(Arguments, other.Arguments);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        var hashCode = new System.HashCode();

        hashCode.Add(Name);

        foreach (var item in Arguments)
            hashCode.Add(item);

        return SyntaxHashCache.Store(ref _cachedHashCode, hashCode.ToHashCode());
    }
}

/// <summary>
/// Elm type annotations: generics, concrete typed names, tuples, records, functions, etc.
/// </summary>
public abstract record TypeAnnotation
{
    private protected long _cachedHashCode;

    /// <summary>Creates a type annotation.</summary>
    protected TypeAnnotation() { }

    /// <summary>Copies an annotation without its memoized hash.</summary>
    protected TypeAnnotation(TypeAnnotation original) { }

    /// <inheritdoc/>
    public virtual bool Equals(TypeAnnotation? other) =>
        other is not null && EqualityContract == other.EqualityContract;

    /// <inheritdoc/>
    public override int GetHashCode() => EqualityContract.GetHashCode();

    /// <summary>Reference to a generic type variable.</summary>
    public sealed record GenericType(
        string Name)
        : TypeAnnotation;

    /// <summary>Concrete type optionally namespaced and with arguments.</summary>
    public sealed record Typed(
        Node<(ModuleName ModuleName, string Name)> TypeName,
        IReadOnlyList<Node<TypeAnnotation>> TypeArguments)
        : TypeAnnotation
    {
        /// <inheritdoc/>
        public bool Equals(Typed? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                TypeName.Equals(other.TypeName) &&
                Enumerable.SequenceEqual(TypeArguments, other.TypeArguments);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hashCode = new System.HashCode();

            hashCode.Add(TypeName);

            foreach (var item in TypeArguments)
                hashCode.Add(item);

            return SyntaxHashCache.Store(ref _cachedHashCode, hashCode.ToHashCode());
        }
    }

    /// <summary>Unit type annotation.</summary>
    public sealed record Unit
        : TypeAnnotation;

    /// <summary>Tuple type annotation.</summary>
    /// <remarks>
    /// The open and close parenthesis locations can be derived from the containing node's range:
    /// - OpenParenLocation = ContainingNode.Range.Start
    /// - CloseParenLocation = ContainingNode.Range.End with Column - 1
    /// </remarks>
    public sealed record Tupled(
        SeparatedSyntaxList<Node<TypeAnnotation>> TypeAnnotations)
        : TypeAnnotation
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return
                SyntaxHashCache.Store(
                    ref _cachedHashCode,
                    SyntaxHashCache.Combine(base.GetHashCode(), TypeAnnotations));
        }
    }

    /// <summary>Record type annotation.</summary>
    /// <remarks>
    /// The open and close brace locations can be derived from the containing node's range:
    /// - OpenBraceLocation = ContainingNode.Range.Start
    /// - CloseBraceLocation = ContainingNode.Range.End with Column - 1
    /// </remarks>
    public sealed record Record(
        RecordDefinition RecordDefinition)
        : TypeAnnotation
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return
                SyntaxHashCache.Store(
                    ref _cachedHashCode,
                    SyntaxHashCache.Combine(base.GetHashCode(), RecordDefinition));
        }
    }

    /// <summary>Record type annotation that extends a generic record.</summary>
    /// <remarks>
    /// The open and close brace locations can be derived from the containing node's range:
    /// - OpenBraceLocation = ContainingNode.Range.Start
    /// - CloseBraceLocation = ContainingNode.Range.End with Column - 1
    /// </remarks>
    public sealed record GenericRecord(
        Node<string> GenericName,
        Location PipeLocation,
        Node<RecordDefinition> RecordDefinition)
        : TypeAnnotation
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hash = SyntaxHashCache.Combine(base.GetHashCode(), GenericName);
            hash = SyntaxHashCache.Combine(hash, PipeLocation);
            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, RecordDefinition));
        }
    }

    /// <summary>Function type annotation mapping argument to return.</summary>
    public sealed record FunctionTypeAnnotation(
        Node<TypeAnnotation> ArgumentType,
        Location ArrowLocation,
        Node<TypeAnnotation> ReturnType)
        : TypeAnnotation
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hash = SyntaxHashCache.Combine(base.GetHashCode(), ArgumentType);
            hash = SyntaxHashCache.Combine(hash, ArrowLocation);
            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, ReturnType));
        }
    }
}

/// <summary>
/// Record type definition listing fields.
/// </summary>
public record RecordDefinition(
    SeparatedSyntaxList<Node<RecordField>> Fields)
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected RecordDefinition(RecordDefinition original)
    {
        Fields = original.Fields;
    }

    /// <inheritdoc/>
    public virtual bool Equals(RecordDefinition? other) =>
        other is not null && EqualityContract == other.EqualityContract &&
        EqualityComparer<SeparatedSyntaxList<Node<RecordField>>>.Default.Equals(Fields, other.Fields);

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        return
            SyntaxHashCache.Store(
                ref _cachedHashCode,
                SyntaxHashCache.Combine(EqualityContract.GetHashCode(), Fields));
    }
}

/// <summary>
/// Single record field definition pairing name with type annotation.
/// </summary>
public record RecordField(
    Node<string> FieldName,
    Location ColonLocation,
    Node<TypeAnnotation> FieldType)
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected RecordField(RecordField original)
    {
        FieldName = original.FieldName;
        ColonLocation = original.ColonLocation;
        FieldType = original.FieldType;
    }

    /// <inheritdoc/>
    public virtual bool Equals(RecordField? other) =>
        other is not null && EqualityContract == other.EqualityContract &&
        EqualityComparer<Node<string>>.Default.Equals(FieldName, other.FieldName) &&
        ColonLocation.Equals(other.ColonLocation) &&
        EqualityComparer<Node<TypeAnnotation>>.Default.Equals(FieldType, other.FieldType);

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        var hash = SyntaxHashCache.Combine(EqualityContract.GetHashCode(), FieldName);
        hash = SyntaxHashCache.Combine(hash, ColonLocation);
        return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, FieldType));
    }
}

/// <summary>
/// Function declaration parts: optional docs and signature plus implementation.
/// </summary>
public record FunctionStruct(
    Node<string>? Documentation,
    Node<Signature>? Signature,
    Node<FunctionImplementation> Declaration)
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected FunctionStruct(FunctionStruct original)
    {
        Documentation = original.Documentation;
        Signature = original.Signature;
        Declaration = original.Declaration;
    }

    /// <inheritdoc/>
    public virtual bool Equals(FunctionStruct? other) =>
        other is not null && EqualityContract == other.EqualityContract &&
        EqualityComparer<Node<string>?>.Default.Equals(Documentation, other.Documentation) &&
        EqualityComparer<Node<Signature>?>.Default.Equals(Signature, other.Signature) &&
        EqualityComparer<Node<FunctionImplementation>>.Default.Equals(Declaration, other.Declaration);

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        var hash = SyntaxHashCache.Combine(EqualityContract.GetHashCode(), Documentation);
        hash = SyntaxHashCache.Combine(hash, Signature);
        return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, Declaration));
    }
}

/// <summary>
/// Function implementation name, argument patterns and body expression.
/// </summary>
public record FunctionImplementation(
    Node<string> Name,
    IReadOnlyList<Node<Pattern>> Arguments,
    Location EqualsTokenLocation,
    Node<Expression> Expression)
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected FunctionImplementation(FunctionImplementation original)
    {
        Name = original.Name;
        Arguments = original.Arguments;
        EqualsTokenLocation = original.EqualsTokenLocation;
        Expression = original.Expression;
    }

    /// <inheritdoc/>
    public virtual bool Equals(FunctionImplementation? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            Name.Equals(other.Name) &&
            Enumerable.SequenceEqual(Arguments, other.Arguments) &&
            EqualsTokenLocation.Equals(other.EqualsTokenLocation) &&
            Expression.Equals(other.Expression);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        var hashCode = new System.HashCode();

        hashCode.Add(Name);

        foreach (var item in Arguments)
            hashCode.Add(item);

        hashCode.Add(EqualsTokenLocation);
        hashCode.Add(Expression);

        return SyntaxHashCache.Store(ref _cachedHashCode, hashCode.ToHashCode());
    }
}

/// <summary>
/// Type signature for a value/function: name and type annotation.
/// </summary>
public record Signature(
    Node<string> Name,
    Location ColonLocation,
    Node<TypeAnnotation> TypeAnnotation)
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected Signature(Signature original)
    {
        Name = original.Name;
        ColonLocation = original.ColonLocation;
        TypeAnnotation = original.TypeAnnotation;
    }

    /// <inheritdoc/>
    public virtual bool Equals(Signature? other) =>
        other is not null && EqualityContract == other.EqualityContract &&
        EqualityComparer<Node<string>>.Default.Equals(Name, other.Name) &&
        ColonLocation.Equals(other.ColonLocation) &&
        EqualityComparer<Node<TypeAnnotation>>.Default.Equals(TypeAnnotation, other.TypeAnnotation);

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        var hash = SyntaxHashCache.Combine(EqualityContract.GetHashCode(), Name);
        hash = SyntaxHashCache.Combine(hash, ColonLocation);
        return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, TypeAnnotation));
    }
}

/// <summary>
/// Pattern matching forms used in destructuring and case expressions.
/// </summary>
public abstract record Pattern
{
    private protected long _cachedHashCode;

    /// <summary>Creates a pattern.</summary>
    protected Pattern() { }

    /// <summary>Copies a pattern without its memoized hash.</summary>
    protected Pattern(Pattern original) { }

    /// <inheritdoc/>
    public virtual bool Equals(Pattern? other) =>
        other is not null && EqualityContract == other.EqualityContract;

    /// <inheritdoc/>
    public override int GetHashCode() => EqualityContract.GetHashCode();

    /// <summary>Pattern matching any value.</summary>
    public sealed record AllPattern
        : Pattern;

    /// <summary>Pattern binding a value to a variable name.</summary>
    public sealed record VarPattern(
        string Name)
        : Pattern;

    /// <summary>Pattern matching the unit value.</summary>
    public sealed record UnitPattern
        : Pattern;

    /// <summary>Pattern matching a specific character literal.</summary>
    public sealed record CharPattern(
        int Value)
        : Pattern;

    /// <summary>Pattern matching a specific string literal.</summary>
    /// <param name="Value">The decoded string value.</param>
    /// <param name="LiteralText">
    /// The original source representation, including its single or triple quote delimiters.
    /// Escape sequences are preserved verbatim and physical line endings are normalized to LF.
    /// Null for synthesized patterns, which are rendered from <paramref name="Value"/>.
    /// </param>
    public sealed record StringPattern(
        string Value,
        string? LiteralText = null)
        : Pattern;

    /// <summary>Pattern matching a decimal integer literal.</summary>
    public sealed record IntPattern(
        long Value)
        : Pattern;

    /// <summary>Pattern matching a hexadecimal integer literal.</summary>
    public sealed record HexPattern(
        long Value)
        : Pattern;

    /// <summary>Pattern matching a floating-point literal.</summary>
    public sealed record FloatPattern(
        float Value)
        : Pattern;

    /// <summary>Pattern matching a tuple.</summary>
    public sealed record TuplePattern(
        SeparatedSyntaxList<Node<Pattern>> Elements)
        : Pattern
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(base.GetHashCode(), Elements));
        }
    }

    /// <summary>Pattern matching a record with specified fields.</summary>
    public sealed record RecordPattern(
        SeparatedSyntaxList<Node<string>> Fields)
        : Pattern
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(base.GetHashCode(), Fields));
        }
    }

    /// <summary>List cons pattern separating head and tail.</summary>
    public sealed record UnConsPattern(
        Node<Pattern> Head,
        Location ConsOperatorLocation,
        Node<Pattern> Tail)
        : Pattern
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hash = SyntaxHashCache.Combine(base.GetHashCode(), Head);
            hash = SyntaxHashCache.Combine(hash, ConsOperatorLocation);
            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, Tail));
        }
    }

    /// <summary>Pattern matching a list of elements.</summary>
    public sealed record ListPattern(
        SeparatedSyntaxList<Node<Pattern>> Elements)
        : Pattern
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(base.GetHashCode(), Elements));
        }
    }

    /// <summary>Pattern matching a named constructor with arguments.</summary>
    public sealed record NamedPattern(
        QualifiedNameRef Name,
        IReadOnlyList<Node<Pattern>> Arguments)
        : Pattern
    {
        /// <inheritdoc/>
        public bool Equals(NamedPattern? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                EqualityComparer<QualifiedNameRef>.Default.Equals(Name, other.Name) &&
                Enumerable.SequenceEqual(Arguments, other.Arguments);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hashCode = new System.HashCode();

            hashCode.Add(Name);

            foreach (var item in Arguments)
                hashCode.Add(item);

            return SyntaxHashCache.Store(ref _cachedHashCode, hashCode.ToHashCode());
        }
    }

    /// <summary>Pattern that aliases a match to a name.</summary>
    public sealed record AsPattern(
        Node<Pattern> Pattern,
        Location AsTokenLocation,
        Node<string> Name)
        : Pattern
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hash = SyntaxHashCache.Combine(base.GetHashCode(), Pattern);
            hash = SyntaxHashCache.Combine(hash, AsTokenLocation);
            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, Name));
        }
    }

    /// <summary>Pattern wrapped in parentheses.</summary>
    public sealed record ParenthesizedPattern(
        Node<Pattern> Pattern)
        : Pattern
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(base.GetHashCode(), Pattern));
        }
    }
}

/// <summary>
/// Qualified reference to a name with its module path.
/// </summary>
public record QualifiedNameRef(
    ModuleName ModuleName,
    string Name)
    : System.IEquatable<QualifiedNameRef>
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected QualifiedNameRef(QualifiedNameRef original)
    {
        ModuleName = original.ModuleName;
        Name = original.Name;
    }

    /// <inheritdoc/>
    virtual public bool Equals(QualifiedNameRef? obj)
    {
        if (obj is not QualifiedNameRef other)
            return false;

        if (Name != other.Name)
            return false;

        if (!Enumerable.SequenceEqual(ModuleName, other.ModuleName))
            return false;

        return true;
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        var hash = new System.HashCode();

        foreach (var part in ModuleName)
        {
            hash.Add(part);
        }

        hash.Add(Name);

        return SyntaxHashCache.Store(ref _cachedHashCode, hash.ToHashCode());
    }

    /// <summary>
    /// Creates a new instance of <see cref="QualifiedNameRef"/> from a fully qualified name string, splitting it into
    /// module and name components.
    /// </summary>
    public static QualifiedNameRef FromFullName(string fullName)
    {
        var parts = fullName.Split('.');

        if (parts.Length is 0)
            throw new System.ArgumentException("Full name cannot be empty", nameof(fullName));


        var moduleName =
            parts.Length > 1
            ?
            parts[..^1]
            :
            [];

        var name = parts[^1];

        return new QualifiedNameRef(moduleName, name);
    }

    /// <inheritdoc/>
    public override string ToString()
    {
        return
            ModuleName.Count > 0
            ?
            string.Join(".", ModuleName) + "." + Name
            :
            Name;
    }
}

/// <summary>
/// Elm expressions: literals, applications, control flow, data structures, lambda, let, case, etc.
/// </summary>
public abstract record Expression
{
    private protected long _cachedHashCode;

    /// <summary>Creates an expression.</summary>
    protected Expression() { }

    /// <summary>Copies an expression without its memoized hash.</summary>
    protected Expression(Expression original) { }

    /// <inheritdoc/>
    public virtual bool Equals(Expression? other) =>
        other is not null && EqualityContract == other.EqualityContract;

    /// <inheritdoc/>
    public override int GetHashCode() => EqualityContract.GetHashCode();

    /// <summary>Expression producing unit.</summary>
    public sealed record UnitExpr
        : Expression;

    /// <summary>
    /// Simple (single-quoted) string literal expression.
    /// Triple-quoted multiline string literals are represented by <see cref="MultilineStringLiteral"/>.
    /// </summary>
    public sealed record StringLiteral(
        string Value,

        /*
         * The original source text of the string literal content (the characters between the opening
         * and closing quotes, excluding the quotes themselves). Escape sequences such as "\n", "\t"
         * or "\u{000A}" are preserved verbatim. This enables the renderer to reproduce the varied
         * character representations found in the source instead of re-encoding the (decoded)
         * <see cref="Value"/> into a canonical form.
         * It is null for literals that were not produced from source text (for example synthesized
         * during lowering), in which case the renderer falls back to encoding <see cref="Value"/>.
         * */
        string? SourceText = null)
        : Expression;

    /// <summary>
    /// Triple-quoted (multiline) string literal expression.
    /// </summary>
    /// <remarks>
    /// In contrast to <see cref="StringLiteral"/>, the original source representation is modelled as an
    /// immutable array of lines (<see cref="SourceLines"/>) rather than a single string. The lines
    /// are the original source text content (between the opening and closing <c>"""</c> delimiters)
    /// split on physical line endings, with escape sequences preserved verbatim. Storing the content
    /// already split into lines keeps rendering simple: each line is emitted as-is and the configured
    /// linebreak style is inserted between lines, avoiding a character-by-character scan.
    /// <see cref="SourceLines"/> is null for literals that were not produced from source text (for
    /// example synthesized during lowering), in which case the renderer falls back to encoding the
    /// decoded <see cref="Value"/>.
    /// </remarks>
    public sealed record MultilineStringLiteral(
        string Value,
        ModuleName? SourceLines = null)
        : Expression, System.IEquatable<MultilineStringLiteral>
    {
        /// <inheritdoc/>
        public bool Equals(MultilineStringLiteral? other) =>
            other is not null &&
            Value == other.Value &&
            (SourceLines is null
            ?
            other.SourceLines is null
            :
            other.SourceLines is not null &&
            Enumerable.SequenceEqual(SourceLines, other.SourceLines));

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hash = new System.HashCode();

            hash.Add(Value);

            if (SourceLines is not null)
            {
                foreach (var line in SourceLines)
                {
                    hash.Add(line);
                }
            }

            return SyntaxHashCache.Store(ref _cachedHashCode, hash.ToHashCode());
        }
    }

    /// <summary>Character literal expression.</summary>
    public sealed record CharLiteral(
        int Value)
        : Expression;

    /// <summary>Integer literal expression (decimal or hexadecimal).</summary>
    /// <remarks>
    /// The original literal string is preserved to enable exact roundtripping of source code.
    /// The string may represent a decimal (e.g., "42", "-123") or hexadecimal (e.g., "0xFF", "-0x1A") literal.
    /// </remarks>
    public sealed record IntegerLiteral(
        string LiteralText)
        : Expression;

    /// <summary>Floating-point literal expression.</summary>
    /// <remarks>
    /// The original literal string is preserved to enable exact roundtripping of source code.
    /// Use <see cref="FloatLiteralConversion.ToElmFloat(string)"/> to convert to the numeric representation.
    /// </remarks>
    public sealed record FloatLiteral(
        string LiteralText)
        : Expression;

    /// <summary>Arithmetic negation of an expression.</summary>
    public sealed record Negation(
        Node<Expression> Expression)
        : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(base.GetHashCode(), Expression));
        }
    }

    /// <summary>List literal expression.</summary>
    public sealed record ListExpr(
        SeparatedSyntaxList<Node<Expression>> Elements)
        : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(base.GetHashCode(), Elements));
        }
    }

    /// <summary>Reference to a function or value.</summary>
    public sealed record Identifier(
        ModuleName ModuleName,
        string Name)
        : Expression, System.IEquatable<Identifier>
    {
        /// <inheritdoc/>
        public bool Equals(Identifier? other) =>
            other is not null &&
            Name == other.Name &&
            Enumerable.SequenceEqual(ModuleName, other.ModuleName);

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hash = new System.HashCode();

            foreach (var part in ModuleName)
            {
                hash.Add(part);
            }

            hash.Add(Name);

            return SyntaxHashCache.Store(ref _cachedHashCode, hash.ToHashCode());
        }

        /// <summary>
        /// Creates a new instance of <see cref="Identifier"/> from a fully qualified name, splitting the name into
        /// module and value components.
        /// </summary>
        public static Identifier FromFullName(string fullName)
        {
            var parts = fullName.Split('.');

            if (parts.Length is 0)
                throw new System.ArgumentException("Full name cannot be empty", nameof(fullName));

            var value = parts[^1];

            var moduleName =
                parts.Length > 1
                ?
                parts[..^1]
                :
                [];

            return new Identifier(moduleName, value);
        }
    }

    /// <summary>Conditional expression with then/else branches.</summary>
    public sealed record IfBlock(
        Location IfTokenLocation,
        Node<Expression> Condition,
        Location ThenTokenLocation,
        Node<Expression> ThenBlock,
        Location ElseTokenLocation,
        Node<Expression> ElseBlock)
        : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hash = SyntaxHashCache.Combine(base.GetHashCode(), IfTokenLocation);
            hash = SyntaxHashCache.Combine(hash, Condition);
            hash = SyntaxHashCache.Combine(hash, ThenTokenLocation);
            hash = SyntaxHashCache.Combine(hash, ThenBlock);
            hash = SyntaxHashCache.Combine(hash, ElseTokenLocation);
            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, ElseBlock));
        }
    }

    /// <summary>Prefix operator expression.</summary>
    public sealed record PrefixOperator(
        string Operator)
        : Expression;

    /// <summary>Parenthesized subexpression.</summary>
    public sealed record Parenthesized(
        Node<Expression> Expression)
        : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(base.GetHashCode(), Expression));
        }
    }

    /// <summary>Function application expression.</summary>
    public sealed record Application(
        Node<Expression> Function,
        IReadOnlyList<Node<Expression>> Arguments)
        : Expression
    {
        /*
         * Aligned with stil4m/elm-syntax v8: Use dedicated property for the 'Function' part:
         * + https://github.com/stil4m/elm-syntax/issues/43
         * + https://github.com/stil4m/elm-syntax/pull/48
         * + https://github.com/stil4m/elm-syntax/blob/4268c7d850577f35ae0975862e376c7365b3064c/src/Elm/Syntax/Expression.elm#L112
         * */

        /// <inheritdoc/>
        public bool Equals(Application? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                EqualityComparer<Node<Expression>>.Default.Equals(Function, other.Function) &&
                Enumerable.SequenceEqual(Arguments, other.Arguments);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hashCode = new System.HashCode();

            hashCode.Add(Function);

            foreach (var item in Arguments)
                hashCode.Add(item);

            return SyntaxHashCache.Store(ref _cachedHashCode, hashCode.ToHashCode());
        }
    }

    /// <summary>Operator application with explicit direction.</summary>
    public sealed record OperatorApplication(
        Node<string> Operator,
        InfixDirection Direction,
        Node<Expression> Left,
        Node<Expression> Right)
        : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hash = SyntaxHashCache.Combine(base.GetHashCode(), Operator);
            hash = SyntaxHashCache.Combine(hash, Direction);
            hash = SyntaxHashCache.Combine(hash, Left);
            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, Right));
        }
    }

    /// <summary>Tuple literal expression.</summary>
    public sealed record TupledExpression(
        SeparatedSyntaxList<Node<Expression>> Elements)
        : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(base.GetHashCode(), Elements));
        }
    }

    /// <summary>Lambda expression.</summary>
    public sealed record LambdaExpression(
        LambdaStruct Lambda)
        : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(base.GetHashCode(), Lambda));
        }
    }

    /// <summary>Case expression matching an expression over patterns.</summary>
    public sealed record CaseExpression(
        CaseBlock CaseBlock)
        : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(base.GetHashCode(), CaseBlock));
        }
    }

    /// <summary>Let expression introducing local declarations.</summary>
    public sealed record LetExpression(
        LetBlock Value)
        : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(base.GetHashCode(), Value));
        }
    }

    /// <summary>
    /// Let block containing declarations and final expression.
    /// </summary>
    public sealed record LetBlock(
        Location LetTokenLocation,
        IReadOnlyList<Node<LetDeclaration>> Declarations,
        Location InTokenLocation,
        Node<Expression> Expression)
    {
        private long _cachedHashCode;

        private LetBlock(LetBlock original)
        {
            LetTokenLocation = original.LetTokenLocation;
            Declarations = original.Declarations;
            InTokenLocation = original.InTokenLocation;
            Expression = original.Expression;
        }

        /// <inheritdoc/>
        public bool Equals(LetBlock? other)
        {
            if (ReferenceEquals(this, other))
                return true;

            if (other is null)
                return false;

            return
                LetTokenLocation.Equals(other.LetTokenLocation) &&
                Enumerable.SequenceEqual(Declarations, other.Declarations) &&
                InTokenLocation.Equals(other.InTokenLocation) &&
                Expression.Equals(other.Expression);
        }

        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hashCode = new System.HashCode();

            hashCode.Add(LetTokenLocation);

            foreach (var item in Declarations)
                hashCode.Add(item);

            hashCode.Add(InTokenLocation);
            hashCode.Add(Expression);

            return SyntaxHashCache.Store(ref _cachedHashCode, hashCode.ToHashCode());
        }
    }

    /// <summary>
    /// Declarations inside a let: nested functions or destructuring bindings.
    /// </summary>
    public abstract record LetDeclaration
    {
        private protected long _cachedHashCode;

        /// <summary>Creates a let declaration.</summary>
        protected LetDeclaration() { }

        /// <summary>Copies a declaration without its memoized hash.</summary>
        protected LetDeclaration(LetDeclaration original) { }

        /// <inheritdoc/>
        public virtual bool Equals(LetDeclaration? other) =>
            other is not null && EqualityContract == other.EqualityContract;

        /// <inheritdoc/>
        public override int GetHashCode() => EqualityContract.GetHashCode();

        /// <summary>Local function declaration inside a let block.</summary>
        public sealed record LetFunction(
            FunctionStruct Function)
            : LetDeclaration
        {
            /// <inheritdoc/>
            public override int GetHashCode()
            {
                if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                    return cached;

                return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(base.GetHashCode(), Function));
            }
        }

        /// <summary>Local binding via pattern destructuring.</summary>
        public sealed record LetDestructuring(
            Node<Pattern> Pattern,
            Location EqualsTokenLocation,
            Node<Expression> Expression)
            : LetDeclaration
        {
            /// <inheritdoc/>
            public override int GetHashCode()
            {
                if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                    return cached;

                var hash = SyntaxHashCache.Combine(base.GetHashCode(), Pattern);
                hash = SyntaxHashCache.Combine(hash, EqualsTokenLocation);
                return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, Expression));
            }
        }
    }

    /// <summary>Record literal expression.</summary>
    public sealed record RecordExpr(
        SeparatedSyntaxList<RecordExprField> Fields)
        : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(base.GetHashCode(), Fields));
        }
    }

    /// <summary>Expression accessing a record field.</summary>
    public sealed record RecordAccess(
        Node<Expression> Record,
        Node<string> FieldName)
        : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hash = SyntaxHashCache.Combine(base.GetHashCode(), Record);
            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, FieldName));
        }
    }

    /// <summary>Expression yielding a getter function for a record field.</summary>
    public sealed record RecordAccessFunction(
        string FunctionName)
        : Expression;

    /// <summary>Expression updating a record value.</summary>
    public sealed record RecordUpdateExpression(
        Node<string> RecordName,
        Location PipeLocation,
        SeparatedSyntaxList<RecordExprField> Fields)
        : Expression
    {
        /// <inheritdoc/>
        public override int GetHashCode()
        {
            if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
                return cached;

            var hash = SyntaxHashCache.Combine(base.GetHashCode(), RecordName);
            hash = SyntaxHashCache.Combine(hash, PipeLocation);
            return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, Fields));
        }
    }

    /// <summary>GLSL shader expression [glsl| ... |].</summary>
    public sealed record GLSLExpression(
        string ShaderCode)
        : Expression;
}

/// <summary>
/// A record expression field with field name, equals sign location, and value expression.
/// Used in RecordExpr and RecordUpdateExpression.
/// </summary>
public record RecordExprField(
    Node<string> FieldName,
    Location EqualsLocation,
    Node<Expression> ValueExpr)
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected RecordExprField(RecordExprField original)
    {
        FieldName = original.FieldName;
        EqualsLocation = original.EqualsLocation;
        ValueExpr = original.ValueExpr;
    }

    /// <inheritdoc/>
    public virtual bool Equals(RecordExprField? other) =>
        other is not null && EqualityContract == other.EqualityContract &&
        EqualityComparer<Node<string>>.Default.Equals(FieldName, other.FieldName) &&
        EqualsLocation.Equals(other.EqualsLocation) &&
        EqualityComparer<Node<Expression>>.Default.Equals(ValueExpr, other.ValueExpr);

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        var hash = SyntaxHashCache.Combine(EqualityContract.GetHashCode(), FieldName);
        hash = SyntaxHashCache.Combine(hash, EqualsLocation);
        return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, ValueExpr));
    }
}


/// <summary>
/// Lambda expression capturing argument patterns and body.
/// </summary>
public record LambdaStruct(
    Location BackslashLocation,
    IReadOnlyList<Node<Pattern>> Arguments,
    Location ArrowLocation,
    Node<Expression> Expression)
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected LambdaStruct(LambdaStruct original)
    {
        BackslashLocation = original.BackslashLocation;
        Arguments = original.Arguments;
        ArrowLocation = original.ArrowLocation;
        Expression = original.Expression;
    }

    /// <inheritdoc/>
    public virtual bool Equals(LambdaStruct? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            BackslashLocation.Equals(other.BackslashLocation) &&
            Enumerable.SequenceEqual(Arguments, other.Arguments) &&
            ArrowLocation.Equals(other.ArrowLocation) &&
            Expression.Equals(other.Expression);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        var hashCode = new System.HashCode();

        hashCode.Add(BackslashLocation);

        foreach (var item in Arguments)
            hashCode.Add(item);

        hashCode.Add(ArrowLocation);
        hashCode.Add(Expression);

        return SyntaxHashCache.Store(ref _cachedHashCode, hashCode.ToHashCode());
    }
}

/// <summary>
/// Case expression block containing scrutinee and branches.
/// </summary>
public record CaseBlock(
    Location CaseTokenLocation,
    Node<Expression> Expression,
    Location OfTokenLocation,
    IReadOnlyList<Case> Cases)
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected CaseBlock(CaseBlock original)
    {
        CaseTokenLocation = original.CaseTokenLocation;
        Expression = original.Expression;
        OfTokenLocation = original.OfTokenLocation;
        Cases = original.Cases;
    }

    /// <inheritdoc/>
    public virtual bool Equals(CaseBlock? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            CaseTokenLocation.Equals(other.CaseTokenLocation) &&
            Expression.Equals(other.Expression) &&
            OfTokenLocation.Equals(other.OfTokenLocation) &&
            Enumerable.SequenceEqual(Cases, other.Cases);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        var hashCode = new System.HashCode();

        hashCode.Add(CaseTokenLocation);
        hashCode.Add(Expression);
        hashCode.Add(OfTokenLocation);

        foreach (var item in Cases)
            hashCode.Add(item);

        return SyntaxHashCache.Store(ref _cachedHashCode, hashCode.ToHashCode());
    }
}

/// <summary>
/// Single case: pattern and resulting expression.
/// </summary>
public record Case(
    Node<Pattern> Pattern,
    Location ArrowLocation,
    Node<Expression> Expression)
{
    private long _cachedHashCode;

    /// <summary>Copies syntax data without its memoized hash.</summary>
    protected Case(Case original)
    {
        Pattern = original.Pattern;
        ArrowLocation = original.ArrowLocation;
        Expression = original.Expression;
    }

    /// <inheritdoc/>
    public virtual bool Equals(Case? other) =>
        other is not null && EqualityContract == other.EqualityContract &&
        EqualityComparer<Node<Pattern>>.Default.Equals(Pattern, other.Pattern) &&
        ArrowLocation.Equals(other.ArrowLocation) &&
        EqualityComparer<Node<Expression>>.Default.Equals(Expression, other.Expression);

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        if (SyntaxHashCache.TryGet(ref _cachedHashCode, out var cached))
            return cached;

        var hash = SyntaxHashCache.Combine(EqualityContract.GetHashCode(), Pattern);
        hash = SyntaxHashCache.Combine(hash, ArrowLocation);
        return SyntaxHashCache.Store(ref _cachedHashCode, SyntaxHashCache.Combine(hash, Expression));
    }
}
