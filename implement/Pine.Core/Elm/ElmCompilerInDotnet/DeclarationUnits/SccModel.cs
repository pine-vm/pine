using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Runtime.CompilerServices;
using System.Text.Json.Serialization;

namespace Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;

/// <summary>A dependency-closed, source-independent node in the canonical declaration graph.</summary>
[JsonPolymorphic(TypeDiscriminatorPropertyName = "kind")]
[JsonDerivedType(typeof(ValueScc), "value")]
[JsonDerivedType(typeof(TypeScc), "type")]
public abstract record SccModel
{
    private protected SccModel() { }

    internal abstract string Fingerprint { get; }

    internal abstract ReadOnlyMemory<byte> FingerprintBytes { get; }

    /// <summary>A component containing only value declarations.</summary>
    public sealed record ValueScc(ValueSccModel Value) : SccModel
    {
        internal override string Fingerprint => Value.Fingerprint;

        internal override ReadOnlyMemory<byte> FingerprintBytes => Value.FingerprintBytes;
    }

    /// <summary>A type component, also used for constructor dependencies in the value namespace.</summary>
    public sealed record TypeScc(TypeSccModel Value) : SccModel
    {
        internal override string Fingerprint => Value.Fingerprint;

        internal override ReadOnlyMemory<byte> FingerprintBytes => Value.FingerprintBytes;
    }
}

/// <summary>Permitted members of a value SCC.</summary>
[JsonPolymorphic(TypeDiscriminatorPropertyName = "kind")]
[JsonDerivedType(typeof(FunctionDeclaration), "function")]
[JsonDerivedType(typeof(PortDeclaration), "port")]
[JsonDerivedType(typeof(InfixDeclaration), "infix")]
public abstract record ValueDeclaration
{
    private protected ValueDeclaration() { }

    internal Declaration Syntax => SccValidation.Syntax(this);

    /// <summary>A function and its optional signature.</summary>
    public sealed record FunctionDeclaration(FunctionStruct Function) : ValueDeclaration;

    /// <summary>A port or explicitly type-only value interface.</summary>
    public sealed record PortDeclaration(Signature Signature) : ValueDeclaration;

    /// <summary>An operator definition.</summary>
    public sealed record InfixDeclaration(Infix Infix) : ValueDeclaration;
}

/// <summary>Permitted members of a type SCC.</summary>
[JsonPolymorphic(TypeDiscriminatorPropertyName = "kind")]
[JsonDerivedType(typeof(ChoiceTypeDeclaration), "choice")]
[JsonDerivedType(typeof(AliasDeclaration), "alias")]
public abstract record TypeDeclaration
{
    private protected TypeDeclaration() { }

    internal Declaration Syntax => SccValidation.Syntax(this);

    /// <summary>A nominal choice type and all its constructors.</summary>
    public sealed record ChoiceTypeDeclaration(ChoiceTypeStruct TypeDeclaration) : TypeDeclaration;

    /// <summary>A type alias, possibly supplying a record constructor.</summary>
    public sealed record AliasDeclaration(TypeAlias TypeAlias) : TypeDeclaration;
}

/// <summary>Value members with independently symbol-keyed value and type dependencies.</summary>
public sealed record ValueSccModel
{
    /// <summary>Constructs and validates one nonempty, strongly connected component.</summary>
    public ValueSccModel(
        ImmutableDictionary<string, ValueDeclaration> members,
        ImmutableDictionary<DeclQualifiedName, SccModel> dependencies,
        ImmutableDictionary<DeclQualifiedName, TypeSccModel> typeDependencies)
    {
        Members = members.WithComparers(StringComparer.Ordinal);
        Dependencies = dependencies;
        TypeDependencies = typeDependencies;
        SccValidation.Validate(this);

        FingerprintBytes = SccEncodingBinary.ComputeHash(this);
        Fingerprint = Convert.ToHexStringLower(FingerprintBytes.Span);
    }

    /// <summary>Only this component's declarations; dependencies never copy their bodies here.</summary>
    public ImmutableDictionary<string, ValueDeclaration> Members { get; }

    /// <summary>Value symbols, including constructor symbols selecting type SCCs.</summary>
    public ImmutableDictionary<DeclQualifiedName, SccModel> Dependencies { get; }

    /// <summary>Type symbols selecting type members.</summary>
    public ImmutableDictionary<DeclQualifiedName, TypeSccModel> TypeDependencies { get; }

    internal string Fingerprint { get; }

    internal ReadOnlyMemory<byte> FingerprintBytes { get; }

    /// <inheritdoc/>
    public bool Equals(ValueSccModel? other) => other is not null && Fingerprint == other.Fingerprint;

    /// <inheritdoc/>
    public override int GetHashCode() => StringComparer.Ordinal.GetHashCode(Fingerprint);
}

/// <summary>Type members and type-only dependency paths. Constructor indexes are derived once.</summary>
public sealed record TypeSccModel
{
    /// <summary>Constructs and validates one nonempty, strongly connected type component.</summary>
    public TypeSccModel(
        ImmutableDictionary<string, TypeDeclaration> members,
        ImmutableDictionary<DeclQualifiedName, TypeSccModel> typeDependencies,
        ImmutableDictionary<string, string>? intrinsicRepresentations = null)
    {
        Members = members.WithComparers(StringComparer.Ordinal);
        TypeDependencies = typeDependencies;
        IntrinsicRepresentations = intrinsicRepresentations ?? [];
        var constructors = ImmutableDictionary.CreateBuilder<string, string>(StringComparer.Ordinal);

        foreach (var (member, declaration) in Members)
        {
            switch (declaration)
            {
                case TypeDeclaration.ChoiceTypeDeclaration choice:
                    foreach (var constructor in choice.TypeDeclaration.Constructors)
                        AddConstructor(constructor.Name, member);

                    break;

                case TypeDeclaration.AliasDeclaration { TypeAlias.TypeAnnotation: TypeAnnotation.Record }:
                    AddConstructor(member, member);
                    break;

                case TypeDeclaration.AliasDeclaration:
                    break;

                default:
                    throw new NotImplementedException(
                        nameof(TypeSccModel) + " does not handle type declaration: " + declaration.GetType().Name);
            }
        }

        ConstructorMembers = constructors.ToImmutable();
        SccValidation.Validate(this);

        FingerprintBytes = SccEncodingBinary.ComputeHash(this);
        Fingerprint = Convert.ToHexStringLower(FingerprintBytes.Span);

        void AddConstructor(string symbol, string member)
        {
            if (!constructors.TryAdd(symbol, member))
                throw new ArgumentException("A constructor symbol must select exactly one type member: " + symbol);
        }
    }

    /// <summary>Only choice types and aliases can be members.</summary>
    public ImmutableDictionary<string, TypeDeclaration> Members { get; }

    /// <summary>Direct type references, never value dependencies.</summary>
    public ImmutableDictionary<DeclQualifiedName, TypeSccModel> TypeDependencies { get; }

    /// <summary>Language-defined runtime roles, not inferred from ordinary choice-type shapes.</summary>
    public ImmutableDictionary<string, string> IntrinsicRepresentations { get; }

    [JsonIgnore]
    internal ImmutableDictionary<string, string> ConstructorMembers { get; }

    internal string Fingerprint { get; }

    internal ReadOnlyMemory<byte> FingerprintBytes { get; }

    /// <inheritdoc/>
    public bool Equals(TypeSccModel? other) => other is not null && Fingerprint == other.Fingerprint;

    /// <inheritdoc/>
    public override int GetHashCode() => StringComparer.Ordinal.GetHashCode(Fingerprint);
}

internal static class SccValidation
{
    private static readonly ConditionalWeakTable<ValueDeclaration, Declaration> s_valueSyntax = [];

    private static readonly ConditionalWeakTable<TypeDeclaration, Declaration> s_typeSyntax = [];

    public static Declaration Member(SccModel model, string member) =>
        model switch
        {
            SccModel.ValueScc value => Syntax(value.Value.Members[member]),
            SccModel.TypeScc type => Syntax(type.Value.Members[member]),

            _ =>
            throw new NotImplementedException(nameof(Member) + " does not handle SCC: " + model.GetType().Name)
        };

    public static IEnumerable<KeyValuePair<string, Declaration>> Members(SccModel model) =>
        model switch
        {
            SccModel.ValueScc value =>
            value.Value.Members.Select(entry => KeyValuePair.Create(entry.Key, Syntax(entry.Value))),

            SccModel.TypeScc type =>
            type.Value.Members.Select(entry => KeyValuePair.Create(entry.Key, Syntax(entry.Value))),

            _ =>
            throw new NotImplementedException(nameof(Members) + " does not handle SCC: " + model.GetType().Name)
        };

    public static Declaration Syntax(ValueDeclaration declaration) =>
        s_valueSyntax.GetValue(
            declaration,
            static member => member switch
            {
                ValueDeclaration.FunctionDeclaration function => new Declaration.FunctionDeclaration(function.Function),
                ValueDeclaration.PortDeclaration port => new Declaration.PortDeclaration(port.Signature),
                ValueDeclaration.InfixDeclaration infix => new Declaration.InfixDeclaration(infix.Infix),

                _ =>
                throw new NotImplementedException(
                    nameof(Syntax) + " does not handle value declaration: " + member.GetType().Name)
            });

    public static Declaration Syntax(TypeDeclaration declaration) =>
        s_typeSyntax.GetValue(
            declaration,
            static member => member switch
            {
                TypeDeclaration.ChoiceTypeDeclaration choice =>
                new Declaration.ChoiceTypeDeclaration(choice.TypeDeclaration),

                TypeDeclaration.AliasDeclaration alias => new Declaration.AliasDeclaration(alias.TypeAlias),

                _ =>
                throw new NotImplementedException(
                    nameof(Syntax) + " does not handle type declaration: " + member.GetType().Name)
            });

    public static string Select(SccModel model, string symbol, bool isType) =>
        model switch
        {
            SccModel.ValueScc value when !isType && value.Value.Members.ContainsKey(symbol) => symbol,

            SccModel.ValueScc =>
            throw new ArgumentException("A value SCC does not export the requested symbol: " + symbol),

            SccModel.TypeScc type => Select(type.Value, symbol, isType),

            _ =>
            throw new NotImplementedException(nameof(Select) + " does not handle SCC: " + model.GetType().Name)
        };

    public static string Select(TypeSccModel model, string symbol, bool isType)
    {
        if (isType && model.Members.ContainsKey(symbol))
            return symbol;

        if (!isType && model.ConstructorMembers.TryGetValue(symbol, out var member))
            return member;

        throw new ArgumentException(
            "A type SCC does not export the requested " + (isType ? "type" : "constructor") + ": " + symbol);
    }

    public static void Validate(ValueSccModel model)
    {
        foreach (var (key, dependency) in model.Dependencies)
        {
            Qualified(key);
            Select(dependency, key.DeclName, false);
        }

        Validate(
            model.Members.ToDictionary(entry => entry.Key, entry => Syntax(entry.Value), StringComparer.Ordinal),
            model.TypeDependencies,
            (name, isType) => !isType && model.Dependencies.ContainsKey(name));
    }

    public static void Validate(TypeSccModel model)
    {
        foreach (var (member, intrinsic) in model.IntrinsicRepresentations)
        {
            if (!model.Members.ContainsKey(member) || intrinsic is not ("String" or "Char"))
            {
                throw new ArgumentException(
                    "Intrinsic representations must select a type member and a language-defined role.");
            }
        }

        Validate(
            model.Members.ToDictionary(entry => entry.Key, entry => Syntax(entry.Value), StringComparer.Ordinal),
            model.TypeDependencies,
            (_, _) => false);
    }

    private static void Qualified(DeclQualifiedName name)
    {
        if (name.Namespaces.Count is 0)
            throw new ArgumentException("Dependency keys must be qualified symbols: " + name);
    }

    internal static bool Native(DeclQualifiedName name) =>
        name.Namespaces is ["Basics"] or ["Debug"] or ["Pine_kernel"] or ["Pine_builtin"] ||
        name.Namespaces is ["List"] && name.DeclName is "List" ||
        name.Namespaces is ["Char"] && name.DeclName is "Char" ||
        name.Namespaces is ["String"] && name.DeclName is "String";

    private static void Validate(
        Dictionary<string, Declaration> members,
        ImmutableDictionary<DeclQualifiedName, TypeSccModel> types,
        Func<DeclQualifiedName, bool, bool> externalValue)
    {
        if (members.Count is 0)
            throw new ArgumentException("An SCC must contain at least one member.");

        foreach (var (key, dependency) in types)
        {
            Qualified(key);
            Select(dependency, key.DeclName, true);
        }

        var edges = members.Keys.ToDictionary(name => name, _ => new HashSet<string>(), StringComparer.Ordinal);

        foreach (var (member, declaration) in members)
        {
            if (ElmCompiler.GetDeclarationName(declaration) != member)
                throw new ArgumentException("An SCC member key must match its declaration name: " + member);

            Canonicalize.MapNames(
                declaration,
                (name, isType) =>
                {
                    if (name.Namespaces.Count is 0)
                    {
                        if (!members.TryGetValue(name.DeclName, out var target))
                            throw new ArgumentException("An SCC contains an unbound member reference: " + name);

                        var targetIsType = target is Declaration.AliasDeclaration or Declaration.ChoiceTypeDeclaration;

                        if (isType != targetIsType)
                            throw new ArgumentException("An SCC member reference uses the wrong namespace: " + name);

                        edges[member].Add(name.DeclName);
                        return name;
                    }

                    if (Native(name) || isType && types.ContainsKey(name) || externalValue(name, isType))
                        return name;

                    throw new ArgumentException("An SCC contains an unbound reference: " + name);
                });
        }

        var reverse = members.Keys.ToDictionary(name => name, _ => new HashSet<string>(), StringComparer.Ordinal);

        foreach (var (member, targets) in edges)
        {
            foreach (var target in targets)
                reverse[target].Add(member);
        }

        if (!ReachesAll(edges) || !ReachesAll(reverse))
            throw new ArgumentException("SCC members must form a strongly connected component.");

        bool ReachesAll(Dictionary<string, HashSet<string>> graph)
        {
            var seen = new HashSet<string>();
            var pending = new Stack<string>([members.Keys.First()]);

            while (pending.TryPop(out var member))
            {
                if (seen.Add(member))
                {
                    foreach (var target in graph[member])
                        pending.Push(target);
                }
            }

            return seen.Count == members.Count;
        }
    }
}
