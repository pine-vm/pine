using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Abstract;
using System.Collections.Immutable;

namespace Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;

/// <summary>Exact immutable input to SCC validation and construction; edges compare closed values, not transitive bodies.</summary>
public sealed record SccConstructionInput(
    ImmutableDictionary<string, Declaration> Members,
    ImmutableDictionary<DeclQualifiedName, SccModel> Dependencies,
    ImmutableDictionary<DeclQualifiedName, TypeSccModel> TypeDependencies,
    ImmutableDictionary<string, string> IntrinsicRepresentations)
{
    private long _cachedHashCode;

    private SccConstructionInput(SccConstructionInput original)
    {
        Members = original.Members;
        Dependencies = original.Dependencies;
        TypeDependencies = original.TypeDependencies;
        IntrinsicRepresentations = original.IntrinsicRepresentations;
    }

    /// <inheritdoc/>
    public bool Equals(SccConstructionInput? other) =>
        ReferenceEquals(this, other) ||
        other is not null &&
        StructuralHash.SameEntries(Members, other.Members) &&
        StructuralHash.SameEntries(Dependencies, other.Dependencies) &&
        StructuralHash.SameEntries(TypeDependencies, other.TypeDependencies) &&
        StructuralHash.SameEntries(IntrinsicRepresentations, other.IntrinsicRepresentations);

    /// <inheritdoc/>
    public override int GetHashCode() =>
        StructuralHash.GetOrCompute(
            ref _cachedHashCode,
            typeof(SccConstructionInput),
            StructuralHash.UnorderedMap(Members),
            StructuralHash.UnorderedMap(Dependencies),
            StructuralHash.UnorderedMap(TypeDependencies),
            StructuralHash.UnorderedMap(IntrinsicRepresentations));
}
