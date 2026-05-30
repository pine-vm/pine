using Pine.Core.CodeAnalysis;
using System.Collections.Immutable;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.Stil4mElmSyntax7;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

/// <summary>
/// Model of Elm syntax declarations used in the optimization stages of the Elm compiler.
/// The Elm compiler uses specializations of original function declarations to achieve optimizations such as
/// partial application elimination and higher-order parameter elimination.
/// </summary>
public record OptimizedElmSyntaxDeclarations(
    ImmutableDictionary<DeclQualifiedName, OptimizedElmSyntaxFunctionDeclaration> FunctionDeclarations,
    ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> OtherDeclarations)
{
    /// <summary>
    /// Lifts a flat declaration dictionary into the structured
    /// <see cref="OptimizedElmSyntaxDeclarations"/> form by bucketing each entry:
    /// <list type="bullet">
    ///   <item><description>
    ///     Every <see cref="SyntaxTypes.Declaration.FunctionDeclaration"/> becomes an
    ///     <see cref="OptimizedElmSyntaxFunctionDeclaration"/> with the declaration itself
    ///     as <see cref="OptimizedElmSyntaxFunctionDeclaration.Original"/> and an empty
    ///     <see cref="OptimizedElmSyntaxFunctionDeclaration.Specializations"/> map. The
    ///     resulting entry is placed under its original qualified name in
    ///     <see cref="FunctionDeclarations"/>.
    ///   </description></item>
    ///   <item><description>
    ///     All other declaration kinds are placed under their qualified name in
    ///     <see cref="OtherDeclarations"/>.
    ///   </description></item>
    /// </list>
    /// <para>
    /// The round-trip
    /// <c>FromFlatDictionary(flat).RenderAsFlatDictionary()</c> preserves <paramref name="flat"/>
    /// exactly (no specializations are synthesised from the flat input).
    /// </para>
    /// </summary>
    public static OptimizedElmSyntaxDeclarations FromFlatDictionary(
        ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> flat)
    {
        var functions =
            ImmutableDictionary.CreateBuilder<DeclQualifiedName, OptimizedElmSyntaxFunctionDeclaration>();

        var others =
            ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (name, declaration) in flat)
        {
            if (declaration is SyntaxTypes.Declaration.FunctionDeclaration functionDeclaration)
            {
                functions.Add(
                    name,
                    new OptimizedElmSyntaxFunctionDeclaration(
                        Original: functionDeclaration,
                        Specializations:
                        []));
            }
            else
            {
                others.Add(name, declaration);
            }
        }

        return
            new OptimizedElmSyntaxDeclarations(
                FunctionDeclarations: functions.ToImmutable(),
                OtherDeclarations: others.ToImmutable());
    }

    /// <summary>
    /// Flattens the model to a single dictionary keyed by fully-qualified declaration name.
    /// <para>
    /// Each entry in <see cref="FunctionDeclarations"/> contributes:
    /// </para>
    /// <list type="bullet">
    ///   <item><description>
    ///     The original function declaration under its existing qualified name.
    ///   </description></item>
    ///   <item><description>
    ///     One entry per specialization, keyed by a <see cref="DeclQualifiedName"/> whose
    ///     <see cref="DeclQualifiedName.Namespaces"/> are inherited from the original declaration
    ///     and whose <see cref="DeclQualifiedName.DeclName"/> is taken from the specialization's
    ///     own <see cref="SyntaxTypes.FunctionImplementation.Name"/>. This derives the module
    ///     qualifier for each specialization from the original declaration it specializes.
    ///   </description></item>
    /// </list>
    /// <para>
    /// All entries from <see cref="OtherDeclarations"/> are then added under their own qualified names.
    /// </para>
    /// </summary>
    /// <exception cref="System.ArgumentException">
    /// Thrown if the flattened set of qualified names would contain duplicates
    /// (for example, two specializations of different originals resolving to the same qualified name,
    /// or a specialization colliding with an entry in <see cref="OtherDeclarations"/>).
    /// </exception>
    public ImmutableDictionary<DeclQualifiedName, SyntaxTypes.Declaration> RenderAsFlatDictionary()
    {
        var builder =
            ImmutableDictionary.CreateBuilder<DeclQualifiedName, SyntaxTypes.Declaration>();

        foreach (var (originalName, optimized) in FunctionDeclarations)
        {
            builder.Add(originalName, optimized.Original);

            foreach (var (_, specialization) in optimized.Specializations)
            {
                var specializationDeclName =
                    specialization.Function.Declaration.Value.Name.Value;

                var specializationQualifiedName =
                    new DeclQualifiedName(
                        Namespaces: originalName.Namespaces,
                        DeclName: specializationDeclName);

                builder.Add(specializationQualifiedName, specialization);
            }
        }

        foreach (var (otherName, otherDeclaration) in OtherDeclarations)
        {
            builder.Add(otherName, otherDeclaration);
        }

        return builder.ToImmutable();
    }

    /// <summary>
    /// Enumerates every declaration in the structured model as a
    /// <see cref="KeyValuePair{TKey,TValue}"/> keyed by fully-qualified
    /// declaration name, in the same flattened form that
    /// <see cref="RenderAsFlatDictionary"/> would materialise.
    /// <para>
    /// Yields one entry per <see cref="OptimizedElmSyntaxFunctionDeclaration.Original"/>
    /// (under the entry's qualified name), one entry per specialization
    /// (under a name derived from the original's namespace and the
    /// specialization's own declaration name), and one entry per
    /// <see cref="OtherDeclarations"/> entry — without materialising the
    /// intermediate dictionary.
    /// </para>
    /// </summary>
    public System.Collections.Generic.IEnumerable<System.Collections.Generic.KeyValuePair<DeclQualifiedName, SyntaxTypes.Declaration>>
        EnumerateAllDeclarations()
    {
        foreach (var (originalName, optimized) in FunctionDeclarations)
        {
            yield return new System.Collections.Generic.KeyValuePair<DeclQualifiedName, SyntaxTypes.Declaration>(
                originalName, optimized.Original);

            foreach (var (_, specialization) in optimized.Specializations)
            {
                var specializationDeclName =
                    specialization.Function.Declaration.Value.Name.Value;

                var specializationQualifiedName =
                    new DeclQualifiedName(
                        Namespaces: originalName.Namespaces,
                        DeclName: specializationDeclName);

                yield return new System.Collections.Generic.KeyValuePair<DeclQualifiedName, SyntaxTypes.Declaration>(
                    specializationQualifiedName, specialization);
            }
        }

        foreach (var (otherName, otherDeclaration) in OtherDeclarations)
        {
            yield return new System.Collections.Generic.KeyValuePair<DeclQualifiedName, SyntaxTypes.Declaration>(
                otherName, otherDeclaration);
        }
    }

    /// <inheritdoc/>
    public virtual bool Equals(OptimizedElmSyntaxDeclarations? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        return
            DictionariesEqual(FunctionDeclarations, other.FunctionDeclarations) &&
            DictionariesEqual(OtherDeclarations, other.OtherDeclarations);
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        return
            System.HashCode.Combine(
                FunctionDeclarations.Count,
                OtherDeclarations.Count);
    }

    private static bool DictionariesEqual<TKey, TValue>(
        ImmutableDictionary<TKey, TValue> a,
        ImmutableDictionary<TKey, TValue> b)
        where TKey : notnull
    {
        if (ReferenceEquals(a, b))
            return true;

        if (a.Count != b.Count)
            return false;

        foreach (var (key, valueA) in a)
        {
            if (!b.TryGetValue(key, out var valueB))
                return false;

            if (!System.Collections.Generic.EqualityComparer<TValue>.Default.Equals(valueA, valueB))
                return false;
        }

        return true;
    }
}

/// <summary>
/// An original function declaration together with the set of specializations the optimizer
/// derived from it. Each specialization is keyed by the
/// <see cref="FunctionSpecialization"/> pattern that describes how its parameter list
/// differs from <see cref="Original"/>.
/// </summary>
public record OptimizedElmSyntaxFunctionDeclaration(
    SyntaxTypes.Declaration.FunctionDeclaration Original,
    ImmutableDictionary<FunctionSpecialization, SyntaxTypes.Declaration.FunctionDeclaration> Specializations)
{
    /// <inheritdoc/>
    public virtual bool Equals(OptimizedElmSyntaxFunctionDeclaration? other)
    {
        if (ReferenceEquals(this, other))
            return true;

        if (other is null)
            return false;

        if (!Original.Equals(other.Original))
            return false;

        if (Specializations.Count != other.Specializations.Count)
            return false;

        foreach (var (key, valueA) in Specializations)
        {
            if (!other.Specializations.TryGetValue(key, out var valueB))
                return false;

            if (!valueA.Equals(valueB))
                return false;
        }

        return true;
    }

    /// <inheritdoc/>
    public override int GetHashCode()
    {
        return System.HashCode.Combine(Original, Specializations.Count);
    }
}
