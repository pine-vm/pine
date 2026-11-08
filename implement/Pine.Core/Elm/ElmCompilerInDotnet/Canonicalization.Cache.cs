using Pine.Core.Caching;
using Pine.Core.CodeAnalysis;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;
using System.Threading.Tasks;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

public partial class Canonicalization
{
    /// <summary>
    /// Compatibility handle for caller-owned, provenance-free declaration and SCC memoization.
    /// </summary>
    public sealed class DeclarationCache : IMarkerCacheLifetime<long>
    {
        /// <summary>The new semantic storage. No source paths, ranges or package labels are retained here.</summary>
        public DeclarationUnits.DeclarationUnitCache Semantic { get; } = new();

        /// <summary>Number of retained declaration/input combinations.</summary>
        public int Count => Semantic.Count;

        /// <inheritdoc/>
        public ICacheUsage SetMarker(long marker) => Semantic.SetMarker(marker);

        /// <inheritdoc/>
        public Task TrimAsync(
            int budget,
            Func<long, CacheRetention> retention,
            CancellationToken cancellationToken = default) =>
            Semantic.TrimAsync(budget, retention, cancellationToken);

        /// <summary>Releases retained entries. In-flight compilations may subsequently add their results.</summary>
        public void Clear() => Semantic.Clear();
    }

    internal enum InputKind
    {
        ModuleDeclaration,
        ModuleType,
        ModuleValue,
        TypeImport,
        ValueImport,
        Alias,
        Operator,
        TypeReference,
        ValueReference,
        UnavailableType,
        UnavailableValue
    }

    internal readonly struct InputQuery : IEquatable<InputQuery>
    {
        public InputKind Kind { get; }

        public string Name { get; }

        private readonly int hashCode;

        public InputQuery(InputKind kind, string name)
        {
            Kind = kind;
            Name = name;
            hashCode = HashCode.Combine(kind, name);
        }

        public bool Equals(InputQuery other) => Kind == other.Kind && Name == other.Name;

        public override bool Equals(object? obj) => obj is InputQuery other && Equals(other);

        public override int GetHashCode() => hashCode;
    }

    internal abstract class InputObservation(InputKind kind, string name)
    {
        public InputQuery Query { get; } = new(kind, name);

        public abstract bool Matches(CanonicalizationContext context);
    }

    internal sealed class ModuleDeclarationInput(string name, bool? typeReference, bool present) :
        InputObservation(
            typeReference switch { true => InputKind.ModuleType, false => InputKind.ModuleValue, null => InputKind.ModuleDeclaration },
            name)
    {
        public override bool Matches(CanonicalizationContext context) =>
            present ==
            context.ModuleDeclarations(
                Query.Kind switch
                {
                    InputKind.ModuleType => true,
                    InputKind.ModuleValue => false,

                    _ =>
                    (bool?)null
                })
            .Contains(Query.Name);
    }

    internal sealed class ImportInput(bool typeReference, string name, IReadOnlyList<ModuleName>? modules) :
        InputObservation(typeReference ? InputKind.TypeImport : InputKind.ValueImport, name)
    {
        private readonly IReadOnlyList<ModuleName>? expected =
            modules is null
            ?
            null
            :
            new CompilerModel.ValueList<ModuleName>(
                System.Linq.Enumerable.Select(
                    modules,
                    module => (ModuleName)new CompilerModel.ValueList<string>(module)));

        public override bool Matches(CanonicalizationContext context) =>
            ImportsComparer.Equal(expected, context.GetImport(Query.Kind is InputKind.TypeImport, Query.Name));
    }

    internal sealed class AliasInput(string name, ModuleName? module) : InputObservation(InputKind.Alias, name)
    {
        private readonly ModuleName? expected = module is null ? null : new CompilerModel.ValueList<string>(module);

        public override bool Matches(CanonicalizationContext context) =>
            ModuleNameComparer.Equal(expected, context.AliasMap.GetValueOrDefault(Query.Name));
    }

    internal sealed class OperatorInput(string name, DeclQualifiedName? function) : InputObservation(InputKind.Operator, name)
    {
        public override bool Matches(CanonicalizationContext context)
        {
            if (!context.OperatorToFunction.TryGetValue(Query.Name, out var current))
                return function is null;

            return
                function is not null && function.DeclName == current.FunctionName &&
                ModuleNameComparer.Equal(function.Namespaces, current.ModuleName);
        }
    }

    internal sealed class SymbolInput(DeclQualifiedName name, bool typeReference, SymbolResolution resolution) :
        InputObservation(typeReference ? InputKind.TypeReference : InputKind.ValueReference, name.FullName)
    {
        public override bool Matches(CanonicalizationContext context) =>
            resolution.Equals(context.LookupSymbol!(name, typeReference));
    }

    internal sealed class AvailabilityInput(DeclQualifiedName name, bool typeReference, SymbolAvailability availability) :
        InputObservation(typeReference ? InputKind.UnavailableType : InputKind.UnavailableValue, name.FullName)
    {
        public override bool Matches(CanonicalizationContext context) =>
            availability == context.LookupAvailability!(name, typeReference);
    }

    internal sealed class InputObservations
    {
        private readonly Dictionary<InputQuery, InputObservation> inputs = [];

        public IEnumerable<InputObservation> Values => inputs.Values;

        public void Record(InputObservation input) => inputs.TryAdd(input.Query, input);
    }

    private static class ModuleNameComparer
    {
        public static bool Equal(ModuleName? first, ModuleName? second)
        {
            if (ReferenceEquals(first, second))
                return true;

            if (first is null || second is null || first.Count != second.Count)
                return false;

            for (var index = 0; index < first.Count; ++index)
            {
                if (first[index] != second[index])
                    return false;
            }

            return true;
        }
    }

    private static class ImportsComparer
    {
        public static bool Equal(IReadOnlyList<ModuleName>? first, IReadOnlyList<ModuleName>? second)
        {
            if (ReferenceEquals(first, second))
                return true;

            if (first is null || second is null || first.Count != second.Count)
                return false;

            for (var index = 0; index < first.Count; ++index)
            {
                if (!ModuleNameComparer.Equal(first[index], second[index]))
                    return false;
            }

            return true;
        }
    }

    internal abstract record SymbolResolution
    {
        public sealed record Native : SymbolResolution;

        public sealed record TypeOnly(CompilerModel.ValueList<DeclQualifiedName> Dependencies) : SymbolResolution;

        public sealed record Declaration(DeclQualifiedName Owner) : SymbolResolution;

        public sealed record Ambiguous : SymbolResolution;

        public sealed record Missing(SymbolAvailability Availability) : SymbolResolution;
    }

    internal enum SymbolAvailability
    {
        Declared, OtherNamespace, MissingModule, MissingName
    }
}
