using Pine.Core.Caching;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.Syntax.Concrete;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Threading;
using System.Threading.Tasks;

using ModuleName = System.Collections.Generic.IReadOnlyList<string>;

namespace Pine.Core.Elm.ElmCompilerInDotnet;

public partial class Canonicalization
{
    /// <summary>
    /// Caller-owned memoization of declarations over their consulted resolution inputs.
    /// Historical input versions are retained until cleared or marker-budget trimming removes them.
    /// Lowered and syntax-preserving canonicalization occupy separate identities.
    /// </summary>
    public sealed class DeclarationCache : IMarkerCacheLifetime<long>
    {
        private readonly ConcurrentDictionary<DeclarationKey, ImmutableList<Entry>> entries = new();

        private readonly MemoryCache<Entry, Entry, long> lifetime;

        private readonly object gate = new();

        /// <summary>Creates independently owned, thread-safe storage and its resolution-query index.</summary>
        public DeclarationCache()
        {
            lifetime = new(0, ReferenceEqualityComparer.Instance, (_, entry) => RemoveEntry(entry));
        }

        /// <summary>Number of retained declaration/input combinations.</summary>
        public int Count => lifetime.Count;

        /// <inheritdoc/>
        public ICacheUsage SetMarker(long marker) => lifetime.SetMarker(marker);

        /// <inheritdoc/>
        public Task TrimAsync(int budget, Func<long, CacheRetention> retention, CancellationToken cancellationToken = default) =>
            lifetime.TrimAsync(budget, retention, cancellationToken);

        private void RemoveEntry(Entry entry)
        {
            lock (gate)
            {
                if (!entries.TryGetValue(entry.Key, out var versions))
                    return;

                versions = versions.Remove(entry, ReferenceEqualityComparer.Instance);
                if (versions.IsEmpty)
                    entries.TryRemove(entry.Key, out _);
                else
                    entries[entry.Key] = versions;
            }
        }

        /// <summary>Releases retained entries. In-flight compilations may subsequently add their results.</summary>
        public void Clear()
        {
            lock (gate)
            {
                entries.Clear();
                lifetime.Clear();
            }
        }

        internal bool TryGet(
            Node<Declaration> declaration,
            CanonicalizationContext context,
            out DeclarationCanonicalizationCacheEntry result)
        {
            if (entries.TryGetValue(new(declaration, context.CurrentModuleName, context.PreserveOperatorApplications), out var versions))
            {
                foreach (var version in versions)
                {
                    if (version.InputsMatch(context) && lifetime.TryGetValue(version, out _))
                    {
                        result = version.Result;
                        return true;
                    }
                }
            }

            result = null!;
            return false;
        }

        internal void Add(
            Node<Declaration> declaration,
            CanonicalizationContext context,
            DeclarationCanonicalizationCacheEntry result,
            InputObservations inputs)
        {
            var entry = new Entry(
                new(declaration, context.CurrentModuleName, context.PreserveOperatorApplications),
                [.. inputs.Values], result);

            lock (gate)
            {
                if (TryGet(declaration, context, out _))
                    return;

                lifetime.Set(entry, entry);
                entries.AddOrUpdate(
                    entry.Key,
                    [entry],
                    (_, versions) => versions.Insert(0, entry));
            }
        }

        private sealed record Entry(
            DeclarationKey Key,
            ImmutableArray<InputObservation> Inputs,
            DeclarationCanonicalizationCacheEntry Result)
        {
            public bool InputsMatch(CanonicalizationContext context)
            {
                foreach (var input in Inputs)
                {
                    if (!input.Matches(context))
                        return false;
                }

                return true;
            }
        }

        private readonly struct DeclarationKey : IEquatable<DeclarationKey>
        {
            private readonly Node<Declaration> declaration;

            private readonly CompilerModel.ValueList<string> module;

            private readonly int hashCode;
            private readonly bool preserveOperatorApplications;

            public DeclarationKey(Node<Declaration> declaration, CompilerModel.ValueList<string> module, bool preserveOperatorApplications)
            {
                this.declaration = declaration;
                this.module = module;
                this.preserveOperatorApplications = preserveOperatorApplications;
                hashCode = HashCode.Combine(declaration, module, preserveOperatorApplications);
            }

            public bool Equals(DeclarationKey other) =>
                preserveOperatorApplications == other.preserveOperatorApplications &&
                declaration.Equals(other.declaration) && module.Equals(other.module);

            public override bool Equals(object? obj) => obj is DeclarationKey other && Equals(other);

            public override int GetHashCode() => hashCode;
        }
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
