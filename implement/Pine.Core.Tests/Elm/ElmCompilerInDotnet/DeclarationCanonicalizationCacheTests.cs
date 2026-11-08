using AwesomeAssertions;
using Pine.Core.Caching;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Concrete;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class DeclarationCanonicalizationCacheTests
{
    private static readonly DeclQualifiedName s_root = DeclQualifiedName.Create(["Main"], "root");

    private static File Parse(string text) =>
        ElmSyntaxParser.ParseModuleText(text).Extract(error => throw new InvalidOperationException(error.ToString()));

    private static DeclarationCanonicalizationResult Run(
        Canonicalization.DeclarationCache cache, params string[] modules) =>
        Run(cache, false, modules);

    private static DeclarationCanonicalizationResult Run(
        Canonicalization.DeclarationCache cache, bool preserveOperators, params string[] modules) =>
        Canonicalization.CanonicalizeDeclarations(
            [.. modules.Select(Parse)],
            [s_root],
            implicitImports: null,
            cache,
            canonicalizedDependencies: null,
            preserveOperatorApplications: preserveOperators)
        .Extract(error => throw new InvalidOperationException(error));

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Import_alias_matching_owner_module_resolves_values_types_and_constructors_before_self_normalization(
        bool preserveOperators)
    {
        const string Main = "module Main exposing (..)\nimport Decode\nroot = ( Decode.run, Decode.local )";

        const string Library =
            "module Bytes.Decode exposing (Decoder(..), decode)\ntype Decoder = Value Int\ndecode bytes = bytes";

        const string Consumer =
            """
            module Decode exposing (run, local)
            import Bytes.Decode as Decode
            run : Decode.Decoder -> Decode.Decoder
            run (Decode.Value bytes) = Decode.Value (Decode.decode bytes)
            local bytes = decode bytes
            decode bytes = 99
            """;

        var cache = new Canonicalization.DeclarationCache();
        var first = Run(cache, preserveOperators, Main, Consumer, Library);
        var again = Run(cache, preserveOperators, Main, Consumer, Library);
        first.Declarations.Values.SelectMany(declaration => declaration.Errors).Should().BeEmpty();
        var run = DeclQualifiedName.Create(["Decode"], "run");
        var local = DeclQualifiedName.Create(["Decode"], "local");
        var importedDecode = DeclQualifiedName.Create(["Bytes", "Decode"], "decode");
        var ownDecode = DeclQualifiedName.Create(["Decode"], "decode");
        var importedType = DeclQualifiedName.Create(["Bytes", "Decode"], "Decoder");
        first.Dependencies[run].Should().BeEquivalentTo([importedDecode, importedType]);
        first.Dependencies[local].Should().Equal(ownDecode);

        first.References[run].Select(reference => reference.ReferencedName)
            .Should().Contain(DeclQualifiedName.Create(["Bytes", "Decode"], "Value"));

        again.Transformations[run].Should().BeSameAs(first.Transformations[run]);
        again.TransformationCount.Should().Be(0);

        var canonicalSources =
            new[] { Parse(Main), Parse(Consumer), Parse(Library) }
            .Select(
                file => file with
                {
                    Declarations =
                    [
                    .. file.Declarations.Select(
                        declaration =>
                        first.Declarations[DeclQualifiedName.Create(
                            Module.GetModuleName(file.ModuleDefinition.Value).Value,
                            Canonicalization.GetDeclarationName(declaration.Value))].Value)
                    ]
                }).ToArray();

        var frozen =
            Canonicalization.CanonicalizeDeclarations(
                canonicalSources,
                [s_root],
                implicitImports: null,
                cache,
                canonicalizedDependencies: first.Declarations.Keys.ToHashSet(),
                preserveOperatorApplications: preserveOperators)
            .Extract(error => throw new InvalidOperationException(error));

        frozen.Declarations.Values.SelectMany(declaration => declaration.Errors).Should().BeEmpty();
        frozen.Dependencies[run].Should().BeEquivalentTo(first.Dependencies[run]);
        frozen.Dependencies[local].Should().Equal(ownDecode);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Effect_interface_dependency_changes_invalidate_cached_consumers(bool preserveOperators)
    {
        const string Main = "module Main exposing (..)\nimport Effects\nroot = Effects.value";

        const string Effect =
            """
            effect module Effects where { subscription = MySub } exposing (value)
            type MySub msg = MySub msg
            type alias One = Int
            type alias Two = String
            value : One
            value = missingKernelImplementation
            """;

        var cache = new Canonicalization.DeclarationCache();
        var first = Run(cache, preserveOperators, Main, Effect);
        var same = Run(cache, preserveOperators, Main, Effect);
        same.Transformations[s_root].Should().BeSameAs(first.Transformations[s_root]);

        var changed =
            Run(cache, preserveOperators, Main, Effect.Replace("value : One", "value : Two", StringComparison.Ordinal));

        changed.Transformations[s_root].Should().NotBeSameAs(first.Transformations[s_root]);
        changed.Declarations.Should().ContainKey(DeclQualifiedName.Create(["Effects"], "Two"));
        changed.Declarations.Should().NotContainKey(DeclQualifiedName.Create(["Effects"], "One"));
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Semantic_inputs_reuse_independent_equal_syntax_and_ignore_unconsulted_scope_changes(
        bool preserveOperators)
    {
        const string Main = "module Main exposing (..)\nimport A exposing (..)\nimport B exposing (..)\nroot = value";
        const string A = "module A exposing (..)\nvalue = 1";
        var cache = new Canonicalization.DeclarationCache();
        var first = Run(cache, preserveOperators, Main, A, "module B exposing (..)\nunused = 2");
        var count = cache.Count;

        var second =
            Run(
                cache,
                preserveOperators,
                Main + "\nunrelated = 3",
                A,
                "module B exposing (..)\nunused = 2\nanother = 4",
                "module Unrelated exposing (..)\nvalue = 9");

        second.Transformations[s_root].Should().BeSameAs(first.Transformations[s_root]);

        second.Transformations[DeclQualifiedName.Create(["A"], "value")]
            .Should().BeSameAs(first.Transformations[DeclQualifiedName.Create(["A"], "value")]);

        cache.Count.Should().Be(count);

        var changedBody =
            Run(
                cache,
                preserveOperators,
                Main,
                A.Replace("value = 1", "value = 5", StringComparison.Ordinal),
                "module B exposing (..)\nunused = 2");

        changedBody.Transformations[s_root].Should().BeSameAs(first.Transformations[s_root]);
        cache.Count.Should().Be(count + 1);
    }

    public static TheoryData<string, string[], string[]> ChangedResolutionInputs =>
        new()
        {
            {
                "unqualified missing value becomes local",
                ["module Main exposing (..)\nroot = value"],
                ["module Main exposing (..)\nroot = value\nvalue = 1"]
            },
            {
                "imported value becomes ambiguous",
                [
                "module Main exposing (..)\nimport A exposing (..)\nimport B exposing (..)\nroot = value",
                "module A exposing (..)\nvalue = 1", "module B exposing (..)\nother = 2"
                ],
                [
                "module Main exposing (..)\nimport A exposing (..)\nimport B exposing (..)\nroot = value",
                "module A exposing (..)\nvalue = 1", "module B exposing (..)\nvalue = 2"
                ]
            },
            {
                "later explicit type import changes with import order",
                [
                "module Main exposing (..)\nimport A exposing (Item)\nimport B exposing (Item)\nroot : Item -> Item\nroot item = item",
                "module A exposing (..)\ntype Item = First", "module B exposing (..)\ntype Item = Second"
                ],
                [
                "module Main exposing (..)\nimport B exposing (Item)\nimport A exposing (Item)\nroot : Item -> Item\nroot item = item",
                "module A exposing (..)\ntype Item = First", "module B exposing (..)\ntype Item = Second"
                ]
            },
            {
                "qualified exposure is removed",
                ["module Main exposing (..)\nimport A\nroot = A.value", "module A exposing (..)\nvalue = 1"],
                [
                "module Main exposing (..)\nimport A\nroot = A.value",
                "module A exposing (other)\nvalue = 1\nother = 2"
                ]
            },
            {
                "constructor owning declaration changes",
                ["module Main exposing (..)\nimport A\nroot = A.Item", "module A exposing (..)\ntype First = Item"],
                ["module Main exposing (..)\nimport A\nroot = A.Item", "module A exposing (..)\ntype Second = Item"]
            },
            {
                "imported module namespace becomes duplicated",
                ["module Main exposing (..)\nimport A\nroot = A.value", "module A exposing (..)\nvalue = 1"],
                [
                "module Main exposing (..)\nimport A\nroot = A.value", "module A exposing (..)\nvalue = 1",
                "module A exposing (..)\nother = 2"
                ]
            },
            {
                "parameter starts shadowing a module binding",
                ["module Main exposing (..)\nroot argument = argument"],
                ["module Main exposing (..)\nroot argument = argument\nargument = 1"]
            },
            {
                "nested destructuring starts shadowing a module binding",
                ["module Main exposing (..)\nroot = let\n    ( first, second ) = ( 1, 2 )\n    in first"],
                ["module Main exposing (..)\nroot = let\n    ( first, second ) = ( 1, 2 )\n    in first\nsecond = 3"]
            },
            {
                "case alias starts shadowing a module binding",
                ["module Main exposing (..)\nroot item = case item of\n    (value as alias) -> value"],
                ["module Main exposing (..)\nroot item = case item of\n    (value as alias) -> value\nalias = 1"]
            },
            {
                "let function starts shadowing a module binding",
                ["module Main exposing (..)\nroot = let\n    local = 1\n    in local"],
                ["module Main exposing (..)\nroot = let\n    local = 1\n    in local\nlocal = 2"]
            },
            {
                "record update temporary collides with a new declaration",
                ["module Main exposing (..)\nroot = { base | field = 1 }\nbase = { field = 0 }"],
                [
                "module Main exposing (..)\nroot = { base | field = 1 }\nbase = { field = 0 }\nbase__recordUpdateBase = 2"
                ]
            },
            {
                "same name is no longer available in the value namespace",
                [
                "module Main exposing (..)\nimport A\nroot = A.Item",
                "module A exposing (..)\ntype alias Item = { field : Int }"
                ],
                ["module Main exposing (..)\nimport A\nroot = A.Item", "module A exposing (..)\ntype alias Item = Int"]
            },
            {
                "operator implementation mapping changes",
                [
                "module Main exposing (..)\nimport A exposing (..)\nroot = 1 + 2",
                "module A exposing (..)\ninfix left 5 (+) = first\nfirst a b = a\nsecond a b = b"
                ],
                [
                "module Main exposing (..)\nimport A exposing (..)\nroot = 1 + 2",
                "module A exposing (..)\ninfix left 5 (+) = second\nfirst a b = a\nsecond a b = b"
                ]
            },
            {
                "qualified alias target changes",
                [
                "module Main exposing (..)\nimport A as Choice\nroot = Choice.value",
                "module A exposing (..)\nvalue = 1", "module B exposing (..)\nvalue = 2"
                ],
                [
                "module Main exposing (..)\nimport B as Choice\nroot = Choice.value",
                "module A exposing (..)\nvalue = 1", "module B exposing (..)\nvalue = 2"
                ]
            }
        };

    [Theory]
    [MemberData(nameof(ChangedResolutionInputs))]
    public void Semantic_inputs_invalidate_exactly_when_a_consulted_resolution_changes(
        string reason, string[] before, string[] after)
    {
        Parse(after[0]).Declarations.Single(node => Canonicalization.GetDeclarationName(node.Value) == "root")
            .Should().Be(
            Parse(before[0]).Declarations.Single(node => Canonicalization.GetDeclarationName(node.Value) == "root"),
            "these cases must exercise changed resolution inputs, not changed declaration syntax");

        foreach (var preserveOperators in new[] { false, true })
        {
            var cache = new Canonicalization.DeclarationCache();
            var first = Run(cache, preserveOperators, before);
            var count = cache.Count;
            var second = Run(cache, preserveOperators, after);

            (second.UnitExports.GetValueOrDefault(s_root)?.Address !=
            first.UnitExports.GetValueOrDefault(s_root)?.Address)
                .Should().BeTrue(reason);

            cache.Count.Should().BeGreaterThanOrEqualTo(count);
            var cold = Run(new Canonicalization.DeclarationCache(), preserveOperators, after);
            second.Should().BeEquivalentTo(cold, options => options.Excluding(result => result.TransformationCount));
            var restored = Run(cache, preserveOperators, before);

            restored.Transformations[s_root].Should().BeSameAs(
                first.Transformations[s_root],
                "earlier input versions remain reusable");
        }
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public void Syntax_preserving_and_lowered_modes_never_share_cached_results(bool firstMode)
    {
        const string Source = "module Main exposing (..)\nroot = 1 + 2";
        var cache = new Canonicalization.DeclarationCache();
        var first = Run(cache, firstMode, Source);
        var other = Run(cache, !firstMode, Source);
        other.Transformations[s_root].Should().NotBeSameAs(first.Transformations[s_root]);
        other.Should().BeEquivalentTo(Run(new(), !firstMode, Source));
        Run(cache, firstMode, Source).Transformations[s_root].Should().BeSameAs(first.Transformations[s_root]);
        Run(cache, !firstMode, Source).Transformations[s_root].Should().BeSameAs(other.Transformations[s_root]);
        cache.Count.Should().Be(2);
    }

    [Fact]
    public async Task Trimming_removes_historical_observation_versions_and_their_index_entries()
    {
        const string Main = "module Main exposing (..)\nimport A\nroot = A.value";
        const string Available = "module A exposing (..)\nvalue = 1";
        const string Hidden = "module A exposing (other)\nvalue = 1\nother = 2";
        var cache = new Canonicalization.DeclarationCache();
        cache.SetMarker(1);
        var original = Run(cache, true, Main, Available);
        cache.SetMarker(2);
        var changed = Run(cache, true, Main, Hidden);
        var last = cache.SetMarker(3);
        Run(cache, true, Main, Available).Transformations[s_root].Should().BeSameAs(original.Transformations[s_root]);
        last.EntryCount.Should().BeGreaterThanOrEqualTo(2);

        await cache.TrimAsync(2, marker => new CacheRetention.Ranked(marker));
        cache.Count.Should().Be(2);
        Run(cache, true, Main, Available).Transformations[s_root].Should().BeSameAs(original.Transformations[s_root]);
        Run(cache, true, Main, Hidden).Transformations[s_root].Should().NotBeSameAs(changed.Transformations[s_root]);
        await cache.TrimAsync(0, _ => new CacheRetention.Remove());
        cache.Count.Should().Be(0);
        Run(cache, true, Main, Available).Transformations[s_root].Should().NotBeSameAs(original.Transformations[s_root]);
    }

    [Fact]
    public void Semantic_inputs_distinguish_type_and_value_lookups_without_invalidating_unused_namespaces()
    {
        const string Main = "module Main exposing (..)\nimport A\nroot : A.Item -> A.Item\nroot item = item";
        var cache = new Canonicalization.DeclarationCache();
        var first = Run(cache, Main, "module A exposing (..)\ntype alias Item = { field : Int }");
        var second = Run(cache, Main, "module A exposing (..)\ntype alias Item = Int");
        second.Transformations[s_root].Should().BeSameAs(first.Transformations[s_root]);
    }

    [Fact]
    public void Semantic_inputs_reuse_when_an_unused_import_or_alias_changes()
    {
        var cache = new Canonicalization.DeclarationCache();
        const string A = "module A exposing (..)\nvalue = 1";
        const string B = "module B exposing (..)\nother = 2";
        const string C = "module C exposing (..)\nother = 3";
        var first = Run(cache, "module Main exposing (..)\nimport A\nimport B as Unused\nroot = A.value", A, B, C);
        var count = cache.Count;
        var second = Run(cache, "module Main exposing (..)\nimport A\nimport C as Ignore\nroot = A.value", A, B, C);
        second.Transformations[s_root].Should().BeSameAs(first.Transformations[s_root]);
        cache.Count.Should().Be(count);
    }

    [Fact]
    public void Semantic_inputs_include_configurable_native_availability()
    {
        var source = Parse("module Main exposing (..)\nroot = Basics.customNative");
        var cache = new Canonicalization.DeclarationCache();
        var original = ImplicitImportConfig.Default;
        var changed = original with { ValueImports = original.ValueImports.SetItem("customNative", ["Basics"]) };

        DeclarationCanonicalizationResult RunWith(ImplicitImportConfig configuration) =>
            Canonicalization.CanonicalizeDeclarations([source], [s_root], configuration, cache)
            .Extract(error => throw new InvalidOperationException(error));

        var first = RunWith(original);
        first.Declarations[s_root].Errors.Should().NotBeEmpty();
        var second = RunWith(changed);
        second.Declarations[s_root].Errors.Should().BeEmpty();
        RunWith(original).Transformations[s_root].Should().BeSameAs(first.Transformations[s_root]);
        RunWith(changed).Transformations[s_root].Should().BeSameAs(second.Transformations[s_root]);
        cache.Count.Should().Be(2);
    }

    [Fact]
    public void Semantic_key_hashes_are_cached_and_do_not_affect_equality_or_survive_changed_clones()
    {
        var name = DeclQualifiedName.Create(["Library", "Nested"], "value");
        var same = DeclQualifiedName.Create(["Library", "Nested"], "value");
        var hash = name.GetHashCode();
        same.Should().Be(name);
        same.GetHashCode().Should().Be(hash);
        var clone = name with { DeclName = "changed", FullName = "Library.Nested.changed" };
        clone.GetHashCode().Should().Be(DeclQualifiedName.Create(["Library", "Nested"], "changed").GetHashCode());
        clone.Should().NotBe(name);
        var namespaces = new[] { "Library", "Nested" };
        var frozen = DeclQualifiedName.Create(namespaces, "value");
        namespaces[0] = "Mutated";
        frozen.Namespaces.Should().Equal("Library", "Nested");
        frozen.GetHashCode().Should().Be(hash);

        var query = new Canonicalization.InputQuery(Canonicalization.InputKind.Alias, "Library");

        var otherQuery =
            new Canonicalization.InputQuery(Canonicalization.InputKind.Alias, new string("Library".ToCharArray()));

        query.Should().Be(otherQuery);
        otherQuery.GetHashCode().Should().Be(query.GetHashCode());
        var imported = ImportedModule.WithAlias(["Library", "Nested"], "L");
        var importedHash = imported.GetHashCode();

        for (var repeat = 0; repeat < 100; ++repeat)
        {
            name.GetHashCode().Should().Be(hash);
            imported.GetHashCode().Should().Be(importedHash);
            query.GetHashCode().Should().Be(otherQuery.GetHashCode());
        }

        imported.Should().Be(ImportedModule.WithAlias(["Library", "Nested"], "L"));

        var countedNamespaces = new CountingNamespaces();
        var counted = name with { Namespaces = countedNamespaces };
        counted.GetHashCode().Should().Be(hash);
        countedNamespaces.IndexReads.Should().Be(countedNamespaces.Count);
        countedNamespaces.Enumerations.Should().Be(0);

        for (var repeat = 0; repeat < 100; ++repeat)
            counted.GetHashCode().Should().Be(hash);

        countedNamespaces.IndexReads.Should().Be(countedNamespaces.Count);
        countedNamespaces.Enumerations.Should().Be(0);
    }

    [Fact]
    public void Semantic_input_lookup_is_allocation_free_after_warming_hashes()
    {
        var declaration =
            Core.Elm.Syntax.Abstract.ConvertFromConcrete.FromDeclaration(
                Parse("module Main exposing (..)\nroot = 1").Declarations.Single().Value);

        var input =
            new Core.Elm.ElmCompilerInDotnet.DeclarationUnits.CanonicalizationInput(declaration, [], false);

        var cache = new Canonicalization.DeclarationCache();

        cache.Semantic.Canonical.Set(
            input,
            Core.Elm.ElmCompilerInDotnet.DeclarationUnits.Canonicalize.Apply(input));

        for (var warmup = 0; warmup < 100; ++warmup)
            cache.Semantic.Canonical.TryGetValue(input, out _).Should().BeTrue();

        var allocatedBefore = GC.GetAllocatedBytesForCurrentThread();
        var hits = 0;

        for (var repeat = 0; repeat < 10_000; ++repeat)
        {
            if (cache.Semantic.Canonical.TryGetValue(input, out _))
                ++hits;
        }

        var allocated = GC.GetAllocatedBytesForCurrentThread() - allocatedBefore;
        hits.Should().Be(10_000);
        allocated.Should().Be(0);
        cache.Semantic.Canonical.TryGetValue(input with { PreserveOperators = true }, out _).Should().BeFalse();
    }

    [Fact]
    public async Task Semantic_cache_supports_shared_callers_and_explicit_clearing()
    {
        var cache = new Canonicalization.DeclarationCache();
        const string Source = "module Main exposing (..)\nroot = value\nvalue = 1";
        var results = await Task.WhenAll(Enumerable.Range(0, 8).Select(_ => Task.Run(() => Run(cache, Source))));

        foreach (var result in results)
            result.Declarations[s_root].Errors.Should().BeEmpty();

        cache.Count.Should().Be(2);
        cache.Clear();
        cache.Count.Should().Be(0);
        var afterClear = Run(cache, Source);
        afterClear.Transformations[s_root].Should().NotBeSameAs(results[0].Transformations[s_root]);
        cache.Count.Should().Be(2);
    }

    private sealed class CountingNamespaces : IReadOnlyList<string>
    {
        private readonly string[] _parts = ["Library", "Nested"];

        public int Enumerations { get; private set; }

        public int IndexReads { get; private set; }

        public int Count => _parts.Length;

        public string this[int index]
        {
            get
            {
                ++IndexReads;
                return _parts[index];
            }
        }

        public IEnumerator<string> GetEnumerator()
        {
            ++Enumerations;
            return ((IEnumerable<string>)_parts).GetEnumerator();
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() => GetEnumerator();
    }
}
