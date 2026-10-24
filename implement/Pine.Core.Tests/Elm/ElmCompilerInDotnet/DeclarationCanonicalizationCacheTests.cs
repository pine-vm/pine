using AwesomeAssertions;
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
    private static readonly DeclQualifiedName Root = DeclQualifiedName.Create(["Main"], "root");

    private static File Parse(string text) =>
        ElmSyntaxParser.ParseModuleText(text).Extract(error => throw new InvalidOperationException(error.ToString()));

    private static DeclarationCanonicalizationResult Run(
        Canonicalization.DeclarationCache cache, params string[] modules) =>
        Canonicalization.CanonicalizeDeclarations([.. modules.Select(Parse)], [Root], cache: cache)
        .Extract(error => throw new InvalidOperationException(error));

    [Fact]
    public void Semantic_inputs_reuse_independent_equal_syntax_and_ignore_unconsulted_scope_changes()
    {
        const string main = "module Main exposing (..)\nimport A exposing (..)\nimport B exposing (..)\nroot = value";
        const string a = "module A exposing (..)\nvalue = 1";
        var cache = new Canonicalization.DeclarationCache();
        var first = Run(cache, main, a, "module B exposing (..)\nunused = 2");
        var count = cache.Count;

        var second =
            Run(
                cache,
                main + "\nunrelated = 3",
                a,
                "module B exposing (..)\nunused = 2\nanother = 4",
                "module Unrelated exposing (..)\nvalue = 9");

        second.Declarations[Root].Should().BeSameAs(first.Declarations[Root]);

        second.Declarations[DeclQualifiedName.Create(["A"], "value")]
            .Should().BeSameAs(first.Declarations[DeclQualifiedName.Create(["A"], "value")]);

        cache.Count.Should().Be(count);

        var changedBody =
            Run(
                cache,
                main,
                a.Replace("value = 1", "value = 5", StringComparison.Ordinal),
                "module B exposing (..)\nunused = 2");

        changedBody.Declarations[Root].Should().BeSameAs(first.Declarations[Root]);
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

        var cache = new Canonicalization.DeclarationCache();
        var first = Run(cache, before);
        var count = cache.Count;
        var second = Run(cache, after);
        second.Declarations[Root].Should().NotBeSameAs(first.Declarations[Root], reason);
        cache.Count.Should().BeGreaterThan(count);
        var cold = Run(new Canonicalization.DeclarationCache(), after);
        second.Should().BeEquivalentTo(cold);
        var restored = Run(cache, before);

        restored.Declarations[Root].Should().BeSameAs(
            first.Declarations[Root],
            "earlier input versions remain reusable");
    }

    [Fact]
    public void Semantic_inputs_distinguish_type_and_value_lookups_without_invalidating_unused_namespaces()
    {
        const string main = "module Main exposing (..)\nimport A\nroot : A.Item -> A.Item\nroot item = item";
        var cache = new Canonicalization.DeclarationCache();
        var first = Run(cache, main, "module A exposing (..)\ntype alias Item = { field : Int }");
        var second = Run(cache, main, "module A exposing (..)\ntype alias Item = Int");
        second.Declarations[Root].Should().BeSameAs(first.Declarations[Root]);
    }

    [Fact]
    public void Semantic_inputs_reuse_when_an_unused_import_or_alias_changes()
    {
        var cache = new Canonicalization.DeclarationCache();
        const string a = "module A exposing (..)\nvalue = 1";
        const string b = "module B exposing (..)\nother = 2";
        const string c = "module C exposing (..)\nother = 3";
        var first = Run(cache, "module Main exposing (..)\nimport A\nimport B as Unused\nroot = A.value", a, b, c);
        var count = cache.Count;
        var second = Run(cache, "module Main exposing (..)\nimport A\nimport C as Ignore\nroot = A.value", a, b, c);
        second.Declarations[Root].Should().BeSameAs(first.Declarations[Root]);
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
            Canonicalization.CanonicalizeDeclarations([source], [Root], configuration, cache)
            .Extract(error => throw new InvalidOperationException(error));

        var first = RunWith(original);
        first.Declarations[Root].Errors.Should().NotBeEmpty();
        var second = RunWith(changed);
        second.Declarations[Root].Errors.Should().BeEmpty();
        RunWith(original).Declarations[Root].Should().BeSameAs(first.Declarations[Root]);
        RunWith(changed).Declarations[Root].Should().BeSameAs(second.Declarations[Root]);
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
        countedNamespaces.Enumerations.Should().Be(1);

        for (var repeat = 0; repeat < 100; ++repeat)
            counted.GetHashCode().Should().Be(hash);

        countedNamespaces.Enumerations.Should().Be(1);
    }

    [Fact]
    public void Semantic_input_lookup_is_allocation_free_after_warming_hashes()
    {
        var declaration = Parse("module Main exposing (..)\nroot = 1").Declarations.Single();

        Canonicalization.CanonicalizationContext Context() =>
            new(
                ["Main"],
                ImmutableDictionary<string, ImmutableList<IReadOnlyList<string>>>.Empty.Add("Item", [["A"]]),
                ImmutableDictionary<string, ImmutableList<IReadOnlyList<string>>>.Empty.Add("value", [["A"]]),
                ImmutableDictionary<string, IReadOnlyList<string>>.Empty.Add("A", ["A"]),
                ["root"],
                [],
                ImmutableDictionary<string, (IReadOnlyList<string>, string)>.Empty.Add("+", (["Basics"], "add")),
                []);

        var inputs = new Canonicalization.InputObservations();
        var observed = Context() with { Inputs = inputs };
        observed.ContainsModuleDeclaration("root");
        observed.ContainsModuleDeclaration("absent");
        observed.TryGetImport(true, "Item", out _);
        observed.TryGetImport(false, "value", out _);
        observed.TryGetImport(false, "absent", out _);
        observed.TryGetAlias("A", out _);
        observed.TryGetAlias("absent", out _);
        observed.TryGetOperator("+", out _);
        observed.TryGetOperator("-", out _);

        var cache = new Canonicalization.DeclarationCache();
        cache.Add(declaration, observed, new(new(declaration, []), []), inputs);
        var context = Context();

        for (var warmup = 0; warmup < 100; ++warmup)
            cache.TryGet(declaration, context, out _).Should().BeTrue();

        var allocatedBefore = GC.GetAllocatedBytesForCurrentThread();
        var hits = 0;

        for (var repeat = 0; repeat < 10_000; ++repeat)
        {
            if (cache.TryGet(declaration, context, out _))
                ++hits;
        }

        var allocated = GC.GetAllocatedBytesForCurrentThread() - allocatedBefore;
        hits.Should().Be(10_000);
        allocated.Should().Be(0);
        cache.TryGet(declaration, context with { CurrentModuleName = ["Other"] }, out _).Should().BeFalse();

        cache.TryGet(
            declaration,
            context with
            {
                AliasMap = context.AliasMap.SetItem("A", ["Different"])
            },
            out _).Should().BeFalse();
    }

    [Fact]
    public async Task Semantic_cache_supports_shared_callers_and_explicit_clearing()
    {
        var cache = new Canonicalization.DeclarationCache();
        const string source = "module Main exposing (..)\nroot = value\nvalue = 1";
        var results = await Task.WhenAll(Enumerable.Range(0, 8).Select(_ => Task.Run(() => Run(cache, source))));

        foreach (var result in results)
            result.Declarations[Root].Errors.Should().BeEmpty();

        cache.Count.Should().Be(2);
        cache.Clear();
        cache.Count.Should().Be(0);
        var afterClear = Run(cache, source);
        afterClear.Declarations[Root].Should().NotBeSameAs(results[0].Declarations[Root]);
        cache.Count.Should().Be(2);
    }

    private sealed class CountingNamespaces : IReadOnlyList<string>
    {
        private readonly string[] parts = ["Library", "Nested"];

        public int Enumerations { get; private set; }

        public int Count => parts.Length;

        public string this[int index] => parts[index];

        public IEnumerator<string> GetEnumerator()
        {
            ++Enumerations;
            return ((IEnumerable<string>)parts).GetEnumerator();
        }

        System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() => GetEnumerator();
    }
}
