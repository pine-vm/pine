using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Testing;
using Pine.Core.Tests.Elm.LanguageServer;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.Json;
using System.Threading.Tasks;
using Xunit;

using Model = Pine.Core.Elm.ElmCompilerInDotnet.CompilerModel;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class SccTransformationCacheTests
{
    private static Dictionary<string, UnitExport> Canonical(
        string source, Canonicalization.DeclarationCache cache)
    {
        var parsed =
            ElmSyntaxParser.ParseModuleText("module Test exposing (..)\n" + source)
            .Extract(error => throw new InvalidOperationException(error.ToString()));

        return
            Canonicalization.CanonicalizeDeclarations(
                [parsed],
                [Name("helper"), Name("caller"), Name("stable")],
                cache: cache)
            .Extract(error => throw new InvalidOperationException(error))
            .UnitExports.ToDictionary(entry => entry.Key.DeclName, entry => entry.Value);
    }

    private static DeclQualifiedName Name(string name) => DeclQualifiedName.Create(["Test"], name);

    private static ImmutableDictionary<string, UnitExport> Transform(
        IReadOnlyDictionary<string, UnitExport> roots,
        DeclarationUnitCache cache,
        bool lowerOperators = true) =>
        SccCompiler.Transform(roots, lowerOperators, false, false, cache)
        .Extract(error => throw new InvalidOperationException(error));

    [Fact]
    public void Body_edits_invalidate_dependent_SCCs_but_reuse_independent_transforms_and_models()
    {
        const string source = "helper n = n + 1\ncaller n = helper n\nstable n = n";
        var cache = new Canonicalization.DeclarationCache();
        var original = Canonical(source, cache);
        var transformed = Transform(original, cache.Semantic);
        var before = SccCompiler.Lower(transformed.Values, cache.Semantic);
        var stable = transformed["stable"];
        cache.Semantic.CompilerModels.TryGetValue(stable.Unit, out var stableModel).Should().BeTrue();
        var passes = cache.Semantic.SyntaxTransforms.Count;
        var models = cache.Semantic.CompilerModels.Count;

        var repeated = Transform(Canonical(source, cache), cache.Semantic);
        var warm = SccCompiler.Lower(repeated.Values, cache.Semantic);
        cache.Semantic.SyntaxTransforms.Count.Should().Be(passes);
        cache.Semantic.CompilerModels.Count.Should().Be(models);

        foreach (var (id, function) in before.Functions)
            warm.Functions[id].Should().BeSameAs(function);

        var edited = Canonical(source.Replace("n + 1", "n + 2", StringComparison.Ordinal), cache);
        edited["helper"].Unit.Should().NotBe(original["helper"].Unit);
        edited["caller"].Unit.Should().NotBe(original["caller"].Unit);
        edited["stable"].Unit.Should().BeSameAs(original["stable"].Unit);
        var changed = Transform(edited, cache.Semantic);
        changed["stable"].Should().BeSameAs(stable);
        var actual = SccCompiler.Lower(changed.Values, cache.Semantic);
        cache.Semantic.CompilerModels.TryGetValue(stable.Unit, out var reused).Should().BeTrue();
        reused.Should().BeSameAs(stableModel);
        cache.Semantic.SyntaxTransforms.Count.Should().Be(passes + 2);
        cache.Semantic.CompilerModels.Count.Should().Be(models + 2);

        var fresh = SccCompiler.Lower(Transform(edited, new()).Values);
        AssertEquivalent(actual, fresh);
        Model.ProgramValidation.Validate(actual);
    }

    [Fact]
    public void Syntax_options_have_independent_SCC_cache_entries()
    {
        var canonical =
            Canonical(
                "helper : Int -> Int\nhelper n = n + 1\ncaller n = helper n\nstable n = n",
                new());

        var cache = new DeclarationUnitCache();
        var unchanged = Transform(canonical, cache, lowerOperators: false);
        var lowered = Transform(canonical, cache);
        lowered["helper"].Unit.Should().NotBe(unchanged["helper"].Unit);
        var count = cache.SyntaxTransforms.Count;
        Transform(canonical, cache, lowerOperators: false)["helper"].Should().BeSameAs(unchanged["helper"]);
        Transform(canonical, cache)["helper"].Should().BeSameAs(lowered["helper"]);
        cache.SyntaxTransforms.Count.Should().Be(count);
    }

    [Fact]
    public void Independent_new_root_prepares_only_its_missing_SCC_not_cached_siblings()
    {
        var cache = new Canonicalization.DeclarationCache();
        var original = Canonical("helper n = n + 1\ncaller n = helper n\nstable n = n", cache);
        _ = SccCompiler.Lower(Transform(original, cache.Semantic).Values, cache.Semantic);

        var parsed =
            ElmSyntaxParser.ParseModuleText("module Added exposing (..)\nvalue = 42")
            .Extract(error => throw new InvalidOperationException(error.ToString()));

        var added =
            Canonicalization.CanonicalizeDeclarations(
                [parsed],
                [DeclQualifiedName.Create(["Added"], "value")],
                cache: cache)
            .Extract(error => throw new InvalidOperationException(error)).UnitExports.Single().Value;

        original.Add("added", added);

        cache.Semantic.CompilerViews.Clear();
        var transformed = Transform(original, cache.Semantic);

        cache.Semantic.CompilerViews.Count.Should().Be(
            1,
            "cached sibling SCCs must not be revisited to prepare one independent syntax miss");

        cache.Semantic.CompilerViews.Clear();
        var program = SccCompiler.Lower(transformed.Values, cache.Semantic);

        cache.Semantic.CompilerViews.Count.Should().Be(
            1,
            "model conversion must prepare only the missing SCC's dependency closure");

        Model.ProgramValidation.Validate(program);
    }

    [Theory]
    [InlineData("type alias Shape = { x : Int }", "type alias Shape = { x : Int, y : Int }",
        "helper : Shape -> Int\nhelper r = r.x")]
    [InlineData("type Shape = Shape Int", "type Shape = Empty | Shape Int",
        "helper n = Shape n")]
    public void Type_and_constructor_edits_do_not_reuse_stale_lowering(
        string oldType, string newType, string helper)
    {
        var source = oldType + "\n" + helper + "\ncaller n = helper n\nstable n = n";
        var cache = new Canonicalization.DeclarationCache();
        var original = Canonical(source, cache);
        var transformed = Transform(original, cache.Semantic);
        _ = SccCompiler.Lower(transformed.Values, cache.Semantic);
        var edited = Canonical(source.Replace(oldType, newType, StringComparison.Ordinal), cache);
        edited["helper"].Unit.Should().NotBe(original["helper"].Unit);
        var changed = Transform(edited, cache.Semantic);
        changed["stable"].Should().BeSameAs(transformed["stable"]);

        AssertEquivalent(
            SccCompiler.Lower(changed.Values, cache.Semantic),
            SccCompiler.Lower(Transform(edited, new()).Values));
    }

    private static void AssertEquivalent(Model.Program actual, Model.Program expected)
    {
        var options = Model.CompilerModelJson.CreateOptions();
        JsonSerializer.Serialize(actual, options).Should().Be(JsonSerializer.Serialize(expected, options));
    }

    [Fact]
    public async Task Runner_SCC_reuse_does_not_merge_captures_or_recursive_closures()
    {
        const string source =
            """
            make expected =
                let
                    even n = if n == 0 then expected else odd (n - 1)
                    odd n = if n == 0 then expected + 1 else even (n - 1)
                    check = Expect.equal expected
                    state = { wrap = Just, read = .answer, value = { answer = expected } }
                in
                \_ ->
                    Expect.all
                        [ \_ -> check (even 4)
                        , \_ -> Expect.equal (Just expected) (state.wrap (state.read state.value))
                        ]
                        ()
            suite =
                Test.describe "captures"
                    [ Test.test "one" (make 3)
                    , Test.test "two" (make 7)
                    , Test.fuzz Fuzz.int "fuzz" (\n -> Expect.equal n n)
                    ]
            """;

        var caches = ElmTestDiscoveryCaches.CreateInMemory();
        await using var session = new ElmTestRunCacheSession(discoveryCaches: caches);

        var discovery =
            ElmTestDiscovery.DiscoverTests(
                LanguageServerTestingTests.Tree(source),
                executionSettings: new(123456, 10),
                caches: caches);

        int? models = null;

        for (var round = 0; round < 2; round++)
        {
            using var invocation = session.BeginCompilationInvocation();
            var entries = discovery.PrepareExecution(canonicalizationCache: session.DeclarationCanonicalization);
            entries.Should().HaveCount(3);

            foreach (var test in discovery.Items.Where(item => entries.ContainsKey(item.Id)))
            {
                ElmTestRunner.ExecuteDiscoveredTest(test, entries[test.Id], session, default)
                    .Kind.Should().Be(CompletedTestKind.Passed);
            }

            if (models is { } count)
                session.DeclarationCanonicalization.Semantic.CompilerModels.Count.Should().Be(count);

            models = session.DeclarationCanonicalization.Semantic.CompilerModels.Count;
            models.Value.Should().BeGreaterThan(0);
        }
    }
}
