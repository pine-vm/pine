using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmCompilerInDotnet.DeclarationUnits;
using Pine.Core.Elm.Syntax;
using Pine.Core.Encodings;
using Pine.Core.Files;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using AST = Pine.Core.Elm.Syntax.Abstract;
using CST = Pine.Core.Elm.Syntax.Concrete;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class DeclarationUnitTests
{
    private static DeclQualifiedName Name(string module, string symbol) =>
        DeclQualifiedName.Create(module.Split('.'), symbol);

    private static CST.File Parse(string text) =>
        ElmSyntaxParser.ParseModuleText(text).Extract(error => throw new InvalidOperationException(error.ToString()));

    private static DeclarationCanonicalizationResult Canonical(
        Canonicalization.DeclarationCache cache, string[] texts, params DeclQualifiedName[] roots) =>
        Canonicalization.CanonicalizeDeclarations([.. texts.Select(Parse)], roots, cache: cache)
        .Extract(error => throw new InvalidOperationException(error));

    private sealed record Package(ElmPackageIdentity Identity, FileTree Sources);

    /// <summary>A closed, deterministic in-memory application and published-package world.</summary>
    internal sealed class World : IElmPackageProvider
    {
        private readonly Dictionary<ElmPackageIdentity, Package> _packages = [];

        private readonly Dictionary<string, string> _direct = new(StringComparer.Ordinal);

        private readonly Dictionary<string, string> _selected = new(StringComparer.Ordinal);

        public World Add(
            string name,
            string version,
            string[] modules,
            string[] exposed,
            string[]? dependencies = null,
            bool isDirect = true)
        {
            dependencies ??= [];
            var identity = new ElmPackageIdentity(name, ElmPackageVersion.Parse(version));

            var manifest =
                JsonSerializer.Serialize(
                    new Dictionary<string, object>
                    {
                        ["type"] = "package",
                        ["name"] = name,
                        ["version"] = version,
                        ["summary"] = "In-memory declaration unit test",
                        ["license"] = "BSD-3-Clause",
                        ["exposed-modules"] = exposed,
                        ["elm-version"] = "0.19.0 <= v < 0.20.0",
                        ["dependencies"] =
                        dependencies.ToDictionary(dependency => dependency, _ => "1.0.0 <= v < 2.0.0"),
                        ["test-dependencies"] = new Dictionary<string, string>()
                    });

            _packages.Add(identity, new(identity, Tree(manifest, modules)));
            _selected[name] = version;

            if (isDirect)
                _direct[name] = version;

            return this;
        }

        public async Task<ElmResolvedBuild> Prepare(string app, params ElmModuleNameMapping[] mappings)
        {
            var manifest =
                JsonSerializer.Serialize(
                    new Dictionary<string, object>
                    {
                        ["type"] = "application",
                        ["source-directories"] = new[] { "src" },
                        ["elm-version"] = "0.19.1",
                        ["dependencies"] =
                        new { direct = _direct, indirect = _selected.Where(pair => !_direct.ContainsKey(pair.Key)).ToDictionary() },
                        ["test-dependencies"] =
                        new { direct = new Dictionary<string, string>(), indirect = new Dictionary<string, string>() }
                    });

            return
                await ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                    Tree(manifest, [app]),
                    ["elm.json"],
                    [["src", "Main.elm"]],
                    new() { Offline = true, ModuleNameMappings = [.. mappings] },
                    this);
        }

        private static FileTree Tree(string manifest, string[] modules) =>
            FileTree.FromSetOfFilesWithStringPath(
                modules.Select(
                    text =>
                    {
                        var name = CST.Module.GetModuleName(Parse(text).ModuleDefinition.Value).Value;

                        return
                            ((IReadOnlyList<string>)["src", .. name.SkipLast(1), name[^1] + ".elm"],
                            (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(text));
                    })
                .Prepend((new[] { "elm.json" }, (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(manifest))));

        public Task<ElmPackageVersionListing> GetVersionsAsync(string name, CancellationToken cancellationToken) =>
            Task.FromResult(
                new ElmPackageVersionListing(
                    [.. _packages.Keys.Where(key => key.Name == name).Select(key => key.Version)],
                    "memory",
                    true));

        public Task<ElmPackageMetadata> GetMetadataAsync(
            ElmPackageIdentity identity,
            CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            var sources = _packages[identity].Sources;

            return
                Task.FromResult(
                    new ElmPackageMetadata(
                        identity,
                        ElmDependencyResolver.ReadManifest(sources, ["elm.json"]),
                        "memory"));
        }

        public Task<FileTree> GetSourcesAsync(ElmPackageIdentity identity, CancellationToken cancellationToken)
        {
            cancellationToken.ThrowIfCancellationRequested();
            return Task.FromResult(_packages[identity].Sources);
        }
    }

    private static DeclarationCanonicalizationResult Canonical(
        ElmResolvedBuild build, Canonicalization.DeclarationCache cache, string root = "root") =>
        Canonicalization.CanonicalizeDeclarations(
            [.. build.CompilerModuleSyntax.Values],
            [Name(build.CompilerModuleNames["src/Main.elm"], root)],
            cache: cache,
            moduleTypeInterfaces: build.CompilerModuleTypeInterfaces)
        .Extract(error => throw new InvalidOperationException(error));

    private static async Task<(ElmResolvedBuild Build, DeclarationCanonicalizationResult Canonical)> Fork(
        string package, string version, int answer, Canonicalization.DeclarationCache cache)
    {
        var world =
            new World().Add(
                package,
                version,
                [$"module Library exposing (answer)\nanswer = {answer}\nunused = Missing.value"],
                ["Library"]);

        var build =
            await world.Prepare("module Main exposing (root)\nimport Library as Selected\nroot _ = Selected.answer");

        return (build, Canonical(build, cache));
    }

    [Fact]
    public async Task Fork_and_version_labels_do_not_change_templates_closed_units_or_emission()
    {
        var cache = new Canonicalization.DeclarationCache();
        var first = await Fork("original/library", "1.0.0", 42, cache);
        var fork = await Fork("different/fork", "1.0.7", 42, cache);
        var root = Name("Main", "root");
        fork.Canonical.TransformationCount.Should().Be(0);
        fork.Canonical.UnitExports[root].Unit.Should().BeSameAs(first.Canonical.UnitExports[root].Unit);
        fork.Canonical.Transformations[root].Should().BeSameAs(first.Canonical.Transformations[root]);

        var initial =
            ElmCompiler.CompileResolvedEnvironment(first.Build, [root], canonicalizationCache: cache)
            .Extract(error => throw new InvalidOperationException(error));

        var emissionCount = cache.Semantic.Emission.Count;

        var changed =
            ElmCompiler.CompileResolvedEnvironment(fork.Build, [root], canonicalizationCache: cache)
            .Extract(error => throw new InvalidOperationException(error));

        changed.compiledEnvValue.Should().Be(initial.compiledEnvValue);
        cache.Semantic.Emission.Count.Should().Be(emissionCount);

        var encodedModule =
            changed.compiledEnvValue.Should().BeOfType<PineValue.ListValue>().Subject.Items.Span[0]
            .Should().BeOfType<PineValue.ListValue>().Subject.Items.Span[1];

        var entry =
            encodedModule.Should().BeOfType<PineValue.ListValue>().Subject.Items.Span[0]
            .Should().BeOfType<PineValue.ListValue>().Subject.Items.Span[1];

        ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(entry, new PineVMParseCache())([PineValue.EmptyList])
            .returnValue.Should().Be(IntegerEncoding.EncodeSignedInteger(42));

        var third = await Fork("different/fork", "1.0.8", 43, cache);
        third.Canonical.Transformations[root].Should().BeSameAs(first.Canonical.Transformations[root]);
        third.Canonical.UnitExports[root].Address.Should().NotBe(first.Canonical.UnitExports[root].Address);
    }

    [Fact]
    public async Task Equal_dependency_artifacts_are_shared_across_a_package_diamond()
    {
        var world =
            new World()
            .Add("test/shared", "1.0.0", ["module Shared exposing (answer)\nanswer = 42"], ["Shared"], isDirect: false)
            .Add(
                "test/left",
                "1.0.0",
                [
                "module Left exposing (answer)\nimport Shared\nanswer = Shared.answer"
                ],
                [
                "Left"
                ],
                [
                "test/shared"
                ])
            .Add(
                "test/right",
                "1.0.0",
                [
                "module Right exposing (answer)\nimport Shared\nanswer = Shared.answer"
                ],
                [
                "Right"
                ],
                [
                "test/shared"
                ]);

        var build =
            await world.Prepare(
                "module Main exposing (root)\nimport Left\nimport Right\nroot = ( Left.answer, Right.answer )");

        var cache = new Canonicalization.DeclarationCache();
        var result = Canonical(build, cache);
        result.TransformationCount.Should().BeLessThanOrEqualTo(4);
        result.Declarations.Keys.Count(name => name.DeclName == "answer").Should().Be(3);
        var shared = result.UnitExports.Single(pair => pair.Key.Namespaces[^1] == "Shared").Value;
        var consumers = result.UnitExports.Where(pair => pair.Key.Namespaces[^1] is "Left" or "Right").ToArray();
        consumers.Should().HaveCount(2);

        foreach (var consumer in consumers)
        {
            consumer.Value.Unit.Should().BeOfType<SccModel.ValueScc>().Which.Value.Dependencies.Values.Single()
                .Should().BeSameAs(shared.Unit);
        }

        UnitConstruction.Flatten([result.UnitExports[Name("Main", "root")]]).Count.Should().Be(
            3,
            "structurally equivalent forwarding functions share one retained declaration");
    }

    [Fact]
    public async Task Private_module_collisions_are_isolated_to_each_importing_package()
    {
        var world =
            new World()
            .Add(
                "test/left",
                "1.0.0",
                [
                    "module Left exposing (answer)\nimport Internal\nanswer = Internal.value",
                    "module Internal exposing (value)\nvalue = 11"
                ],
                ["Left"])
            .Add(
                "test/right",
                "1.0.0",
                [
                    "module Right exposing (answer)\nimport Internal\nanswer = Internal.value",
                    "module Internal exposing (value)\nvalue = 22"
                ],
                ["Right"]);

        var build =
            await world.Prepare(
                "module Main exposing (root)\nimport Left\nimport Right\nroot _ = ( Left.answer, Right.answer )");

        var result = Canonical(build, new());
        result.Declarations.Values.SelectMany(declaration => declaration.Errors).Should().BeEmpty();

        result.UnitExports
            .Where(pair => pair.Key.DeclName == "value").Select(pair => pair.Value.Address).Distinct().Count().Should()
            .Be(2);

        var bad = await world.Prepare("module Main exposing (root)\nimport Internal\nroot = Internal.value");
        Canonical(bad, new()).Declarations[Name("Main", "root")].Errors.Should().NotBeEmpty();
    }

    [Fact]
    public async Task Exposed_module_collisions_in_separate_package_scopes_do_not_collide()
    {
        var world =
            new World()
            .Add(
                "test/first-provider",
                "1.0.0",
                [
                "module Common exposing (value)\nvalue = 11"
                ],
                [
                "Common"
                ],
                isDirect: false)
            .Add(
                "test/second-provider",
                "1.0.0",
                [
                "module Common exposing (value)\nvalue = 22"
                ],
                [
                "Common"
                ],
                isDirect: false)
            .Add(
                "test/left",
                "1.0.0",
                ["module Left exposing (answer)\nimport Common\nanswer = Common.value"],
                ["Left"],
                ["test/first-provider"])
            .Add(
                "test/right",
                "1.0.0",
                ["module Right exposing (answer)\nimport Common\nanswer = Common.value"],
                ["Right"],
                ["test/second-provider"]);

        var build =
            await world.Prepare(
                "module Main exposing (root)\nimport Left\nimport Right\nroot = ( Left.answer, Right.answer )");

        build.ImportDiagnostics.Should().BeEmpty();

        var result =
            ElmCompiler.CompileResolvedEnvironment(build, [Name("Main", "root")])
            .Extract(error => throw new InvalidOperationException(error));

        var evaluated =
            ElmSourceCompilation.EvaluateZeroParameterRoots(
                result.compiledEnvValue,
                Core.Runtime.DirectInterpreter.WithLocalEvalCache(new PineVMParseCache()))
            .Extract(error => throw new InvalidOperationException(error));

        var module =
            evaluated.Should().BeOfType<PineValue.ListValue>().Subject.Items.Span[0]
            .Should().BeOfType<PineValue.ListValue>().Subject.Items.Span[1];

        var value =
            module.Should().BeOfType<PineValue.ListValue>().Subject.Items.Span[0]
            .Should().BeOfType<PineValue.ListValue>().Subject.Items.Span[1];

        value.Should().Be(
            ElmValueEncoding.ElmValueAsPineValue(ElmValue.TupleInstance(ElmValue.Integer(11), ElmValue.Integer(22))));
    }

    [Fact]
    public async Task Mappings_are_edge_local_and_aliases_preserve_closed_artifact_reuse()
    {
        var world =
            new World()
            .Add("test/shared", "1.0.0", ["module Common exposing (value)\nvalue = 42"], ["Common"], isDirect: false)
            .Add(
                "test/left",
                "1.0.0",
                [
                "module Left exposing (answer)\nimport Chosen.Common as Used\nanswer = Used.value"
                ],
                ["Left"],
                ["test/shared"])
            .Add(
                "test/right",
                "1.0.0",
                ["module Right exposing (answer)\nimport Common\nanswer = Common.value"],
                ["Right"],
                ["test/shared"]);

        var build =
            await world.Prepare(
                "module Main exposing (root)\nimport Left\nimport Right\nroot = ( Left.answer, Right.answer )",
                new ElmModuleNameMapping("test/left", "test/shared", "Chosen"));

        build.ImportDiagnostics.Should().BeEmpty();
        var canonical = Canonical(build, new());

        canonical.UnitExports.Where(pair => pair.Key.DeclName == "answer").Select(pair => pair.Value.Unit)
            .Distinct().Should().ContainSingle();

        build.CompilerModuleOrigins.Values.Count(origin => origin.ModuleName == "Common").Should().Be(1);

        var cache = new Canonicalization.DeclarationCache();

        var direct =
            new World().Add("test/shared", "1.0.0", ["module Common exposing (value)\nvalue = 42"], ["Common"]);

        var first =
            Canonical(
                await direct.Prepare(
                    "module Main exposing (root)\nimport First.Common as Used\nroot = Used.value",
                    new ElmModuleNameMapping(null, "test/shared", "First")),
                cache);

        var second =
            Canonical(
                await direct.Prepare(
                    "module Main exposing (root)\nimport Second.Common as Used\nroot = Used.value",
                    new ElmModuleNameMapping(null, "test/shared", "Second")),
                cache);

        second.TransformationCount.Should().Be(0);
        second.UnitExports[Name("Main", "root")].Unit.Should().BeSameAs(first.UnitExports[Name("Main", "root")].Unit);
    }

    [Fact]
    public async Task Shared_failures_recover_current_package_provenance_from_the_demand_path()
    {
        var cache = new Canonicalization.DeclarationCache();
        const string App = "module Main exposing (root)\nimport Library as Used\nroot = Used.answer";

        var firstBuild =
            await new World().Add(
                "original/library",
                "1.0.0",
                ["module Library exposing (answer)\nanswer = Missing.value"],
                ["Library"]).Prepare(App);

        var first = Canonical(firstBuild, cache);

        var secondBuild =
            await new World().Add(
                "different/fork",
                "1.0.7",
                ["\n\nmodule Library exposing (answer)\nanswer = Missing.value"],
                ["Library"]).Prepare(App);

        var second = Canonical(secondBuild, cache);
        var firstName = first.Declarations.Keys.Single(name => name.DeclName == "answer");
        var secondName = second.Declarations.Keys.Single(name => name.DeclName == "answer");
        second.Transformations[secondName].Should().BeSameAs(first.Transformations[firstName]);

        second.Declarations[secondName].Errors.Single().Should().BeOfType<CanonicalizationError.UnresolvedReference>()
            .Which.Range.Start.Row.Should().Be(4);

        var failure =
            ElmCompiler.CompileResolvedEnvironment(secondBuild, [Name("Main", "root")], canonicalizationCache: cache)
            .IsErrOrNull();

        failure.Should().Contain("elm-packages/different/fork/src/Library.elm").And.NotContain("original/library");
    }

    [Fact]
    public async Task Synthetic_multi_component_import_aliases_do_not_depend_on_unrelated_import_order()
    {
        var world =
            new World().Add(
                "test/library",
                "1.0.0",
                [
                    "module Library.One exposing (value)\nvalue = 42",
                    "module Library.Two exposing (value)\nvalue = 43"
                ],
                ["Library.One", "Library.Two"]);

        var cache = new Canonicalization.DeclarationCache();

        var first =
            Canonical(
                await world.Prepare(
                    "module Main exposing (root)\nimport Library.One\nroot = Library.One.value"),
                cache);

        var second =
            Canonical(
                await world.Prepare(
                    "module Main exposing (root)\nimport Library.Two\nimport Library.One\nroot = Library.One.value"),
                cache);

        second.TransformationCount.Should().Be(0);
        second.Transformations[Name("Main", "root")].Should().BeSameAs(first.Transformations[Name("Main", "root")]);
        second.UnitExports[Name("Main", "root")].Unit.Should().BeSameAs(first.UnitExports[Name("Main", "root")].Unit);
    }

    [Fact]
    public async Task Cached_emission_failures_follow_the_current_fork_source_path()
    {
        var cache = new Canonicalization.DeclarationCache();
        const string App = "module Main exposing (root)\nimport Library as Used\nroot = Used.answer";
        const string Library = "module Library exposing (answer)\nanswer = Basics.sin 8";
        var first = await new World().Add("original/library", "1.0.0", [Library], ["Library"]).Prepare(App);

        ElmCompiler.CompileResolvedEnvironment(first, [Name("Main", "root")], canonicalizationCache: cache)
            .IsErrOrNull().Should().Contain("original/library").And.Contain("Basics.sin");

        var emissionCount = cache.Semantic.Emission.Count;
        emissionCount.Should().BeGreaterThan(0);
        var second = await new World().Add("different/fork", "1.0.7", ["\n\n" + Library], ["Library"]).Prepare(App);

        ElmCompiler.CompileResolvedEnvironment(second, [Name("Main", "root")], canonicalizationCache: cache)
            .IsErrOrNull().Should().Contain("elm-packages/different/fork/src/Library.elm:4")
            .And.NotContain("original/library");

        cache.Semantic.Emission.Count.Should().Be(emissionCount);
    }

    [Fact]
    public async Task Direct_scope_ambiguity_is_preserved_and_reference_prefixes_resolve_it()
    {
        var world =
            new World()
            .Add("test/left", "1.0.0", ["module Common exposing (value)\nvalue = 11"], ["Common"])
            .Add("test/right", "1.0.0", ["module Common exposing (value)\nvalue = 22"], ["Common"]);

        var ambiguous = await world.Prepare("module Main exposing (root)\nimport Common\nroot = Common.value");
        ambiguous.ImportDiagnostics.Should().ContainSingle();
        Canonical(ambiguous, new()).UnitExports.Should().NotContainKey(Name("Main", "root"));

        var mapped =
            await world.Prepare(
                "module Main exposing (root)\nimport First.Common as A\nimport Second.Common as B\nroot = ( A.value, B.value )",
                new(null, "test/left", "First"),
                new(null, "test/right", "Second"));

        mapped.ImportDiagnostics.Should().BeEmpty();
        var result = Canonical(mapped, new());
        result.Declarations.Values.SelectMany(declaration => declaration.Errors).Should().BeEmpty();
        ElmCompiler.CompileResolvedEnvironment(mapped, [Name("Main", "root")]).IsOkOrNullable().Should().NotBeNull();
    }

    [Theory]
    [InlineData(8)]
    [InlineData(64)]
    [InlineData(256)]
    public void Cold_and_warm_diamond_graphs_transform_each_body_at_most_once(int count)
    {
        var code = new StringBuilder("module Main exposing (..)\nvalue0 = 0\nvalue1 = value0\n");

        for (var index = 2; index < count; index++)
            code.Append($"value{index} = ( value{index - 1}, value{index - 2} )\n");

        var cache = new Canonicalization.DeclarationCache();
        var first = Canonical(cache, [code.ToString()], Name("Main", "value" + (count - 1)));
        first.Declarations.Should().HaveCount(count);
        first.TransformationCount.Should().Be(count);
        var again = Canonical(cache, [code.ToString()], Name("Main", "value" + (count - 1)));
        again.TransformationCount.Should().Be(0);

        foreach (var result in new[] { first, again })
        {
            var chains =
                result.DependencyChains.Should().BeOfType<Canonicalization.DeclarationDependencyChains>().Subject;

            chains.Predecessors.Should().HaveCount(count);
            chains.Predecessors.Values.Count(parent => parent is not null).Should().Be(count - 1);
        }

        again.UnitExports.Values.Select(export => export.Unit).Distinct().Count().Should().Be(count);
        UnitConstruction.Flatten([again.UnitExports[Name("Main", "value" + (count - 1))]]).Should().HaveCount(count);
    }

    [Theory]
    [InlineData(64)]
    [InlineData(512)]
    public void Deep_dependency_graphs_retain_one_predecessor_per_declaration_not_complete_paths(int count)
    {
        var code = new StringBuilder("module Main exposing (..)\nvalue0 = 0\n");

        for (var index = 1; index < count; index++)
            code.Append($"value{index} = ( value{index - 1}, {index} )\n");

        var root = Name("Main", "value" + (count - 1));
        var cache = new Canonicalization.DeclarationCache();
        var cold = Canonical(cache, [code.ToString()], root);
        var warm = Canonical(cache, [code.ToString()], root);
        cold.TransformationCount.Should().Be(count);
        warm.TransformationCount.Should().Be(0);

        foreach (var result in new[] { cold, warm })
        {
            result.Dependencies.Values.Sum(edges => edges.Count).Should().Be(count - 1);

            var chains =
                result.DependencyChains.Should().BeOfType<Canonicalization.DeclarationDependencyChains>().Subject;

            chains.Predecessors.Should().HaveCount(count);
            chains.Predecessors[root].Should().BeNull();

            for (var index = 0; index < count - 1; index++)
                chains.Predecessors[Name("Main", "value" + index)].Should().Be(Name("Main", "value" + (index + 1)));

            var expected =
                Enumerable.Range(0, count).Reverse().Select(index => Name("Main", "value" + index)).ToArray();

            var path = chains[Name("Main", "value0")];
            path.Should().Equal(expected);

            path.Should().NotBeSameAs(
                chains[Name("Main", "value0")],
                "expanded paths are not retained by the invocation");

            chains.Predecessors.Should().HaveCount(count, "diagnostic reconstruction must not change retained storage");
        }
    }

    [Fact]
    public void Lazy_dependency_paths_preserve_first_demand_through_shared_and_recursive_edges()
    {
        const string Source =
            """
            module Main exposing (..)
            root = ( left, right )
            left = shared
            right = shared
            shared = root
            unused = 0
            """;

        var root = Name("Main", "root");
        var left = Name("Main", "left");
        var right = Name("Main", "right");
        var shared = Name("Main", "shared");
        var result = Canonical(new(), [Source], root);
        var chains = result.DependencyChains.Should().BeOfType<Canonicalization.DeclarationDependencyChains>().Subject;
        chains.Predecessors.Should().HaveCount(4);
        chains.Predecessors[root].Should().BeNull();
        chains.Predecessors[left].Should().Be(root);
        chains.Predecessors[right].Should().Be(root);
        chains.Predecessors[shared].Should().Be(left);
        chains[shared].Should().Equal(root, left, shared);
        chains[root].Should().Equal(root);

        var paths = chains.ToDictionary(entry => entry.Key, entry => entry.Value);
        paths.Keys.Should().BeEquivalentTo(chains.Keys);
        chains.Values.Should().BeEquivalentTo(paths.Values);
        chains.TryGetValue(shared, out var found).Should().BeTrue();
        found.Should().Equal(root, left, shared);
        var missing = Name("Main", "unused");
        chains.ContainsKey(missing).Should().BeFalse();
        chains.TryGetValue(missing, out var absent).Should().BeFalse();
        absent.Should().BeNull();
        Action missingPath = () => _ = chains[missing];
        missingPath.Should().Throw<KeyNotFoundException>();
    }

    [Fact]
    public void All_roots_keep_root_paths_regardless_of_duplicate_roots_or_backreferences()
    {
        const string Source =
            "module Main exposing (..)\nalpha = shared\nomega = shared\nshared = alpha\n";

        var alpha = Name("Main", "alpha");
        var omega = Name("Main", "omega");
        var shared = Name("Main", "shared");
        var result = Canonical(new(), [Source], omega, alpha, omega);
        var chains = result.DependencyChains.Should().BeOfType<Canonicalization.DeclarationDependencyChains>().Subject;
        chains.Predecessors.Should().HaveCount(3);
        chains.Predecessors.Values.Count(parent => parent is null).Should().Be(2);
        chains[alpha].Should().Equal(alpha);
        chains[omega].Should().Equal(omega);
        chains[shared].Should().Equal(alpha, shared);
        Canonical(new(), [Source], alpha, omega).DependencyChains[shared].Should().Equal(chains[shared]);
        Canonical(new(), [Source]).DependencyChains.Should().BeEmpty();
    }

    [Fact]
    public void Recursive_slots_are_independent_of_module_names_and_root_enumeration()
    {
        const string First = "module Alpha exposing (..)\nf value = g value\ng value = f value";
        var cache = new Canonicalization.DeclarationCache();
        var original = Canonical(cache, [First], Name("Alpha", "f"), Name("Alpha", "g"));

        var renamed =
            Canonical(
                cache,
                [
                    First.Replace("Alpha", "Last", StringComparison.Ordinal)
                ],
                Name("Last", "g"),
                Name("Last", "f"));

        renamed.UnitExports[Name("Last", "f")].Unit.Should().BeSameAs(original.UnitExports[Name("Alpha", "f")].Unit);
        original.UnitExports.Values.Select(export => export.Unit).Distinct().Should().ContainSingle();

        original.UnitExports
            .Values.First().Unit.Should().BeOfType<SccModel.ValueScc>().Which.Value.Members.Should().HaveCount(2);
    }

    [Fact]
    public void All_syntactic_dependencies_are_demanded_even_when_execution_does_not_use_them()
    {
        var result =
            Canonical(
                new(),
                [
                    """
                    module Main exposing (..)
                    import Types
                    root : Types.Alias -> Int
                    root (Types.Item value) =
                        let
                            unused = Missing.value
                        in
                        value
                    """,
                    """
                    module Types exposing (..)
                    type Payload = Item Int
                    type alias Alias = Payload
                    unrelated = Elsewhere.missing
                    """
                ],
                Name("Main", "root"));

        result.Declarations.Should().ContainKey(Name("Types", "Alias")).And.ContainKey(Name("Types", "Payload"));
        result.Declarations.Should().NotContainKey(Name("Types", "unrelated"));
        result.Declarations[Name("Main", "root")].Errors.Should().ContainSingle();

        result.References[Name("Main", "root")].Select(reference => reference.ReferencedName)
            .Should().Contain(Name("Types", "Item"));
    }

    [Fact]
    public void Shared_artifacts_do_not_unify_source_nominal_types()
    {
        var texts =
            new[]
            {
                "module A exposing (..)\ntype Item = Item",
                "module B exposing (..)\ntype Item = Item",
                "module Main exposing (..)\nimport A\nimport B\nroot : A.Item\nroot = B.Item"
            };

        var result = Canonical(new(), texts, Name("Main", "root"));
        result.UnitExports[Name("A", "Item")].Unit.Should().BeSameAs(result.UnitExports[Name("B", "Item")].Unit);

        var value = result.UnitExports[Name("Main", "root")].Unit.Should().BeOfType<SccModel.ValueScc>().Which.Value;
        value.Dependencies.Count.Should().Be(1);

        value.TypeDependencies.Count.Should().Be(
            1,
            "distinct nominal binding roles must survive even if both refer to the same artifact");

        value.Dependencies.Keys.Single().Namespaces.Should().NotEqual(value.TypeDependencies.Keys.Single().Namespaces);

        var typedUsage =
            texts.Select(
                text => text
                .Replace("type Item = Item", "type Item = Item\nvalue : Item\nvalue = Item", StringComparison.Ordinal)
                .Replace("root = B.Item", "root = B.value", StringComparison.Ordinal)).ToArray();

        var tree = TestCase.DefaultAppWithoutPackages(typedUsage).AsFileTree();
        ElmCompiler.AnalyzeApplication(tree).Should().Contain(diagnostic => diagnostic.Code == "elm-type");
    }

    [Fact]
    public void Diagnostic_paths_rebind_to_current_parentheses_and_record_field_locations()
    {
        var cache = new Canonicalization.DeclarationCache();
        const string Initial = "module Main exposing (..)\nroot = { z = 0, a = ( Missing.value ) }";
        var first = Canonical(cache, [Initial], Name("Main", "root"));

        var shifted =
            Canonical(
                cache,
                [
                "\n\n" + Initial.Replace("( Missing.value )", "(( Missing.value ))", StringComparison.Ordinal)
                ],
                Name("Main", "root"));

        shifted.Transformations[Name("Main", "root")].Should().BeSameAs(first.Transformations[Name("Main", "root")]);
        var error = shifted.Declarations[Name("Main", "root")].Errors.Single();

        error.Should().BeOfType<CanonicalizationError.UnresolvedReference>()
            .Which.Range.Start.Row.Should().Be(4);
    }

    [Fact]
    public async Task Cached_models_contain_no_source_paths_owners_versions_or_concrete_nodes()
    {
        var cache = new Canonicalization.DeclarationCache();
        var fork = await Fork("sensitive-owner/package", "1.0.17", 42, cache);
        var unit = fork.Canonical.UnitExports[Name("Main", "root")].Unit;
        var text = JsonSerializer.Serialize(unit, AST.ElmSyntaxAbstractJson.JsonSerializerOptions);

        text.Should().NotContain("sensitive-owner").And.NotContain("1.0.17").And.NotContain("PinePackage")
            .And.NotContain(".elm").And.NotContain("SourcePath").And.NotContain("Range");

        foreach (var model in new[]
        {
            typeof(CanonicalizationInput), typeof(CanonicalDeclaration),
            typeof(ValueSccModel), typeof(TypeSccModel), typeof(InterfaceDefinition)
        })
        {
            model.GetProperties().Should().NotContain(
                property =>
                property.PropertyType.Namespace == typeof(CST.Range).Namespace ||
                property.PropertyType == typeof(ElmModuleOrigin));
        }
    }

    [Fact]
    public void Unit_construction_rejects_open_references_and_invalid_exports()
    {
        var syntax =
            AST.ConvertFromConcrete.FromDeclaration(
                Parse("module Main exposing (..)\nroot = Unknown.value").Declarations[0].Value);

        Action open = () => Create(syntax);
        open.Should().Throw<ArgumentException>().WithMessage("*unbound reference*");

        var closed =
            Create(
                AST.ConvertFromConcrete.FromDeclaration(
                    Parse("module Main exposing (..)\nroot = 42").Declarations[0].Value));

        Action badExport = () => _ = new UnitExport(closed, "absent");
        badExport.Should().Throw<ArgumentException>().WithMessage("*nonexistent member*");
        Action empty = () => SccGraph.Create([], [], [], []);
        empty.Should().Throw<ArgumentException>();

        var wrongSymbol =
            AST.ConvertFromConcrete.FromDeclaration(
                Parse("module Main exposing (..)\nroot = PineDependency.D0.absent").Declarations[0].Value);

        Action dangling =
            () => SccGraph.Create(
                ImmutableDictionary<string, AST.Declaration>.Empty.Add("root", wrongSymbol),
                ImmutableDictionary<DeclQualifiedName, SccModel>.Empty.Add(Name("PineDependency.D0", "absent"), closed),
                [],
                []);

        dangling.Should().Throw<ArgumentException>().WithMessage("*requested symbol*");

        Action disconnected =
            () => SccGraph.Create(
                ImmutableDictionary<string, AST.Declaration>.Empty
                .Add(
                    "root",
                    AST.ConvertFromConcrete.FromDeclaration(
                        Parse("module M exposing (..)\nroot = 1").Declarations[0].Value))
                .Add(
                    "other",
                    AST.ConvertFromConcrete.FromDeclaration(
                        Parse("module M exposing (..)\nother = 1").Declarations[0].Value)),
                [],
                [],
                []);

        disconnected.Should().Throw<ArgumentException>().WithMessage("*strongly connected*");
    }

    [Fact]
    public void Dependency_equality_uses_closed_addresses_without_expanding_transitive_bodies()
    {
        var syntax =
            AST.ConvertFromConcrete.FromDeclaration(
                Parse("module Main exposing (..)\nroot = 42").Declarations[0].Value);

        var first = Create(syntax);
        var second = Create(syntax);
        first.Should().NotBeSameAs(second);
        first.Should().Be(second);
        first.GetHashCode().Should().Be(second.GetHashCode());
        new UnitExport(first, "root").Should().Be(new UnitExport(second, "root"));
    }

    [Fact]
    public async Task Concurrent_interning_retains_and_returns_one_representation()
    {
        var input =
            Create(
                AST.ConvertFromConcrete.FromDeclaration(
                    Parse("module Main exposing (..)\nroot = 42").Declarations[0].Value));

        var cache = new DeclarationUnitCache();
        var results = await Task.WhenAll(Enumerable.Range(0, 32).Select(_ => Task.Run(() => cache.Intern(input))));

        foreach (var result in results)
            result.Should().BeSameAs(results[0]);

        cache.Units.Count.Should().Be(1);
    }

    private static SccModel Create(AST.Declaration syntax) =>
        SccGraph.Create(
            ImmutableDictionary<string, AST.Declaration>.Empty.Add(ElmCompiler.GetDeclarationName(syntax)!, syntax),
            [],
            [],
            []);

    [Fact]
    public void Type_and_constructor_symbols_use_separate_collections_and_share_type_values()
    {
        var result =
            Canonical(
                new(),
                [
                    """
                    module Types exposing (..)
                    type Payload = Thunk Int
                    type Tree = Payload Payload
                    """,
                    """
                    module Main exposing (..)
                    import Types
                    root : Types.Payload -> Types.Tree
                    root value = Types.Payload value
                    unwrap (Types.Payload value) = value
                    """
                ],
                Name("Main", "root"),
                Name("Main", "unwrap"));

        var payload =
            result.UnitExports[Name("Types", "Payload")].Unit.Should().BeOfType<SccModel.TypeScc>().Which.Value;

        var tree = result.UnitExports[Name("Types", "Tree")].Unit.Should().BeOfType<SccModel.TypeScc>().Which.Value;
        tree.TypeDependencies.Values.Single().Should().BeSameAs(payload);
        var root = result.UnitExports[Name("Main", "root")].Unit.Should().BeOfType<SccModel.ValueScc>().Which.Value;

        root.TypeDependencies.Values.Should().Contain(value => ReferenceEquals(value, payload))
            .And.Contain(value => ReferenceEquals(value, tree));

        root.Dependencies.Values.Single().Should().BeOfType<SccModel.TypeScc>().Which.Value.Should().BeSameAs(tree);
        tree.Members.Keys.Should().Equal("Tree");

        // The same spelling selects different declarations in the two namespaces.
        var key = Name("Types", "Payload");

        var namespaced =
            new SccModel.ValueScc(
                new(
                    ImmutableDictionary<string, ValueDeclaration>.Empty.Add(
                        "use",
                        new ValueDeclaration.FunctionDeclaration(
                            ((AST.Declaration.FunctionDeclaration)AST.ConvertFromConcrete.FromDeclaration(
                                Parse(
                        "module M exposing (..)\nuse : Types.Payload -> Types.Tree\nuse x = Types.Payload x").Declarations[0].Value)).Function)),
                    ImmutableDictionary<DeclQualifiedName, SccModel>.Empty.Add(key, new SccModel.TypeScc(tree)),
                    ImmutableDictionary<DeclQualifiedName, TypeSccModel>.Empty.Add(key, payload).Add(
                        Name("Types", "Tree"),
                        tree)));

        UnitConstruction.Lookup(namespaced, key, true).Member.Should().Be("Payload");
        UnitConstruction.Lookup(namespaced, key, false).Member.Should().Be("Tree");
        UnitConstruction.Flatten([new(namespaced, "use")]).Should().HaveCount(3);

        Core.Elm.ElmCompilerInDotnet.CompilerModel.ProgramValidation.Validate(
            SccCompiler.Lower([new(namespaced, "use")]));
    }

    [Fact]
    public void Constructor_expressions_patterns_and_record_aliases_select_owning_type_members()
    {
        var result =
            Canonical(
                new(),
                [
                    """
                    module Main exposing (..)
                    type Choice = First Int | Second
                    type alias Record = { value : Int }
                    root value =
                        case value of
                            First x -> Record x
                            Second -> Record 0
                    """
                ],
                Name("Main", "root"));

        var value = result.UnitExports[Name("Main", "root")].Unit.Should().BeOfType<SccModel.ValueScc>().Which.Value;
        value.Dependencies.Should().HaveCount(3);
        var choices = value.Dependencies.Where(entry => entry.Key.DeclName is "First" or "Second").ToArray();
        choices[0].Value.Should().BeSameAs(choices[1].Value);

        foreach (var choice in choices)
            UnitConstruction.Lookup(new SccModel.ValueScc(value), choice.Key, false).Member.Should().Be("Choice");

        UnitConstruction.Lookup(
            new SccModel.ValueScc(value),
            value.Dependencies.Keys.Single(key => key.DeclName == "Record"),
            false)
            .Member.Should().Be("Record");
    }

    [Fact]
    public void Graph_rewriting_splits_removed_cycles_and_qualifies_new_external_references()
    {
        var source =
            Canonical(
                new(),
                ["module Main exposing (..)\nf x = g x\ng x = f x"],
                Name("Main", "f"),
                Name("Main", "g"));

        var input = source.UnitExports[Name("Main", "f")].Unit.Should().BeOfType<SccModel.ValueScc>().Which;

        var members =
            input.Value.Members.ToImmutableDictionary(
                entry => entry.Key,
                entry => (AST.Declaration)new AST.Declaration.FunctionDeclaration(
                    ((ValueDeclaration.FunctionDeclaration)entry.Value).Function));

        members =
            members.SetItem(
                "f",
                AST.ConvertFromConcrete.FromDeclaration(
                    Parse("module M exposing (..)\nf x = x").Declarations[0].Value));

        var result = SccGraph.Repartition(input, members);
        result["f"].Unit.Should().NotBe(result["g"].Unit);
        var g = result["g"].Unit.Should().BeOfType<SccModel.ValueScc>().Which.Value;
        g.Members.Should().ContainSingle();
        g.Dependencies.Values.Single().Should().BeSameAs(result["f"].Unit);
        g.Dependencies.Keys.Single().Namespaces.Should().NotBeEmpty();
        UnitConstruction.Flatten(result.Values).Should().HaveCount(2);
    }

    [Fact]
    public void Syntax_passes_repartition_graphs_and_reuse_independent_results()
    {
        var cache = new DeclarationUnitCache();

        var canonical =
            Canonical(
                new(),
                ["module Main exposing (..)\nf x = Debug.log (g x) x\ng x = f x"],
                Name("Main", "f"),
                Name("Main", "g"));

        var roots = canonical.UnitExports.ToDictionary(entry => entry.Key.FullName, entry => entry.Value);

        var cold =
            SccCompiler.Transform(roots, false, false, true, cache)
            .Extract(error => throw new InvalidOperationException(error));

        cold["Main.f"].Unit.Should().NotBe(cold["Main.g"].Unit);
        var count = cache.SyntaxTransforms.Count;

        var warm =
            SccCompiler.Transform(roots, false, false, true, cache)
            .Extract(error => throw new InvalidOperationException(error));

        warm["Main.f"].Should().BeSameAs(cold["Main.f"]);
        warm["Main.g"].Should().BeSameAs(cold["Main.g"]);
        cache.SyntaxTransforms.Count.Should().Be(count);
        var firstProgram = SccCompiler.Lower(cold.Values, cache);
        var loweredCount = cache.CompilerModels.Count;
        var secondProgram = SccCompiler.Lower(warm.Values, cache);
        cache.CompilerModels.Count.Should().Be(loweredCount);

        foreach (var entry in firstProgram.Functions)
            secondProgram.Functions[entry.Key].Should().BeSameAs(entry.Value);
    }

    [Fact]
    public void SCC_construction_rejects_mixed_members_and_wrong_namespace_selections()
    {
        var syntax =
            Parse("module M exposing (..)\ntype Item = Constructor\nvalue = 1").Declarations
            .Select(node => AST.ConvertFromConcrete.FromDeclaration(node.Value)).ToArray();

        Action mixed =
            () => SccGraph.Create(
                syntax.ToImmutableDictionary(item => ElmCompiler.GetDeclarationName(item)!),
                [],
                [],
                []);

        mixed.Should().Throw<ArgumentException>().WithMessage("*value members*");
        var type = Create(syntax[0]).Should().BeOfType<SccModel.TypeScc>().Subject;

        Action wrongValue =
            () => new ValueSccModel(
                ImmutableDictionary<string, ValueDeclaration>.Empty.Add(
                    "value",
                    new ValueDeclaration.FunctionDeclaration(((AST.Declaration.FunctionDeclaration)syntax[1]).Function)),
                ImmutableDictionary<DeclQualifiedName, SccModel>.Empty.Add(Name("M", "Item"), type),
                []);

        wrongValue.Should().Throw<ArgumentException>().WithMessage("*constructor*");

        Action wrongType =
            () => new TypeSccModel(
                type.Value.Members,
                ImmutableDictionary<DeclQualifiedName, TypeSccModel>.Empty.Add(Name("M", "Constructor"), type.Value));

        wrongType.Should().Throw<ArgumentException>().WithMessage("*type*");
    }

    [Fact]
    public void Compiler_stage_caches_are_invocation_owned_and_can_be_reused_across_compilations()
    {
        var tree =
            TestCase.DefaultAppWithoutPackages(
                ["module Main exposing (..)\nroot value = (\\x -> x) value\nother x = x"]).AsFileTree();

        var root = Name("Main", "root");
        var cache = new Canonicalization.DeclarationCache();

        CompilationPipelineStageResults Lower(
            Canonicalization.DeclarationCache? storage,
            params DeclQualifiedName[] roots) =>
            ElmCompiler.LowerToCompilerModel(
                tree,
                roots,
                canonicalizationCache: storage,
                includeBundledKernelModules: false)
            .Extract(error => throw new InvalidOperationException(error));

        var cold = Lower(cache, root);
        var views = cache.Semantic.CompilerViews.Count;
        var models = cache.Semantic.CompilerModels.Count;
        var passes = cache.Semantic.SyntaxTransforms.Count;
        var lifting = cache.Semantic.LambdaLifting.Count;
        var warm = Lower(cache, root);
        cache.Semantic.CompilerViews.Count.Should().Be(views);
        cache.Semantic.CompilerModels.Count.Should().Be(models);
        cache.Semantic.SyntaxTransforms.Count.Should().Be(passes);
        cache.Semantic.LambdaLifting.Count.Should().Be(lifting);

        foreach (var entry in cold.CompilerProgram.Functions)
            warm.CompilerProgram.Functions[entry.Key].Should().BeSameAs(entry.Value);

        var added = Lower(cache, Name("Main", "other"), root);

        foreach (var entry in cold.CompilerProgram.Functions)
            added.CompilerProgram.Functions[entry.Key].Should().BeSameAs(entry.Value);

        var independent = Lower(null, root);

        independent.CompilerProgram.Functions[cold.SourceFunctionIds[root]]
            .Should().NotBeSameAs(cold.CompilerProgram.Functions[cold.SourceFunctionIds[root]]);

        cache.Clear();
        cache.Semantic.CompilerViews.Count.Should().Be(0);
        cache.Semantic.CompilerModels.Count.Should().Be(0);
        cache.Semantic.SyntaxTransforms.Count.Should().Be(0);
        cache.Semantic.LambdaLifting.Count.Should().Be(0);
    }

    [Fact]
    public void SCC_lowering_preserves_constructor_and_record_runtime_behavior()
    {
        var (environment, pipeline) =
            ElmCompilerTestHelper.CompileElmModules(
                [
                    """
                    module Main exposing (..)
                    type Choice = First Record | Second
                    type alias Record = { value : Int, rest : List Choice }
                    root _ =
                        case First (Record 42 []) of
                            First x -> x.value
                            Second -> 0
                    """
                ],
                disableInlining: false);

        OptimizationOpportunityFinder.FindGenericRecordAccessOpportunities(pipeline.CompilerProgram).Should().BeEmpty();

        var value =
            environment.Modules.Single(module => module.moduleName == "Main").moduleContent.FunctionDeclarations["root"];

        ElmCompilerTestHelper.CreateFunctionValueInvocationDelegate(value, new PineVMParseCache())([PineValue.EmptyList])
            .returnValue.Should().Be(IntegerEncoding.EncodeSignedInteger(42));
    }
}
