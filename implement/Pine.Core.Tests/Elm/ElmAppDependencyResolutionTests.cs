using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Reflection;
using System.Text;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using Syntax = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Tests.Elm;

public class ElmAppDependencyResolutionTests
{
    [Theory]
    [InlineData("1.0.0 <= v < 2.0.0", "1.0.5", true)]
    [InlineData("1.0.0 < v <= 2.0.0", "1.0.0", false)]
    [InlineData("1.0.0 < v <= 2.0.0", "2.0.0", true)]
    [InlineData("1.0.0 <= v < 2.0.0", "2.0.0", false)]
    public void Version_constraints_preserve_boundaries(string range, string version, bool expected) =>
        ElmPackageVersionConstraint.Parse(range).Contains(ElmPackageVersion.Parse(version)).Should().Be(expected);

    [Fact]
    public void Version_comparison_is_numeric_and_empty_touching_intervals_are_rejected()
    {
        ElmPackageVersion.Parse("1.10.0").CompareTo(ElmPackageVersion.Parse("1.9.0")).Should().BeGreaterThan(0);

        ElmPackageVersionConstraint.Parse("1.0.0 <= v < 2.0.0")
            .Intersect(ElmPackageVersionConstraint.Parse("2.0.0 <= v < 3.0.0")).Should().BeNull();

        ElmPackageVersionConstraint.Parse("1.0.0 <= v <= 2.0.0")
            .Intersect(ElmPackageVersionConstraint.Parse("2.0.0 <= v < 3.0.0"))
            .Should().Be(ElmPackageVersionConstraint.Exact(ElmPackageVersion.Parse("2.0.0")));
    }

    [Theory]
    [InlineData("1.0")]
    [InlineData("01.0.0")]
    [InlineData("1.0.0-beta")]
    [InlineData("2.0.0 <= v < 1.0.0")]
    [InlineData("1.0.0 <= v < 1.0.0")]
    public void Malformed_requirements_are_not_guessed(string text)
    {
        Action parse = () => ElmPackageVersionConstraint.Parse(text);
        parse.Should().Throw<FormatException>();
    }

    [Fact]
    public async Task Exact_pin_and_package_range_are_compatible()
    {
        var provider =
            new Provider().Add("author/a", "1.0.0", Deps(("elm/core", "1.0.0 <= v < 2.0.0")))
            .Add("elm/core", "1.0.5");

        var report = await Resolve(Application(Deps(("author/a", "1.0.0")), Deps(("elm/core", "1.0.5"))), provider);
        report.Succeeded.Should().BeTrue();
        report.Packages["elm/core"].Identity.Version.ToString().Should().Be("1.0.5");
        report.Requirements.Count(item => item.PackageName == "elm/core").Should().Be(2);
        provider.VersionQueries.Should().BeEmpty();
    }

    [Fact]
    public async Task Nested_projects_do_not_contribute_versions_or_malformed_manifests()
    {
        var tree =
            Tree(Application(Deps(("elm/core", "1.0.5"))))
            .SetNodeAtPathSorted(["review", "elm.json"], FileTree.File("{ malformed"u8.ToArray()))
            .SetNodeAtPathSorted(
                [
                "example",
                "elm.json"
                ],
                FileTree.File(Encoding.UTF8.GetBytes(Application(Deps(("elm/core", "1.0.4"))))));

        var provider = new Provider().Add("elm/core", "1.0.5");
        var report = await ElmDependencyResolver.ResolveAsync(tree, ["elm.json"], new(), provider);
        report.Succeeded.Should().BeTrue();
        provider.MetadataQueries.Should().Equal(new ElmPackageIdentity("elm/core", ElmPackageVersion.Parse("1.0.5")));
    }

    [Fact]
    public async Task Solver_backtracks_and_retains_rejected_branches_and_metadata()
    {
        var provider =
            new Provider()
            .Add("author/a", "2.0.0", Deps(("author/shared", "2.0.0 <= v < 3.0.0")))
            .Add("author/a", "1.0.0", Deps(("author/shared", "1.0.0 <= v < 2.0.0")))
            .Add("author/b", "1.0.0", Deps(("author/shared", "1.0.0 <= v < 2.0.0")))
            .Add("author/shared", "1.0.0")
            .Add("author/shared", "2.0.0");

        var report =
            await Resolve(
                Package(Deps(("author/a", "1.0.0 <= v < 3.0.0"), ("author/b", "1.0.0 <= v < 2.0.0"))),
                provider);

        report.Succeeded.Should().BeTrue();
        report.Packages["author/a"].Identity.Version.ToString().Should().Be("1.0.0");

        report.Trace.Should().Contain(
            item => item.Action == "Backtracked" && item.Candidate == ElmPackageVersion.Parse("2.0.0"));

        report.Trace.Should().Contain(
            item => item.Failure != null && item.Failure.Kind == ElmResolutionFailureKind.ConstraintConflict);

        report.Trace.Should().Contain(item => item.Action == "Metadata" && item.Detail.Contains("author/shared"));
        report.Requirements.Count(item => item.PackageName == "author/shared").Should().Be(2);
        report.ToJson().Should().Contain("DependencyPath").And.Contain("Backtracked");
        provider.MetadataQueries.Distinct().Count().Should().Be(provider.MetadataQueries.Count);
    }

    [Fact]
    public async Task Actual_conflict_reports_both_dependency_paths_without_changing_pins()
    {
        var provider =
            new Provider().Add("author/a", "1.0.0", Deps(("author/shared", "2.0.0 <= v < 3.0.0")))
            .Add("author/shared", "1.0.0");

        var report =
            await Resolve(Application(Deps(("author/a", "1.0.0")), Deps(("author/shared", "1.0.0"))), provider);

        report.Succeeded.Should().BeFalse();
        report.Failures.Should().Contain(item => item.Kind == ElmResolutionFailureKind.ConstraintConflict);
        var exception = new ElmDependencyResolutionException(report);

        exception.Message.Should().Contain("author/a@1.0.0")
            .And.Contain("author/shared")
            .And.Contain("1.0.0")
            .And.Contain("2.0.0 <= v < 3.0.0")
            .And.Contain("elm.json")
            .And.Contain("never silently upgrade");

        report.Failures.SelectMany(item => item.Requirements).Should().Contain(
            item => item.Scope == ElmDependencyScope.Indirect);
    }

    [Fact]
    public async Task Root_tests_are_included_but_dependencies_tests_are_not()
    {
        var provider =
            new Provider()
            .Add("author/a", "1.0.0", testDependencies: Deps(("missing/dependency-tests", "1.0.0 <= v < 2.0.0")))
            .Add("author/test", "1.0.0");

        var manifest = Package(Deps(("author/a", "1.0.0 <= v < 2.0.0")), Deps(("author/test", "1.0.0 <= v < 2.0.0")));
        var normal = await Resolve(manifest, provider);
        var tests = await Resolve(manifest, provider, new() { IncludeTests = true });
        normal.Packages.Keys.Should().BeEquivalentTo(["author/a"]);
        tests.Packages.Keys.Should().BeEquivalentTo(["author/a", "author/test"]);
        provider.VersionQueries.Should().NotContain("missing/dependency-tests");
    }

    [Fact]
    public async Task Empty_published_set_and_incomplete_offline_set_are_distinct()
    {
        var manifest = Package(Deps(("author/a", "1.0.0 <= v < 2.0.0")));
        var online = await Resolve(manifest, new Provider());
        var offline = await Resolve(manifest, new Provider { Complete = false }, new() { Offline = true });
        online.Failures[0].Kind.Should().Be(ElmResolutionFailureKind.NoMatchingVersion);
        offline.Failures[0].Kind.Should().Be(ElmResolutionFailureKind.OfflineUnavailable);
        new ElmDependencyResolutionException(offline).Message.Should().Contain("Retry online");
    }

    [Fact]
    public async Task Substitution_version_aliases_terminate_all_upstream_searches()
    {
        var substitution =
            ElmPackageSubstitution.Create(
                "elm/core",
                "same-core-implementation",
                ["1.0.4", "1.0.5"],
                FileTree.EmptyTree,
                []);

        var provider = new Provider { RejectRequests = true };
        var configuration = new ElmDependencyResolutionConfiguration { Substitutions = [substitution] };
        var report = await Resolve(Package(Deps(("elm/core", "1.0.0 <= v < 2.0.0"))), provider, configuration);
        report.Succeeded.Should().BeTrue();
        report.Packages["elm/core"].SubstitutionImplementationId.Should().Be("same-core-implementation");
        report.Packages["elm/core"].Identity.Version.ToString().Should().Be("1.0.5");
        report.Packages["elm/core"].Dependencies.Should().BeEmpty();
        report.Configuration.Substitutions[0].Versions.Should().HaveCount(2);
        provider.MetadataQueries.Should().BeEmpty();
        provider.VersionQueries.Should().BeEmpty();

        var oldest =
            await Resolve(
                Package(Deps(("elm/core", "1.0.0 <= v < 2.0.0"))),
                provider,
                configuration with { PreferOldest = true });

        oldest.Packages["elm/core"].Identity.Version.ToString().Should().Be("1.0.4");
    }

    [Fact]
    public async Task Unsupported_substitution_versions_fail_without_falling_through()
    {
        var configuration =
            new ElmDependencyResolutionConfiguration
            {
                Substitutions =
                [
                ElmPackageSubstitution.Create("author/kernel", "kernel", ["1.0.0"], FileTree.EmptyTree, [])
                ],
            };

        var report =
            await Resolve(
                Application(Deps(("author/kernel", "2.0.0"))),
                new Provider { RejectRequests = true },
                configuration);

        report.Failures[0].Kind.Should().Be(ElmResolutionFailureKind.UnsupportedSubstitutionVersion);

        new ElmDependencyResolutionException(report).Message.Should().Contain("supports only [1.0.0]").And.Contain(
            "disabled");
    }

    [Fact]
    public async Task Replacement_dependencies_are_explicit_and_upstream_dependencies_are_not_loaded()
    {
        var configuration =
            new ElmDependencyResolutionConfiguration
            {
                Substitutions =
                [
                    ElmPackageSubstitution.Create("author/kernel", "kernel", ["1.0.0"], FileTree.EmptyTree, []) with
                    {
                        ImplementationDependencies = Deps(("author/local", "1.0.0 <= v < 2.0.0")),
                    },
                ],
            };

        var provider = new Provider().Add("author/local", "1.0.0");
        var report = await Resolve(Package(Deps(("author/kernel", "1.0.0 <= v < 2.0.0"))), provider, configuration);
        report.Succeeded.Should().BeTrue();
        provider.MetadataQueries.Select(item => item.Name).Should().Equal("author/local");
        report.Requirements.Should().Contain(item => item.Scope == ElmDependencyScope.SubstitutionDependency);
    }

    [Fact]
    public async Task Compiler_incompatibility_can_backtrack_but_root_mismatch_cannot()
    {
        var provider =
            new Provider().Add("author/a", "2.0.0", elmVersion: "0.20.0 <= v < 0.21.0").Add("author/a", "1.0.0");

        var report = await Resolve(Package(Deps(("author/a", "1.0.0 <= v < 3.0.0"))), provider);
        report.Succeeded.Should().BeTrue();
        report.Packages["author/a"].Identity.Version.ToString().Should().Be("1.0.0");

        report.Trace.Should().Contain(
            item => item.Failure != null && item.Failure.Kind == ElmResolutionFailureKind.CompilerIncompatible);

        var root = await Resolve(Application(Deps(), elmVersion: "0.19.2"), new Provider { RejectRequests = true });
        root.Failures[0].Kind.Should().Be(ElmResolutionFailureKind.CompilerIncompatible);
    }

    [Fact]
    public async Task Missing_application_transitive_pins_and_cycles_are_actionable_errors()
    {
        var provider =
            new Provider()
            .Add("author/a", "1.0.0", Deps(("author/b", "1.0.0 <= v < 2.0.0")))
            .Add("author/b", "1.0.0", Deps(("author/a", "1.0.0 <= v < 2.0.0")));

        var app = await Resolve(Application(Deps(("author/a", "1.0.0"))), provider);
        app.Failures[0].Kind.Should().Be(ElmResolutionFailureKind.UndeclaredDependency);
        var package = await Resolve(Package(Deps(("author/a", "1.0.0 <= v < 2.0.0"))), provider);
        package.Failures[0].Kind.Should().Be(ElmResolutionFailureKind.DependencyCycle);
        package.Failures[0].Message.Should().Contain("author/a -> author/b -> author/a");
    }

    [Fact]
    public async Task Malformed_root_manifest_retains_an_error_instead_of_being_ignored()
    {
        var report = await Resolve("{ broken", new Provider { RejectRequests = true });
        report.Succeeded.Should().BeFalse();
        report.Failures[0].Kind.Should().Be(ElmResolutionFailureKind.InvalidManifest);
        report.Failures[0].Message.Should().Contain("elm.json");
    }

    [Fact]
    public async Task Cancellation_is_not_converted_to_a_version_conflict()
    {
        using var cancellation = new CancellationTokenSource();
        cancellation.Cancel();

        Func<Task> resolve =
            () =>
            ElmDependencyResolver.ResolveAsync(
                Tree(Package(Deps())),
                [
                "elm.json"
                ],
                new(),
                new Provider(),
                cancellation.Token);

        await resolve.Should().ThrowAsync<OperationCanceledException>();
    }

    [Fact]
    public async Task Build_uses_substitution_sources_and_retains_fingerprints()
    {
        var substitution =
            ElmPackageSubstitution.Create(
                "author/kernel",
                "kernel-v1",
                ["1.0.0"],
                Source("Kernel", "value = 42"),
                ["Kernel"]);

        var configuration = new ElmDependencyResolutionConfiguration { Substitutions = [substitution] };

        var tree =
            Tree(Application(Deps(("author/kernel", "1.0.0"))))
            .SetNodeAtPathSorted(
                ["src", "Main.elm"],
                FileTree.File("module Main exposing (value)\nimport Kernel\nvalue = Kernel.value\n"u8.ToArray()));

        var provider = new Provider { RejectRequests = true };

        var build =
            await ElmResolvedBuildPreparation.PrepareAsync(
                tree,
                [
                "elm.json"
                ],
                [
                ["src", "Main.elm"]
                ],
                configuration,
                provider);

        build.PackageSources.Keys.Should().BeEquivalentTo(["author/kernel"]);
        build.PackageSourceFingerprints["author/kernel"].Should().HaveLength(64);
        build.Resolution.Fingerprint.Should().HaveLength(64);
        build.Sources.GetNodeAtPath(["elm-packages", "author", "kernel", "src", "Kernel.elm"]).Should().NotBeNull();
        provider.SourceQueries.Should().BeEmpty();
    }

    [Fact]
    public async Task Private_and_indirect_imports_are_rejected_before_compilation()
    {
        var provider = new Provider().Add("author/a", "1.0.0", sources: Source("Hidden", "value = 42"));

        var tree =
            Tree(Application(Deps(), Deps(("author/a", "1.0.0"))))
            .SetNodeAtPathSorted(
                ["src", "Main.elm"],
                FileTree.File("module Main exposing (value)\nimport Hidden\nvalue = Hidden.value\n"u8.ToArray()));

        Func<Task> prepare =
            () =>
            ElmResolvedBuildPreparation.PrepareAsync(tree, ["elm.json"], [["src", "Main.elm"]], new(), provider);

        var exception = (await prepare.Should().ThrowAsync<ElmDependencyResolutionException>()).Which;
        exception.Report.Failures[0].Kind.Should().Be(ElmResolutionFailureKind.InvalidModuleImport);
        exception.Message.Should().Contain("indirect/undeclared").And.Contain("src/Main.elm").And.Contain("Hidden");
    }

    [Fact]
    public async Task Declaration_demand_preparation_retains_sources_with_unused_missing_Platform_import()
    {
        var tree =
            FileTree.MergeFiles(
                FileTree.MergeFiles(
                    Tree(Application(Deps(("elm/core", "1.0.5")))),
                    Source("Main", "import Platform\nvalue = 42\nunused = Platform.worker {}")),
                Source("Disconnected", "import Missing exposing (..)\nvalue = 1"));

        var configuration = ElmPackageSubstitutions.DefaultBuild.Value;
        var provider = new Provider { RejectRequests = true };

        var build =
            await ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                tree,
                ["elm.json"],
                [["src", "Main.elm"]],
                configuration,
                provider);

        build.Resolution.Succeeded.Should().BeTrue();
        build.ProjectSources.Should().Be(tree);
        build.Sources.GetNodeAtPath(["src", "Disconnected.elm"]).Should().NotBeNull();
        build.CompilerModuleNames.Should().ContainKey("src/Disconnected.elm");
        build.CompilerModuleNames.Should().ContainKey("elm-packages/elm/core/src/List.elm");
        build.ImportDiagnostics.Should().Contain(diagnostic => diagnostic.ImportedModuleName == "Missing");

        var platform = build.ImportDiagnostics.Single(diagnostic => diagnostic.ImportedModuleName == "Platform");
        platform.FilePath.Should().Be("src/Main.elm");
        platform.ModuleName.Should().Be("Main");
        platform.ImportRange.Start.Row.Should().Be(2);
        platform.ImportRange.Start.Column.Should().Be(1);
        platform.Message.Should().Contain("has no implementation").And.Contain("not searched upstream");
        provider.SourceQueries.Should().BeEmpty();

        Func<Task> strict =
            () => ElmResolvedBuildPreparation.PrepareAsync(
                tree,
                ["elm.json"],
                [["src", "Main.elm"]],
                configuration,
                provider);

        await strict.Should().ThrowAsync<ElmDependencyResolutionException>().WithMessage("*Import 'Platform'*");
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Declaration_demand_preparation_isolates_private_and_indirect_imports(bool indirect)
    {
        var provider =
            new Provider().Add(
                "author/a",
                "1.0.0",
                sources: Source("Internal.Hidden", "value = 42"),
                exposedModules: indirect ? ["Internal.Hidden"] : []);

        var dependencies = Deps(("author/a", "1.0.0"));

        var tree =
            FileTree.MergeFiles(
                Tree(Application(indirect ? Deps() : dependencies, indirect ? dependencies : Deps())),
                Source("Main", "import Internal.Hidden exposing (value)\nroot = 1\nunused = Internal.Hidden.value"));

        var build =
            await ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                tree,
                ["elm.json"],
                [["src", "Main.elm"]],
                new(),
                provider);

        var parsed = PreparedSource(build, ["src", "Main.elm"]);
        var importedName = string.Join(".", parsed.Imports.Single().Value.ModuleName.Value);
        importedName.Should().StartWith("PineUnavailable.");
        build.CompilerModuleNames.Values.Should().NotContain(importedName);

        string.Join(".", parsed.Imports.Single().Value.ModuleAlias!.Value.Alias.Value)
            .Should().StartWith("PineDependency");

        build.ImportDiagnostics.Should().ContainSingle();
        build.ImportDiagnostics[0].Message.Should().Contain("private modules or indirect/undeclared");
        build.CompilerModuleNames["elm-packages/author/a/src/Internal.Hidden.elm"].Should().StartWith("PinePackage.");
    }

    [Fact]
    public async Task Declaration_demand_preparation_uses_root_independent_owner_identities()
    {
        var provider =
            new Provider()
            .Add(
                "author/a",
                "1.0.0",
                sources: FileTree.MergeFiles(Source("Shared", "value = 1"), Source("Internal.Unused", "value = 10")),
                exposedModules: ["Shared"])
            .Add(
                "author/b",
                "1.0.0",
                sources: FileTree.MergeFiles(Source("Shared", "value = 2"), Source("Internal.Unused", "value = 20")),
                exposedModules: ["Shared"]);

        var tree =
            FileTree.MergeFiles(
                FileTree.MergeFiles(
                    Tree(Application(Deps(("author/a", "1.0.0"), ("author/b", "1.0.0")))),
                    Source("Main", "import Shared\nvalue = 1")),
                Source("Other", "value = 2"));

        var build =
            await ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                tree,
                ["elm.json"],
                [["src", "Main.elm"]],
                new(),
                provider);

        var rootless =
            await ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                tree,
                ["elm.json"],
                [],
                new(),
                provider);

        var other =
            await ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                tree,
                ["elm.json"],
                [["src", "Other.elm"]],
                new(),
                provider);

        build.CompilerModuleNames.Should().BeEquivalentTo(rootless.CompilerModuleNames).And.BeEquivalentTo(
            other.CompilerModuleNames);

        build.Sources.Should().Be(rootless.Sources).And.Be(other.Sources);

        build.ImportDiagnostics.Should().BeEquivalentTo(rootless.ImportDiagnostics).And.BeEquivalentTo(
            other.ImportDiagnostics);

        build.Resolution.Fingerprint.Should().Be(rootless.Resolution.Fingerprint).And.Be(other.Resolution.Fingerprint);
        build.CompilerModuleNames.Values.Should().OnlyHaveUniqueItems();
        build.CompilerModuleNames.Should().HaveCount(6);
        build.CompilerModuleSourcePaths.Should().HaveCount(6);

        foreach (var (path, name) in build.CompilerModuleNames)
            build.CompilerModuleSourcePaths[name].Should().Be(path);

        build.ImportDiagnostics.Should().ContainSingle();
        build.ImportDiagnostics[0].Message.Should().Contain("ambiguous").And.Contain("author/a").And.Contain("author/b");

        var importedName =
            string.Join(".", PreparedSource(build, ["src", "Main.elm"]).Imports.Single().Value.ModuleName.Value);

        build.CompilerModuleNames.Values.Should().NotContain(importedName);
    }

    [Fact]
    public async Task Declaration_demand_preparation_reports_rootless_invalid_imported_APIs()
    {
        var apiSources =
            FileTree.FromSetOfFilesWithStringPath(
                [
                (new[] { "src", "Api.elm" },
                (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(
                    "module Api exposing (value, Closed, Open(..))\nvalue = 1\nprivate = 2\ntype Closed = Closed\ntype Open = Open\n"))
                ]);

        var provider = new Provider().Add("author/a", "1.0.0", sources: apiSources, exposedModules: ["Api"]);

        var tree =
            FileTree.MergeFiles(
                Tree(Application(Deps(("author/a", "1.0.0")))),
                Source(
                    "Disconnected",
                    "import Api exposing (value, private, absent, Closed(..), Open(..))\nvalue = 1"));

        var build =
            await ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(tree, ["elm.json"], [], new(), provider);

        build.Resolution.Succeeded.Should().BeTrue();
        build.ImportDiagnostics.Should().HaveCount(3);

        build.ImportDiagnostics.Select(diagnostic => diagnostic.Message)
            .Should().Contain(message => message.Contains("'private'"))
            .And.Contain(message => message.Contains("'absent'"))
            .And.Contain(message => message.Contains("'Closed(..)'"));

        build.ImportDiagnostics.Should().OnlyContain(
            diagnostic => diagnostic.FilePath == "src/Disconnected.elm" &&
                diagnostic.ImportRange.Start.Row == 2 && diagnostic.ModuleName == "Disconnected");

        var importedName =
            string.Join(
                ".",
                PreparedSource(build, ["src", "Disconnected.elm"]).Imports.Single().Value.ModuleName.Value);

        importedName.Should().Be(build.CompilerModuleNames["elm-packages/author/a/src/Api.elm"]);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Prepared_module_origins_preserve_project_test_and_package_provenance(bool declarationDemand)
    {
        var provider =
            new Provider()
            .Add(
                "author/a",
                "1.0.0",
                sources: FileTree.MergeFiles(
                    Source("A", "import Internal.Helper\nvalue = Internal.Helper.value"),
                    Source("Internal.Helper", "value = 1")),
                exposedModules: ["A"])
            .Add(
                "author/b",
                "2.0.0",
                sources: FileTree.MergeFiles(
                    Source("B", "import Internal.Helper\nvalue = Internal.Helper.value"),
                    Source("Internal.Helper", "value = 2")),
                exposedModules: ["B"]);

        var substitution =
            ElmPackageSubstitution.Create(
                "author/replacement",
                "replacement-v1",
                ["3.0.0"],
                Source("Replacement", "value = 3"),
                ["Replacement"]);

        var manifest =
            Application(
                Deps(("author/a", "1.0.0"), ("author/b", "2.0.0"), ("author/replacement", "3.0.0")),
                sourceDirectories: ["src", "../src"]);

        var tree =
            Source("Shared", "value = 4")
            .SetNodeAtPathSorted(["example", "elm.json"], FileTree.File(Encoding.UTF8.GetBytes(manifest)))
            .SetNodeAtPathSorted(
                ["example", "tests", "Tests.elm"],
                FileTree.File(
                    """
                    module Tests exposing (value)
                    import A
                    import B
                    import Replacement
                    import Shared
                    value = A.value + B.value + Replacement.value + Shared.value
                    """u8.ToArray()));

        var configuration =
            new ElmDependencyResolutionConfiguration { IncludeTests = true, Substitutions = [substitution] };

        var build =
            await (declarationDemand
            ?
            ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                tree,
                ["example", "elm.json"],
                [["example", "tests", "Tests.elm"]],
                configuration,
                provider)
            :
            ElmResolvedBuildPreparation.PrepareAsync(
                tree,
                ["example", "elm.json"],
                [["example", "tests", "Tests.elm"]],
                configuration,
                provider));

        build.CompilerModuleOrigins.Keys.Should().BeEquivalentTo(build.CompilerModuleSourcePaths.Keys);

        build.CompilerModuleOrigins["Tests"].Should().Be(
            new ElmModuleOrigin.Project("Tests", "example/tests/Tests.elm", "example/elm.json", true));

        build.CompilerModuleOrigins["Shared"].Should().Be(
            new ElmModuleOrigin.Project("Shared", "src/Shared.elm", "example/elm.json", false));

        foreach (var (packageName, version) in new[] { ("author/a", "1.0.0"), ("author/b", "2.0.0") })
        {
            var path = "elm-packages/" + packageName + "/src/Internal.Helper.elm";
            var compilerName = build.CompilerModuleNames[path];

            compilerName.Should().StartWith("PinePackage.");

            build.CompilerModuleOrigins[compilerName].Should().Be(
                new ElmModuleOrigin.PublishedPackage(
                    "Internal.Helper",
                    path,
                    "elm-packages/" + packageName + "/elm.json",
                    new ElmPackageIdentity(packageName, ElmPackageVersion.Parse(version)),
                    "registry:" + packageName + "@" + version));
        }

        var replacementName = build.CompilerModuleNames["elm-packages/author/replacement/src/Replacement.elm"];

        build.CompilerModuleOrigins[replacementName].Should().Be(
            new ElmModuleOrigin.Substitution(
                "Replacement",
                "elm-packages/author/replacement/src/Replacement.elm",
                "elm-packages/author/replacement/elm.json",
                new ElmPackageIdentity("author/replacement", ElmPackageVersion.Parse("3.0.0")),
                "replacement-v1"));

        using var debugJson = JsonDocument.Parse(build.ToDebugJson());
        var origins = debugJson.RootElement.GetProperty("CompilerModuleOrigins");

        origins.GetProperty("Tests").GetProperty("Project")[1].GetString()
            .Should().Be("example/tests/Tests.elm");

        origins.GetProperty(replacementName).GetProperty("Substitution")[4].GetString()
            .Should().Be("replacement-v1");

        var roundTripped =
            JsonSerializer.Deserialize<ImmutableDictionary<string, ElmModuleOrigin>>(origins.GetRawText());

        roundTripped.Should().BeEquivalentTo(build.CompilerModuleOrigins);
    }

    [Fact]
    public void Module_origin_variants_expose_only_the_metadata_valid_for_their_source()
    {
        var package = new ElmPackageIdentity("author/a", ElmPackageVersion.Parse("1.0.0"));

        ElmModuleOrigin[] origins =
            [
            new ElmModuleOrigin.Project("Main", "src/Main.elm", "elm.json", false),
            new ElmModuleOrigin.PublishedPackage(
                "A",
                "elm-packages/author/a/src/A.elm",
                "elm-packages/author/a/elm.json",
                package,
                "registry:author/a@1.0.0"),
            new ElmModuleOrigin.Substitution(
                "A",
                "elm-packages/author/a/src/A.elm",
                "elm-packages/author/a/elm.json",
                package,
                "replacement-v1"),
            ];

        var json = JsonSerializer.Serialize(origins);
        using var document = JsonDocument.Parse(json);

        document.RootElement[0].GetProperty("Project").GetArrayLength().Should().Be(4);

        typeof(ElmModuleOrigin.Project).GetProperties().Select(property => property.Name)
            .Should().BeEquivalentTo(["ModuleName", "SourcePath", "ManifestPath", "IsTestModule"]);

        document.RootElement[1].GetProperty("PublishedPackage").GetArrayLength().Should().Be(5);

        typeof(ElmModuleOrigin.PublishedPackage).GetProperties().Select(property => property.Name)
            .Should().BeEquivalentTo(["ModuleName", "SourcePath", "ManifestPath", "Package", "ProviderOrigin"]);

        document.RootElement[2].GetProperty("Substitution").GetArrayLength().Should().Be(5);

        typeof(ElmModuleOrigin.Substitution).GetProperties().Select(property => property.Name)
            .Should().BeEquivalentTo(
            [
            "ModuleName",
            "SourcePath",
            "ManifestPath",
            "ReplacedPackage",
            "ImplementationId"
            ]);

        JsonSerializer.Deserialize<ElmModuleOrigin[]>(json).Should().Equal(origins);

        var roundTripped = JsonSerializer.Deserialize<ElmModuleOrigin[]>(json)!;

        for (var index = 0; index < origins.Length; ++index)
        {
            (roundTripped[index] == origins[index]).Should().BeTrue();
            roundTripped[index].GetHashCode().Should().Be(origins[index].GetHashCode());
        }

        (origins[1] == origins[2]).Should().BeFalse();
        (origins[0] == null).Should().BeFalse();
    }

    [Fact]
    public void Module_origin_hierarchy_is_closed_and_requires_variant_metadata()
    {
        typeof(ElmModuleOrigin).GetConstructors(BindingFlags.Instance | BindingFlags.Public | BindingFlags.NonPublic)
            .Should().OnlyContain(constructor => constructor.IsPrivate);

        typeof(ElmModuleOrigin).GetNestedTypes().Where(type => typeof(ElmModuleOrigin).IsAssignableFrom(type))
            .Should().HaveCount(3).And.OnlyContain(type => type.IsSealed);

        Action missingPackage =
            () => new ElmModuleOrigin.PublishedPackage("A", "src/A.elm", "elm.json", null!, "registry");

        Action missingReplacement =
            () => new ElmModuleOrigin.Substitution(
                "A",
                "src/A.elm",
                "elm.json",
                new("author/a", ElmPackageVersion.Parse("1.0.0")),
                null!);

        missingPackage.Should().Throw<ArgumentNullException>();
        missingReplacement.Should().Throw<ArgumentNullException>();
    }

    [Fact]
    public async Task Declaration_demand_preparation_accepts_compiler_native_List_type_import()
    {
        var tree =
            FileTree.MergeFiles(
                Tree(Application(Deps(("elm/core", "1.0.5")))),
                Source("Main", "import List exposing (List)\nvalue : List Int\nvalue = []"));

        var build =
            await ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                tree,
                [
                "elm.json"
                ],
                [],
                ElmPackageSubstitutions.DefaultBuild.Value,
                new Provider { RejectRequests = true });

        build.ImportDiagnostics.Should().NotContain(diagnostic => diagnostic.FilePath == "src/Main.elm");
    }

    [Fact]
    public async Task Declaration_demand_preparation_reports_invalid_native_imported_APIs_without_inventing_sources()
    {
        var tree =
            FileTree.MergeFiles(
                Tree(Application(Deps(("elm/core", "1.0.5")))),
                Source(
                    "Main",
                    "import Basics exposing (Int, Bool(..), identity, missing)\nimport Debug exposing (log, absent)\nvalue = 1"));

        var build =
            await ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                tree,
                [
                "elm.json"
                ],
                [],
                ElmPackageSubstitutions.DefaultBuild.Value,
                new Provider { RejectRequests = true });

        build.ImportDiagnostics.Should().HaveCount(2);

        build.ImportDiagnostics.Select(diagnostic => diagnostic.Message)
            .Should().Contain(message => message.Contains("'missing'")).And.Contain(message => message.Contains("'absent'"));

        build.CompilerModuleNames.Values.Should().NotContain("Debug");
        build.CompilerModuleSyntax.Keys.Should().NotContain("Debug");
    }

    [Fact]
    public async Task Declaration_demand_preparation_preserves_original_ranges_in_rewritten_syntax()
    {
        var original =
            """
            module Main exposing (value)
            import Internal.Missing


            value=Internal.Missing.value
            """;

        var tree =
            Tree(Application(Deps()))
            .SetNodeAtPathSorted(["src", "Main.elm"], FileTree.File(Encoding.UTF8.GetBytes(original)));

        var build =
            await ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(tree, ["elm.json"], [], new(), new Provider());

        var parsedOriginal =
            ElmSyntaxParser.ParseModuleText(original).Extract(
                error => throw new InvalidOperationException(error.ToString()));

        var syntax = build.CompilerModuleSyntax["Main"];

        syntax.Declarations.Single().Range.Should().Be(parsedOriginal.Declarations.Single().Range);
        syntax.Imports.Single().Range.Should().Be(parsedOriginal.Imports.Single().Range);

        syntax.Imports.Single().Value.ModuleName.Range.Should().Be(
            parsedOriginal.Imports.Single().Value.ModuleName.Range);

        string.Join(".", syntax.Imports.Single().Value.ModuleName.Value).Should().StartWith("PineUnavailable.");
        build.CompilerModuleSourcePaths["Main"].Should().Be("src/Main.elm");
        build.ProjectSources.GetNodeAtPath(["src", "Main.elm"]).Should().Be(tree.GetNodeAtPath(["src", "Main.elm"]));
    }

    [Theory]
    [InlineData("Fuzz")]
    [InlineData("Internal.Fuzz")]
    public async Task Declaration_demand_preparation_rewrites_private_self_qualified_references_preserving_ranges(
        string moduleName)
    {
        var original =
            $$"""
            module {{moduleName}} exposing (root)

            type Fuzzer = Fuzzer Int
            type Nested = Nested {{moduleName}}.Fuzzer

            root : {{moduleName}}.Fuzzer
            root =
                case {{moduleName}}.make of
                    {{moduleName}}.Fuzzer _ ->
                        {{moduleName}}.make

            make = {{moduleName}}.Fuzzer 1
            message = "{{moduleName}}.Fuzzer"
            -- {{moduleName}}.Fuzzer stays literal in comments.
            """;

        var sourcePath = new[] { "src", moduleName + ".elm" };

        var packageSources =
            FileTree.FromSetOfFilesWithStringPath(
                [(sourcePath, (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(original))]);

        var provider =
            new Provider().Add("author/fuzz", "1.0.0", sources: packageSources, exposedModules: [moduleName]);

        var tree =
            FileTree.MergeFiles(
                Tree(Application(Deps(("author/fuzz", "1.0.0")))),
                Source("Main", $"import {moduleName} exposing (Fuzzer(..))\nvalue = 1"));

        var build =
            await ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(tree, ["elm.json"], [], new(), provider);

        var compilerName = build.CompilerModuleNames["elm-packages/author/fuzz/" + string.Join("/", sourcePath)];
        var compilerNamespace = compilerName.Split('.');
        var syntax = build.CompilerModuleSyntax[compilerName];

        var parsedOriginal =
            ElmSyntaxParser.ParseModuleText(original).Extract(
                error => throw new InvalidOperationException(error.ToString()));

        var root = Function(syntax, "root");
        var originalRoot = Function(parsedOriginal, "root");
        var signature = (Syntax.TypeAnnotation.Typed)root.Signature!.Value.TypeAnnotation.Value;
        signature.TypeName.Value.ModuleName.Should().Equal(compilerNamespace);
        root.Signature.Value.TypeAnnotation.Range.Should().Be(originalRoot.Signature!.Value.TypeAnnotation.Range);

        signature.TypeName.Range.Should().Be(
            ((Syntax.TypeAnnotation.Typed)originalRoot.Signature.Value.TypeAnnotation.Value).TypeName.Range);

        var body = (Syntax.Expression.CaseExpression)root.Declaration.Value.Expression.Value;
        var originalBody = (Syntax.Expression.CaseExpression)originalRoot.Declaration.Value.Expression.Value;
        ((Syntax.Expression.Identifier)body.CaseBlock.Expression.Value).ModuleName.Should().Equal(compilerNamespace);
        body.CaseBlock.Expression.Range.Should().Be(originalBody.CaseBlock.Expression.Range);
        var branch = body.CaseBlock.Cases.Single();
        ((Syntax.Pattern.NamedPattern)branch.Pattern.Value).Name.ModuleName.Should().Equal(compilerNamespace);
        ((Syntax.Expression.Identifier)branch.Expression.Value).ModuleName.Should().Equal(compilerNamespace);
        branch.Pattern.Range.Should().Be(originalBody.CaseBlock.Cases.Single().Pattern.Range);
        branch.Expression.Range.Should().Be(originalBody.CaseBlock.Cases.Single().Expression.Range);

        var make = (Syntax.Expression.Application)Function(syntax, "make").Declaration.Value.Expression.Value;
        ((Syntax.Expression.Identifier)make.Function.Value).ModuleName.Should().Equal(compilerNamespace);

        var nested =
            syntax.Declarations.Select(node => node.Value).OfType<Syntax.Declaration.ChoiceTypeDeclaration>()
            .Single(declaration => declaration.TypeDeclaration.Name.Value == "Nested");

        ((Syntax.TypeAnnotation.Typed)nested.TypeDeclaration.Constructors.Single().Value.Arguments.Single().Value)
            .TypeName.Value.ModuleName.Should().Equal(compilerNamespace);

        syntax.Declarations.Select(declaration => declaration.Range)
            .Should().Equal(parsedOriginal.Declarations.Select(declaration => declaration.Range));

        ((Syntax.Module.NormalModule)syntax.ModuleDefinition.Value).ModuleData.ExposingList
            .Should().Be(((Syntax.Module.NormalModule)parsedOriginal.ModuleDefinition.Value).ModuleData.ExposingList);

        Function(syntax, "message").Should().Be(Function(parsedOriginal, "message"));
        syntax.Comments.Should().Equal(parsedOriginal.Comments);
        build.ImportDiagnostics.Should().ContainSingle();
        build.ImportDiagnostics[0].Message.Should().Contain("'Fuzzer(..)'").And.Contain("does not expose");

        static Syntax.FunctionStruct Function(Syntax.File file, string name) =>
            file.Declarations.Select(declaration => declaration.Value).OfType<Syntax.Declaration.FunctionDeclaration>()
            .Single(declaration => declaration.Function.Declaration.Value.Name.Value == name).Function;
    }

    [Fact]
    public async Task Declaration_demand_preparation_preserves_import_alias_matching_the_current_module_name()
    {
        var codec =
            """
            module Encode exposing (read)
            import Bytes.Encode as Encode
            type Encoder = Encoder String
            value : Encode.Encoder
            value = Encode.Encoder 7
            read = Encode.encode value
            """;

        var bytes =
            """
            module Bytes.Encode exposing (Encoder(..), encode)
            type Encoder = Encoder Int
            encode (Encoder value) = value
            """;

        var provider =
            new Provider()
            .Add(
                "author/codec",
                "1.0.0",
                dependencies: Deps(("author/bytes", "1.0.0 <= v < 2.0.0")),
                exposedModules: ["Encode"],
                sources: FileTree.FromSetOfFilesWithStringPath(
                    [(new[] { "src", "Encode.elm" }, (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(codec))]))
            .Add(
                "author/bytes",
                "1.0.0",
                exposedModules: ["Bytes.Encode"],
                sources: FileTree.FromSetOfFilesWithStringPath(
                    [
                    (new[] { "src", "Bytes", "Encode.elm" }, (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(bytes))
                    ]));

        var tree =
            FileTree.MergeFiles(
                Tree(Application(Deps(("author/codec", "1.0.0")), Deps(("author/bytes", "1.0.0")))),
                Source("Main", "import Encode\nresult = Encode.read"));

        var build =
            await ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                tree,
                ["elm.json"],
                [],
                new(),
                provider);

        var compilerName = build.CompilerModuleNames["elm-packages/author/codec/src/Encode.elm"];
        var syntax = build.CompilerModuleSyntax[compilerName];

        var functions =
            syntax.Declarations.Select(node => node.Value)
            .OfType<Syntax.Declaration.FunctionDeclaration>()
            .ToDictionary(function => function.Function.Declaration.Value.Name.Value, function => function.Function);

        var signature = (Syntax.TypeAnnotation.Typed)functions["value"].Signature!.Value.TypeAnnotation.Value;
        signature.TypeName.Value.ModuleName.Should().Equal("Encode");
        var expression = (Syntax.Expression.Application)functions["read"].Declaration.Value.Expression.Value;
        ((Syntax.Expression.Identifier)expression.Function.Value).ModuleName.Should().Equal("Encode");
        syntax.Imports.Single().Value.ModuleAlias!.Value.Alias.Value.Should().Equal("Encode");

        syntax.Imports.Single().Value.ModuleName.Value.Should().Equal(
            build.CompilerModuleNames["elm-packages/author/bytes/src/Bytes/Encode.elm"].Split('.'));

        ElmCompiler.CompileResolvedEnvironment(build, [DeclQualifiedName.Create(["Main"], "result")])
            .IsOkOrNullable().Should().NotBeNull();
    }

    [Fact]
    public async Task Declaration_demand_preparation_rejects_duplicate_ownership_in_disconnected_sources()
    {
        var tree =
            FileTree.MergeFiles(
                FileTree.MergeFiles(Tree(Application(Deps())), Source("Same", "value = 1")),
                Source("Same", "value = 2").SetNodeAtPathSorted(
                    ["src", "Other.elm"],
                    FileTree.File("module Same exposing (..)\nvalue = 3\n"u8.ToArray())));

        Func<Task> prepare =
            () => ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(tree, ["elm.json"], [], new(), new Provider());

        await prepare.Should().ThrowAsync<ElmDependencyResolutionException>().WithMessage("*declared more than once*");
    }

    [Fact]
    public async Task Declaration_demand_preparation_rejects_package_source_path_ownership_collisions()
    {
        var provider =
            new Provider().Add("author/a", "1.0.0", sources: Source("Api", "value = 1"), exposedModules: ["Api"]);

        var tree =
            Tree(Application(Deps(("author/a", "1.0.0")), sourceDirectories: ["."]))
            .SetNodeAtPathSorted(
                ["elm-packages", "author", "a", "src", "Api.elm"],
                FileTree.File("module ProjectApi exposing (..)\nvalue = 2\n"u8.ToArray()));

        Func<Task> prepare =
            () => ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(tree, ["elm.json"], [], new(), provider);

        await prepare.Should().ThrowAsync<ElmDependencyResolutionException>()
        .WithMessage("*owned by both*must not overwrite project sources*");
    }

    [Theory]
    [InlineData("Basics")]
    [InlineData("Debug")]
    [InlineData("List")]
    [InlineData("Platform")]
    [InlineData("Platform.Cmd")]
    public async Task Declaration_demand_preparation_prevents_project_core_shadowing(string name)
    {
        var tree =
            FileTree.MergeFiles(Tree(Application(Deps(("elm/core", "1.0.5")))), Source(name, "value = 1"));

        Func<Task> prepare =
            () => ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                tree,
                [
                "elm.json"
                ],
                [],
                ElmPackageSubstitutions.DefaultBuild.Value,
                new Provider { RejectRequests = true });

        await prepare.Should().ThrowAsync<ElmDependencyResolutionException>().WithMessage("*conflicts with an elm/core*");
    }

    [Fact]
    public void Shared_parent_sources_use_the_selected_application_manifest()
    {
        var tree =
            Tree(Package(Deps()))
            .SetNodeAtPathSorted(
                [
                "src",
                "Shared.elm"
                ],
                FileTree.File("module Shared exposing (value)\nvalue = 1\n"u8.ToArray()))
            .SetNodeAtPathSorted(
                ["example", "elm.json"],
                FileTree.File(Encoding.UTF8.GetBytes(Application(Deps(), sourceDirectories: ["src", "../src"]))))
            .SetNodeAtPathSorted(
                [
                "example",
                "src",
                "Main.elm"
                ],
                FileTree.File("module Main exposing (value)\nvalue = 1\n"u8.ToArray()));

        var manifest = ElmDependencyResolver.ReadManifest(tree, ["example", "elm.json"]);
        var selected = ElmResolvedBuildPreparation.SelectProjectSources(tree, ["example", "elm.json"], manifest, false);
        selected.GetNodeAtPath(["src", "Shared.elm"]).Should().NotBeNull();

        ElmAppDependencyResolution.FindElmJsonForEntryPoint(tree, ["example", "src", "Main.elm"])!.Value.filePath
            .Should().Equal("example", "elm.json");
    }

    [Fact]
    public async Task Resolution_fingerprint_changes_with_substitution_sources_and_test_mode()
    {
        var substitution = ElmPackageSubstitution.Create("author/a", "a", ["1.0.0"], Source("A", "value = 1"), ["A"]);
        var configuration = new ElmDependencyResolutionConfiguration { Substitutions = [substitution] };
        var manifest = Package(Deps(("author/a", "1.0.0 <= v < 2.0.0")));
        var first = await Resolve(manifest, new Provider(), configuration);

        var changed =
            await Resolve(
                manifest,
                new Provider(),
                configuration with
                {
                    Substitutions = [substitution with { Sources = Source("A", "value = 2") }],
                });

        var tests = await Resolve(manifest, new Provider(), configuration with { IncludeTests = true });
        first.Fingerprint.Should().NotBe(changed.Fingerprint).And.NotBe(tests.Fingerprint);
    }

    [Fact]
    public async Task Explicit_resolution_lock_reproduces_versions_after_new_releases()
    {
        var provider = new Provider().Add("author/a", "1.0.0");
        var manifest = Package(Deps(("author/a", "1.0.0 <= v < 2.0.0")));
        var original = await Resolve(manifest, provider);
        provider.Add("author/a", "1.1.0");
        var unlocked = await Resolve(manifest, provider);

        var locked =
            await Resolve(
                manifest,
                provider,
                new()
                {
                    LockedVersions =
                    original.Packages.ToImmutableDictionary(item => item.Key, item => item.Value.Identity.Version),
                });

        unlocked.Packages["author/a"].Identity.Version.ToString().Should().Be("1.1.0");
        locked.Packages["author/a"].Identity.Version.ToString().Should().Be("1.0.0");
        locked.Requirements.Should().Contain(item => item.Scope == ElmDependencyScope.ResolutionLock);
    }

    [Fact]
    public async Task Duplicate_json_declarations_are_not_silently_overwritten()
    {
        var manifest =
            Application(Deps(("author/a", "1.0.0")))
            .Replace("\"author/a\":\"1.0.0\"", "\"author/a\":\"1.0.0\",\"author/a\":\"2.0.0\"");

        var report = await Resolve(manifest, new Provider { RejectRequests = true });
        report.Failures[0].Kind.Should().Be(ElmResolutionFailureKind.InvalidManifest);
        report.ManifestText.Should().Be(manifest);
        report.Failures[0].Message.Should().Contain("Duplicate JSON property").And.Contain("author/a");
    }

    [Theory]
    [InlineData("\"direct\":{}", "\"direct\":null", "direct")]
    [InlineData("\"source-directories\":[\"src\"]", "\"source-directories\":[null]", "source-directories")]
    [InlineData("\"source-directories\":[\"src\"]", "\"source-directories\":[\"/absolute\"]", "source-directories")]
    [InlineData("\"dependencies\":{", "\"dependencies\":{\"author/a\":99,", "author/a")]
    public async Task Malformed_manifest_fields_are_classified_and_preserve_the_original_json(
        string original, string malformed, string field)
    {
        var manifest = Application(Deps()).Replace(original, malformed);
        var report = await Resolve(manifest, new Provider { RejectRequests = true });
        report.Failures[0].Kind.Should().Be(ElmResolutionFailureKind.InvalidManifest);
        report.Failures[0].Message.Should().Contain(field);
        report.ManifestText.Should().Be(manifest);
    }

    [Fact]
    public async Task Null_exposed_module_sections_are_reported_as_invalid_metadata()
    {
        var manifest = Package(Deps()).Replace("\"exposed-modules\":[]", "\"exposed-modules\":{\"Bad\":null}");
        var report = await Resolve(manifest, new Provider { RejectRequests = true });
        report.Failures[0].Kind.Should().Be(ElmResolutionFailureKind.InvalidManifest);
        report.Failures[0].Message.Should().Contain("exposed-modules");
        report.ManifestText.Should().Be(manifest);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Packages_with_identically_named_private_modules_compile_independently(bool explicitAlias)
    {
        var dependencies = Deps(("elm/core", "1.0.0 <= v < 2.0.0"));
        var qualifier = explicitAlias ? "Helper" : "Internal.Helper";

        var aDeclarations =
            $$"""
            import Internal.Helper{{(explicitAlias ? " as Helper" : "")}}
            -- Internal.Helper.value must stay literal in comments and strings.
            type alias Wrapped = { value : {{qualifier}}.Value }
            unwrap : Wrapped -> Int
            unwrap wrapped =
                case wrapped.value of
                    {{qualifier}}.Value number ->
                        number
            value =
                let
                    wrapped = { value = {{qualifier}}.value }
                in
                unwrap wrapped
            message = "Internal.Helper.value"
            """;

        var provider =
            new Provider()
            .Add(
                "author/a",
                "1.0.0",
                dependencies,
                sources: FileTree.MergeFiles(
                    FileTree.MergeFiles(
                        Source("A", aDeclarations),
                        Source("Internal.Helper", "type Value = Value Int\nvalue = Value 1")),
                    Source("Internal.Unused", "value = 100")),
                exposedModules: ["A"])
            .Add(
                "author/b",
                "1.0.0",
                dependencies,
                sources: FileTree.MergeFiles(
                    FileTree.MergeFiles(
                        Source("B", "import Internal.Helper as Helper\nvalue = Helper.value"),
                        Source("Internal.Helper", "value = 2")),
                    Source("Internal.Unused", "value = 200")),
                exposedModules: ["B"]);

        var tree =
            Tree(Application(Deps(("author/a", "1.0.0"), ("author/b", "1.0.0"), ("elm/core", "1.0.5"))))
            .SetNodeAtPathSorted(
                ["src", "Main.elm"],
                FileTree.File(
                    "module Main exposing (value)\nimport A\nimport B\nvalue = A.value + B.value\n"u8.ToArray()));

        var build =
            await ElmResolvedBuildPreparation.PrepareAsync(
                tree,
                ["elm.json"],
                [["src", "Main.elm"]],
                ElmPackageSubstitutions.DefaultBuild.Value,
                provider);

        var helperNames =
            build.CompilerModuleNames
            .Where(item => item.Key.EndsWith("Internal.Helper.elm")).Select(item => item.Value).ToArray();

        helperNames.Should().HaveCount(2).And.OnlyHaveUniqueItems();

        var compilation =
            ElmCompiler.CompileResolvedEnvironment(
                build,
                rootDeclarations: [DeclQualifiedName.Create(["Main"], "value")]);

        var environment =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(
                ElmSourceCompilation.EvaluateZeroParameterRoots(
                    compilation.Extract(error => throw new InvalidOperationException(error)).compiledEnvValue,
                    Pine.Core.Interpreter.DirectInterpreter.WithLocalEvalCache(new PineVMParseCache()))
                .Extract(error => throw new InvalidOperationException(error)))
            .Extract(error => throw new InvalidOperationException(error));

        environment.Modules.Single(module => module.moduleName == "Main")
            .moduleContent.FunctionDeclarations["value"].Should().Be(IntegerEncoding.EncodeSignedInteger(3));

        build.ToDebugJson().Should().Contain("CompilerModuleNames").And.Contain("PinePackage");

        var aText =
            Encoding.UTF8.GetString(
                build.Sources.GetNodeAtPath(["elm-packages", "author", "a", "src", "A.elm"])!
                .EnumerateFilesTransitive().Single().fileContent.Span);

        aText.Should().Contain("\"Internal.Helper.value\"").And.Contain("-- Internal.Helper.value");
    }

    [Fact]
    public async Task Disjoint_version_sets_choose_their_own_terminal_implementation()
    {
        var first =
            ElmPackageSubstitution.Create("author/a", "old", ["1.0.0", "1.0.1"], Source("A", "value = 1"), ["A"]);

        var second =
            ElmPackageSubstitution.Create("author/a", "new", ["2.0.0", "2.1.0"], Source("A", "value = 2"), ["A"]);

        var provider = new Provider { RejectRequests = true };
        var configuration = new ElmDependencyResolutionConfiguration { Substitutions = [first, second] };
        var report = await Resolve(Package(Deps(("author/a", "1.0.0 <= v < 2.0.0"))), provider, configuration);
        report.Succeeded.Should().BeTrue();
        report.Packages["author/a"].SubstitutionImplementationId.Should().Be("old");
        report.Packages["author/a"].Identity.Version.ToString().Should().Be("1.0.1");

        var overlapping =
            async () => await Resolve(
                Package(Deps()),
                provider,
                configuration with
                {
                    Substitutions = [first, second with { Versions = first.Versions }],
                });

        await overlapping.Should().ThrowAsync<ArgumentException>().WithMessage("*disjoint*");
    }

    [Fact]
    public async Task Downloaded_sources_must_match_the_metadata_and_keep_failure_details()
    {
        var provider = new Provider().Add("author/a", "1.0.0");
        provider.SourceOverride = Tree(Package(Deps(), name: "author/wrong"));

        var action =
            async () => await ElmResolvedBuildPreparation.PrepareAsync(
                Tree(Application(Deps(("author/a", "1.0.0")))),
                ["elm.json"],
                [],
                new(),
                provider);

        var exception = (await action.Should().ThrowAsync<ElmDependencyResolutionException>()).Which;
        exception.Report.Failures[0].Kind.Should().Be(ElmResolutionFailureKind.InvalidManifest);
        exception.Report.Failures[0].ExceptionDetail.Should().Contain("Source archive").And.Contain("author/a");
        exception.Report.Trace.Should().Contain(entry => entry.Action == "Metadata");
    }

    [Fact]
    public async Task Offline_registry_uses_local_Elm_metadata_without_claiming_a_complete_version_listing()
    {
        var directory = Path.Combine(Path.GetTempPath(), "pine-package-provider-" + Guid.NewGuid().ToString("N"));
        var elmHome = Path.Combine(directory, "elm");
        var packageDirectory = Path.Combine(elmHome, "0.19.1", "packages", "author", "a", "1.0.0");

        try
        {
            Directory.CreateDirectory(Path.Combine(packageDirectory, "src"));
            File.WriteAllText(Path.Combine(packageDirectory, "elm.json"), Package(Deps(), name: "author/a"));
            File.WriteAllText(Path.Combine(packageDirectory, "src", "A.elm"), "module A exposing (value)\nvalue = 1\n");

            var provider =
                new ElmRegistryPackageProvider(
                    offline: true,
                    cacheDirectory: Path.Combine(directory, "metadata"),
                    sourceCacheDirectories: [],
                    elmHome: elmHome);

            var versions = await provider.GetVersionsAsync("author/a", CancellationToken.None);
            versions.IsComplete.Should().BeFalse();
            versions.Versions.Should().ContainSingle().Which.Should().Be(ElmPackageVersion.Parse("1.0.0"));

            var available =
                await ElmDependencyResolver.ResolveAsync(
                    Tree(Package(Deps(("author/a", "1.0.0 <= v < 2.0.0")))),
                    [
                    "elm.json"
                    ],
                    new() { Offline = true },
                    provider);

            available.Succeeded.Should().BeTrue();

            var unavailable =
                await ElmDependencyResolver.ResolveAsync(
                    Tree(Package(Deps(("author/missing", "1.0.0 <= v < 2.0.0")))),
                    [
                    "elm.json"
                    ],
                    new() { Offline = true },
                    provider);

            unavailable.Failures[0].Kind.Should().Be(ElmResolutionFailureKind.OfflineUnavailable);
        }
        finally
        {
            if (Directory.Exists(directory))
                Directory.Delete(directory, recursive: true);
        }
    }

    [Fact]
    public async Task Resolution_fingerprints_ignore_cache_origins_but_reports_retain_them()
    {
        var manifest = Package(Deps(("author/a", "1.0.0 <= v < 2.0.0")));
        var first = await Resolve(manifest, new Provider { OriginPrefix = "registry:" }.Add("author/a", "1.0.0"));
        var second = await Resolve(manifest, new Provider { OriginPrefix = "cache:" }.Add("author/a", "1.0.0"));
        first.Fingerprint.Should().Be(second.Fingerprint);
        first.Packages["author/a"].Origin.Should().NotBe(second.Packages["author/a"].Origin);
    }

    [Fact]
    public async Task Private_package_modules_do_not_rename_application_roots_or_their_requested_declarations()
    {
        var provider =
            new Provider().Add(
                "author/a",
                "1.0.0",
                Deps(("elm/core", "1.0.0 <= v < 2.0.0")),
                sources:
                FileTree.MergeFiles(Source("A", "import Main\nvalue = Main.value"), Source("Main", "value = 71")),
                exposedModules: ["A"]);

        var sources =
            Tree(Application(Deps(("author/a", "1.0.0"), ("elm/core", "1.0.5"))))
            .SetNodeAtPathSorted(
                ["src", "Main.elm"],
                FileTree.File(
                    "module Main exposing (value)\nimport A\nvalue = A.value\n"u8.ToArray()));

        var build =
            await ElmResolvedBuildPreparation.PrepareAsync(
                sources,
                ["elm.json"],
                [["src", "Main.elm"]],
                ElmPackageSubstitutions.DefaultBuild.Value,
                provider);

        build.CompilerModuleNames["src/Main.elm"].Should().Be("Main");
        build.CompilerModuleNames["elm-packages/author/a/src/Main.elm"].Should().NotBe("Main");

        var compilation =
            ElmCompiler.CompileResolvedEnvironment(
                build,
                rootDeclarations: [DeclQualifiedName.Create(["Main"], "value")]);

        var environment =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(
                ElmSourceCompilation.EvaluateZeroParameterRoots(
                    compilation.Extract(error => throw new InvalidOperationException(error)).compiledEnvValue,
                    Pine.Core.Interpreter.DirectInterpreter.WithLocalEvalCache(new PineVMParseCache()))
                .Extract(error => throw new InvalidOperationException(error)))
            .Extract(error => throw new InvalidOperationException(error));

        environment.Modules.Single(module => module.moduleName == "Main")
            .moduleContent.FunctionDeclarations["value"].Should().Be(IntegerEncoding.EncodeSignedInteger(71));

        build.ProjectSources.Should().Be(sources);
        build.ProjectSourceFingerprint.Should().NotBeNullOrEmpty();
    }

    [Fact]
    public async Task Regex_substitution_compiles_without_upstream_or_host_regex_dependencies()
    {
        var provider = new Provider { RejectRequests = true };

        var sources =
            FileTree.MergeFiles(
                Tree(Application(Deps(("elm/core", "1.0.5"), ("elm/regex", "1.0.0")))),
                Source(
                    "Main",
                    """
                    import Regex
                    value =
                        Regex.fromString "a+"
                            |> Maybe.map (\regex -> Regex.contains regex "baaa")
                            |> Maybe.withDefault False
                    """));

        var build =
            await ElmResolvedBuildPreparation.PrepareAsync(
                sources,
                ["elm.json"],
                [["src", "Main.elm"]],
                ElmPackageSubstitutions.DefaultBuild.Value,
                provider);

        build.Resolution.Packages["elm/regex"].SubstitutionImplementationId.Should().Be("pine-bundled:elm/regex");
        provider.VersionQueries.Should().BeEmpty();
        provider.MetadataQueries.Should().BeEmpty();
        provider.SourceQueries.Should().BeEmpty();

        var compiled =
            ElmCompiler.CompileResolvedEnvironment(
                build,
                rootDeclarations: [DeclQualifiedName.Create(["Main"], "value")])
            .Extract(error => throw new InvalidOperationException(error));

        var environment =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(
                ElmSourceCompilation.EvaluateZeroParameterRoots(
                    compiled.compiledEnvValue,
                    Pine.Core.Interpreter.DirectInterpreter.WithLocalEvalCache(new PineVMParseCache()))
                .Extract(error => throw new InvalidOperationException(error)))
            .Extract(error => throw new InvalidOperationException(error));

        environment.Modules.Single(module => module.moduleName == "Main")
            .moduleContent.FunctionDeclarations["value"]
            .Should().Be(ElmValueEncoding.ElmValueAsPineValue(ElmValue.TrueValue));
    }

    private static Task<ElmDependencyResolutionReport> Resolve(
        string manifest, Provider provider, ElmDependencyResolutionConfiguration? configuration = null) =>
        ElmDependencyResolver.ResolveAsync(Tree(manifest), ["elm.json"], configuration ?? new(), provider);

    private static Syntax.File PreparedSource(ElmResolvedBuild build, string[] path) =>
        ElmSyntaxParser.ParseModuleText(
            Encoding.UTF8.GetString(((FileTree.FileNode)build.Sources.GetNodeAtPath(path)!).Bytes.Span))
        .Extract(error => throw new InvalidOperationException(error.ToString()));

    private static ImmutableDictionary<string, string> Deps(params (string name, string requirement)[] dependencies) =>
        dependencies.ToImmutableDictionary(item => item.name, item => item.requirement);

    private static string Application(
        ImmutableDictionary<string, string> direct,
        ImmutableDictionary<string, string>? indirect = null,
        string elmVersion = "0.19.1",
        string[]? sourceDirectories = null) =>
        JsonSerializer.Serialize(
            new Dictionary<string, object>
            {
                ["type"] = "application",
                ["source-directories"] = sourceDirectories ?? ["src"],
                ["elm-version"] = elmVersion,
                ["dependencies"] = new { direct, indirect = indirect ?? Deps() },
                ["test-dependencies"] = new { direct = Deps(), indirect = Deps() },
            });

    private static string Package(
        ImmutableDictionary<string, string> dependencies,
        ImmutableDictionary<string, string>? testDependencies = null,
        string name = "root/project",
        string version = "1.0.0",
        string elmVersion = "0.19.0 <= v < 0.20.0",
        string[]? exposedModules = null) =>
        JsonSerializer.Serialize(
            new Dictionary<string, object>
            {
                ["type"] = "package",
                ["name"] = name,
                ["version"] = version,
                ["elm-version"] = elmVersion,
                ["dependencies"] = dependencies,
                ["test-dependencies"] = testDependencies ?? Deps(),
                ["exposed-modules"] = exposedModules ?? [],
            });

    private static FileTree Tree(string manifest) =>
        FileTree.FromSetOfFilesWithStringPath(
            [(new[] { "elm.json" }, (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(manifest))]);

    private static FileTree Source(string name, string declarations) =>
        FileTree.FromSetOfFilesWithStringPath(
            [
            (new[] { "src", name + ".elm" }, (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes($"module {name} exposing (..)\n{declarations}\n"))
            ]);

    private sealed class Provider : IElmPackageProvider
    {
        private readonly Dictionary<ElmPackageIdentity, (ElmPackageMetadata metadata, FileTree sources)> _packages = [];

        public bool Complete { get; init; } = true;

        public bool RejectRequests { get; init; }

        public string OriginPrefix { get; init; } = "registry:";

        public FileTree? SourceOverride { get; set; }

        public List<string> VersionQueries { get; } = [];

        public List<ElmPackageIdentity> MetadataQueries { get; } = [];

        public List<ElmPackageIdentity> SourceQueries { get; } = [];

        public Provider Add(
            string name,
            string version,
            ImmutableDictionary<string, string>? dependencies = null,
            ImmutableDictionary<string, string>? testDependencies = null,
            string elmVersion = "0.19.0 <= v < 0.20.0",
            FileTree? sources = null,
            string[]? exposedModules = null)
        {
            var identity = new ElmPackageIdentity(name, ElmPackageVersion.Parse(version));

            var tree =
                Tree(Package(dependencies ?? Deps(), testDependencies, name, version, elmVersion, exposedModules));

            if (sources is not null)
                tree = FileTree.MergeFiles(tree, sources);

            _packages.Add(
                identity,
                (new(identity, ElmDependencyResolver.ReadManifest(tree, ["elm.json"]), OriginPrefix + identity), tree));

            return this;
        }

        public Task<ElmPackageVersionListing> GetVersionsAsync(string packageName, CancellationToken cancellationToken)
        {
            if (RejectRequests)
                throw new InvalidOperationException("Unexpected upstream version query: " + packageName);

            VersionQueries.Add(packageName);

            return
                Task.FromResult(
                    new ElmPackageVersionListing(
                        [
                        .. _packages.Keys.Where(identity => identity.Name == packageName).Select(
                            identity => identity.Version)
                        ],
                        "test-registry",
                        Complete));
        }

        public Task<ElmPackageMetadata> GetMetadataAsync(
            ElmPackageIdentity identity,
            CancellationToken cancellationToken)
        {
            if (RejectRequests)
                throw new InvalidOperationException("Unexpected upstream metadata query: " + identity);

            MetadataQueries.Add(identity);
            return Task.FromResult(_packages[identity].metadata);
        }

        public Task<FileTree> GetSourcesAsync(ElmPackageIdentity identity, CancellationToken cancellationToken)
        {
            if (RejectRequests)
                throw new InvalidOperationException("Unexpected upstream source query: " + identity);

            SourceQueries.Add(identity);
            return Task.FromResult(SourceOverride ?? _packages[identity].sources);
        }
    }
}
