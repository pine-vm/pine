using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Files;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

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
                rootDeclarationsAsPlainValues: [DeclQualifiedName.Create(["Main"], "value")]);

        var environment =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(
                compilation.Extract(error => throw new InvalidOperationException(error)).compiledEnvValue)
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
                rootDeclarationsAsPlainValues: [DeclQualifiedName.Create(["Main"], "value")]);

        var environment =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(
                compilation.Extract(error => throw new InvalidOperationException(error)).compiledEnvValue)
            .Extract(error => throw new InvalidOperationException(error));

        environment.Modules.Single(module => module.moduleName == "Main")
            .moduleContent.FunctionDeclarations["value"].Should().Be(IntegerEncoding.EncodeSignedInteger(71));

        build.ProjectSources.Should().Be(sources);
        build.ProjectSourceFingerprint.Should().NotBeNullOrEmpty();
    }

    private static Task<ElmDependencyResolutionReport> Resolve(
        string manifest, Provider provider, ElmDependencyResolutionConfiguration? configuration = null) =>
        ElmDependencyResolver.ResolveAsync(Tree(manifest), ["elm.json"], configuration ?? new(), provider);

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
