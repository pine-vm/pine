using AwesomeAssertions;
using Pine.CLI;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using Pine.Core.Functional;
using Pine.Core.Runtime.IntermediateVM;
using Pine.Core.Testing;
using Pine.Elm019;
using Pine.Elm019.Testing;
using System;
using System.Collections.Generic;
using System.CommandLine;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests.Elm;

public class ElmMakeEcmascriptTests
{
    private static string CorpusDirectory => TestResultSummary.FindTestDataDirectory("ElmMakeEcmascript");

    private static ElmMakeScenario StaticHtmlTextEmptyScenario =>
        DevelopmentScenario();

    private static ElmMakeScenario DevelopmentScenario()
    {
        var scenario =
            ElmMakeEcmascriptScenarios.SelectScenario(
                ElmMakeEcmascriptScenarios.Discover(CorpusDirectory),
                "static-html-text-empty");

        return scenario with { Expectations = [.. scenario.Expectations.Where(item => item.Flavor == ElmMakeFlavor.Development)] };
    }

    [Theory]
    [InlineData("simple/expect/0-19-1/src/Main.elm.js", ElmMakeFlavor.Development, ElmMakeOutputType.JavaScript, false)]
    [InlineData("simple/expect/0-19-1/src/Main.elm.html", ElmMakeFlavor.Development, ElmMakeOutputType.Html, false)]
    [InlineData("simple/expect/0-19-1/src/Main.elm.debug.js", ElmMakeFlavor.Debug, ElmMakeOutputType.JavaScript, false)]
    [InlineData("simple/expect/0-19-1/src/Main.elm.optimize.html", ElmMakeFlavor.Optimize, ElmMakeOutputType.Html, false)]
    [InlineData("simple/skip/expect/0-19-1/nested/src/Main.elm.debug.html", ElmMakeFlavor.Debug, ElmMakeOutputType.Html, true)]
    [InlineData("simple\\expect\\0-19-1\\src\\Main.elm.optimize.js", ElmMakeFlavor.Optimize, ElmMakeOutputType.JavaScript, false)]
    public void ElmMakeEcmascript_expectation_paths_are_typed_and_round_trip(
        string path, ElmMakeFlavor flavor, ElmMakeOutputType output, bool skipped)
    {
        var expectation = ElmMakeEcmascriptScenarios.ParseExpectation(path);
        expectation.CompilerVersion.Should().Be(new ElmPackageVersion(0, 19, 1));
        expectation.Flavor.Should().Be(flavor);
        expectation.OutputType.Should().Be(output);
        expectation.Skipped.Should().Be(skipped);
        expectation.RelativePath.Should().Be(path.Replace('\\', '/'));
        expectation.SourcePath.Should().EndWith("src/Main.elm");
    }

    [Theory]
    [InlineData("simple/expect/0-19-1/src/Main.elm.prod.js")]
    [InlineData("simple/expect/0-19-1/src/Main.elm.debug.optimize.js")]
    [InlineData("simple/expect/0-19-1/src/Main.js")]
    [InlineData("simple/expect/0-19-1/../Main.elm.js")]
    [InlineData("/simple/expect/0-19-1/Main.elm.js")]
    [InlineData("C:\\simple\\expect\\0-19-1\\Main.elm.js")]
    [InlineData("simple/Main.elm.js")]
    [InlineData("simple")]
    [InlineData("simple/expect/src/Main.elm.js")]
    [InlineData("simple/skip/expect/src/Main.elm.js")]
    [InlineData("simple/expect/0.19.1/src/Main.elm.js")]
    [InlineData("simple/expect/00-19-1/src/Main.elm.js")]
    [InlineData("simple/expect/0-19/src/Main.elm.js")]
    [InlineData("simple/expect/0-19-1-2/src/Main.elm.js")]
    [InlineData("simple/expect/2147483648-19-1/src/Main.elm.js")]
    public void ElmMakeEcmascript_rejects_malformed_expectation_paths(string path)
    {
        Action parse = () => ElmMakeEcmascriptScenarios.ParseExpectation(path);
        parse.Should().Throw<ArgumentException>();
    }

    [Theory]
    [InlineData("example/expect/0-19-2/nested/src/Main.elm.js", 0, 19, 2, false)]
    [InlineData("example\\skip\\expect\\1-0-0\\nested\\src\\Main.elm.debug.html", 1, 0, 0, true)]
    public void ElmMakeEcmascript_expectation_paths_preserve_other_compiler_versions(
        string path, int major, int minor, int patch, bool skipped)
    {
        var expectation = ElmMakeEcmascriptScenarios.ParseExpectation(path);
        expectation.CompilerVersion.Should().Be(new ElmPackageVersion(major, minor, patch));
        expectation.SourcePath.Should().Be("nested/src/Main.elm");
        expectation.Skipped.Should().Be(skipped);
        expectation.RelativePath.Should().Be(path.Replace('\\', '/'));
    }

    [Fact]
    public async Task ElmMakeEcmascript_unimplemented_compiler_versions_fail_before_compiling_or_generating()
    {
        using var temporary = new TemporaryCorpus();
        var expectation = ElmMakeEcmascriptScenarios.ParseExpectation("example/expect/0-19-2/src/Main.elm.js");
        var scenario = new ElmMakeScenario("example", FileTree.EmptyTree, [expectation]);
        var actual = () => ElmMakeScenarioCompiler.CompileOutcomeAsync(temporary.Path, scenario, expectation);
        await actual.Should().ThrowAsync<NotSupportedException>().WithMessage("*0.19.2*");
        var reference = new ElmMakeReferenceCompiler(temporary.Path);
        var compileReference = () => reference.CompileOutcomeAsync(scenario, expectation);
        await compileReference.Should().ThrowAsync<NotSupportedException>().WithMessage("*0.19.2*");
        var generate = () => reference.GenerateAsync(scenario, expectation, TextWriter.Null);
        await generate.Should().ThrowAsync<NotSupportedException>().WithMessage("*0.19.2*");
        Directory.EnumerateFileSystemEntries(temporary.Path).Should().BeEmpty();
    }

    [Fact]
    public void ElmMakeEcmascript_registry_matches_Data_Binary_and_round_trips_extended_versions()
    {
        Elm019Registry.Write([new("elm/core", new(1, 0, 5))]).Should().Equal(
            Convert.FromHexString("0000000000000001000000000000000103656C6D04636F72650100050000000000000000"));

        ElmPackageIdentity[] packages =
            [
                new("a-b/core", new(1, 0, 0)),
                new("a/z", new(256, 1, 65535)),
                new("a/z", new(255, 256, 0)),
                new("a/z", new(1, 0, 0)),
                new("elm/core", new(1, 0, 5)),
                new("elm/core", new(1, 0, 0)),
            ];

        var bytes = Elm019Registry.Write(packages);
        var parsed = Elm019Registry.Read(bytes);
        parsed.Should().BeEquivalentTo(packages);
        parsed[0].Name.Should().Be("a/z", "Haskell orders the author and project separately");
        Elm019Registry.Write(parsed.Reverse().Concat(parsed)).Should().Equal(bytes);

        Action truncated = () => Elm019Registry.Read(bytes[..^1]);
        truncated.Should().Throw<InvalidDataException>();
        Action trailing = () => Elm019Registry.Read([.. bytes, 0]);
        trailing.Should().Throw<InvalidDataException>();
        var invalidCount = bytes.ToArray();
        invalidCount[7]++;
        Action incorrect = () => Elm019Registry.Read(invalidCount);
        incorrect.Should().Throw<InvalidDataException>();
    }

    [Fact]
    public async Task ElmMakeEcmascript_corpus_has_complete_offline_package_and_registry_snapshots()
    {
        var scenarios = ElmMakeEcmascriptScenarios.Discover(CorpusDirectory);
        var scenario = ElmMakeEcmascriptScenarios.SelectScenario(scenarios, "static-html-text-empty");

        var development =
            scenario.Expectations.Where(item => item.Flavor == ElmMakeFlavor.Development && !item.Skipped).ToArray();

        development.Should().NotBeEmpty();

        var snapshots = new ElmMakePackageSnapshots(CorpusDirectory);
        var required = await snapshots.RequiredPackagesAsync([scenario]);

        required.Select(item => item.ToString()).Should().BeEquivalentTo(
            "elm/core@1.0.5",
            "elm/html@1.0.0",
            "elm/json@1.1.3",
            "elm/virtual-dom@1.0.3");

        var allRequired = await snapshots.RequiredPackagesAsync(scenarios);
        allRequired.Except(snapshots.Inventory()).Should().BeEmpty();
        allRequired.Except(snapshots.ReadRegistry()).Should().BeEmpty();
        var inputs = await snapshots.PrepareAsync(scenario, scenario.Expectations[0]);

        inputs.Packages.Single(item => item.Key.Name == "elm/virtual-dom").Value
            .GetNodeAtPath(["src", "Elm", "Kernel", "VirtualDom.js"]).Should().BeOfType<FileTree.FileNode>();
    }

    [Fact]
    public async Task ElmMakeEcmascript_compiler_source_typechecks_with_classic_Elm()
    {
        var app = StaticHtmlTextEmptyScenario.AppFiles;
        var manifestFile = (FileTree.FileNode)app.GetNodeAtPath(["elm.json"])!;
        var manifest = System.Text.Json.Nodes.JsonNode.Parse(manifestFile.Bytes.Span)!;

        manifest["dependencies"]!["direct"]!["elm/json"] =
            manifest["dependencies"]!["indirect"]!["elm/json"]!.DeepClone();

        manifest["dependencies"]!["indirect"]!.AsObject().Remove("elm/json");
        app = app.SetNodeAtPathSorted(["elm.json"], FileTree.File(Encoding.UTF8.GetBytes(manifest.ToJsonString())));
        var sources = BundledFiles.CompilerSourceContainerFilesDefault.Value;

        foreach (var (path, content) in sources.EnumerateFilesTransitive().Where(
            file => file.path[^1].EndsWith(".elm", StringComparison.Ordinal) &&
                ((file.path[0] == "elm-make-ecmascript" && !file.path.Contains("tests")) ||
                file.path[0] == "pine-elm-syntax")))
        {
            var start = path.TakeWhile(segment => segment != "src").Count() + 1;
            app = app.SetNodeAtPathSorted(["src", .. path.Skip(start)], FileTree.File(content));
        }

        app =
            app.SetNodeAtPathSorted(
                ["src", "Main.elm"],
                FileTree.File(
                    Encoding.UTF8.GetBytes(
                        """
                        module Main exposing (main)
                        import ElmMake
                        import ElmMake.Model as Model
                        import Html
                        main =
                            Html.text
                                (case ElmMake.make
                                    { files = [], manifestPath = [], entryPoint = []
                                    , packages = [], outputType = Model.JavaScript, flavor = Model.Development
                                    }
                                 of
                                    Ok output -> output
                                    Err _ -> ""
                                )
                        """)));

        var scenario = StaticHtmlTextEmptyScenario with { AppFiles = app };
        var expectation = scenario.Expectations.First();
        var output = await new ElmMakeReferenceCompiler(CorpusDirectory).CompileAsync(scenario, expectation);
        output.Should().Contain("$author$project$ElmMake$make");
    }

    [Theory]
    [InlineData(ElmMakeExecutionPath.PineVM)]
    //[InlineData(ElmMakeExecutionPath.SyntaxInterpreter)]
    public async Task ElmMakeEcmascript_Run_all_snapshots(ElmMakeExecutionPath executionPath)
    {
        Console.WriteLine($"Backend: Elm-based ECMAScript compiler ({executionPath}), not the reference compiler.");
        var scenarios = ElmMakeEcmascriptScenarios.Discover(CorpusDirectory);

        var compilationCache =
            executionPath is ElmMakeExecutionPath.PineVM
            ?
            new ElmMakeCompilationCache()
            :
            null;

        Console.WriteLine(
            "Explicitly skipped expectations: " +
            scenarios.Sum(scenario => scenario.Expectations.Count(expectation => expectation.Skipped)));

        var results =
            await ElmMakeEcmascriptScenarios.RunAsync(
                CorpusDirectory,
                scenarios,
                (scenario, expectation, token) => ElmMakeScenarioCompiler.CompileOutcomeAsync(
                    CorpusDirectory,
                    scenario,
                    expectation,
                    token,
                    executionPath,
                    compilationCache));

        var report = ElmMakeEcmascriptScenarios.RenderResults(results);
        Console.WriteLine(report);
        results.Should().OnlyContain(result => result.Passed, report);
    }

    [Fact]
    public async Task ElmMakeEcmascript_PineVM_caches_reuse_work_across_targets_and_entry_points()
    {
        var scenario = StaticHtmlTextEmptyScenario;

        scenario =
            scenario with
            {
                AppFiles =
                scenario.AppFiles.SetNodeAtPathSorted(
                    ["src", "Other.elm"],
                    FileTree.File(
                        Encoding.UTF8.GetBytes(
                            "module Other exposing (main)\nimport Html\nmain = Html.text \"another entry point\"\n"))),
            };

        var first = scenario.Expectations.First();
        var second = first with { SourcePath = "src/Other.elm", OutputType = ElmMakeOutputType.JavaScript };
        var statistics = new InvocationCacheStatistics();
        var shared = new ElmMakeCompilationCache(statistics.Observe(new ConcurrentInvocationCache()));
        await ElmMakeScenarioCompiler.CompileAsync(CorpusDirectory, scenario, first, compilationCache: shared);
        var warmStatistics = statistics.GetSnapshot().Totals;
        var warmCompilations = shared.VmCaches.ExpressionCompilations.Count;
        warmCompilations.Should().BeGreaterThan(0);

        var output =
            await ElmMakeScenarioCompiler.CompileAsync(CorpusDirectory, scenario, second, compilationCache: shared);

        output.Should().Contain("$author$project$Other$main").And.Contain("another entry point");
        var reusedStatistics = statistics.GetSnapshot().Totals;
        reusedStatistics.HitCount.Should().BeGreaterThan(warmStatistics.HitCount);

        var coldStatistics = new InvocationCacheStatistics();
        var cold = new ElmMakeCompilationCache(coldStatistics.Observe(new ConcurrentInvocationCache()));

        var coldOutput =
            await ElmMakeScenarioCompiler.CompileAsync(CorpusDirectory, scenario, second, compilationCache: cold);

        coldOutput.Should().Be(output);

        (reusedStatistics.MissCount - warmStatistics.MissCount).Should().BeLessThan(
            coldStatistics.GetSnapshot().Totals.MissCount);

        (shared.VmCaches.ExpressionCompilations.Count - warmCompilations).Should().BeLessThan(
            cold.VmCaches.ExpressionCompilations.Count);

    }

    [Fact]
    public void ElmMakeEcmascript_execution_paths_process_debug_input_and_return_the_same_structured_error()
    {
        var vm =
            ElmMakeCompiler.Compile(
                FileTree.EmptyTree,
                ["elm.json"],
                ["src", "Main.elm"],
                ElmMakeOutputType.JavaScript,
                ElmMakeFlavor.Debug,
                new Dictionary<ElmPackageIdentity, FileTree>());

        var interpreter =
            ElmMakeCompiler.Compile(
                FileTree.EmptyTree,
                ["elm.json"],
                ["src", "Main.elm"],
                ElmMakeOutputType.JavaScript,
                ElmMakeFlavor.Debug,
                new Dictionary<ElmPackageIdentity, FileTree>(),
                executionPath: ElmMakeExecutionPath.SyntaxInterpreter);

        var error = vm.IsErrOrNull().Should().BeOfType<ElmMakeError.Diagnostics>().Subject;
        error.Problems.Should().ContainSingle().Which.Kind.Code.Should().Be(ElmMakeProblemCode.NoManifest);
        interpreter.IsErrOrNull().Should().BeEquivalentTo(error);
    }

    [Fact]
    public async Task ElmMakeEcmascript_actual_output_changes_when_source_changes()
    {
        var original = StaticHtmlTextEmptyScenario;
        var expectation = original.Expectations.First();
        var before = await ElmMakeScenarioCompiler.CompileAsync(CorpusDirectory, original, expectation);

        var changed =
            original with
            {
                AppFiles =
                original.AppFiles.SetNodeAtPathSorted(
                    ["src", "Main.elm"],
                    FileTree.File(
                        Encoding.UTF8.GetBytes(
                            "module Main exposing (main)\nimport Html\nmain = Html.text \"compiled from changed source\"\n"))),
            };

        var after = await ElmMakeScenarioCompiler.CompileAsync(CorpusDirectory, changed, expectation);
        after.Should().NotBe(before);
        after.Should().Contain("$author$project$Main$main = $elm$html$Html$text('compiled from changed source')");
    }

    [Fact]
    public async Task ElmMakeEcmascript_unsupported_features_are_failed_expectations_unless_skipped()
    {
        var original = StaticHtmlTextEmptyScenario;

        var portSource =
            """
            port module Main exposing (main)
            import Html
            port send : String -> Cmd msg
            main = Html.text ""
            """;

        var scenario =
            original with
            {
                AppFiles =
                original.AppFiles.SetNodeAtPathSorted(
                    [
                    "src",
                    "Main.elm"
                    ],
                    FileTree.File(Encoding.UTF8.GetBytes(portSource))),
            };

        var results =
            await ElmMakeEcmascriptScenarios.RunAsync(
                CorpusDirectory,
                [scenario],
                (app, expectation, token) => ElmMakeScenarioCompiler.CompileOutcomeAsync(CorpusDirectory, app, expectation, token));

        results.Should().HaveCount(scenario.Expectations.Count(item => !item.Skipped)).And.OnlyContain(
            result => !result.Passed);

        results.Should().OnlyContain(
            result =>
            result.Error != null && result.Error.Contains("UnsupportedFeature") && result.Error.Contains("Ports"));

        var debug = original.Expectations.First() with { Flavor = ElmMakeFlavor.Debug };

        results =
            await ElmMakeEcmascriptScenarios.RunAsync(
                CorpusDirectory,
                [scenario with { Expectations = [debug, original.Expectations[0] with { Skipped = true }] }],
                (app, expectation, token) => ElmMakeScenarioCompiler.CompileOutcomeAsync(CorpusDirectory, app, expectation, token));

        results.Should().ContainSingle();
        results[0].Passed.Should().BeFalse();
        results[0].Error.Should().Contain("UnsupportedFeature").And.Contain("Ports");
    }

    [Theory]
    [InlineData("module Main exposing (main)\nimport Html\nmain = Html.text missing\n", "NAMING ERROR")]
    [InlineData("module Main exposing (main)\nimport Html\nmain =\n", "SYNTAX PROBLEM")]
    [InlineData("module Main exposing (main)\nimport Html\nmain = Html.text \"\"\nunused = missing\n", "NAMING ERROR")]
    [InlineData("module Main exposing (main)\nimport Html\nimport NotPresent\nmain = Html.text \"\"\n", "MODULE NOT FOUND")]
    [InlineData("module Main exposing (main)\nimport Html exposing (missing)\nmain = Html.text \"\"\n", "NAMING ERROR")]
    [InlineData("module Main exposing (main, missing)\nimport Html\nmain = Html.text \"\"\n", "NAMING ERROR")]
    [InlineData("module Main exposing (main)\nimport Html\nmain : Html msg\nmain = Html.text \"\"\n", "NAMING ERROR")]
    [InlineData("module Main exposing (main)\nimport Html\nmain = Html.text \"\"\nunused : Typo\nunused = 1\n", "NAMING ERROR")]
    public async Task ElmMakeEcmascript_actual_compilation_reports_source_errors(string text, string expectedKind)
    {
        var original = StaticHtmlTextEmptyScenario;

        var scenario =
            original with
            {
                AppFiles =
                original.AppFiles.SetNodeAtPathSorted(["src", "Main.elm"], FileTree.File(Encoding.UTF8.GetBytes(text))),
            };

        var compile =
            async () => await ElmMakeScenarioCompiler.CompileAsync(CorpusDirectory, scenario, scenario.Expectations[0]);

        await compile.Should().ThrowAsync<InvalidDataException>().WithMessage("*" + expectedKind + "*");
    }

    [Fact]
    public async Task ElmMakeEcmascript_actual_uses_governing_manifest_and_ignores_stale_artifacts()
    {
        var original = StaticHtmlTextEmptyScenario;

        var nestedFiles =
            original.AppFiles.EnumerateFilesTransitive().Select(
                file => ((IReadOnlyList<string>)["nested", .. file.path], file.fileContent));

        var app = FileTree.FromSetOfFilesWithStringPath(nestedFiles);

        app =
            app.SetNodeAtPathSorted(
                [
                "elm.json"
                ],
                FileTree.File(Encoding.UTF8.GetBytes("invalid unrelated manifest")));

        app =
            app.SetNodeAtPathSorted(
                [
                "nested",
                "elm-stuff",
                "0.19.1",
                "artifacts.dat"
                ],
                FileTree.File(new byte[] { 1, 2, 3 }));

        var scenario = original with { AppFiles = app };
        var expectation = original.Expectations.First() with { SourcePath = "nested/src/Main.elm" };
        var actual = await ElmMakeScenarioCompiler.CompileAsync(CorpusDirectory, scenario, expectation);

        var expected =
            await ElmMakeScenarioCompiler.CompileAsync(
                CorpusDirectory,
                original,
                original.Expectations.First());

        actual.Should().Be(expected);
    }

    [Fact]
    public async Task ElmMakeEcmascript_actual_compilation_needs_no_registry_or_expectation_files()
    {
        using var temporary = new TemporaryCorpus();
        temporary.CopyPackages();
        var scenario = StaticHtmlTextEmptyScenario;
        var expectation = scenario.Expectations.First();
        File.Exists(new ElmMakePackageSnapshots(temporary.Path).RegistryPath).Should().BeFalse();
        File.Exists(ElmMakeEcmascriptScenarios.ExpectationPath(temporary.Path, expectation)).Should().BeFalse();

        var actual = await ElmMakeScenarioCompiler.CompileAsync(temporary.Path, scenario, expectation);

        var expected =
            await File.ReadAllTextAsync(ElmMakeEcmascriptScenarios.ExpectationPath(CorpusDirectory, expectation));

        actual.ReplaceLineEndings("\n").Should().Be(expected.ReplaceLineEndings("\n"));
    }

    [Fact]
    public async Task ElmMakeEcmascript_missing_and_invalid_archives_fail_without_cache_fallback_or_refresh()
    {
        using var temporary = new TemporaryCorpus();
        var scenario = StaticHtmlTextEmptyScenario;
        var snapshots = new ElmMakePackageSnapshots(temporary.Path);
        var core = new ElmPackageIdentity("elm/core", new(1, 0, 5));

        var missing = async () => await snapshots.PrepareAsync(scenario, scenario.Expectations[0]);

        (await missing.Should().ThrowAsync<InvalidDataException>()).Which.Message
            .Should().Contain("elm/core@1.0.5").And.Contain(snapshots.ArchivePath(core));

        Directory.CreateDirectory(System.IO.Path.GetDirectoryName(snapshots.ArchivePath(core))!);
        byte[] corrupt = [1, 2, 3];
        await File.WriteAllBytesAsync(snapshots.ArchivePath(core), corrupt);

        var invalid =
            async () => await snapshots.PrepareAsync(scenario, scenario.Expectations[0], acquireMissing: true);

        (await invalid.Should().ThrowAsync<InvalidDataException>()).Which.Message
            .Should().Contain(snapshots.ArchivePath(core));

        File.ReadAllBytes(snapshots.ArchivePath(core)).Should().Equal(corrupt);
    }

    [Fact]
    public async Task ElmMakeEcmascript_includes_application_test_dependency_pins()
    {
        var original = StaticHtmlTextEmptyScenario;

        var manifest =
            """
            {
                "type": "application",
                "source-directories": ["src"],
                "elm-version": "0.19.1",
                "dependencies": {
                    "direct": { "elm/core": "1.0.5", "elm/json": "1.1.3" },
                    "indirect": {}
                },
                "test-dependencies": {
                    "direct": { "elm/html": "1.0.0" },
                    "indirect": { "elm/virtual-dom": "1.0.3" }
                }
            }
            """;

        var scenario =
            original with
            {
                AppFiles =
                original.AppFiles.SetNodeAtPathSorted(["elm.json"], FileTree.File(Encoding.UTF8.GetBytes(manifest))),
            };

        var inputs =
            await new ElmMakePackageSnapshots(CorpusDirectory).PrepareAsync(scenario, scenario.Expectations[0]);

        inputs.Packages.Keys.Should().HaveCount(4);
    }

    [Fact]
    public async Task ElmMakeEcmascript_maintenance_is_idempotent_and_retains_unused_packages_and_versions()
    {
        using var temporary = new TemporaryCorpus();
        temporary.CopyPackages();
        var snapshots = new ElmMakePackageSnapshots(temporary.Path);
        ElmMakeScenario[] scenarios = [StaticHtmlTextEmptyScenario];
        var extra = new ElmPackageIdentity("elm/core", new(1, 0, 0));
        Directory.CreateDirectory(System.IO.Path.GetDirectoryName(snapshots.RegistryPath)!);
        var original = Elm019Registry.Write(snapshots.Inventory().Append(extra));
        await File.WriteAllBytesAsync(snapshots.RegistryPath, original);

        var archiveBefore =
            snapshots.Inventory().ToDictionary(item => item, item => File.ReadAllBytes(snapshots.ArchivePath(item)));

        var output = new StringWriter();
        await snapshots.UpdateAsync(scenarios, output);
        await snapshots.UpdateAsync(scenarios, output);
        File.ReadAllBytes(snapshots.RegistryPath).Should().Equal(original);
        output.ToString().Should().Contain("acquired: 0");

        foreach (var (identity, bytes) in archiveBefore)
            File.ReadAllBytes(snapshots.ArchivePath(identity)).Should().Equal(bytes);

        snapshots.ReadRegistry().Should().Contain(extra);

        await snapshots.UpdateAsync(
            [scenarios.Single() with { Expectations = [scenarios.Single().Expectations[0] with { Skipped = true }] }],
            output);

        var smallerManifest =
            """
            {
                "type": "application",
                "source-directories": ["src"],
                "elm-version": "0.19.1",
                "dependencies": {
                    "direct": { "elm/core": "1.0.5", "elm/json": "1.1.3" },
                    "indirect": {}
                },
                "test-dependencies": { "direct": {}, "indirect": {} }
            }
            """;

        await snapshots.UpdateAsync(
            [
                scenarios.Single() with
                {
                    AppFiles =
                    scenarios.Single().AppFiles.SetNodeAtPathSorted(
                        ["elm.json"],
                        FileTree.File(Encoding.UTF8.GetBytes(smallerManifest))),
                }
            ],
            output);

        output.ToString().Should().Contain("Unused package snapshot (retained): elm/html@1.0.0");
        snapshots.Inventory().Should().HaveCount(archiveBefore.Count);
        File.ReadAllBytes(snapshots.RegistryPath).Should().Equal(original);
    }

    [Fact]
    public async Task ElmMakeEcmascript_generation_refuses_overwrite_before_acquiring_dependencies()
    {
        using var temporary = new TemporaryCorpus();
        var scenario = StaticHtmlTextEmptyScenario;
        var expectation = scenario.Expectations[0];
        var path = ElmMakeEcmascriptScenarios.ExpectationPath(temporary.Path, expectation);
        Directory.CreateDirectory(System.IO.Path.GetDirectoryName(path)!);
        await File.WriteAllTextAsync(path, "preserve");

        var generate =
            async () => await new ElmMakeReferenceCompiler(temporary.Path).GenerateAsync(scenario, expectation, TextWriter.Null);

        (await generate.Should().ThrowAsync<IOException>()).Which.Message.Should().Contain("remove it manually");
        File.ReadAllText(path).Should().Be("preserve");
        Directory.Exists(System.IO.Path.Combine(temporary.Path, "packages")).Should().BeFalse();
    }

    [Fact]
    public async Task ElmMakeEcmascript_runner_normalizes_only_line_endings()
    {
        using var temporary = new TemporaryCorpus();
        var expectation = ElmMakeEcmascriptScenarios.ParseExpectation("example/expect/0-19-1/src/Main.elm.js");
        var path = ElmMakeEcmascriptScenarios.ExpectationPath(temporary.Path, expectation);
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        await File.WriteAllTextAsync(path, "first\r\nsecond\rthird\n");
        ElmMakeScenario[] scenarios = [new("example", FileTree.EmptyTree, [expectation])];

        var identical =
            await ElmMakeEcmascriptScenarios.RunAsync(
                temporary.Path,
                scenarios,
                (_, _, _) => Task.FromResult<ElmMakeCompilation>(new ElmMakeCompilation.Output("first\nsecond\nthird\r\n")));

        identical.Single().Passed.Should().BeTrue();

        foreach (var different in new[]
        {
            "first\nsecond\nthird", "first \nsecond\nthird\n", "first\nSECOND\nthird\n",
            "first\nsecond\u0085third\n", "first\nsecond\u2028third\n", "first\nsecond\fthird\n",
        })
        {
            var results =
                await ElmMakeEcmascriptScenarios.RunAsync(
                    temporary.Path,
                    scenarios,
                    (_, _, _) => Task.FromResult<ElmMakeCompilation>(new ElmMakeCompilation.Output(different)));

            results.Single().Passed.Should().BeFalse(
                "normalization must preserve text, spaces, Unicode data and final newlines");
        }
    }

    [Theory]
    [InlineData("module Main exposing (main)\nimport Html\nmain = Html.text \"a quote: ' and a backslash: \\\\\"\n")]
    [InlineData("module Main exposing (main)\nimport Html\nmain : Html.Html msg\nmain = Html.text \"typed\"\n")]
    [InlineData("module Main exposing (main)\nimport Html exposing (text)\nmain = text \"exposed\"\n")]
    [InlineData("module Main exposing (main)\nimport Html\nmain = Html.text greeting\ngreeting = if 2 < 3 then \"yes\" else \"no\"\n")]
    [InlineData("module Main exposing (main)\nimport Html\nmain = Html.text (count 4)\ncount n = if n < 1 then \"done\" else count (n - 1)\n")]
    [InlineData("module Main exposing (main)\nimport Html\nmain = Html.text (choose [])\nchoose chars = case chars of\n    'x' :: _ -> \"x\"\n    _ -> \"other\"\n")]
    public async Task ElmMakeEcmascript_development_variations_match_classic_reference(string source)
    {
        var original = StaticHtmlTextEmptyScenario;

        var scenario =
            original with
            {
                AppFiles =
                original.AppFiles.SetNodeAtPathSorted(
                    [
                    "src",
                    "Main.elm"
                    ],
                    FileTree.File(Encoding.UTF8.GetBytes(source))),
            };

        var expectation = scenario.Expectations.First();
        var actual = await ElmMakeScenarioCompiler.CompileAsync(CorpusDirectory, scenario, expectation);
        var reference = await new ElmMakeReferenceCompiler(CorpusDirectory).CompileAsync(scenario, expectation);
        actual.ReplaceLineEndings("\n").Should().Be(reference.ReplaceLineEndings("\n"));
    }

    [Theory]
    [InlineData(
        "module Sandbox exposing (main)\nimport Browser\nimport Html\nmain = Browser.sandbox { init = (), update = \\_ model -> model, view = \\_ -> Html.text \"inferred\" }\n")]
    [InlineData(
        "module Sandbox exposing (main)\nimport Browser\nimport Html\ntype alias Flags = ()\ntype alias App flags = Program flags () ()\nmain : App Flags\nmain = Browser.element { init = \\() -> ((), Cmd.none), update = \\_ model -> (model, Cmd.none), subscriptions = \\_ -> Sub.none, view = \\_ -> Html.text \"aliases\" }\n")]
    [InlineData(
        "module Sandbox exposing (main)\nimport Platform\nmain : Program () () Never\nmain = Platform.worker { init = \\() -> ((), Cmd.none), update = \\message _ -> never message, subscriptions = \\_ -> Sub.none }\n")]
    public async Task ElmMakeEcmascript_program_main_variations_match_classic_reference(string source)
    {
        var original =
            ElmMakeEcmascriptScenarios.SelectScenario(
                ElmMakeEcmascriptScenarios.Discover(CorpusDirectory),
                "browser-programs");

        var scenario =
            original with
            {
                AppFiles =
                original.AppFiles.SetNodeAtPathSorted(
                    [
                    "src",
                    "Sandbox.elm"
                    ],
                    FileTree.File(Encoding.UTF8.GetBytes(source))),
            };

        var expectation =
            new ElmMakeExpectation(
                scenario.Name,
                "src/Sandbox.elm",
                new(0, 19, 1),
                ElmMakeOutputType.JavaScript,
                ElmMakeFlavor.Development,
                false);

        var actual = await ElmMakeScenarioCompiler.CompileAsync(CorpusDirectory, scenario, expectation);
        var reference = await new ElmMakeReferenceCompiler(CorpusDirectory).CompileAsync(scenario, expectation);
        actual.ReplaceLineEndings("\n").Should().Be(reference.ReplaceLineEndings("\n"));
    }

    [Fact]
    public async Task ElmMakeEcmascript_runner_reports_hunks_errors_and_grouped_statistics()
    {
        using var temporary = new TemporaryCorpus();
        var development = ElmMakeEcmascriptScenarios.ParseExpectation("example/expect/0-19-1/src/Main.elm.js");
        var debug = ElmMakeEcmascriptScenarios.ParseExpectation("example/expect/0-19-1/src/Main.elm.debug.html");
        var optimize = ElmMakeEcmascriptScenarios.ParseExpectation("example/expect/0-19-1/src/Main.elm.optimize.js");
        var skipped = ElmMakeEcmascriptScenarios.ParseExpectation("example/skip/expect/0-19-1/src/Main.elm.js");
        var path = ElmMakeEcmascriptScenarios.ExpectationPath(temporary.Path, development);
        Directory.CreateDirectory(System.IO.Path.GetDirectoryName(path)!);
        var expected = "first\n" + string.Concat(Enumerable.Repeat("same\n", 12)) + "last\n";
        var actual = expected.Replace("first", "FIRST").Replace("last", "LAST");
        await File.WriteAllTextAsync(path, expected);
        ElmMakeScenario[] scenarios = [new("example", FileTree.EmptyTree, [development, debug, optimize, skipped])];
        var calls = new List<ElmMakeExpectation>();

        Task<ElmMakeCompilation> Compile(
            ElmMakeScenario scenario,
            ElmMakeExpectation expectation,
            CancellationToken token)
        {
            calls.Add(expectation);

            return
                expectation.Flavor is ElmMakeFlavor.Development
                ?
                Task.FromResult<ElmMakeCompilation>(new ElmMakeCompilation.Output(actual))
                :
                throw new InvalidOperationException("test compiler error");
        }

        var results =
            await ElmMakeEcmascriptScenarios.RunAsync(temporary.Path, scenarios, Compile, maxDiffHunks: 1, skipOptimize: true);

        calls.Should().Equal(development, debug);
        results[0].Error.Should().Contain("2 differing hunk(s); showing 1.").And.Contain("FIRST").And.NotContain("LAST");
        results[1].Error.Should().Contain("test compiler error");
        var report = ElmMakeEcmascriptScenarios.RenderResults(results);
        report.Should().Contain(development.RelativePath).And.Contain(debug.RelativePath);

        report.Should().Contain("Development: total 1, succeeded 0, failed 1").And.Contain(
            "Html: total 1, succeeded 0, failed 1");

        report.Should().Contain("Compiler 0.19.1: total 2, succeeded 0, failed 2");

        var filtered =
            await ElmMakeEcmascriptScenarios.RunAsync(
                temporary.Path,
                scenarios,
                Compile,
                maxDiffHunks: 0,
                skipDebug: true,
                skipOptimize: true);

        filtered.Should().ContainSingle();
        filtered[0].Error.Should().Be("2 differing hunk(s); showing 0.\n");

        var allFiltered =
            await ElmMakeEcmascriptScenarios.RunAsync(
                temporary.Path,
                [scenarios[0] with { Expectations = [debug, optimize] }],
                (_, _, _) => throw new Exception("Filtered expectations must not compile."),
                skipDebug: true,
                skipOptimize: true);

        allFiltered.Should().BeEmpty();
    }

    [Theory]
    [InlineData("Main.elm.js")]
    [InlineData("Main.elm.html")]
    [InlineData("Main.elm.debug.js")]
    [InlineData("Main.elm.optimize.html")]
    public async Task ElmMakeEcmascript_discovery_is_scoped_to_the_scenarios_directory_and_accepts_single_expectations(
        string fileName)
    {
        using var temporary = new TemporaryCorpus();
        Directory.CreateDirectory(Path.Combine(temporary.Path, "packages"));
        Directory.CreateDirectory(Path.Combine(temporary.Path, "elm-reference"));
        Directory.CreateDirectory(Path.Combine(temporary.Path, "legacy", "input-app"));

        Action missingScenarios = () => ElmMakeEcmascriptScenarios.Discover(temporary.Path);

        missingScenarios.Should().Throw<DirectoryNotFoundException>()
            .WithMessage("*scenarios directory not found*scenarios");

        var scenarioDirectory = Path.Combine(temporary.Path, "scenarios", "packages");
        Directory.CreateDirectory(Path.Combine(scenarioDirectory, "input-app"));
        Directory.CreateDirectory(Path.Combine(scenarioDirectory, "expect", "0-19-1", "src"));
        var expectedPath = Path.Combine(scenarioDirectory, "expect", "0-19-1", "src", fileName);
        await File.WriteAllTextAsync(expectedPath, "snapshot");

        var scenario = ElmMakeEcmascriptScenarios.Discover(temporary.Path).Should().ContainSingle().Which;
        scenario.Name.Should().Be("packages", "shared resource names are not reserved within scenarios");
        var expectation = scenario.Expectations.Should().ContainSingle().Which;
        expectation.RelativePath.Should().Be("packages/expect/0-19-1/src/" + fileName);
        ElmMakeEcmascriptScenarios.ExpectationPath(temporary.Path, expectation).Should().Be(expectedPath);
        var calls = new List<ElmMakeExpectation>();

        var results =
            await ElmMakeEcmascriptScenarios.RunAsync(
                temporary.Path,
                [scenario],
                (_, request, _) =>
                {
                    calls.Add(request);
                    return Task.FromResult<ElmMakeCompilation>(new ElmMakeCompilation.Output("snapshot"));
                });

        calls.Should().Equal(expectation);
        results.Should().ContainSingle().Which.Passed.Should().BeTrue();
    }

    [Fact]
    public async Task ElmMakeEcmascript_discovery_and_runner_reject_empty_scenarios_and_preserve_skips()
    {
        using var temporary = new TemporaryCorpus();
        Directory.CreateDirectory(System.IO.Path.Combine(temporary.Path, "scenarios", "empty", "input-app"));
        Action discover = () => ElmMakeEcmascriptScenarios.Discover(temporary.Path);
        discover.Should().Throw<InvalidDataException>().WithMessage("*zero expectation*");
        ElmMakeEcmascriptScenarios.Discover(temporary.Path, allowEmptyExpectations: true).Should().ContainSingle();

        var skipped =
            System.IO.Path.Combine(
                temporary.Path,
                "scenarios",
                "empty",
                "skip",
                "expect",
                "0-19-1",
                "src",
                "Main.elm.js");

        Directory.CreateDirectory(System.IO.Path.GetDirectoryName(skipped)!);
        await File.WriteAllTextAsync(skipped, "");
        var scenarios = ElmMakeEcmascriptScenarios.Discover(temporary.Path);
        scenarios.Single().Expectations.Single().Skipped.Should().BeTrue();

        var skippedResults =
            await ElmMakeEcmascriptScenarios.RunAsync(
                temporary.Path,
                scenarios,
                (_, _, _) => throw new Exception("must not compile"));

        skippedResults.Should().BeEmpty();
        Action select = () => ElmMakeEcmascriptScenarios.SelectScenario(scenarios, "empy");
        select.Should().Throw<ArgumentException>().WithMessage("*Existing scenarios: empty. Closest name: empty*");
    }

    [Theory]
    [InlineData("stats")]
    [InlineData("update-packages")]
    [InlineData("generate", "static-html-text-empty/expect/0-19-1/src/Main.elm.js")]
    [InlineData("run", "static-html-text-empty", "--reference", "--max-diff-hunks", "0", "--skip-expect-debug", "--skip-expect-optimize")]
    public void ElmMakeEcmascript_CLI_registers_all_maintenance_actions(params string[] arguments)
    {
        var root = new RootCommand { ElmCommand.Create() };
        var result = root.Parse(["elm", "maintain", "make-ecmascript", "--test-data", CorpusDirectory, .. arguments]);
        result.Errors.Should().BeEmpty();
    }

    private sealed class TemporaryCorpus : IDisposable
    {
        public string Path { get; } =
            System.IO.Path.Combine(
                System.IO.Path.GetTempPath(),
                "pine-ecmascript-test-" + Guid.NewGuid().ToString("N"));

        public TemporaryCorpus() => Directory.CreateDirectory(Path);

        public void CopyPackages()
        {
            foreach (var file in Directory.EnumerateFiles(
                System.IO.Path.Combine(CorpusDirectory, "packages"),
                "*.zip",
                SearchOption.AllDirectories))
            {
                var destination = System.IO.Path.Combine(Path, System.IO.Path.GetRelativePath(CorpusDirectory, file));
                Directory.CreateDirectory(System.IO.Path.GetDirectoryName(destination)!);
                File.Copy(file, destination);
            }
        }

        public void Dispose() => Directory.Delete(Path, recursive: true);
    }
}
