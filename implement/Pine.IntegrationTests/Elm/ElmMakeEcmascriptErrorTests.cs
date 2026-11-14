using AwesomeAssertions;
using Pine.Core.Elm;
using Pine.Core.Files;
using Pine.Core.Functional;
using Pine.Core.Testing;
using Pine.Elm019;
using Pine.Elm019.Testing;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests.Elm;

public class ElmMakeEcmascriptErrorTests
{
    private static string CorpusDirectory => TestResultSummary.FindTestDataDirectory("ElmMakeEcmascript");

    private static ElmMakeScenario ValidScenario =>
        ElmMakeEcmascriptScenarios.SelectScenario(
            ElmMakeEcmascriptScenarios.Discover(CorpusDirectory),
            "static-html-text-empty");

    [Theory]
    [InlineData("example/expect/0-19-1/src/Main.elm.js.errors-omitting-details.txt", ElmMakeFlavor.Development, ElmMakeOutputType.JavaScript, false)]
    [InlineData("example/expect/0-19-1/src/Main.elm.debug.html.errors-omitting-details.txt", ElmMakeFlavor.Debug, ElmMakeOutputType.Html, false)]
    [InlineData("example\\skip\\expect\\0-19-1\\nested\\Main.elm.optimize.js.errors-omitting-details.txt", ElmMakeFlavor.Optimize, ElmMakeOutputType.JavaScript, true)]
    public void ElmMakeEcmascript_error_paths_round_trip(
        string path,
        ElmMakeFlavor flavor,
        ElmMakeOutputType output,
        bool skipped)
    {
        var expectation = ElmMakeEcmascriptScenarios.ParseExpectation(path);
        expectation.CompilerVersion.Should().Be(new ElmPackageVersion(0, 19, 1));
        expectation.Kind.Should().Be(ElmMakeExpectationKind.ErrorsOmittingDetails);
        expectation.Flavor.Should().Be(flavor);
        expectation.OutputType.Should().Be(output);
        expectation.Skipped.Should().Be(skipped);
        expectation.RelativePath.Should().Be(path.Replace('\\', '/'));
    }

    [Theory]
    [InlineData("example/expect/0-19-1/src/Main.elm.errors-omitting-details.txt")]
    [InlineData("example/expect/0-19-1/src/Main.elm.js.errors.txt")]
    [InlineData("example/expect/0-19-1/src/Main.elm.js.errors-omitting-details.json")]
    [InlineData("example/expect/0-19-1/src/Main.elm.js.errors-omitting-details.txt.extra")]
    public void ElmMakeEcmascript_error_paths_reject_ambiguous_suffixes(string path)
    {
        Action parse = () => ElmMakeEcmascriptScenarios.ParseExpectation(path);
        parse.Should().Throw<ArgumentException>();
    }

    [Fact]
    public void ElmMakeEcmascript_error_text_orders_all_locations_numerically_and_preserves_duplicates_and_escapes()
    {
        ElmMakeSnapshotProblem[] problems =
            [
                new("src/Z.elm", new(new(2, 1), new(3, 5)), "LAST"),
                new("src/A.elm", new(new(10, 2), new(10, 4)), "TENTH"),
                new("src/A.elm", new(new(2, 2), new(2, 9)), "B"),
                new("src/A.elm", new(new(2, 2), new(2, 8)), "A"),
                new("src/A.elm", new(new(2, 2), new(2, 8)), "A"),
                new("src/A.elm", new(new(2, 2), new(2, 8)), "B"),
                new("src/A.elm", null, "NO REGION"),
                new(null, null, "A\\B\tC\rD\nE"),
            ];

        var text = ElmMakeErrorSnapshots.Render(problems);
        text.Should().StartWith("File: <none>\nRegion: <none>\nTitle: A\\\\B\\tC\\rD\\nE\n\n");

        text.IndexOf("Title: A\n", StringComparison.Ordinal).Should().BeLessThan(
            text.IndexOf("Title: TENTH", StringComparison.Ordinal));

        text.Should().Contain("Region: 2:1-3:5");
        ElmMakeErrorSnapshots.Parse(text.Replace("\n", "\r\n", StringComparison.Ordinal)).Should().HaveCount(8);
        ElmMakeErrorSnapshots.Render(ElmMakeErrorSnapshots.Parse(text)).Should().Be(text);
        ElmMakeErrorSnapshots.Render(problems.Reverse()).Should().Be(text);
    }

    [Theory]
    [InlineData("")]
    [InlineData("File: src/A.elm\nRegion: 0:1-1:2\nTitle: ERROR\n")]
    [InlineData("File: src/A.elm\nRegion: 1:9-1:2\nTitle: ERROR\n")]
    [InlineData("File: <none>\nRegion: 1:1-1:2\nTitle: ERROR\n")]
    [InlineData("File: ../A.elm\nRegion: <none>\nTitle: ERROR\n")]
    [InlineData("File: src/A.elm\nRegion: <none>\nTitle: ERROR")]
    [InlineData("File: src/A.elm\nRegion: <none>\nTitle: ERROR\n\n")]
    [InlineData("File: src/A.elm\nRegion: <none>\nTitle: \\q\n")]
    [InlineData("\uFEFFFile: src/A.elm\nRegion: <none>\nTitle: ERROR\n")]
    [InlineData("File: src/B.elm\nRegion: <none>\nTitle: ERROR\n\nFile: src/A.elm\nRegion: <none>\nTitle: ERROR\n")]
    public void ElmMakeEcmascript_error_text_rejects_noncanonical_snapshots(string text)
    {
        Action parse = () => ElmMakeErrorSnapshots.Parse(text);
        parse.Should().Throw<InvalidDataException>();
    }

    [Fact]
    public void ElmMakeEcmascript_reference_projection_maps_known_roots_and_keeps_all_problem_records()
    {
        using var corpus = new TemporaryCorpus();
        var app = Path.Combine(corpus.Path, "application");
        var working = Path.Combine(app, "nested");
        var package = Path.Combine(corpus.Path, "home", "core");
        var roots = new Dictionary<ElmPackageIdentity, string> { [new("elm/core", new(1, 0, 5))] = package };

        var report =
            JsonSerializer.Serialize(
                new
                {
                    type = "compile-errors",
                    errors =
                    new[]
                    {
                        new
                        {
                            path = Path.Combine(app, "nested", "src", "Main.elm"),
                            name = "Main",
                            problems =
                            new[]
                            {
                                Problem("SECOND", 10), Problem("FIRST", 2), Problem("FIRST", 2),
                            }
                        },
                        new { path = Path.Combine(package, "src", "Basics.elm"), name = "Basics", problems = new[] { Problem("PACKAGE", 4) }},
                    },
                });

        var projected = ElmMakeErrorSnapshots.ReadReferenceReport(report, app, working, roots);
        projected.Should().HaveCount(4);
        var text = ElmMakeErrorSnapshots.Render(projected);
        text.Should().Contain("File: nested/src/Main.elm\nRegion: 2:3-2:9\nTitle: FIRST");
        text.Should().Contain("File: package:elm/core@1.0.5/src/Basics.elm");
        text.Should().NotContain("details").And.NotContain(corpus.Path);
        ElmMakeErrorSnapshots.NormalizeReferencePath("elm.json", app, working, roots).Should().Be("nested/elm.json");

        Action outside =
            () => ElmMakeErrorSnapshots.NormalizeReferencePath(
                Path.Combine(corpus.Path, "application-other", "Main.elm"),
                app,
                working,
                roots);

        outside.Should().Throw<InvalidDataException>();

        static object Problem(string title, int line) =>
            new
            {
                title,
                region = new { start = new { line, column = 3 }, end = new { line, column = 9 } },
                message =
                new object[] { "details", new { bold = true, underline = false, color = "RED", @string = "diagram" } },
            };
    }

    [Theory]
    [InlineData("{\"type\":\"compile-errors\",\"errors\":[]}")]
    [InlineData("{\"type\":\"error\",\"path\":null,\"title\":\"CORRUPT CACHE\",\"message\":[]}")]
    [InlineData("{\"type\":\"error\",\"path\":null,\"title\":\"PROBLEM DOWNLOADING PACKAGE\",\"message\":[]}")]
    [InlineData("{\"type\":\"error\",\"path\":null,\"title\":\"TROUBLE VERIFYING DEPENDENCIES\",\"message\":[]}")]
    public void ElmMakeEcmascript_reference_projection_rejects_empty_or_infrastructure_reports(string json)
    {
        using var corpus = new TemporaryCorpus();

        Action project =
            () => ElmMakeErrorSnapshots.ReadReferenceReport(
                json,
                corpus.Path,
                corpus.Path,
                new Dictionary<ElmPackageIdentity, string>());

        project.Should().Throw<InvalidDataException>();
    }

    [Theory]
    [InlineData("elm: Prelude.last: empty list")]
    [InlineData("{\"type\":\"error\",\"path\":null,\"title\":\"FILE NOT FOUND\",\"message\":false}")]
    [InlineData("{\"type\":\"error\",\"title\":\"FILE NOT FOUND\",\"message\":[]}")]
    [InlineData("{\"type\":\"compile-errors\",\"errors\":[{\"path\":\"Main.elm\",\"name\":\"Main\",\"problems\":[{\"title\":\"ERROR\",\"message\":[]}]}]}")]
    public void ElmMakeEcmascript_reference_projection_rejects_malformed_reports(string json)
    {
        using var corpus = new TemporaryCorpus();

        Action project =
            () => ElmMakeErrorSnapshots.ReadReferenceReport(
                json,
                corpus.Path,
                corpus.Path,
                new Dictionary<ElmPackageIdentity, string>());

        project.Should().Throw<JsonException>();
    }

    [Fact]
    public void ElmMakeEcmascript_error_bridge_decodes_all_structured_problems_with_optional_files_and_regions()
    {
        var position = ElmValue.RecordInstance([("line", ElmValue.Integer(3)), ("column", ElmValue.Integer(7))]);
        var region = ElmValue.TagInstance("Region", [position, position]);

        var file =
            ElmValue.TagInstance(
                "ApplicationFile",
                [
                ElmValue.ListInstance([ElmValue.StringInstance("src"), ElmValue.StringInstance("A.elm")])
                ]);

        var diagnostic =
            ElmValue.RecordInstance(
                [
                    ("location", ElmValue.TagInstance("Source", [file, region])),
                    ("kind", ElmValue.TagInstance("NamingProblem", [ElmValue.TagInstance("UnknownValue", [ElmValue.ListInstance([]), ElmValue.StringInstance("missing")])]))
                ]);

        var project =
            ElmValue.RecordInstance(
                [("location", ElmValue.TagInstance("Project", [])), ("kind", ElmValue.TagInstance("NoMain", []))]);

        var result =
            ElmValue.TagInstance(
                "Err",
                [
                ElmValue.TagInstance("CompilerErrors", [diagnostic, ElmValue.ListInstance([diagnostic, project])])
                ]);

        var decoded = ElmMakeCompiler.DecodeResult(ElmValueEncoding.ElmValueAsPineValue(result)).IsErrOrNull()!;
        var problems = decoded.Should().BeOfType<ElmMakeError.Diagnostics>().Subject.Problems;
        problems.Should().HaveCount(3);

        problems[0].Location.Should().BeOfType<ElmMakeLocation.Source>()
            .Which.Region.Should().Be(new ElmMakeRegion(new(3, 7), new(3, 7)));

        problems[0].Kind.Code.Should().Be(ElmMakeProblemCode.UnknownValue);
        problems[0].Kind.Names.Should().Equal("missing");
        problems[2].Location.Should().BeOfType<ElmMakeLocation.Project>();
    }

    [Theory]
    [InlineData(ElmMakeExecutionPath.PineVM)]
    [InlineData(ElmMakeExecutionPath.SyntaxInterpreter)]
    public async Task ElmMakeEcmascript_structured_naming_errors_collect_across_declarations_and_imported_modules(
        ElmMakeExecutionPath executionPath)
    {
        var original = ValidScenario;
        var expectation = original.Expectations[0];
        var inputs = await new ElmMakePackageSnapshots(CorpusDirectory).PrepareAsync(original, expectation);
        FileTree Source(string text) => FileTree.File(Encoding.UTF8.GetBytes(text));

        var app =
            original.AppFiles
            .SetNodeAtPathSorted(
                [
                "src",
                "Main.elm"
                ],
                Source("module Main exposing (main)\nimport Html\nimport A\nimport B\nmain = Html.text (A.value ++ B.value)\n"))
            .SetNodeAtPathSorted(
                [
                "src",
                "A.elm"
                ],
                Source("module A exposing (value)\nvalue = (missingOne, missingTwo)\n"))
            .SetNodeAtPathSorted(["src", "B.elm"], Source("module B exposing (value)\nvalue = missingThree\n"));

        var result =
            ElmMakeCompiler.Compile(
                app,
                inputs.ManifestPath,
                ["src", "Main.elm"],
                expectation.OutputType,
                expectation.Flavor,
                inputs.Packages,
                executionPath: executionPath);

        var diagnostics = result.IsErrOrNull().Should().BeOfType<ElmMakeError.Diagnostics>().Subject.Problems;
        diagnostics.Should().HaveCount(3);
        diagnostics.Should().OnlyContain(problem => problem.Kind.Code == ElmMakeProblemCode.UnknownValue);

        diagnostics.SelectMany(problem => problem.Kind.Names).Should().BeEquivalentTo(
            "missingOne",
            "missingTwo",
            "missingThree");

        var locations =
            diagnostics.Select(
                problem => problem.Location.Should().BeOfType<ElmMakeLocation.Source>().Subject).ToArray();

        locations.Select(location => location.FileId.Should().BeOfType<ElmMakeFile.Application>().Subject.Path)
            .Select(path => string.Join("/", path)).Should().BeEquivalentTo("src/A.elm", "src/A.elm", "src/B.elm");

        locations.Select(location => location.Region.End.Line - location.Region.Start.Line + 1).Should().OnlyContain(
            lines => lines == 1);

        locations.OrderBy(location => location.Region.Start.Column).Select(location => location.Region.Start.Column)
            .Should().Equal(9, 10, 22);

        var scenario =
            original with { AppFiles = app, Expectations = [expectation with { Kind = ElmMakeExpectationKind.ErrorsOmittingDetails }] };

        var actual =
            await ElmMakeScenarioCompiler.CompileAsync(
                CorpusDirectory,
                scenario,
                scenario.Expectations[0],
                executionPath: executionPath);

        var reference =
            await new ElmMakeReferenceCompiler(CorpusDirectory).CompileAsync(scenario, scenario.Expectations[0]);

        actual.Should().Be(reference);
    }

    [Fact]
    public void ElmMakeEcmascript_structured_regions_allow_multiline_transformations_and_reject_invalid_bounds()
    {
        var range = new ElmMakeRegion(new(3, 5), new(8, 2));
        (range.End.Line - range.Start.Line + 1).Should().Be(6);
        Action zeroBased = () => new ElmMakePosition(0, 1);
        zeroBased.Should().Throw<ArgumentOutOfRangeException>();
        Action reversed = () => new ElmMakeRegion(new(8, 2), new(3, 5));
        reversed.Should().Throw<ArgumentException>();
    }

    [Theory]
    [InlineData(ElmMakeExecutionPath.PineVM)]
    [InlineData(ElmMakeExecutionPath.SyntaxInterpreter)]
    public async Task ElmMakeEcmascript_structured_multiline_patterns_preserve_range_and_semantic_arity(
        ElmMakeExecutionPath executionPath)
    {
        var original = ValidScenario;
        var expectation = original.Expectations[0];
        var inputs = await new ElmMakePackageSnapshots(CorpusDirectory).PrepareAsync(original, expectation);

        const string Source =
            "module Main exposing (main)\nimport Html\ntype Choice = Pair Int Int\nmain = Html.text \"\"\nchoose value =\n    case value of\n        Pair\n            only -> only\n";

        var app =
            original.AppFiles.SetNodeAtPathSorted(["src", "Main.elm"], FileTree.File(Encoding.UTF8.GetBytes(Source)));

        var result =
            ElmMakeCompiler.Compile(
                app,
                inputs.ManifestPath,
                ["src", "Main.elm"],
                expectation.OutputType,
                expectation.Flavor,
                inputs.Packages,
                executionPath: executionPath);

        var diagnostics = result.IsErrOrNull().Should().BeOfType<ElmMakeError.Diagnostics>().Subject.Problems;
        var problem = diagnostics.Should().ContainSingle().Subject;
        problem.Kind.Code.Should().Be(ElmMakeProblemCode.WrongConstructorArity);
        problem.Kind.Names.Should().Equal("Pair");
        problem.Kind.ExpectedArity.Should().Be(2);
        problem.Kind.ActualArity.Should().Be(1);
        var region = problem.Location.Should().BeOfType<ElmMakeLocation.Source>().Which.Region;
        region.Start.Should().Be(new ElmMakePosition(7, 9));
        region.End.Should().Be(new ElmMakePosition(8, 17));
        (region.End.Line - region.Start.Line + 1).Should().Be(2);
    }

    [Fact]
    public async Task ElmMakeEcmascript_structured_syntax_failures_accumulate_and_retain_unicode_and_eof_positions()
    {
        var original = ValidScenario;
        var expectation = original.Expectations[0];
        var inputs = await new ElmMakePackageSnapshots(CorpusDirectory).PrepareAsync(original, expectation);

        var app =
            original.AppFiles
            .SetNodeAtPathSorted(
                [
                "src",
                "A.elm"
                ],
                FileTree.File(Encoding.UTF8.GetBytes("module A exposing (..)\nvalue =\n")))
            .SetNodeAtPathSorted(
                [
                "src",
                "B.elm"
                ],
                FileTree.File(Encoding.UTF8.GetBytes("module B exposing (..)\nvalue = \"🙂\" +\n")));

        var result =
            ElmMakeCompiler.Compile(
                app,
                inputs.ManifestPath,
                ["src", "Main.elm"],
                expectation.OutputType,
                expectation.Flavor,
                inputs.Packages);

        var diagnostics = result.IsErrOrNull().Should().BeOfType<ElmMakeError.Diagnostics>().Subject.Problems;
        diagnostics.Should().HaveCount(2).And.OnlyContain(problem => problem.Kind.Code == ElmMakeProblemCode.Syntax);

        diagnostics.Select(problem => problem.Location.Should().BeOfType<ElmMakeLocation.Source>().Subject.Region.Start)
            .Should().OnlyContain(position => position.Line == 3 && position.Column == 1);
    }

    [Fact]
    public async Task ElmMakeEcmascript_structured_missing_package_sources_are_internal_not_source_diagnostics()
    {
        var scenario = ValidScenario;

        var inputs =
            await new ElmMakePackageSnapshots(CorpusDirectory).PrepareAsync(scenario, scenario.Expectations[0]);

        var packages =
            inputs.Packages.Where(item => item.Key.Name != "elm/html").ToDictionary(
                item => item.Key,
                item => item.Value);

        var result =
            ElmMakeCompiler.Compile(
                scenario.AppFiles,
                inputs.ManifestPath,
                ["src", "Main.elm"],
                ElmMakeOutputType.JavaScript,
                ElmMakeFlavor.Development,
                packages);

        result.IsErrOrNull().Should().BeOfType<ElmMakeError.Internal>().Which.Problem.Kind
            .Should().Be(ElmMakeInternalProblemKind.MissingPackageSources);
    }

    [Fact]
    public async Task ElmMakeEcmascript_structured_package_identity_and_unicode_columns_do_not_require_path_or_message_parsing()
    {
        var original = ValidScenario;
        var expectation = original.Expectations[0];
        var inputs = await new ElmMakePackageSnapshots(CorpusDirectory).PrepareAsync(original, expectation);
        var identity = inputs.Packages.Keys.Single(package => package.Name == "elm/html");
        var html = inputs.Packages[identity];
        var source = Encoding.UTF8.GetString(((FileTree.FileNode)html.GetNodeAtPath(["src", "Html.elm"])!).Bytes.Span);
        var packages = inputs.Packages.ToDictionary(pair => pair.Key, pair => pair.Value);

        packages[identity] =
            html.SetNodeAtPathSorted(
                ["src", "Html.elm"],
                FileTree.File(Encoding.UTF8.GetBytes(source + "\nunused = (\"🙂\", missingValue)\n")));

        var result =
            ElmMakeCompiler.Compile(
                original.AppFiles,
                inputs.ManifestPath,
                ["src", "Main.elm"],
                expectation.OutputType,
                expectation.Flavor,
                packages);

        var problem =
            result.IsErrOrNull().Should().BeOfType<ElmMakeError.Diagnostics>().Subject.Problems
            .Should().ContainSingle().Subject;

        var location = problem.Location.Should().BeOfType<ElmMakeLocation.Source>().Subject;
        var file = location.FileId.Should().BeOfType<ElmMakeFile.Package>().Subject;
        file.Identity.Should().Be(identity);
        file.Path.Should().Equal("src", "Html.elm");
        location.Region.Start.Column.Should().Be(16);
        location.Region.End.Column.Should().Be(28);
        problem.Kind.Names.Should().Equal("missingValue");
    }

    [Fact]
    public async Task ElmMakeEcmascript_structured_cyclic_alias_is_a_located_source_problem_not_an_invalid_manifest()
    {
        var original = ValidScenario;
        var expectation = original.Expectations[0];
        var inputs = await new ElmMakePackageSnapshots(CorpusDirectory).PrepareAsync(original, expectation);

        const string Source =
            "module Main exposing (main)\nimport Html\ntype alias Loop = Loop\nmain : Loop\nmain = Html.text \"\"\n";

        var app =
            original.AppFiles.SetNodeAtPathSorted(["src", "Main.elm"], FileTree.File(Encoding.UTF8.GetBytes(Source)));

        var result =
            ElmMakeCompiler.Compile(
                app,
                inputs.ManifestPath,
                ["src", "Main.elm"],
                expectation.OutputType,
                expectation.Flavor,
                inputs.Packages);

        var problem =
            result.IsErrOrNull().Should().BeOfType<ElmMakeError.Diagnostics>().Subject.Problems.Should().ContainSingle()
            .Subject;

        problem.Kind.Code.Should().Be(ElmMakeProblemCode.CyclicTypeAlias);
        problem.Kind.Names.Should().Equal("Loop");

        problem.Location.Should().BeOfType<ElmMakeLocation.Source>().Which.Region
            .Should().Be(new ElmMakeRegion(new(3, 12), new(3, 16)));
    }

    [Fact]
    public async Task ElmMakeEcmascript_versions_keep_requests_and_outcomes_independent()
    {
        using var corpus = new TemporaryCorpus();
        var output = ElmMakeEcmascriptScenarios.ParseExpectation("example/expect/0-19-1/src/Main.elm.js");

        var errors =
            ElmMakeEcmascriptScenarios.ParseExpectation(
                "example/expect/0-19-2/src/Main.elm.js.errors-omitting-details.txt");

        ElmMakeSnapshotProblem[] problems = [new("src/Main.elm", new(new(3, 4), new(3, 10)), "NAMING ERROR")];
        await corpus.WriteExpectation(output, "compiled output");
        await corpus.WriteExpectation(errors, ElmMakeErrorSnapshots.Render(problems));

        var scenarios = ElmMakeEcmascriptScenarios.Discover(corpus.Path);
        scenarios.Should().ContainSingle().Which.Expectations.Should().Equal(output, errors);
        var calls = new List<ElmMakeExpectation>();

        var results =
            await ElmMakeEcmascriptScenarios.RunAsync(
                corpus.Path,
                scenarios,
                (_, expectation, _) =>
                {
                    calls.Add(expectation);

                    return
                        Task.FromResult<ElmMakeCompilation>(
                            expectation.CompilerVersion == output.CompilerVersion
                            ?
                            new ElmMakeCompilation.Output("compiled output")
                            :
                            new ElmMakeCompilation.CompilerErrors(problems));
                });

        calls.Should().Equal(output, errors);
        results.Should().HaveCount(2).And.OnlyContain(result => result.Passed);

        ElmMakeEcmascriptScenarios.RenderResults(results)
            .Should().Contain("Compiler 0.19.1: total 1, succeeded 1, failed 0")
            .And.Contain("Compiler 0.19.2: total 1, succeeded 1, failed 0");
    }

    [Fact]
    public async Task ElmMakeEcmascript_error_runner_distinguishes_outcomes_failures_filters_and_conflicts()
    {
        using var corpus = new TemporaryCorpus();

        var expectation =
            ElmMakeEcmascriptScenarios.ParseExpectation(
                "example/expect/0-19-1/src/Main.elm.js.errors-omitting-details.txt");

        ElmMakeSnapshotProblem[] problems = [new("src/Main.elm", new(new(3, 4), new(3, 10)), "NAMING ERROR")];
        await corpus.WriteExpectation(expectation, ElmMakeErrorSnapshots.Render(problems));
        ElmMakeScenario[] scenarios = [new("example", FileTree.EmptyTree, [expectation])];

        var passing = await Run(new ElmMakeCompilation.CompilerErrors(problems));
        passing.Single().Passed.Should().BeTrue();

        ElmMakeEcmascriptScenarios.RenderResults(passing).Should().Contain(
            "ErrorsOmittingDetails: total 1, succeeded 1, failed 0");

        (await Run(new ElmMakeCompilation.Output(ElmMakeErrorSnapshots.Render(problems)))).Single().Error
            .Should().Contain("compilation succeeded");

        (await Run(new ElmMakeCompilation.CompilerErrors([problems[0] with { Title = "OTHER" }]))).Single().Error
            .Should().Contain("differing hunk");

        (await Run(new ElmMakeCompilation.CompilerErrors([]))).Single().Passed.Should().BeFalse();

        var failure =
            await ElmMakeEcmascriptScenarios.RunAsync(
                corpus.Path,
                scenarios,
                (_, _, _) => throw new InvalidOperationException("VM failed"));

        failure.Single().Error.Should().Contain("VM failed");

        var cancelled =
            () => ElmMakeEcmascriptScenarios.RunAsync(
                corpus.Path,
                scenarios,
                (_, _, _) => throw new OperationCanceledException());

        await cancelled.Should().ThrowAsync<OperationCanceledException>();

        var debug = expectation with { Flavor = ElmMakeFlavor.Debug };
        var optimize = expectation with { Flavor = ElmMakeFlavor.Optimize };

        var filtered =
            await ElmMakeEcmascriptScenarios.RunAsync(
                corpus.Path,
                [scenarios[0] with { Expectations = [debug, optimize, expectation with { Skipped = true }] }],
                (_, _, _) => throw new Exception("must not compile"),
                skipDebug: true,
                skipOptimize: true);

        filtered.Should().BeEmpty();

        var outputExpectation = expectation with { Kind = ElmMakeExpectationKind.Output };
        await corpus.WriteExpectation(outputExpectation, "output");

        var wrongOutcome =
            await ElmMakeEcmascriptScenarios.RunAsync(
                corpus.Path,
                [scenarios[0] with { Expectations = [outputExpectation] }],
                (_, _, _) => Task.FromResult<ElmMakeCompilation>(new ElmMakeCompilation.CompilerErrors(problems)));

        wrongOutcome.Single().Error.Should().Contain("Expected successful output");
        Action conflicts = () => ElmMakeEcmascriptScenarios.Discover(corpus.Path);
        conflicts.Should().Throw<InvalidDataException>().WithMessage("*Contradictory*");

        Task<IReadOnlyList<ElmMakeSnapshotResult>> Run(ElmMakeCompilation compilation) =>
            ElmMakeEcmascriptScenarios.RunAsync(corpus.Path, scenarios, (_, _, _) => Task.FromResult(compilation));
    }

    [Theory]
    [InlineData(ElmMakeExecutionPath.PineVM)]
    [InlineData(ElmMakeExecutionPath.SyntaxInterpreter)]
    public async Task ElmMakeEcmascript_error_generation_and_actual_backends_agree_on_project_diagnostics(
        ElmMakeExecutionPath executionPath)
    {
        using var corpus = new TemporaryCorpus();

        var expectation =
            ElmMakeEcmascriptScenarios.ParseExpectation(
                "example/expect/0-19-1/src/Main.elm.js.errors-omitting-details.txt");

        var scenario =
            new ElmMakeScenario(
                "example",
                FileTree.EmptyTree.SetNodeAtPathSorted(
                    [
                    "src",
                    "Main.elm"
                    ],
                    FileTree.File(Encoding.UTF8.GetBytes("module Main exposing (main)\nmain = 1\n"))),
                [expectation]);

        await corpus.WriteApp(scenario);
        var reference = new ElmMakeReferenceCompiler(corpus.Path);
        await reference.GenerateAsync(scenario, expectation, TextWriter.Null);
        var discovered = ElmMakeEcmascriptScenarios.Discover(corpus.Path);

        var results =
            await ElmMakeEcmascriptScenarios.RunAsync(
                corpus.Path,
                discovered,
                (app, item, token) => ElmMakeScenarioCompiler.CompileOutcomeAsync(corpus.Path, app, item, token, executionPath));

        results.Should().ContainSingle().Which.Passed.Should().BeTrue(ElmMakeEcmascriptScenarios.RenderResults(results));
        var overwrite = () => reference.GenerateAsync(scenario, expectation, TextWriter.Null);
        await overwrite.Should().ThrowAsync<IOException>().WithMessage("*remove it manually*");
    }

    [Fact]
    public async Task ElmMakeEcmascript_error_reference_generation_collects_imported_errors_and_replays_without_temp_paths()
    {
        using var corpus = new TemporaryCorpus();
        corpus.CopyPackages();
        var original = ValidScenario;

        var expectation =
            ElmMakeEcmascriptScenarios.ParseExpectation(
                "example/expect/0-19-1/src/Main.elm.html.errors-omitting-details.txt");

        var app =
            original.AppFiles
            .SetNodeAtPathSorted(
                [
                "src",
                "Main.elm"
                ],
                Source("module Main exposing (main)\nimport Html\nimport A\nimport B\nmain = Html.text (A.value ++ B.value)\n"))
            .SetNodeAtPathSorted(["src", "A.elm"], Source("module A exposing (value)\nvalue = missingA\n"))
            .SetNodeAtPathSorted(["src", "B.elm"], Source("module B exposing (value)\nvalue = missingB\n"));

        var scenario = new ElmMakeScenario("example", app, [expectation]);
        var reference = new ElmMakeReferenceCompiler(corpus.Path);
        await reference.GenerateAsync(scenario, expectation, TextWriter.Null);
        var text = await File.ReadAllTextAsync(ElmMakeEcmascriptScenarios.ExpectationPath(corpus.Path, expectation));
        var problems = ElmMakeErrorSnapshots.Parse(text);
        problems.Select(problem => problem.File).Should().BeEquivalentTo("src/A.elm", "src/B.elm");
        problems.Should().OnlyContain(problem => problem.Region != null && problem.Title == "NAMING ERROR");
        (await reference.CompileAsync(scenario, expectation)).Should().Be(text);
        var replay = await ElmMakeEcmascriptScenarios.RunAsync(corpus.Path, [scenario], reference.CompileOutcomeAsync);
        replay.Should().ContainSingle().Which.Passed.Should().BeTrue();

        static FileTree Source(string text) => FileTree.File(Encoding.UTF8.GetBytes(text));
    }

    [Fact]
    public async Task ElmMakeEcmascript_error_generation_rejects_success_and_opposite_expectations_without_writing()
    {
        using var corpus = new TemporaryCorpus();
        corpus.CopyPackages();
        var original = ValidScenario;
        var expectation = original.Expectations[0] with { Kind = ElmMakeExpectationKind.ErrorsOmittingDetails };
        var reference = new ElmMakeReferenceCompiler(corpus.Path);
        var conflict = () => reference.GenerateAsync(original, expectation, TextWriter.Null);
        await conflict.Should().ThrowAsync<InvalidDataException>().WithMessage("*Contradictory*");

        var succeeds =
            () => reference.GenerateAsync(original with { Expectations = [expectation] }, expectation, TextWriter.Null);

        await succeeds.Should().ThrowAsync<InvalidDataException>().WithMessage("*compilation succeeded*");
        File.Exists(ElmMakeEcmascriptScenarios.ExpectationPath(corpus.Path, expectation)).Should().BeFalse();
    }

    [Fact]
    public async Task ElmMakeEcmascript_error_runner_does_not_promote_unsupported_failures()
    {
        using var corpus = new TemporaryCorpus();
        corpus.CopyPackages();
        var original = ValidScenario;
        var expectation = original.Expectations[0] with { Kind = ElmMakeExpectationKind.ErrorsOmittingDetails };

        await corpus.WriteExpectation(
            expectation,
            ElmMakeErrorSnapshots.Render([new("src/Main.elm", new(new(3, 1), new(3, 2)), "SYNTAX PROBLEM")]));

        foreach (var source in new[]
        {
            "port module Main exposing (main)\nimport Html\nport send : String -> Cmd msg\nmain = Html.text \"\"\n",
        })
        {
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
                    Expectations = [expectation],
                };

            var results =
                await ElmMakeEcmascriptScenarios.RunAsync(
                    corpus.Path,
                    [scenario],
                    (app, item, token) => ElmMakeScenarioCompiler.CompileOutcomeAsync(corpus.Path, app, item, token));

            results.Should().ContainSingle().Which.Error.Should().Contain("UnsupportedFeature");
        }
    }

    [Fact]
    public async Task ElmMakeEcmascript_error_preparation_allows_invalid_inputs_but_not_missing_archives()
    {
        using var corpus = new TemporaryCorpus();
        var original = ValidScenario;
        var error = original.Expectations[0] with { Kind = ElmMakeExpectationKind.ErrorsOmittingDetails };
        var snapshots = new ElmMakePackageSnapshots(corpus.Path);
        var missing = () => snapshots.PrepareAsync(original, error);
        await missing.Should().ThrowAsync<InvalidDataException>().WithMessage("*elm/core@1.0.5*");

        var invalid =
            original with
            {
                AppFiles =
                original.AppFiles.SetNodeAtPathSorted(["elm.json"], FileTree.File(Encoding.UTF8.GetBytes("{"))),
                Expectations = [error],
            };

        (await snapshots.PrepareAsync(invalid, error)).Packages.Should().BeEmpty();
        var compiler = new ElmMakeReferenceCompiler(corpus.Path);
        var crashes = () => compiler.GenerateAsync(invalid, error, TextWriter.Null);
        await crashes.Should().ThrowAsync<InvalidDataException>().WithMessage("*Invalid reference compiler failure*");
        File.Exists(ElmMakeEcmascriptScenarios.ExpectationPath(corpus.Path, error)).Should().BeFalse();

        var manifest =
            Encoding.UTF8.GetString(((FileTree.FileNode)original.AppFiles.GetNodeAtPath(["elm.json"])!).Bytes.Span);

        invalid =
            invalid with
            {
                AppFiles =
                original.AppFiles.SetNodeAtPathSorted(
                    ["elm.json"],
                    FileTree.File(
                        Encoding.UTF8.GetBytes(
                            manifest.Replace("\"application\"", "\"not-a-project-type\"", StringComparison.Ordinal)))),
            };

        await compiler.GenerateAsync(invalid, error, TextWriter.Null);
        var generated = await File.ReadAllTextAsync(ElmMakeEcmascriptScenarios.ExpectationPath(corpus.Path, error));

        ElmMakeErrorSnapshots.Parse(generated).Should().ContainSingle()
            .Which.File.Should().Be("elm.json");
    }

    [Fact]
    public async Task ElmMakeEcmascript_error_generation_allows_invalid_dependency_graphs_but_verifies_explicit_pins()
    {
        using var corpus = new TemporaryCorpus();
        corpus.CopyPackages();
        var original = ValidScenario;
        var expectation = original.Expectations[0] with { Kind = ElmMakeExpectationKind.ErrorsOmittingDetails };

        var manifest =
            JsonSerializer.Deserialize<System.Text.Json.Nodes.JsonObject>(
            ((FileTree.FileNode)original.AppFiles.GetNodeAtPath(["elm.json"])!).Bytes.Span)!;

        manifest["dependencies"]!["direct"]!.AsObject().Remove("elm/core");

        var scenario =
            original with
            {
                AppFiles =
                original.AppFiles.SetNodeAtPathSorted(
                    [
                    "elm.json"
                    ],
                    FileTree.File(Encoding.UTF8.GetBytes(manifest.ToJsonString()))),
                Expectations = [expectation],
            };

        await new ElmMakeReferenceCompiler(corpus.Path).GenerateAsync(scenario, expectation, TextWriter.Null);
        var text = await File.ReadAllTextAsync(ElmMakeEcmascriptScenarios.ExpectationPath(corpus.Path, expectation));

        ElmMakeErrorSnapshots.Parse(text).Should().ContainSingle().Which
            .Should().Be(new ElmMakeSnapshotProblem("elm.json", null, "MISSING DEPENDENCY"));

        var snapshots = new ElmMakePackageSnapshots(corpus.Path);
        var path = snapshots.ArchivePath(new("elm/html", new(1, 0, 0)));
        await File.WriteAllBytesAsync(path, new byte[] { 1, 2, 3 });
        var corrupt = () => snapshots.PrepareAsync(scenario, expectation, acquireMissing: true);
        await corrupt.Should().ThrowAsync<InvalidDataException>().WithMessage("*elm/html@1.0.0*");
        File.ReadAllBytes(path).Should().Equal(1, 2, 3);
    }

    private sealed class TemporaryCorpus : IDisposable
    {
        public string Path { get; } =
            System.IO.Path.Combine(System.IO.Path.GetTempPath(), "pine-errors-" + Guid.NewGuid().ToString("N"));

        public TemporaryCorpus() => Directory.CreateDirectory(Path);

        public async Task WriteExpectation(ElmMakeExpectation expectation, string text)
        {
            Directory.CreateDirectory(System.IO.Path.Combine(Path, "scenarios", expectation.ScenarioName, "input-app"));
            var destination = ElmMakeEcmascriptScenarios.ExpectationPath(Path, expectation);
            Directory.CreateDirectory(System.IO.Path.GetDirectoryName(destination)!);
            await File.WriteAllTextAsync(destination, text);
        }

        public async Task WriteApp(ElmMakeScenario scenario)
        {
            var input = System.IO.Path.Combine(Path, "scenarios", scenario.Name, "input-app");
            Directory.CreateDirectory(input);

            foreach (var (path, content) in scenario.AppFiles.EnumerateFilesTransitive())
            {
                var destination = System.IO.Path.Combine([input, .. path]);
                Directory.CreateDirectory(System.IO.Path.GetDirectoryName(destination)!);
                await File.WriteAllBytesAsync(destination, content.ToArray());
            }
        }

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
