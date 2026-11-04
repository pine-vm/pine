using AwesomeAssertions;
using AwesomeAssertions.Execution;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Elm.Testing;
using Pine.Core.Files;
using Pine.Core.PineLanguage;
using Pine.Core.Tests.Elm.LanguageServer;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text.Json;
using System.Threading;
using System.Threading.Tasks;
using Xunit;

using Concrete = Pine.Core.Elm.Syntax.Concrete;
using ConvertFromConcrete = Pine.Core.Elm.Syntax.Abstract.ConvertFromConcrete;
using IntermediatePineVM = Pine.Core.Runtime.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Elm.ElmTest;

public class ElmTestDiscoveryTests
{
    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void Discovery_timings_partition_source_declarations_construction_and_inventory(bool fromDirectory)
    {
        using var project = new ElmFuzzTests.Project("""suite = Test.test "pass" (\_ -> Expect.pass)""");

        var discovery =
            fromDirectory
            ?
            ElmTestDiscovery.DiscoverTests(project.Directory)
            :
            ElmTestDiscovery.DiscoverTests(
                LanguageServerTestingTests.Tree("""suite = Test.test "pass" (\_ -> Expect.pass)"""));

        discovery.SourceResolutionDuration.Should().BeGreaterThan(TimeSpan.Zero);
        discovery.DeclarationPreparationDuration.Should().BeGreaterThan(TimeSpan.Zero);
        discovery.TestConstructionDuration.Should().BeGreaterThan(TimeSpan.Zero);
        discovery.InventoryProjectionDuration.Should().BeGreaterThan(TimeSpan.Zero);

        discovery.Duration.Should().BeGreaterThanOrEqualTo(
            discovery.SourceResolutionDuration + discovery.DeclarationPreparationDuration +
            discovery.TestConstructionDuration + discovery.InventoryProjectionDuration);
    }

    [Theory]
    [InlineData("""suite = Test.test "pass" (\_ -> Expect.pass)""")]
    [InlineData("""suite = Test.describe "generated" (List.map (\n -> Test.test (String.fromInt n) (\_ -> Expect.equal n n)) [ 1, 2 ])""")]
    [InlineData("""
        suite =
            let
                even n = if n == 0 then True else odd (n - 1)
                odd n = if n == 0 then False else even (n - 1)
            in
            Test.test "recursive" (\_ -> Expect.equal True (even 4))
        """)]
    [InlineData("""
        suite =
            let
                expected = 3
                check = Expect.equal expected
            in
            Test.test "partial" (\_ -> check 3)
        """)]
    [InlineData("""
        suite =
            let
                state = { wrap = Just, read = .answer, original = { answer = 7 } }
            in
            Test.test "function record" (\_ -> Expect.equal (Just 7) (state.wrap (state.read state.original)))
        """)]
    [InlineData("""
        type alias Holder = { fn : Int -> Int, answer : Int }
        suite =
            let
                make = Holder ((+) 2)
            in
            Test.test "record constructor" (\_ -> Expect.equal 5 ((make 3).fn (make 3).answer))
        """)]
    [InlineData("""
        suite =
            let
                n = 7
                add n = n + 1
                record = { answer = n }
                changed = { record | answer = 1 }
            in
            Test.test "shadowing" (\_ -> Expect.equal 2 (add changed.answer))
        """)]
    [InlineData("""
        original = { answer = 7 }
        updated = { original | answer = 42 }
        suite = Test.test "global update receiver" (\_ -> Expect.equal 42 updated.answer)
        """)]
    [InlineData("""
        suite = Test.test "direct log" (\_ -> Expect.equal 42 (Debug.log "ignored" 42))
        """)]
    [InlineData("""
        logged value = Debug.log "ignored" (value + 1)
        suite = Test.test "helper log" (\_ -> Expect.equal 42 (logged 41))
        """)]
    [InlineData("""
        suite =
            let
                log = Debug.log "ignored"
            in
            Test.test "captured log" (\_ -> Expect.equal 42 (log 42))
        """)]
    [InlineData("""
        suite =
            let
                state = { log = Debug.log }
            in
            Test.test "higher order log" (\_ -> Expect.equal 42 (state.log "ignored" 42))
        """)]
    [InlineData("""
        suite = Test.test "logged function" (\_ -> Expect.equal 42 (Debug.log "ignored" ((+) 1) 41))
        """)]
    [InlineData("""
        suite = Test.test "ignored message" (\_ -> Expect.equal 42 (Debug.log (Debug.todo "message must not run") 42))
        """)]
    public void Syntax_test_discovery_compiles_captured_runners_to_Pine_evaluations(string declarations) =>
        AssertCompiledRunnersPass(declarations);

    [Theory]
    [InlineData("""
        type Expression = Environment | Builtin String Expression | Label Expression
        evaluate expression environment =
            case expression of
                Environment -> Ok environment
                Builtin name input ->
                    evaluate input environment
                        |> Result.mapError ((++) "input: ")
                        |> Result.andThen (applyBuiltin name)
                Label tagged -> evaluate tagged environment
        applyBuiltin name input =
            case name of
                "head" -> Ok (List.take 1 input)
                _ -> Err "unknown"
        suite =
            let
                environment = [ 7, 9 ]
                run expression = evaluate expression environment
                head = Builtin "head" Environment
            in
            Test.test "captured pipeline" (\_ -> Expect.equal (Ok [ 7 ]) (run (Label head)))
        """)]
    [InlineData("""
        fail input = case input of
            Just value -> value
        suite = Test.test "lazy and" (\_ -> Expect.equal False (False && fail Nothing))
        """)]
    [InlineData("""
        fail input = case input of
            Just value -> value
        suite = Test.test "lazy or" (\_ -> Expect.equal True (True || fail Nothing))
        """)]
    public void Syntax_test_execution_preserves_operator_semantics(string declarations) =>
        AssertCompiledRunnersPass(declarations);

    [Theory]
    [InlineData("PineTests.elm", "Label leaves evaluation unchanged")]
    [InlineData("ElmInteractive2024Tests.elm", "floor 0.1")]
    [InlineData("LanguageService/CompletionTests.elm", "Function arguments are suggested within their scope")]
    public void Syntax_test_execution_preserves_bundled_project_semantics(string testFile, string selector)
    {
        var sources =
            FileTree.FromSetOfFiles(
                BundledFiles.CompilerSourceContainerFilesDefault.Value.EnumerateFilesTransitive()
                .Where(
                    file => file.path[0] != "tests" ||
                        string.Join('/', file.path.Skip(1)) == testFile ||
                        file.path.SequenceEqual(["tests", "LanguageService", "TestHelpers.elm"]))
                .Select(file => (file.path, file.fileContent)));

        var discovery = ElmTestDiscovery.DiscoverTests(sources);

        var selected =
            discovery.Tests.Where(
                test => test.ListedTest.Name == selector ||
                    test.ListedTest.DescriptionPath.Contains(selector)).ToArray();

        selected.Should().NotBeEmpty();
        var entries = discovery.PrepareExecution(selected.Select(test => test.Id));
        entries.Should().HaveCount(selected.Length);
        using var cacheSession = new ElmTestRunCacheSession();

        var vm =
            IntermediatePineVM.CreateCustom(
                evalCache: null,
                evaluationConfigDefault: ElmTestRunner.DefaultEvaluationConfig,
                reportFunctionApplication: null,
                compilationEnvClasses: null,
                disableReductionInCompilation: false,
                selectPrecompiled: null,
                skipInlineForExpression: _ => false,
                enableTailRecursionOptimization: true,
                parseCache: null,
                precompiledLeaves: cacheSession.PrecompiledLeaves,
                reportEnterPrecompiledLeaf: null,
                reportExitPrecompiledLeaf: null,
                optimizationParametersSerial: null,
                cacheFileStore: null,
                invocationCache: cacheSession.InvocationCache);

        foreach (var test in selected)
        {
            using var scope = new AssertionScope(test.ListedTest.Name);
            var evaluation = entries[test.Id].Evaluation;

            var value =
                vm.EvaluateExpression(evaluation.Expression, evaluation.Environment)
                .Extract(error => throw new InvalidOperationException(error));

            AssertPassingExpectations(value);
        }
    }

    private static void AssertCompiledRunnersPass(string declarations)
    {
        using var project = new ElmFuzzTests.Project(declarations);
        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        var entries = discovery.PrepareExecution();
        entries.Should().HaveCount(discovery.Tests.Length);
        var vm = ElmCompilerInDotnet.ElmCompilerTestHelper.PineVMForProfiling(_ => { });

        foreach (var test in discovery.Tests)
        {
            var evaluation = entries[test.Id].Evaluation;

            var value =
                vm.EvaluateExpression(evaluation.Expression, evaluation.Environment)
                .Extract(error => throw new InvalidOperationException(error));

            AssertPassingExpectations(value);
        }
    }

    [Fact]
    public void Syntax_test_execution_does_not_repeat_suite_construction()
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                slow n = if n == 0 then 42 else slow (n - 1)
                suite =
                    let captured = slow 2000
                    in Test.test "captured" (\_ -> Expect.equal 42 captured)
                """);

        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        var test = discovery.Tests.Single();
        var evaluation = discovery.PrepareExecution([test.Id])[test.Id].Evaluation;

        var report =
            ElmCompilerInDotnet.ElmCompilerTestHelper.EvaluateWithProfiling(
                evaluation.Expression,
                evaluation.Environment).evalReport;

        AssertPassingExpectations(report.ReturnValue.Evaluate());
        report.InvocationCount.Should().BeLessThan(2000);
        report.LoopIterationCount.Should().BeLessThan(2000);
    }

    [Fact]
    public void Execution_settings_reuse_constructed_inventory_and_current_preparation_cancellation()
    {
        using var project =
            new ElmFuzzTests.Project(
                """suite = Test.test "captured" (let expected = 42 in \_ -> Expect.equal 42 expected)""");

        using var discoveryCancellation = new CancellationTokenSource();

        var discovery =
            ElmTestDiscovery.DiscoverTests(
                project.Directory,
                cancellationToken: discoveryCancellation.Token);

        discoveryCancellation.Cancel();

        discovery.WithExecutionSettings(new(0, 100)).Should().BeSameAs(discovery);
        var configured = discovery.WithExecutionSettings(new(123, 3));
        configured.CompileExecution.Should().BeSameAs(discovery.CompileExecution);
        configured.SourceLocations.Should().BeSameAs(discovery.SourceLocations);
        configured.Tests.Should().Equal(discovery.Tests);
        configured.Items.Select(item => item.Id).Should().Equal(discovery.Items.Select(item => item.Id));

        Action cancelled = () => configured.PrepareExecution(cancellationToken: new CancellationToken(true));
        cancelled.Should().Throw<OperationCanceledException>();
        Action emptyCancelled = () => configured.PrepareExecution([], cancellationToken: new CancellationToken(true));
        emptyCancelled.Should().Throw<OperationCanceledException>();
        configured.PrepareExecution([]).Should().BeEmpty();
        Action invalidSettings = () => discovery.WithExecutionSettings(new(0, 0));
        invalidSettings.Should().Throw<ArgumentOutOfRangeException>();
        Action invalidPreparation = () => discovery.PrepareExecution([], new(0, 0));
        invalidPreparation.Should().Throw<ArgumentOutOfRangeException>();

        var entry = configured.PrepareExecution().Single().Value;
        using var cache = new ElmTestRunCacheSession();

        ElmTestRunner.ExecuteDiscoveredTest(configured.Items.Single(), entry, cache)
            .Kind.Should().Be(CompletedTestKind.Passed);
    }

    [Theory]
    [InlineData(false)]
    [InlineData(true)]
    public async Task Reseeding_selected_fuzz_roots_matches_fresh_discovery_and_preserves_overrides(bool only)
    {
        var declarations =
            """
            suite = Test.describe "suite"
                [ Test.fuzz Fuzz.int "before" (\_ -> Expect.pass)
                , Test.skip (Test.fuzz Fuzz.int "skipped" (\_ -> Expect.pass))
                , Test.fuzzWith { runs = 7, distribution = Test.noDistribution }
                    Fuzz.int "fixed" (\_ -> Expect.pass)
                , SELECTED
                ]
            """.Replace(
                "SELECTED",
                only
                ?
                """Test.only (Test.fuzz Fuzz.int "chosen" (\_ -> Expect.pass))"""
                :
                """Test.fuzz Fuzz.int "chosen" (\_ -> Expect.pass)""");

        using var project = new ElmFuzzTests.Project(declarations);
        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        var chosen = discovery.Items.Single(item => item.ListedTest.Name == "chosen");

        var settings = new[] { new ElmTestExecutionSettings(42, 3), new ElmTestExecutionSettings(123, 1) };

        var entries =
            await Task.WhenAll(
                settings.Select(
                    setting => Task.Run(
                        () =>
                        discovery.PrepareExecution([chosen.Id], setting)[chosen.Id])));

        for (var index = 0; index < settings.Length; index++)
        {
            var setting = settings[index];
            var configured = discovery.WithExecutionSettings(setting);
            configured.CompileExecution.Should().BeSameAs(discovery.CompileExecution);
            configured.Items.Single(item => item.ListedTest.Name == "chosen").FuzzRuns.Should().Be(setting.FuzzRuns);
            configured.Items.Single(item => item.ListedTest.Name == "fixed").FuzzRuns.Should().Be(7);
            configured.Items.Single(item => item.ListedTest.Name == "before").FuzzRuns.Should().Be(setting.FuzzRuns);
            discovery.Items.Single(item => item.ListedTest.Name == "chosen").FuzzRuns.Should().Be(100);

            var fresh =
                ElmTestDiscovery.DiscoverTests(
                    discovery.SourceFiles,
                    executionSettings: setting);

            var expected = fresh.PrepareExecution([chosen.Id])[chosen.Id];
            entries[index].Fuzz.Should().BeEquivalentTo(expected.Fuzz);
            entries[index].Evaluation.Should().Be(expected.Evaluation);

            if (!only)
            {
                var fixedTest = configured.Items.Single(item => item.ListedTest.Name == "fixed");
                configured.PrepareExecution([fixedTest.Id])[fixedTest.Id].Fuzz!.RunsRequested.Should().Be(7);
            }
        }
    }

    [Fact]
    public void Syntax_listing_includes_skipped_tests_without_creating_execution_VMs()
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                suite = Test.describe "group"
                    [ Test.test "active" (\_ -> Expect.pass)
                    , Test.skip (Test.test "skipped" (\_ -> Debug.todo "must never execute"))
                    ]
                """);

        var listed =
            ElmTestRunner.DiscoverAndRunTests(
                project.Directory,
                listTests: true,
                pineVmFactory: (_, _) => throw new InvalidOperationException("Listing must not create an execution VM"))
            .Should().BeOfType<ElmTestRun.Listed>().Subject;

        listed.Tests.Should().ContainSingle().Which.Name.Should().Be("active");
        listed.SkippedTests.Should().ContainSingle().Which.Name.Should().Be("skipped");

        var completed =
            ElmTestRunner.DiscoverAndRunTests(project.Directory)
            .Should().BeOfType<ElmTestRun.Completed>().Subject;

        completed.Tests.Should().ContainSingle().Which.Kind.Should().Be(CompletedTestKind.Passed);
        completed.IncompleteReason.Should().Contain("Test.skip");
    }

    [Fact]
    public void Syntax_execution_preparation_omits_TODOs_and_rejects_nonselected_IDs()
    {
        using var project =
            new ElmFuzzTests.Project(
                """suite = Test.describe "group" [ Test.todo "todo", Test.skip (Test.todo "skipped") ]""");

        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.PrepareExecution().Should().BeEmpty();
        var group = discovery.Items.Single(item => item.Kind == ElmTestDiscoveryItemKind.Group);
        var skipped = discovery.Items.Single(item => item.Selection == ElmTestDiscoverySelection.Skipped);
        var compileGroup = () => discovery.PrepareExecution([group.Id]);
        var compileSkipped = () => discovery.PrepareExecution([skipped.Id]);
        var compileUnknown = () => discovery.PrepareExecution(["unknown"]);
        compileGroup.Should().Throw<InvalidOperationException>().WithMessage("*group or is not selected*");
        compileSkipped.Should().Throw<InvalidOperationException>().WithMessage("*group or is not selected*");
        compileUnknown.Should().Throw<KeyNotFoundException>();
    }

    private static void AssertPassingExpectations(PineValue value)
    {
        var decoded =
            ElmValueEncoding.PineValueAsElmValue(value, null, null)
            .Extract(error => throw new InvalidOperationException(error));

        var expectations = decoded.Should().BeOfType<ElmValue.ElmList>().Subject.Items;
        expectations.Should().NotBeEmpty();

        foreach (var expectation in expectations)
            expectation.Should().BeOfType<ElmValue.ElmTag>().Subject.TagName.Should().Be("Pass");
    }

    [Fact]
    public void Syntax_test_discovery_resolves_test_IDs_to_exact_nested_source_expressions()
    {
        const string first = """Test.test "first" (\_ -> Expect.pass)""";
        const string second = """Test.test "second" (\_ -> Expect.pass)""";

        using var project =
            new ElmFuzzTests.Project(
                "suite = Test.describe \"group\" [ ((" + first + ")), " + second + " ]");

        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Select(test => test.ListedTest.Name).Should().Equal("first", "second");
        AssertSource(project, discovery, discovery.Tests[0].Id, first);
        AssertSource(project, discovery, discovery.Tests[1].Id, second);
        discovery.Tests.Select(test => test.Id).Distinct().Should().HaveCount(2);
        discovery.GetSourceLocation(discovery.Tests[0].Id).ExpressionPath.Steps.Should().NotBeEmpty();
    }

    [Theory]
    [InlineData("""suite = { z = Test.test "z" (\_ -> Expect.pass), a = Test.test "a" (\_ -> Expect.pass) }.z""",
        """Test.test "z" (\_ -> Expect.pass)""", "z")]
    [InlineData("""suite = let local = Test.test "local" (\_ -> Expect.pass) in local""",
        """Test.test "local" (\_ -> Expect.pass)""", "local")]
    [InlineData("""suite = let make label = Test.test label (\_ -> Expect.pass) in make "local function" """,
        """Test.test label (\_ -> Expect.pass)""", "local function")]
    [InlineData("""suite = let ( chosen, ignored ) = (Test.test "tuple" (\_ -> Expect.pass), 1) in chosen""",
        """Test.test "tuple" (\_ -> Expect.pass)""", "tuple")]
    [InlineData("""
        suite =
            case 1 of
                0 -> Test.test "not selected" (\_ -> Expect.pass)
                1 -> Test.test "constant branch" (\_ -> Expect.pass)
                _ -> Test.todo "fallback"
        """, """Test.test "constant branch" (\_ -> Expect.pass)""", "constant branch")]
    [InlineData("""suite = Test.test "pipeline" <| (\_ -> Expect.pass)""",
        """Test.test "pipeline" <| (\_ -> Expect.pass)""", "pipeline")]
    [InlineData("""suite = (\_ -> Expect.pass) |> Test.test "reverse pipeline" """,
        """(\_ -> Expect.pass) |> Test.test "reverse pipeline" """, "reverse pipeline")]
    public void Syntax_test_discovery_maps_normalized_AST_paths_back_to_CST(
        string declarations, string expectedExpression, string expectedName)
    {
        using var project = new ElmFuzzTests.Project(declarations);
        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Should().ContainSingle().Which.ListedTest.Name.Should().Be(expectedName);
        AssertSource(project, discovery, discovery.Tests[0].Id, expectedExpression.TrimEnd());
    }

    [Fact]
    public void Syntax_test_discovery_maps_computed_and_generated_tests_to_the_factory_source()
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                factory index =
                    Test.test ("case " ++ String.fromInt index) (\_ -> Expect.pass)

                suite =
                    Test.describe "generated" (List.map factory (List.range 1 4))
                """);

        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Select(test => test.ListedTest.Name).Should().Equal("case 1", "case 2", "case 3", "case 4");
        discovery.Tests.Select(test => test.Id).Distinct().Should().HaveCount(4);

        foreach (var test in discovery.Tests)
        {
            AssertSource(
                project,
                discovery,
                test.Id,
                """Test.test ("case " ++ String.fromInt index) (\_ -> Expect.pass)""");
        }
    }

    [Fact]
    public void Syntax_test_discovery_preserves_imported_helper_source_provenance()
    {
        using var project = new ElmFuzzTests.Project("""suite = Helpers.make "from helper" """);
        var testFile = Path.Combine(project.Directory, "tests", "Tests.elm");

        File.WriteAllText(
            testFile,
            File.ReadAllText(testFile).Replace("import Test\n", "import Test\nimport Helpers\n"));

        const string helper =
            """
            module Helpers exposing (make)
            import Test
            import Expect

            make label =
                Test.test label (\_ -> Expect.pass)
            """;

        File.WriteAllText(Path.Combine(project.Directory, "tests", "Helpers.elm"), helper);

        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        var location = discovery.GetSourceLocation(discovery.Tests.Single().Id);
        location.FilePath.Should().Be("tests/Helpers.elm");
        location.ModuleOrigin.ModuleName.Should().Be("Helpers");
        location.DeclarationName.Should().Be("make");
        Slice(helper, location.Range).Should().Be("""Test.test label (\_ -> Expect.pass)""");
    }

    [Fact]
    public void Syntax_test_discovery_does_not_execute_assertions_or_fuzz_callbacks()
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                loop _ = loop ()
                suite =
                    Test.describe "safe discovery"
                        [ Test.test "unit" (\_ -> loop ())
                        , Test.fuzz Fuzz.int "fuzz" (\_ -> loop ())
                        , Test.todo "later"
                        ]
                """);

        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Select(test => test.Kind).Should().Equal("unit", "fuzz", "todo");

        foreach (var test in discovery.Tests)
            discovery.GetSourceLocation(test.Id).FilePath.Should().Be("tests/Tests.elm");
    }

    [Fact]
    public void Syntax_test_discovery_inventory_keeps_groups_skipped_tests_and_empty_groups()
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                suite =
                    Test.describe "outer"
                        [ Test.test "active" (\_ -> Expect.pass)
                        , Test.skip
                            (Test.describe "skipped group"
                                [ Test.test "skipped unit" (\_ -> Expect.pass)
                                , Test.fuzz Fuzz.int "skipped fuzz" (\_ -> Expect.pass)
                                , Test.todo "skipped todo"
                                ]
                            )
                        , Test.describe "empty" []
                        ]
                """);

        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);

        discovery.Items.Select(item => item.Kind).Should().Equal(
            ElmTestDiscoveryItemKind.Group,
            ElmTestDiscoveryItemKind.Unit,
            ElmTestDiscoveryItemKind.Group,
            ElmTestDiscoveryItemKind.Unit,
            ElmTestDiscoveryItemKind.Fuzz,
            ElmTestDiscoveryItemKind.Todo,
            ElmTestDiscoveryItemKind.Group);

        discovery.Items.Select(item => item.Selection).Should().Equal(
            ElmTestDiscoverySelection.Included,
            ElmTestDiscoverySelection.Included,
            ElmTestDiscoverySelection.Skipped,
            ElmTestDiscoverySelection.Skipped,
            ElmTestDiscoverySelection.Skipped,
            ElmTestDiscoverySelection.Skipped,
            ElmTestDiscoverySelection.Included);

        discovery.Tests.Should().ContainSingle().Which.ListedTest.Name.Should().Be("active");
        discovery.Items[2].ParentId.Should().Be(discovery.Items[0].Id);
        discovery.Items[3].ParentId.Should().Be(discovery.Items[2].Id);
        discovery.Items[4].FuzzRuns.Should().Be(100);
        discovery.Items[^1].IsEmptyGroup.Should().BeTrue();
        discovery.HasOnly.Should().BeFalse();

        foreach (var item in discovery.Items)
        {
            discovery.GetSourceLocation(item.Id).Should().Be(item.SourceLocation);

            var source =
                Slice(
                    File.ReadAllText(Path.Combine(project.Directory, item.SourceLocation.FilePath)),
                    item.SourceLocation.Range);

            source.Should().NotBeNullOrWhiteSpace();

            if (item.Kind == ElmTestDiscoveryItemKind.Group)
                source.Should().StartWith("Test.describe");
        }

        var executeGroup = () => discovery.GetExecutionFilter(discovery.Items[0].Id);
        executeGroup.Should().Throw<InvalidOperationException>().WithMessage("*group or is not selected*");
        var executeSkipped = () => discovery.GetExecutionFilter(discovery.Items[3].Id);
        executeSkipped.Should().Throw<InvalidOperationException>().WithMessage("*group or is not selected*");
    }

    [Fact]
    public void Syntax_test_discovery_inventory_distinguishes_skip_from_only_exclusion_and_aggregates_groups()
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                suite =
                    Test.describe "outer"
                        [ Test.describe "all skipped" [ Test.skip (Test.todo "skipped") ]
                        , Test.describe "not focused" [ Test.todo "excluded" ]
                        , Test.only (Test.describe "focused" [ Test.todo "included" ])
                        , Test.skip (Test.only (Test.todo "skip wins"))
                        ]
                """);

        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.HasOnly.Should().BeTrue();

        discovery.Items.Single(item => item.ListedTest.Name == "all skipped").Selection.Should().Be(
            ElmTestDiscoverySelection.Skipped);

        discovery.Items.Single(item => item.ListedTest.Name == "not focused").Selection.Should().Be(
            ElmTestDiscoverySelection.ExcludedByOnly);

        discovery.Items.Single(item => item.ListedTest.Name == "focused").IsOnly.Should().BeTrue();
        discovery.Items.Single(item => item.ListedTest.Name == "included").IsOnly.Should().BeTrue();
        var skipped = discovery.Items.Single(item => item.ListedTest.Name == "skip wins");
        skipped.Selection.Should().Be(ElmTestDiscoverySelection.Skipped);
        skipped.IsOnly.Should().BeTrue();
        discovery.Tests.Should().ContainSingle().Which.ListedTest.Name.Should().Be("included");
        discovery.Items[0].Selection.Should().Be(ElmTestDiscoverySelection.Included);
    }

    [Fact]
    public void Syntax_test_discovery_inventory_IDs_do_not_change_when_a_sibling_is_skipped()
    {
        const string declarations =
            """
            suite = Test.describe "group"
                [ Test.test "first" (\_ -> Expect.pass)
                , Test.test "second" (\_ -> Expect.pass)
                ]
            """;

        using var project = new ElmFuzzTests.Project(declarations);
        var before = ElmTestDiscovery.DiscoverTests(project.Directory);
        var path = Path.Combine(project.Directory, "tests", "Tests.elm");

        File.WriteAllText(
            path,
            File.ReadAllText(path).Replace(
                """Test.test "first" (\_ -> Expect.pass)""",
                """Test.skip (Test.test "first" (\_ -> Expect.pass))"""));

        var after = ElmTestDiscovery.DiscoverTests(project.Directory);
        after.Items.Select(item => item.Id).Should().Equal(before.Items.Select(item => item.Id));
        after.Tests.Single().Id.Should().Be(before.Tests[1].Id);
        after.Tests.Single().DiscoveryIndex.Should().Be(1);
    }

    [Fact]
    public void Syntax_test_discovery_inventory_does_not_merge_equal_group_labels_from_different_roots()
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                suite = Test.describe "same group" [ Test.todo "same leaf" ]
                second = Test.describe "same group" [ Test.todo "same leaf" ]
                """);

        var file = Path.Combine(project.Directory, "tests", "Tests.elm");
        File.WriteAllText(file, File.ReadAllText(file).Replace("exposing (suite)", "exposing (suite, second)"));
        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Items.Select(item => item.Id).Distinct().Should().HaveCount(4);
        discovery.Items[0].ParentId.Should().BeNull();
        discovery.Items[2].ParentId.Should().BeNull();
        discovery.Items[1].ParentId.Should().Be(discovery.Items[0].Id);
        discovery.Items[3].ParentId.Should().Be(discovery.Items[2].Id);
        discovery.Items[0].RootDeclarationName.Should().Be("suite");
        discovery.Items[2].RootDeclarationName.Should().Be("second");
        discovery.GetExecutionFilter(discovery.Tests[1].Id).Should().Be("#2");
    }

    [Fact]
    public void Syntax_test_discovery_inventory_retains_group_kind_for_invalid_describe()
    {
        using var project =
            new ElmFuzzTests.Project(
                """suite = Test.describe "duplicates" [ Test.todo "same", Test.todo "same" ]""");

        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Items.Should().HaveCount(2);
        discovery.Items[0].Kind.Should().Be(ElmTestDiscoveryItemKind.Group);
        discovery.Items[1].Kind.Should().Be(ElmTestDiscoveryItemKind.Unit);
        discovery.Items[1].ParentId.Should().Be(discovery.Items[0].Id);
        discovery.Tests.Should().ContainSingle();
    }

    [Theory]
    [InlineData("2147483648", 2147483648u)]
    [InlineData("4294967295", uint.MaxValue)]
    public void Syntax_test_discovery_inventory_preserves_unsigned_fuzz_counts(string runCount, uint expected)
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                suite =
                    Test.fuzzWith { runs = RUNS, distribution = Test.noDistribution }
                        Fuzz.int
                        "large count"
                        (\_ -> Expect.pass)
                """.Replace("RUNS", runCount, StringComparison.Ordinal));

        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Items.Should().ContainSingle().Which.FuzzRuns.Should().Be(expected);

        var listed =
            ElmTestRunner.CompileAndRunTests(project.Directory, listTests: true)
            .Should().BeOfType<ElmTestRun.Listed>().Which;

        discovery.Tests.Select(test => test.ListedTest).Should().Equal(listed.Tests);
    }

    [Theory]
    [InlineData("""
        suite = Test.describe "group"
            [ Test.test "unit" (\_ -> Expect.pass)
            , Test.fuzz Fuzz.int "fuzz" (\_ -> Expect.pass)
            , Test.todo "later"
            ]
        """)]
    [InlineData("""
        suite = Test.describe "group"
            [ Test.skip (Test.test "skip" (\_ -> Expect.pass))
            , Test.only (Test.test "only" (\_ -> Expect.pass))
            , Test.test "ordinary" (\_ -> Expect.pass)
            ]
        """)]
    [InlineData("""
        suite = Test.concat
            [ Test.skip (Test.only (Test.test "hidden" (\_ -> Expect.pass)))
            , Test.test "visible" (\_ -> Expect.pass)
            ]
        """)]
    [InlineData("""
        suite = Test.describe "empty" []
        """)]
    [InlineData("""
        suite = Test.describe "duplicates"
            [ Test.test "same" (\_ -> Expect.pass)
            , Test.test "same" (\_ -> Expect.pass)
            ]
        """)]
    public void Syntax_test_discovery_matches_existing_compiler_listing(string declarations)
    {
        using var project = new ElmFuzzTests.Project(declarations);

        var compiled =
            ElmTestRunner.CompileAndRunTests(project.Directory, listTests: true)
            .Should().BeOfType<ElmTestRun.Listed>().Which;

        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Select(test => test.ListedTest).Should().Equal(compiled.Tests);

        foreach (var test in discovery.Tests)
            discovery.GetSourceLocation(test.Id).FilePath.Should().Be("tests/Tests.elm");
    }

    [Fact]
    public void Syntax_test_discovery_ID_selects_a_duplicate_label_for_existing_PineVM_execution()
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                suite = Test.test "same" (\_ -> Expect.pass)
                second = Test.test "same" (\_ -> Expect.fail "selected second")
                """);

        var file = Path.Combine(project.Directory, "tests", "Tests.elm");
        File.WriteAllText(file, File.ReadAllText(file).Replace("exposing (suite)", "exposing (suite, second)"));
        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Should().HaveCount(2);
        discovery.Tests[0].Id.Should().NotBe(discovery.Tests[1].Id);

        AssertSource(
            project,
            discovery,
            discovery.Tests[1].Id,
            """Test.test "same" (\_ -> Expect.fail "selected second")""");

        discovery.GetExecutionFilter(discovery.Tests[1].Id).Should().Be("#2");

        var run =
            ElmTestRunner.CompileAndRunTests(
                project.Directory,
                filter: discovery.GetExecutionFilter(discovery.Tests[1].Id));

        run.Should().BeOfType<ElmTestRun.Completed>().Which.Tests.Should().ContainSingle()
            .Which.Kind.Should().Be(CompletedTestKind.Failed);
    }

    [Fact]
    public void Syntax_test_discovery_preserves_execution_ordinals_before_global_only_filtering()
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                suite = Test.test "ordinary" (\_ -> Expect.pass)
                second = Test.only (Test.test "same" (\_ -> Expect.pass))
                third = Test.only (Test.test "same" (\_ -> Expect.fail "selected third"))
                """);

        var file = Path.Combine(project.Directory, "tests", "Tests.elm");
        File.WriteAllText(file, File.ReadAllText(file).Replace("exposing (suite)", "exposing (suite, second, third)"));
        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Select(test => test.DiscoveryIndex).Should().Equal(2, 3);
        discovery.GetExecutionFilter(discovery.Tests[1].Id).Should().Be("#3");

        var run =
            ElmTestRunner.CompileAndRunTests(
                project.Directory,
                filter: discovery.GetExecutionFilter(discovery.Tests[1].Id));

        run.Should().BeOfType<ElmTestRun.Completed>().Which.Tests.Should().ContainSingle()
            .Which.Kind.Should().Be(CompletedTestKind.Failed);
    }

    [Fact]
    public void Syntax_test_discovery_exact_paths_include_only_excluded_leaves_but_not_skipped_leaves_or_groups()
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                suite = Test.todo "same"
                second = Test.only (Test.todo "unique")
                third = Test.only (Test.todo "same")
                fourth = Test.skip (Test.todo "unique")
                fifth = Test.describe "unique" []
                """);

        var file = Path.Combine(project.Directory, "tests", "Tests.elm");

        File.WriteAllText(
            file,
            File.ReadAllText(file).Replace("exposing (suite)", "exposing (suite, second, third, fourth, fifth)"));

        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);

        discovery.Tests.Select(test => test.DiscoveryIndex).Should().Equal(2, 3);
        discovery.GetExecutionFilter(discovery.Tests[0].Id).Should().Be("=tests/Tests.elm/unique");
        discovery.GetExecutionFilter(discovery.Tests[1].Id).Should().Be("#3");

        discovery.Items.Single(item => item.RootDeclarationName == "suite").Selection.Should().Be(
            ElmTestDiscoverySelection.ExcludedByOnly);

        discovery.Items.Single(item => item.RootDeclarationName == "fourth").Selection.Should().Be(
            ElmTestDiscoverySelection.Skipped);

        discovery.Items.Where(
            item => item.Kind == ElmTestDiscoveryItemKind.Group ||
                item.Selection != ElmTestDiscoverySelection.Included)
            .Should().OnlyContain(item => item.ExecutionFilter == null);
    }

    [Fact]
    public void Syntax_test_discovery_keeps_paths_inside_operator_operands_and_record_updates()
    {
        const string expression = """Test.test ("case " ++ String.fromInt n) (\_ -> Expect.pass)""";

        using var project =
            new ElmFuzzTests.Project(
                "suite = List.range 1 2 |> List.map (\\n -> let record = { z = 0, a = Test.todo \"unused\" } in " +
                "{ record | a = " + expression + " }.a) |> Test.describe \"mapped\"\n");

        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Select(test => test.ListedTest.Name).Should().Equal("case 1", "case 2");

        foreach (var test in discovery.Tests)
            AssertSource(project, discovery, test.Id, expression);
    }

    [Fact]
    public void Syntax_test_discovery_qualifies_custom_operator_targets_without_changing_paths()
    {
        using var project = new ElmFuzzTests.Project("""suite = "custom" <?> (\_ -> Expect.pass)""");
        var testFile = Path.Combine(project.Directory, "tests", "Tests.elm");

        File.WriteAllText(
            testFile,
            File.ReadAllText(testFile).Replace("import Test\n", "import Test\nimport Helpers exposing ((<?>))\n"));

        const string helper =
            """
            module Helpers exposing ((<?>))
            import Test
            import Expect
            infix left 5 (<?>) = makeTest
            makeTest label assertion = Test.test label assertion
            """;

        File.WriteAllText(Path.Combine(project.Directory, "tests", "Helpers.elm"), helper);
        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Single().ListedTest.Name.Should().Be("custom");
        var source = discovery.GetSourceLocation(discovery.Tests[0].Id);
        source.FilePath.Should().Be("tests/Helpers.elm");
        Slice(helper, source.Range).Should().Be("Test.test label assertion");
    }

    [Fact]
    public void Syntax_test_discovery_has_explicit_unknown_ID_and_cancellation_errors()
    {
        using var project = new ElmFuzzTests.Project("""suite = Test.todo "later" """);
        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        var lookup = () => discovery.GetSourceLocation("unknown");
        lookup.Should().Throw<KeyNotFoundException>().WithMessage("*Unknown Elm test ID*");

        var discover =
            () => ElmTestDiscovery.DiscoverTests(project.Directory, cancellationToken: new CancellationToken(true));

        discover.Should().Throw<OperationCanceledException>();

        var program =
            ElmInterpreterEnvironment.Create(["module Loop exposing (loop)\nloop _ = loop ()"])
            .Extract(error => throw new InvalidOperationException(error));

        var interpret =
            () => program.Invoke(
                Core.CodeAnalysis.DeclQualifiedName.FromString("Loop.loop"),
                [Core.Internal.PineValueInProcess.Create(PineValue.EmptyList)],
                ElmSyntaxInterpreter.EvaluationConfig.Default with { CancellationToken = new CancellationToken(true) });

        interpret.Should().Throw<OperationCanceledException>();
    }

    [Fact]
    public void Syntax_test_discovery_reports_source_errors_and_ignores_unused_helper_bodies()
    {
        using var project = new ElmFuzzTests.Project("""suite = Test.todo "later" """);

        File.WriteAllText(
            Path.Combine(project.Directory, "tests", "Helpers.elm"),
            "module Helpers exposing (identity)\nidentity value = value\nunused = Missing.value\n");

        ElmTestDiscovery.DiscoverTests(project.Directory).Tests.Should().ContainSingle();

        File.WriteAllText(
            Path.Combine(project.Directory, "tests", "Tests.elm"),
            "module Tests exposing (suite)\nsuite = Missing.value\n");

        var discover = () => ElmTestDiscovery.DiscoverTests(project.Directory);
        discover.Should().Throw<Core.Elm.ElmCompilerInDotnet.ElmCompilationException>().WithMessage("*Tests.suite*");
    }

    [Fact]
    public void Syntax_test_discovery_paths_reject_mismatched_CST_instead_of_inventing_ranges()
    {
        var concrete =
            ElmSyntaxParser.ParseModuleText("module Tests exposing (suite)\nsuite = (({ z = 0x10, a = 2 }))")
            .Extract(error => throw new InvalidOperationException(error.ToString()));

        var function = (Concrete.Declaration.FunctionDeclaration)concrete.Declarations.Single().Value;

        var expression =
            ElmSyntaxInterpreter.PrepareExpression(
                ConvertFromConcrete.FromExpression(function.Function.Declaration.Value.Expression.Value));

        var fields = ElmSyntaxInterpreter.EnumerateExpressionPaths(expression).ToArray();
        fields[1].Path.Steps[0].Should().Be(new ElmSyntaxExpressionPath.Step.RecordField("a", 0));

        fields[1].Path.Resolve(function.Function.Declaration.Value.Expression)!.Value
            .Should().BeOfType<Concrete.Expression.IntegerLiteral>()
            .Which.LiteralText.Should().Be("2");

        fields[2].Path.Resolve(function.Function.Declaration.Value.Expression)!.Value
            .Should().BeOfType<Concrete.Expression.IntegerLiteral>()
            .Which.LiteralText.Should().Be("0x10");

        ElmSyntaxExpressionPath.Root.Append(new ElmSyntaxExpressionPath.Step.Child(99))
            .Resolve(function.Function.Declaration.Value.Expression).Should().BeNull();

        ElmSyntaxExpressionPath.Root.Append(new ElmSyntaxExpressionPath.Step.RecordField("a", 0))
            .Should().Be(ElmSyntaxExpressionPath.Root.Append(new ElmSyntaxExpressionPath.Step.RecordField("a", 0)));
    }

    [Fact]
    public void Syntax_test_discovery_paths_roundtrip_JSON_without_source_ranges()
    {
        var path =
            ElmSyntaxExpressionPath.Root.Append(
                new ElmSyntaxExpressionPath.Step.LetDeclaration(2),
                new ElmSyntaxExpressionPath.Step.Implementation(),
                new ElmSyntaxExpressionPath.Step.Body(),
                new ElmSyntaxExpressionPath.Step.RecordField("a", 1),
                new ElmSyntaxExpressionPath.Step.Child(0));

        var json = JsonSerializer.Serialize(path);
        JsonSerializer.Deserialize<ElmSyntaxExpressionPath>(json).Should().Be(path);
        json.Should().NotContain("Range");
    }

    private static void AssertSource(
        ElmFuzzTests.Project project,
        ElmTestDiscoveryResult discovery,
        string id,
        string expected)
    {
        var location = discovery.GetSourceLocation(id);
        location.FilePath.Should().Be("tests/Tests.elm");

        Slice(
            File.ReadAllText(Path.Combine(project.Directory, "tests", "Tests.elm")),
            location.Range).Should().Be(expected);
    }

    private static string Slice(string text, Concrete.Range range)
    {
        var lines = text.Split('\n');
        var start = lines.Take(range.Start.Row - 1).Sum(line => line.Length + 1) + range.Start.Column - 1;
        var end = lines.Take(range.End.Row - 1).Sum(line => line.Length + 1) + range.End.Column - 1;
        return text[start..end];
    }
}
