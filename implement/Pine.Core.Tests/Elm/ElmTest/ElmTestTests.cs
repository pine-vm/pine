using AwesomeAssertions;
using Pine.Core.Elm;
using Pine.Core.Elm.Testing;
using Pine.Core.Files;
using Pine.Core.Functional;
using Pine.Core.PineLanguage;
using Pine.Core.Runtime;
using Pine.Core.Testing;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet;
using System;
using System.Collections.Concurrent;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmTest;

public class ElmTestTests
{
    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void Duration_report_renders_independent_phase_and_VM_timings(bool includeTotal)
    {
        var output =
            ElmTestRunner.RenderTestResults(
                [],
                includeTestDetails: false,
                duration: includeTotal ? TimeSpan.FromMilliseconds(100) : null,
                compilationDuration: TimeSpan.FromMilliseconds(2),
                preparationDuration: TimeSpan.FromMilliseconds(3),
                executionDuration: TimeSpan.FromMilliseconds(5),
                preparationVmDuration: TimeSpan.FromMilliseconds(1),
                executionVmDuration: TimeSpan.FromMilliseconds(11))
            .PlainText;

        output.Should().Contain(
            """
              compilation: 2 ms
              preparation: 3 ms
                VM calls (sum): 1 ms
              execution:   5 ms
                VM calls (sum): 11 ms
            """);

        if (includeTotal)
            output.Should().Contain("Duration: 100 ms");

        else
            output.Should().NotContain("Duration:");
    }

    [Fact]
    public void Duration_report_does_not_infer_unmeasured_components()
    {
        var output =
            ElmTestRunner.RenderTestResults(
                [],
                includeTestDetails: false,
                duration: TimeSpan.FromMilliseconds(100),
                compilationDuration: TimeSpan.FromMilliseconds(2))
            .PlainText;

        output.Should().Contain("  compilation: 2 ms")
            .And.NotContain("preparation:")
            .And.NotContain("execution:")
            .And.NotContain("VM calls");
    }

    [Fact]
    public void Default_Elm_test_execution_has_no_invocation_or_loop_work_quota()
    {
        ElmTestRunner.DefaultEvaluationConfig.InvocationCountLimit.Should().BeNull();
        ElmTestRunner.DefaultEvaluationConfig.LoopIterationCountLimit.Should().BeNull();
        ElmTestRunner.DefaultEvaluationConfig.StackDepthLimit.Should().Be(100_000);
    }

    [Fact]
    public void No_Elm_test_modules_is_a_test_run_result()
    {
        var projectDirectory =
            Path.Combine(
                Path.GetTempPath(),
                "pine-elm-test-runner-tests",
                Guid.NewGuid().ToString("N"));

        Directory.CreateDirectory(projectDirectory);

        try
        {
            var testRun = ElmTestRunner.CompileAndRunTests(projectDirectory);

            testRun.Should().BeOfType<ElmTestRun.NoTestModules>()
                .Which.AppDirectory.Should().Be(Path.GetFullPath(projectDirectory));
        }
        finally
        {
            Directory.Delete(projectDirectory, recursive: true);
        }
    }


    [Fact]
    public void Listed_test_and_listing_equality_is_structural()
    {
        var first =
            new ListedTest(
                "tests/Tests.elm",
                ["Root", "Group"],
                "test name");

        var second =
            new ListedTest(
                new string("tests/Tests.elm".ToCharArray()),
                [new string("Root".ToCharArray()), new string("Group".ToCharArray())],
                new string("test name".ToCharArray()));

        first.Equals(second).Should().BeTrue();
        (first == second).Should().BeTrue();
        first.GetHashCode().Should().Be(second.GetHashCode());

        var firstListing = new ElmTestRun.Listed([first]);
        var secondListing = new ElmTestRun.Listed([second]);

        firstListing.Equals(secondListing).Should().BeTrue();
        (firstListing == secondListing).Should().BeTrue();
        firstListing.GetHashCode().Should().Be(secondListing.GetHashCode());

        var firstFilteredListing =
            new ElmTestRun.Listed([first], [first with { Name = "excluded test" }]);

        var secondFilteredListing =
            new ElmTestRun.Listed([second], [second with { Name = "excluded test" }]);

        firstFilteredListing.Equals(secondFilteredListing).Should().BeTrue();
        (firstFilteredListing == secondFilteredListing).Should().BeTrue();
        firstFilteredListing.GetHashCode().Should().Be(secondFilteredListing.GetHashCode());
        firstFilteredListing.Equals(firstListing).Should().BeFalse();
    }


    [Theory]
    [InlineData(1, 1)]
    [InlineData(2, 1)]
    [InlineData(3, 1)]
    [InlineData(4, 2)]
    [InlineData(8, 4)]
    public void Default_worker_count_uses_requested_cpu_formula(
        int processorCount,
        int expectedWorkerCount)
    {
        ElmTestRunner.DefaultWorkerCount(processorCount).Should().Be(expectedWorkerCount);
    }


    [Fact]
    public void Package_sources_are_added_under_package_specific_paths()
    {
        var appCodeTree =
            FileTree.FromSetOfFilesWithStringPath(
                [
                (new[] { "tests", "Tests.elm" }, (ReadOnlyMemory<byte>)"app"u8.ToArray()),
                ]);

        var packageFiles =
            FileTree.FromSetOfFilesWithStringPath(
                [
                (new[] { "src", "Result", "Extra.elm" }, (ReadOnlyMemory<byte>)"package"u8.ToArray()),
                (new[] { "tests", "Main.elm" }, (ReadOnlyMemory<byte>)"package test"u8.ToArray()),
                ]);

        var combinedTree =
            ElmTestRunner.AddPackageSources(
                appCodeTree,
                [("elm-community/result-extra", packageFiles)]);

        combinedTree.GetNodeAtPath(["tests", "Tests.elm"])
            .Should().BeOfType<FileTree.FileNode>()
            .Which.Bytes.Span.ToArray().Should().Equal("app"u8.ToArray());

        combinedTree.GetNodeAtPath(
            ["elm-packages", "elm-community", "result-extra", "src", "Result", "Extra.elm"])
            .Should().BeOfType<FileTree.FileNode>()
            .Which.Bytes.Span.ToArray().Should().Equal("package"u8.ToArray());

        combinedTree.GetNodeAtPath(
            ["elm-packages", "elm-community", "result-extra", "tests", "Main.elm"])
            .Should().BeNull();
    }

    [Fact]
    public void Test_compilation_loads_dependencies()
    {
        var appCodeTree =
            FileTree.FromSetOfFilesWithStringPath(
                [
                (new[] { "elm.json" },
                (ReadOnlyMemory<byte>)
                """
                {
                    "type": "application",
                    "source-directories": [ "src" ],
                    "elm-version": "0.19.1",
                    "dependencies": {
                        "direct": {
                            "first-author/first-package": "1.0.0",
                            "second-author/second-package": "2.0.0"
                        },
                        "indirect": {}
                    },
                    "test-dependencies": {
                        "direct": {},
                        "indirect": {}
                    }
                }
                """u8.ToArray())
                ]);

        var loadedPackages = new List<(string packageName, string version)>();

        IReadOnlyDictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>> LoadPackage(
            string packageName,
            string version)
        {
            loadedPackages.Add((packageName, version));

            return
                new Dictionary<IReadOnlyList<string>, ReadOnlyMemory<byte>>(
                    EnumerableExtensions.EqualityComparer<IReadOnlyList<string>>())
                {
                    [["elm.json"]] =
                    Encoding.UTF8.GetBytes(
                        $$"""
                        {
                            "type": "package",
                            "name": "{{packageName}}",
                            "summary": "",
                            "license": "BSD-3-Clause",
                            "version": "{{version}}",
                            "exposed-modules": [],
                            "elm-version": "0.19.0 <= v < 0.20.0",
                            "dependencies": {},
                            "test-dependencies": {}
                        }
                        """)
                };
        }

        var packages =
            ElmTestRunner.LoadPackagesForTestCompilation(appCodeTree, LoadPackage);

        loadedPackages.Should().BeEquivalentTo(
            [
            ("first-author/first-package", "1.0.0"),
            ("second-author/second-package", "2.0.0"),
            ]);

        packages.Keys.Should().BeEquivalentTo(
            ["first-author/first-package", "second-author/second-package"]);
    }


    [Theory]
    [InlineData(null, 3)]
    [InlineData("*", 3)]
    [InlineData("Another Test Title", 2)]
    [InlineData("Tests/Group Title/Test Title", 1)]
    public void Each_test_uses_a_separate_vm_and_shared_caches(string? filter, int expectedTestCount)
    {
        var testCasesDirectory =
            TestResultSummary.FindTestDataDirectory(
                Path.Combine("Elm", "CommandElmTest"));

        var appDirectory =
            Path.Combine(
                testCasesDirectory,
                "single-suite-three-equal-all-pass",
                "input-app");

        var factoryCalls = 0;
        var discoveredTestCount = 0;
        var sharedCaches = new ConcurrentBag<object>();
        var workersObservedDiscovery = new ConcurrentBag<bool>();
        var selectedTestCount = 0;
        var filteredOutTestCount = 0;

        var testRun =
            ElmTestRunner.CompileAndRunTests(
                appDirectory: appDirectory,
                workers: 1,
                filter: filter,
                pineVmFactory:
                (_, caches) =>
                {
                    Interlocked.Increment(ref factoryCalls);
                    sharedCaches.Add(caches);

                    workersObservedDiscovery.Add(
                        Volatile.Read(ref discoveredTestCount) == expectedTestCount &&
                        Volatile.Read(ref selectedTestCount) == expectedTestCount &&
                        Volatile.Read(ref filteredOutTestCount) == 3 - expectedTestCount);

                    return ElmCompilerTestHelper.PineVMForProfiling(_ => { });
                },
                onTestsDiscovered:
                testCount =>
                Interlocked.Exchange(ref discoveredTestCount, testCount),
                onTestsSelected:
                (testCount, filteredOutCount) =>
                {
                    Interlocked.Exchange(ref selectedTestCount, testCount);
                    Interlocked.Exchange(ref filteredOutTestCount, filteredOutCount);
                });

        var completed = testRun.Should().BeOfType<ElmTestRun.Completed>().Which;
        completed.CompilationDuration.Should().BeGreaterThan(TimeSpan.Zero);
        completed.PreparationVmDuration.Should().BeGreaterThan(TimeSpan.Zero);
        completed.PreparationDuration.Should().BeGreaterThanOrEqualTo(completed.PreparationVmDuration);
        completed.ExecutionVmDuration.Should().BeGreaterThan(TimeSpan.Zero);
        completed.ExecutionDuration.Should().BeGreaterThanOrEqualTo(completed.ExecutionVmDuration);

        completed.Duration.Should().Be(
            completed.CompilationDuration + completed.PreparationDuration + completed.ExecutionDuration);

        factoryCalls.Should().Be(expectedTestCount);
        sharedCaches.Distinct(ReferenceEqualityComparer.Instance).Should().HaveCount(1);
        workersObservedDiscovery.Should().OnlyContain(observed => observed);
    }

    [Theory]
    [InlineData("module Tests.Elm.TypeInference.Helpers exposing (identity)\nidentity value = value")]
    [InlineData("module Tests.Elm.TypeInference.Helpers exposing (Kind(..))\ntype Kind = Kind")]
    [InlineData("module Tests.Elm.TypeInference.Helpers exposing (identity)\nidentity value = value\nprivateValue = 42")]
    public void Test_helpers_without_exposed_zero_parameter_declarations_need_not_be_compiled(string helperSource)
    {
        using var project =
            new ElmFuzzTests.Project(
                "suite = Test.test \"passes\" (\\_ -> Expect.equal 1 1)");

        File.WriteAllText(Path.Combine(project.Directory, "tests", "Helpers.elm"), helperSource);

        var run = ElmTestRunner.CompileAndRunTests(project.Directory, listTests: true);

        run.Should().BeOfType<ElmTestRun.Listed>().Which.Tests
            .Should().ContainSingle().Which.Name.Should().Be("passes");
    }

    [Fact]
    public void Test_discovery_keeps_helper_dependencies_and_ignores_non_test_values()
    {
        using var project =
            new ElmFuzzTests.Project(
                "suite = Test.test \"passes\" (\\_ -> Expect.equal (Helpers.identity 1) 1)");

        File.WriteAllText(
            Path.Combine(project.Directory, "tests", "Tests.elm"),
            File.ReadAllText(Path.Combine(project.Directory, "tests", "Tests.elm"))
            .Replace("import Test\n", "import Test\nimport Helpers\n"));

        File.WriteAllText(
            Path.Combine(project.Directory, "tests", "Helpers.elm"),
            "module Helpers exposing (identity)\nidentity value = value");

        File.WriteAllText(
            Path.Combine(project.Directory, "tests", "Constants.elm"),
            "module Constants exposing (answer)\nanswer = 42");

        var run = ElmTestRunner.CompileAndRunTests(project.Directory);

        run.Should().BeOfType<ElmTestRun.Completed>().Which.Tests
            .Should().ContainSingle().Which.Kind.Should().Be(CompletedTestKind.Passed);
    }

    [Fact]
    public void Filtered_listing_preserves_excluded_test_metadata()
    {
        var testCasesDirectory =
            TestResultSummary.FindTestDataDirectory(
                Path.Combine("Elm", "CommandElmTest"));

        var appDirectory =
            Path.Combine(
                testCasesDirectory,
                "single-suite-three-equal-all-pass",
                "input-app");

        var testRun =
            ElmTestRunner.CompileAndRunTests(
                appDirectory,
                filter: "Tests/Group Title/Test Title",
                listTests: true);

        var listed = testRun.Should().BeOfType<ElmTestRun.Listed>().Subject;
        listed.Tests.Should().ContainSingle().Which.Name.Should().Be("Test Title");

        listed.FilteredOutTests.Select(test => test.Name).Should().Equal(
            "Another Test Title",
            "Yet Another Test Title");

        listed.FilteredOutTests.Should().OnlyContain(
            test => test.FilePath == "tests/Tests.elm" && test.DescriptionPath.SequenceEqual(new[] { "Group Title" }));
    }


    [Fact]
    public void No_matching_tests_returns_suggestions_without_starting_workers()
    {
        var testCasesDirectory =
            TestResultSummary.FindTestDataDirectory(
                Path.Combine("Elm", "CommandElmTest"));

        var appDirectory =
            Path.Combine(
                testCasesDirectory,
                "single-suite-three-equal-all-pass",
                "input-app");

        var discoveryCallbackCalled = false;
        const string filter = "tests/**/missing test name";

        var testRun =
            ElmTestRunner.CompileAndRunTests(
                appDirectory: appDirectory,
                workers: 2,
                pineVmFactory: (_, _) => throw new InvalidOperationException("No worker should be started."),
                filter: filter,
                onTestsDiscovered: _ => discoveryCallbackCalled = true,
                onTestsSelected: (_, _) => discoveryCallbackCalled = true);

        var noMatches = testRun.Should().BeOfType<ElmTestRun.NoMatchingTests>().Subject;
        noMatches.Filter.Should().Be(filter);
        noMatches.ClosestTests.Should().HaveCount(3);
        noMatches.FilteredOutTests.Should().HaveCount(3);

        noMatches.ClosestTests.Should().OnlyContain(
            test => test.FullPath.StartsWith("tests/", StringComparison.Ordinal));

        discoveryCallbackCalled.Should().BeFalse();
    }


    [Fact]
    public void Evaluation_failure_fails_only_the_individual_test()
    {
        var testCasesDirectory =
            TestResultSummary.FindTestDataDirectory(
                Path.Combine("Elm", "CommandElmTest"));

        var appDirectory =
            Path.Combine(
                testCasesDirectory,
                "single-suite-three-equal-all-pass",
                "input-app");

        var testRun =
            ElmTestRunner.CompileAndRunTests(
                appDirectory,
                new FirstEvaluationFailsPineVm());

        var completed =
            testRun.Should().BeOfType<ElmTestRun.Completed>().Subject;

        completed.Tests.Should().HaveCount(3);
        completed.Tests.Count(test => test.Kind is CompletedTestKind.Failed).Should().Be(1);
        completed.Tests.Count(test => test.Kind is CompletedTestKind.Passed).Should().Be(2);

        completed.Tests
            .Single(test => test.Kind is CompletedTestKind.Failed)
            .Failure.Should().Be(
            new MessageFailure(
                "Failed evaluating test: Invocation count limit exceeded: 10_000_000"));
    }


    [Fact]
    public void Verify_elm_test_results_for_scenarios_from_files()
    {
        var results =
            TestResultSummary.RunFileBasedTestCases(
                Path.Combine("Elm", "CommandElmTest"),
                RunTestCase,
                trimWhitespace: true);

        var summary = TestResultSummary.RenderSummary(results);

        results.Where(result => !result.Passed).Should().BeEmpty(summary);
    }


    private static (string expected, string actual) RunTestCase(string caseDirectory)
    {
        var expectationFiles =
            Directory.GetFiles(caseDirectory, "expected-*.txt", SearchOption.TopDirectoryOnly)
            .Order(StringComparer.Ordinal)
            .ToArray();

        if (expectationFiles.Length is 0)
        {
            throw new InvalidOperationException(
                "Expected at least one expectation file in " + caseDirectory);
        }

        var testRun =
            ElmTestRunner.CompileAndRunTests(
                Path.Combine(caseDirectory, "input-app"),
                ElmCompilerTestHelper.PineVMForProfiling(_ => { }));

        if (testRun is not ElmTestRun.Completed completed)
            throw new InvalidOperationException("Expected a completed Elm test run, got " + testRun.GetType());

        var expectedSections = new List<string>();
        var actualSections = new List<string>();

        foreach (var expectationFile in expectationFiles)
        {
            var expectationFileName = Path.GetFileName(expectationFile);

            var includeTestDetails =
                expectationFileName switch
                {
                    "expected-without-details.txt" => false,
                    "expected-with-test-details.txt" => true,

                    _ =>
                    throw new InvalidOperationException(
                        "Unsupported expectation file name: " + expectationFileName)
                };

            expectedSections.Add(
                expectationFileName + "\n" +
                NormalizeExpectedOutput(File.ReadAllText(expectationFile)));

            actualSections.Add(
                expectationFileName + "\n" +
                ElmTestRunner.RenderTestResults(
                    completed.Tests,
                    includeTestDetails)
                .PlainText);
        }

        return
            (expected: string.Join("\n\n", expectedSections),
            actual: string.Join("\n\n", actualSections));
    }


    private static string NormalizeExpectedOutput(string output)
    {
        var lines =
            output
            .Replace("\r\n", "\n", StringComparison.Ordinal)
            .Replace('\r', '\n')
            .Split('\n')
            .Where(line => !line.StartsWith("Duration:", StringComparison.Ordinal));

        return string.Join('\n', lines).Trim();
    }


    private sealed class FirstEvaluationFailsPineVm : IPineVM
    {
        private int evaluationCount;

        public Result<string, PineValue> EvaluateExpression(
            Expression expression,
            PineValue environment)
        {
            if (Interlocked.Increment(ref evaluationCount) is 1)
                return "Invocation count limit exceeded: 10_000_000";

            return
                PineValue.List(
                    [
                    ElmValueEncoding.TagAsPineValue(
                        "Pass",
                        [
                        ElmValueEncoding.ElmRecordAsPineValue(
                            [
                            ("distributionReport", ElmValueEncoding.TagAsPineValue("NoDistribution", [])),
                            ("fuzzDetails", ElmValueEncoding.TagAsPineValue("Nothing", [])),
                            ])
                        ])
                    ]);
        }
    }
}
