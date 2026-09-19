using AwesomeAssertions;
using Pine.Core.Elm;
using Pine.Core.Elm.Testing;
using Pine.Core.Files;
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


    [Fact]
    public void Each_test_uses_a_separate_vm_and_shared_caches()
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

        var testRun =
            ElmTestRunner.CompileAndRunTests(
                appDirectory,
                workers: 1,
                pineVmFactory:
                (_, caches) =>
                {
                    Interlocked.Increment(ref factoryCalls);
                    sharedCaches.Add(caches);
                    workersObservedDiscovery.Add(Volatile.Read(ref discoveredTestCount) is 3);

                    return ElmCompilerTestHelper.PineVMForProfiling(_ => { });
                },
                onTestsDiscovered:
                testCount =>
                Interlocked.Exchange(ref discoveredTestCount, testCount));

        testRun.Should().BeOfType<ElmTestRun.Completed>();
        factoryCalls.Should().Be(3);
        sharedCaches.Distinct(ReferenceEqualityComparer.Instance).Should().HaveCount(1);
        workersObservedDiscovery.Should().OnlyContain(observed => observed);
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


    private sealed class FirstEvaluationFailsPineVm : Pine.Core.PineVM.IPineVM
    {
        private int evaluationCount;

        public Result<string, PineValue> EvaluateExpression(
            Expression expression,
            PineValue environment)
        {
            if (Interlocked.Increment(ref evaluationCount) is 1)
                return "Invocation count limit exceeded: 10_000_000";

            return ElmValueEncoding.TagAsPineValue("Pass", []);
        }
    }
}
