using AwesomeAssertions;
using Pine.Core.Elm.Testing;
using Pine.Core.Tests.Elm.ElmCompilerInDotnet;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
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
}
