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
                    testRun.Tests,
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
