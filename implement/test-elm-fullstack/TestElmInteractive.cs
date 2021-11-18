using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;

namespace test_elm_fullstack;

[TestClass]
public class TestElmInteractive
{
    static string pathToScenariosDirectory => @"./../../../elm-interactive-scenarios";

    record InteractiveScenarioTestResult(
        bool passed,
        Exception exception);

    [TestMethod]
    public void TestElmInteractiveScenarios()
    {
        var scenariosResults = new Dictionary<string, InteractiveScenarioTestResult>();

        foreach (var scenarioDirectory in Directory.EnumerateDirectories(pathToScenariosDirectory))
        {
            var scenarioName = Path.GetFileName(scenarioDirectory);

            if (!Directory.EnumerateFiles(scenarioDirectory, "*", searchOption: SearchOption.AllDirectories).Take(1).Any())
            {
                // Do not stumble over empty directory here. It could be a leftover after git checkout.
                continue;
            }

            scenariosResults[scenarioName] = TestElmInteractiveScenario(scenarioDirectory);
        }

        Console.WriteLine("Total scenarios: " + scenariosResults.Count);
        Console.WriteLine("Passed: " + scenariosResults.Values.Count(scenarioResult => scenarioResult.passed));

        var failedScenarios =
            scenariosResults
            .Where(scenarioNameAndResult => !scenarioNameAndResult.Value.passed)
            .ToImmutableList();

        foreach (var scenarioNameAndResult in failedScenarios)
        {
            var causeText =
                scenarioNameAndResult.Value.exception != null ?
                "exception:\n" + scenarioNameAndResult.Value.exception.ToString()
                :
                "unknown cause";

            Console.WriteLine("Scenario '" + scenarioNameAndResult.Key + "' failed with " + causeText);
        }

        if (failedScenarios.Any())
        {
            throw new Exception(
                "Failed for " + failedScenarios.Count + " scenarios:\n" +
                string.Join("\n", failedScenarios.Select(scenarioNameAndResult => scenarioNameAndResult.Key)));
        }
    }

    static InteractiveScenarioTestResult TestElmInteractiveScenario(string scenarioDirectory)
    {
        try
        {
            var appCodeTree =
                LoadFromLocalFilesystem.LoadSortedTreeFromPath(Path.Combine(scenarioDirectory, "context-app"));

            var stepsDirectories =
                Directory.EnumerateDirectories(
                    Path.Combine(scenarioDirectory, "steps"),
                    searchPattern: "*",
                    searchOption: SearchOption.TopDirectoryOnly)
                .OrderBy(directory => directory)
                .ToImmutableList();

            if (!stepsDirectories.Any())
                throw new Exception("Found no stepsDirectories");

            using (var interactiveSession = new elm_fullstack.ElmInteractive.InteractiveSession(appCodeTree: appCodeTree))
            {
                foreach (var stepDirectory in stepsDirectories)
                {
                    var stepName = Path.GetFileName(stepDirectory);

                    string submission = null;

                    try
                    {
                        submission =
                            File.ReadAllText(Path.Combine(stepDirectory, "submission"), System.Text.Encoding.UTF8);

                        var evalResult =
                            interactiveSession.SubmitAndGetResultingValue(submission);

                        var expectedValueFilePath = Path.Combine(stepDirectory, "expected-value");

                        if (File.Exists(expectedValueFilePath))
                        {
                            var expectedValue = File.ReadAllText(expectedValueFilePath, System.Text.Encoding.UTF8);

                            Assert.IsNull(evalResult.Err, "Submission result has error: " + evalResult.Err);

                            Assert.AreEqual(
                                expectedValue,
                                evalResult.Ok?.valueAsElmExpressionText,
                                "Value from evaluation does not match expected value.");
                        }
                    }
                    catch (Exception e)
                    {
                        throw new Exception("Failed step '" + stepName + "' with exception.\nSubmission in this step:\n" + submission, e);
                    }
                }
            }

            return new InteractiveScenarioTestResult(passed: true, exception: null);
        }
        catch (Exception e)
        {
            return new InteractiveScenarioTestResult(passed: false, exception: e);
        }
    }
}
