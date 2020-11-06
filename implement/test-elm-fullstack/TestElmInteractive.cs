using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using Kalmit;
using Microsoft.VisualStudio.TestTools.UnitTesting;

namespace test_elm_fullstack
{
    [TestClass]
    public class TestElmInteractive
    {
        static string pathToScenariosDirectory => @"./../../../elm-interactive-scenarios";

        class InteractiveScenarioTestResult
        {
            public bool passed;

            public Exception exception;
        }

        [TestMethod]
        public void TestElmInteractiveScenarios()
        {
            var scenariosResults = new Dictionary<string, InteractiveScenarioTestResult>();

            foreach (var scenarioDirectory in Directory.EnumerateDirectories(pathToScenariosDirectory))
            {
                var scenarioName = Path.GetFileName(scenarioDirectory);

                if (Directory.EnumerateFiles(scenarioDirectory, "*", searchOption: SearchOption.AllDirectories).Take(1).Count() < 1)
                {
                    // Do not stumble over empty directory here. It could be a leftover after git checkout.
                    continue;
                }

                bool scenarioPassed = false;
                Exception scenarioException = null;

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

                    if (stepsDirectories.Count < 1)
                        throw new Exception("stepsDirectories.Count < 1");

                    using (var interactiveSession = new elm_fullstack.InteractiveSession(appCodeTree: appCodeTree))
                    {
                        foreach (var stepDirectory in stepsDirectories)
                        {
                            var stepName = Path.GetFileName(stepDirectory);

                            try
                            {
                                var submission =
                                    File.ReadAllText(Path.Combine(stepDirectory, "submission"), System.Text.Encoding.UTF8);

                                var evalResult =
                                    interactiveSession.SubmitAndGetResultingValue(submission);

                                var expectedValueFilePath = Path.Combine(stepDirectory, "expected-value");

                                if (File.Exists(expectedValueFilePath))
                                {
                                    var expectedValue = File.ReadAllText(expectedValueFilePath, System.Text.Encoding.UTF8);

                                    Assert.AreEqual(
                                        expectedValue,
                                        evalResult.Ok?.valueAsElmExpressionText,
                                        "Value from evaluation matches expected value in scenario '" + scenarioName + "'");
                                }
                            }
                            catch (Exception e)
                            {
                                throw new Exception("Failed step '" + stepName + "' with exception.", e);
                            }
                        }
                    }

                    scenarioPassed = true;
                }
                catch (Exception e)
                {
                    scenarioException = e;
                }

                scenariosResults[scenarioName] =
                    new InteractiveScenarioTestResult
                    {
                        passed = scenarioPassed,
                        exception = scenarioException,
                    };
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

            if (0 < failedScenarios.Count)
            {
                throw new Exception(
                    "Failed for " + failedScenarios.Count + " scenarios:\n" +
                    string.Join("\n", failedScenarios.Select(scenarioNameAndResult => scenarioNameAndResult.Key)));
            }
        }
    }
}
