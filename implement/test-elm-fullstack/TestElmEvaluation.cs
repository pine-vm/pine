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
    public class TestElmEvaluation
    {
        static string pathToScenariosDirectory => @"./../../../elm-evaluation-scenarios";

        class EvaluationScenarioTestResult
        {
            public bool passed;

            public Exception exception;
        }

        [TestMethod]
        public void TestElmEvaluationScenarios()
        {
            var scenariosResults = new Dictionary<string, EvaluationScenarioTestResult>();

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
                    var appCodeTree = LoadFromLocalFilesystem.LoadSortedTreeFromPath(Path.Combine(scenarioDirectory, "context-app"));

                    var expression =
                        File.ReadAllText(Path.Combine(scenarioDirectory, "expression"), System.Text.Encoding.UTF8);

                    var expectedValueJson =
                        File.ReadAllText(
                            Path.Combine(scenarioDirectory, "expected-value.json"),
                            System.Text.Encoding.UTF8);

                    var evalResult =
                        elm_fullstack.ElmEngine.EvaluateElm.EvaluateSubmissionAndGetResultingValueJsonString(
                            appCodeTree: appCodeTree,
                            submission: expression,
                            previousLocalSubmissions: null);

                    Assert.AreEqual(
                        expectedValueJson,
                        evalResult.Ok,
                        "Value from evaluation matches expected value in scenario '" + scenarioName + "'");

                    scenarioPassed = true;
                }
                catch (Exception e)
                {
                    scenarioException = e;
                }

                scenariosResults[scenarioName] =
                    new EvaluationScenarioTestResult
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
