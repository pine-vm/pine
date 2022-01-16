using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using static elm_fullstack.ElmInteractive.TestElmInteractive;

namespace test_elm_fullstack;

[TestClass]
public class TestElmInteractive
{
    static string pathToScenariosDirectory => @"./../../../elm-interactive-scenarios";

    [TestMethod]
    public void TestElmInteractiveScenarios()
    {
        var scenariosResults = new Dictionary<string, InteractiveScenarioTestResult>();

        foreach (var scenarioDirectory in Directory.EnumerateDirectories(pathToScenariosDirectory))
        {
            var scenarioName = Path.GetFileName(scenarioDirectory);

            if (!Directory.EnumerateFiles(scenarioDirectory, "*", searchOption: SearchOption.AllDirectories).Any())
            {
                // Do not stumble over empty directory here. It could be a leftover after git checkout.
                continue;
            }

            scenariosResults[scenarioName] = TestElmInteractiveScenario(scenarioDirectory);
        }

        Console.WriteLine("Total scenarios: " + scenariosResults.Count);
        Console.WriteLine("Passed: " + scenariosResults.Values.Count(scenarioResult => scenarioResult.Passed));

        var failedScenarios =
            scenariosResults
            .Where(scenarioNameAndResult => !scenarioNameAndResult.Value.Passed)
            .ToImmutableList();

        foreach (var scenarioNameAndResult in failedScenarios)
        {
            var causeText =
                scenarioNameAndResult.Value.Exception != null ?
                "exception:\n" + scenarioNameAndResult.Value.Exception.ToString()
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

    InteractiveScenarioTestResult TestElmInteractiveScenario(string scenarioDirectory) =>
        elm_fullstack.ElmInteractive.TestElmInteractive.TestElmInteractiveScenario(
            LoadFromLocalFilesystem.LoadSortedTreeFromPath(scenarioDirectory)!);
}
