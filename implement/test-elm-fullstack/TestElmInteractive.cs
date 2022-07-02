using System;
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

    [TestMethod]
    public void TestElmInteractiveScenarios()
    {
        var scenarios =
            Directory.EnumerateDirectories(pathToScenariosDirectory)
            .SelectMany(scenarioDirectory =>
            {
                var scenarioName = Path.GetFileName(scenarioDirectory);

                if (!Directory.EnumerateFiles(scenarioDirectory, "*", searchOption: SearchOption.AllDirectories).Any())
                {
                    // Do not stumble over empty directory here. It could be a leftover after git checkout.
                    return ImmutableList<(string scenarioName, string scenarioDirectory)>.Empty;
                }

                return ImmutableList.Create((scenarioName, scenarioDirectory));
            })
            .ToImmutableList();

        var scenariosResults =
            elm_fullstack.ElmInteractive.TestElmInteractive.TestElmInteractiveScenarios(
                scenarios,
                scenario => LoadFromLocalFilesystem.LoadSortedTreeFromPath(scenario.scenarioDirectory)!);

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

            Console.WriteLine("Scenario '" + scenarioNameAndResult.Key.scenarioName + "' failed with " + causeText);
        }

        if (failedScenarios.Any())
        {
            throw new Exception(
                "Failed for " + failedScenarios.Count + " scenarios:\n" +
                string.Join("\n", failedScenarios.Select(scenarioNameAndResult => scenarioNameAndResult.Key.scenarioName)));
        }
    }
}
