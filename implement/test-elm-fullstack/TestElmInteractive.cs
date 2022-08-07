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
        var console = (IConsole)StaticConsole.Instance;

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
                scenario => LoadFromLocalFilesystem.LoadSortedTreeFromPath(scenario.scenarioDirectory)!,
                elm_fullstack.ElmInteractive.ElmEngineType.JavaScript);

        var allSteps =
            scenariosResults
            .SelectMany(scenario => scenario.Value.stepsReports.Select(step => (scenario, step)))
            .ToImmutableList();

        var passedSteps =
            allSteps.Where(step => step.step.result.IsOk()).ToImmutableList();

        var failedSteps =
            allSteps.Where(step => !step.step.result.IsOk()).ToImmutableList();

        var failedScenarios =
            failedSteps
            .GroupBy(failedStep => failedStep.scenario.Key.scenarioName)
            .ToImmutableSortedDictionary(
                keySelector: failedScenario => failedScenario.Key,
                elementSelector: failedScenario => failedScenario);

        console.WriteLine("Total scenarios: " + scenariosResults.Count);
        console.WriteLine("Total steps: " + allSteps.Count);
        console.WriteLine("Passed scenarios: " + scenariosResults.Values.Count(scenarioResult => scenarioResult.Passed));
        console.WriteLine("Passed steps: " + passedSteps.Count);

        foreach (var failedScenario in failedScenarios)
        {
            var scenarioId = failedScenario.Value.Key;

            console.WriteLine(
                "Failed " + failedScenario.Value.Count() + " step(s) in scenario " + scenarioId + ":",
                color: IConsole.TextColor.Red);

            foreach (var failedStep in failedScenario.Value)
            {
                console.WriteLine(
                    "Failed step '" + failedStep.step.name + "':\n" + failedStep.step.result?.Err?.errorAsText!,
                    color: IConsole.TextColor.Red);
            }
        }

        if (failedScenarios.Any())
        {
            throw new Exception(
                "Failed for " + failedScenarios.Count + " scenarios:\n" +
                string.Join("\n", failedScenarios.Select(scenarioNameAndResult => scenarioNameAndResult.Key)));
        }
    }
}
