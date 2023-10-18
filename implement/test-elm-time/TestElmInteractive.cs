using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using System;
using System.Collections.Immutable;
using System.IO;
using System.Linq;

namespace TestElmTime;

[TestClass]
public class TestElmInteractive
{
    private static string PathToCoreScenariosDirectory => @"./../../../../test-and-train/elm-interactive-scenarios-core";

    private static string PathToKernelScenariosDirectory => @"./../../../../test-and-train/elm-interactive-scenarios-kernel";

    [TestMethod]
    public void TestElmInteractiveScenarios()
    {
        using var dynamicPGOShare = new Pine.PineVM.DynamicPGOShare();

        var console = (IConsole)StaticConsole.Instance;

        var scenarios =
            new[]
            {
                PathToCoreScenariosDirectory,
                PathToKernelScenariosDirectory
            }
            .SelectMany(Directory.EnumerateDirectories)
            .SelectMany(scenarioDirectory =>
            {
                var scenarioName = Path.GetFileName(scenarioDirectory);

                if (!Directory.EnumerateFiles(scenarioDirectory, "*", searchOption: SearchOption.AllDirectories).Any())
                {
                    // Do not stumble over empty directory here. It could be a leftover after git checkout.
                    return [];
                }

                return ImmutableList.Create((scenarioName, scenarioDirectory));
            })
            .ToImmutableList();

        var scenariosTree =
            TreeNodeWithStringPath.SortedTree(
                scenarios
                .Select(scenario =>
                (name: scenario.scenarioName,
                component: LoadFromLocalFilesystem.LoadSortedTreeFromPath(scenario.scenarioDirectory)!))
                .ToImmutableList());

        var parsedScenarios =
            ElmTime.ElmInteractive.TestElmInteractive.ParseElmInteractiveScenarios(scenariosTree, console);

        ElmTime.ElmInteractive.IInteractiveSession newInteractiveSessionFromAppCode(TreeNodeWithStringPath? appCodeTree) =>
            new ElmTime.ElmInteractive.InteractiveSessionPine(
                compileElmProgramCodeFiles: ElmTime.ElmInteractive.IInteractiveSession.CompileElmProgramCodeFilesDefault.Value,
                appCodeTree: appCodeTree,
                dynamicPGOShare.GetVMAutoUpdating());

        {
            var warmupStopwatch = System.Diagnostics.Stopwatch.StartNew();

            using var session = newInteractiveSessionFromAppCode(null);

            session.Submit("1 + 3");

            console.WriteLine(
                "Warmup completed in " +
                warmupStopwatch.Elapsed.TotalSeconds.ToString("0.##") + " seconds.");
        }

        var scenariosResults =
            ElmTime.ElmInteractive.TestElmInteractive.TestElmInteractiveScenarios(
                parsedScenarios.NamedDistinctScenarios,
                namedScenario => namedScenario.Value,
                newInteractiveSessionFromAppCode);

        var allSteps =
            scenariosResults
            .SelectMany(scenario => scenario.Value.StepsReports.Select(step => (scenario, step)))
            .ToImmutableList();

        var passedSteps =
            allSteps.Where(step => step.step.result.IsOk()).ToImmutableList();

        var failedSteps =
            allSteps.Where(step => !step.step.result.IsOk()).ToImmutableList();

        var failedScenarios =
            failedSteps
            .GroupBy(failedStep => failedStep.scenario.Key.Key)
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
                    "Failed step '" + failedStep.step.name + "':\n" +
                    failedStep.step.result.Unpack(fromErr: err => err, fromOk: _ => throw new NotImplementedException()).errorAsText,
                    color: IConsole.TextColor.Red);
            }
        }

        if (!failedScenarios.IsEmpty)
        {
            throw new Exception(
                "Failed for " + failedScenarios.Count + " scenarios:\n" +
                string.Join("\n", failedScenarios.Select(scenarioNameAndResult => scenarioNameAndResult.Key)));
        }
    }
}
