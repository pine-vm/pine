using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace ElmTime.ElmInteractive;

public class TestElmInteractive
{
    public record Scenario(
        TreeNodeWithStringPath? appCodeTree,
        IReadOnlyList<(string stepName, ScenarioStep step)> steps);

    public record ScenarioStep(
        string submission,
        string? expectedResponse);

    public record InteractiveScenarioTestReport(
        Scenario scenario,
        ImmutableList<(string name, Result<InteractiveScenarioTestStepFailure, object> result)> stepsReports,
        TimeSpan elapsedTime)
    {
        public bool Passed => stepsReports.All(s => s.result.IsOk());
    }

    public record InteractiveScenarioTestStepFailure(
        string submission,
        string errorAsText);

    public static ImmutableDictionary<string, InteractiveScenarioTestReport> TestElmInteractiveScenarios(
        TreeNodeWithStringPath scenariosTree,
        Func<TreeNodeWithStringPath?, IInteractiveSession> interactiveSessionFromAppCode,
        IConsole console)
    {
        var scenariosTreeComposition =
            PineValueComposition.FromTreeWithStringPath(scenariosTree);

        var scenariosTreeCompositionHash =
            CommonConversion.StringBase16(PineValueHashTree.ComputeHash(scenariosTreeComposition));

        var namedDistinctScenarios =
            scenariosTree switch
            {
                TreeNodeWithStringPath.TreeNode treeNode => treeNode.Elements,

                _ => throw new InvalidOperationException("Unexpected scenarios tree type: " + scenariosTree.GetType().FullName),
            };

        console.WriteLine(
            "Successfully loaded " + namedDistinctScenarios.Count +
            " distinct scenario(s) from composition " + scenariosTreeCompositionHash + ".");

        var exceptLoadingStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var scenariosResults =
            TestElmInteractiveScenarios(
                namedDistinctScenarios,
                namedScenario => namedScenario.component,
                interactiveSessionFromAppCode);

        var allSteps =
            scenariosResults
            .SelectMany(scenario => scenario.Value.stepsReports.Select(step => (scenario, step)))
        .ToImmutableList();

        console.WriteLine(scenariosResults.Count + " scenario(s) resulted in " + allSteps.Count + " steps.");

        var passedSteps =
            allSteps.Where(step => step.step.result.IsOk()).ToImmutableList();

        var failedSteps =
            allSteps.Where(step => !step.step.result.IsOk()).ToImmutableList();

        var aggregateDuration =
            scenariosResults
            .Select(scenario => scenario.Value.elapsedTime)
            .Aggregate(seed: TimeSpan.Zero, func: (aggregate, scenarioTime) => aggregate + scenarioTime);

        var overallStats = new[]
        {
            (label : "Failed", value : failedSteps.Count.ToString()),
            (label : "Passed", value : passedSteps.Count.ToString()),
            (label : "Total", value : allSteps.Count.ToString()),
            (label : "Duration", value : CommandLineInterface.FormatIntegerForDisplay((long)aggregateDuration.TotalMilliseconds) + " ms"),
        };

        console.WriteLine(
            string.Join(
                " - ",
                (!failedSteps.IsEmpty ? "Failed" : "Passed") + "!",
                string.Join(", ", overallStats.Select(stat => stat.label + ": " + stat.value)),
                scenariosTreeCompositionHash[..10] + " (elm-time " + Program.AppVersionId + ")"),
            color: !failedSteps.IsEmpty ? IConsole.TextColor.Red : IConsole.TextColor.Green);

        var failedScenarios =
            failedSteps
            .GroupBy(failedStep => failedStep.scenario.Key.name)
            .ToImmutableSortedDictionary(
                keySelector: failedScenario => failedScenario.Key,
                elementSelector: failedScenario => failedScenario);

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
                    failedStep.step.result.Unpack(fromErr: error => error, fromOk: _ => throw new Exception()).errorAsText,
                    color: IConsole.TextColor.Red);
            }
        }

        return
            scenariosResults
            .ToImmutableDictionary(
                i => i.Key.name,
                i => i.Value);
    }

    public static ImmutableDictionary<TContainer, InteractiveScenarioTestReport> TestElmInteractiveScenarios<TContainer>(
        IReadOnlyCollection<TContainer> scenarioContainers,
        Func<TContainer, TreeNodeWithStringPath> getScenario,
        Func<TreeNodeWithStringPath?, IInteractiveSession> interactiveSessionFromAppCode) where TContainer : notnull =>
        scenarioContainers
        .AsParallel()
        .WithDegreeOfParallelism(3)
        .Select(scenarioContainer =>
        (scenarioContainer, testReport: TestElmInteractiveScenario(getScenario(scenarioContainer), interactiveSessionFromAppCode)))
        .ToImmutableDictionary(
            s => s.scenarioContainer,
            elementSelector: s => s.testReport);

    public static InteractiveScenarioTestReport TestElmInteractiveScenario(
        TreeNodeWithStringPath scenarioTree,
        Func<TreeNodeWithStringPath?, IInteractiveSession> interactiveSessionFromAppCode)
    {
        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var parsedScenario =
            ParseScenario(scenarioTree)
            .Extract(err => throw new Exception("Failed parsing scenario: " + err));

        using var interactiveSession = interactiveSessionFromAppCode(parsedScenario.appCodeTree);

        var stepsReports =
            parsedScenario.steps
            .Select(sessionStep =>
            {
                Result<InteractiveScenarioTestStepFailure, object> getResult()
                {
                    try
                    {
                        var evalResult = interactiveSession.Submit(sessionStep.step.submission);

                        var evalOk =
                        evalResult
                        .Extract(evalError => throw new AssertFailedException("Submission result has error: " + evalError));

                        if (sessionStep.step.expectedResponse is { } expectedResponse)
                        {
                            if (expectedResponse != evalOk.interactiveResponse?.displayText)
                            {
                                var errorText =
                                "Response from interactive does not match expected value. Expected:\n" +
                                expectedResponse +
                                "\nBut got this response:\n" +
                                evalOk.interactiveResponse?.displayText;

                                return Result<InteractiveScenarioTestStepFailure, object>.err(
                                    new InteractiveScenarioTestStepFailure(
                                        submission: sessionStep.step.submission,
                                        errorAsText: errorText));
                            }
                        }
                    }
                    catch (Exception e)
                    {
                        return Result<InteractiveScenarioTestStepFailure, object>.err(
                            new InteractiveScenarioTestStepFailure(
                                submission: sessionStep.step.submission,
                                errorAsText: "Runtime exception:\n" + e));
                    }

                    return Result<InteractiveScenarioTestStepFailure, object>.ok(new object());
                }

                return (sessionStep.stepName, getResult());
            })
            .ToImmutableList();

        return new InteractiveScenarioTestReport(
            scenario: parsedScenario,
            stepsReports: stepsReports,
            elapsedTime: totalStopwatch.Elapsed);
    }

    public static Result<string, Scenario> ParseScenario(TreeNodeWithStringPath scenarioTree)
    {
        var appCodeTree =
            scenarioTree.GetNodeAtPath(["context-app"]);

        var stepsDirectory =
            scenarioTree.GetNodeAtPath(["steps"]);

        var testScenarioSteps =
            stepsDirectory switch
            {
                null => throw new Exception(nameof(stepsDirectory) + " is null"),
                not null => stepsDirectory.Map(
                    fromBlob: _ => throw new Exception(nameof(stepsDirectory) + " is blob"),
                    fromTree: tree =>
                    0 < tree.Count
                    ?
                    tree
                    :
                    throw new Exception("Found no stepsDirectories"))
            };

        var stepsNames = testScenarioSteps.Select(s => s.itemName).ToImmutableList();

        if (!stepsNames.Order().SequenceEqual(stepsNames.OrderByNatural()))
        {
            return Result<string, Scenario>.err(
                "Ambiguous sort order of steps (" + string.Join(", ", stepsNames) + "). Rename these steps to make the ordering obvious");
        }

        return
            testScenarioSteps
            .Select(sessionStep =>
            {
                var stepName = sessionStep.itemName;

                return
                ParseScenarioStep(sessionStep.itemValue)
                .MapError(err => "Failed to parse step " + stepName + ": " + err)
                .Map(parsedStep => (stepName, parsedStep));
            })
            .ListCombine()
            .Map(steps => new Scenario(appCodeTree: appCodeTree, steps: steps));
    }

    public static Result<string, ScenarioStep> ParseScenarioStep(TreeNodeWithStringPath sessionStep)
    {
        var expectedResponse =
            sessionStep.GetNodeAtPath(["expected-value.txt"]) is TreeNodeWithStringPath.BlobNode expectedValueBlob
            ?
            Encoding.UTF8.GetString(expectedValueBlob.Bytes.Span)
            :
            null;

        return
            (sessionStep.GetNodeAtPath(["submission.txt"]) switch
            {
                TreeNodeWithStringPath.BlobNode submissionBlob => Result<string, string>.ok(Encoding.UTF8.GetString(submissionBlob.Bytes.Span)),
                _ => Result<string, string>.err("Missing submission"),
            })
            .Map(submission => new ScenarioStep(submission, expectedResponse));
    }
}
