using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace ElmTime.ElmInteractive;

public class TestElmInteractive
{
    public record ParsedScenarios(
        TreeNodeWithStringPath ScenariosTree,
        string ScenariosTreeCompositionHash,
        IReadOnlyDictionary<string, Scenario> NamedDistinctScenarios);

    public record Scenario(
        TreeNodeWithStringPath? AppCodeTree,
        IReadOnlyList<(string stepName, ScenarioStep step)> Steps);

    public record ScenarioStep(
        string Submission,
        string? ExpectedResponse);

    public record InteractiveScenarioTestReport(
        Scenario Scenario,
        ImmutableList<(string name, Result<InteractiveScenarioTestStepFailure, IInteractiveSession.SubmissionResponse> result)> StepsReports,
        TimeSpan ElapsedTime)
    {
        public bool Passed => StepsReports.All(s => s.result.IsOk());
    }

    public record InteractiveScenarioTestStepFailure(
        string submission,
        string errorAsText);

    public static ParsedScenarios ParseElmInteractiveScenarios(
        TreeNodeWithStringPath scenariosTree,
        IConsole console)
    {
        var scenariosTreeComposition =
            PineValueComposition.FromTreeWithStringPath(scenariosTree);

        var scenariosTreeCompositionHash =
            CommonConversion.StringBase16(PineValueHashTree.ComputeHash(scenariosTreeComposition));

        var namedDistinctScenarios =
            scenariosTree switch
            {
                TreeNodeWithStringPath.TreeNode treeNode =>
                treeNode.Elements
                .ToImmutableDictionary(
                    keySelector:
                    treeNode => treeNode.name,
                    elementSelector:
                    treeNode =>
                    ParseScenario(treeNode.component)
                    .Extract(err => throw new Exception("Failed parsing scenario: " + err))),

                _ => throw new InvalidOperationException("Unexpected scenarios tree type: " + scenariosTree.GetType().FullName),
            };

        console.WriteLine(
            "Successfully loaded " + namedDistinctScenarios.Count +
            " distinct scenario(s) from composition " + scenariosTreeCompositionHash + ".");

        return new ParsedScenarios(
            ScenariosTree: scenariosTree,
            ScenariosTreeCompositionHash: scenariosTreeCompositionHash,
            NamedDistinctScenarios: namedDistinctScenarios);
    }

    public static ImmutableDictionary<string, InteractiveScenarioTestReport> TestElmInteractiveScenarios(
        ParsedScenarios scenarios,
        IInteractiveSessionConfig interactiveConfig,
        IConsole console)
    {
        var exceptLoadingStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var scenariosResults =
            TestElmInteractiveScenarios(
                scenarios.NamedDistinctScenarios,
                namedScenario => namedScenario.Value,
                interactiveConfig.InteractiveSessionFromAppCode);

        var allSteps =
            scenariosResults
            .SelectMany(scenario => scenario.Value.StepsReports.Select(step => (scenario, step)))
        .ToImmutableList();

        console.WriteLine(scenariosResults.Count + " scenario(s) resulted in " + allSteps.Count + " steps.");

        var passedSteps =
            allSteps.Where(step => step.step.result.IsOk()).ToImmutableList();

        var failedSteps =
            allSteps.Where(step => !step.step.result.IsOk()).ToImmutableList();

        var aggregateDuration =
            scenariosResults
            .Select(scenario => scenario.Value.ElapsedTime)
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
                scenarios.ScenariosTreeCompositionHash[..10] +
                " (elm-time " + Program.AppVersionId + " with Elm compiler " + interactiveConfig.CompilerId + ")"),
            color: !failedSteps.IsEmpty ? IConsole.TextColor.Red : IConsole.TextColor.Green);

        var failedScenarios =
            failedSteps
            .GroupBy(failedStep => failedStep.scenario.Key.Key)
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
                i => i.Key.Key,
                i => i.Value);
    }

    public static ImmutableDictionary<TContainer, InteractiveScenarioTestReport> TestElmInteractiveScenarios<TContainer>(
        IReadOnlyCollection<TContainer> scenarioContainers,
        Func<TContainer, Scenario> getScenario,
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
        Scenario parsedScenario,
        Func<TreeNodeWithStringPath?, IInteractiveSession> interactiveSessionFromAppCode)
    {
        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        using var interactiveSession = interactiveSessionFromAppCode(parsedScenario.AppCodeTree);

        var stepsReports =
            parsedScenario.Steps
            .Select(sessionStep =>
            {
                Result<InteractiveScenarioTestStepFailure, IInteractiveSession.SubmissionResponse> getResult()
                {
                    try
                    {
                        var submissionResult =
                        interactiveSession.Submit(sessionStep.step.Submission)
                        .MapError(err => new InteractiveScenarioTestStepFailure(
                            submission: sessionStep.step.Submission,
                            errorAsText: "Submission result has error: " + err));

                        return
                        submissionResult
                        .AndThen(submissionResultOk =>
                        {
                            if (sessionStep.step.ExpectedResponse is { } expectedResponse)
                            {
                                if (expectedResponse != submissionResultOk.interactiveResponse?.displayText)
                                {
                                    var errorText =
                                    "Response from interactive does not match expected value. Expected:\n" +
                                    expectedResponse +
                                    "\nBut got this response:\n" +
                                    submissionResultOk.interactiveResponse?.displayText;

                                    return Result<InteractiveScenarioTestStepFailure, IInteractiveSession.SubmissionResponse>.err(
                                        new InteractiveScenarioTestStepFailure(
                                            submission: sessionStep.step.Submission,
                                            errorAsText: errorText));
                                }
                            }

                            return submissionResult;
                        });
                    }
                    catch (Exception e)
                    {
                        return Result<InteractiveScenarioTestStepFailure, IInteractiveSession.SubmissionResponse>.err(
                            new InteractiveScenarioTestStepFailure(
                                submission: sessionStep.step.Submission,
                                errorAsText: "Runtime exception:\n" + e));
                    }
                }

                return (sessionStep.stepName, getResult());
            })
            .ToImmutableList();

        return new InteractiveScenarioTestReport(
            Scenario: parsedScenario,
            StepsReports: stepsReports,
            ElapsedTime: totalStopwatch.Elapsed);
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
            .Map(steps => new Scenario(AppCodeTree: appCodeTree, Steps: steps));
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
