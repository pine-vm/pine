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

    public static ImmutableDictionary<TContainer, InteractiveScenarioTestReport> TestElmInteractiveScenarios<TContainer>(
        IReadOnlyCollection<TContainer> scenarioContainers,
        Func<TContainer, TreeNodeWithStringPath> getScenario,
        ElmEngineType implementationType) where TContainer : notnull =>
        scenarioContainers
        .AsParallel()
        .WithDegreeOfParallelism(3)
        .Select(scenarioContainer =>
        (scenarioContainer, testReport: TestElmInteractiveScenario(getScenario(scenarioContainer), implementationType)))
        .ToImmutableDictionary(
            s => s.scenarioContainer,
            elementSelector: s => s.testReport);

    public static InteractiveScenarioTestReport TestElmInteractiveScenario(
        TreeNodeWithStringPath scenarioTree,
        ElmEngineType implementationType)
    {
        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var parsedScenario =
            ParseScenario(scenarioTree)
            .Extract(err => throw new Exception("Failed parsing scenario: " + err));

        using var interactiveSession = IInteractiveSession.Create(appCodeTree: parsedScenario.appCodeTree, implementationType);

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

                        if (sessionStep.step.expectedResponse is string expectedResponse)
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
            scenarioTree.GetNodeAtPath(new[] { "context-app" });

        var stepsDirectory =
            scenarioTree.GetNodeAtPath(new[] { "steps" });

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
            sessionStep.GetNodeAtPath(new[] { "expected-value.txt" }) is TreeNodeWithStringPath.BlobNode expectedValueBlob
            ?
            Encoding.UTF8.GetString(expectedValueBlob.Bytes.Span)
            :
            null;

        return
            (sessionStep.GetNodeAtPath(new[] { "submission.txt" }) switch
            {
                TreeNodeWithStringPath.BlobNode submissionBlob => Result<string, string>.ok(Encoding.UTF8.GetString(submissionBlob.Bytes.Span)),
                _ => Result<string, string>.err("Missing submission"),
            })
            .Map(submission => new ScenarioStep(submission, expectedResponse));
    }
}
