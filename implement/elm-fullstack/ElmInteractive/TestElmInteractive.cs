using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace ElmFullstack.ElmInteractive;

public class TestElmInteractive
{
    public record InteractiveScenarioTestReport(
        ImmutableList<(string name, Result<InteractiveScenarioTestStepFailure, object> result)> stepsReports,
        TimeSpan elapsedTime)
    {
        public bool Passed => stepsReports.All(s => s.result.IsOk());
    }

    public record InteractiveScenarioTestStepFailure(
        string submission,
        string errorAsText);

    static public ImmutableDictionary<TContainer, InteractiveScenarioTestReport> TestElmInteractiveScenarios<TContainer>(
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

    static public InteractiveScenarioTestReport TestElmInteractiveScenario(
        TreeNodeWithStringPath scenarioTree,
        ElmEngineType implementationType)
    {
        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var appCodeTree =
            scenarioTree.GetNodeAtPath(new[] { "context-app" });

        var stepsDirectory =
            scenarioTree.GetNodeAtPath(new[] { "steps" });

        if (stepsDirectory?.TreeContent == null)
            throw new Exception(nameof(stepsDirectory) + " is null");

        if (!stepsDirectory.TreeContent.Any())
            throw new Exception("Found no stepsDirectories");

        using var interactiveSession = IInteractiveSession.Create(appCodeTree: appCodeTree, implementationType);

        var testScenarioSteps = stepsDirectory.TreeContent;

        var stepsReports =
            testScenarioSteps
            .Select(sessionStep =>
            {
                var stepName = sessionStep.name;

                var (submission, expectedResponse) =
                ParseStep(sessionStep.component)
                .Extract(fromErr: error => throw new Exception(error));

                return new
                {
                    stepName,
                    submission,
                    expectedResponse
                };
            })
            .Select(sessionStep =>
            {

                Result<InteractiveScenarioTestStepFailure, object> getResult()
                {
                    try
                    {
                        var evalResult = interactiveSession.Submit(sessionStep.submission);

                        var evalOk =
                        evalResult
                        .Extract(evalError => throw new AssertFailedException("Submission result has error: " + evalError));

                        if (sessionStep.expectedResponse != null)
                        {
                            if (sessionStep.expectedResponse != evalOk.interactiveResponse?.displayText)
                            {
                                var errorText =
                                "Response from interactive does not match expected value. Expected:\n" +
                                sessionStep.expectedResponse +
                                "\nBut got this response:\n" +
                                evalOk.interactiveResponse?.displayText;

                                return Result<InteractiveScenarioTestStepFailure, object>.err(
                                    new InteractiveScenarioTestStepFailure(
                                        submission: sessionStep.submission,
                                        errorAsText: errorText));
                            }
                        }
                    }
                    catch (Exception e)
                    {
                        return Result<InteractiveScenarioTestStepFailure, object>.err(
                            new InteractiveScenarioTestStepFailure(
                                submission: sessionStep.submission,
                                errorAsText: "Runtime exception:\n" + e.ToString()));
                    }

                    return Result<InteractiveScenarioTestStepFailure, object>.ok(new object());
                }

                return (sessionStep.stepName, getResult());
            })
            .ToImmutableList();

        return new InteractiveScenarioTestReport(
            stepsReports: stepsReports,
            elapsedTime: totalStopwatch.Elapsed);
    }

    static public Result<string, (string submission, string? expectedResponse)> ParseStep(TreeNodeWithStringPath sessionStep)
    {
        var expectedResponse =
            sessionStep.GetBlobAtPath(new[] { "expected-value" }) is ReadOnlyMemory<byte> expectedValueFile
            ?
            Encoding.UTF8.GetString(expectedValueFile.Span)
            :
            null;

        return
            (sessionStep.GetBlobAtPath(new[] { "submission" }) switch
            {
                null => Result<string, string>.err("Missing submission"),
                ReadOnlyMemory<byte> submissionBlob => Result<string, string>.ok(Encoding.UTF8.GetString(submissionBlob.Span))
            })
            .Map(submission => (submission, expectedResponse));
    }
}
