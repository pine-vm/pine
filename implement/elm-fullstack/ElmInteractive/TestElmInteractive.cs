using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace elm_fullstack.ElmInteractive;

public class TestElmInteractive
{
    public record InteractiveScenarioTestReport(
        int totalStepsCount,
        IReadOnlyList<InteractiveScenarioTestStepResult> testedSteps,
        TimeSpan elapsedTime)
    {
        public bool Passed => testedSteps.Count == totalStepsCount && testedSteps.All(s => s.Passed) && Exception == null;

        public Exception? Exception => testedSteps.Select(s => s.exception).WhereNotNull().FirstOrDefault();
    }

    public record InteractiveScenarioTestStepResult(
        int durationMs,
        Exception? exception)
    {
        public bool Passed => exception == null;
    }

    static public ImmutableDictionary<TContainer, InteractiveScenarioTestReport> TestElmInteractiveScenarios<TContainer>(
        IReadOnlyCollection<TContainer> scenarioContainers,
        Func<TContainer, Composition.TreeWithStringPath> getScenario) where TContainer : notnull =>
        scenarioContainers
        .AsParallel()
        .WithDegreeOfParallelism(4)
        .Select(scenarioContainer => (scenarioContainer, testReport: TestElmInteractiveScenario(getScenario(scenarioContainer))))
        .ToImmutableDictionary(
            s => s.scenarioContainer,
            elementSelector: s => s.testReport);

    static public InteractiveScenarioTestReport TestElmInteractiveScenario(Composition.TreeWithStringPath scenarioTree)
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

        using var interactiveSession = new InteractiveSession(appCodeTree: appCodeTree);

        var testScenarioSteps = stepsDirectory.TreeContent;

        var testedSteps =
            testScenarioSteps
            .Select(testStep =>
            {
                var stepStopwatch = System.Diagnostics.Stopwatch.StartNew();

                var stepName = testStep.name;

                string? submission = null;

                try
                {
                    submission =
                        Encoding.UTF8.GetString(testStep.component.GetBlobAtPath(new[] { "submission" })!.Value.Span);

                    var evalResult =
                        interactiveSession.Submit(submission);

                    var expectedValueFile =
                        testStep.component.GetBlobAtPath(new[] { "expected-value" });

                    if (expectedValueFile != null)
                    {
                        var expectedValue = Encoding.UTF8.GetString(expectedValueFile.Value.Span);

                        Assert.IsNull(evalResult.Err, "Submission result has error: " + evalResult.Err);

                        Assert.AreEqual(
                            expectedValue,
                            evalResult.Ok?.SubmissionResponseValue?.valueAsElmExpressionText,
                            "Value from evaluation does not match expected value.");
                    }

                    return new InteractiveScenarioTestStepResult(
                        durationMs: (int)stepStopwatch.Elapsed.TotalMilliseconds,
                        exception: null);
                }
                catch (Exception e)
                {
                    return new InteractiveScenarioTestStepResult(
                        durationMs: (int)stepStopwatch.Elapsed.TotalMilliseconds,
                        exception: new Exception("Failed step '" + stepName + "' with exception.\nSubmission in this step:\n" + submission, e));
                }
            })
            .TakeUntil(s => !s.Passed)
            .ToImmutableList();

        return new InteractiveScenarioTestReport(
            totalStepsCount: testScenarioSteps.Count,
            testedSteps: testedSteps,
            elapsedTime: totalStopwatch.Elapsed);
    }
}
