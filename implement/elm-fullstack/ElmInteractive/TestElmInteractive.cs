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
    public record InteractiveScenarioTestResult(
        int totalStepsCount,
        IReadOnlyList<InteractiveScenarioTestStepResult> testedSteps)
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

    static public InteractiveScenarioTestResult TestElmInteractiveScenario(Composition.TreeWithStringPath scenarioTree)
    {
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
                        Encoding.UTF8.GetString(testStep.component.GetBlobAtPath(new[] { "submission" })!.ToArray());

                    var evalResult =
                        interactiveSession.Submit(submission);

                    var expectedValueFile =
                        testStep.component.GetBlobAtPath(new[] { "expected-value" });

                    if (expectedValueFile != null)
                    {
                        var expectedValue = Encoding.UTF8.GetString(expectedValueFile.ToArray());

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

        return new InteractiveScenarioTestResult(
            totalStepsCount: testScenarioSteps.Count,
            testedSteps: testedSteps);
    }
}
