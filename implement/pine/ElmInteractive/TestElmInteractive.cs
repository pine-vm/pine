using Pine;
using Pine.Core;
using Pine.Core.Json;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;

namespace ElmTime.ElmInteractive;

public class TestElmInteractive
{

    [System.Text.Json.Serialization.JsonConverter(typeof(JsonConverterForChoiceType))]
    public abstract record TestInteractiveScenariosLogEntry
    {
        public sealed record ScenarioLogEntry(ScenarioLogEntryStruct Struct)
            : TestInteractiveScenariosLogEntry;

        public record ScenarioLogEntryStruct(
            string ScenarioName,
            TestInteractiveScenarioLogEntry Entry);
    }

    [System.Text.Json.Serialization.JsonConverter(typeof(JsonConverterForChoiceType))]
    public abstract record TestInteractiveScenarioLogEntry
    {
        public sealed record SubmissionStart(SubmissionStartStruct Struct)
            : TestInteractiveScenarioLogEntry;

        public sealed record SubmissionStartStruct(
            string StepName,
            string Submission)
            : TestInteractiveScenarioLogEntry;

        public sealed record SubmissionResponse(SubmissionResponseStruct Struct)
            : TestInteractiveScenarioLogEntry;

        public sealed record SubmissionResponseStruct(
            Result<string, IInteractiveSession.SubmissionResponse> Result)
            : TestInteractiveScenarioLogEntry;
    }

    public record ParsedScenarios(
        TreeNodeWithStringPath ScenariosTree,
        string ScenariosTreeCompositionHash,
        IReadOnlyDictionary<string, Scenario> NamedDistinctScenarios);

    public record Scenario(
        TreeNodeWithStringPath? AppCodeTree,
        IReadOnlyList<(string stepName, ScenarioStep step)> Steps);

    public record ScenarioStep(
        string Submission,
        string? ExpectedResponse,
        string? ExpectedErrorContains);

    public record InteractiveScenarioTestReport(
        Scenario Scenario,
        ImmutableList<(string name, Result<InteractiveScenarioTestStepFailure, InteractiveScenarioTestStepSuccess> result)> StepsReports,
        TimeSpan ElapsedTime)
    {
        public bool Passed => StepsReports.All(s => s.result.IsOk());
    }

    public record InteractiveScenarioTestStepFailure(
        string submission,
        string errorAsText);

    public abstract record InteractiveScenarioTestStepSuccess
    {
        public record ErrorAsExpected(string ErrorAsText)
            : InteractiveScenarioTestStepSuccess;

        public record SubmissionResponseOk(IInteractiveSession.SubmissionResponse SubmissionResponse)
            : InteractiveScenarioTestStepSuccess;
    }

    public static ParsedScenarios ParseElmInteractiveScenarios(
        TreeNodeWithStringPath scenariosTree,
        IConsole console)
    {
        var scenariosTreeComposition =
            PineValueComposition.FromTreeWithStringPath(scenariosTree);

        var scenariosTreeCompositionHash =
            Convert.ToHexStringLower(
                PineValueHashTree.ComputeHash(scenariosTreeComposition).Span);

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
        IConsole console,
        Action<TestInteractiveScenariosLogEntry>? asyncLogDelegate)
    {
        var exceptLoadingStopwatch = System.Diagnostics.Stopwatch.StartNew();

        var scenariosResults =
            TestElmInteractiveScenarios(
                scenarios.NamedDistinctScenarios,
                namedScenario => (namedScenario.Key, namedScenario.Value),
                interactiveConfig.InteractiveSessionFromAppCode,
                asyncLogDelegate: asyncLogDelegate);

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
                " (Pine " + Program.AppVersionId + " with Elm compiler " + interactiveConfig.CompilerId + ")"),
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
        Func<TContainer, (string, Scenario)> getScenario,
        Func<TreeNodeWithStringPath?, IInteractiveSession> interactiveSessionFromAppCode,
        Action<TestInteractiveScenariosLogEntry>? asyncLogDelegate)
        where TContainer : notnull =>
        scenarioContainers
        .AsParallel()
        .WithDegreeOfParallelism(3)
        .Select(scenarioContainer =>
        {
            var (scenarioName, scenario) = getScenario(scenarioContainer);

            return
            (scenarioContainer,
            testReport: TestElmInteractiveScenario(
                scenario,
                interactiveSessionFromAppCode,
                asyncLogDelegate: scenarioEntry =>
                asyncLogDelegate?.Invoke(new TestInteractiveScenariosLogEntry.ScenarioLogEntry(
                    new TestInteractiveScenariosLogEntry.ScenarioLogEntryStruct(
                        ScenarioName: scenarioName,
                        scenarioEntry)))));
        })
        .ToImmutableDictionary(
            s => s.scenarioContainer,
            elementSelector: s => s.testReport);

    public static InteractiveScenarioTestReport TestElmInteractiveScenario(
        Scenario parsedScenario,
        Func<TreeNodeWithStringPath?, IInteractiveSession> interactiveSessionFromAppCode,
        Action<TestInteractiveScenarioLogEntry>? asyncLogDelegate)
    {
        var totalStopwatch = System.Diagnostics.Stopwatch.StartNew();

        using var interactiveSession = interactiveSessionFromAppCode(parsedScenario.AppCodeTree);

        var stepsReports =
            parsedScenario.Steps
            .Select(sessionStep =>
            {
                Result<InteractiveScenarioTestStepFailure, InteractiveScenarioTestStepSuccess> continueWithErrorMessage(
                    string errorMessage)
                {
                    if (sessionStep.step.ExpectedErrorContains is { } expectedErrorContains)
                    {
                        if (!errorMessage.Contains(expectedErrorContains, StringComparison.InvariantCultureIgnoreCase))
                        {
                            var errorText =
                            "Error from interactive does not contain expected value. Expected:\n" +
                            expectedErrorContains +
                            "\nBut got this error:\n" +
                            errorMessage;

                            return
                            (Result<InteractiveScenarioTestStepFailure, InteractiveScenarioTestStepSuccess>)
                            new InteractiveScenarioTestStepFailure(
                                submission: sessionStep.step.Submission,
                                errorAsText: errorText);
                        }

                        return
                        (Result<InteractiveScenarioTestStepFailure, InteractiveScenarioTestStepSuccess>)
                        new InteractiveScenarioTestStepSuccess.ErrorAsExpected(errorMessage);
                    }

                    return
                    new InteractiveScenarioTestStepFailure(
                        submission: sessionStep.step.Submission,
                        errorAsText: errorMessage);
                }

                Result<InteractiveScenarioTestStepFailure, InteractiveScenarioTestStepSuccess> getResult()
                {
                    try
                    {
                        asyncLogDelegate?.Invoke(
                            new TestInteractiveScenarioLogEntry.SubmissionStart(
                                new TestInteractiveScenarioLogEntry.SubmissionStartStruct(
                                    StepName: sessionStep.stepName,
                                    Submission: sessionStep.step.Submission)));

                        var submissionResult = interactiveSession.Submit(sessionStep.step.Submission);

                        asyncLogDelegate?.Invoke(
                            new TestInteractiveScenarioLogEntry.SubmissionResponse(
                                new TestInteractiveScenarioLogEntry.SubmissionResponseStruct(
                                    Result: submissionResult)));

                        return
                        submissionResult
                        .Unpack(
                            fromErr: err => continueWithErrorMessage("Submission result has error: " + err),
                            fromOk: submissionResultOk =>
                            {
                                if (sessionStep.step.ExpectedResponse is { } expectedResponse)
                                {
                                    if (submissionResultOk.InteractiveResponse?.DisplayText is not { } responseDisplayText)
                                    {
                                        return continueWithErrorMessage(
                                            "Expected response but got null as display text");
                                    }

                                    if (Testing.CompareStringsChunkwiseAndReportFirstDifference(
                                        [expectedResponse], responseDisplayText) is { } firstDifference)
                                    {
                                        var errorText =
                                        string.Join(
                                            "\n",
                                            [
                                                "Response from interactive does not match expected value:",
                                                firstDifference,
                                                "The complete response is " +
                                                CommandLineInterface.FormatIntegerForDisplay(responseDisplayText.Length) +
                                                " characters in length as follows:",
                                                responseDisplayText
                                            ]);

                                        return
                                        new InteractiveScenarioTestStepFailure(
                                            submission: sessionStep.step.Submission,
                                            errorAsText: errorText);
                                    }
                                }

                                if (sessionStep.step.ExpectedErrorContains is { } expectedErrorContains)
                                {
                                    var errorText =
                                    "Assertion failed: Expected error containing '" + expectedErrorContains +
                                    "', but submission succeeded";

                                    return
                                    new InteractiveScenarioTestStepFailure(
                                        submission: sessionStep.step.Submission,
                                        errorAsText: errorText);
                                }

                                return new InteractiveScenarioTestStepSuccess.SubmissionResponseOk(submissionResultOk);
                            });
                    }
                    catch (Exception e)
                    {
                        return continueWithErrorMessage("Runtime exception:\n" + e);
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

        var expectedErrorContains =
            sessionStep.GetNodeAtPath(["expected-error-contains.txt"]) is TreeNodeWithStringPath.BlobNode expectedErrorContainsBlob
            ?
            Encoding.UTF8.GetString(expectedErrorContainsBlob.Bytes.Span)
            :
            null;

        return
            (sessionStep.GetNodeAtPath(["submission.txt"]) switch
            {
                TreeNodeWithStringPath.BlobNode submissionBlob =>
                Result<string, string>.ok(Encoding.UTF8.GetString(submissionBlob.Bytes.Span)),

                _ =>
                Result<string, string>.err("Missing submission"),
            })
            .Map(submission => new ScenarioStep(
                submission,
                ExpectedResponse: expectedResponse,
                ExpectedErrorContains: expectedErrorContains));
    }
}
