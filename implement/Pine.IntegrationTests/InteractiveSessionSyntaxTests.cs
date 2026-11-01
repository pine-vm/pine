using AwesomeAssertions;
using ElmTime.ElmInteractive;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Files;
using Pine.Core.Functional;
using System;
using System.Linq;
using Xunit;

namespace Pine.IntegrationTests;

public class InteractiveSessionSyntaxTests
{
    private static string Submit(IInteractiveSession session, string source) =>
        session.Submit(source).Extract(error => throw new Exception(error)).InteractiveResponse.DisplayText;

    [Fact(Timeout = 1000 * 60 * 10)]
    public void TestElmInteractiveScenarios()
    {
        var scenarios =
            TestElmInteractive.LoadScenariosFromDirectories(
                [
                TestElmInteractive.FindScenariosDirectory("elm-interactive-scenarios-core"),
                TestElmInteractive.FindScenariosDirectory("elm-interactive-scenarios-kernel"),
                ],
                StaticConsole.Instance);

        var results =
            TestElmInteractive.TestElmInteractiveScenarios(
                scenarios.NamedDistinctScenarios,
                scenario => (scenario.Key, scenario.Value),
                appCodeTree => new InteractiveSessionSyntax(appCodeTree),
                asyncLogDelegate: null);

        results.Count.Should().Be(scenarios.NamedDistinctScenarios.Count);

        results.Sum(scenario => scenario.Value.StepsReports.Count).Should().Be(
            scenarios.NamedDistinctScenarios.Sum(scenario => scenario.Value.Steps.Count));

        var failures =
            results.Where(scenario => !scenario.Value.Passed)
            .SelectMany(
                scenario =>
                scenario.Value.StepsReports
                .Where(step => step.result.IsErrOrNull() is not null)
                .Select(step => scenario.Key.Key + "/" + step.name + ": " + step.result.IsErrOrNull()!.errorAsText));

        failures.Should().BeEmpty();
    }

    [Fact]
    public void Failed_submissions_preserve_state_and_historical_bindings()
    {
        using var session = new InteractiveSessionSyntax((FileTree?)null);
        Submit(session, "x = 10");
        Submit(session, "old n = x + n");
        Submit(session, "x = 20");
        session.Submit("x = (let r = 123 in r.missing)").IsErrOrNull().Should().Contain("non-record");
        session.Submit("x = ]").IsErrOrNull().Should().Contain("parse");
        Submit(session, "[ old 1, x ]").Should().Be("[ 11, 20 ]");
    }

    [Fact]
    public void Partial_constructor_declarations_and_Debug_functions_work_across_submissions()
    {
        using var session = new InteractiveSessionSyntax((FileTree?)null);
        Submit(session, "type alias Point = { x : Int, y : Int }");
        Submit(session, "partial = Point 1");
        Submit(session, "(partial 2).y").Should().Be("2");
        Submit(session, "render = Debug.toString");
        Submit(session, "render [ 1, 2 ]").Should().Be("\"[1,2]\"");

        var result = session.Submit("Debug.log \"ignored\" 42").Extract(error => throw new Exception(error));
        result.InteractiveResponse.DisplayText.Should().Be("42");
        result.InspectionLog.Should().BeNullOrEmpty();
    }

    [Fact]
    public void Quota_failure_does_not_replace_a_previous_declaration()
    {
        using var session =
            new InteractiveSessionSyntax(
                (FileTree?)null,
                new ElmSyntaxInterpreter.EvaluationConfig(1000, 1000));

        Submit(session, "x = 7");
        Submit(session, "loop n = loop n");
        session.Submit("x = loop 0").IsErrOrNull().Should().NotBeNull();
        Submit(session, "x").Should().Be("7");
    }

    [Fact]
    public void Sessions_are_independent_and_disposal_is_enforced()
    {
        using var first = new InteractiveSessionSyntax((FileTree?)null);
        using var second = new InteractiveSessionSyntax((FileTree?)null);
        Submit(first, "value = 7");
        second.Submit("value").IsErrOrNull().Should().NotBeNull();
        first.Dispose();
        Action submit = () => first.Submit("1");
        submit.Should().Throw<ObjectDisposedException>();
    }
}
