using AwesomeAssertions;
using ElmTime.ElmInteractive;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Files;
using Pine.Core.Functional;
using System;
using System.IO;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.IntegrationTests;

public sealed class ElmInteractivePreparationTests : IDisposable
{
    private readonly string _directory =
        Path.Combine(Path.GetTempPath(), "pine-interactive-preparation", Guid.NewGuid().ToString("N"));

    private string Suite(string name) => Path.Combine(_directory, "implement", "test-and-train", name);

    private void Write(string path, string content)
    {
        Directory.CreateDirectory(Path.GetDirectoryName(path)!);
        File.WriteAllText(path, content);
    }

    private TestElmInteractive.ParsedScenarios Load(params string[] directories) =>
        TestElmInteractive.LoadScenariosFromDirectories(directories, StaticConsole.Instance);

    [Theory]
    [InlineData("elm-interactive-scenarios-core", 43, 516)]
    [InlineData("elm-interactive-scenarios-kernel", 10, 203)]
    public void Repository_scenarios_are_discoverable_without_executing_them(string suite, int scenarios, int steps)
    {
        var path = TestElmInteractive.FindScenariosDirectory(suite);
        var loaded = Load(path);

        loaded.NamedDistinctScenarios.Count.Should().Be(scenarios);
        loaded.NamedDistinctScenarios.Sum(item => item.Value.Steps.Count).Should().Be(steps);

        if (suite.EndsWith("-core", StringComparison.Ordinal))
        {
            loaded.NamedDistinctScenarios["elm-syntax-float"].Steps.Should().NotContain(step => step.stepName == "110");

            loaded.NamedDistinctScenarios["regression-101-recursive-function-in-let-block"].Steps
                .Select(step => step.stepName).Should().Equal("placeholder");
        }
    }

    [Fact]
    public void Discovery_is_independent_of_build_depth_and_starting_directory()
    {
        Write(Path.Combine(_directory, "pine.slnx"), "");
        var suite = Suite("examples");
        Directory.CreateDirectory(suite);
        var build = Path.Combine(_directory, "implement", "Tests", "bin", "Release", "net10.0");
        Directory.CreateDirectory(build);

        TestElmInteractive.FindScenariosDirectory("examples", _directory).Should().Be(suite);
        TestElmInteractive.FindScenariosDirectory("examples", build).Should().Be(suite);
    }

    [Fact]
    public void Loading_is_deterministic_and_preserves_steps_and_context_assets()
    {
        var suite = Suite("examples");
        Write(Path.Combine(suite, "scenario", "steps", "02", "submission.txt"), "x");
        Write(Path.Combine(suite, "scenario", "steps", "02", "expected-value.txt"), "7");
        Write(Path.Combine(suite, "scenario", "steps", "01", "submission.txt"), "x = 7");
        Write(Path.Combine(suite, "scenario", "context-app", "asset.txt"), "retained");
        Write(Path.Combine(suite, "scenario", "steps-disabled", "00", "submission.txt"), "not active");
        Directory.CreateDirectory(Path.Combine(suite, "empty-leftover"));

        var first = Load(suite);
        var second = Load(suite);
        first.ScenariosTreeCompositionHash.Should().Be(second.ScenariosTreeCompositionHash);
        first.NamedDistinctScenarios.Keys.Should().Equal("scenario");
        var scenario = first.NamedDistinctScenarios["scenario"];
        scenario.Steps.Select(step => step.stepName).Should().Equal("01", "02");
        scenario.Steps[0].step.ExpectedResponse.Should().BeNull();
        scenario.Steps[1].step.ExpectedResponse.Should().Be("7");
        scenario.AppCodeTree!.GetNodeAtPath(["asset.txt"]).Should().BeOfType<FileTree.FileNode>();
    }

    [Fact]
    public void Missing_empty_and_duplicate_suites_are_explicit_failures()
    {
        var first = Suite("first");
        Directory.CreateDirectory(first);
        Action empty = () => Load(first);
        empty.Should().Throw<ArgumentException>().WithMessage("*No scenarios*");

        Action missing = () => Load(Suite("missing"));
        missing.Should().Throw<DirectoryNotFoundException>();

        var second = Suite("second");
        Write(Path.Combine(first, "same", "steps", "01", "submission.txt"), "1");
        Write(Path.Combine(second, "same", "steps", "01", "submission.txt"), "2");
        Action duplicate = () => Load(first, second);
        duplicate.Should().Throw<ArgumentException>().WithMessage("*Duplicate scenario name*same*");
    }

    [Fact]
    public void Malformed_scenarios_report_the_scenario_and_step()
    {
        var suite = Suite("examples");
        Write(Path.Combine(suite, "broken", "steps", "01", "expected-value.txt"), "1");
        Action load = () => Load(suite);
        load.Should().Throw<Exception>().WithMessage("*broken*01*Missing submission*");
    }

    [Fact]
    public void Ambiguous_step_order_is_rejected()
    {
        var suite = Suite("examples");

        foreach (var step in new[] { "1", "2", "10" })
            Write(Path.Combine(suite, "broken", "steps", step, "submission.txt"), "1");

        Action load = () => Load(suite);
        load.Should().Throw<Exception>().WithMessage("*Ambiguous sort order*");
    }

    [Fact]
    public void Read_source_file_scenario_uses_shared_lowering_and_interpreter_preparation()
    {
        var fixtures = Load(TestElmInteractive.FindScenariosDirectory("elm-interactive-scenarios-core"));
        var scenario = fixtures.NamedDistinctScenarios["read-source-file"];

        var environment =
            ElmInterpreterAppPreparation.Prepare(scenario.AppCodeTree)
            .Extract(error => throw new Exception(error));

        foreach (var (_, step) in scenario.Steps)
        {
            var result =
                environment.PrepareExpression(step.Submission)
                .Extract(error => throw new Exception(error))
                .Evaluate()
                .Extract(error => throw new Exception(error.ToString()));

            ElmSyntaxInterpreter.RenderAsElmExpression(result).expressionString.Should().Be(step.ExpectedResponse);
        }
    }

    [Theory]
    [InlineData("elm-choice-type")]
    [InlineData("elm-choice-type-expose-tags")]
    [InlineData("use-declaration-from-context")]
    public void Context_app_sources_are_prepared_for_scenario_expressions(string scenarioName)
    {
        var scenario =
            Load(TestElmInteractive.FindScenariosDirectory("elm-interactive-scenarios-core"))
            .NamedDistinctScenarios[scenarioName];

        var environment =
            ElmInterpreterAppPreparation.Prepare(scenario.AppCodeTree)
            .Extract(error => throw new Exception(error));

        foreach (var (_, step) in scenario.Steps)
        {
            var submission =
                environment.PrepareSubmission(step.Submission)
                .Extract(error => throw new Exception(error));

            switch (submission)
            {
                case ElmInterpreterSubmission.Declaration declaration:
                    environment = declaration.Environment;
                    break;

                case ElmInterpreterSubmission.Expression expression:
                    var value =
                        expression.Prepared.Evaluate()
                        .Extract(error => throw new Exception(error.ToString()));

                    ElmSyntaxInterpreter.RenderAsElmExpression(value).expressionString.Should().Be(
                        step.ExpectedResponse);

                    break;

                default:
                    throw new NotImplementedException(
                        $"{nameof(Context_app_sources_are_prepared_for_scenario_expressions)} does not handle submission variant: {submission.GetType().Name}");
            }
        }
    }

    [Fact]
    public void Invalid_lowering_inputs_are_returned_to_the_consumer()
    {
        var tree = FileTree.EmptyTree.SetNodeAtPathSorted(["elm.json"], FileTree.File(Encoding.UTF8.GetBytes("{")));
        ElmInterpreterAppPreparation.Prepare(tree).IsErrOrNull().Should().Contain("elm.json");
    }

    [Fact]
    public void Explicit_lowering_root_must_be_in_the_selected_source_directories()
    {
        var scenario =
            Load(TestElmInteractive.FindScenariosDirectory("elm-interactive-scenarios-core"))
            .NamedDistinctScenarios["read-source-file"];

        ElmInterpreterAppPreparation.LowerAppSourceTree(scenario.AppCodeTree!, ["missing.elm"])
            .IsErrOrNull().Should().Contain("missing.elm");
    }

    [Fact]
    public void Conflicting_step_expectations_are_rejected_during_loading()
    {
        var suite = Suite("examples");
        var step = Path.Combine(suite, "broken", "steps", "01");
        Write(Path.Combine(step, "submission.txt"), "1");
        Write(Path.Combine(step, "expected-value.txt"), "1");
        Write(Path.Combine(step, "expected-error-contains.txt"), "error");

        Action load = () => Load(suite);
        load.Should().Throw<Exception>().WithMessage("*both a value and an error*");
    }

    public void Dispose()
    {
        if (Directory.Exists(_directory))
            Directory.Delete(_directory, recursive: true);
    }
}
