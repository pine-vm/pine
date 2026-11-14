using AwesomeAssertions;
using Pine.Core.Files;
using Pine.Core.Testing;
using Pine.Elm019;
using Pine.Elm019.Testing;
using System;
using System.IO;
using System.Linq;
using System.Text;
using System.Text.Json;
using System.Threading.Tasks;
using Xunit;

namespace Pine.IntegrationTests.Elm;

public class ElmMakeEcmascriptFlavorTests
{
    private static string CorpusDirectory => TestResultSummary.FindTestDataDirectory("ElmMakeEcmascript");

    [Fact]
    public async Task ElmMakeEcmascript_debug_expectations_include_real_debugger_runtime_and_message_metadata()
    {
        var scenario =
            ElmMakeEcmascriptScenarios.SelectScenario(
                ElmMakeEcmascriptScenarios.Discover(CorpusDirectory),
                "browser-programs");

        var expectations = scenario.Expectations.Where(item => item.Flavor == ElmMakeFlavor.Debug).ToArray();
        expectations.Should().NotBeEmpty().And.OnlyContain(item => !item.Skipped);

        foreach (var expectation in expectations)
        {
            var output =
                await File.ReadAllTextAsync(ElmMakeEcmascriptScenarios.ExpectationPath(CorpusDirectory, expectation));

            output.Should().Contain("Compiled in DEBUG mode").And.Contain("var _Debugger_element = F4(")
                .And.Contain("var _Debugger_document = F4(");

            var offset = output.IndexOf("{\"versions\":", StringComparison.Ordinal);
            offset.Should().BeGreaterThan(0);
            var bytes = Encoding.UTF8.GetBytes(output[offset..]);
            var reader = new Utf8JsonReader(bytes);
            using var metadata = JsonDocument.ParseValue(ref reader);
            metadata.RootElement.GetProperty("versions").GetProperty("elm").GetString().Should().Be("0.19.1");
            var module = Path.GetFileNameWithoutExtension(expectation.SourcePath);
            metadata.RootElement.GetProperty("types").GetProperty("message").GetString().Should().Be(module + ".Msg");

            metadata.RootElement.GetProperty("types").GetProperty("unions").GetProperty(module + ".Msg")
                .GetProperty("tags").EnumerateObject().Should().NotBeEmpty();
        }
    }

    [Theory]
    [InlineData(ElmMakeFlavor.Development)]
    [InlineData(ElmMakeFlavor.Debug)]
    [InlineData(ElmMakeFlavor.Optimize)]
    public async Task ElmMakeEcmascript_flavors_match_reference_for_debug_todo_and_dead_code(ElmMakeFlavor flavor)
    {
        const string Source =
            """
            module Main exposing (main)
            import Html
            type Box = Box Int
            main = Html.text (String.fromInt (value (Box 7)))
            value (Box n) = if n > 0 then n else Debug.todo "unreachable"
            unused = Debug.log "dead" 3
            """;

        var scenario =
            ElmMakeEcmascriptScenarios.SelectScenario(
                ElmMakeEcmascriptScenarios.Discover(CorpusDirectory),
                "static-html-text-empty");

        scenario =
            scenario with
            {
                AppFiles =
                scenario.AppFiles.SetNodeAtPathSorted(
                    [
                    "src",
                    "Main.elm"
                    ],
                    FileTree.File(Encoding.UTF8.GetBytes(Source))),
            };

        var expectation =
            new ElmMakeExpectation(
                scenario.Name,
                "src/Main.elm",
                new(0, 19, 1),
                ElmMakeOutputType.JavaScript,
                flavor,
                false);

        var actual = await ElmMakeScenarioCompiler.CompileOutcomeAsync(CorpusDirectory, scenario, expectation);
        var reference = await new ElmMakeReferenceCompiler(CorpusDirectory).CompileOutcomeAsync(scenario, expectation);

        if (flavor == ElmMakeFlavor.Optimize)
        {
            actual.Should().BeOfType<ElmMakeCompilation.CompilerErrors>().Which.Problems.Should()
                .ContainSingle().Which.Title.Should().Be("DEBUG REMNANTS");

            ElmMakeErrorSnapshots.Render(((ElmMakeCompilation.CompilerErrors)actual).Problems).Should()
                .Be(ElmMakeErrorSnapshots.Render(((ElmMakeCompilation.CompilerErrors)reference).Problems));
        }
        else
        {
            var output = actual.Should().BeOfType<ElmMakeCompilation.Output>().Which.Text;

            output.Should().Be(((ElmMakeCompilation.Output)reference).Text)
                .And.Contain("_Debug_todo(").And.NotContain("$author$project$Main$unused =");
        }
    }

    [Theory]
    [InlineData(
        """
        module Sandbox exposing (main)
        import Browser
        import Html
        import AppTypes
        type Msg = Receive (AppTypes.Payload Int) | Nested (Maybe Msg)
        main : AppTypes.App Msg
        main = Browser.element { init = \() -> ((), Cmd.none), update = \_ model -> (model, Cmd.none), subscriptions = \_ -> Sub.none, view = \_ -> Html.text "imported alias" }
        """)]
    [InlineData(
        """
        module Sandbox exposing (main)
        import Browser
        import Html
        type Msg = Increment
        main = Browser.sandbox { init = 0, update = update, view = \_ -> Html.text "inferred" }
        update : Msg -> Int -> Int
        update _ model = model + 1
        """)]
    [InlineData(
        """
        module Sandbox exposing (main)
        import Browser
        import Html
        type alias App msg = Program () () msg
        type alias Payload a = { value : a, entries : List a, pair : (a, a) }
        type Msg = Receive (Payload Int) | Nested (Maybe Msg)
        main : App Msg
        main = Browser.element { init = \() -> ((), Cmd.none), update = \_ model -> (model, Cmd.none), subscriptions = \_ -> Sub.none, view = \_ -> Html.text "metadata" }
        """)]
    public async Task ElmMakeEcmascript_debug_metadata_generalizes_beyond_fixture_message_types(string source)
    {
        var scenario =
            ElmMakeEcmascriptScenarios.SelectScenario(
                ElmMakeEcmascriptScenarios.Discover(CorpusDirectory),
                "browser-programs");

        scenario =
            scenario with
            {
                AppFiles =
                scenario.AppFiles
                .SetNodeAtPathSorted(["src", "Sandbox.elm"], FileTree.File(Encoding.UTF8.GetBytes(source)))
                .SetNodeAtPathSorted(
                    ["src", "AppTypes.elm"],
                    FileTree.File(
                        Encoding.UTF8.GetBytes(
                            """
                            module AppTypes exposing (App, Payload)
                            type alias App msg = Program () () msg
                            type alias Payload a = { value : a, entries : List a, pair : (a, a) }
                            """))),
            };

        var expectation =
            new ElmMakeExpectation(
                scenario.Name,
                "src/Sandbox.elm",
                new(0, 19, 1),
                ElmMakeOutputType.JavaScript,
                ElmMakeFlavor.Debug,
                false);

        var actual = await ElmMakeScenarioCompiler.CompileAsync(CorpusDirectory, scenario, expectation);
        var reference = await new ElmMakeReferenceCompiler(CorpusDirectory).CompileAsync(scenario, expectation);
        actual.ReplaceLineEndings("\n").Should().Be(reference.ReplaceLineEndings("\n"));
    }

    [Fact]
    public async Task ElmMakeEcmascript_optimize_rejects_debug_calls_even_in_dead_declarations()
    {
        var scenario =
            ElmMakeEcmascriptScenarios.SelectScenario(
                ElmMakeEcmascriptScenarios.Discover(CorpusDirectory),
                "static-html-text-empty");

        scenario =
            scenario with
            {
                AppFiles =
                scenario.AppFiles.SetNodeAtPathSorted(
                    ["src", "Main.elm"],
                    FileTree.File(
                        Encoding.UTF8.GetBytes(
                            "module Main exposing (main)\nimport Html\nmain = Html.text \"ok\"\nunused = Debug.log \"dead\" 3\n"))),
            };

        var expectation =
            new ElmMakeExpectation(
                scenario.Name,
                "src/Main.elm",
                new(0, 19, 1),
                ElmMakeOutputType.JavaScript,
                ElmMakeFlavor.Optimize,
                false);

        var actual = await ElmMakeScenarioCompiler.CompileOutcomeAsync(CorpusDirectory, scenario, expectation);
        var reference = await new ElmMakeReferenceCompiler(CorpusDirectory).CompileOutcomeAsync(scenario, expectation);
        var problems = actual.Should().BeOfType<ElmMakeCompilation.CompilerErrors>().Which.Problems;
        problems.Should().ContainSingle().Which.Title.Should().Be("DEBUG REMNANTS");

        ElmMakeErrorSnapshots.Render(problems).Should()
            .Be(ElmMakeErrorSnapshots.Render(((ElmMakeCompilation.CompilerErrors)reference).Problems));
    }
}
