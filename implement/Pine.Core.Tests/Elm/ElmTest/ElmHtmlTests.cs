using AwesomeAssertions;
using Pine.Core.Elm.Testing;
using System.IO;
using System.Linq;
using System.Text.Json.Nodes;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmTest;

public class ElmHtmlTests
{
    [Theory]
    [InlineData("1.0.3", true)]
    [InlineData("1.0.3", false)]
    [InlineData("1.0.5", true)]
    [InlineData("1.0.5", false)]
    public void Html_queries_preserve_keyed_lazy_nodes_attributes_and_events(
        string virtualDomVersion,
        bool syntaxDiscovery)
    {
        using var project =
            HtmlProject(
                """
                import Html
                import Html.Attributes as Attributes
                import Html.Events as Events
                import Html.Keyed as Keyed
                import Html.Lazy as Lazy
                import Test.Html.Query as Query
                import Test.Html.Selector as Selector
                import Test.Html.Event as Event
                import VirtualDom
                import Json.Decode as Decode

                view =
                    Keyed.node "ul" [ Attributes.class "first", Attributes.class "second" ]
                        [ ( "item", Lazy.lazy (\label -> Html.li [ Attributes.id "item" ] [ Html.text label ]) "deferred text" ) ]

                suite =
                    Test.describe "HTML"
                        [ Test.test "merged classes" (\_ ->
                            Query.fromHtml view |> Query.has [ Selector.classes [ "first", "second" ] ])
                        , Test.test "keyed lazy text" (\_ ->
                            Query.fromHtml view |> Query.has [ Selector.text "deferred text" ])
                        , Test.test "child count" (\_ ->
                            Query.fromHtml view |> Query.findAll [ Selector.tag "li" ] |> Query.count (Expect.equal 1))
                        , Test.test "styles and attributes" (\_ ->
                            Html.input [ Attributes.disabled True, Attributes.title "hello", Attributes.style "color" "red" ] []
                                |> Query.fromHtml
                                |> Query.has [ Selector.attribute (Attributes.disabled True), Selector.attribute (Attributes.title "hello"), Selector.style "color" "red" ])
                        , Test.test "mapped event" (\_ ->
                            Html.button [ Events.onClick 1 ] []
                                |> Html.map ((+) 1)
                                |> Html.map String.fromInt
                                |> Query.fromHtml
                                |> Event.simulate Event.click
                                |> Event.expect "2")
                        , Test.test "mapped attribute" (\_ ->
                            Html.button [ Attributes.map ((+) 1) (Events.onClick 1) ] []
                                |> Query.fromHtml |> Event.simulate Event.click |> Event.expect 2)
                        , Test.test "custom event flags" (\_ ->
                            Html.button [ VirtualDom.on "click" (VirtualDom.Custom (Decode.succeed { message = 7, stopPropagation = True, preventDefault = True })) ] []
                                |> Query.fromHtml |> Event.simulate Event.click |> Event.expect 7)
                        , Test.test "legacy true" (\_ -> Expect.true "must be true" True)
                        , Test.test "legacy false" (\_ -> Expect.false "must be false" False)
                        , Test.test "namespaced node" (\_ ->
                            VirtualDom.nodeNS "http://www.w3.org/2000/svg" "svg" [] [ VirtualDom.text "vector" ]
                                |> Query.fromHtml |> Query.has [ Selector.tag "svg", Selector.text "vector" ])
                        , Test.test "last property wins" (\_ ->
                            Html.div [ Attributes.title "old", Attributes.title "new" ] []
                                |> Query.fromHtml |> Query.has [ Selector.attribute (Attributes.title "new") ])
                        ]
                """,
                "1.2.2",
                virtualDomVersion);

        var completed = Run(project, syntaxDiscovery);
        completed.Tests.Should().HaveCount(11);

        completed.Tests.Should().OnlyContain(
            test => test.Kind == CompletedTestKind.Passed,
            ElmTestRunner.RenderTestResults(completed.Tests, includeTestDetails: true).PlainText);
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void Html_query_mismatches_and_legacy_boolean_expectations_fail(bool syntaxDiscovery)
    {
        using var project =
            HtmlProject(
                """
                import Html
                import Html.Attributes as Attributes
                import Test.Html.Query as Query
                import Test.Html.Selector as Selector

                suite =
                    Test.describe "Failures"
                        [ Test.test "missing class" (\_ ->
                            Html.div [ Attributes.class "actual" ] []
                                |> Query.fromHtml |> Query.has [ Selector.class "missing" ])
                        , Test.test "missing text" (\_ ->
                            Html.div [] [ Html.text "actual text" ]
                                |> Query.fromHtml |> Query.has [ Selector.text "missing text" ])
                        , Test.test "wrong count" (\_ ->
                            Html.div [] [] |> Query.fromHtml |> Query.findAll [ Selector.tag "li" ] |> Query.count (Expect.equal 1))
                        , Test.test "duplicate class representations" (\_ ->
                            Html.div [ Attributes.class "one", Attributes.attribute "class" "two" ] []
                                |> Query.fromHtml |> Query.has [ Selector.class "one" ])
                        , Test.test "legacy true failure" (\_ -> Expect.true "true message" False)
                        , Test.test "legacy false failure" (\_ -> Expect.false "false message" True)
                        ]
                """,
                "1.2.2");

        var completed = Run(project, syntaxDiscovery);
        completed.Tests.Should().HaveCount(6).And.OnlyContain(test => test.Kind == CompletedTestKind.Failed);
        var rendered = ElmTestRunner.RenderTestResults(completed.Tests, includeTestDetails: true).PlainText;
        rendered.Should().Contain("missing").And.Contain("true message").And.Contain("false message");
        rendered.Should().Contain("className").And.NotContain("Failed evaluating test:");
    }

    [Theory]
    [InlineData(true)]
    [InlineData(false)]
    public void Pi_and_degrees_work_in_discovery_and_as_function_values(bool syntaxDiscovery)
    {
        using var project =
            new ElmFuzzTests.Project(
                """
                label = if degrees 180 > 3 && pi < 4 then "angles" else "wrong"
                weighted = Fuzz.frequency [ ( 1, Fuzz.constant 42 ), ( 1, Fuzz.constant 42 ) ]
                suite =
                    Test.describe label
                        [ Test.test "degrees" (\_ -> Expect.within (Expect.Absolute 0.000000000001) pi (degrees 180))
                        , Test.test "higher order" (\_ ->
                            List.map degrees [ 0, 90, 180 ]
                                |> List.map round
                                |> Expect.equal [ 0, 2, 3 ])
                        , Test.describe (String.fromInt (round -1.5))
                            [ Test.test "ties" (\_ ->
                                List.map round [ -1.6, -1.5, -0.5, 0.5, 1.5 ] |> Expect.equal [ -2, -1, 0, 1, 2 ])
                            ]
                        , Test.fuzz weighted "frequency construction" (\n -> Expect.equal 42 n)
                        ]
                """);

        var completed = Run(project, syntaxDiscovery);
        completed.Tests.Should().HaveCount(4).And.OnlyContain(test => test.Kind == CompletedTestKind.Passed);
        completed.Tests.Should().Contain(test => test.Path.Contains("-1"));
        completed.Tests.Should().OnlyContain(test => test.Path.Contains("angles"));
    }

    private static ElmTestRun.Completed Run(ElmFuzzTests.Project project, bool syntaxDiscovery) =>
        (syntaxDiscovery
        ?
        ElmTestRunner.DiscoverAndRunTests(project.Directory)
        :
        ElmTestRunner.CompileAndRunTests(project.Directory))
        .Should().BeOfType<ElmTestRun.Completed>().Which;

    private static ElmFuzzTests.Project HtmlProject(
        string declarations,
        string version,
        string virtualDomVersion = "1.0.5")
    {
        var project = new ElmFuzzTests.Project(declarations);
        var path = Path.Combine(project.Directory, "elm.json");
        var manifest = JsonNode.Parse(File.ReadAllText(path))!;
        var dependencies = manifest["test-dependencies"]!["direct"]!;
        dependencies["elm-explorations/test"] = version;
        dependencies["elm/html"] = "1.0.0";
        dependencies["elm/json"] = "1.1.4";
        dependencies["elm/virtual-dom"] = virtualDomVersion;
        File.WriteAllText(path, manifest.ToJsonString());
        return project;
    }
}
