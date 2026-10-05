using AwesomeAssertions;
using MoreLinq;
using Pine.Core;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using System;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.IntegrationTests;

public class ElmWebServiceAppTests
{
    [Fact]
    public void Cleaning_generated_JavaScript_main_removes_runtime_imports_without_changing_strings()
    {
        var source =
            """
            module Backend.InterfaceToHost_Root exposing (config)
            import Platform
            import Platform.Cmd
            import Platform.Sub
            import Json.Decode
            config = "import Platform"

            main : Program Int () String
            main = Platform.worker { init = always ( (), Cmd.none ), update = always ( (), Cmd.none ), subscriptions = always Sub.none }
            """;

        var tree =
            FileTree.EmptyTree.SetNodeAtPathSorted(
                ElmTime.ElmTimeJsonAdapter.RootFilePath,
                FileTree.File(Encoding.UTF8.GetBytes(source)));

        var cleaned = ElmTime.ElmTimeJsonAdapter.CleanUpFromLoweredForJavaScript(tree);
        var text = Encoding.UTF8.GetString(cleaned.EnumerateFilesTransitive().Single().fileContent.Span);

        var parsed =
            ElmSyntaxParser.ParseModuleText(text).Extract(
                error => throw new InvalidOperationException(error.ToString()));

        parsed.Imports.Select(import => string.Join(".", import.Value.ModuleName.Value)).Should().Equal("Json.Decode");
        text.Should().Contain("\"import Platform\"").And.NotContain("Platform.worker");
    }

    public static PineValue CounterWebApp =>
        TestSetup.AppConfigComponentFromFiles(TestSetup.CounterElmWebApp);

    public static PineValue CalculatorWebApp =>
        TestSetup.AppConfigComponentFromFiles(TestSetup.CalculatorWebApp);

    public static PineValue StringBuilderWebApp =>
        TestSetup.AppConfigComponentFromFiles(TestSetup.StringBuilderElmWebApp);

    public static PineValue CrossPropagateHttpHeadersToAndFromBody =>
        TestSetup.AppConfigComponentFromFiles(TestSetup.CrossPropagateHttpHeadersToAndFromBodyElmWebApp);

    public static PineValue HttpProxyWebApp =>
        TestSetup.AppConfigComponentFromFiles(TestSetup.HttpProxyWebApp);

    [Fact]
    public async System.Threading.Tasks.Task Restore_counter_http_web_app_on_server_restart()
    {
        var eventsAndExpectedResponses =
            TestSetup.CounterProcessTestEventsAndExpectedResponses(
                [
                    (0, 0),
                    (1, 1),
                    (3, 4),
                    (5, 9),
                    (7, 16),
                    (11, 27),
                    (-13, 14),
                ]).ToList();

        var eventsAndExpectedResponsesBatches = eventsAndExpectedResponses.Batch(3).ToList();

        eventsAndExpectedResponsesBatches.Should().HaveCountGreaterThan(
            2,
            "More than two batches of events to test with.");

        using var testSetup =
            WebHostAdminInterfaceTestSetup.Setup(deployAppAndInitElmState: CounterWebApp);

        foreach (var eventsAndExpectedResponsesBatch in eventsAndExpectedResponsesBatches)
        {
            using var server = testSetup.StartWebHost();

            foreach (var (serializedEvent, expectedResponse) in eventsAndExpectedResponsesBatch)
            {
                using var client =
                    testSetup.BuildPublicAppHttpClient();

                var httpResponse =
                    await client.PostAsync(
                        "",
                        new System.Net.Http.StringContent(serializedEvent, System.Text.Encoding.UTF8));

                var httpResponseContent =
                    await httpResponse.Content.ReadAsStringAsync();

                httpResponseContent.Should().Be(expectedResponse, "server response");
            }
        }
    }
}
