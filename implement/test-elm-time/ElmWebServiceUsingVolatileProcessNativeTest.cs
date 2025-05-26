using FluentAssertions;
using Pine.Core;
using Pine.Core.Elm;
using Pine.Elm.Platform;
using System;
using System.Linq;
using System.Net.Http;
using System.Threading.Tasks;
using Xunit;

namespace TestElmTime;

public class ElmWebServiceUsingVolatileProcessNativeTest
{
    public static PineValue ElmWebServiceVolatileProcessNative =>
        TestSetup.AppConfigComponentFromFiles(TestSetup.VolatileProcessNativeWebApp);

    [Fact]
    public async Task Volatile_process_native_echo_json()
    {
        using var testSetup =
            WebHostAdminInterfaceTestSetup.Setup(
                deployAppAndInitElmState: ElmWebServiceVolatileProcessNative);

        using var server = testSetup.StartWebHost();

        using var client = testSetup.BuildPublicAppHttpClient();

        var httpResponse =
            await client.PostAsync(
                "/",
                new StringContent("[ 1,  3, 4 ]\n"));

        var httpResponseContent = await httpResponse.Content.ReadAsStringAsync();

        httpResponseContent.Trim().Should().Be("[1,3,4]", "Response content should match the expected JSON format.");
    }

    [Fact]
    public async Task Volatile_process_native_echo_json_sandbox()
    {
        var webAppSource =
            ElmWebServiceVolatileProcessNative;

        var webAppSourceTree =
            PineValueComposition.ParseAsTreeWithStringPath(webAppSource)
            .Extract(err => throw new Exception("Failed parsing app source files as tree: " + err));

        var webServiceConfig =
            WebServiceInterface.ConfigFromSourceFilesAndEntryFileName(
                webAppSourceTree,
                ["src", "Backend", "Main.elm"]);

        var webServiceApp =
            new MutatingWebServiceApp(webServiceConfig);

        await using var volatileProcessHost =
            new VolatileProcessHost([webAppSource]);

        var formatHttpRequestId = "123";

        static long posixTimeMilli() =>
            DateTimeOffset.UtcNow.ToUnixTimeMilliseconds();

        webServiceApp.EventHttpRequest(
            new WebServiceInterface.HttpRequestEventStruct
            (
                HttpRequestId: formatHttpRequestId,
                PosixTimeMilli: posixTimeMilli(),
                RequestContext: new WebServiceInterface.HttpRequestContext
                (
                    ClientAddress: null
                ),
                Request: new WebServiceInterface.HttpRequestProperties
                (
                    Method: "POST",
                    Uri: "http://demohost/api",
                    Body:
                    System.Text.Encoding.UTF8.GetBytes("[ 1,  3, 4 ]\n"),
                    Headers: []
                )
            ));

        for (var i = 0; i < 15; i++)
        {
            await Task.Delay(TimeSpan.FromSeconds(1));

            webServiceApp.UpdateForPosixTime(posixTimeMilli());

            await volatileProcessHost.ExchangeAsync(webServiceApp);
        }

        var httpResponses = webServiceApp.CopyHttpResponses();

        var formatResponseCommand =
            httpResponses
            .FirstOrDefault(cmd => cmd.Respond.HttpRequestId == formatHttpRequestId);

        var appStateElm =
            ElmValueEncoding.PineValueAsElmValue(
                webServiceApp.AppState,
                null,
                null);

        if (formatResponseCommand is null)
        {
            throw new Exception(
                "Did not find expected response to format request among " +
                httpResponses.Count + " HTTP response commands.");
        }

        var formatHttpResponseBody =
            formatResponseCommand.Respond.Response.Body;

        if (formatHttpResponseBody is null)
        {
            throw new Exception("Expected a response body.");
        }

        var formatResponseContentAsString =
            System.Text.Encoding.UTF8.GetString(formatHttpResponseBody.Value.Span);

        formatResponseCommand.Respond.Response.StatusCode.Should().Be(
            200,
            "Response status code should be OK.\nresponseContentAsString:\n" + formatResponseContentAsString);

        ElmTime.ElmSyntax.ElmModule.ModuleLines(formatResponseContentAsString)
            .First().Should().Be("[1,3,4]", "Response content should match the expected JSON format.");
    }
}
