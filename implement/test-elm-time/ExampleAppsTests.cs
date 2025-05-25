using FluentAssertions;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using Pine.Core.Elm;
using Pine.Elm.Platform;
using System;
using System.Linq;
using System.Net;
using System.Threading.Tasks;

namespace TestElmTime;

[TestClass]
public class ExampleAppsTests
{
    public static PineValue ExampleAppValueFromExampleName(string exampleName) =>
        TestSetup.AppConfigComponentFromFiles(
            TestSetup.GetElmAppFromDirectoryPath(
                [".", "..", "..", "..", "..", "example-apps", exampleName]));

    [TestMethod]
    public async Task Example_app_minimal_backend_hello_world()
    {
        var webAppSource =
            ExampleAppValueFromExampleName("minimal-backend-hello-world");

        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppAndInitElmState: webAppSource);
        using var server = testSetup.StartWebHost();
        using var publicAppClient = testSetup.BuildPublicAppHttpClient();

        var httpResponse =
            await publicAppClient.GetAsync("");

        var responseContentAsString =
            await httpResponse.Content.ReadAsStringAsync();

        httpResponse.StatusCode.Should().Be(
            HttpStatusCode.OK,
            "Response status code should be OK.\nresponseContentAsString:\n" + responseContentAsString);

        responseContentAsString.Should().Be(
            "Hello, World!",
            "response content as string");
    }

    [TestMethod]
    public void Example_app_minimal_webservice_hello_world_sandbox()
    {
        var webAppSource =
            ExampleAppValueFromExampleName("minimal-backend-hello-world");

        var webAppSourceTree =
            PineValueComposition.ParseAsTreeWithStringPath(webAppSource)
            .Extract(err => throw new Exception("Failed parsing app source files as tree: " + err));

        var webServiceConfig =
            WebServiceInterface.ConfigFromSourceFilesAndEntryFileName(
                webAppSourceTree,
                ["src", "Backend", "Main.elm"]);

        var webServiceApp =
            new MutatingWebServiceApp(webServiceConfig);

        var eventResponse =
            webServiceApp.EventHttpRequest(
                new WebServiceInterface.HttpRequestEventStruct
                (
                    HttpRequestId: "1",
                    PosixTimeMilli: 0,
                    RequestContext: new WebServiceInterface.HttpRequestContext
                    (
                        ClientAddress: null
                    ),
                    Request: new WebServiceInterface.HttpRequestProperties
                    (
                        Method: "GET",
                        Uri: "/",
                        Body: null,
                        Headers: []
                    )
                ));

        var responseCommand = eventResponse.ResponseCommands.Single();

        if (responseCommand.cmdParsed is not WebServiceInterface.Command.RespondToHttpRequest httpResponseCommand)
        {
            throw new Exception("Expected a HTTP response command.");
        }

        var responseBody =
            httpResponseCommand.Respond.Response.Body;

        if (responseBody is null)
        {
            throw new Exception("Expected a response body.");
        }

        var responseContentAsString =
            System.Text.Encoding.UTF8.GetString(responseBody.Value.Span);

        httpResponseCommand.Respond.Response.StatusCode.Should().Be(
            200,
            "Response status code should be OK.\nresponseContentAsString:\n" + responseContentAsString);

        responseContentAsString.Should().Be(
            "Hello, World!",
            "response content as string");
    }

    [TestMethod]
    public async Task Example_app_Elm_editor_webservice_sandbox()
    {
        var elmModuleTextBeforeFormatting =
            """"
            module Common exposing (..)

            a =
                let
                    b =
                        1
                    c =
                        2
                in
                b   +      c
            """";

        var expectedElmModuleTextAfterFormatting =
            """"
            module Common exposing (..)


            a =
                let
                    b =
                        1

                    c =
                        2
                in
                b + c
            """";

        var webAppSource =
            ExampleAppValueFromExampleName("elm-editor");

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

        // Send a request for warmup.

        var formatRequestEventResponse =
            webServiceApp.EventHttpRequest(
                new WebServiceInterface.HttpRequestEventStruct
                (
                    HttpRequestId: "1",
                    PosixTimeMilli: 0,
                    RequestContext: new WebServiceInterface.HttpRequestContext
                    (
                        ClientAddress: null
                    ),
                    Request: new WebServiceInterface.HttpRequestProperties
                    (
                        Method: "POST",
                        Uri: "http://demohost/api",
                        Body: null,
                        Headers: []
                    )
                ));

        for (var i = 0; i < 10; i++)
        {
            await Task.Delay(TimeSpan.FromSeconds(1));

            await volatileProcessHost.ExchangeAsync(webServiceApp);
        }

        var formatHttpRequestId = "123";

        var formatRequest =
            new ElmEditorApi.ElmEditorApiRequestStructure(
                FormatElmModuleTextRequest: [elmModuleTextBeforeFormatting]);

        webServiceApp.EventHttpRequest(
            new WebServiceInterface.HttpRequestEventStruct
            (
                HttpRequestId: formatHttpRequestId,
                PosixTimeMilli: 0,
                RequestContext: new WebServiceInterface.HttpRequestContext
                (
                    ClientAddress: null
                ),
                Request: new WebServiceInterface.HttpRequestProperties
                (
                    Method: "POST",
                    Uri: "http://demohost/api",
                    Body:
                    System.Text.Encoding.UTF8.GetBytes(
                        System.Text.Json.JsonSerializer.Serialize(formatRequest)),
                    Headers: []
                )
            ));

        for (var i = 0; i < 10; i++)
        {
            await Task.Delay(TimeSpan.FromSeconds(1));

            await volatileProcessHost.ExchangeAsync(webServiceApp);
        }

        var httpResponses = webServiceApp.CopyHttpResponses();

        var formatResponseCommand =
            httpResponses
            .FirstOrDefault(cmd => cmd.Respond.HttpRequestId == formatHttpRequestId);

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

        var formatResponseStructure =
            System.Text.Json.JsonSerializer.Deserialize
            <ElmEditorApi.ElmEditorApiResponseStructure>(
                formatResponseContentAsString)!;

        formatResponseStructure.ErrorResponse.Should().BeNull(
            "formatResponseStructure.ErrorResponse should be null.\n" + formatResponseStructure.ErrorResponse);

        var formattedText =
            formatResponseStructure
            ?.FormatElmModuleTextResponse
            ?.FirstOrDefault()
            ?.formattedText
            .WithDefault(null);

        formattedText.Should().NotBeNull(
            "formatResponseStructure.FormatElmModuleTextResponse should contain a formatted text.");

        NormalizeStringTestingElmFormat(formattedText).Should().Be(
            NormalizeStringTestingElmFormat(expectedElmModuleTextAfterFormatting));
    }

    private static string NormalizeStringTestingElmFormat(string originalString) =>
        originalString.Trim().Replace("\n\r", "\n").Replace("\r\n", "\n");


    [TestMethod]
    public void Counter_webapp_Json_adapter()
    {
        var webAppSourceTree =
            PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(
                TestSetup.CounterElmWebApp);

        var loweringResult =
            ElmTime.ElmAppCompilation.AsCompletelyLoweredElmApp(
                PineValueComposition.TreeToFlatDictionaryWithPathComparer(webAppSourceTree),
                workingDirectoryRelative: [],
                ElmTime.ElmAppInterfaceConfig.Default);

        if (loweringResult.IsErrOrNull() is { } loweringErr)
        {
            throw new Exception("Failed lowering: " + loweringErr);
        }

        if (loweringResult.IsOkOrNull() is not { } loweringOk)
        {
            throw new Exception("Unexpected result type: " + loweringResult);
        }

        var loweredTree =
            PineValueComposition.SortedTreeFromSetOfBlobsWithStringPath(loweringOk.Result.CompiledFiles);

        var loweredTreeValue =
            PineValueComposition.FromTreeWithStringPath(loweredTree);

        var webServiceConfig =
            WebServiceInterface.ConfigFromSourceFilesAndEntryFileName(
                loweredTree,
                ["src", "Backend", "Main.elm"]);

        var pineVMCache = new Pine.PineVM.PineVMCache();

        var pineVM = new Pine.PineVM.PineVM(pineVMCache.EvalCache);

        var initStateToJsonValue =
            webServiceConfig.JsonAdapter.EncodeAppStateAsJsonValue(
                webServiceConfig.Init.State,
                pineVM)
            .Extract(err => throw new Exception("Failed encoding app state as JSON: " + err));

        var initStateFromJsonValueElm =
            ElmValueEncoding.PineValueAsElmValue(initStateToJsonValue, null, null)
            .Extract(err => throw new Exception("Failed encoding app state as Elm value: " + err));

        var initStateJsonValueExpr =
            ElmValue.RenderAsElmExpression(initStateFromJsonValueElm);

        initStateJsonValueExpr.expressionString.Should().Be(
            "IntValue 0");

        {
            var decodedAppState =
                webServiceConfig.JsonAdapter.DecodeAppStateFromJsonValue(
                    initStateToJsonValue,
                    pineVM)
                .Extract(err => throw new Exception("Failed decoding app state from JSON: " + err));

            var decodedAppStateElmValue =
                ElmValueEncoding.PineValueAsElmValue(decodedAppState, null, null)
                .Extract(err => throw new Exception("Failed encoding app state as Elm value: " + err));

            ElmValue.RenderAsElmExpression(decodedAppStateElmValue).expressionString.Should().Be(
                "0");
        }

        {
            var decodedAppState =
                webServiceConfig.JsonAdapter.DecodeAppStateFromJsonString(
                    ElmValueEncoding.StringAsPineValue("123"),
                    pineVM)
                .Extract(err => throw new Exception("Failed decoding app state from JSON: " + err));

            var decodedAppStateElmValue =
                ElmValueEncoding.PineValueAsElmValue(decodedAppState, null, null)
                .Extract(err => throw new Exception("Failed encoding app state as Elm value: " + err));

            ElmValue.RenderAsElmExpression(decodedAppStateElmValue).expressionString.Should().Be(
                "123");
        }

        {
            /*
             * Migration function in this example app is the identity function.
             * */

            webServiceConfig.JsonAdapter.JsonDecodeMigratePreviousState.Should().NotBeNull(
                nameof(webServiceConfig.JsonAdapter.JsonDecodeMigratePreviousState) + " should not be null.");

            var decodedAppState =
                webServiceConfig.JsonAdapter.DecodePreviousAppStateFromJsonValue(
                    initStateToJsonValue,
                    pineVM)
                .Extract(err => throw new Exception("Failed decoding previous app state from JSON: " + err));

            var decodedAppStateElmValue =
                ElmValueEncoding.PineValueAsElmValue(decodedAppState, null, null)
                .Extract(err => throw new Exception("Failed encoding app state as Elm value: " + err));

            ElmValue.RenderAsElmExpression(decodedAppStateElmValue).expressionString.Should().Be(
                "0");

            webServiceConfig.JsonAdapter.Migrate.Should().NotBeNull(
                nameof(webServiceConfig.JsonAdapter.Migrate) + " should not be null.");
        }
    }


    [TestMethod]
    public void Example_app_demo_backend_state()
    {
        var webAppSource =
            ExampleAppValueFromExampleName("demo-backend-state");

        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppAndInitElmState: webAppSource);
        using var server = testSetup.StartWebHost();
        using var publicAppClient = testSetup.BuildPublicAppHttpClient();
    }
}
