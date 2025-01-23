using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
using Pine.Elm.Platform;
using System;
using System.Linq;
using System.Net;

namespace TestElmTime;

[TestClass]
public class ExampleAppsTests
{
    public static PineValue ExampleAppValueFromExampleName(string exampleName) =>
        TestSetup.AppConfigComponentFromFiles(
            TestSetup.GetElmAppFromDirectoryPath(
                [".", "..", "..", "..", "..", "example-apps", exampleName]));

    [TestMethod]
    public void Example_app_minimal_backend_hello_world()
    {
        var webAppSource =
            ExampleAppValueFromExampleName("minimal-backend-hello-world");

        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppAndInitElmState: webAppSource);
        using var server = testSetup.StartWebHost();
        using var publicAppClient = testSetup.BuildPublicAppHttpClient();

        var httpResponse =
            publicAppClient.GetAsync("").Result;

        var responseContentAsString =
            httpResponse.Content.ReadAsStringAsync().Result;

        Assert.AreEqual(
            HttpStatusCode.OK,
            httpResponse.StatusCode,
            "Response status code should be OK.\nresponseContentAsString:\n" + responseContentAsString);

        Assert.AreEqual(
            "Hello, World!",
            responseContentAsString,
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
            WebServiceInterface.ConfigFromSourceFilesAndModuleName(
                webAppSourceTree,
                ["Backend", "Main"]);

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
                        BodyAsBase64: null,
                        Headers: []
                    )
                ));

        var responseCommand = eventResponse.Commands.Single();

        if (responseCommand is not WebServiceInterface.Command.RespondToHttpRequest httpResponseCommand)
        {
            throw new Exception("Expected a HTTP response command.");
        }

        var responseBodyBase64 =
            httpResponseCommand.Respond.Response.BodyAsBase64;

        if (responseBodyBase64 is null)
        {
            throw new Exception("Expected a response body.");
        }

        var responseBody =
            Convert.FromBase64String(responseBodyBase64);

        var responseContentAsString =
            System.Text.Encoding.UTF8.GetString(responseBody);

        Assert.AreEqual(
            200,
            httpResponseCommand.Respond.Response.StatusCode,
            "Response status code should be OK.\nresponseContentAsString:\n" + responseContentAsString);

        Assert.AreEqual(
            "Hello, World!",
            responseContentAsString,
            "response content as string");
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
