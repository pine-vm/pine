using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine.Core;
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
    public void Example_app_demo_backend_state()
    {
        var webAppSource =
            ExampleAppValueFromExampleName("demo-backend-state");

        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(deployAppAndInitElmState: webAppSource);
        using var server = testSetup.StartWebHost();
        using var publicAppClient = testSetup.BuildPublicAppHttpClient();
    }
}
