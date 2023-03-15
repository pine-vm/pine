using Microsoft.VisualStudio.TestTools.UnitTesting;
using System;
using System.Collections.Immutable;
using System.Linq;
using System.Net.Http.Headers;
using System.Net.Http.Json;
using System.Threading.Tasks;

namespace TestElmTime;

[TestClass]
public class TestRunFunctionOnDatabase
{
    [TestMethod]
    public async Task Apply_exposed_function_via_admin_interface_adding_to_counter_web_app()
    {
        var adminPassword = "test";

        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(
            deployAppAndInitElmState: TestElmWebAppHttpServer.CounterWebApp,
            adminPassword: adminPassword);

        var expectedCounter = 0;

        using (var server = testSetup.StartWebHost())
        {
            using var publicClient = testSetup.BuildPublicAppHttpClient();

            using var adminClient = testSetup.BuildAdminInterfaceHttpClient();

            adminClient.DefaultRequestHeaders.Authorization = new AuthenticationHeaderValue(
                "Basic",
                Convert.ToBase64String(System.Text.Encoding.UTF8.GetBytes(
                    ElmTime.Platform.WebServer.Configuration.BasicAuthenticationForAdmin(adminPassword))));

            {
                var additionViaPublicInterface = 1234;

                var httpResponse =
                    await publicClient.PostAsync("", JsonContent.Create(new { addition = additionViaPublicInterface }));

                var httpResponseContent = await httpResponse.Content.ReadAsStringAsync();

                expectedCounter += additionViaPublicInterface;

                Assert.AreEqual(expectedCounter.ToString(), httpResponseContent, false, "response content");
            }

            {
                var applyFunctionResponse =
                    await adminClient.PostAsJsonAsync(
                        ElmTime.Platform.WebServer.StartupAdminInterface.PathApiApplyFunctionOnDatabase,
                        new ElmTime.AdminInterface.ApplyFunctionOnDatabaseRequest(
                            functionName: "Backend.ExposeFunctionsToAdmin.addToCounter",
                            serializedArgumentsJson: ImmutableList.Create(17.ToString()),
                            commitResultingState: false));

                Assert.AreEqual(200, (int)applyFunctionResponse.StatusCode, message: "applyFunctionResponse.StatusCode");
            }

            {
                var httpResponse = await publicClient.PostAsync("", JsonContent.Create(new { addition = 0 }));

                var httpResponseContent = await httpResponse.Content.ReadAsStringAsync();

                Assert.AreEqual(expectedCounter.ToString(), httpResponseContent, false, "response content");
            }

            {
                var additionViaAdminInterface = 4567;

                var applyFunctionResponse =
                    await adminClient.PostAsJsonAsync(
                        ElmTime.Platform.WebServer.StartupAdminInterface.PathApiApplyFunctionOnDatabase,
                        new ElmTime.AdminInterface.ApplyFunctionOnDatabaseRequest(
                            functionName: "Backend.ExposeFunctionsToAdmin.addToCounter",
                            serializedArgumentsJson: ImmutableList.Create(additionViaAdminInterface.ToString()),
                            commitResultingState: true));

                expectedCounter += additionViaAdminInterface;

                Assert.AreEqual(200, (int)applyFunctionResponse.StatusCode, message: "applyFunctionResponse.StatusCode");
            }

            {
                var httpResponse = await publicClient.PostAsync("", JsonContent.Create(new { addition = 0 }));

                var httpResponseContent = await httpResponse.Content.ReadAsStringAsync();

                Assert.AreEqual(expectedCounter.ToString(), httpResponseContent, false, "response content");
            }
        }

        using (var server = testSetup.StartWebHost())
        {
            using var publicClient = testSetup.BuildPublicAppHttpClient();

            {
                var httpResponse = await publicClient.PostAsync("", JsonContent.Create(new { addition = 0 }));

                var httpResponseContent = await httpResponse.Content.ReadAsStringAsync();

                Assert.AreEqual(expectedCounter.ToString(), httpResponseContent, false, "response content");
            }
        }
    }
}
