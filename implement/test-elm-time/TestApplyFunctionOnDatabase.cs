using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.Json;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Net.Http.Json;
using System.Text.Json;
using System.Threading.Tasks;

namespace TestElmTime;

[TestClass]
public class TestApplyFunctionOnDatabase
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

            testSetup.SetDefaultRequestHeaderAuthorizeForAdmin(adminClient);

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
                        ElmTime.Platform.WebService.StartupAdminInterface.PathApiApplyDatabaseFunction,
                        new ElmTime.AdminInterface.ApplyDatabaseFunctionRequest(
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
                        ElmTime.Platform.WebService.StartupAdminInterface.PathApiApplyDatabaseFunction,
                        new ElmTime.AdminInterface.ApplyDatabaseFunctionRequest(
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

    [TestMethod]
    public async Task Apply_function_via_admin_interface_report_from_calculator()
    {
        var adminPassword = "test";

        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(
            deployAppAndInitElmState: TestElmWebAppHttpServer.CalculatorWebApp,
            adminPassword: adminPassword);

        var expectedResultingNumber = 0;

        using var server = testSetup.StartWebHost();

        using var publicClient = testSetup.BuildPublicAppHttpClient();

        using var adminClient = testSetup.BuildAdminInterfaceHttpClient();

        testSetup.SetDefaultRequestHeaderAuthorizeForAdmin(adminClient);

        {
            var additionViaPublicInterface = 1234;

            var httpResponse =
                await publicClient.PostAsync(
                    "",
                    JsonContent.Create<CalculatorOperation>(new CalculatorOperation.AddOperation(additionViaPublicInterface)));

            var httpResponseContent = await httpResponse.Content.ReadAsStringAsync();

            expectedResultingNumber += additionViaPublicInterface;

            Assert.AreEqual(
                new CalculatorBackendStateRecord(
                    httpRequestCount: 1,
                    operationsViaHttpRequestCount: 1,
                    resultingNumber: expectedResultingNumber),
                JsonSerializer.Deserialize<CalculatorBackendStateRecord>(httpResponseContent),
                "response content");
        }

        {
            var applyFunctionResponse =
                await adminClient.PostAsJsonAsync(
                    ElmTime.Platform.WebService.StartupAdminInterface.PathApiApplyDatabaseFunction,
                    new ElmTime.AdminInterface.ApplyDatabaseFunctionRequest(
                        functionName: "Backend.ExposeFunctionsToAdmin.customUsageReport",
                        serializedArgumentsJson: ImmutableList.Create(JsonSerializer.Serialize("Hello world")),
                        commitResultingState: false));

            var applyFunctionResponseContent = await applyFunctionResponse.Content.ReadAsStringAsync();

            Assert.AreEqual(200, (int)applyFunctionResponse.StatusCode, message: "applyFunctionResponse.StatusCode");

            var applyFunctionResponseStruct =
                JsonSerializer.Deserialize<Result<string, ElmTime.AdminInterface.ApplyDatabaseFunctionSuccess>>(
                    applyFunctionResponseContent);

            var applyFunctionSuccess =
                applyFunctionResponseStruct
                .Extract(fromErr: err => throw new Exception(err));

            var resultLessStateJson =
                applyFunctionSuccess.functionApplicationResult.resultLessStateJson
                .ToResult("resultLessStateJson is Nothing")
                .Extract(fromErr: err => throw new Exception(err));

            var customReport = JsonSerializer.Deserialize<CustomUsageReportStruct>(resultLessStateJson);

            Assert.AreEqual(
                new CustomUsageReportStruct(
                    httpRequestCount: 1,
                    operationsViaHttpRequestCount: 1,
                    anotherField: "Custom content from argument: Hello world"),
                customReport);
        }
    }

    [TestMethod]
    public async Task List_exposed_functions_via_admin_interface()
    {
        var adminPassword = "test";

        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(
            deployAppAndInitElmState: TestElmWebAppHttpServer.CalculatorWebApp,
            adminPassword: adminPassword);

        using var server = testSetup.StartWebHost();

        using var adminClient = testSetup.BuildAdminInterfaceHttpClient();

        testSetup.SetDefaultRequestHeaderAuthorizeForAdmin(adminClient);

        var listFunctionsResponse =
            await adminClient.GetAsync(ElmTime.Platform.WebService.StartupAdminInterface.PathApiListDatabaseFunctions);

        var listFunctionsResponseContent = await listFunctionsResponse.Content.ReadAsStringAsync();

        Assert.AreEqual(200, (int)listFunctionsResponse.StatusCode, message: "listFunctionsResponse.StatusCode");

        var listFunctionsResponseStruct =
            JsonSerializer.Deserialize<Result<string, IReadOnlyList<ElmTime.StateShim.InterfaceToHost.NamedExposedFunction>>>(
                listFunctionsResponseContent);

        var functionsDescriptions =
            listFunctionsResponseStruct
            .Extract(fromErr: err => throw new Exception(err));

        ElmTime.StateShim.InterfaceToHost.ExposedFunctionDescription? functionDescriptionFromName(string functionName) =>
            functionsDescriptions
            ?.FirstOrDefault(functionDescription => functionDescription.functionName == functionName)
            ?.functionDescription;

        var applyCalculatorOperationFunctionDescription =
            functionDescriptionFromName("Backend.ExposeFunctionsToAdmin.applyCalculatorOperation");

        var customUsageReportFunctionDescription =
            functionDescriptionFromName("Backend.ExposeFunctionsToAdmin.customUsageReport");

        Assert.IsNotNull(applyCalculatorOperationFunctionDescription);
        Assert.IsNotNull(customUsageReportFunctionDescription);

        Assert.AreEqual(
            new ElmTime.StateShim.InterfaceToHost.ExposedFunctionReturnTypeDescription(
                sourceCodeText: "Backend.State.State",
                containsAppStateType: true),
            applyCalculatorOperationFunctionDescription!.returnType);

        CollectionAssert.AreEqual(
            new[]
            {
                new ElmTime.StateShim.InterfaceToHost.ExposedFunctionParameterDescription(
                    patternSourceCodeText: "operation",
                    typeSourceCodeText: "Calculator.CalculatorOperation",
                    typeIsAppStateType: false),
                new ElmTime.StateShim.InterfaceToHost.ExposedFunctionParameterDescription(
                    patternSourceCodeText: "stateBefore",
                    typeSourceCodeText: "Backend.State.State",
                    typeIsAppStateType: true)
            },
            applyCalculatorOperationFunctionDescription!.parameters.ToList());

        Assert.AreEqual(
            new ElmTime.StateShim.InterfaceToHost.ExposedFunctionReturnTypeDescription(
                sourceCodeText: "CustomUsageReport",
                containsAppStateType: false),
            customUsageReportFunctionDescription!.returnType);

        CollectionAssert.AreEqual(
            new[]
            {
                new ElmTime.StateShim.InterfaceToHost.ExposedFunctionParameterDescription(
                    patternSourceCodeText: "customArgument",
                    typeSourceCodeText: "String",
                    typeIsAppStateType: false),
                new ElmTime.StateShim.InterfaceToHost.ExposedFunctionParameterDescription(
                    patternSourceCodeText: "state",
                    typeSourceCodeText: "Backend.State.State",
                    typeIsAppStateType: true),
            },
            customUsageReportFunctionDescription!.parameters.ToList());
    }

    [System.Text.Json.Serialization.JsonConverter(typeof(JsonConverterForChoiceType))]
    private abstract record CalculatorOperation
    {
        public record AddOperation(int Operand)
            : CalculatorOperation;
    }

    private record CalculatorBackendStateRecord(
        int httpRequestCount,
        int operationsViaHttpRequestCount,
        int resultingNumber);

    private record CustomUsageReportStruct(
        int httpRequestCount,
        int operationsViaHttpRequestCount,
        string anotherField);
}
