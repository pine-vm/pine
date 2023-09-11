using ElmTime.StateShim.InterfaceToHost;
using Microsoft.VisualStudio.TestTools.UnitTesting;
using Pine;
using Pine.Json;
using System;
using System.Collections.Immutable;
using System.Linq;
using System.Security.Cryptography;
using System.Text.Json;

namespace TestElmTime;

[TestClass]
public class StateShimTests
{
    [TestMethod]
    public void Test_state_shim_with_calculator_app()
    {
        using var testSetup = WebHostAdminInterfaceTestSetup.Setup(
            deployAppAndInitElmState: TestElmWebAppHttpServer.CalculatorWebApp);

        var fileStore = new FileStoreFromSystemIOFile(testSetup.ProcessStoreDirectory);

        var processStore = new ElmTime.Platform.WebService.ProcessStoreSupportingMigrations.ProcessStoreWriterInFileStore(
            fileStore,
            getTimeForCompositionLogBatch:
            () => DateTimeOffset.UtcNow, fileStore);

        using var calculatorProcess = testSetup.BranchProcess()!;

        var estimateStateLengthResult = calculatorProcess.EstimateSerializedStateLengthOnMainBranch();

        var firstEstimatedStateLength = estimateStateLengthResult.Extract(err => throw new Exception(err));

        Assert.IsTrue(0 < firstEstimatedStateLength);

        {
            var applyOperationResult =
                calculatorProcess.ApplyFunctionOnMainBranch(
                    processStore,
                    new ElmTime.AdminInterface.ApplyDatabaseFunctionRequest(
                        functionName: "Backend.ExposeFunctionsToAdmin.applyCalculatorOperation",
                        serializedArgumentsJson: ImmutableList.Create(
                            JsonSerializer.Serialize<CalculatorOperation>(new CalculatorOperation.AddOperation(12345678))),
                        commitResultingState: true));

            applyOperationResult.Extract(err => throw new Exception(err));
        }

        estimateStateLengthResult = calculatorProcess.EstimateSerializedStateLengthOnMainBranch();

        var secondEstimatedStateLength = estimateStateLengthResult.Extract(err => throw new Exception(err));

        var estimatedStateLengthGrowth = secondEstimatedStateLength - firstEstimatedStateLength;

        Assert.IsTrue(3 < estimatedStateLengthGrowth);

        {
            var newBranchesNames = Enumerable.Range(0, 3).Select(i => "test-branch-" + i).ToImmutableList();

            var originalBranchesNames = calculatorProcess.ListBranches().Extract(err => throw new Exception(err));

            var originalBranchName = originalBranchesNames.Single();

            Assert.AreEqual(1, originalBranchesNames.Count);

            {
                var setBranchesResult =
                    calculatorProcess.SetBranchesState(
                        new StateSource.BranchStateSource(originalBranchName),
                        newBranchesNames);

                var branchesNames = calculatorProcess.ListBranches().Extract(err => throw new Exception(err));

                Assert.AreEqual(1 + newBranchesNames.Count, branchesNames.Count);
            }

            {
                var removeBranchesResult =
                    calculatorProcess.RemoveBranches(newBranchesNames.Take(1).ToImmutableList());

                var removeBranchesResultOk = removeBranchesResult.Extract(err => throw new Exception(err));

                Assert.AreEqual(1, removeBranchesResultOk.removedCount);

                var branchesNames = calculatorProcess.ListBranches().Extract(err => throw new Exception(err));

                Assert.AreEqual(1 + newBranchesNames.Count - 1, branchesNames.Count);
            }
        }
    }

    [TestMethod]
    public void Test_state_shim_are_states_equal_with_calculator_app()
    {
        var deployment =
            PineValueComposition.ParseAsTreeWithStringPath(TestElmWebAppHttpServer.CalculatorWebApp)
            .Extract(err => throw new Exception(err.ToString()));

        var preparedProcess =
            ElmTime.Platform.WebService.PersistentProcessLiveRepresentation.ProcessFromDeployment(
                deployment,
                /*
                 * 2023-08-14 Adapt to failures on macOS observed in GitHub Actions:
                 * switch to V8 to avoid stack overflow seen in Jint
                 * */
                overrideJavaScriptEngineFactory:
                ElmTime.JavaScript.JavaScriptEngineFromJavaScriptEngineSwitcher.ConstructJavaScriptEngine);

        using var calculatorProcess = preparedProcess.startProcess();

        Result<string, ElmTime.AdminInterface.ApplyDatabaseFunctionSuccess> applyCalculatorOperationOnBranch(
            string branchName,
            CalculatorOperation calculatorOperation)
        {
            return
                ElmTime.StateShim.StateShim.ApplyFunction(
                    calculatorProcess,
                    new ElmTime.AdminInterface.ApplyDatabaseFunctionRequest(
                        functionName: "Backend.ExposeFunctionsToAdmin.applyCalculatorOperation",
                        serializedArgumentsJson: ImmutableList.Create(
                            JsonSerializer.Serialize(calculatorOperation)),
                        commitResultingState: true),
                    stateSource: Maybe.NothingFromNull<StateSource>(new StateSource.BranchStateSource(branchName)),
                    stateDestinationBranches: ImmutableList.Create(branchName));
        }

        ElmTime.Platform.WebService.PersistentProcessLiveRepresentation.InitBranchesInElmInJsProcess(
            calculatorProcess,
            ImmutableList.Create("alfa", "beta"))
            .Extract(err => throw new Exception(err));

        Assert.IsTrue(
            ElmTime.StateShim.StateShim.TestAreBranchesEqual(calculatorProcess, ImmutableList.Create("alfa", "beta"))
            .Extract(err => throw new Exception(err)));

        {
            var applyOperationResult =
                applyCalculatorOperationOnBranch(
                    "alfa",
                    new CalculatorOperation.AddOperation(1));

            applyOperationResult.Extract(err => throw new Exception(err));
        }

        {
            var applyOperationResult =
                applyCalculatorOperationOnBranch(
                    "alfa",
                    new CalculatorOperation.AddOperation(3));

            applyOperationResult.Extract(err => throw new Exception(err));
        }

        Assert.IsFalse(
            ElmTime.StateShim.StateShim.TestAreBranchesEqual(calculatorProcess, ImmutableList.Create("alfa", "beta"))
            .Extract(err => throw new Exception(err)));


        {
            var applyOperationResult =
                applyCalculatorOperationOnBranch(
                    "beta",
                    new CalculatorOperation.AddOperation(5));

            applyOperationResult.Extract(err => throw new Exception(err));
        }

        {
            var applyOperationResult =
                applyCalculatorOperationOnBranch(
                    "beta",
                    new CalculatorOperation.AddOperation(-1));

            applyOperationResult.Extract(err => throw new Exception(err));
        }

        Assert.IsTrue(
            ElmTime.StateShim.StateShim.TestAreBranchesEqual(calculatorProcess, ImmutableList.Create("alfa", "beta"))
            .Extract(err => throw new Exception(err)));
    }

    [TestMethod]
    public void Test_state_shim_estimate_serialized_state_length()
    {
        var webAppProgram =
            TestSetup.AppConfigComponentFromFiles(
                TestSetup.GetElmAppFromDirectoryPath(
                    ImmutableList.Create(".", "..", "..", "..", "..", "example-apps", "test-database-scale")));

        var deployment =
            PineValueComposition.ParseAsTreeWithStringPath(webAppProgram)
            .Extract(err => throw new Exception(err.ToString()));


        var preparedProcess =
            ElmTime.Platform.WebService.PersistentProcessLiveRepresentation.ProcessFromDeployment(
                deployment,
                /*
                 * 2023-08-14 Adapt to failures on macOS observed in GitHub Actions:
                 * switch to V8 to avoid stack overflow seen in Jint
                 * */
                overrideJavaScriptEngineFactory:
                ElmTime.JavaScript.JavaScriptEngineFromJavaScriptEngineSwitcher.ConstructJavaScriptEngine);

        using var elmProcess = preparedProcess.startProcess();

        Result<string, ElmTime.AdminInterface.ApplyDatabaseFunctionSuccess> setStoreEntryBase64(
            string branchName,
            int entryId,
            string entryBase64)
        {
            return
                ElmTime.StateShim.StateShim.ApplyFunction(
                    elmProcess,
                    new ElmTime.AdminInterface.ApplyDatabaseFunctionRequest(
                        functionName: "Backend.ExposeFunctionsToAdmin.setStoreEntryBase64",
                        serializedArgumentsJson: ImmutableList.Create(
                            JsonSerializer.Serialize(new { entryId, entryBase64 })),
                        commitResultingState: true),
                    stateSource: Maybe.NothingFromNull<StateSource>(new StateSource.BranchStateSource(branchName)),
                    stateDestinationBranches: ImmutableList.Create(branchName));
        }

        ElmTime.Platform.WebService.PersistentProcessLiveRepresentation.InitBranchesInElmInJsProcess(
            elmProcess,
            ImmutableList.Create("main"))
            .Extract(err => throw new Exception(err));

        var stateSizeInitial =
            ElmTime.StateShim.StateShim.EstimateSerializedStateLengthOnBranch(elmProcess, "main")
            .Extract(err => throw new Exception(err));

        var entrySize = 1_000;

        setStoreEntryBase64(
            "main",
            entryId: 1,
            entryBase64: Convert.ToBase64String(RandomNumberGenerator.GetBytes(entrySize)))
            .Extract(err => throw new Exception(err));

        var stateSizeAfterSettingFirstEntry =
            ElmTime.StateShim.StateShim.EstimateSerializedStateLengthOnBranch(elmProcess, "main")
            .Extract(err => throw new Exception(err));

        Assert.IsTrue(stateSizeAfterSettingFirstEntry > stateSizeInitial + entrySize);

        setStoreEntryBase64(
            "main",
            entryId: 2,
            entryBase64: Convert.ToBase64String(RandomNumberGenerator.GetBytes(entrySize)))
            .Extract(err => throw new Exception(err));

        var stateSizeAfterSettingSecondEntry =
            ElmTime.StateShim.StateShim.EstimateSerializedStateLengthOnBranch(elmProcess, "main")
            .Extract(err => throw new Exception(err));

        Assert.IsTrue(stateSizeAfterSettingSecondEntry > stateSizeAfterSettingFirstEntry + entrySize);

        setStoreEntryBase64(
            "main",
            entryId: 1,
            entryBase64: Convert.ToBase64String(Array.Empty<byte>()))
            .Extract(err => throw new Exception(err));

        var stateSizeAfterReplacingFirstEntry =
            ElmTime.StateShim.StateShim.EstimateSerializedStateLengthOnBranch(elmProcess, "main")
            .Extract(err => throw new Exception(err));

        Assert.IsTrue(stateSizeAfterReplacingFirstEntry < stateSizeAfterSettingSecondEntry - entrySize);

        for (int i = 0; i < 1000; ++i)
        {
            setStoreEntryBase64(
                "main",
                entryId: 1000 + i,
                entryBase64: Convert.ToBase64String(RandomNumberGenerator.GetBytes(entrySize)))
                .Extract(err => throw new Exception(err));
        }

        var stateSizeAfterSetting1000MoreEntries =
            ElmTime.StateShim.StateShim.EstimateSerializedStateLengthOnBranch(elmProcess, "main")
            .Extract(err => throw new Exception(err));

        Assert.IsTrue(
            stateSizeAfterSetting1000MoreEntries > stateSizeAfterReplacingFirstEntry + 1000 * entrySize);
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
