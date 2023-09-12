using ElmTime.StateShim.InterfaceToHost;
using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Diagnostics;
using System.Linq;
using System.Text.Json;

namespace ElmTime.StateShim;

public class StateShim
{
    public static StateSource MainStateBranch => new StateSource.BranchStateSource(MainBranchName);

    public static string MainBranchName => "main";

    public static Result<string, string> SetAppStateOnMainBranch(
        IProcess<string, string> process,
        JsonElement stateJson) =>
        SetAppState(process: process, stateJson: stateJson, branchName: "main");

    public static Result<string, string> SetAppState(
        IProcess<string, string> process,
        JsonElement stateJson,
        string branchName)
    {
        return
            ProcessStateShimRequest(
                process,
                new StateShimRequestStruct.SetBranchesStateShimRequest(
                    new StateSource.JsonStateSource(stateJson), Branches: [branchName]))
            .AndThen(decodeOk => decodeOk.Response switch
            {
                StateShimResponseStruct.SetBranchesStateShimResponse setBranchesResponse =>
                setBranchesResponse.Result,

                _ =>
                Result<string, string>.err(
                    "Unexpected type of response: " + JsonSerializer.Serialize(decodeOk))
            });
    }

    public static Result<string, JsonElement> GetAppStateFromMainBranch(IProcess<string, string> process) =>
        GetSerializedState(process, branchName: MainBranchName);

    public static Result<string, JsonElement> GetSerializedState(
        IProcess<string, string> process,
        string branchName)
    {
        return
            ProcessStateShimRequest(
                process,
                new StateShimRequestStruct.SerializeStateShimRequest(new StateSource.BranchStateSource(branchName)))
            .AndThen(decodeOk => decodeOk.Response switch
            {
                StateShimResponseStruct.SerializeStateShimResponse serializeStateResponse =>
                serializeStateResponse.Result,

                _ =>
                Result<string, JsonElement>.err(
                    "Unexpected type of response: " + JsonSerializer.Serialize(decodeOk))
            });
    }

    public static Result<string, IReadOnlyList<NamedExposedFunction>> ListExposedFunctions(IProcess<string, string> process)
    {
        return
            ProcessStateShimRequest(
                process,
                new StateShimRequestStruct.ListExposedFunctionsShimRequest())
            .AndThen(decodeOk => decodeOk.Response switch
            {
                StateShimResponseStruct.ListExposedFunctionsShimResponse listExposedFunctionsResponse =>
                Result<string, IReadOnlyList<NamedExposedFunction>>.ok(listExposedFunctionsResponse.Functions),

                _ =>
                Result<string, IReadOnlyList<NamedExposedFunction>>.err(
                    "Unexpected type of response: " + JsonSerializer.Serialize(decodeOk))
            });
    }

    public static Result<string, AdminInterface.ApplyDatabaseFunctionSuccess> ApplyFunctionOnMainBranch(
        IProcess<string, string> process,
        AdminInterface.ApplyDatabaseFunctionRequest request) =>
        ApplyFunction(
            process,
            request: request,
            stateSource: Maybe<StateSource>.just(MainStateBranch),
            stateDestinationBranches: request.commitResultingState ? [MainBranchName] : []);

    public static Result<string, AdminInterface.ApplyDatabaseFunctionSuccess> ApplyFunction(
        IProcess<string, string> process,
        AdminInterface.ApplyDatabaseFunctionRequest request,
        Maybe<StateSource> stateSource,
        IReadOnlyList<string> stateDestinationBranches)
    {
        return
            request.serializedArgumentsJson
            .Select((asString, argumentIndex) =>
            CommonConversion.CatchExceptionAsResultErr<Exception, JsonElement>(
                () => JsonSerializer.Deserialize<JsonElement>(asString))
            .MapError(exception => "Parsing argument " + argumentIndex + " failed: " + exception))
            .ListCombine()
            .AndThen(serializedArgumentsJson =>
            ListExposedFunctions(process)
            .AndThen(exposedFunctions =>
            {
                var matchingFunctions = exposedFunctions.Where(c => c.functionName == request.functionName).ToImmutableList();

                if (matchingFunctions.IsEmpty)
                    return Result<string, NamedExposedFunction>.err(
                        "None of the exposed functions matches name '" + request.functionName +
                        "'. This app only exposes the following " + exposedFunctions.Count + " functions: "
                        + string.Join(", ", exposedFunctions.Select(ef => ef.functionName)));

                return
                Result<string, NamedExposedFunction>.ok(matchingFunctions.First());
            })
            .AndThen(matchingFunction =>
            {
                var stateArgument =
                matchingFunction.functionDescription.parameters.Any(param => param.typeIsAppStateType) ?
                stateSource :
                Maybe<StateSource>.nothing();

                var applyFunctionRequest =
                new ApplyFunctionShimRequestStruct(
                    functionName: matchingFunction.functionName,
                    arguments: new ApplyFunctionArguments<Maybe<StateSource>>(
                        stateArgument: stateArgument,
                        serializedArgumentsJson: serializedArgumentsJson),
                    stateDestinationBranches: stateDestinationBranches);

                try
                {
                    return
                        ProcessStateShimRequest(
                            process,
                            new StateShimRequestStruct.ApplyFunctionShimRequest(applyFunctionRequest))
                        .AndThen(responseOk => responseOk.Response switch
                        {
                            StateShimResponseStruct.ApplyFunctionShimResponse applyFunctionResponse =>
                            applyFunctionResponse.Result
                            .MapError(err => "Failed to apply function " + request.functionName + ": " + err)
                            .Map(applyFunctionOk => new AdminInterface.ApplyDatabaseFunctionSuccess(
                                applyFunctionOk,
                                processStateShimRequestReport: responseOk,
                                committedResultingState: stateDestinationBranches.Contains(MainBranchName))),

                            _ =>
                            Result<string, AdminInterface.ApplyDatabaseFunctionSuccess>.err(
                                "Unexpected type of response: " + JsonSerializer.Serialize(responseOk))
                        });
                }
                catch (Exception e)
                {
                    return Result<string, AdminInterface.ApplyDatabaseFunctionSuccess>.err(
                        "Failed to parse response string: " + e);
                }
            }));
    }

    public static Result<string, long> EstimateSerializedStateLengthOnMainBranch(
        IProcess<string, string> process) =>
        EstimateSerializedStateLengthOnBranch(process, MainBranchName);

    public static Result<string, long> EstimateSerializedStateLengthOnBranch(
        IProcess<string, string> process,
        string branchName)
    {
        return
            ProcessStateShimRequest(
                process,
                new StateShimRequestStruct.EstimateSerializedStateLengthShimRequest(new StateSource.BranchStateSource(branchName)))
            .AndThen(responseOk => responseOk.Response switch
            {
                StateShimResponseStruct.EstimateSerializedStateLengthShimResponse estimateResponse =>
                estimateResponse.Result
                .MapError(err => "Failed to estimate serialized state length: " + err),

                _ =>
                Result<string, long>.err(
                    "Unexpected type of response: " + JsonSerializer.Serialize(responseOk))
            });
    }

    public static Result<string, IReadOnlyList<string>> ListBranches(
        IProcess<string, string> process)
    {
        return
            ProcessStateShimRequest(
                process,
                new StateShimRequestStruct.ListBranchesShimRequest())
            .AndThen(responseOk => responseOk.Response switch
            {
                StateShimResponseStruct.ListBranchesShimResponse listBranchesResponse =>
                Result<string, IReadOnlyList<string>>.ok(listBranchesResponse.BranchesNames),

                _ =>
                Result<string, IReadOnlyList<string>>.err(
                    "Unexpected type of response: " + JsonSerializer.Serialize(responseOk))
            });
    }

    public static Result<string, string> SetBranchesState(
        IProcess<string, string> process,
        StateSource stateSource,
        IReadOnlyList<string> branches)
    {
        return
            ProcessStateShimRequest(
                process,
                new StateShimRequestStruct.SetBranchesStateShimRequest(StateSource: stateSource, Branches: branches))
            .AndThen(responseOk => responseOk.Response switch
            {
                StateShimResponseStruct.SetBranchesStateShimResponse setBranchesResponse =>
                setBranchesResponse.Result,

                _ =>
                Result<string, string>.err(
                    "Unexpected type of response: " + JsonSerializer.Serialize(responseOk))
            });
    }

    public static Result<string, RemoveBranchesShimResponseStruct> RemoveBranches(
        IProcess<string, string> process,
        IReadOnlyList<string> branches)
    {
        return
            ProcessStateShimRequest(
                process,
                new StateShimRequestStruct.RemoveBranchesShimRequest(BranchesNames: branches))
            .AndThen(responseOk => responseOk.Response switch
            {
                StateShimResponseStruct.RemoveBranchesShimResponse removeBranchesResponse =>
                Result<string, RemoveBranchesShimResponseStruct>.ok(removeBranchesResponse.ResponseStruct),

                _ =>
                Result<string, RemoveBranchesShimResponseStruct>.err(
                    "Unexpected type of response: " + JsonSerializer.Serialize(responseOk))
            });
    }

    public static Result<string, bool> TestAreBranchesEqual(
        IProcess<string, string> process,
        IReadOnlyList<string> branches) =>
        TestAreStatesEqual(
            process,
            branches.Select(branch => new StateSource.BranchStateSource(branch)).ToImmutableList());

    public static Result<string, bool> TestAreStatesEqual(
        IProcess<string, string> process,
        IReadOnlyList<StateSource> statesSources)
    {
        return
            ProcessStateShimRequest(
                process,
                new StateShimRequestStruct.TestAreStatesEqualRequest(statesSources))
            .AndThen(responseOk => responseOk.Response switch
            {
                StateShimResponseStruct.TestAreStatesEqualResponse testAreEqualResponse =>
                testAreEqualResponse.Result,

                _ =>
                Result<string, bool>.err(
                    "Unexpected type of response: " + JsonSerializer.Serialize(responseOk))
            });
    }

    public static Result<string, ProcessStateShimRequestReport> ProcessStateShimRequest(
       IProcess<string, string> process,
       StateShimRequestStruct stateShimRequest)
    {
        var serializeStopwatch = Stopwatch.StartNew();

        var serializedInterfaceEvent = stateShimRequest.SerializeToJsonString();

        serializeStopwatch.Stop();

        var processEventAndSerializeStopwatch = Stopwatch.StartNew();

        var responseString = process!.ProcessEvent(serializedInterfaceEvent);

        processEventAndSerializeStopwatch.Stop();

        try
        {
            return
                JsonSerializer.Deserialize<Result<string, StateShimResponseStruct>>(responseString)
                .Map(responseOk =>
                new ProcessStateShimRequestReport(
                SerializeTimeInMilliseconds: (int)serializeStopwatch.ElapsedMilliseconds,
                SerializedRequest: serializedInterfaceEvent,
                SerializedResponse: responseString,
                Response: responseOk,
                ProcessEventAndSerializeTimeInMilliseconds: (int)processEventAndSerializeStopwatch.ElapsedMilliseconds));
        }
        catch (Exception e)
        {
            return Result<string, ProcessStateShimRequestReport>.err(
                "Failed to parse response string: " + e);
        }
    }
}
