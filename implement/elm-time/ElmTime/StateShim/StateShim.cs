using ElmTime.StateShim.InterfaceToHost;
using Pine;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text.Json;

namespace ElmTime.StateShim;

public class StateShim
{
    static public StateSource MainStateBranch => new StateSource.BranchStateSource(MainBranchName);

    static public string MainBranchName => "main";

    static public Result<string, string> SetAppStateOnMainBranch(
        IProcess<string, string> process,
        string stateJson) =>
        SetAppState(process: process, stateJson: stateJson, branchName: "main");

    static public Result<string, string> SetAppState(
        IProcess<string, string> process,
        string stateJson,
        string branchName)
    {
        return
            ProcessStateShimRequest(
                process,
                new StateShimRequestStruct.SetBranchesStateShimRequest(
                    new StateSource.SerializedJsonStateSource(stateJson), Branches: ImmutableList.Create(branchName)))
            .AndThen(decodeOk => decodeOk switch
            {
                StateShimResponseStruct.SetBranchesStateShimResponse setBranchesResponse =>
                setBranchesResponse.Result,

                _ =>
                Result<string, string>.err(
                    "Unexpected type of response: " + JsonSerializer.Serialize(decodeOk))
            });
    }

    static public Result<string, string> GetAppStateFromMainBranch(IProcess<string, string> process) =>
        GetSerializedState(process, branchName: MainBranchName);

    static public Result<string, string> GetSerializedState(
        IProcess<string, string> process,
        string branchName)
    {
        return
            ProcessStateShimRequest(
                process,
                new StateShimRequestStruct.SerializeStateShimRequest(new StateSource.BranchStateSource(branchName)))
            .AndThen(decodeOk => decodeOk switch
            {
                StateShimResponseStruct.SerializeStateShimResponse serializeStateResponse =>
                serializeStateResponse.Result,

                _ =>
                Result<string, string>.err(
                    "Unexpected type of response: " + JsonSerializer.Serialize(decodeOk))
            });
    }

    static public Result<string, IReadOnlyList<NamedExposedFunction>> ListExposedFunctions(IProcess<string, string> process)
    {
        return
            ProcessStateShimRequest(
                process,
                new StateShimRequestStruct.ListExposedFunctionsShimRequest())
            .AndThen(decodeOk => decodeOk switch
            {
                StateShimResponseStruct.ListExposedFunctionsShimResponse listExposedFunctionsResponse =>
                Result<string, IReadOnlyList<NamedExposedFunction>>.ok(listExposedFunctionsResponse.Functions),

                _ =>
                Result<string, IReadOnlyList<NamedExposedFunction>>.err(
                    "Unexpected type of response: " + JsonSerializer.Serialize(decodeOk))
            });
    }


    static public Result<string, AdminInterface.ApplyFunctionOnDatabaseSuccess> ApplyFunctionOnMainBranch(
        IProcess<string, string> process,
        AdminInterface.ApplyFunctionOnDatabaseRequest request)
    {
        return
            ListExposedFunctions(process)
            .AndThen(exposedFunctions =>
            {
                var matchingFunctions = exposedFunctions.Where(c => c.functionName == request.functionName).ToImmutableList();

                if (!matchingFunctions.Any())
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
                matchingFunction.functionDescription.hasAppStateParam ?
                Maybe<StateSource>.just(MainStateBranch) :
                Maybe<StateSource>.nothing();

                var stateDestinationBranches =
                    request.commitResultingState ? ImmutableList.Create(MainBranchName) : ImmutableList<string>.Empty;

                var applyFunctionRequest =
                new ApplyFunctionShimRequestStruct(
                    functionName: matchingFunction.functionName,
                    arguments: new ApplyFunctionArguments<Maybe<StateSource>>(
                        stateArgument: stateArgument,
                        serializedArgumentsJson: request.serializedArgumentsJson),
                    stateDestinationBranches: stateDestinationBranches);

                try
                {
                    return
                        ProcessStateShimRequest(
                            process,
                            new StateShimRequestStruct.ApplyFunctionShimRequest(applyFunctionRequest))
                        .AndThen(responseOk => responseOk switch
                        {
                            StateShimResponseStruct.ApplyFunctionShimResponse applyFunctionResponse =>
                            applyFunctionResponse.Result
                            .MapError(err => "Failed to apply function " + request.functionName + ": " + err),

                            _ =>
                            Result<string, FunctionApplicationResult>.err(
                                "Unexpected type of response: " + JsonSerializer.Serialize(responseOk))
                        })
                        .Map(applyFunctionOk => new AdminInterface.ApplyFunctionOnDatabaseSuccess(
                            applyFunctionOk,
                            changedState: stateDestinationBranches.Contains(MainBranchName)));
                }
                catch (Exception e)
                {
                    return Result<string, AdminInterface.ApplyFunctionOnDatabaseSuccess>.err(
                        "Failed to parse response string: " + e.ToString());
                }
            });
    }

    static public Result<string, long> EstimateSerializedStateLengthOnMainBranch(
        IProcess<string, string> process)
    {
        return
            ProcessStateShimRequest(
                process,
                new StateShimRequestStruct.EstimateSerializedStateLengthShimRequest(MainStateBranch))
            .AndThen(responseOk => responseOk switch
            {
                StateShimResponseStruct.EstimateSerializedStateLengthShimResponse estimateResponse =>
                estimateResponse.Result
                .MapError(err => "Failed to estimate serialized state length: " + err),

                _ =>
                Result<string, long>.err(
                    "Unexpected type of response: " + JsonSerializer.Serialize(responseOk))
            });
    }

    static public Result<string, StateShimResponseStruct> ProcessStateShimRequest(
       IProcess<string, string> process,
       StateShimRequestStruct stateShimRequest)
    {
        var serializedInterfaceEvent = stateShimRequest.SerializeToJsonString();

        var responseString = process!.ProcessEvent(serializedInterfaceEvent);

        try
        {
            return JsonSerializer.Deserialize<Result<string, StateShimResponseStruct>>(responseString);
        }
        catch (Exception e)
        {
            return Result<string, StateShimResponseStruct>.err(
                "Failed to parse response string: " + e.ToString());
        }
    }
}
