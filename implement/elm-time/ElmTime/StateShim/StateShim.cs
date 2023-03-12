using Pine;
using System.Collections.Immutable;
using System.Text.Json;

namespace ElmTime.StateShim;

public class StateShim
{
    static public Result<string, string> SetSerializedState(
        IProcess<string, string> process,
        string stateJson) =>
        SetSerializedState(process: process, stateJson: stateJson, branchName: "main");

    static public Result<string, string> SetSerializedState(
        IProcess<string, string> process,
        string stateJson,
        string branchName)
    {
        var requestSerial =
            JsonSerializer.Serialize<InterfaceToHost.StateShimRequestStruct>(
                new InterfaceToHost.StateShimRequestStruct.SetBranchesStateShimRequest(
                    new InterfaceToHost.StateSource.SerializedJsonStateSource(stateJson),
                    Branches: ImmutableList.Create(branchName)));

        var responseSerial = process.ProcessEvent(requestSerial);

        var response = JsonSerializer.Deserialize<Result<string, InterfaceToHost.StateShimResponseStruct>>(responseSerial);

        return
            response
            .AndThen(decodeOk => decodeOk switch
            {
                InterfaceToHost.StateShimResponseStruct.SetBranchesStateShimResponse setBranchesResponse =>
                setBranchesResponse.Result,

                _ =>
                Result<string, string>.err(
                    "Unexpected type of response: " + JsonSerializer.Serialize(decodeOk))
            });
    }

    static public Result<string, string> GetSerializedState(IProcess<string, string> process) =>
        GetSerializedState(process, branchName: "main");

    static public Result<string, string> GetSerializedState(
        IProcess<string, string> process,
        string branchName)
    {
        var requestSerial =
            JsonSerializer.Serialize<InterfaceToHost.StateShimRequestStruct>(
                new InterfaceToHost.StateShimRequestStruct.SerializeStateShimRequest(
                    new InterfaceToHost.StateSource.BranchStateSource(branchName)));

        var responseSerial = process.ProcessEvent(requestSerial);

        var response = JsonSerializer.Deserialize<Result<string, InterfaceToHost.StateShimResponseStruct>>(responseSerial);

        return
            response
            .AndThen(decodeOk => decodeOk switch
            {
                InterfaceToHost.StateShimResponseStruct.SerializeStateShimResponse serializeStateResponse =>
                serializeStateResponse.Result,

                _ =>
                Result<string, string>.err(
                    "Unexpected type of response: " + JsonSerializer.Serialize(decodeOk))
            });
    }
}
