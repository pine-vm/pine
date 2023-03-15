using System.Collections.Generic;

namespace ElmTime.AdminInterface;

public record ApplyFunctionOnDatabaseRequest(
    string functionName,
    IReadOnlyList<string> serializedArgumentsJson,
    bool commitResultingState);


public record ApplyFunctionOnDatabaseSuccess(
    StateShim.InterfaceToHost.FunctionApplicationResult functionApplicationResult,
    bool changedState);

