using System.Collections.Generic;

namespace ElmTime.AdminInterface;

public record ApplyDatabaseFunctionRequest(
    string functionName,
    IReadOnlyList<string> serializedArgumentsJson,
    bool commitResultingState);

public record ApplyDatabaseFunctionSuccess(
    StateShim.InterfaceToHost.FunctionApplicationResult functionApplicationResult,
    StateShim.ProcessStateShimRequestReport processStateShimRequestReport,
    bool committedResultingState);
