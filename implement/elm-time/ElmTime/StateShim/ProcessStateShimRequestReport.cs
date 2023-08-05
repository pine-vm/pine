using ElmTime.StateShim.InterfaceToHost;

namespace ElmTime.StateShim;

public record ProcessStateShimRequestReport(
    int SerializeTimeInMilliseconds,
    string SerializedRequest,
    string SerializedResponse,
    StateShimResponseStruct Response,
    int ProcessEventAndSerializeTimeInMilliseconds);
