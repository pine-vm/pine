using System;

namespace ElmFullstack.WebHost.InterfaceToHost
{
    public record AppEventStructure(
        ArrivedAtTimeEventStructure ArrivedAtTimeEvent = null,
        HttpRequestEvent HttpRequestEvent = null,
        ResultFromTaskWithId TaskCompleteEvent = null,
        object InitStateEvent = null,
        string SetStateEvent = null,
        string MigrateStateEvent = null)
    {
        static public readonly Newtonsoft.Json.JsonSerializerSettings JsonSerializerSettings = new()
        {
            DefaultValueHandling = Newtonsoft.Json.DefaultValueHandling.Ignore,
        };
    }

    public record ResponseOverSerialInterface(string DecodeEventError = null, AppEventResponseStructure DecodeEventSuccess = null);

    public record AppEventResponseStructure(
        NotifyWhenPosixTimeHasArrivedRequestStructure notifyWhenPosixTimeHasArrived,
        StartTask[] startTasks,
        HttpResponseRequest[] completeHttpResponses,
        Maybe<Result<string, object>> migrateResult);

    public record HttpRequestEvent(Int64 posixTimeMilli, string httpRequestId, HttpRequestContext requestContext, HttpRequest request);

    public record HttpRequestContext(string clientAddress);

    public record HttpRequest(string method, string uri, string bodyAsBase64, HttpHeader[] headers);

    public record HttpHeader(string name, string[] values);

    public record HttpResponseRequest(string httpRequestId, HttpResponse response);

    public record HttpResponse(int statusCode, string bodyAsBase64, HttpHeader[] headersToAdd);

    public record ArrivedAtTimeEventStructure(Int64 posixTimeMilli);

    public record NotifyWhenPosixTimeHasArrivedRequestStructure(Int64 minimumPosixTimeMilli);

    public record Result<ErrT, OkT>(ErrT Err = default, OkT Ok = default)
    {
        public Result<ErrT, NewOkT> map<NewOkT>(Func<OkT, NewOkT> mapOk) =>
            Ok != null ?
            new Result<ErrT, NewOkT> { Ok = mapOk(Ok) }
            :
            new Result<ErrT, NewOkT> { Err = Err };

        public Result<NewErrT, OkT> mapErr<NewErrT>(Func<ErrT, NewErrT> mapError) =>
            Ok != null ?
            new Result<NewErrT, OkT> { Ok = Ok }
            :
            new Result<NewErrT, OkT> { Err = mapError(Err) };
    }

    public record Maybe<JustT>(object Nothing = default, JustT Just = default);

    public record ResultFromTaskWithId(string taskId, TaskResult taskResult);

    public record TaskResult(
        Result<CreateVolatileProcessErrorStructure, CreateVolatileProcessComplete> CreateVolatileProcessResponse = null,
        Result<RequestToVolatileProcessError, RequestToVolatileProcessComplete> RequestToVolatileProcessResponse = null,
        object CompleteWithoutResult = null);

    public record CreateVolatileProcessErrorStructure(string exceptionToString);

    public record CreateVolatileProcessComplete(string processId);

    public record RequestToVolatileProcessError(object ProcessNotFound);

    public record RequestToVolatileProcessComplete(string exceptionToString, string returnValueToString, long durationInMilliseconds);

    public record StartTask(string taskId, Task task);

    public record Task(
        CreateVolatileProcessStruct CreateVolatileProcess = null,
        RequestToVolatileProcessStruct RequestToVolatileProcess = null,
        TerminateVolatileProcessStruct TerminateVolatileProcess = null);

    public record CreateVolatileProcessStruct(string programCode);

    public record RequestToVolatileProcessStruct(string processId, string request);

    public record TerminateVolatileProcessStruct(string processId);
}
