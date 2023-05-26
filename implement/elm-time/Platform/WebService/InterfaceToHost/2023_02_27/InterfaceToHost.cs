using System;

namespace ElmTime.Platform.WebService.InterfaceToHost._2023_02_27;

public record AppEventStructure(
    ArrivedAtTimeEventStructure? ArrivedAtTimeEvent = null,
    HttpRequestEvent? HttpRequestEvent = null,
    ResultFromTaskWithId? TaskCompleteEvent = null,
    object? InitStateEvent = null,
    string? SetStateEvent = null,
    string? MigrateStateEvent = null)
{
    public static readonly System.Text.Json.JsonSerializerOptions JsonSerializerSettings = new()
    {
        DefaultIgnoreCondition = System.Text.Json.Serialization.JsonIgnoreCondition.WhenWritingDefault
    };
}

public record ResponseOverSerialInterface(string? DecodeEventError = null, AppEventResponseStructure? DecodeEventSuccess = null);

public record AppEventResponseStructure(
    NotifyWhenPosixTimeHasArrivedRequestStructure notifyWhenPosixTimeHasArrived,
    StartTask[] startTasks,
    HttpResponseRequest[] completeHttpResponses,
    Maybe<Result<string, object>> migrateResult);

public record HttpRequestEvent(long posixTimeMilli, string httpRequestId, HttpRequestContext requestContext, HttpRequest request);

public record HttpRequestContext(string? clientAddress);

public record HttpRequest(string method, string uri, string bodyAsBase64, HttpHeader[] headers);

public record HttpResponseRequest(string httpRequestId, HttpResponse response);

public record HttpResponse(int statusCode, string bodyAsBase64, HttpHeader[] headersToAdd);

public record ArrivedAtTimeEventStructure(long posixTimeMilli);

public record NotifyWhenPosixTimeHasArrivedRequestStructure(long minimumPosixTimeMilli);

public record Result<ErrT, OkT>(ErrT? Err = default, OkT? Ok = default)
{
    public Pine.Result<ErrT, OkT> AsPineResult()
    {
        if (Ok is { } ok)
            return Pine.Result<ErrT, OkT>.ok(ok);

        if (Err is { } err)
            return Pine.Result<ErrT, OkT>.err(err);

        throw new NotImplementedException();
    }
}

public record Maybe<JustT>(object? Nothing = default, JustT? Just = default)
{
    public Pine.Maybe<JustT> AsPineMaybe()
    {
        if (Just is { } just)
            return Pine.Maybe<JustT>.just(just);

        return Pine.Maybe<JustT>.nothing();
    }
}

public record ResultFromTaskWithId(string taskId, TaskResult taskResult);

public record TaskResult(
    Result<CreateVolatileProcessErrorStructure, CreateVolatileProcessComplete>? CreateVolatileProcessResponse = null,
    Result<RequestToVolatileProcessError, RequestToVolatileProcessComplete>? RequestToVolatileProcessResponse = null,
    object? CompleteWithoutResult = null);

public record RequestToVolatileProcessComplete(string? exceptionToString, string? returnValueToString, long durationInMilliseconds);

public record StartTask(string taskId, Task task);

public record Task(
    CreateVolatileProcessStruct? CreateVolatileProcess = null,
    RequestToVolatileProcessStruct? RequestToVolatileProcess = null,
    TerminateVolatileProcessStruct? TerminateVolatileProcess = null);

public record CreateVolatileProcessStruct(string programCode);

public record RequestToVolatileProcessStruct(string processId, string request);

public record TerminateVolatileProcessStruct(string processId);
