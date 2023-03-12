using Pine;
using Pine.Json;
using System.Text.Json.Serialization;

namespace ElmTime.Platform.WebServer.InterfaceToHost;

[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record BackendEventStruct
{
    public record PosixTimeHasArrivedEvent(PosixTimeHasArrivedEventStruct Structure)
        : BackendEventStruct;

    public record HttpRequestEvent(HttpRequestEventStruct Struct)
        : BackendEventStruct;

    public record TaskCompleteEvent(ResultFromTaskWithId Result)
        : BackendEventStruct;
}

public record BackendEventResponseStruct(
    Maybe<NotifyWhenPosixTimeHasArrivedRequestStruct> notifyWhenPosixTimeHasArrived,
    StartTask[] startTasks,
    HttpResponseRequest[] completeHttpResponses);

public record HttpRequestEventStruct(
    long posixTimeMilli,
    string httpRequestId,
    HttpRequestContext requestContext,
    HttpRequest request);

public record HttpRequestContext(Maybe<string> clientAddress);

public record HttpRequest(string method, string uri, Maybe<string> bodyAsBase64, HttpHeader[] headers);

public record HttpHeader(string name, string[] values);

public record HttpResponseRequest(string httpRequestId, HttpResponse response);

public record HttpResponse(int statusCode, Maybe<string> bodyAsBase64, HttpHeader[] headersToAdd);

public record PosixTimeHasArrivedEventStruct(long posixTimeMilli);

public record NotifyWhenPosixTimeHasArrivedRequestStruct(long minimumPosixTimeMilli);

public record ResultFromTaskWithId(string taskId, TaskResult taskResult);

[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record TaskResult
{
    public record CreateVolatileProcessResponse(
        Result<CreateVolatileProcessErrorStructure, CreateVolatileProcessComplete> Result)
        : TaskResult;

    public record RequestToVolatileProcessResponse(
        Result<RequestToVolatileProcessError, RequestToVolatileProcessComplete> Result)
        : TaskResult;

    public record CompleteWithoutResult()
        : TaskResult;
}

public record CreateVolatileProcessErrorStructure(string exceptionToString);

public record CreateVolatileProcessComplete(string processId);

public record RequestToVolatileProcessError(object ProcessNotFound);

public record RequestToVolatileProcessComplete(
    Maybe<string> exceptionToString,
    Maybe<string> returnValueToString,
    long durationInMilliseconds);

public record StartTask(string taskId, Task task);

[JsonConverter(typeof(JsonConverterForChoiceType))]
public abstract record Task
{
    public record CreateVolatileProcess(CreateVolatileProcessStruct Create)
        : Task;

    public record RequestToVolatileProcess(RequestToVolatileProcessStruct RequestTo)
        : Task;

    public record TerminateVolatileProcess(TerminateVolatileProcessStruct Terminate)
        : Task;
}

public record CreateVolatileProcessStruct(string programCode);

public record RequestToVolatileProcessStruct(string processId, string request);

public record TerminateVolatileProcessStruct(string processId);
