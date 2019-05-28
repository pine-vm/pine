using System;

namespace Kalmit.PersistentProcess.InterfaceToHost
{
    public class Event
    {
        public HttpRequestEvent httpRequest;

        public ResultFromTaskWithId taskComplete;
    }

    public class ResponseOverSerialInterface
    {
        public string decodeEventError;

        public ProcessRequest[] decodeEventSuccess;
    }

    public class ProcessRequest
    {
        public HttpResponseRequest completeHttpResponse;

        public StartTask startTask;
    }

    public class HttpRequestEvent
    {
        public Int64 posixTimeMilli;

        public string httpRequestId;

        public HttpRequestContext requestContext;

        public HttpRequest request;
    }

    public class HttpRequestContext
    {
        public string clientAddress;
    }

    public class HttpRequest
    {
        public string method;

        public string uri;

        public string bodyAsString;

        public HttpHeader[] headers;
    }

    public class HttpHeader
    {
        public string name;

        public string[] values;
    }

    public class HttpResponseRequest
    {
        public string httpRequestId;

        public HttpResponse response;
    }

    public class HttpResponse
    {
        public int statusCode;

        public string bodyAsString;

        public HttpHeader[] headersToAdd;
    }

    public class Result<Err, Ok>
    {
        public Err err;

        public Ok ok;
    }

    public class ResultFromTaskWithId
    {
        public string taskId;

        public TaskResult taskResult;
    }

    public class TaskResult
    {
        public Result<object, CreateVolatileHostComplete> createVolatileHostResponse;

        public Result<RunInVolatileHostError, RunInVolatileHostComplete> runInVolatileHostResponse;

        public object completeWithoutResult;

        public class CreateVolatileHostComplete
        {
            public string hostId;
        }

        public class RunInVolatileHostError
        {
            public object hostNotFound;
        }

        public class RunInVolatileHostComplete
        {
            public string exceptionToString;

            public string returnValueToString;

            public long durationInMilliseconds;
        }
    }

    public class StartTask
    {
        public string taskId;

        public Task task;
    }

    public class Task
    {
        public object createVolatileHost;

        public RunInVolatileHost runInVolatileHost;

        public ReleaseVolatileHost releaseVolatileHost;

        public class RunInVolatileHost
        {
            public string hostId;

            public string script;
        }

        public class ReleaseVolatileHost
        {
            public string hostId;
        }
    }
}
