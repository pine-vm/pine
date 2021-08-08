using System;

namespace ElmFullstack.InterfaceToHost
{
    public class AppEventStructure
    {
        public ArrivedAtTimeEventStructure ArrivedAtTimeEvent;

        public HttpRequestEvent HttpRequestEvent;

        public ResultFromTaskWithId TaskCompleteEvent;
    }

    public class ResponseOverSerialInterface
    {
        public string DecodeEventError;

        public AppEventResponseStructure DecodeEventSuccess;
    }

    public class AppEventResponseStructure
    {
        public NotifyWhenArrivedAtTimeRequestStructure notifyWhenArrivedAtTime;

        public NotifyWhenPosixTimeHasArrivedRequestStructure notifyWhenPosixTimeHasArrived;

        public StartTask[] startTasks;

        public HttpResponseRequest[] completeHttpResponses;
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

        public string bodyAsBase64;

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

        public string bodyAsBase64;

        public HttpHeader[] headersToAdd;
    }

    public class ArrivedAtTimeEventStructure
    {
        public Int64 posixTimeMilli;
    }

    /// <summary>
    /// TODO: Clean up: Remove NotifyWhenArrivedAtTimeRequestStructure
    /// </summary>
    public class NotifyWhenArrivedAtTimeRequestStructure
    {
        public Int64 posixTimeMilli;
    }

    public class NotifyWhenPosixTimeHasArrivedRequestStructure
    {
        public Int64 minimumPosixTimeMilli;
    }

    public class Result<ErrT, OkT>
    {
        public ErrT Err;

        public OkT Ok;

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

    public class ResultFromTaskWithId
    {
        public string taskId;

        public TaskResult taskResult;
    }

    public class TaskResult
    {
        public Result<CreateVolatileHostErrorStructure, CreateVolatileHostComplete> CreateVolatileHostResponse;

        public Result<RequestToVolatileHostError, RequestToVolatileHostComplete> RequestToVolatileHostResponse;

        public Result<CreateVolatileProcessErrorStructure, CreateVolatileProcessComplete> CreateVolatileProcessResponse;

        public Result<RequestToVolatileProcessError, RequestToVolatileProcessComplete> RequestToVolatileProcessResponse;

        public object CompleteWithoutResult;

        /*
         * TODO: Clean up: Remove CreateVolatileHostErrorStructure, CreateVolatileHostComplete, RequestToVolatileHostError, RequestToVolatileHostComplete
         * */

        public class CreateVolatileHostErrorStructure
        {
            public string exceptionToString;
        }

        public class CreateVolatileHostComplete
        {
            public string hostId;

            public CreateVolatileHostComplete(CreateVolatileProcessComplete complete)
            {
                hostId = complete.processId;
            }
        }

        public class RequestToVolatileHostError
        {
            public object HostNotFound;

            public RequestToVolatileHostError(RequestToVolatileProcessError error)
            {
                HostNotFound = error.ProcessNotFound;
            }
        }

        public class RequestToVolatileHostComplete
        {
            public string exceptionToString;

            public string returnValueToString;

            public long durationInMilliseconds;

            public RequestToVolatileHostComplete(RequestToVolatileProcessComplete complete)
            {
                exceptionToString = complete.exceptionToString;
                returnValueToString = complete.returnValueToString;
                durationInMilliseconds = complete.durationInMilliseconds;
            }
        }

        public class CreateVolatileProcessErrorStructure
        {
            public string exceptionToString;
        }

        public class CreateVolatileProcessComplete
        {
            public string processId;
        }

        public class RequestToVolatileProcessError
        {
            public object ProcessNotFound;
        }

        public class RequestToVolatileProcessComplete
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
        public CreateVolatileHostStructure CreateVolatileHost;

        public RequestToVolatileHostStructure RequestToVolatileHost;

        public ReleaseVolatileHostStructure ReleaseVolatileHost;


        public CreateVolatileProcessStruct CreateVolatileProcess;

        public RequestToVolatileProcessStruct RequestToVolatileProcess;

        public TerminateVolatileProcessStruct TerminateVolatileProcess;

        /*
         * TODO: Clean up after migrating apps in production: Remove CreateVolatileHostStructure, RequestToVolatileHostStructure, ReleaseVolatileHostStructure
         * 
         * */

        public class CreateVolatileHostStructure
        {
            public string script;
        }

        public class RequestToVolatileHostStructure
        {
            public string hostId;

            public string request;
        }

        public class ReleaseVolatileHostStructure
        {
            public string hostId;
        }

        public class CreateVolatileProcessStruct
        {
            public string programCode;
        }

        public class RequestToVolatileProcessStruct
        {
            public string processId;

            public string request;
        }

        public class TerminateVolatileProcessStruct
        {
            public string processId;
        }
    }
}
