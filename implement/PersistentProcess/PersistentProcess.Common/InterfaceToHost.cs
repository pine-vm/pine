using System;

namespace Kalmit.PersistentProcess.InterfaceToHost
{
    public class Event
    {
        public HttpRequestEvent httpRequest;
    }

    public class ResponseOverSerialInterface
    {
        public string decodeError;

        public Response[] decodeSuccess;
    }

    public class Response
    {
        public HttpResponseResponse completeHttpResponse;
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

    public class HttpResponseResponse
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
}
