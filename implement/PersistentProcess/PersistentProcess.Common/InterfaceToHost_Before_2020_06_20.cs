using System;
using System.Linq;

namespace Kalmit.PersistentProcess.InterfaceToHost_Before_2020_06_20
{
    public class Event
    {
        public InterfaceToHost.HttpRequestEvent httpRequest;

        public InterfaceToHost.ResultFromTaskWithId taskComplete;

        public InterfaceToHost.ArrivedAtTimeEventStructure ArrivedAtTimeEvent;

        static public Event FromAppEvent(InterfaceToHost.AppEventStructure appEvent)
        {
            if (appEvent.ArrivedAtTimeEvent != null)
                return new Event { ArrivedAtTimeEvent = appEvent.ArrivedAtTimeEvent };

            if (appEvent.TaskCompleteEvent != null)
                return new Event { taskComplete = appEvent.TaskCompleteEvent };

            if (appEvent.HttpRequestEvent != null)
                return new Event { httpRequest = appEvent.HttpRequestEvent };

            throw new NotImplementedException("Programming error: Case not implemented.");
        }

        public InterfaceToHost.AppEventStructure AsAppEvent()
        {
            if (ArrivedAtTimeEvent != null)
                return new InterfaceToHost.AppEventStructure { ArrivedAtTimeEvent = ArrivedAtTimeEvent };

            if (httpRequest != null)
                return new InterfaceToHost.AppEventStructure { HttpRequestEvent = httpRequest };

            if (taskComplete != null)
                return new InterfaceToHost.AppEventStructure { TaskCompleteEvent = taskComplete };

            throw new NotImplementedException("Programming error: Case not implemented.");
        }
    }

    public class ResponseOverSerialInterface
    {
        public string decodeEventError;

        public ProcessRequest[] decodeEventSuccess;

        public InterfaceToHost.ResponseOverSerialInterface TranslateToNewStructure()
        {
            if (decodeEventSuccess == null)
                return new InterfaceToHost.ResponseOverSerialInterface { DecodeEventError = decodeEventError };

            var notifyWhenArrivedAtTime =
                decodeEventSuccess.Select(request => request.NotifyWhenArrivedAtTimeRequest).WhereNotNull().FirstOrDefault();

            var startTasks =
                decodeEventSuccess.Select(request => request.startTask).WhereNotNull().ToArray();

            var completeHttpResponses =
                decodeEventSuccess.Select(request => request.completeHttpResponse).WhereNotNull().ToArray();

            return new InterfaceToHost.ResponseOverSerialInterface
            {
                DecodeEventSuccess = new InterfaceToHost.AppEventResponseStructure
                {
                    startTasks = startTasks,
                    completeHttpResponses = completeHttpResponses,
                    notifyWhenArrivedAtTime = notifyWhenArrivedAtTime,
                }
            };
        }
    }

    public class ProcessRequest
    {
        public InterfaceToHost.HttpResponseRequest completeHttpResponse;

        public InterfaceToHost.StartTask startTask;

        public InterfaceToHost.NotifyWhenArrivedAtTimeRequestStructure NotifyWhenArrivedAtTimeRequest;
    }
}
