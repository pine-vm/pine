module ElmFullstack exposing (..)


type BackendEvent
    = HttpRequestEvent HttpRequestEventStructure
    | TaskCompleteEvent TaskCompleteEventStructure
    | PosixTimeHasArrivedEvent { posixTimeMilli : Int }


type alias BackendEventResponse =
    { startTasks : List StartTaskStructure
    , notifyWhenPosixTimeHasArrived : Maybe { minimumPosixTimeMilli : Int }
    , completeHttpResponses : List HttpResponseRequest
    }


type alias TaskCompleteEventStructure =
    { taskId : TaskId
    , taskResult : TaskResultStructure
    }


type TaskResultStructure
    = CreateVolatileProcessResponse (Result CreateVolatileProcessErrorStruct CreateVolatileProcessComplete)
    | RequestToVolatileProcessResponse (Result RequestToVolatileProcessError RequestToVolatileProcessComplete)
    | CompleteWithoutResult


type alias StartTaskStructure =
    { taskId : TaskId
    , task : Task
    }


type Task
    = CreateVolatileProcess CreateVolatileProcessStruct
    | RequestToVolatileProcess RequestToVolatileProcessStruct
    | TerminateVolatileProcess TerminateVolatileProcessStruct


type alias HttpRequestEventStructure =
    { httpRequestId : String
    , posixTimeMilli : Int
    , requestContext : HttpRequestContext
    , request : HttpRequestProperties
    }


type ResponseOverSerialInterface
    = DecodeEventError String
    | DecodeEventSuccess BackendEventResponse


type alias HttpResponseRequest =
    { httpRequestId : String
    , response : HttpResponse
    }


type alias TaskId =
    String


type alias BackendConfiguration state =
    { init : state
    , update : BackendEvent -> state -> ( state, BackendEventResponse )
    }


type alias HttpRequestEventStruct =
    { httpRequestId : String
    , posixTimeMilli : Int
    , requestContext : HttpRequestContext
    , request : HttpRequestProperties
    }


type alias HttpRequestContext =
    { clientAddress : Maybe String
    }


type alias HttpRequestProperties =
    { method : String
    , uri : String
    , bodyAsBase64 : Maybe String
    , headers : List HttpHeader
    }


type alias RespondToHttpRequestStruct =
    { httpRequestId : String
    , response : HttpResponse
    }


type alias HttpResponse =
    { statusCode : Int
    , bodyAsBase64 : Maybe String
    , headersToAdd : List HttpHeader
    }


type alias HttpHeader =
    { name : String
    , values : List String
    }


type alias CreateVolatileProcessStruct =
    { programCode : String
    }


type alias CreateVolatileProcessErrorStruct =
    { exceptionToString : String
    }


type alias CreateVolatileProcessComplete =
    { processId : String }


type alias RequestToVolatileProcessStruct =
    { processId : String
    , request : String
    }


type RequestToVolatileProcessError
    = ProcessNotFound


type alias RequestToVolatileProcessComplete =
    { exceptionToString : Maybe String
    , returnValueToString : Maybe String
    , durationInMilliseconds : Int
    }


type alias TerminateVolatileProcessStruct =
    { processId : String }


passiveBackendEventResponse : BackendEventResponse
passiveBackendEventResponse =
    { startTasks = []
    , completeHttpResponses = []
    , notifyWhenPosixTimeHasArrived = Nothing
    }


withStartTasksAdded : List StartTaskStructure -> BackendEventResponse -> BackendEventResponse
withStartTasksAdded startTasksToAdd responseBefore =
    { responseBefore | startTasks = responseBefore.startTasks ++ startTasksToAdd }


withCompleteHttpResponsesAdded : List HttpResponseRequest -> BackendEventResponse -> BackendEventResponse
withCompleteHttpResponsesAdded httpResponsesToAdd responseBefore =
    { responseBefore | completeHttpResponses = responseBefore.completeHttpResponses ++ httpResponsesToAdd }


concatBackendEventResponse : List BackendEventResponse -> BackendEventResponse
concatBackendEventResponse responses =
    let
        notifyWhenPosixTimeHasArrived =
            responses
                |> List.filterMap .notifyWhenPosixTimeHasArrived
                |> List.map .minimumPosixTimeMilli
                |> List.minimum
                |> Maybe.map (\posixTimeMilli -> { minimumPosixTimeMilli = posixTimeMilli })

        startTasks =
            responses |> List.concatMap .startTasks

        completeHttpResponses =
            responses |> List.concatMap .completeHttpResponses
    in
    { notifyWhenPosixTimeHasArrived = notifyWhenPosixTimeHasArrived
    , startTasks = startTasks
    , completeHttpResponses = completeHttpResponses
    }
