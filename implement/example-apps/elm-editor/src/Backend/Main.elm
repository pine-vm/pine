module Backend.Main exposing
    ( State
    , webServiceMain
    )

import Backend.Route
import Backend.VolatileProcess as VolatileProcess
import Base64
import Bytes
import Bytes.Decode
import Bytes.Encode
import Common
import CompilationInterface.ElmMake
import CompilationInterface.SourceFiles
import Dict
import FileTree
import Platform.WebService
import Set
import Url


type Event
    = HttpRequestEvent Platform.WebService.HttpRequestEventStruct
    | CreateVolatileProcessResponse Platform.WebService.CreateVolatileProcessResult
    | RequestToVolatileProcessResponse String Platform.WebService.RequestToVolatileProcessResult


type alias State =
    { posixTimeMilli : Int
    , volatileProcessesIds : Set.Set String
    , pendingHttpRequests : List Platform.WebService.HttpRequestEventStruct
    , pendingTasksForRequestVolatileProcess : Dict.Dict String { volatileProcessId : String, startPosixTimeMilli : Int }
    }


parallelVolatileProcessesCount : Int
parallelVolatileProcessesCount =
    3


requestToVolatileProcessTimeoutMilliseconds : Int
requestToVolatileProcessTimeoutMilliseconds =
    1000 * 30


webServiceMain : Platform.WebService.WebServiceConfig State
webServiceMain =
    { init = ( initState, [] )
    , subscriptions = subscriptions
    }


initState : State
initState =
    { posixTimeMilli = 0
    , volatileProcessesIds = Set.empty
    , pendingHttpRequests = []
    , pendingTasksForRequestVolatileProcess = Dict.empty
    }


subscriptions : State -> Platform.WebService.Subscriptions State
subscriptions _ =
    { httpRequest = updateForHttpRequestEvent
    , posixTimeIsPast = Nothing
    }


updateForHttpRequestEvent :
    Platform.WebService.HttpRequestEventStruct
    -> State
    -> ( State, Platform.WebService.Commands State )
updateForHttpRequestEvent httpRequestEvent state =
    processEvent (HttpRequestEvent httpRequestEvent) state


processEvent : Event -> State -> ( State, Platform.WebService.Commands State )
processEvent hostEvent stateBefore =
    let
        ( ( state, toVolatileProcessesCmds ), responseCmds ) =
            updateExceptRequestsToVolatileProcess hostEvent stateBefore
                |> Tuple.mapFirst updatePartRequestsToVolatileProcess
    in
    ( state
    , responseCmds ++ toVolatileProcessesCmds
    )


updatePartRequestsToVolatileProcess : State -> ( State, Platform.WebService.Commands State )
updatePartRequestsToVolatileProcess stateBefore =
    let
        tasksToEnsureEnoughVolatileProcessesCreated =
            if Set.size stateBefore.volatileProcessesIds < parallelVolatileProcessesCount then
                [ Platform.WebService.CreateVolatileProcess
                    { programCode = VolatileProcess.volatileProcessProgramCode
                    , update =
                        \createVolatileProcessResult state ->
                            processEvent
                                (CreateVolatileProcessResponse createVolatileProcessResult)
                                state
                    }
                ]

            else
                []

        pendingHttpRequestsWithTaskId =
            stateBefore.pendingHttpRequests
                |> List.map (\httpRequest -> ( taskIdForHttpRequest httpRequest, httpRequest ))

        pendingTasksForRequestVolatileProcessAfterTimeLimit =
            stateBefore.pendingTasksForRequestVolatileProcess
                |> Dict.filter
                    (\_ pendingTask ->
                        stateBefore.posixTimeMilli < pendingTask.startPosixTimeMilli + requestToVolatileProcessTimeoutMilliseconds
                    )

        pendingHttpRequestsWithoutPendingRequestToVolatileProcess =
            pendingHttpRequestsWithTaskId
                |> List.filter
                    (\( taskId, _ ) -> pendingTasksForRequestVolatileProcessAfterTimeLimit |> Dict.member taskId |> not)

        freeVolatileProcessesIds =
            stateBefore.volatileProcessesIds
                |> Set.filter
                    (\volatileProcessId ->
                        pendingTasksForRequestVolatileProcessAfterTimeLimit
                            |> Dict.values
                            |> List.any (.volatileProcessId >> (==) volatileProcessId)
                            |> not
                    )

        continueWithoutRequestToVolatileProcess =
            ( stateBefore, tasksToEnsureEnoughVolatileProcessesCreated )
    in
    case pendingHttpRequestsWithoutPendingRequestToVolatileProcess |> List.head of
        Nothing ->
            continueWithoutRequestToVolatileProcess

        Just ( taskId, httpRequestEvent ) ->
            case List.head (Set.toList freeVolatileProcessesIds) of
                Nothing ->
                    continueWithoutRequestToVolatileProcess

                Just volatileProcessId ->
                    let
                        task =
                            Platform.WebService.RequestToVolatileProcess
                                { processId = volatileProcessId
                                , request =
                                    case httpRequestEvent.request.body of
                                        Nothing ->
                                            "Error: No body in request"

                                        Just body ->
                                            case Bytes.Decode.decode (Bytes.Decode.string (Bytes.width body)) body of
                                                Nothing ->
                                                    "Error: Failed to decode body"

                                                Just bodyString ->
                                                    bodyString
                                , update =
                                    \requestToVolatileProcessResponse state ->
                                        processEvent
                                            (RequestToVolatileProcessResponse taskId requestToVolatileProcessResponse)
                                            state
                                }

                        pendingTasksForRequestVolatileProcess =
                            pendingTasksForRequestVolatileProcessAfterTimeLimit
                                |> Dict.insert taskId
                                    { startPosixTimeMilli = stateBefore.posixTimeMilli
                                    , volatileProcessId = volatileProcessId
                                    }
                    in
                    ( { stateBefore
                        | pendingTasksForRequestVolatileProcess = pendingTasksForRequestVolatileProcess
                      }
                    , [ task ]
                    )


updateExceptRequestsToVolatileProcess : Event -> State -> ( State, Platform.WebService.Commands State )
updateExceptRequestsToVolatileProcess hostEvent stateBefore =
    case hostEvent of
        HttpRequestEvent httpRequestEvent ->
            updateForHttpRequestEventExceptRequestsToVolatileProcess httpRequestEvent stateBefore

        RequestToVolatileProcessResponse taskId requestToVolatileProcessResponse ->
            updateForRequestToVolatileProcessResult taskId requestToVolatileProcessResponse stateBefore

        CreateVolatileProcessResponse createVolatileProcessResponse ->
            updateForCreateVolatileProcessResult createVolatileProcessResponse stateBefore


updateForHttpRequestEventExceptRequestsToVolatileProcess :
    Platform.WebService.HttpRequestEventStruct
    -> State
    -> ( State, Platform.WebService.Commands State )
updateForHttpRequestEventExceptRequestsToVolatileProcess httpRequestEvent stateBeforeUpdatingTime =
    let
        stateBefore : State
        stateBefore =
            { stateBeforeUpdatingTime
                | posixTimeMilli = httpRequestEvent.posixTimeMilli
            }

        staticContentHttpHeaders { contentType, contentEncoding } =
            { cacheMaxAgeMinutes = Just (60 * 24)
            , contentType = contentType
            , contentEncoding = contentEncoding
            }

        httpResponseOkWithBody :
            Maybe Bytes.Bytes
            ->
                { contentType : String
                , contentEncoding : Maybe String
                , cacheMaxAgeMinutes : Maybe Int
                }
            -> Platform.WebService.HttpResponse
        httpResponseOkWithBody body contentConfig =
            { statusCode = 200
            , body = body
            , headersToAdd =
                [ ( "Cache-Control"
                  , contentConfig.cacheMaxAgeMinutes
                        |> Maybe.map (\maxAgeMinutes -> "public, max-age=" ++ String.fromInt (maxAgeMinutes * 60))
                  )
                , ( "Content-Type", Just contentConfig.contentType )
                , ( "Content-Encoding", contentConfig.contentEncoding )
                ]
                    |> List.concatMap
                        (\( name, maybeValue ) ->
                            maybeValue
                                |> Maybe.map (\value -> [ { name = name, values = [ value ] } ])
                                |> Maybe.withDefault []
                        )
            }

        httpResponseOkWithStringContent :
            String
            ->
                { contentType : String
                , contentEncoding : Maybe String
                , cacheMaxAgeMinutes : Maybe Int
                }
            -> Platform.WebService.HttpResponse
        httpResponseOkWithStringContent stringContent httpResponseHeaders =
            httpResponseOkWithBody (Just (bodyFromString stringContent)) httpResponseHeaders

        frontendHtmlDocumentResponse frontendConfig =
            httpResponseOkWithStringContent (frontendHtmlDocument frontendConfig)
                (staticContentHttpHeaders { contentType = "text/html", contentEncoding = Nothing })

        continueWithStaticHttpResponse httpResponse =
            ( stateBefore
            , [ Platform.WebService.RespondToHttpRequest
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response = httpResponse
                    }
              ]
            )
    in
    case Url.fromString httpRequestEvent.request.uri of
        Nothing ->
            continueWithStaticHttpResponse
                { statusCode = 500
                , body =
                    Just
                        (bodyFromString
                            ("Failed to parse URL ("
                                ++ httpRequestEvent.request.uri
                                ++ ")"
                            )
                        )
                , headersToAdd = []
                }

        Just url ->
            case
                CompilationInterface.SourceFiles.file_tree____static
                    |> mapFileTreeNodeFromSource
                    |> FileTree.flatListOfBlobsFromFileTreeNode
                    |> List.filter (Tuple.first >> (==) (String.split "/" url.path |> List.filter (String.isEmpty >> not)))
                    |> List.head
            of
                Just ( filePath, matchingFile ) ->
                    let
                        contentType : String
                        contentType =
                            filePath
                                |> List.concatMap (String.split ".")
                                |> List.reverse
                                |> List.head
                                |> Maybe.andThen (Dict.get >> (|>) contentTypeFromStaticFileExtensionDict)
                                |> Maybe.withDefault ""
                    in
                    httpResponseOkWithBody
                        (Base64.toBytes matchingFile.base64)
                        (staticContentHttpHeaders { contentType = contentType, contentEncoding = Nothing })
                        |> continueWithStaticHttpResponse

                Nothing ->
                    case url |> Backend.Route.routeFromUrl of
                        Nothing ->
                            frontendHtmlDocumentResponse { debug = False }
                                |> continueWithStaticHttpResponse

                        Just (Backend.Route.StaticFileRoute (Backend.Route.FrontendHtmlDocumentRoute frontendConfig)) ->
                            frontendHtmlDocumentResponse frontendConfig
                                |> continueWithStaticHttpResponse

                        Just (Backend.Route.StaticFileRoute (Backend.Route.FrontendElmJavascriptRoute { debug })) ->
                            httpResponseOkWithBody
                                (CompilationInterface.ElmMake.elm_make____src_Frontend_Main_elm.javascript.base64
                                    |> Base64.toBytes
                                )
                                (staticContentHttpHeaders { contentType = "text/javascript", contentEncoding = Nothing })
                                |> continueWithStaticHttpResponse

                        Just (Backend.Route.StaticFileRoute Backend.Route.FrontendLanguageServiceWorkerJavaScriptRoute) ->
                            httpResponseOkWithBody
                                (Base64.toBytes (languageServiceWorkerJavaScriptBase64 ()))
                                (staticContentHttpHeaders { contentType = "text/javascript", contentEncoding = Nothing })
                                |> continueWithStaticHttpResponse

                        Just Backend.Route.ApiRoute ->
                            ( { stateBefore
                                | pendingHttpRequests = httpRequestEvent :: stateBefore.pendingHttpRequests |> List.take 10
                              }
                            , []
                            )


bodyFromString : String -> Bytes.Bytes
bodyFromString string =
    string
        |> Bytes.Encode.string
        |> Bytes.Encode.encode


languageServiceWorkerJavaScript : () -> String
languageServiceWorkerJavaScript () =
    case Base64.toString CompilationInterface.ElmMake.elm_make____src_LanguageServiceWorker_elm.javascript.base64 of
        Nothing ->
            "Failed decoding base64"

        Just workerJavaScript ->
            workerJavaScript ++ """
const app = Elm.LanguageServiceWorker.init();

onmessage = function ({ data }) {
    app.ports.receiveRequest.send(data);
};

app.ports.sendResponse.subscribe(function(response) {
    console.log(response);
    postMessage(response);
})

"""


languageServiceWorkerJavaScriptBase64 : () -> String
languageServiceWorkerJavaScriptBase64 () =
    case Base64.fromString (languageServiceWorkerJavaScript ()) of
        Nothing ->
            "Failed encoding as base64"

        Just base64 ->
            base64


updateForRequestToVolatileProcessResult :
    String
    -> Platform.WebService.RequestToVolatileProcessResult
    -> State
    -> ( State, Platform.WebService.Commands State )
updateForRequestToVolatileProcessResult taskId requestToVolatileProcessResult stateBefore =
    case
        stateBefore.pendingHttpRequests
            |> List.filter (taskIdForHttpRequest >> (==) taskId)
            |> List.head
    of
        Nothing ->
            ( stateBefore
            , []
            )

        Just httpRequestEvent ->
            let
                httpResponseInternalServerError errorMessage =
                    { statusCode = 500
                    , body = Just (bodyFromString errorMessage)
                    , headersToAdd = []
                    }

                ( httpResponse, maybeVolatileProcessToRemoveId ) =
                    case stateBefore.pendingTasksForRequestVolatileProcess |> Dict.get taskId of
                        Nothing ->
                            ( httpResponseInternalServerError
                                ("Error: Did not find entry for task ID '" ++ taskId ++ "'")
                            , stateBefore.pendingTasksForRequestVolatileProcess
                                |> Dict.get taskId
                                |> Maybe.map .volatileProcessId
                            )

                        Just pendingTask ->
                            case requestToVolatileProcessResult of
                                Err Platform.WebService.ProcessNotFound ->
                                    ( httpResponseInternalServerError
                                        ("Error: Volatile process '"
                                            ++ pendingTask.volatileProcessId
                                            ++ "' disappeared. Starting volatile process again... Please retry."
                                        )
                                    , Just pendingTask.volatileProcessId
                                    )

                                Ok requestToVolatileProcessComplete ->
                                    case requestToVolatileProcessComplete.exceptionToString of
                                        Just exceptionToString ->
                                            ( { statusCode = 500
                                              , body =
                                                    Just
                                                        (bodyFromString
                                                            ("Exception in volatile process:\n"
                                                                ++ exceptionToString
                                                            )
                                                        )
                                              , headersToAdd = []
                                              }
                                            , Nothing
                                            )

                                        Nothing ->
                                            let
                                                ( headersToAdd, body ) =
                                                    case requestToVolatileProcessComplete.returnValueToString of
                                                        Nothing ->
                                                            ( [], Nothing )

                                                        Just returnValueToString ->
                                                            ( []
                                                            , Just (bodyFromString returnValueToString)
                                                            )
                                            in
                                            ( { statusCode = 200
                                              , body = body
                                              , headersToAdd = headersToAdd
                                              }
                                            , Nothing
                                            )

                volatileProcessesIds =
                    case maybeVolatileProcessToRemoveId of
                        Nothing ->
                            stateBefore.volatileProcessesIds

                        Just volatileProcessToRemoveId ->
                            stateBefore.volatileProcessesIds |> Set.remove volatileProcessToRemoveId
            in
            ( { stateBefore
                | pendingHttpRequests = stateBefore.pendingHttpRequests |> List.filter ((==) httpRequestEvent >> not)
                , volatileProcessesIds = volatileProcessesIds
                , pendingTasksForRequestVolatileProcess = stateBefore.pendingTasksForRequestVolatileProcess |> Dict.remove taskId
              }
            , [ Platform.WebService.RespondToHttpRequest
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response = httpResponse
                    }
              ]
            )


updateForCreateVolatileProcessResult :
    Platform.WebService.CreateVolatileProcessResult
    -> State
    -> ( State, Platform.WebService.Commands State )
updateForCreateVolatileProcessResult createVolatileProcessResult stateBefore =
    case createVolatileProcessResult of
        Err createVolatileProcessError ->
            let
                httpResponse =
                    { statusCode = 500
                    , body =
                        Just
                            (bodyFromString
                                ("Failed to create volatile process: " ++ createVolatileProcessError.exceptionToString)
                            )
                    , headersToAdd = []
                    }

                httpResponses =
                    stateBefore.pendingHttpRequests
                        |> List.map
                            (\httpRequest ->
                                { httpRequestId = httpRequest.httpRequestId
                                , response = httpResponse
                                }
                            )
            in
            ( { stateBefore | pendingHttpRequests = [] }
            , List.map Platform.WebService.RespondToHttpRequest httpResponses
            )

        Ok { processId } ->
            ( { stateBefore
                | volatileProcessesIds = Set.insert processId stateBefore.volatileProcessesIds
              }
            , []
            )


taskIdForHttpRequest : Platform.WebService.HttpRequestEventStruct -> String
taskIdForHttpRequest httpRequestEvent =
    "http-request-api-" ++ httpRequestEvent.httpRequestId


frontendHtmlDocument : { debug : Bool } -> String
frontendHtmlDocument { debug } =
    let
        elmMadeScriptFileName =
            if debug then
                Backend.Route.elmMadeScriptFileNameDebug

            else
                Backend.Route.elmMadeScriptFileNameDefault
    in
    """
<!DOCTYPE HTML>
<html>

<head>
  <meta charset="UTF-8">
  <title>Elm Editor</title>
  <link rel="icon" href="/"""
        ++ Common.faviconPath
        ++ """" type="image/svg+xml">
  <script type="text/javascript" src="""
        ++ elmMadeScriptFileName
        ++ """></script>
</head>

<body>
    <div id="elm-app-container"></div>
</body>

<script type="text/javascript">

var app = Elm.Frontend.Main.init({
    node: document.getElementById('elm-app-container')
});

app.ports.sendMessageToMonacoFrame.subscribe(function(message) {
    document.getElementById('monaco-iframe')?.contentWindow?.dispatchMessage?.(message);
});

function messageFromMonacoFrame(message) {
    app.ports.receiveMessageFromMonacoFrame?.send(message);
}

window.addEventListener('message', function(e) {

    if (e.data.type === 'clicked-link-in-iframe') {

        app.ports.receiveMessageFromContainerHtml?.send({
            "ClickedLinkInPreview":
                [{"href":e.data.href}]});
    }
}, false);


if (window.Worker) {
    const langServiceWorker = new Worker("language-service-worker.js");

    app.ports.sendRequestToLanguageService.subscribe(function(message) {
        langServiceWorker.postMessage(message);
    });

    function receiveResponseFromLanguageService(message) {
        app.ports.receiveResponseFromLanguageService?.send(message);
    }

    langServiceWorker.onmessage = function(message) {
        console.log('Message received from language service worker');
        receiveResponseFromLanguageService(message.data);
    }
} else {
    console.error("Failed to set up language service: Browser doesn't support web workers.");
}

</script>

</html>
"""


contentTypeFromStaticFileExtensionDict : Dict.Dict String String
contentTypeFromStaticFileExtensionDict =
    [ ( "svg", "image/svg+xml" )
    , ( "js", "text/javascript" )
    ]
        |> Dict.fromList


mapFileTreeNodeFromSource : CompilationInterface.SourceFiles.FileTreeNode a -> FileTree.FileTreeNode a
mapFileTreeNodeFromSource node =
    case node of
        CompilationInterface.SourceFiles.BlobNode blob ->
            FileTree.BlobNode blob

        CompilationInterface.SourceFiles.TreeNode tree ->
            tree |> List.map (Tuple.mapSecond mapFileTreeNodeFromSource) |> FileTree.TreeNode
