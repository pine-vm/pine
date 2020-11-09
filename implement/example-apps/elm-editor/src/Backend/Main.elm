module Backend.Main exposing
    ( State
    , interfaceToHost_initState
    , interfaceToHost_processEvent
    )

import Backend.ElmMakeVolatileHost as ElmMakeVolatileHost
import Backend.InterfaceToHost as InterfaceToHost
import Backend.Route
import Base64
import Bytes.Encode
import Common
import ElmFullstackCompilerInterface.ElmMake
import ElmFullstackCompilerInterface.SourceFiles
import Url


type alias State =
    { volatileHostId : Maybe String
    , pendingHttpRequests : List InterfaceToHost.HttpRequestEventStructure
    }


processEvent : InterfaceToHost.AppEvent -> State -> ( State, InterfaceToHost.AppEventResponse )
processEvent hostEvent stateBefore =
    let
        ( state, responseBeforeTasks ) =
            processEventBeforeDerivingTasks hostEvent stateBefore

        tasks =
            tasksFromState state
    in
    ( state
    , responseBeforeTasks |> InterfaceToHost.withStartTasksAdded tasks
    )


processEventBeforeDerivingTasks : InterfaceToHost.AppEvent -> State -> ( State, InterfaceToHost.AppEventResponse )
processEventBeforeDerivingTasks hostEvent stateBefore =
    case hostEvent of
        InterfaceToHost.ArrivedAtTimeEvent _ ->
            ( stateBefore
            , InterfaceToHost.passiveAppEventResponse
            )

        InterfaceToHost.HttpRequestEvent httpRequestEvent ->
            let
                bodyFromString =
                    Bytes.Encode.string >> Bytes.Encode.encode >> Base64.fromBytes

                httpResponseOkWithStringContent stringContent =
                    httpResponseOkWithBodyAsBase64 (bodyFromString stringContent)

                httpResponseOkWithBodyAsBase64 bodyAsBase64 =
                    { httpRequestId = httpRequestEvent.httpRequestId
                    , response =
                        { statusCode = 200
                        , bodyAsBase64 = bodyAsBase64
                        , headersToAdd = []
                        }
                    }
            in
            case httpRequestEvent.request.uri |> Url.fromString |> Maybe.andThen Backend.Route.routeFromUrl of
                Nothing ->
                    ( stateBefore
                    , InterfaceToHost.passiveAppEventResponse
                        |> InterfaceToHost.withCompleteHttpResponsesAdded
                            [ httpResponseOkWithStringContent frontendHtmlDocument
                            ]
                    )

                Just (Backend.Route.StaticFileRoute Backend.Route.FrontendElmJavascriptRoute) ->
                    ( stateBefore
                    , InterfaceToHost.passiveAppEventResponse
                        |> InterfaceToHost.withCompleteHttpResponsesAdded
                            [ httpResponseOkWithBodyAsBase64 (Just ElmFullstackCompilerInterface.ElmMake.elm_make__debug__javascript__base64____src_FrontendWeb_Main_elm) ]
                    )

                Just (Backend.Route.StaticFileRoute Backend.Route.MonacoFrameDocumentRoute) ->
                    ( stateBefore
                    , InterfaceToHost.passiveAppEventResponse
                        |> InterfaceToHost.withCompleteHttpResponsesAdded
                            [ httpResponseOkWithStringContent monacoHtmlDocument ]
                    )

                Just (Backend.Route.StaticFileRoute Backend.Route.MonarchJavascriptRoute) ->
                    ( stateBefore
                    , InterfaceToHost.passiveAppEventResponse
                        |> InterfaceToHost.withCompleteHttpResponsesAdded
                            [ httpResponseOkWithBodyAsBase64 (ElmFullstackCompilerInterface.SourceFiles.file____src_monarch_js |> Base64.fromBytes) ]
                    )

                Just Backend.Route.ApiRoute ->
                    ( { stateBefore | pendingHttpRequests = httpRequestEvent :: stateBefore.pendingHttpRequests |> List.take 10 }
                    , InterfaceToHost.passiveAppEventResponse
                    )

        InterfaceToHost.TaskCompleteEvent taskComplete ->
            case taskComplete.taskResult of
                InterfaceToHost.CreateVolatileHostResponse createVolatileHostResponse ->
                    case createVolatileHostResponse of
                        Err createVolatileHostError ->
                            let
                                bodyFromString =
                                    Bytes.Encode.string >> Bytes.Encode.encode >> Base64.fromBytes

                                httpResponse =
                                    { statusCode = 500
                                    , bodyAsBase64 = bodyFromString ("Failed to create volatile host: " ++ createVolatileHostError.exceptionToString)
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
                            , InterfaceToHost.passiveAppEventResponse
                                |> InterfaceToHost.withCompleteHttpResponsesAdded httpResponses
                            )

                        Ok { hostId } ->
                            ( { stateBefore | volatileHostId = Just hostId }, InterfaceToHost.passiveAppEventResponse )

                InterfaceToHost.RequestToVolatileHostResponse requestToVolatileHostResponse ->
                    case
                        stateBefore.pendingHttpRequests
                            |> List.filter (taskIdForHttpRequest >> (==) taskComplete.taskId)
                            |> List.head
                    of
                        Nothing ->
                            ( stateBefore
                            , InterfaceToHost.passiveAppEventResponse
                            )

                        Just httpRequestEvent ->
                            let
                                bodyFromString =
                                    Bytes.Encode.string >> Bytes.Encode.encode >> Base64.fromBytes

                                httpResponse =
                                    case requestToVolatileHostResponse of
                                        Err _ ->
                                            { statusCode = 500
                                            , bodyAsBase64 = bodyFromString "Error running in volatile host."
                                            , headersToAdd = []
                                            }

                                        Ok requestToVolatileHostComplete ->
                                            case requestToVolatileHostComplete.exceptionToString of
                                                Just exceptionToString ->
                                                    { statusCode = 500
                                                    , bodyAsBase64 = bodyFromString ("Exception in volatile host:\n" ++ exceptionToString)
                                                    , headersToAdd = []
                                                    }

                                                Nothing ->
                                                    let
                                                        headersToAdd =
                                                            []
                                                    in
                                                    { statusCode = 200
                                                    , bodyAsBase64 =
                                                        bodyFromString
                                                            (requestToVolatileHostComplete.returnValueToString
                                                                |> Maybe.withDefault ""
                                                            )
                                                    , headersToAdd = headersToAdd
                                                    }
                            in
                            ( { stateBefore
                                | pendingHttpRequests =
                                    stateBefore.pendingHttpRequests
                                        |> List.filter ((==) httpRequestEvent >> not)
                              }
                            , InterfaceToHost.passiveAppEventResponse
                                |> InterfaceToHost.withCompleteHttpResponsesAdded
                                    [ { httpRequestId = httpRequestEvent.httpRequestId
                                      , response = httpResponse
                                      }
                                    ]
                            )

                InterfaceToHost.CompleteWithoutResult ->
                    ( stateBefore
                    , InterfaceToHost.passiveAppEventResponse
                    )


tasksFromState : State -> List InterfaceToHost.StartTaskStructure
tasksFromState state =
    case state.pendingHttpRequests |> List.head of
        Nothing ->
            []

        Just httpRequestEvent ->
            case state.volatileHostId of
                Nothing ->
                    [ { taskId = "create-vhost"
                      , task = InterfaceToHost.CreateVolatileHost { script = ElmMakeVolatileHost.volatileHostScript }
                      }
                    ]

                Just volatileHostId ->
                    let
                        task =
                            { hostId = volatileHostId
                            , request =
                                httpRequestEvent.request.bodyAsBase64
                                    |> Maybe.andThen Common.decodeBase64ToString
                                    |> Maybe.withDefault "Error decoding base64"
                            }
                                |> InterfaceToHost.RequestToVolatileHost
                    in
                    [ { taskId = taskIdForHttpRequest httpRequestEvent
                      , task = task
                      }
                    ]


taskIdForHttpRequest : InterfaceToHost.HttpRequestEventStructure -> String
taskIdForHttpRequest httpRequestEvent =
    "http-request-api-" ++ httpRequestEvent.httpRequestId


interfaceToHost_initState : State
interfaceToHost_initState =
    { volatileHostId = Nothing, pendingHttpRequests = [] }


interfaceToHost_processEvent : String -> State -> ( State, String )
interfaceToHost_processEvent =
    InterfaceToHost.wrapForSerialInterface_processEvent processEvent


frontendHtmlDocument : String
frontendHtmlDocument =
    """
<!DOCTYPE HTML>
<html>

<head>
  <meta charset="UTF-8">
  <title>Elm Editor</title>
  <script type="text/javascript" src="elm-made.js"></script>
</head>

<body>
    <div id="elm-app-container"></div>
</body>

<script type="text/javascript">

var app = Elm.FrontendWeb.Main.init({
    node: document.getElementById('elm-app-container')
});

app.ports.sendMessageToMonacoFrame.subscribe(function(message) {
    document.getElementById('monaco-iframe').contentWindow?.dispatchMessage?.(message);
});

function messageFromMonacoFrame(message) {
    app.ports.receiveMessageFromMonacoFrame?.send(message);
}

</script>

</html>
"""


monacoHtmlDocument : String
monacoHtmlDocument =
    monacoHtmlDocumentFromCdnUrl (monacoCdnURLs |> List.head |> Maybe.withDefault "Missing URL to CDN")


{-| Combine from samples:

  - <https://github.com/microsoft/monaco-editor/blob/1396f98763b08e4b8dc3d9e16e23ceb67d8456e9/docs/integrate-amd.md>
  - <https://github.com/microsoft/monaco-editor/blob/1396f98763b08e4b8dc3d9e16e23ceb67d8456e9/docs/integrate-amd-cross.md>

-}
monacoHtmlDocumentFromCdnUrl : String -> String
monacoHtmlDocumentFromCdnUrl cdnUrlToMin =
    """
<!DOCTYPE html>
<html>
<head>
    <meta http-equiv="Content-Type" content="text/html;charset=utf-8" >
    <script type="text/javascript" src="monarch.js"></script>
</head>
<body style="margin:0;height:93vh;">

<div id="container" style="height:100%;width:100%;"></div>

<script type="text/javascript" src="url-to-the-monaco-cdn-directory-min/vs/loader.js"></script>

<script>

    function getEditorModel() {
        if(typeof monaco != "object")
            return null;

        return monaco?.editor?.getModels()[0];
    }

    function monacoEditorSetValue(newValue) {
        getEditorModel()?.setValue(newValue);
    }

    function dispatchMessage(message) {
        if(message.SetValue)
            monacoEditorSetValue(message.SetValue[0]);
    }

    function tryCompleteSetup() {
        var editorModel = getEditorModel();

        if(editorModel == null) {
            setTimeout(tryCompleteSetup, 500);
        }
        else {
            editorModel.onDidChangeContent(function() {
                var content = getEditorModel().getValue();

                // console.log("onDidChangeContent:\\n" + content);

                parent?.messageFromMonacoFrame?.({"DidChangeContentEvent":[content]});
            });

            parent?.messageFromMonacoFrame?.({"CompletedSetupEvent":[]});
        }
    }

</script>

<script>
  require.config({ paths: { 'vs': 'url-to-the-monaco-cdn-directory-min/vs' }});

  // Before loading vs/editor/editor.main, define a global MonacoEnvironment that overwrites
  // the default worker url location (used when creating WebWorkers). The problem here is that
  // HTML5 does not allow cross-domain web workers, so we need to proxy the instantiation of
  // a web worker through a same-domain script
  window.MonacoEnvironment = {
    getWorkerUrl: function(workerId, label) {
      return `data:text/javascript;charset=utf-8,${encodeURIComponent(`
        self.MonacoEnvironment = {
          baseUrl: 'url-to-the-monaco-cdn-directory-min/'
        };
        importScripts('url-to-the-monaco-cdn-directory-min/vs/base/worker/workerMain.js');`
      )}`;
    }
  };
    require(['vs/editor/editor.main'], function() {

        monaco.languages.register({ id: 'Elm' });

        monaco.languages.setMonarchTokensProvider('Elm', window.elm_monarch);

        monaco.editor.defineTheme('dark-plus', {
            base: 'vs-dark',
            inherit: true,
            rules: [
                { token: 'keyword', foreground: '#C586C0' },
                { token: 'type', foreground: '#569CD6' },
                { token: 'function.name', foreground: '#DCDCAA' },
            ]
        });

        var editor = monaco.editor.create(document.getElementById('container'), {
            value: "Initialization of editor is not complete yet",
            language: 'Elm',
            automaticLayout: true,
            scrollBeyondLastLine: false,
            theme: "dark-plus"
        });

        tryCompleteSetup();
    });
</script>

</body>
</html>
"""
        |> String.replace "url-to-the-monaco-cdn-directory-min" cdnUrlToMin


{-| <https://github.com/microsoft/monaco-editor/issues/583>
-}
monacoCdnURLs : List String
monacoCdnURLs =
    [ "https://cdnjs.cloudflare.com/ajax/libs/monaco-editor/0.20.0/min"
    , "https://unpkg.com/monaco-editor@0.20.0/min"
    , "https://cdn.jsdelivr.net/npm/monaco-editor@0.20/min"
    ]
