module Backend.Main exposing
    ( State
    , webServiceMain
    )

import Backend.Route
import Backend.VolatileProcess as VolatileProcess
import Base64
import Bytes.Encode
import Common
import CompilationInterface.ElmMake
import CompilationInterface.SourceFiles
import Dict
import FileTree
import MonacoHtml
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


updateForHttpRequestEvent : Platform.WebService.HttpRequestEventStruct -> State -> ( State, Platform.WebService.Commands State )
updateForHttpRequestEvent httpRequestEvent =
    processEvent (HttpRequestEvent httpRequestEvent)


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
                        \createVolatileProcessResult ->
                            processEvent (CreateVolatileProcessResponse createVolatileProcessResult)
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
                                    httpRequestEvent.request.bodyAsBase64
                                        |> Maybe.andThen Common.decodeBase64ToString
                                        |> Maybe.withDefault "Error decoding base64"
                                , update =
                                    \requestToVolatileProcessResponse ->
                                        processEvent
                                            (RequestToVolatileProcessResponse taskId requestToVolatileProcessResponse)
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
        stateBefore =
            { stateBeforeUpdatingTime | posixTimeMilli = httpRequestEvent.posixTimeMilli }

        bodyFromString =
            Bytes.Encode.string >> Bytes.Encode.encode >> Base64.fromBytes

        staticContentHttpHeaders { contentType, contentEncoding } =
            { cacheMaxAgeMinutes = Just (60 * 24)
            , contentType = contentType
            , contentEncoding = contentEncoding
            }

        httpResponseOkWithStringContent stringContent httpResponseHeaders =
            httpResponseOkWithBodyAsBase64 (bodyFromString stringContent) httpResponseHeaders

        httpResponseOkWithBodyAsBase64 bodyAsBase64 contentConfig =
            { statusCode = 200
            , bodyAsBase64 = bodyAsBase64
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
    case httpRequestEvent.request.uri |> Url.fromString of
        Nothing ->
            continueWithStaticHttpResponse
                { statusCode = 500
                , bodyAsBase64 = bodyFromString "Failed to parse URL"
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
                        contentType =
                            filePath
                                |> List.concatMap (String.split ".")
                                |> List.reverse
                                |> List.head
                                |> Maybe.andThen (Dict.get >> (|>) contentTypeFromStaticFileExtensionDict)
                                |> Maybe.withDefault ""
                    in
                    httpResponseOkWithBodyAsBase64
                        (Just matchingFile.base64)
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
                            httpResponseOkWithBodyAsBase64
                                (Just
                                    (if debug then
                                        CompilationInterface.ElmMake.elm_make____src_Frontend_Main_elm.debug.javascript.base64

                                     else
                                        CompilationInterface.ElmMake.elm_make____src_Frontend_Main_elm.javascript.base64
                                    )
                                )
                                (staticContentHttpHeaders { contentType = "text/javascript", contentEncoding = Nothing })
                                |> continueWithStaticHttpResponse

                        Just (Backend.Route.StaticFileRoute Backend.Route.MonacoFrameDocumentRoute) ->
                            httpResponseOkWithStringContent monacoHtmlDocument
                                (staticContentHttpHeaders { contentType = "text/html", contentEncoding = Nothing })
                                |> continueWithStaticHttpResponse

                        Just Backend.Route.ApiRoute ->
                            ( { stateBefore
                                | pendingHttpRequests = httpRequestEvent :: stateBefore.pendingHttpRequests |> List.take 10
                              }
                            , []
                            )


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
                bodyFromString =
                    Bytes.Encode.string >> Bytes.Encode.encode >> Base64.fromBytes

                httpResponseInternalServerError errorMessage =
                    { statusCode = 500
                    , bodyAsBase64 = bodyFromString errorMessage
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
                                              , bodyAsBase64 = bodyFromString ("Exception in volatile process:\n" ++ exceptionToString)
                                              , headersToAdd = []
                                              }
                                            , Nothing
                                            )

                                        Nothing ->
                                            let
                                                ( headersToAdd, bodyAsBase64 ) =
                                                    case requestToVolatileProcessComplete.returnValueToString of
                                                        Nothing ->
                                                            ( [], Nothing )

                                                        Just returnValueToString ->
                                                            let
                                                                deflateEncodedBody =
                                                                    returnValueToString
                                                                        |> Bytes.Encode.string
                                                                        |> Bytes.Encode.encode
                                                            in
                                                            ( []
                                                            , deflateEncodedBody |> Base64.fromBytes
                                                            )
                                            in
                                            ( { statusCode = 200
                                              , bodyAsBase64 = bodyAsBase64
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
                bodyFromString =
                    Bytes.Encode.string >> Bytes.Encode.encode >> Base64.fromBytes

                httpResponse =
                    { statusCode = 500
                    , bodyAsBase64 = bodyFromString ("Failed to create volatile process: " ++ createVolatileProcessError.exceptionToString)
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

</script>

</html>
"""


monacoHtmlDocument : String
monacoHtmlDocument =
    MonacoHtml.monacoHtmlDocumentFromCdnUrl (monacoCdnURLs |> List.head |> Maybe.withDefault "Missing URL to CDN")


{-| <https://github.com/microsoft/monaco-editor/issues/583>
-}
monacoCdnURLs : List String
monacoCdnURLs =
    [ "https://cdn.jsdelivr.net/npm/monaco-editor@0.38.0/min"
    , "https://unpkg.com/monaco-editor@0.38.0/min"
    ]


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
