port module FrontendWeb.Main exposing (Event(..), State, defaultProject, init, main, receiveMessageFromMonacoFrame, sendMessageToMonacoFrame, update, view)

import Base64
import Browser
import Browser.Navigation as Navigation
import Bytes
import Bytes.Decode
import Bytes.Encode
import Common
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Element.Region
import ElmFullstackCompilerInterface.GenerateJsonCoders
import ElmFullstackCompilerInterface.SourceFiles
import ElmMakeExecutableFile
import FrontendBackendInterface
import FrontendWeb.BrowserApplicationInitWithTime as BrowserApplicationInitWithTime
import FrontendWeb.MonacoEditor
import FrontendWeb.ProjectStateInUrl
import FrontendWeb.Visuals as Visuals
import Html
import Html.Attributes as HA
import Html.Events
import Http
import Json.Decode
import Json.Encode
import List.Extra
import ProjectState
import ProjectState_2021_01
import Result.Extra
import String.Extra
import Time
import Url
import Url.Builder


port sendMessageToMonacoFrame : Json.Encode.Value -> Cmd msg


port receiveMessageFromMonacoFrame : (Json.Encode.Value -> msg) -> Sub msg


type alias ElmMakeRequestStructure =
    FrontendBackendInterface.ElmMakeRequestStructure


type alias ElmMakeResponseStructure =
    FrontendBackendInterface.ElmMakeResponseStructure


type alias State =
    { navigationKey : Navigation.Key
    , url : Url.Url
    , time : Time.Posix
    , workspace : WorkspaceStateStructure
    , modalDialog : Maybe ModalDialogState
    , lastBackendLoadFromGitResult : Maybe ( String, Result Http.Error FrontendBackendInterface.LoadCompositionResponseStructure )
    }


type WorkspaceStateStructure
    = WorkspaceOk WorkingProjectStateStructure
    | WorkspaceLoadingFromLink
        { projectStateDescription : ProjectState_2021_01.ProjectState
        , filePathToOpen : Maybe (List String)
        , expectedCompositionHash : Maybe String
        }
    | WorkspaceErr String


type alias WorkingProjectStateStructure =
    { fileTree : ProjectState.FileTreeNode
    , editing : { filePathOpenedInEditor : Maybe (List String) }
    , decodeMessageFromMonacoEditorError : Maybe Json.Decode.Error
    , lastTextReceivedFromEditor : Maybe String
    , pendingElmMakeRequest : Maybe { time : Time.Posix, request : ElmMakeRequestStructure }
    , elmMakeResult : Maybe ( ElmMakeRequestStructure, Result Http.Error ElmMakeResultStructure )
    , elmFormatResult : Maybe (Result Http.Error FrontendBackendInterface.FormatElmModuleTextResponseStructure)
    , viewEnlargedPane : Maybe WorkspacePane
    }


type WorkspacePane
    = EditorPane
    | OutputPane


type alias ElmMakeResultStructure =
    { response : ElmMakeResponseStructure
    , compiledHtmlDocument : Maybe String
    , reportFromJson : Maybe (Result String ElmMakeExecutableFile.ElmMakeReportFromJson)
    }


type Event
    = TimeHasArrived Time.Posix
    | UserInputLoadFromGit UserInputLoadFromGitEventStructure
    | UserInputCloseModalDialog
    | BackendLoadFromGitResultEvent String (Result Http.Error FrontendBackendInterface.LoadCompositionResponseStructure)
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | WorkspaceEvent WorkspaceEventStructure
    | UserInputSave (Maybe { createDiffIfBaseAvailable : Bool })
    | DiscardEvent


type WorkspaceEventStructure
    = MonacoEditorEvent Json.Decode.Value
    | UserInputChangeTextInEditor String
    | UserInputOpenFileInEditor (List String)
    | UserInputFormat
    | UserInputCompile
    | UserInputRevealPositionInEditor { filePath : List String, lineNumber : Int, column : Int }
    | BackendElmFormatResponseEvent { filePath : List String, result : Result Http.Error FrontendBackendInterface.FormatElmModuleTextResponseStructure }
    | BackendElmMakeResponseEvent ElmMakeRequestStructure (Result Http.Error ElmMakeResponseStructure)
    | UserInputSetEnlargedPane (Maybe WorkspacePane)


type UserInputLoadFromGitEventStructure
    = LoadFromGitOpenDialog
    | LoadFromGitEnterUrlEvent { urlIntoGitRepository : String }
    | LoadFromGitBeginRequestEvent { urlIntoGitRepository : String }
    | LoadFromGitTakeResultAsProjectStateEvent


type ModalDialogState
    = SaveOrShareDialog SaveOrShareDialogState
    | LoadFromGitDialog LoadFromGitDialogState


type alias SaveOrShareDialogState =
    { urlToProject : Maybe String }


type alias LoadFromGitDialogState =
    { urlIntoGitRepository : String
    , requestTime : Maybe Time.Posix
    , loadCompositionResult : Maybe (Result Http.Error FrontendBackendInterface.LoadCompositionResponseStructure)
    }


main : Program () (BrowserApplicationInitWithTime.State () State Event) (BrowserApplicationInitWithTime.Event Event)
main =
    BrowserApplicationInitWithTime.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , viewWhileWaitingForTime =
            { title = "Elm Editor - Initializing"
            , body = [ Html.text "Waiting for first measurement of time..." ]
            }
        , view = view
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }


subscriptions : State -> Sub Event
subscriptions _ =
    [ receiveMessageFromMonacoFrame (MonacoEditorEvent >> WorkspaceEvent)
    , Time.every 500 TimeHasArrived
    ]
        |> Sub.batch


init : () -> Url.Url -> Navigation.Key -> Time.Posix -> ( State, Cmd Event )
init _ url navigationKey time =
    { navigationKey = navigationKey
    , url = url
    , time = time
    , workspace = defaultProject |> initWorkspaceFromFileTreeAndFileSelection |> WorkspaceOk
    , modalDialog = Nothing
    , lastBackendLoadFromGitResult = Nothing
    }
        |> update (UrlChange url)


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    case event of
        TimeHasArrived time ->
            ( { stateBefore | time = time }, Cmd.none )

        UrlChange url ->
            processEventUrlChanged url stateBefore

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( stateBefore, Navigation.pushUrl stateBefore.navigationKey (Url.toString url) )

                Browser.External url ->
                    ( stateBefore, Navigation.load url )

        UserInputSave maybeGenerateLink ->
            case stateBefore.workspace of
                WorkspaceOk workingState ->
                    let
                        dialogBefore =
                            (case stateBefore.modalDialog of
                                Just (SaveOrShareDialog saveOrShareDialog) ->
                                    Just saveOrShareDialog

                                _ ->
                                    Nothing
                            )
                                |> Maybe.withDefault { urlToProject = Nothing }

                        ( dialog, cmd ) =
                            case maybeGenerateLink of
                                Nothing ->
                                    ( dialogBefore, Cmd.none )

                                Just generateLink ->
                                    let
                                        baseToUse =
                                            if not generateLink.createDiffIfBaseAvailable then
                                                Nothing

                                            else
                                                case stateBefore.lastBackendLoadFromGitResult of
                                                    Nothing ->
                                                        Nothing

                                                    Just ( loadFromGitUrl, loadFromGitResult ) ->
                                                        case loadFromGitResult of
                                                            Err _ ->
                                                                Nothing

                                                            Ok loadFromGitOk ->
                                                                Just
                                                                    ( loadFromGitUrl
                                                                    , fileTreeNodeFromListFileWithPath loadFromGitOk.filesAsFlatList
                                                                    )

                                        url =
                                            stateBefore.url
                                                |> FrontendWeb.ProjectStateInUrl.setProjectStateInUrl
                                                    workingState.fileTree
                                                    baseToUse
                                                    { filePathToOpen = workingState.editing.filePathOpenedInEditor }
                                                |> Url.toString
                                    in
                                    ( { dialogBefore | urlToProject = Just url }
                                    , Navigation.replaceUrl stateBefore.navigationKey url
                                    )
                    in
                    ( { stateBefore | modalDialog = Just (SaveOrShareDialog dialog) }, cmd )

                _ ->
                    ( stateBefore, Cmd.none )

        UserInputLoadFromGit LoadFromGitOpenDialog ->
            case stateBefore.modalDialog of
                Nothing ->
                    ( { stateBefore
                        | modalDialog =
                            Just
                                (LoadFromGitDialog
                                    { urlIntoGitRepository = ""
                                    , requestTime = Nothing
                                    , loadCompositionResult = Nothing
                                    }
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( stateBefore, Cmd.none )

        UserInputLoadFromGit (LoadFromGitEnterUrlEvent { urlIntoGitRepository }) ->
            case stateBefore.modalDialog of
                Just (LoadFromGitDialog dialogStateBefore) ->
                    if dialogStateBefore.requestTime /= Nothing || dialogStateBefore.loadCompositionResult /= Nothing then
                        ( stateBefore, Cmd.none )

                    else
                        let
                            dialogState =
                                { urlIntoGitRepository = urlIntoGitRepository
                                , requestTime = Nothing
                                , loadCompositionResult = Nothing
                                }
                        in
                        ( { stateBefore | modalDialog = Just (LoadFromGitDialog dialogState) }
                        , Cmd.none
                        )

                _ ->
                    ( stateBefore, Cmd.none )

        UserInputLoadFromGit (LoadFromGitBeginRequestEvent { urlIntoGitRepository }) ->
            case stateBefore.modalDialog of
                Just (LoadFromGitDialog _) ->
                    let
                        dialogState =
                            { urlIntoGitRepository = urlIntoGitRepository
                            , requestTime = Just stateBefore.time
                            , loadCompositionResult = Nothing
                            }
                    in
                    ( { stateBefore | modalDialog = Just (LoadFromGitDialog dialogState) }
                    , loadFromGitCmd urlIntoGitRepository
                    )

                _ ->
                    ( stateBefore, Cmd.none )

        UserInputLoadFromGit LoadFromGitTakeResultAsProjectStateEvent ->
            case stateBefore.modalDialog of
                Just (LoadFromGitDialog dialogStateBefore) ->
                    case dialogStateBefore.loadCompositionResult of
                        Just (Ok loadOk) ->
                            ( { stateBefore
                                | modalDialog = Nothing
                                , workspace =
                                    { fileTree = fileTreeNodeFromListFileWithPath loadOk.filesAsFlatList
                                    , filePathOpenedInEditor = Nothing
                                    }
                                        |> initWorkspaceFromFileTreeAndFileSelection
                                        |> WorkspaceOk
                              }
                            , Cmd.none
                            )

                        _ ->
                            ( stateBefore, Cmd.none )

                _ ->
                    ( stateBefore, Cmd.none )

        BackendLoadFromGitResultEvent urlIntoGitRepository result ->
            processEventBackendLoadFromGitResult urlIntoGitRepository result stateBefore

        UserInputCloseModalDialog ->
            ( { stateBefore | modalDialog = Nothing }, Cmd.none )

        WorkspaceEvent workspaceEvent ->
            case stateBefore.workspace of
                WorkspaceOk workspaceBefore ->
                    let
                        ( workspace, workspaceCmd ) =
                            workspaceBefore
                                |> updateWorkspace { time = stateBefore.time } workspaceEvent
                    in
                    ( { stateBefore | workspace = WorkspaceOk workspace }
                    , Cmd.map WorkspaceEvent workspaceCmd
                    )

                WorkspaceLoadingFromLink _ ->
                    ( stateBefore, Cmd.none )

                WorkspaceErr _ ->
                    ( stateBefore, Cmd.none )

        DiscardEvent ->
            ( stateBefore, Cmd.none )


updateWorkspace : { time : Time.Posix } -> WorkspaceEventStructure -> WorkingProjectStateStructure -> ( WorkingProjectStateStructure, Cmd WorkspaceEventStructure )
updateWorkspace updateConfig event stateBeforeApplyingEvent =
    let
        ( stateBeforeConsiderCompile, cmd ) =
            updateWorkspaceWithoutCmdToUpdateEditor updateConfig event stateBeforeApplyingEvent

        textForEditor =
            stateBeforeConsiderCompile
                |> fileOpenedInEditorFromWorkspace
                |> Maybe.andThen (Tuple.second >> stringFromFileContent)
                |> Maybe.withDefault "Failed to map file content to string."

        setTextToEditorCmd =
            if Just textForEditor == state.lastTextReceivedFromEditor then
                Nothing

            else
                Just (setTextInMonacoEditorCmd textForEditor)

        setModelMarkersToEditorCmd =
            case fileOpenedInEditorFromWorkspace stateBeforeConsiderCompile of
                Nothing ->
                    Nothing

                Just fileOpenedInEditor ->
                    case stateBeforeConsiderCompile.elmMakeResult of
                        Nothing ->
                            Nothing

                        Just ( elmMakeRequest, elmMakeResult ) ->
                            if
                                (stateBeforeConsiderCompile.elmMakeResult == stateBeforeApplyingEvent.elmMakeResult)
                                    && (Just (Tuple.first fileOpenedInEditor) == filePathOpenedInEditorFromWorkspace stateBeforeApplyingEvent)
                                    && (setTextToEditorCmd == Nothing)
                            then
                                Nothing

                            else
                                elmMakeResult
                                    |> Result.toMaybe
                                    |> Maybe.andThen .reportFromJson
                                    |> editorDocumentMarkersFromElmMakeReport
                                        { elmMakeRequest = elmMakeRequest, fileOpenedInEditor = fileOpenedInEditor }
                                    |> setModelMarkersInMonacoEditorCmd
                                    |> Just

        triggerCompile =
            (stateBeforeConsiderCompile
                |> filePathOpenedInEditorFromWorkspace
                |> Maybe.andThen (List.reverse >> List.head)
                |> Maybe.map (String.endsWith ".elm")
                |> Maybe.withDefault False
            )
                && (stateBeforeConsiderCompile.pendingElmMakeRequest == Nothing)
                && (stateBeforeConsiderCompile.elmMakeResult == Nothing)

        ( state, compileCmd ) =
            if triggerCompile then
                userInputCompileFileOpenedInEditor updateConfig stateBeforeConsiderCompile

            else
                ( stateBeforeConsiderCompile, Cmd.none )
    in
    ( state
    , Cmd.batch
        [ cmd
        , Maybe.withDefault Cmd.none setTextToEditorCmd
        , Maybe.withDefault Cmd.none setModelMarkersToEditorCmd
        , compileCmd
        ]
    )


filePathOpenedInEditorFromWorkspace : WorkingProjectStateStructure -> Maybe (List String)
filePathOpenedInEditorFromWorkspace =
    fileOpenedInEditorFromWorkspace >> Maybe.map Tuple.first


fileOpenedInEditorFromWorkspace : WorkingProjectStateStructure -> Maybe ( List String, Bytes.Bytes )
fileOpenedInEditorFromWorkspace workingState =
    case workingState.editing.filePathOpenedInEditor of
        Nothing ->
            Nothing

        Just filePathOpenedInEditor ->
            case workingState.fileTree |> ProjectState.getBlobAtPathFromFileTree filePathOpenedInEditor of
                Nothing ->
                    Nothing

                Just fileContent ->
                    Just ( filePathOpenedInEditor, fileContent )


updateWorkspaceWithoutCmdToUpdateEditor : { time : Time.Posix } -> WorkspaceEventStructure -> WorkingProjectStateStructure -> ( WorkingProjectStateStructure, Cmd WorkspaceEventStructure )
updateWorkspaceWithoutCmdToUpdateEditor updateConfig event stateBefore =
    case event of
        UserInputOpenFileInEditor filePath ->
            if ProjectState.getBlobAtPathFromFileTree filePath stateBefore.fileTree == Nothing then
                ( stateBefore, Cmd.none )

            else
                ( let
                    editing =
                        stateBefore.editing
                  in
                  { stateBefore | editing = { editing | filePathOpenedInEditor = Just filePath } }
                , Cmd.none
                )

        UserInputChangeTextInEditor inputText ->
            ( case stateBefore.editing.filePathOpenedInEditor of
                Nothing ->
                    stateBefore

                Just filePath ->
                    { stateBefore
                        | fileTree =
                            stateBefore.fileTree
                                |> ProjectState.setBlobAtPathInSortedFileTree ( filePath, fileContentFromString inputText )
                        , lastTextReceivedFromEditor = Just inputText
                    }
            , Cmd.none
            )

        UserInputRevealPositionInEditor revealPositionInEditor ->
            ( stateBefore
            , if (stateBefore |> fileOpenedInEditorFromWorkspace |> Maybe.map Tuple.first) == Just revealPositionInEditor.filePath then
                revealPositionInCenterInMonacoEditorCmd
                    { lineNumber = revealPositionInEditor.lineNumber, column = revealPositionInEditor.column }

              else
                Cmd.none
            )

        MonacoEditorEvent monacoEditorEvent ->
            case
                monacoEditorEvent
                    |> Json.Decode.decodeValue ElmFullstackCompilerInterface.GenerateJsonCoders.jsonDecodeMessageFromMonacoEditor
            of
                Err decodeError ->
                    ( { stateBefore | decodeMessageFromMonacoEditorError = Just decodeError }, Cmd.none )

                Ok decodedMonacoEditorEvent ->
                    case decodedMonacoEditorEvent of
                        FrontendWeb.MonacoEditor.DidChangeContentEvent content ->
                            stateBefore |> updateWorkspaceWithoutCmdToUpdateEditor updateConfig (UserInputChangeTextInEditor content)

                        FrontendWeb.MonacoEditor.CompletedSetupEvent ->
                            ( { stateBefore | lastTextReceivedFromEditor = Nothing }, Cmd.none )

                        FrontendWeb.MonacoEditor.EditorActionCloseFileEvent ->
                            ( let
                                editing =
                                    stateBefore.editing
                              in
                              { stateBefore | editing = { editing | filePathOpenedInEditor = Nothing } }
                            , Cmd.none
                            )

                        FrontendWeb.MonacoEditor.EditorActionFormatDocumentEvent ->
                            updateWorkspaceWithoutCmdToUpdateEditor updateConfig UserInputFormat stateBefore

                        FrontendWeb.MonacoEditor.EditorActionCompileEvent ->
                            updateWorkspaceWithoutCmdToUpdateEditor updateConfig UserInputCompile stateBefore

        UserInputFormat ->
            ( stateBefore, elmFormatCmd stateBefore |> Maybe.withDefault Cmd.none )

        UserInputCompile ->
            userInputCompileFileOpenedInEditor updateConfig { stateBefore | viewEnlargedPane = Nothing }

        BackendElmFormatResponseEvent formatResponseEvent ->
            ( if Just formatResponseEvent.filePath /= stateBefore.editing.filePathOpenedInEditor then
                stateBefore

              else
                case formatResponseEvent.result |> Result.toMaybe |> Maybe.andThen .formattedText of
                    Nothing ->
                        stateBefore

                    Just formattedText ->
                        { stateBefore
                            | fileTree =
                                stateBefore.fileTree
                                    |> ProjectState.setBlobAtPathInSortedFileTree
                                        ( formatResponseEvent.filePath
                                        , fileContentFromString formattedText
                                        )
                            , elmFormatResult = Just formatResponseEvent.result
                        }
            , Cmd.none
            )

        BackendElmMakeResponseEvent elmMakeRequest httpResponse ->
            let
                elmMakeResult =
                    httpResponse
                        |> Result.map
                            (\elmMakeResponse ->
                                let
                                    compiledHtmlDocument =
                                        case elmMakeResponse.outputFileContentBase64 of
                                            Nothing ->
                                                Nothing

                                            Just outputFileContentBase64 ->
                                                outputFileContentBase64
                                                    |> Common.decodeBase64ToString
                                                    |> Maybe.withDefault ("Error decoding base64: " ++ outputFileContentBase64)
                                                    |> Just

                                    reportFromJson =
                                        case
                                            elmMakeResponse.reportJsonProcessOutput.standardError
                                                |> Json.Decode.decodeString Json.Decode.value
                                        of
                                            Err _ ->
                                                Nothing

                                            Ok elmMakeReportJson ->
                                                elmMakeReportJson
                                                    |> Json.Decode.decodeValue ElmMakeExecutableFile.jsonDecodeElmMakeReport
                                                    |> Result.mapError Json.Decode.errorToString
                                                    |> Just
                                in
                                { response = elmMakeResponse
                                , compiledHtmlDocument = compiledHtmlDocument
                                , reportFromJson = reportFromJson
                                }
                            )
            in
            ( { stateBefore
                | elmMakeResult = Just ( elmMakeRequest, elmMakeResult )
                , pendingElmMakeRequest = Nothing
              }
            , Cmd.none
            )

        UserInputSetEnlargedPane enlargedPane ->
            ( { stateBefore | viewEnlargedPane = enlargedPane }, Cmd.none )


processEventUrlChanged : Url.Url -> State -> ( State, Cmd Event )
processEventUrlChanged url stateBefore =
    let
        projectStateExpectedCompositionHash =
            FrontendWeb.ProjectStateInUrl.projectStateHashFromUrl url

        filePathToOpen =
            FrontendWeb.ProjectStateInUrl.filePathToOpenFromUrl url
                |> Maybe.map (String.split "/" >> List.concatMap (String.split "\\"))

        projectWithMatchingStateHashAlreadyLoaded =
            case stateBefore.workspace of
                WorkspaceOk workingState ->
                    Just (FrontendWeb.ProjectStateInUrl.projectStateCompositionHash workingState.fileTree)
                        == projectStateExpectedCompositionHash

                _ ->
                    False

        continueWithDiffProjectState projectStateDescription =
            ( { stateBefore
                | workspace =
                    WorkspaceLoadingFromLink
                        { projectStateDescription = projectStateDescription
                        , filePathToOpen = filePathToOpen
                        , expectedCompositionHash = projectStateExpectedCompositionHash
                        }
              }
            , loadFromGitCmd projectStateDescription.base
            )
    in
    case FrontendWeb.ProjectStateInUrl.projectStateDescriptionFromUrl url of
        Nothing ->
            ( stateBefore, Cmd.none )

        Just (Err fromUrlError) ->
            ( { stateBefore
                | workspace = WorkspaceErr ("Failed to decode project state from URL: " ++ fromUrlError)
              }
            , Cmd.none
            )

        Just (Ok projectStateDescription) ->
            if projectWithMatchingStateHashAlreadyLoaded then
                ( stateBefore, Cmd.none )

            else
                case projectStateDescription of
                    FrontendWeb.ProjectStateInUrl.LiteralProjectState projectState ->
                        updateForLoadedProjectState
                            { expectedCompositionHash = projectStateExpectedCompositionHash
                            , filePathToOpen = filePathToOpen
                            }
                            projectState
                            ProjectState_2021_01.noDifference
                            stateBefore

                    FrontendWeb.ProjectStateInUrl.LinkProjectState linkToProjectState ->
                        continueWithDiffProjectState
                            { base = linkToProjectState
                            , differenceFromBase = ProjectState_2021_01.noDifference
                            }

                    FrontendWeb.ProjectStateInUrl.DiffProjectState diffProjectState ->
                        continueWithDiffProjectState diffProjectState


processEventBackendLoadFromGitResult : String -> Result Http.Error FrontendBackendInterface.LoadCompositionResponseStructure -> State -> ( State, Cmd Event )
processEventBackendLoadFromGitResult urlIntoGitRepository result stateBeforeRememberingResult =
    let
        stateBefore =
            { stateBeforeRememberingResult | lastBackendLoadFromGitResult = Just ( urlIntoGitRepository, result ) }
    in
    case stateBefore.modalDialog of
        Just (LoadFromGitDialog dialogStateBefore) ->
            let
                dialogState =
                    { dialogStateBefore | loadCompositionResult = Just result }
            in
            ( { stateBefore | modalDialog = Just (LoadFromGitDialog dialogState) }, Cmd.none )

        _ ->
            case stateBefore.workspace of
                WorkspaceLoadingFromLink projectStateLoadingFromLink ->
                    if urlIntoGitRepository == projectStateLoadingFromLink.projectStateDescription.base then
                        case result of
                            Ok loadOk ->
                                updateForLoadedProjectState
                                    { expectedCompositionHash = projectStateLoadingFromLink.expectedCompositionHash
                                    , filePathToOpen = projectStateLoadingFromLink.filePathToOpen
                                    }
                                    (fileTreeNodeFromListFileWithPath loadOk.filesAsFlatList)
                                    projectStateLoadingFromLink.projectStateDescription.differenceFromBase
                                    stateBefore

                            Err loadingError ->
                                ( { stateBefore
                                    | workspace = WorkspaceErr (describeErrorLoadingContentsFromGit loadingError)
                                  }
                                , Cmd.none
                                )

                    else
                        ( stateBefore, Cmd.none )

                _ ->
                    ( stateBefore, Cmd.none )


updateForLoadedProjectState :
    { expectedCompositionHash : Maybe String, filePathToOpen : Maybe (List String) }
    -> ProjectState.FileTreeNode
    -> ProjectState_2021_01.ProjectStateDifference
    -> State
    -> ( State, Cmd Event )
updateForLoadedProjectState config loadedBaseProjectState projectStateDiff stateBefore =
    ( (case ProjectState.applyProjectStateDifference_2021_01 projectStateDiff loadedBaseProjectState of
        Err error ->
            Err ("Failed to apply difference model to compute project state: " ++ error)

        Ok composedProjectState ->
            let
                composedProjectStateHashBase16 =
                    FrontendWeb.ProjectStateInUrl.projectStateCompositionHash composedProjectState

                continueIfHashOk =
                    { stateBefore
                        | workspace =
                            { fileTree = composedProjectState, filePathOpenedInEditor = config.filePathToOpen }
                                |> initWorkspaceFromFileTreeAndFileSelection
                                |> WorkspaceOk
                    }
                        |> Ok
            in
            case config.expectedCompositionHash of
                Nothing ->
                    continueIfHashOk

                Just expectedCompositionHash ->
                    if composedProjectStateHashBase16 == expectedCompositionHash then
                        continueIfHashOk

                    else
                        Err
                            ("Composed project state has hash "
                                ++ composedProjectStateHashBase16
                                ++ " instead of the expected hash "
                                ++ expectedCompositionHash
                            )
      )
        |> Result.Extra.extract
            (\error -> { stateBefore | workspace = WorkspaceErr error })
    , Cmd.none
    )


fileTreeNodeFromListFileWithPath : List FrontendBackendInterface.FileWithPath -> ProjectState.FileTreeNode
fileTreeNodeFromListFileWithPath =
    List.map
        (\file ->
            ( file.path
            , file.contentBase64
                |> Base64.toBytes
                |> Maybe.withDefault ("Failed to decode from Base64" |> Bytes.Encode.string |> Bytes.Encode.encode)
            )
        )
        >> ProjectState.sortedFileTreeFromListOfBlobs


elmFormatCmd : WorkingProjectStateStructure -> Maybe (Cmd WorkspaceEventStructure)
elmFormatCmd state =
    case fileOpenedInEditorFromWorkspace state of
        Nothing ->
            Nothing

        Just ( filePath, fileContent ) ->
            case stringFromFileContent fileContent of
                Nothing ->
                    Nothing

                Just fileContentString ->
                    let
                        request =
                            FrontendBackendInterface.FormatElmModuleTextRequest fileContentString

                        jsonDecoder backendResponse =
                            case backendResponse of
                                FrontendBackendInterface.FormatElmModuleTextResponse formatResponse ->
                                    Json.Decode.succeed formatResponse

                                _ ->
                                    Json.Decode.fail "Unexpected response"
                    in
                    requestToApiCmd request
                        jsonDecoder
                        (\result -> BackendElmFormatResponseEvent { filePath = filePath, result = result })
                        |> Just


loadFromGitCmd : String -> Cmd Event
loadFromGitCmd urlIntoGitRepository =
    let
        backendResponseJsonDecoder backendResponse =
            case backendResponse of
                FrontendBackendInterface.LoadCompositionResponse loadComposition ->
                    Json.Decode.succeed loadComposition

                FrontendBackendInterface.ErrorResponse error ->
                    Json.Decode.fail ("The server reported an error: " ++ error)

                _ ->
                    Json.Decode.fail "Unexpected response: Not a LoadCompositionResponse"
    in
    requestToApiCmd
        (FrontendBackendInterface.LoadCompositionRequest (String.trim urlIntoGitRepository))
        backendResponseJsonDecoder
        (BackendLoadFromGitResultEvent urlIntoGitRepository)


userInputCompileFileOpenedInEditor : { time : Time.Posix } -> WorkingProjectStateStructure -> ( WorkingProjectStateStructure, Cmd WorkspaceEventStructure )
userInputCompileFileOpenedInEditor { time } stateBefore =
    case elmMakeRequestForFileOpenedInEditor stateBefore of
        Nothing ->
            ( stateBefore, Cmd.none )

        Just elmMakeRequest ->
            let
                jsonDecoder backendResponse =
                    case backendResponse of
                        FrontendBackendInterface.ElmMakeResponse elmMakeResponse ->
                            Json.Decode.succeed elmMakeResponse

                        _ ->
                            Json.Decode.fail "Unexpected response"
            in
            ( { stateBefore
                | pendingElmMakeRequest = Just { time = time, request = elmMakeRequest }
                , elmMakeResult = Nothing
              }
            , requestToApiCmd
                (FrontendBackendInterface.ElmMakeRequest elmMakeRequest)
                jsonDecoder
                (BackendElmMakeResponseEvent elmMakeRequest)
            )


elmMakeRequestForFileOpenedInEditor : WorkingProjectStateStructure -> Maybe ElmMakeRequestStructure
elmMakeRequestForFileOpenedInEditor workspace =
    case workspace.editing.filePathOpenedInEditor of
        Nothing ->
            Nothing

        Just filePath ->
            let
                base64FromBytes : Bytes.Bytes -> String
                base64FromBytes =
                    Base64.fromBytes
                        >> Maybe.withDefault "Error encoding in base64"

                entryPointFilePath =
                    filePath
            in
            Just
                { entryPointFilePath = entryPointFilePath
                , files =
                    workspace.fileTree
                        |> ProjectState.flatListOfBlobsFromFileTreeNode
                        |> List.map
                            (\( path, content ) ->
                                { path = path
                                , contentBase64 = content |> base64FromBytes
                                }
                            )
                }


requestToApiCmd :
    FrontendBackendInterface.RequestStructure
    -> (FrontendBackendInterface.ResponseStructure -> Json.Decode.Decoder event)
    -> (Result Http.Error event -> mappedEvent)
    -> Cmd mappedEvent
requestToApiCmd request jsonDecoderSpecialization eventConstructor =
    let
        jsonDecoder =
            ElmFullstackCompilerInterface.GenerateJsonCoders.jsonDecodeResponseStructure
                |> Json.Decode.andThen jsonDecoderSpecialization
    in
    Http.post
        { url = Url.Builder.absolute [ "api" ] []
        , body =
            Http.jsonBody
                (request |> ElmFullstackCompilerInterface.GenerateJsonCoders.jsonEncodeRequestStructure)
        , expect = Http.expectJson (\response -> eventConstructor response) jsonDecoder
        }


view : State -> Browser.Document Event
view state =
    let
        elementToDisplayLoadFromGitError loadError =
            [ describeErrorLoadingContentsFromGit loadError
                |> String.left 500
                |> Element.text
            ]
                |> Element.paragraph [ Element.Font.color (Element.rgb 1 0.64 0) ]

        mainContentFromLoadingFromLink { linkUrl, progressOrResultElement, expectedCompositionHash } =
            [ Element.text "Loading project from "
            , linkElementFromUrlAndTextLabel
                { url = linkUrl
                , labelText = linkUrl
                }
            , Element.text (expectedCompositionHash |> Maybe.map ((++) "Expecting composition with hash ") |> Maybe.withDefault "")
            , progressOrResultElement
            ]
                |> List.map (Element.el [ Element.centerX ])
                |> Element.column
                    [ Element.spacing (defaultFontSize // 2)
                    , Element.width Element.fill
                    ]

        mainContent =
            case state.workspace of
                WorkspaceLoadingFromLink loadingProjectStateFromLink ->
                    let
                        loadResult =
                            state.lastBackendLoadFromGitResult
                                |> Maybe.andThen
                                    (\( requestUrl, result ) ->
                                        if requestUrl == loadingProjectStateFromLink.projectStateDescription.base then
                                            Just result

                                        else
                                            Nothing
                                    )

                        progressOrResultElement =
                            case loadResult of
                                Nothing ->
                                    Element.text "Loading in progress ..."

                                Just (Err loadError) ->
                                    elementToDisplayLoadFromGitError loadError

                                Just (Ok _) ->
                                    Element.text "Completed"

                        expectedCompositionHash =
                            if loadingProjectStateFromLink.projectStateDescription.differenceFromBase == ProjectState_2021_01.noDifference then
                                loadingProjectStateFromLink.expectedCompositionHash

                            else
                                Nothing
                    in
                    mainContentFromLoadingFromLink
                        { linkUrl = loadingProjectStateFromLink.projectStateDescription.base
                        , progressOrResultElement = progressOrResultElement
                        , expectedCompositionHash = expectedCompositionHash
                        }

                WorkspaceOk workingState ->
                    let
                        paneLayout paneProperties =
                            let
                                widthFillPortion =
                                    if workingState.viewEnlargedPane == Just paneProperties.pane then
                                        40

                                    else
                                        4
                            in
                            [ [ paneProperties.buttons
                                    |> Element.row
                                        [ Element.spacing defaultFontSize
                                        , Element.width Element.fill
                                        , Element.clipX
                                        ]
                              , toggleEnlargedPaneButton workingState paneProperties.pane |> Element.map WorkspaceEvent
                              ]
                                |> Element.row
                                    [ Element.spacing defaultFontSize
                                    , Element.padding (defaultFontSize // 2)
                                    , Element.width Element.fill
                                    ]
                            , paneProperties.mainContent
                            ]
                                |> Element.column
                                    [ Element.width (Element.fillPortion widthFillPortion |> Element.minimum 80)
                                    , Element.height Element.fill
                                    ]

                        workspaceView =
                            case workingState.editing.filePathOpenedInEditor of
                                Nothing ->
                                    { editorPaneButtons =
                                        [ saveButton, loadFromGitOpenDialogButton ]
                                    , outputPaneButtons =
                                        []
                                    , editorPaneContent =
                                        let
                                            selectEventFromFileTreeNode upperPath ( nodeName, nodeContent ) =
                                                case nodeContent of
                                                    ProjectState.BlobNode _ ->
                                                        Just (UserInputOpenFileInEditor (upperPath ++ [ nodeName ]))

                                                    ProjectState.TreeNode _ ->
                                                        Nothing
                                        in
                                        [ Element.text "Choose one of the files in the project to open in the editor"
                                        , viewFileTree { selectEventFromNode = selectEventFromFileTreeNode } workingState.fileTree
                                        ]
                                            |> Element.column
                                                [ Element.spacing defaultFontSize
                                                , Element.padding defaultFontSize
                                                , Element.width Element.fill
                                                ]
                                            |> Element.map WorkspaceEvent
                                    }

                                Just _ ->
                                    { editorPaneButtons =
                                        [ saveButton, buttonElement { label = "📄 Format", onPress = Just UserInputFormat } |> Element.map WorkspaceEvent ]
                                    , outputPaneButtons =
                                        [ buttonElement { label = "▶️ Compile", onPress = Just UserInputCompile } ]
                                            |> List.map (Element.map WorkspaceEvent)
                                    , editorPaneContent =
                                        monacoEditorElement state
                                    }
                    in
                    [ paneLayout
                        { pane = EditorPane
                        , buttons = workspaceView.editorPaneButtons
                        , mainContent = workspaceView.editorPaneContent
                        }
                    , paneLayout
                        { pane = OutputPane
                        , buttons = workspaceView.outputPaneButtons
                        , mainContent =
                            workingState
                                |> viewOutputPaneContent
                                |> Element.el
                                    [ Element.width Element.fill
                                    , Element.height Element.fill

                                    -- https://github.com/mdgriffith/elm-ui/issues/149#issuecomment-531480958
                                    , Element.clip
                                    , Element.htmlAttribute (HA.style "flex-shrink" "1")
                                    ]
                                |> Element.map WorkspaceEvent
                        }
                    ]
                        |> Element.row [ Element.width Element.fill, Element.height Element.fill ]

                WorkspaceErr projectStateError ->
                    [ [ loadFromGitOpenDialogButton ]
                        |> Element.row
                            [ Element.spacing defaultFontSize
                            , Element.padding (defaultFontSize // 2)
                            ]
                    , [ ("Failed to load project state: "
                            ++ (projectStateError |> String.left 500)
                        )
                            |> Element.text
                      ]
                        |> Element.paragraph
                            [ Element.Font.color (Element.rgb 1 0.64 0)
                            , Element.padding defaultFontSize
                            , Element.width Element.fill
                            ]
                    ]
                        |> Element.column
                            [ Element.spacing (defaultFontSize // 2)
                            , Element.width (Element.fillPortion 4)
                            , Element.height Element.fill
                            ]

        popupAttributes =
            case state.modalDialog of
                Nothing ->
                    []

                Just (SaveOrShareDialog saveOrShareDialog) ->
                    case state.workspace of
                        WorkspaceOk workingState ->
                            viewSaveOrShareDialog saveOrShareDialog workingState.fileTree
                                |> popupElementAttributesFromAttributes

                        _ ->
                            []

                Just (LoadFromGitDialog dialogState) ->
                    let
                        urlInputElement =
                            Element.Input.text
                                [ Element.Background.color backgroundColor ]
                                { onChange = \url -> UserInputLoadFromGit (LoadFromGitEnterUrlEvent { urlIntoGitRepository = url })
                                , text = dialogState.urlIntoGitRepository
                                , placeholder = Just (Element.Input.placeholder [] (Element.text "URL to tree in git repository"))
                                , label = Element.Input.labelAbove [] (Element.text "URL to tree in git repository")
                                }

                        offerBeginLoading =
                            (dialogState.urlIntoGitRepository /= "")
                                && (dialogState.requestTime == Nothing)
                                && (dialogState.loadCompositionResult == Nothing)

                        sendRequestButton =
                            buttonElement
                                { label = "Begin Loading"
                                , onPress =
                                    Just
                                        (UserInputLoadFromGit
                                            (LoadFromGitBeginRequestEvent { urlIntoGitRepository = dialogState.urlIntoGitRepository })
                                        )
                                }
                                |> Element.el [ Element.centerX, elementTransparent (not offerBeginLoading) ]

                        resultElement =
                            case dialogState.loadCompositionResult of
                                Nothing ->
                                    Element.none

                                Just (Err loadError) ->
                                    elementToDisplayLoadFromGitError loadError

                                Just (Ok loadOk) ->
                                    [ [ ("Loaded composition "
                                            ++ loadOk.compositionId
                                            ++ " containing "
                                            ++ (loadOk.filesAsFlatList |> List.length |> String.fromInt)
                                            ++ " files:"
                                        )
                                            |> Element.text
                                      ]
                                        |> Element.paragraph []
                                    , loadOk.filesAsFlatList
                                        |> List.sortBy (.path >> List.length)
                                        |> List.map (.path >> String.join "/" >> Element.text)
                                        |> Element.column [ Element.spacing 4, Element.padding 8 ]
                                    , buttonElement
                                        { label = "Set these files as project state"
                                        , onPress = Just (UserInputLoadFromGit LoadFromGitTakeResultAsProjectStateEvent)
                                        }
                                        |> Element.el [ Element.centerX ]
                                    ]
                                        |> Element.column [ Element.spacing (defaultFontSize // 2) ]
                    in
                    popupElementAttributesFromAttributes
                        { title = "Load Project from Tree in Git Repository"
                        , guide = "Load project files from a URL to a tree in a git repository. Here is an example of such a URL: https://github.com/onlinegamemaker/making-online-games/tree/7b0fe6018e6f464bbee193f063d26c80cc6e6653/games-program-codes/simple-snake"
                        , contentElement =
                            [ urlInputElement
                            , sendRequestButton
                            , resultElement
                            ]
                                |> Element.column
                                    [ Element.width Element.fill
                                    , Element.spacing defaultFontSize
                                    ]
                        }

        body =
            [ activityBar, mainContent ]
                |> Element.row
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    ]
                |> Element.layout
                    ([ Element.Font.family (rootFontFamily |> List.map Element.Font.typeface)
                     , Element.Font.size defaultFontSize
                     , Element.Font.color (Element.rgb 0.95 0.95 0.95)
                     , Element.Background.color backgroundColor
                     , Element.width Element.fill
                     ]
                        ++ popupAttributes
                    )
    in
    { title = "Elm Editor", body = [ body ] }


type alias FileTreeNodeViewModel event =
    { indentLevel : Int
    , label : String
    , selectEvent : Maybe event
    }


viewFileTree :
    { selectEventFromNode : List String -> ( String, ProjectState.FileTreeNode ) -> Maybe event }
    -> ProjectState.FileTreeNode
    -> Element.Element event
viewFileTree configuration rootNode =
    case rootNode of
        ProjectState.BlobNode _ ->
            Element.text "Error: Root node is a blob, not a tree"

        ProjectState.TreeNode treeItems ->
            treeItems
                |> List.concatMap (buildFileTreeViewList configuration [])
                |> viewFileTreeList


buildFileTreeViewList :
    { selectEventFromNode : List String -> ( String, ProjectState.FileTreeNode ) -> Maybe event }
    -> List String
    -> ( String, ProjectState.FileTreeNode )
    -> List (FileTreeNodeViewModel event)
buildFileTreeViewList configuration path ( currentNodeName, currentNodeContent ) =
    case currentNodeContent of
        ProjectState.BlobNode _ ->
            [ { indentLevel = List.length path
              , label = currentNodeName
              , selectEvent = configuration.selectEventFromNode path ( currentNodeName, currentNodeContent )
              }
            ]

        ProjectState.TreeNode tree ->
            { indentLevel = List.length path
            , label = currentNodeName
            , selectEvent = configuration.selectEventFromNode path ( currentNodeName, currentNodeContent )
            }
                :: List.concatMap (buildFileTreeViewList configuration (path ++ [ currentNodeName ])) tree


viewFileTreeList : List (FileTreeNodeViewModel event) -> Element.Element event
viewFileTreeList items =
    items
        |> List.map
            (\item ->
                let
                    interactionAttributes =
                        item.selectEvent
                            |> Maybe.map
                                (\event ->
                                    [ Element.Events.onClick event
                                    , Element.mouseOver [ Element.Background.color (Element.rgba 1 1 1 0.1) ]
                                    , Element.pointer
                                    ]
                                )
                            |> Maybe.withDefault []

                    currentNodeElement =
                        Element.text item.label
                            |> Element.el [ Element.padding 5 ]
                            |> Element.el
                                (Element.paddingEach { left = item.indentLevel * defaultFontSize, right = 0, top = 0, bottom = 0 }
                                    :: Element.width Element.fill
                                    :: interactionAttributes
                                )
                in
                currentNodeElement
            )
        |> Element.column [ Element.width Element.fill ]


toggleEnlargedPaneButton : WorkingProjectStateStructure -> WorkspacePane -> Element.Element WorkspaceEventStructure
toggleEnlargedPaneButton state pane =
    let
        isPaneEnlarged =
            state.viewEnlargedPane == Just pane

        ( icon, onPress ) =
            if isPaneEnlarged then
                ( Visuals.ShrinkActionIcon, Nothing )

            else
                ( Visuals.GrowActionIcon, Just pane )

        iconSize =
            24
    in
    Element.Input.button
        [ Element.Background.color (Element.rgb 0.2 0.2 0.2)
        , Element.mouseOver [ Element.Background.color (Element.rgb 0 0.5 0.8) ]
        , Element.padding 4
        ]
        { label =
            Visuals.iconSvgElementFromIcon { color = "rgba(255,255,255,0.7)" } icon
                |> Element.el [ Element.width (Element.px iconSize), Element.height (Element.px iconSize) ]
        , onPress = Just (UserInputSetEnlargedPane onPress)
        }
        |> Element.el [ Element.alignRight ]


type alias PopupAttributes event =
    { title : String
    , guide : String
    , contentElement : Element.Element event
    }


viewSaveOrShareDialog : SaveOrShareDialogState -> ProjectState.FileTreeNode -> PopupAttributes Event
viewSaveOrShareDialog saveOrShareDialog projectState =
    let
        projectFiles =
            projectState |> ProjectState.flatListOfBlobsFromFileTreeNode

        projectSummaryElement =
            [ ("This project contains "
                ++ (projectFiles |> List.length |> String.fromInt)
                ++ " files with an aggregate size of "
                ++ (projectFiles |> List.map (Tuple.second >> Bytes.width) |> List.sum |> String.fromInt)
                ++ " bytes."
              )
                |> Element.text
            ]
                |> Element.paragraph []

        buttonGenerateUrl =
            buttonElement
                { label = "Generate link to project"
                , onPress = Just (UserInputSave (Just { createDiffIfBaseAvailable = True }))
                }

        linkElementFromUrl urlToProject =
            let
                maybeDependencyUrl =
                    case
                        urlToProject
                            |> Url.fromString
                            |> Maybe.andThen FrontendWeb.ProjectStateInUrl.projectStateDescriptionFromUrl
                    of
                        Nothing ->
                            Nothing

                        Just (Err _) ->
                            Nothing

                        Just (Ok projectDescription) ->
                            case projectDescription of
                                FrontendWeb.ProjectStateInUrl.LiteralProjectState _ ->
                                    Nothing

                                FrontendWeb.ProjectStateInUrl.LinkProjectState link ->
                                    Just link

                                FrontendWeb.ProjectStateInUrl.DiffProjectState diffProjectState ->
                                    Just diffProjectState.base

                dependenciesDescriptionLines =
                    maybeDependencyUrl
                        |> Maybe.map
                            (\dependencyUrl ->
                                [ [ Element.text "The project state model in this link depends on loading related data from the following URL: "
                                  , linkElementFromUrlAndTextLabel { url = dependencyUrl, labelText = dependencyUrl }
                                  ]
                                    |> Element.paragraph []
                                ]
                            )
                        |> Maybe.withDefault []

                linkDescriptionLines =
                    Element.paragraph [] [ Element.text ("Length of this link URL: " ++ String.fromInt (String.length urlToProject)) ]
                        :: dependenciesDescriptionLines
            in
            [ Element.html (htmlOfferingTextToCopy urlToProject)
            , linkDescriptionLines
                |> Element.textColumn [ Element.Font.size ((defaultFontSize * 4) // 5), Element.padding defaultFontSize ]
            ]
                |> Element.column
                    [ Element.spacing defaultFontSize
                    , Element.width Element.fill
                    ]

        urlElement =
            case saveOrShareDialog.urlToProject of
                Nothing ->
                    linkElementFromUrl ""
                        |> Element.el [ elementTransparent True ]

                Just urlToProject ->
                    linkElementFromUrl urlToProject
    in
    { title = "Save or Share Project"
    , guide = "Get a link that you or others can later use to load the project's current state into the editor again."
    , contentElement =
        [ projectSummaryElement
        , buttonGenerateUrl |> Element.el [ Element.centerX ]
        , urlElement |> Element.el [ Element.width Element.fill ]
        ]
            |> Element.column
                [ Element.spacing defaultFontSize
                , Element.width Element.fill
                ]
    }


describeErrorLoadingContentsFromGit : Http.Error -> String
describeErrorLoadingContentsFromGit loadError =
    "Failed to load contents from git: " ++ describeHttpError loadError


activityBarWidth : Int
activityBarWidth =
    50


activityBar : Element.Element event
activityBar =
    let
        actionItemSpacing =
            4

        actionItemIconPadding =
            8

        bottomActionItems =
            [ { icon = Visuals.ChatActionIcon, linkUrl = Just "https://github.com/elm-fullstack/elm-fullstack/discussions" }
            , { icon = Visuals.GitHubActionIcon, linkUrl = Just "https://github.com/elm-fullstack/elm-fullstack/tree/main/implement/example-apps/elm-editor" }
            ]

        actionItemWrapper { icon, linkUrl } =
            let
                linkWrapper =
                    case linkUrl of
                        Nothing ->
                            identity

                        Just justLinkUrl ->
                            \linkLabel ->
                                Element.link
                                    [ Element.width Element.fill ]
                                    { url = justLinkUrl, label = linkLabel }
            in
            Visuals.iconSvgElementFromIcon { color = "white" } icon
                |> Element.el
                    [ Element.padding actionItemIconPadding
                    , Element.centerX
                    , Element.width Element.fill
                    , Element.alpha 0.4
                    , Element.mouseOver [ Element.alpha 1 ]
                    , Element.pointer
                    ]
                >> linkWrapper
    in
    [ bottomActionItems
        |> List.map actionItemWrapper
        |> Element.column
            [ Element.width Element.fill
            , Element.spacing actionItemSpacing
            , Element.alignBottom
            ]
    ]
        |> Element.column
            [ Element.width (Element.px activityBarWidth)
            , Element.height Element.fill
            , Element.Background.color (Element.rgb 0.2 0.2 0.2)
            ]


popupElementAttributesFromAttributes : PopupAttributes Event -> List (Element.Attribute Event)
popupElementAttributesFromAttributes { title, guide, contentElement } =
    [ [ title |> Element.text |> Element.el (headingAttributes 3)
      , [ guide |> Element.text ] |> Element.paragraph [ elementFontSizePercent 80 ]
      , contentElement
      ]
        |> Element.column
            [ Element.spacing defaultFontSize
            , Element.padding defaultFontSize
            , Element.width Element.fill
            ]
        |> Element.el
            [ Element.Background.color (Element.rgb 0 0 0)
            , Element.Border.color (Element.rgb 0.8 0.8 0.8)
            , Element.Border.width 2
            , Element.width (Element.px 800)
            , Element.centerX
            , Element.centerY
            , Element.htmlAttribute
                (Html.Events.custom "click"
                    (Json.Decode.succeed
                        { message = DiscardEvent
                        , stopPropagation = True
                        , preventDefault = False
                        }
                    )
                )
            ]
        |> Element.el
            [ Element.Background.color (Element.rgba 0 0 0 0.3)
            , Element.height Element.fill
            , Element.width Element.fill
            , Element.Events.onClick UserInputCloseModalDialog
            , Element.htmlAttribute (HA.style "backdrop-filter" "blur(1px)")
            ]
        |> Element.inFront
    ]


viewOutputPaneContent : WorkingProjectStateStructure -> Element.Element WorkspaceEventStructure
viewOutputPaneContent state =
    case state.elmMakeResult of
        Nothing ->
            case state.pendingElmMakeRequest of
                Nothing ->
                    if filePathOpenedInEditorFromWorkspace state == Nothing then
                        Element.none

                    else
                        [ "No compilation started. You can use the 'Compile' button to check program text for errors and see your app in action."
                            |> Element.text
                        ]
                            |> Element.paragraph [ Element.padding defaultFontSize ]

                Just _ ->
                    Element.text "Compiling..." |> Element.el [ Element.padding defaultFontSize ]

        Just ( elmMakeRequest, elmMakeResult ) ->
            case elmMakeResult of
                Err elmMakeError ->
                    ("Error: " ++ describeHttpError elmMakeError) |> Element.text

                Ok elmMakeOk ->
                    let
                        elmMakeRequestFromCurrentState =
                            elmMakeRequestForFileOpenedInEditor state

                        currentFileContentIsStillSame =
                            Just elmMakeRequest.files
                                == (elmMakeRequestFromCurrentState |> Maybe.map .files)

                        warnAboutOutdatedCompilationText =
                            if
                                Just elmMakeRequest.entryPointFilePath
                                    /= filePathOpenedInEditorFromWorkspace state
                            then
                                Just
                                    ("⚠️ Last compilation started for another file: '"
                                        ++ String.join "/" elmMakeRequest.entryPointFilePath
                                        ++ "'"
                                    )

                            else if currentFileContentIsStillSame then
                                Nothing

                            else
                                Just "⚠️ File contents changed since compiling"

                        warnAboutOutdatedCompilationElement =
                            warnAboutOutdatedCompilationText
                                |> Maybe.withDefault ""
                                |> Element.text
                                |> Element.el
                                    [ Element.padding (defaultFontSize // 2)
                                    , Element.Background.color (Element.rgb 0.3 0.2 0.1)
                                    , Element.width Element.fill
                                    , Element.transparent (warnAboutOutdatedCompilationText == Nothing)
                                    ]

                        outputElementFromPlainText outputText =
                            [ outputText
                                |> Html.text
                                |> Element.html
                            ]
                                |> Element.paragraph
                                    [ Element.htmlAttribute (HA.style "white-space" "pre-wrap")
                                    , Element.htmlAttribute attributeMonospaceFont
                                    ]

                        compileResultElement =
                            case elmMakeOk.compiledHtmlDocument of
                                Nothing ->
                                    let
                                        standardErrorElement =
                                            case elmMakeOk.reportFromJson of
                                                Nothing ->
                                                    outputElementFromPlainText elmMakeOk.response.processOutput.standardError

                                                Just (Err decodeError) ->
                                                    outputElementFromPlainText
                                                        ("Failed to decode JSON report: " ++ decodeError)

                                                Just (Ok (ElmMakeExecutableFile.CompileErrorsReport compileErrors)) ->
                                                    compileErrors
                                                        |> List.map (viewElmMakeCompileError elmMakeRequest)
                                                        |> Element.column
                                                            [ Element.spacing defaultFontSize
                                                            , Element.width Element.fill
                                                            ]

                                                Just (Ok (ElmMakeExecutableFile.ErrorReport error)) ->
                                                    [ error.title
                                                        |> Element.text
                                                        |> Element.el [ Element.Font.bold ]
                                                    , viewElementFromElmMakeCompileErrorMessage error.message
                                                    ]
                                                        |> Element.column
                                                            [ Element.spacing (defaultFontSize // 2)
                                                            , Element.width Element.fill
                                                            ]
                                    in
                                    [ ( "standard error", standardErrorElement )
                                    , ( "standard output", outputElementFromPlainText elmMakeOk.response.processOutput.standardOutput )
                                    ]
                                        |> List.map
                                            (\( channel, outputElement ) ->
                                                [ channel |> Element.text |> Element.el (headingAttributes 3)
                                                , outputElement |> indentOneLevel
                                                ]
                                                    |> Element.column
                                                        [ Element.spacing (defaultFontSize // 2)
                                                        , Element.width Element.fill
                                                        ]
                                            )
                                        |> Element.column
                                            [ Element.spacing (defaultFontSize * 2)
                                            , Element.width Element.fill
                                            , Element.height Element.fill
                                            , Element.scrollbarY
                                            , Element.padding (defaultFontSize // 2)
                                            ]

                                Just compiledHtmlDocument ->
                                    Html.iframe
                                        [ HA.srcdoc compiledHtmlDocument
                                        , HA.style "height" "98%"
                                        ]
                                        []
                                        |> Element.html
                                        |> Element.el
                                            [ Element.width Element.fill
                                            , Element.height Element.fill
                                            ]
                    in
                    [ warnAboutOutdatedCompilationElement, compileResultElement ]
                        |> Element.column
                            [ Element.spacing (defaultFontSize // 2)
                            , Element.width Element.fill
                            , Element.height Element.fill
                            ]


viewElmMakeCompileError : FrontendBackendInterface.ElmMakeRequestStructure -> ElmMakeExecutableFile.ElmMakeReportCompileErrorStructure -> Element.Element WorkspaceEventStructure
viewElmMakeCompileError elmMakeRequest elmMakeError =
    elmMakeError.problems
        |> List.map
            (\elmMakeProblem ->
                let
                    displayPath =
                        filePathFromExistingPathsAndElmMakeReportPathString
                            (elmMakeRequest.files |> List.map .path)
                            elmMakeError.path

                    problemHeadingElement =
                        [ elmMakeProblem.title
                            |> elmEditorProblemDisplayTitleFromReportTitle
                            |> Element.text
                            |> Element.el [ Element.Font.bold ]
                        , (String.join "/" displayPath
                            ++ " - Line "
                            ++ String.fromInt elmMakeProblem.region.start.line
                            ++ ", Column "
                            ++ String.fromInt elmMakeProblem.region.start.column
                          )
                            |> Element.text
                            |> Element.el
                                (Element.Events.onClick
                                    (UserInputRevealPositionInEditor
                                        { filePath = displayPath
                                        , lineNumber = elmMakeProblem.region.start.line
                                        , column = elmMakeProblem.region.start.column
                                        }
                                    )
                                    :: elementLinkStyleAttributes
                                )
                        ]
                            |> Element.column
                                [ Element.spacing (defaultFontSize // 2)
                                , Element.width Element.fill
                                ]
                in
                [ problemHeadingElement
                , viewElementFromElmMakeCompileErrorMessage elmMakeProblem.message |> indentOneLevel
                ]
                    |> Element.column
                        [ Element.spacing (defaultFontSize // 2)
                        , Element.width Element.fill
                        ]
            )
        |> Element.column
            [ Element.spacing defaultFontSize
            , Element.width Element.fill
            ]


viewElementFromElmMakeCompileErrorMessage : List ElmMakeExecutableFile.ElmMakeReportMessageListItem -> Element.Element a
viewElementFromElmMakeCompileErrorMessage =
    let
        elementFromStyledTextElement styledTextElement =
            [ styledTextElement.string |> Html.text ]
                |> Html.span
                    [ HA.style "font-weight"
                        (if styledTextElement.bold then
                            "bold"

                         else
                            "inherit"
                        )
                    , HA.style "text-decoration"
                        (if styledTextElement.underline then
                            "underline"

                         else
                            "none"
                        )
                    , HA.style "color" (styledTextElement.color |> Maybe.withDefault "inherit")
                    ]
    in
    List.map styledTextFromElmMakeReportMessageListItem
        >> List.map elementFromStyledTextElement
        >> Html.span [ HA.style "font-size" "90%", HA.style "filter" "contrast(0.5) brightness(1.3)" ]
        >> Element.html
        >> List.singleton
        >> Element.paragraph
            [ Element.htmlAttribute (HA.style "white-space" "pre-wrap")
            , Element.htmlAttribute attributeMonospaceFont
            ]


editorDocumentMarkersFromElmMakeReport :
    { elmMakeRequest : ElmMakeRequestStructure, fileOpenedInEditor : ( List String, Bytes.Bytes ) }
    -> Maybe (Result String ElmMakeExecutableFile.ElmMakeReportFromJson)
    -> List FrontendWeb.MonacoEditor.EditorMarker
editorDocumentMarkersFromElmMakeReport { elmMakeRequest, fileOpenedInEditor } maybeReportFromJson =
    case maybeReportFromJson of
        Nothing ->
            []

        Just reportFromJson ->
            let
                filePathOpenedInEditor =
                    Tuple.first fileOpenedInEditor

                fileOpenedInEditorBase64 =
                    fileOpenedInEditor
                        |> Tuple.second
                        |> Base64.fromBytes
                        |> Maybe.withDefault "Error encoding in base64"
            in
            case elmMakeRequest.files |> List.filter (.path >> (==) filePathOpenedInEditor) |> List.head of
                Nothing ->
                    []

                Just requestFile ->
                    if requestFile.contentBase64 /= fileOpenedInEditorBase64 then
                        []

                    else
                        case reportFromJson of
                            Err _ ->
                                []

                            Ok report ->
                                case report of
                                    ElmMakeExecutableFile.ErrorReport _ ->
                                        []

                                    ElmMakeExecutableFile.CompileErrorsReport compileErrors ->
                                        compileErrors
                                            |> List.filter
                                                (.path
                                                    >> filePathFromExistingPathsAndElmMakeReportPathString
                                                        (elmMakeRequest.files |> List.map .path)
                                                    >> (==) filePathOpenedInEditor
                                                )
                                            |> List.concatMap .problems
                                            |> List.map editorDocumentMarkerFromElmMakeProblem


elmEditorProblemDisplayTitleFromReportTitle : String -> String
elmEditorProblemDisplayTitleFromReportTitle =
    String.toLower >> String.Extra.toTitleCase


elmMakeReportTextFromMessageItem : ElmMakeExecutableFile.ElmMakeReportMessageListItem -> String
elmMakeReportTextFromMessageItem messageItem =
    case messageItem of
        ElmMakeExecutableFile.ElmMakeReportMessageListItemPlain text ->
            text

        ElmMakeExecutableFile.ElmMakeReportMessageListItemStyled styled ->
            styled.string


editorDocumentMarkerFromElmMakeProblem : ElmMakeExecutableFile.ElmMakeReportProblem -> FrontendWeb.MonacoEditor.EditorMarker
editorDocumentMarkerFromElmMakeProblem elmMakeProblem =
    { message =
        "# "
            ++ elmEditorProblemDisplayTitleFromReportTitle elmMakeProblem.title
            ++ "\n"
            ++ (elmMakeProblem.message |> List.map elmMakeReportTextFromMessageItem |> String.join "")
    , startLineNumber = elmMakeProblem.region.start.line
    , startColumn = elmMakeProblem.region.start.column
    , endLineNumber = elmMakeProblem.region.end.line
    , endColumn = elmMakeProblem.region.end.column
    , severity = FrontendWeb.MonacoEditor.ErrorSeverity
    }


filePathFromExistingPathsAndElmMakeReportPathString : List (List String) -> String -> List String
filePathFromExistingPathsAndElmMakeReportPathString existingPaths elmMakeReportPathString =
    let
        pathSegments =
            elmMakeReportPathString
                |> String.split "/"
                |> List.concatMap (String.split "\\")
    in
    pathSegments
        |> List.Extra.tails
        |> List.filter (List.isEmpty >> not)
        |> List.filter (\pathEnd -> existingPaths |> List.member pathEnd)
        |> List.head
        |> Maybe.withDefault pathSegments


styledTextFromElmMakeReportMessageListItem : ElmMakeExecutableFile.ElmMakeReportMessageListItem -> { string : String, bold : Bool, underline : Bool, color : Maybe String }
styledTextFromElmMakeReportMessageListItem elmMakeReportMessageListItem =
    case elmMakeReportMessageListItem of
        ElmMakeExecutableFile.ElmMakeReportMessageListItemPlain text ->
            { string = text
            , bold = False
            , underline = False
            , color = Nothing
            }

        ElmMakeExecutableFile.ElmMakeReportMessageListItemStyled styled ->
            styled


buttonElement : { label : String, onPress : Maybe event } -> Element.Element event
buttonElement buttonConfig =
    Element.Input.button
        [ Element.Background.color (Element.rgb 0.2 0.2 0.2)
        , Element.mouseOver
            [ Element.Background.color (Element.rgb 0 0.5 0.8) ]
        , Element.paddingXY defaultFontSize (defaultFontSize // 2)
        , Element.Border.widthEach { left = 0, right = 0, top = 0, bottom = 2 }
        , Element.Border.color (Element.rgb 0 0.5 0.8)
        ]
        { label = Element.text buttonConfig.label
        , onPress = buttonConfig.onPress
        }


saveButton : Element.Element Event
saveButton =
    buttonElement { label = "💾 Save", onPress = Just (UserInputSave Nothing) }


loadFromGitOpenDialogButton : Element.Element Event
loadFromGitOpenDialogButton =
    buttonElement
        { label = "📂 Load From Git"
        , onPress = Just (UserInputLoadFromGit LoadFromGitOpenDialog)
        }


setTextInMonacoEditorCmd : String -> Cmd WorkspaceEventStructure
setTextInMonacoEditorCmd =
    FrontendWeb.MonacoEditor.SetValue
        >> ElmFullstackCompilerInterface.GenerateJsonCoders.jsonEncodeMessageToMonacoEditor
        >> sendMessageToMonacoFrame


revealPositionInCenterInMonacoEditorCmd : { lineNumber : Int, column : Int } -> Cmd WorkspaceEventStructure
revealPositionInCenterInMonacoEditorCmd =
    FrontendWeb.MonacoEditor.RevealPositionInCenter
        >> ElmFullstackCompilerInterface.GenerateJsonCoders.jsonEncodeMessageToMonacoEditor
        >> sendMessageToMonacoFrame


setModelMarkersInMonacoEditorCmd : List FrontendWeb.MonacoEditor.EditorMarker -> Cmd WorkspaceEventStructure
setModelMarkersInMonacoEditorCmd =
    FrontendWeb.MonacoEditor.SetModelMarkers
        >> ElmFullstackCompilerInterface.GenerateJsonCoders.jsonEncodeMessageToMonacoEditor
        >> sendMessageToMonacoFrame


monacoEditorElement : State -> Element.Element event
monacoEditorElement _ =
    Html.iframe
        [ HA.src "/monaco"
        , HA.id "monaco-iframe"
        , HA.style "width" "100%"
        , HA.style "height" "100%"
        , HA.style "border" "0"
        ]
        []
        |> Element.html


defaultProject : { fileTree : ProjectState.FileTreeNode, filePathOpenedInEditor : Maybe (List String) }
defaultProject =
    { fileTree =
        ProjectState.TreeNode
            [ ( "elm.json"
              , ProjectState.BlobNode ElmFullstackCompilerInterface.SourceFiles.file____default_app_elm_json
              )
            , ( "src"
              , ProjectState.TreeNode
                    [ ( "Main.elm"
                      , ProjectState.BlobNode ElmFullstackCompilerInterface.SourceFiles.file____default_app_src_Main_elm
                      )
                    ]
              )
            ]
    , filePathOpenedInEditor = Just [ "src", "Main.elm" ]
    }


initWorkspaceFromFileTreeAndFileSelection :
    { fileTree : ProjectState.FileTreeNode, filePathOpenedInEditor : Maybe (List String) }
    -> WorkingProjectStateStructure
initWorkspaceFromFileTreeAndFileSelection { fileTree, filePathOpenedInEditor } =
    { fileTree = fileTree
    , editing = { filePathOpenedInEditor = filePathOpenedInEditor }
    , decodeMessageFromMonacoEditorError = Nothing
    , lastTextReceivedFromEditor = Nothing
    , pendingElmMakeRequest = Nothing
    , elmMakeResult = Nothing
    , elmFormatResult = Nothing
    , viewEnlargedPane = Nothing
    }


stringFromFileContent : Bytes.Bytes -> Maybe String
stringFromFileContent bytes =
    Bytes.Decode.decode (Bytes.Decode.string (Bytes.width bytes)) bytes


fileContentFromString : String -> Bytes.Bytes
fileContentFromString =
    Bytes.Encode.string >> Bytes.Encode.encode


htmlOfferingTextToCopy : String -> Html.Html event
htmlOfferingTextToCopy text =
    Html.input
        [ HA.type_ "text"
        , HA.value text
        , HA.readonly True
        , HA.style "width" "100%"
        , HA.style "padding" "0.4em"
        , HA.style "font-family" "inherit"
        , HA.style "color" "inherit"
        , HA.style "background" "inherit"
        ]
        []


describeHttpError : Http.Error -> String
describeHttpError httpError =
    case httpError of
        Http.BadUrl errorMessage ->
            "Bad Url: " ++ errorMessage

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "Network Error"

        Http.BadStatus statusCode ->
            "BadStatus: " ++ (statusCode |> String.fromInt)

        Http.BadBody errorMessage ->
            "BadPayload: " ++ errorMessage


headingAttributes : Int -> List (Element.Attribute event)
headingAttributes rank =
    let
        fontSizePercent =
            max 0 (90 - rank * 20) + 100
    in
    [ elementFontSizePercent fontSizePercent
    , Element.Region.heading rank
    ]


{-| Avoid bug with elm-ui `Element.transparent` as described at <https://github.com/mdgriffith/elm-ui/issues/52#issuecomment-442612549>
-}
elementTransparent : Bool -> Element.Attribute msg
elementTransparent makeTransparent =
    Element.htmlAttribute
        (HA.style "visibility"
            (if makeTransparent then
                "hidden"

             else
                "inherit"
            )
        )


elementFontSizePercent : Int -> Element.Attribute a
elementFontSizePercent percent =
    Element.htmlAttribute (HA.style "font-size" ((percent |> String.fromInt) ++ "%"))


indentOneLevel : Element.Element a -> Element.Element a
indentOneLevel =
    Element.el [ Element.paddingEach { left = defaultFontSize, right = 0, top = 0, bottom = 0 } ]


attributeMonospaceFont : Html.Attribute a
attributeMonospaceFont =
    HA.style "font-family" "monospace, monospace"


defaultFontSize : Int
defaultFontSize =
    16


backgroundColor : Element.Color
backgroundColor =
    Element.rgb 0.13 0.13 0.13


rootFontFamily : List String
rootFontFamily =
    [ "Segoe UI", "Tahoma", "Geneva", "Verdana", "sans-serif" ]


linkElementFromUrlAndTextLabel : { url : String, labelText : String } -> Element.Element event
linkElementFromUrlAndTextLabel { url, labelText } =
    Element.link
        elementLinkStyleAttributes
        { url = url
        , label = labelText |> Element.text
        }


elementLinkStyleAttributes : List (Element.Attribute a)
elementLinkStyleAttributes =
    [ Element.pointer

    -- https://github.com/mdgriffith/elm-ui/issues/158#issuecomment-624231895
    , Element.Border.widthEach { bottom = 1, left = 0, top = 0, right = 0 }
    , Element.Border.color <| Element.rgba 0 0 0 0
    , Element.mouseOver [ Element.Border.color <| Element.rgba 0.7 0.7 1 0.5 ]
    ]
