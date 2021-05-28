port module FrontendWeb.Main exposing (Event(..), State, defaultProject, init, main, receiveMessageFromMonacoFrame, sendMessageToMonacoFrame, update, view)

import Base64
import Browser
import Browser.Events
import Browser.Navigation as Navigation
import Bytes
import Bytes.Decode
import Bytes.Encode
import Common
import CompilationInterface.GenerateJsonCoders
import CompilationInterface.SourceFiles
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Element.Region
import ElmMakeExecutableFile
import File
import File.Download
import File.Select
import FontAwesome.Icon
import FontAwesome.Solid
import FontAwesome.Styles
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
import Keyboard.Event
import Keyboard.Key
import List.Extra
import ProjectState
import ProjectState_2021_01
import Result.Extra
import String.Extra
import Task
import Time
import Url
import Url.Builder
import Zip
import Zip.Entry


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
    , popup : Maybe PopupState
    , lastBackendLoadFromGitResult : Maybe ( String, Result Http.Error BackendLoadFromGitOkWithCache )
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
    , enableInspectionOnCompile : Bool
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
    | UserInputLoadOrImportTakeProjectStateEvent ProjectState.FileTreeNode
    | UserInputClosePopup
    | BackendLoadFromGitResultEvent String (Result Http.Error FrontendBackendInterface.LoadCompositionResponseStructure)
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | WorkspaceEvent WorkspaceEventStructure
    | UserInputGetLinkToProject (Maybe { createDiffIfBaseAvailable : Bool })
    | UserInputLoadedZipArchiveFile File.File
    | UserInputLoadedZipArchiveBytes Bytes.Bytes
    | UserInputImportProjectFromZipArchive UserInputImportFromZipArchiveEventStructure
    | UserInputExportProjectToZipArchive { sendDownloadCmd : Bool }
    | UserInputToggleTitleBarMenu TitlebarMenuEntry
    | UserInputMouseOverTitleBarMenu (Maybe TitlebarMenuEntry)
    | UserInputKeyDownEvent Keyboard.Event.KeyboardEvent
    | UserInputMouseDownOutsideTitlebarMenu
    | DiscardEvent


type WorkspaceEventStructure
    = MonacoEditorEvent Json.Decode.Value
    | UserInputChangeTextInEditor String
    | UserInputOpenFileInEditor (List String)
    | UserInputFormat
    | UserInputCompile
    | UserInputCloseEditor
    | UserInputRevealPositionInEditor { filePath : List String, lineNumber : Int, column : Int }
    | BackendElmFormatResponseEvent { filePath : List String, result : Result Http.Error FrontendBackendInterface.FormatElmModuleTextResponseStructure }
    | BackendElmMakeResponseEvent ElmMakeRequestStructure (Result Http.Error ElmMakeResponseStructure)
    | UserInputSetEnlargedPane (Maybe WorkspacePane)
    | UserInputSetInspectionOnCompile Bool


type UserInputLoadFromGitEventStructure
    = LoadFromGitOpenDialog
    | LoadFromGitEnterUrlEvent { urlIntoGitRepository : String }
    | LoadFromGitBeginRequestEvent { urlIntoGitRepository : String }


type UserInputImportFromZipArchiveEventStructure
    = ImportFromZipArchiveOpenDialog
    | ImportFromZipArchiveSelectFile


type PopupState
    = ModalDialog DialogState
    | TitlebarMenu TitlebarMenuEntry Bool


type DialogState
    = GetLinkToProjectDialog GetLinkToProjectDialogState
    | LoadFromGitDialog LoadFromGitDialogState
    | ExportToZipArchiveDialog
    | ImportFromZipArchiveDialog ImportFromZipArchiveDialogState


type TitlebarMenuEntry
    = ProjectMenuEntry


type alias GetLinkToProjectDialogState =
    { urlToProject : Maybe String }


type alias LoadFromGitDialogState =
    { urlIntoGitRepository : String
    , requestTime : Maybe Time.Posix
    , loadCompositionResult : Maybe (Result Http.Error BackendLoadFromGitOkWithCache)
    }


type alias ImportFromZipArchiveDialogState =
    { loadCompositionResult : Maybe (Result String { fileTree : ProjectState.FileTreeNode, compositionIdCache : String })
    }


type alias BackendLoadFromGitOkWithCache =
    { urlInCommit : String
    , compositionIdCache : String
    , fileTree : ProjectState.FileTreeNode
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
    , Browser.Events.onKeyDown (Keyboard.Event.decodeKeyboardEvent |> Json.Decode.map UserInputKeyDownEvent)
    , Browser.Events.onMouseDown (isTargetOutsideParentWithId titleBarMenubarElementId UserInputMouseDownOutsideTitlebarMenu)
    ]
        |> Sub.batch


init : () -> Url.Url -> Navigation.Key -> Time.Posix -> ( State, Cmd Event )
init _ url navigationKey time =
    { navigationKey = navigationKey
    , url = url
    , time = time
    , workspace = defaultProject |> initWorkspaceFromFileTreeAndFileSelection |> WorkspaceOk
    , popup = Nothing
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

        UserInputToggleTitleBarMenu menuEntry ->
            let
                fromOpened opened =
                    Just (TitlebarMenu menuEntry opened)

                popup =
                    if stateBefore.popup == fromOpened True then
                        fromOpened False

                    else
                        fromOpened True
            in
            ( { stateBefore | popup = popup }, Cmd.none )

        UserInputMouseOverTitleBarMenu maybeMenuEntry ->
            case stateBefore.workspace of
                WorkspaceOk _ ->
                    let
                        fromOpened opened =
                            maybeMenuEntry |> Maybe.map (\menuEntry -> TitlebarMenu menuEntry opened)

                        popup =
                            case stateBefore.popup of
                                Nothing ->
                                    fromOpened False

                                Just (ModalDialog _) ->
                                    stateBefore.popup

                                Just (TitlebarMenu _ opened) ->
                                    if opened && maybeMenuEntry == Nothing then
                                        stateBefore.popup

                                    else
                                        fromOpened opened
                    in
                    ( { stateBefore | popup = popup }, Cmd.none )

                _ ->
                    ( stateBefore, Cmd.none )

        UserInputGetLinkToProject maybeGenerateLink ->
            case stateBefore.workspace of
                WorkspaceOk workingState ->
                    let
                        dialogBefore =
                            (case stateBefore.popup of
                                Just (ModalDialog (GetLinkToProjectDialog getLinkDialog)) ->
                                    Just getLinkDialog

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
                                                stateBefore.lastBackendLoadFromGitResult
                                                    |> Maybe.andThen (Tuple.second >> Result.toMaybe)

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
                    ( { stateBefore | popup = Just (ModalDialog (GetLinkToProjectDialog dialog)) }, cmd )

                _ ->
                    ( stateBefore, Cmd.none )

        UserInputLoadFromGit LoadFromGitOpenDialog ->
            ( { stateBefore
                | popup =
                    Just
                        (ModalDialog
                            (LoadFromGitDialog
                                { urlIntoGitRepository = ""
                                , requestTime = Nothing
                                , loadCompositionResult = Nothing
                                }
                            )
                        )
              }
            , Cmd.none
            )

        UserInputLoadFromGit (LoadFromGitEnterUrlEvent { urlIntoGitRepository }) ->
            case stateBefore.popup of
                Just (ModalDialog (LoadFromGitDialog dialogStateBefore)) ->
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
                        ( { stateBefore | popup = Just (ModalDialog (LoadFromGitDialog dialogState)) }
                        , Cmd.none
                        )

                _ ->
                    ( stateBefore, Cmd.none )

        UserInputLoadFromGit (LoadFromGitBeginRequestEvent { urlIntoGitRepository }) ->
            case stateBefore.popup of
                Just (ModalDialog (LoadFromGitDialog _)) ->
                    let
                        dialogState =
                            { urlIntoGitRepository = urlIntoGitRepository
                            , requestTime = Just stateBefore.time
                            , loadCompositionResult = Nothing
                            }
                    in
                    ( { stateBefore | popup = Just (ModalDialog (LoadFromGitDialog dialogState)) }
                    , loadFromGitCmd urlIntoGitRepository
                    )

                _ ->
                    ( stateBefore, Cmd.none )

        UserInputLoadOrImportTakeProjectStateEvent fileTree ->
            ( { stateBefore
                | popup = Nothing
                , workspace =
                    { fileTree = fileTree
                    , filePathOpenedInEditor = Nothing
                    }
                        |> initWorkspaceFromFileTreeAndFileSelection
                        |> WorkspaceOk
              }
            , Cmd.none
            )

        UserInputExportProjectToZipArchive { sendDownloadCmd } ->
            case stateBefore.workspace of
                WorkspaceOk workingState ->
                    let
                        cmd =
                            if sendDownloadCmd then
                                let
                                    projectStateHash =
                                        FrontendWeb.ProjectStateInUrl.projectStateCompositionHash workingState.fileTree
                                in
                                workingState.fileTree
                                    |> buildZipArchiveFromFileTree
                                    |> Zip.toBytes
                                    |> File.Download.bytes
                                        ("elm-app-" ++ projectStateHash ++ ".zip")
                                        "application/zip"

                            else
                                Cmd.none
                    in
                    ( { stateBefore | popup = Just (ModalDialog ExportToZipArchiveDialog) }, cmd )

                _ ->
                    ( stateBefore, Cmd.none )

        UserInputImportProjectFromZipArchive ImportFromZipArchiveOpenDialog ->
            ( { stateBefore
                | popup = Just (ModalDialog (ImportFromZipArchiveDialog { loadCompositionResult = Nothing }))
              }
            , Cmd.none
            )

        UserInputImportProjectFromZipArchive ImportFromZipArchiveSelectFile ->
            ( stateBefore
            , File.Select.file [ "application/zip" ] UserInputLoadedZipArchiveFile
            )

        UserInputLoadedZipArchiveFile file ->
            ( stateBefore
            , file |> File.toBytes |> Task.perform UserInputLoadedZipArchiveBytes
            )

        UserInputLoadedZipArchiveBytes bytes ->
            case stateBefore.popup of
                Just (ModalDialog (ImportFromZipArchiveDialog _)) ->
                    let
                        loadCompositionResult =
                            bytes
                                |> Zip.fromBytes
                                |> Maybe.map extractFileTreeFromZipArchive
                                |> Maybe.withDefault (Err "Failed to decode this file as zip archive")
                                |> Result.map
                                    (\fileTree ->
                                        { fileTree = fileTree
                                        , compositionIdCache = FrontendWeb.ProjectStateInUrl.projectStateCompositionHash fileTree
                                        }
                                    )
                    in
                    ( { stateBefore
                        | popup =
                            Just
                                (ModalDialog
                                    (ImportFromZipArchiveDialog { loadCompositionResult = Just loadCompositionResult })
                                )
                      }
                    , Cmd.none
                    )

                _ ->
                    ( stateBefore, Cmd.none )

        BackendLoadFromGitResultEvent urlIntoGitRepository result ->
            processEventBackendLoadFromGitResult urlIntoGitRepository result stateBefore

        UserInputClosePopup ->
            ( { stateBefore | popup = Nothing }, Cmd.none )

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

        UserInputKeyDownEvent keyboardEvent ->
            if keyboardEvent.keyCode == Keyboard.Key.Escape then
                update UserInputClosePopup stateBefore

            else
                ( stateBefore, Cmd.none )

        UserInputMouseDownOutsideTitlebarMenu ->
            let
                popupIsOpenForTitlebarMenu =
                    case stateBefore.popup of
                        Just (TitlebarMenu _ _) ->
                            True

                        _ ->
                            False
            in
            if popupIsOpenForTitlebarMenu then
                update UserInputClosePopup stateBefore

            else
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
                    |> Json.Decode.decodeValue CompilationInterface.GenerateJsonCoders.jsonDecodeMessageFromMonacoEditor
            of
                Err decodeError ->
                    ( { stateBefore | decodeMessageFromMonacoEditorError = Just decodeError }, Cmd.none )

                Ok decodedMonacoEditorEvent ->
                    case decodedMonacoEditorEvent of
                        FrontendWeb.MonacoEditor.DidChangeContentEvent content ->
                            stateBefore |> updateWorkspaceWithoutCmdToUpdateEditor updateConfig (UserInputChangeTextInEditor content)

                        FrontendWeb.MonacoEditor.CompletedSetupEvent ->
                            ( { stateBefore | lastTextReceivedFromEditor = Nothing }, Cmd.none )

                        FrontendWeb.MonacoEditor.EditorActionCloseEditorEvent ->
                            updateWorkspaceWithoutCmdToUpdateEditor updateConfig UserInputCloseEditor stateBefore

                        FrontendWeb.MonacoEditor.EditorActionFormatDocumentEvent ->
                            updateWorkspaceWithoutCmdToUpdateEditor updateConfig UserInputFormat stateBefore

                        FrontendWeb.MonacoEditor.EditorActionCompileEvent ->
                            updateWorkspaceWithoutCmdToUpdateEditor updateConfig UserInputCompile stateBefore

        UserInputFormat ->
            ( stateBefore, elmFormatCmd stateBefore |> Maybe.withDefault Cmd.none )

        UserInputCompile ->
            userInputCompileFileOpenedInEditor updateConfig { stateBefore | viewEnlargedPane = Nothing }

        UserInputCloseEditor ->
            ( let
                editing =
                    stateBefore.editing
              in
              { stateBefore | editing = { editing | filePathOpenedInEditor = Nothing } }
            , Cmd.none
            )

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

        UserInputSetInspectionOnCompile enableInspection ->
            let
                state =
                    { stateBefore | enableInspectionOnCompile = enableInspection }
            in
            if state == stateBefore then
                ( stateBefore, Cmd.none )

            else
                updateWorkspaceWithoutCmdToUpdateEditor updateConfig UserInputCompile state


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
        resultWithFileTreeAndCache =
            result
                |> Result.map
                    (\loadOk ->
                        { fileTree = fileTreeNodeFromListFileWithPath loadOk.filesAsFlatList
                        , compositionIdCache = loadOk.compositionId
                        , urlInCommit = loadOk.urlInCommit
                        }
                    )

        stateBefore =
            { stateBeforeRememberingResult
                | lastBackendLoadFromGitResult = Just ( urlIntoGitRepository, resultWithFileTreeAndCache )
            }
    in
    case stateBefore.popup of
        Just (ModalDialog (LoadFromGitDialog dialogStateBefore)) ->
            let
                dialogState =
                    { dialogStateBefore | loadCompositionResult = Just resultWithFileTreeAndCache }
            in
            ( { stateBefore | popup = Just (ModalDialog (LoadFromGitDialog dialogState)) }, Cmd.none )

        _ ->
            case stateBefore.workspace of
                WorkspaceLoadingFromLink projectStateLoadingFromLink ->
                    if urlIntoGitRepository == projectStateLoadingFromLink.projectStateDescription.base then
                        case resultWithFileTreeAndCache of
                            Ok loadOk ->
                                updateForLoadedProjectState
                                    { expectedCompositionHash = projectStateLoadingFromLink.expectedCompositionHash
                                    , filePathToOpen = projectStateLoadingFromLink.filePathToOpen
                                    }
                                    loadOk.fileTree
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

        Just entryPointFilePath ->
            let
                base64FromBytes : Bytes.Bytes -> String
                base64FromBytes =
                    Base64.fromBytes
                        >> Maybe.withDefault "Error encoding in base64"

                allFilePaths =
                    workspace.fileTree
                        |> ProjectState.flatListOfBlobsFromFileTreeNode
                        |> List.map Tuple.first

                directoryContainsElmJson directoryPath =
                    allFilePaths |> List.member (directoryPath ++ [ "elm.json" ])

                workingDirectoryPath =
                    entryPointFilePath
                        |> List.reverse
                        |> List.drop 1
                        |> List.reverse
                        |> List.Extra.inits
                        |> List.sortBy (List.length >> negate)
                        |> List.filter directoryContainsElmJson
                        |> List.head
                        |> Maybe.withDefault []
            in
            Just
                { files =
                    workspace.fileTree
                        |> ProjectState.flatListOfBlobsFromFileTreeNode
                        |> List.map
                            (\( path, content ) ->
                                { path = path
                                , contentBase64 = content |> base64FromBytes
                                }
                            )
                , workingDirectoryPath = workingDirectoryPath
                , entryPointFilePathFromWorkingDirectory = entryPointFilePath |> List.drop (List.length workingDirectoryPath)
                , makeOptionDebug = workspace.enableInspectionOnCompile
                }


requestToApiCmd :
    FrontendBackendInterface.RequestStructure
    -> (FrontendBackendInterface.ResponseStructure -> Json.Decode.Decoder event)
    -> (Result Http.Error event -> mappedEvent)
    -> Cmd mappedEvent
requestToApiCmd request jsonDecoderSpecialization eventConstructor =
    let
        jsonDecoder =
            CompilationInterface.GenerateJsonCoders.jsonDecodeResponseStructure
                |> Json.Decode.andThen jsonDecoderSpecialization
    in
    Http.post
        { url = Url.Builder.absolute [ "api" ] []
        , body =
            Http.jsonBody
                (request |> CompilationInterface.GenerateJsonCoders.jsonEncodeRequestStructure)
        , expect = Http.expectJson (\response -> eventConstructor response) jsonDecoder
        }


view : State -> Browser.Document Event
view state =
    let
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

        titlebarEntries =
            [ ProjectMenuEntry ]
                |> List.map (titlebarMenuEntryButton state)

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
                        workspacePaneLayout paneProperties =
                            let
                                widthFillPortion =
                                    if workingState.viewEnlargedPane == Just paneProperties.pane then
                                        40

                                    else
                                        4
                            in
                            [ [ paneProperties.headerElement
                                    |> Element.el
                                        [ Element.width Element.fill
                                        , Element.height Element.fill

                                        -- https://github.com/mdgriffith/elm-ui/issues/149#issuecomment-531480958
                                        , Element.clip
                                        , Element.htmlAttribute (HA.style "flex-shrink" "1")
                                        ]
                              , toggleEnlargedPaneButton workingState paneProperties.pane
                                    |> Element.el [ Element.alignTop, Element.alignRight ]
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
                                    { editorPaneHeader =
                                        [ Element.text "Choose a file to open in the editor" ]
                                            |> Element.paragraph []
                                    , editorPaneContent =
                                        let
                                            selectEventFromFileTreeNode upperPath ( nodeName, nodeContent ) =
                                                case nodeContent of
                                                    ProjectState.BlobNode _ ->
                                                        Just (UserInputOpenFileInEditor (upperPath ++ [ nodeName ]))

                                                    ProjectState.TreeNode _ ->
                                                        Nothing
                                        in
                                        viewFileTree
                                            { selectEventFromNode = selectEventFromFileTreeNode
                                            , iconFromFileName = iconFromFileName
                                            }
                                            (sortFileTreeForExplorerView workingState.fileTree)
                                            |> Element.el
                                                [ Element.scrollbars
                                                , Element.width Element.fill
                                                , Element.height Element.fill
                                                , Element.padding defaultFontSize
                                                ]
                                    }

                                Just filePathOpenedInEditor ->
                                    let
                                        headerIconElementFromTypeAndColor maybeTypeAndColor =
                                            maybeTypeAndColor
                                                |> Maybe.map
                                                    (\( iconType, iconColor ) ->
                                                        Visuals.iconSvgElementFromIcon { color = iconColor } iconType
                                                    )
                                                |> Maybe.withDefault Element.none
                                                |> Element.el [ Element.width (Element.px (defaultFontSize * 8 // 10)) ]

                                        filePathElement =
                                            case List.reverse filePathOpenedInEditor of
                                                [] ->
                                                    Element.none

                                                fileName :: directoryPathReversed ->
                                                    let
                                                        directorySeparatorIconElement =
                                                            headerIconElementFromTypeAndColor (Just ( Visuals.DirectoryCollapsedIcon, "white" ))

                                                        fileIconElement =
                                                            filePathOpenedInEditor
                                                                |> List.reverse
                                                                |> List.head
                                                                |> Maybe.andThen iconFromFileName
                                                                |> headerIconElementFromTypeAndColor
                                                    in
                                                    (directoryPathReversed |> List.reverse |> List.map Element.text)
                                                        ++ [ [ fileIconElement, fileName |> Element.text ]
                                                                |> Element.row [ Element.spacing (defaultFontSize // 2) ]
                                                           ]
                                                        |> List.intersperse directorySeparatorIconElement
                                                        |> Element.row
                                                            [ Element.spacing (defaultFontSize // 2)
                                                            , elementFontSizePercent 80
                                                            ]

                                        closeEditorElement =
                                            Element.Input.button
                                                [ Element.mouseOver [ Element.Background.color (Element.rgba 0 0.5 0.8 0.5) ]
                                                , Element.padding 4
                                                ]
                                                { label = headerIconElementFromTypeAndColor (Just ( Visuals.CloseEditorIcon, "white" ))
                                                , onPress = Just UserInputCloseEditor
                                                }

                                        headerElement =
                                            [ [ filePathElement, closeEditorElement ]
                                                |> Element.row
                                                    [ Element.width Element.fill
                                                    , Element.height Element.fill
                                                    , Element.spacing defaultFontSize
                                                    , Element.alignLeft
                                                    , Element.alpha 0.8
                                                    ]
                                            , [ buttonElement { label = "📄 Format", onPress = Just UserInputFormat }
                                              , buttonElement { label = "▶️ Compile", onPress = Just UserInputCompile }
                                              ]
                                                |> Element.row
                                                    [ Element.spacing defaultFontSize
                                                    , Element.width Element.fill
                                                    ]
                                            ]
                                                |> Element.wrappedRow
                                                    [ Element.spacing defaultFontSize
                                                    , Element.paddingXY defaultFontSize 0
                                                    , Element.width Element.fill
                                                    ]
                                    in
                                    { editorPaneHeader = headerElement
                                    , editorPaneContent = monacoEditorElement state
                                    }

                        outputPaneElements =
                            viewOutputPaneContent workingState
                    in
                    [ [ workspacePaneLayout
                            { pane = EditorPane
                            , headerElement = workspaceView.editorPaneHeader
                            , mainContent = workspaceView.editorPaneContent
                            }
                      , workspacePaneLayout
                            { pane = OutputPane
                            , headerElement = outputPaneElements.header
                            , mainContent =
                                outputPaneElements.mainContent
                                    |> Element.el
                                        [ Element.width Element.fill
                                        , Element.height Element.fill

                                        -- https://github.com/mdgriffith/elm-ui/issues/149#issuecomment-531480958
                                        , Element.clip
                                        , Element.htmlAttribute (HA.style "flex-shrink" "1")
                                        ]
                            }
                      ]
                        |> Element.row [ Element.width Element.fill, Element.height Element.fill ]
                        |> Element.map WorkspaceEvent
                    ]
                        |> Element.column [ Element.width Element.fill, Element.height Element.fill ]

                WorkspaceErr projectStateError ->
                    [ [ ("Failed to load project state: " ++ (projectStateError |> String.left 500))
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

        logoElement =
            [ Visuals.elmEditorIconSvg "1.2em" |> Element.html |> Element.el []
            , "Elm Editor" |> Element.text |> Element.el [ Element.Font.semiBold ]
            ]
                |> Element.row
                    [ Element.spacing defaultFontSize
                    , Element.htmlAttribute (HA.style "cursor" "default")
                    , Element.htmlAttribute (HA.style "user-select" "none")
                    ]

        titlebar =
            [ logoElement |> Element.el [ Element.paddingXY defaultFontSize 0 ]
            , titlebarEntries
                |> Element.row
                    [ Element.htmlAttribute (HA.id titleBarMenubarElementId)
                    , Element.spacing defaultFontSize
                    ]
            ]
                |> Element.row
                    [ Element.spacing defaultFontSize
                    , Element.width Element.fill
                    , Element.Background.color (Element.rgb 0.24 0.24 0.24)
                    ]

        popupWindowAttributes =
            case state.popup of
                Nothing ->
                    []

                Just (TitlebarMenu _ _) ->
                    []

                Just (ModalDialog (GetLinkToProjectDialog dialog)) ->
                    case state.workspace of
                        WorkspaceOk workingState ->
                            viewGetLinkToProjectDialog dialog workingState.fileTree
                                |> popupWindowElementAttributesFromAttributes

                        _ ->
                            []

                Just (ModalDialog (LoadFromGitDialog dialogState)) ->
                    viewLoadFromGitDialog dialogState
                        |> popupWindowElementAttributesFromAttributes

                Just (ModalDialog ExportToZipArchiveDialog) ->
                    case state.workspace of
                        WorkspaceOk workingState ->
                            viewExportToZipArchiveDialog workingState.fileTree
                                |> popupWindowElementAttributesFromAttributes

                        _ ->
                            []

                Just (ModalDialog (ImportFromZipArchiveDialog dialogState)) ->
                    viewImportFromZipArchiveDialog dialogState
                        |> popupWindowElementAttributesFromAttributes

        body =
            [ Element.html FontAwesome.Styles.css
            , titlebar
            , [ activityBar, mainContent ]
                |> Element.row [ Element.width Element.fill, Element.height Element.fill ]
            ]
                |> Element.column [ Element.width Element.fill, Element.height Element.fill ]
                |> Element.layout
                    ([ Element.Font.family (rootFontFamily |> List.map Element.Font.typeface)
                     , Element.Font.size defaultFontSize
                     , Element.Font.color (Element.rgb 0.95 0.95 0.95)
                     , Element.Background.color backgroundColor
                     , Element.width Element.fill
                     ]
                        ++ popupWindowAttributes
                    )
    in
    { title = "Elm Editor", body = [ body ] }


titleBarMenubarElementId : String
titleBarMenubarElementId =
    "titlebar-menubar"


type alias FileTreeNodeViewModel event =
    { indentLevel : Int
    , label : String
    , selectEvent : Maybe event
    , icon : Maybe ( Visuals.Icon, String )
    }


sortFileTreeForExplorerView : ProjectState.FileTreeNode -> ProjectState.FileTreeNode
sortFileTreeForExplorerView node =
    case node of
        ProjectState.BlobNode _ ->
            node

        ProjectState.TreeNode tree ->
            tree
                |> List.map (Tuple.mapSecond sortFileTreeForExplorerView)
                |> List.sortBy
                    (\( _, child ) ->
                        if ProjectState.isBlobNode child then
                            0

                        else
                            1
                    )
                |> ProjectState.TreeNode


viewFileTree :
    { selectEventFromNode : List String -> ( String, ProjectState.FileTreeNode ) -> Maybe event
    , iconFromFileName : String -> Maybe ( Visuals.Icon, String )
    }
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
    { selectEventFromNode : List String -> ( String, ProjectState.FileTreeNode ) -> Maybe event
    , iconFromFileName : String -> Maybe ( Visuals.Icon, String )
    }
    -> List String
    -> ( String, ProjectState.FileTreeNode )
    -> List (FileTreeNodeViewModel event)
buildFileTreeViewList configuration path ( currentNodeName, currentNodeContent ) =
    let
        icon =
            case currentNodeContent of
                ProjectState.TreeNode _ ->
                    Just ( Visuals.DirectoryExpandedIcon, "white" )

                ProjectState.BlobNode _ ->
                    configuration.iconFromFileName currentNodeName

        currentItem =
            { indentLevel = List.length path
            , label = currentNodeName
            , selectEvent = configuration.selectEventFromNode path ( currentNodeName, currentNodeContent )
            , icon = icon
            }
    in
    case currentNodeContent of
        ProjectState.BlobNode _ ->
            [ currentItem ]

        ProjectState.TreeNode tree ->
            currentItem
                :: List.concatMap (buildFileTreeViewList configuration (path ++ [ currentNodeName ])) tree


viewFileTreeList : List (FileTreeNodeViewModel event) -> Element.Element event
viewFileTreeList =
    List.map
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

                iconElement =
                    case item.icon of
                        Nothing ->
                            Element.none

                        Just ( iconType, iconColor ) ->
                            Visuals.iconSvgElementFromIcon { color = iconColor } iconType

                currentNodeElement =
                    [ iconElement |> Element.el [ Element.width (Element.px defaultFontSize), Element.alpha 0.8 ]
                    , Element.text item.label
                    ]
                        |> Element.row [ Element.spacing (defaultFontSize // 2), Element.padding 5 ]
                        |> Element.el
                            (Element.paddingEach { left = item.indentLevel * defaultFontSize, right = 0, top = 0, bottom = 0 }
                                :: Element.width Element.fill
                                :: interactionAttributes
                            )
            in
            currentNodeElement
        )
        >> Element.column [ Element.width Element.fill ]


iconFromFileName : String -> Maybe ( Visuals.Icon, String )
iconFromFileName fileName =
    if String.endsWith ".elm" fileName then
        Just ( Visuals.FileTypeElmIcon, "rgb(127, 201, 255)" )

    else
        Nothing


toggleEnlargedPaneButton : WorkingProjectStateStructure -> WorkspacePane -> Element.Element WorkspaceEventStructure
toggleEnlargedPaneButton state pane =
    let
        ( icon, onPress ) =
            if state.viewEnlargedPane == Just pane then
                ( Visuals.ShrinkActionIcon, Nothing )

            else if state.viewEnlargedPane == Nothing then
                ( Visuals.GrowActionIcon, Just pane )

            else
                ( Visuals.GrowActionIcon, Nothing )

        iconSize =
            24
    in
    Element.Input.button
        [ Element.Background.color (Element.rgb 0.2 0.2 0.2)
        , Element.mouseOver [ Element.Background.color buttonMouseOverColor ]
        , Element.padding 4
        ]
        { label =
            Visuals.iconSvgElementFromIcon { color = "rgba(255,255,255,0.7)" } icon
                |> Element.el [ Element.width (Element.px iconSize), Element.height (Element.px iconSize) ]
        , onPress = Just (UserInputSetEnlargedPane onPress)
        }
        |> Element.el [ Element.alignRight ]


type alias PopupWindowAttributes event =
    { title : String
    , titleIcon : Maybe FontAwesome.Icon.Icon
    , guideParagraphItems : List (Element.Element event)
    , contentElement : Element.Element event
    }


viewGetLinkToProjectDialog : GetLinkToProjectDialogState -> ProjectState.FileTreeNode -> PopupWindowAttributes Event
viewGetLinkToProjectDialog dialogState projectState =
    let
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
                    Element.paragraph [] [ Element.text ("Length of this link: " ++ String.fromInt (String.length urlToProject)) ]
                        :: dependenciesDescriptionLines
            in
            [ linkElementWithWrappedLabel
                { url = urlToProject, labelText = urlToProject }
                |> Element.el
                    [ Element.height (Element.px 100)
                    , Element.scrollbarY
                    , Element.Font.size ((defaultFontSize * 4) // 5)
                    ]
            , linkDescriptionLines
                |> Element.textColumn [ Element.Font.size ((defaultFontSize * 4) // 5), Element.padding defaultFontSize ]
            ]
                |> Element.column
                    [ Element.spacing defaultFontSize
                    , Element.width Element.fill
                    ]

        buttonGenerateLinkOrResultElement =
            case dialogState.urlToProject of
                Nothing ->
                    buttonElement
                        { label = "Generate link to project"
                        , onPress = Just (UserInputGetLinkToProject (Just { createDiffIfBaseAvailable = True }))
                        }
                        |> Element.el [ Element.centerX ]

                Just urlToProject ->
                    linkElementFromUrl urlToProject
    in
    { title = "Get Link to Project for Bookmarking or Sharing"
    , titleIcon = Just FontAwesome.Solid.bookmark
    , guideParagraphItems =
        [ Element.text "Get a link that you can later use to load the project's current state into the editor again. This link is a fast way to share your project state with other people." ]
    , contentElement =
        [ projectSummaryElementForDialog projectState
        , buttonGenerateLinkOrResultElement |> Element.el [ Element.width Element.fill ]
        ]
            |> Element.column
                [ Element.spacing defaultFontSize
                , Element.width Element.fill
                ]
    }


linkElementWithWrappedLabel : { url : String, labelText : String } -> Element.Element e
linkElementWithWrappedLabel { url, labelText } =
    [ Element.html
        (Html.node "style"
            []
            [ Html.text """
a:link {
  color: inherit;
  text-decoration: none;
}

a:visited {
  color: inherit;
}

a:hover {
  color: inherit;
  text-decoration: underline;
}"""
            ]
        )
    , Element.html
        (Html.a
            [ HA.href url
            , HA.style "overflow-wrap" "anywhere"
            , HA.style "white-space" "pre-wrap"
            ]
            [ Html.text labelText ]
        )
    ]
        |> Element.column []


viewLoadFromGitDialog : LoadFromGitDialogState -> PopupWindowAttributes Event
viewLoadFromGitDialog dialogState =
    let
        userInputBeginLoadingEvent =
            UserInputLoadFromGit
                (LoadFromGitBeginRequestEvent { urlIntoGitRepository = dialogState.urlIntoGitRepository })

        urlInputElement =
            Element.Input.text
                [ Element.Background.color backgroundColor
                , onKeyDownEnter userInputBeginLoadingEvent
                ]
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
                , onPress = Just userInputBeginLoadingEvent
                }
                |> Element.el [ Element.centerX, elementTransparent (not offerBeginLoading) ]

        resultElement =
            dialogState.loadCompositionResult
                |> Maybe.map
                    (viewLoadOrImportDialogResultElement
                        elementToDisplayLoadFromGitError
                        UserInputLoadOrImportTakeProjectStateEvent
                    )
                |> Maybe.withDefault Element.none

        exampleUrl =
            "https://github.com/onlinegamemaker/making-online-games/tree/fd35d23d89a50014097e64d362f1a991a8af206f/games-program-codes/simple-snake"
    in
    { title = "Load Project from Git Repository"
    , titleIcon = Just FontAwesome.Solid.cloudDownloadAlt
    , guideParagraphItems =
        [ Element.text "Load project files from a URL to a tree in a git repository. Here is an example of such a URL: "
        , linkElementFromUrlAndTextLabel { url = exampleUrl, labelText = exampleUrl }
        ]
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


viewExportToZipArchiveDialog : ProjectState.FileTreeNode -> PopupWindowAttributes Event
viewExportToZipArchiveDialog projectState =
    let
        buttonDownloadArchive =
            buttonElement
                { label = "Download Archive"
                , onPress = Just (UserInputExportProjectToZipArchive { sendDownloadCmd = True })
                }
    in
    { title = "Export Project to Zip Archive"
    , titleIcon = Just FontAwesome.Solid.fileExport
    , guideParagraphItems =
        [ Element.text "Download a zip archive containing all files in your project. You can also use this archive with the 'Import from Zip Archive' function to load the project's state into the editor again." ]
    , contentElement =
        [ projectSummaryElementForDialog projectState
        , buttonDownloadArchive |> Element.el [ Element.centerX ]
        ]
            |> Element.column
                [ Element.spacing defaultFontSize
                , Element.width Element.fill
                ]
    }


viewImportFromZipArchiveDialog : ImportFromZipArchiveDialogState -> PopupWindowAttributes Event
viewImportFromZipArchiveDialog dialogState =
    let
        selectFileButton =
            buttonElement
                { label = "Select zip archive file"
                , onPress = Just (UserInputImportProjectFromZipArchive ImportFromZipArchiveSelectFile)
                }
                |> Element.el [ Element.centerX ]

        resultElement =
            dialogState.loadCompositionResult
                |> Maybe.map
                    (viewLoadOrImportDialogResultElement
                        (Element.text >> List.singleton >> Element.paragraph [ Element.Font.color (Element.rgb 1 0.64 0) ])
                        UserInputLoadOrImportTakeProjectStateEvent
                    )
                |> Maybe.withDefault Element.none
    in
    { title = "Import Project from Zip Archive"
    , titleIcon = Just FontAwesome.Solid.fileImport
    , guideParagraphItems =
        [ Element.text "Load project files from a zip archive. Here you can select a zip archive file from your system to load as the project state." ]
    , contentElement =
        [ selectFileButton
        , resultElement
        ]
            |> Element.column
                [ Element.width Element.fill
                , Element.spacing defaultFontSize
                ]
    }


elementToDisplayLoadFromGitError : Http.Error -> Element.Element msg
elementToDisplayLoadFromGitError loadError =
    [ describeErrorLoadingContentsFromGit loadError
        |> String.left 500
        |> Element.text
    ]
        |> Element.paragraph [ Element.Font.color (Element.rgb 1 0.64 0) ]


projectSummaryElementForDialog : ProjectState.FileTreeNode -> Element.Element e
projectSummaryElementForDialog projectState =
    let
        projectFiles =
            projectState |> ProjectState.flatListOfBlobsFromFileTreeNode
    in
    [ ("This project contains "
        ++ (projectFiles |> List.length |> String.fromInt)
        ++ " files with an aggregate size of "
        ++ (projectFiles |> List.map (Tuple.second >> Bytes.width) |> List.sum |> String.fromInt)
        ++ " bytes."
      )
        |> Element.text
    ]
        |> Element.paragraph []


viewLoadOrImportDialogResultElement :
    (err -> Element.Element e)
    -> (ProjectState.FileTreeNode -> e)
    -> Result err { r | fileTree : ProjectState.FileTreeNode, compositionIdCache : String }
    -> Element.Element e
viewLoadOrImportDialogResultElement elementToDisplayFromError commitEvent loadCompositionResult =
    case loadCompositionResult of
        Err loadError ->
            elementToDisplayFromError loadError

        Ok loadOk ->
            [ [ ("Loaded composition "
                    ++ loadOk.compositionIdCache
                    ++ " containing "
                    ++ (loadOk.fileTree |> ProjectState.flatListOfBlobsFromFileTreeNode |> List.length |> String.fromInt)
                    ++ " files:"
                )
                    |> Element.text
              ]
                |> Element.paragraph []
            , viewFileTree
                { selectEventFromNode = always (always Nothing)
                , iconFromFileName = iconFromFileName
                }
                (sortFileTreeForExplorerView loadOk.fileTree)
                |> Element.el
                    [ Element.scrollbars
                    , Element.width Element.fill
                    , Element.height (Element.px 200)
                    , Element.padding defaultFontSize
                    , Element.Border.width 1
                    , Element.Border.color (Element.rgba 1 1 1 0.5)
                    ]
            , buttonElement
                { label = "Set these files as project state"
                , onPress = Just (commitEvent loadOk.fileTree)
                }
                |> Element.el [ Element.centerX ]
            ]
                |> Element.column [ Element.spacing (defaultFontSize // 2) ]


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


popupWindowElementAttributesFromAttributes : PopupWindowAttributes Event -> List (Element.Attribute Event)
popupWindowElementAttributesFromAttributes { title, titleIcon, guideParagraphItems, contentElement } =
    [ [ [ titleIcon
            |> Maybe.map (FontAwesome.Icon.present >> FontAwesome.Icon.view >> Element.html >> Element.el [])
            |> Maybe.withDefault Element.none
        , title |> Element.text
        ]
            |> Element.row (Element.spacing defaultFontSize :: headingAttributes 3)
      , guideParagraphItems |> Element.paragraph [ elementFontSizePercent 80 ]
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
            , Element.Events.onClick UserInputClosePopup
            , Element.htmlAttribute (HA.style "backdrop-filter" "blur(1px)")
            ]
        |> Element.inFront
    ]


viewOutputPaneContent :
    WorkingProjectStateStructure
    -> { mainContent : Element.Element WorkspaceEventStructure, header : Element.Element WorkspaceEventStructure }
viewOutputPaneContent state =
    case state.elmMakeResult of
        Nothing ->
            { mainContent =
                case state.pendingElmMakeRequest of
                    Nothing ->
                        if filePathOpenedInEditorFromWorkspace state == Nothing then
                            Element.none

                        else
                            [ "No compilation started. You can use the 'Compile' button to check program text for errors and see your app in action."
                                |> Element.text
                            ]
                                |> Element.paragraph [ Element.padding defaultFontSize ]

                    Just pendingElmMakeRequest ->
                        let
                            elmMakeRequestEntryPointFilePathAbs =
                                pendingElmMakeRequest.request.workingDirectoryPath
                                    ++ pendingElmMakeRequest.request.entryPointFilePathFromWorkingDirectory
                        in
                        [ Element.text
                            ("Compiling module '"
                                ++ String.join "/" elmMakeRequestEntryPointFilePathAbs
                                ++ "' with inspection "
                                ++ (if pendingElmMakeRequest.request.makeOptionDebug then
                                        "enabled"

                                    else
                                        "disabled"
                                   )
                                ++ " ..."
                            )
                        ]
                            |> Element.paragraph [ Element.padding defaultFontSize ]
            , header = Element.none
            }

        Just ( elmMakeRequest, elmMakeResult ) ->
            case elmMakeResult of
                Err elmMakeError ->
                    { mainContent = ("Error: " ++ describeHttpError elmMakeError) |> Element.text, header = Element.none }

                Ok elmMakeOk ->
                    let
                        elmMakeRequestFromCurrentState =
                            elmMakeRequestForFileOpenedInEditor state

                        currentFileContentIsStillSame =
                            Just elmMakeRequest.files
                                == (elmMakeRequestFromCurrentState |> Maybe.map .files)

                        elmMakeRequestEntryPointFilePathAbs =
                            elmMakeRequest.workingDirectoryPath
                                ++ elmMakeRequest.entryPointFilePathFromWorkingDirectory

                        warnAboutOutdatedCompilationText =
                            if
                                Just elmMakeRequestEntryPointFilePathAbs
                                    /= filePathOpenedInEditorFromWorkspace state
                            then
                                Just
                                    ("⚠️ Last compilation started for another file: '"
                                        ++ String.join "/" elmMakeRequestEntryPointFilePathAbs
                                        ++ "'"
                                    )

                            else if currentFileContentIsStillSame then
                                Nothing

                            else
                                Just "⚠️ File contents changed since compiling"

                        ( toggleInspectionLabel, toggleInspectionEvent ) =
                            if state.enableInspectionOnCompile then
                                ( "Disable Inspection", UserInputSetInspectionOnCompile False )

                            else
                                ( "🔍 Enable Inspection", UserInputSetInspectionOnCompile True )

                        warnAboutOutdatedOrOfferModifyCompilationElement =
                            case warnAboutOutdatedCompilationText of
                                Just warnText ->
                                    warnText
                                        |> Element.text
                                        |> List.singleton
                                        |> Element.paragraph
                                            [ Element.padding (defaultFontSize // 2)
                                            , Element.Background.color (Element.rgb 0.3 0.2 0.1)
                                            , Element.width Element.fill
                                            , Element.transparent (warnAboutOutdatedCompilationText == Nothing)
                                            ]

                                Nothing ->
                                    buttonElement { label = toggleInspectionLabel, onPress = Just toggleInspectionEvent }

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
                                    [ if
                                        (elmMakeOk.response.reportJsonProcessOutput.exitCode == 0)
                                            && (elmMakeOk.response.reportJsonProcessOutput.standardError |> String.trim |> String.isEmpty)
                                      then
                                        ( "✔ No Errors"
                                        , [ ("Found no errors in module '"
                                                ++ String.join "/" elmMakeRequestEntryPointFilePathAbs
                                                ++ "'"
                                            )
                                                |> Element.text
                                          ]
                                            |> Element.paragraph []
                                        )

                                      else
                                        ( "Errors"
                                        , case elmMakeOk.reportFromJson of
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
                                        )
                                    , ( "Elm make standard output", outputElementFromPlainText elmMakeOk.response.processOutput.standardOutput )
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
                    { mainContent = compileResultElement
                    , header = warnAboutOutdatedOrOfferModifyCompilationElement
                    }


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
        , Element.mouseOver [ Element.Background.color buttonMouseOverColor ]
        , Element.paddingXY defaultFontSize (defaultFontSize // 2)
        , Element.Border.widthEach { left = 0, right = 0, top = 0, bottom = 2 }
        , Element.Border.color buttonMouseOverColor
        ]
        { label = Element.text buttonConfig.label
        , onPress = buttonConfig.onPress
        }


buttonMouseOverColor : Element.Color
buttonMouseOverColor =
    Element.rgb 0 0.278 0.443


titlebarMenuEntryButton : State -> TitlebarMenuEntry -> Element.Element Event
titlebarMenuEntryButton state menuEntry =
    let
        isOpen =
            state.popup == Just (TitlebarMenu menuEntry True)

        isHighlighted =
            state.popup == Just (TitlebarMenu menuEntry False) || isOpen

        buttonBackgroundColor =
            if isHighlighted then
                buttonMouseOverColor

            else
                Element.rgb 0.2 0.2 0.2

        dropdownAttributes =
            if not isOpen then
                []

            else
                [ titlebarMenuEntryDropdownContent state menuEntry
                    |> Element.below
                ]
    in
    Element.Input.button
        [ Element.Background.color buttonBackgroundColor
        , Element.paddingXY defaultFontSize (defaultFontSize // 2)
        , Element.Border.widthEach { left = 0, right = 0, top = 0, bottom = 2 }
        , Element.Border.color buttonMouseOverColor
        , Element.Events.onMouseEnter (UserInputMouseOverTitleBarMenu (Just menuEntry))
        , Element.Events.onMouseLeave (UserInputMouseOverTitleBarMenu Nothing)
        ]
        { label = Element.text (titlebarMenuEntryLabel menuEntry)
        , onPress = Just (UserInputToggleTitleBarMenu menuEntry)
        }
        |> Element.el dropdownAttributes


titlebarMenuEntryDropdownContent : State -> TitlebarMenuEntry -> Element.Element Event
titlebarMenuEntryDropdownContent state menuEntry =
    let
        canSaveProject =
            case state.workspace of
                WorkspaceOk _ ->
                    True

                _ ->
                    False

        menuEntries =
            case menuEntry of
                ProjectMenuEntry ->
                    [ titlebarMenuEntry
                        (UserInputLoadFromGit LoadFromGitOpenDialog)
                        (Just FontAwesome.Solid.cloudDownloadAlt)
                        "Load From Git Repository"
                        True
                    , titlebarMenuEntry
                        (UserInputImportProjectFromZipArchive ImportFromZipArchiveOpenDialog)
                        (Just FontAwesome.Solid.fileImport)
                        "Import From Zip Archive"
                        True
                    , titlebarMenuEntry
                        (UserInputGetLinkToProject Nothing)
                        (Just FontAwesome.Solid.bookmark)
                        "Get Link for Bookmarking or Sharing"
                        canSaveProject
                    , titlebarMenuEntry
                        (UserInputExportProjectToZipArchive { sendDownloadCmd = False })
                        (Just FontAwesome.Solid.fileExport)
                        "Export To Zip Archive"
                        canSaveProject
                    ]
    in
    menuEntries
        |> Element.column
            [ Element.spacing 2
            , Element.paddingXY 2 8
            , Element.width Element.shrink
            ]
        |> Element.el
            [ Element.Background.color (Element.rgb 0.145 0.145 0.145)
            , Element.Border.glow (Element.rgb 0 0 0) 2
            ]


titlebarMenuEntry : Event -> Maybe FontAwesome.Icon.Icon -> String -> Bool -> Element.Element Event
titlebarMenuEntry onPressEventIfEnabled maybeIcon label isEnabled =
    let
        mouseOverAttributes =
            if isEnabled then
                [ Element.mouseOver [ Element.Background.color buttonMouseOverColor ] ]

            else
                []
    in
    Element.Input.button
        [ Element.width Element.fill
        , Element.paddingXY defaultFontSize 4
        ]
        { label =
            [ maybeIcon
                |> Maybe.map
                    (FontAwesome.Icon.present
                        >> FontAwesome.Icon.view
                        >> Element.html
                        >> Element.el [ Element.centerX ]
                    )
                |> Maybe.withDefault Element.none
                |> Element.el [ Element.width (Element.px (defaultFontSize * 6 // 5)) ]
            , Element.text label
                |> Element.el
                    [ Element.alpha
                        (if isEnabled then
                            1

                         else
                            0.5
                        )
                    ]
            ]
                |> Element.row [ Element.spacing (defaultFontSize // 2) ]
        , onPress =
            if isEnabled then
                Just onPressEventIfEnabled

            else
                Nothing
        }
        |> Element.el
            ([ Element.width Element.fill ] ++ mouseOverAttributes)


titlebarMenuEntryLabel : TitlebarMenuEntry -> String
titlebarMenuEntryLabel menuEntry =
    case menuEntry of
        ProjectMenuEntry ->
            "Project"


setTextInMonacoEditorCmd : String -> Cmd WorkspaceEventStructure
setTextInMonacoEditorCmd =
    FrontendWeb.MonacoEditor.SetValue
        >> CompilationInterface.GenerateJsonCoders.jsonEncodeMessageToMonacoEditor
        >> sendMessageToMonacoFrame


revealPositionInCenterInMonacoEditorCmd : { lineNumber : Int, column : Int } -> Cmd WorkspaceEventStructure
revealPositionInCenterInMonacoEditorCmd =
    FrontendWeb.MonacoEditor.RevealPositionInCenter
        >> CompilationInterface.GenerateJsonCoders.jsonEncodeMessageToMonacoEditor
        >> sendMessageToMonacoFrame


setModelMarkersInMonacoEditorCmd : List FrontendWeb.MonacoEditor.EditorMarker -> Cmd WorkspaceEventStructure
setModelMarkersInMonacoEditorCmd =
    FrontendWeb.MonacoEditor.SetModelMarkers
        >> CompilationInterface.GenerateJsonCoders.jsonEncodeMessageToMonacoEditor
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
              , ProjectState.BlobNode CompilationInterface.SourceFiles.file____default_app_elm_json
              )
            , ( "src"
              , ProjectState.TreeNode
                    [ ( "Main.elm"
                      , ProjectState.BlobNode CompilationInterface.SourceFiles.file____default_app_src_Main_elm
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
    , enableInspectionOnCompile = False
    }


stringFromFileContent : Bytes.Bytes -> Maybe String
stringFromFileContent bytes =
    Bytes.Decode.decode (Bytes.Decode.string (Bytes.width bytes)) bytes


fileContentFromString : String -> Bytes.Bytes
fileContentFromString =
    Bytes.Encode.string >> Bytes.Encode.encode


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


isTargetOutsideParentWithId : String -> event -> Json.Decode.Decoder event
isTargetOutsideParentWithId parentId event =
    Json.Decode.field "target" (isOutsideParentWithId parentId)
        |> Json.Decode.andThen
            (\isOutside ->
                if isOutside then
                    Json.Decode.succeed event

                else
                    Json.Decode.fail ("is not outside " ++ parentId)
            )


isOutsideParentWithId : String -> Json.Decode.Decoder Bool
isOutsideParentWithId parentId =
    Json.Decode.oneOf
        [ Json.Decode.field "id" Json.Decode.string
            |> Json.Decode.andThen
                (\id ->
                    if parentId == id then
                        -- found match by id
                        Json.Decode.succeed False

                    else
                        -- try next decoder
                        Json.Decode.fail "continue"
                )
        , Json.Decode.lazy (\_ -> isOutsideParentWithId parentId |> Json.Decode.field "parentNode")

        -- fallback if all previous decoders failed
        , Json.Decode.succeed True
        ]


buildZipArchiveFromFileTree : ProjectState.FileTreeNode -> Zip.Zip
buildZipArchiveFromFileTree =
    ProjectState.flatListOfBlobsFromFileTreeNode
        >> List.map
            (\( path, blobContent ) ->
                blobContent
                    |> Zip.Entry.store
                        { path = String.join "/" path
                        , lastModified = ( Time.utc, Time.millisToPosix 0 )
                        , comment = Nothing
                        }
            )
        >> Zip.fromEntries


extractFileTreeFromZipArchive : Zip.Zip -> Result String ProjectState.FileTreeNode
extractFileTreeFromZipArchive =
    Zip.entries
        >> List.map
            (\entry ->
                case Zip.Entry.toBytes entry of
                    Err _ ->
                        Err ("Failed to extract entry '" ++ Zip.Entry.path entry ++ "'")

                    Ok entryBytes ->
                        Ok
                            ( entry |> Zip.Entry.path |> String.split "/" |> List.concatMap (String.split "\\")
                            , entryBytes
                            )
            )
        >> Result.Extra.combine
        >> Result.map ProjectState.sortedFileTreeFromListOfBlobs


onKeyDownEnter : msg -> Element.Attribute msg
onKeyDownEnter msg =
    Element.htmlAttribute
        (Html.Events.on "keydown"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed msg

                        else
                            Json.Decode.fail "Not the enter key"
                    )
            )
        )
