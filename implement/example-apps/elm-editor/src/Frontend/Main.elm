port module Frontend.Main exposing
    ( Event(..)
    , State
    , init
    , main
    , receiveMessageFromMonacoFrame
    , sendMessageToMonacoFrame
    , update
    , view
    )

import Base64
import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation as Navigation
import Bytes
import Bytes.Encode
import Common
import CompilationInterface.GenerateJsonConverters
import CompileElmApp
import Dict
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Element.Region
import Elm.Syntax.File
import Elm.Syntax.Range
import ElmMakeExecutableFile
import File
import File.Download
import File.Select
import FileTree
import FileTreeInWorkspace
import FontAwesome.Icon
import FontAwesome.Solid
import FontAwesome.Styles
import Frontend.BrowserApplicationInitWithTime as BrowserApplicationInitWithTime
import Frontend.MonacoEditor
import Frontend.ProjectStateInUrl
import Frontend.Visuals as Visuals
import FrontendBackendInterface
import Html
import Html.Attributes as HA
import Html.Events
import Http
import Http.Detailed
import Json.Decode
import Json.Encode
import Keyboard.Event
import Keyboard.Key
import LanguageService
import List.Extra
import Parser
import ProjectState_2021_01
import Result.Extra
import SHA256
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
    , lastBackendLoadFromGitResult : Maybe ( String, Result (Http.Detailed.Error String) (Result String BackendLoadFromGitOkWithCache) )
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
    { fileTree : FileTreeInWorkspace.FileTreeNode
    , editing : { filePathOpenedInEditor : Maybe (List String) }
    , decodeMessageFromMonacoEditorError : Maybe Json.Decode.Error
    , lastTextReceivedFromEditor : Maybe String
    , compilation : Maybe CompilationState
    , syntaxInspection : Maybe SyntaxInspectionState
    , elmFormat : Maybe ElmFormatState
    , viewEnlargedPane : Maybe WorkspacePane
    , enableInspectionOnCompile : Bool
    , languageServiceState : LanguageService.LanguageServiceState
    }


type ElmFormatState
    = ElmFormatInProgress ElmFormatStartStruct
    | ElmFormatResult ElmFormatStartStruct ElmFormatResultStruct


type alias ElmFormatResultStruct =
    Result (Http.Detailed.Error String) (Result FrontendBackendInterface.FormatElmModuleTextResponseStructure String)


type alias ElmFormatStartStruct =
    { startTime : Time.Posix
    , inputModuleText : String
    }


type alias CompilationState =
    { origin : CompilationOrigin
    , loweringLastIteration : LoweringIterationComplete
    , dependenciesFromCompletedIterations : List ( CompileElmApp.DependencyKey, Bytes.Bytes )
    }


type alias CompilationOrigin =
    { requestFromUser : ElmMakeRequestStructure
    , loweredElmAppFromDependencies :
        List ( CompileElmApp.DependencyKey, Bytes.Bytes )
        -> Result (List CompileElmApp.LocatedCompilationError) CompileElmApp.AppFiles
    }


type LoweringIterationComplete
    = LoweringContinue CompilationIterationRequestStructure
    | LoweringComplete LoweringCompleteStruct


type alias CompilationIterationRequestStructure =
    { elmMakeRequest : ElmMakeRequestStructure
    , forDependencyElmMakeRequest : CompileElmApp.ElmMakeRequestStructure
    }


type alias LoweringCompleteStruct =
    { loweringResult : Result LoweringError ()
    , elmMakeRequest : ElmMakeRequestStructure
    , elmMakeResult : Maybe (Result (Http.Detailed.Error String) ElmMakeResultStructure)
    }


type LoweringError
    = LoweringError (List CompileElmApp.LocatedCompilationError)
    | DependencyLoweringError (Http.Detailed.Error String)


type alias SyntaxInspectionState =
    { parseFileResult : Result String ( String, Result (List Parser.DeadEnd) Elm.Syntax.File.File )
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
    | UserInputLoadFromGitOpenDialog
    | UserInputLoadFromGit UserInputLoadFromGitEventStructure
    | UserInputLoadOrImportTakeProjectStateEvent LoadOrImportProjectStateOrigin
    | UserInputClosePopup
    | BackendLoadFromGitResultEvent String (Result (Http.Detailed.Error String) (Result String FrontendBackendInterface.LoadCompositionResponseStructure))
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | WorkspaceEvent WorkspaceEventStructure
    | UserInputGetLinkToProject { createDiffIfBaseAvailable : Bool }
    | UserInputLoadedZipArchiveFile File.File
    | UserInputLoadedZipArchiveBytes Bytes.Bytes
    | UserInputImportProjectFromZipArchive UserInputImportFromZipArchiveEventStructure
    | UserInputExportProjectToZipArchive { sendDownloadCmd : Bool }
    | UserInputToggleTitleBarMenu TitlebarMenuEntry
    | UserInputMouseOverTitleBarMenu (Maybe TitlebarMenuEntry)
    | UserInputKeyDownEvent Keyboard.Event.KeyboardEvent
    | UserInputFocusOutsideTitlebarMenu
    | DiscardEvent


type WorkspaceEventStructure
    = MonacoEditorEvent Json.Decode.Value
    | UserInputChangeTextInEditor String
    | UserInputOpenFileInEditor (List String)
    | UserInputFormat
    | UserInputCancelFormatting
    | UserInputCompile
    | UserInputInspectSyntax
    | UserInputCloseEditor
    | UserInputRevealPositionInEditor { filePath : List String, lineNumber : Int, column : Int }
    | BackendElmFormatResponseEvent { filePath : List String, result : Result (Http.Detailed.Error String) FrontendBackendInterface.FormatElmModuleTextResponseStructure }
    | BackendElmMakeResponseEvent ElmMakeRequestStructure (Result (Http.Detailed.Error String) ElmMakeResponseStructure)
    | UserInputSetEnlargedPane (Maybe WorkspacePane)
    | UserInputSetInspectionOnCompile Bool


type UserInputLoadFromGitEventStructure
    = LoadFromGitEnterUrlEvent { urlIntoGitRepository : String }
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
    { urlToProject : String }


type alias LoadFromGitDialogState =
    { urlIntoGitRepository : String
    , request : Maybe { url : String, time : Time.Posix }
    , loadCompositionResult : Maybe (Result (Http.Detailed.Error String) (Result String BackendLoadFromGitOkWithCache))
    }


type alias ImportFromZipArchiveDialogState =
    { loadCompositionResult : Maybe (Result String ImportFromZipArchiveOk)
    }


type LoadOrImportProjectStateOrigin
    = FromZipArchiveProjectState ImportFromZipArchiveOk
    | FromGitProjectState BackendLoadFromGitOkWithCache


type alias ImportFromZipArchiveOk =
    { fileTree : FileTreeInWorkspace.FileTreeNode
    , compositionIdCache : String
    }


type alias BackendLoadFromGitOkWithCache =
    { urlInCommit : String
    , compositionIdCache : String
    , fileTree : FileTreeInWorkspace.FileTreeNode
    }


main : BrowserApplicationInitWithTime.Program () State Event
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
    , Browser.Events.onMouseDown
        (isTargetOutsideParentWithId titleBarMenubarElementId UserInputFocusOutsideTitlebarMenu)
    ]
        |> Sub.batch


init : () -> Url.Url -> Navigation.Key -> Time.Posix -> ( State, Cmd Event )
init _ url navigationKey time =
    { navigationKey = navigationKey
    , url = url
    , time = time
    , workspace = WorkspaceErr "Initial update failed"
    , popup = Nothing
    , lastBackendLoadFromGitResult = Nothing
    }
        |> foldUpdates
            [ update (UrlChange url)
            , updateBeginLoadDefaultProjectIfWorkspaceErr
            ]


updateBeginLoadDefaultProjectIfWorkspaceErr : State -> ( State, Cmd Event )
updateBeginLoadDefaultProjectIfWorkspaceErr stateBefore =
    case stateBefore.workspace of
        WorkspaceErr _ ->
            updateToBeginLoadProjectState
                { projectStateExpectedCompositionHash = Nothing
                , filePathToOpen = Just [ "src", "Main.elm" ]
                }
                stateBefore
                { base = defaultProjectLink
                , differenceFromBase = ProjectState_2021_01.noDifference
                }

        _ ->
            ( stateBefore, Cmd.none )


foldUpdates : List (State -> ( State, Cmd Event )) -> State -> ( State, Cmd Event )
foldUpdates events stateBefore =
    List.foldl
        (\event ( state, cmds ) ->
            let
                ( nextState, nextCmd ) =
                    event state
            in
            ( nextState, nextCmd :: cmds )
        )
        ( stateBefore, [] )
        events
        |> Tuple.mapSecond Cmd.batch


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

        UserInputGetLinkToProject generateLink ->
            case stateBefore.workspace of
                WorkspaceOk workingState ->
                    let
                        urlToProject =
                            setProjectStateInUrlForBookmark
                                { createDiffIfBaseAvailable = generateLink.createDiffIfBaseAvailable }
                                workingState
                                stateBefore
                                |> Url.toString

                        dialog =
                            { urlToProject = urlToProject }

                        cmd =
                            Navigation.pushUrl stateBefore.navigationKey urlToProject
                    in
                    ( { stateBefore | popup = Just (ModalDialog (GetLinkToProjectDialog dialog)) }, cmd )

                _ ->
                    ( stateBefore, Cmd.none )

        UserInputLoadFromGitOpenDialog ->
            ( { stateBefore
                | popup =
                    Just
                        (ModalDialog
                            (LoadFromGitDialog
                                { urlIntoGitRepository = ""
                                , request = Nothing
                                , loadCompositionResult = Nothing
                                }
                            )
                        )
              }
            , focusInputUrlElementCmd
            )

        UserInputLoadFromGit dialogEvent ->
            case stateBefore.popup of
                Just (ModalDialog (LoadFromGitDialog dialogStateBefore)) ->
                    let
                        ( dialogState, cmd ) =
                            updateForUserInputLoadFromGit { time = stateBefore.time } dialogEvent dialogStateBefore
                    in
                    ( { stateBefore | popup = Just (ModalDialog (LoadFromGitDialog dialogState)) }
                    , cmd
                    )

                _ ->
                    ( stateBefore, Cmd.none )

        UserInputLoadOrImportTakeProjectStateEvent origin ->
            let
                ( fileTree, urlToPush ) =
                    case origin of
                        FromZipArchiveProjectState fromZip ->
                            ( fromZip.fileTree
                            , Url.Builder.absolute
                                (List.filter (String.isEmpty >> not) (String.split "/" stateBefore.url.path))
                                []
                            )

                        FromGitProjectState fromGit ->
                            ( fromGit.fileTree
                            , stateBefore.url
                                |> Frontend.ProjectStateInUrl.setProjectStateInUrl
                                    fromGit.fileTree
                                    (Just fromGit)
                                    { filePathToOpen = Nothing }
                                |> Url.toString
                            )
            in
            ( { stateBefore
                | popup = Nothing
                , workspace =
                    { fileTree = fileTree
                    , filePathOpenedInEditor = Nothing
                    }
                        |> initWorkspaceFromFileTreeAndFileSelection
                        |> WorkspaceOk
              }
            , urlToPush |> Navigation.pushUrl stateBefore.navigationKey
            )

        UserInputExportProjectToZipArchive { sendDownloadCmd } ->
            case stateBefore.workspace of
                WorkspaceOk workingState ->
                    let
                        cmd =
                            if sendDownloadCmd then
                                let
                                    projectStateHash =
                                        FileTreeInWorkspace.compositionHashFromFileTreeNode workingState.fileTree
                                in
                                workingState.fileTree
                                    |> buildZipArchiveFromFileTree
                                    |> Zip.toBytes
                                    |> File.Download.bytes
                                        ("elm-app-" ++ SHA256.toHex projectStateHash ++ ".zip")
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
                                        , compositionIdCache = Frontend.ProjectStateInUrl.projectStateCompositionHash (FileTreeInWorkspace.mapBlobsToBytes fileTree)
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
                        workspaceEventIsEditorGotFocus =
                            case workspaceEvent of
                                MonacoEditorEvent monacoEditorEventJson ->
                                    monacoEditorEventJson
                                        |> Json.Decode.decodeValue CompilationInterface.GenerateJsonConverters.jsonDecodeMessageFromMonacoEditor
                                        |> (==) (Ok Frontend.MonacoEditor.DidFocusEditorWidgetEvent)

                                _ ->
                                    False

                        mapForFocusOutsideTitlebarMenu =
                            if workspaceEventIsEditorGotFocus then
                                userInputFocusOutsideTitlebarMenu

                            else
                                identity

                        ( workspace, workspaceCmd ) =
                            workspaceBefore
                                |> updateWorkspace { time = stateBefore.time } workspaceEvent

                        shouldUpdateUrl =
                            case workspaceEvent of
                                UserInputCompile ->
                                    True

                                _ ->
                                    False

                        setProjectStateInUrlCmd =
                            if shouldUpdateUrl then
                                setProjectStateInUrlForBookmark
                                    { createDiffIfBaseAvailable = True }
                                    workspaceBefore
                                    stateBefore
                                    |> Url.toString
                                    |> Navigation.pushUrl stateBefore.navigationKey

                            else
                                Cmd.none
                    in
                    ( { stateBefore | workspace = WorkspaceOk workspace }
                        |> mapForFocusOutsideTitlebarMenu
                    , [ Cmd.map WorkspaceEvent workspaceCmd
                      , setProjectStateInUrlCmd
                      ]
                        |> Cmd.batch
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

        UserInputFocusOutsideTitlebarMenu ->
            ( userInputFocusOutsideTitlebarMenu stateBefore, Cmd.none )

        DiscardEvent ->
            ( stateBefore, Cmd.none )


userInputFocusOutsideTitlebarMenu : State -> State
userInputFocusOutsideTitlebarMenu stateBefore =
    let
        popupIsOpenForTitlebarMenu =
            case stateBefore.popup of
                Just (TitlebarMenu _ _) ->
                    True

                _ ->
                    False
    in
    if popupIsOpenForTitlebarMenu then
        { stateBefore | popup = Nothing }

    else
        stateBefore


setProjectStateInUrlForBookmark : { createDiffIfBaseAvailable : Bool } -> WorkingProjectStateStructure -> State -> Url.Url
setProjectStateInUrlForBookmark { createDiffIfBaseAvailable } workingState state =
    let
        baseToUse =
            if not createDiffIfBaseAvailable then
                Nothing

            else
                state.lastBackendLoadFromGitResult
                    |> Maybe.andThen (Tuple.second >> Result.toMaybe)
                    |> Maybe.andThen Result.toMaybe
    in
    state.url
        |> Frontend.ProjectStateInUrl.setProjectStateInUrl
            workingState.fileTree
            baseToUse
            { filePathToOpen = workingState.editing.filePathOpenedInEditor }


updateForUserInputLoadFromGit : { time : Time.Posix } -> UserInputLoadFromGitEventStructure -> LoadFromGitDialogState -> ( LoadFromGitDialogState, Cmd Event )
updateForUserInputLoadFromGit { time } event dialogStateBefore =
    case event of
        LoadFromGitEnterUrlEvent { urlIntoGitRepository } ->
            if dialogStateBefore.request /= Nothing || dialogStateBefore.loadCompositionResult /= Nothing then
                ( dialogStateBefore, Cmd.none )

            else
                let
                    dialogState =
                        { urlIntoGitRepository = urlIntoGitRepository
                        , request = Nothing
                        , loadCompositionResult = Nothing
                        }
                in
                ( dialogState
                , Cmd.none
                )

        LoadFromGitBeginRequestEvent { urlIntoGitRepository } ->
            let
                trimmedUrl =
                    String.trim urlIntoGitRepository
            in
            if trimmedUrl == "" then
                ( dialogStateBefore, Cmd.none )

            else
                let
                    dialogState =
                        { urlIntoGitRepository = urlIntoGitRepository
                        , request = Just { url = urlIntoGitRepository, time = time }
                        , loadCompositionResult = Nothing
                        }
                in
                ( dialogState
                , loadFromGitCmd urlIntoGitRepository
                )


updateWorkspace :
    { time : Time.Posix }
    -> WorkspaceEventStructure
    -> WorkingProjectStateStructure
    -> ( WorkingProjectStateStructure, Cmd WorkspaceEventStructure )
updateWorkspace updateConfig event stateBeforeApplyingEvent =
    let
        ( stateBeforeConsiderCompile, cmd ) =
            updateWorkspaceWithoutCmdToUpdateEditor updateConfig event stateBeforeApplyingEvent

        textForEditor =
            stateBeforeConsiderCompile
                |> fileOpenedInEditorFromWorkspace
                |> Maybe.andThen (Tuple.second >> .asBytes >> stringFromFileContent)
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
                    if
                        (stateBeforeConsiderCompile.compilation == stateBeforeApplyingEvent.compilation)
                            && (Just (Tuple.first fileOpenedInEditor) == filePathOpenedInEditorFromWorkspace stateBeforeApplyingEvent)
                            && (setTextToEditorCmd == Nothing)
                    then
                        Nothing

                    else
                        case stateBeforeConsiderCompile.compilation of
                            Just compilation ->
                                case compilation.loweringLastIteration of
                                    LoweringComplete loweringComplete ->
                                        let
                                            loweringMarkers =
                                                case loweringComplete.loweringResult of
                                                    Ok _ ->
                                                        []

                                                    Err (DependencyLoweringError _) ->
                                                        []

                                                    Err (LoweringError loweringErrors) ->
                                                        loweringErrors
                                                            |> editorDocumentMarkersFromFailedLowering
                                                                { compileRequest = compilation.origin.requestFromUser
                                                                , fileOpenedInEditor = fileOpenedInEditor
                                                                }

                                            elmMakeMarkers =
                                                loweringComplete.elmMakeResult
                                                    |> Maybe.andThen Result.toMaybe
                                                    |> Maybe.andThen .reportFromJson
                                                    |> editorDocumentMarkersFromElmMakeReport
                                                        { elmMakeRequest = loweringComplete.elmMakeRequest
                                                        , fileOpenedInEditor = fileOpenedInEditor
                                                        }
                                        in
                                        [ loweringMarkers, elmMakeMarkers ]
                                            |> List.concat
                                            |> setModelMarkersInMonacoEditorCmd
                                            |> Just

                                    _ ->
                                        Nothing

                            _ ->
                                Nothing

        triggerCompileForFirstOpenedModule =
            (stateBeforeConsiderCompile
                |> filePathOpenedInEditorFromWorkspace
                |> Maybe.andThen (List.reverse >> List.head)
                |> Maybe.map (String.endsWith ".elm")
                |> Maybe.withDefault False
            )
                && (stateBeforeConsiderCompile.compilation == Nothing)
                && (stateBeforeConsiderCompile.syntaxInspection == Nothing)

        ( state, compileCmd ) =
            if triggerCompileForFirstOpenedModule then
                compileFileOpenedInEditor stateBeforeConsiderCompile

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


fileOpenedInEditorFromWorkspace : WorkingProjectStateStructure -> Maybe ( List String, FileTreeInWorkspace.BlobNodeWithCache )
fileOpenedInEditorFromWorkspace workingState =
    case workingState.editing.filePathOpenedInEditor of
        Nothing ->
            Nothing

        Just filePathOpenedInEditor ->
            case workingState.fileTree |> FileTree.getBlobAtPathFromFileTree filePathOpenedInEditor of
                Nothing ->
                    Nothing

                Just fileContent ->
                    Just ( filePathOpenedInEditor, fileContent )


updateWorkspaceWithoutCmdToUpdateEditor :
    { time : Time.Posix }
    -> WorkspaceEventStructure
    -> WorkingProjectStateStructure
    -> ( WorkingProjectStateStructure, Cmd WorkspaceEventStructure )
updateWorkspaceWithoutCmdToUpdateEditor updateConfig event stateBefore =
    case event of
        UserInputOpenFileInEditor filePath ->
            if FileTree.getBlobAtPathFromFileTree filePath stateBefore.fileTree == Nothing then
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
                                |> FileTreeInWorkspace.setBlobAtPathInSortedFileTreeFromBytes ( filePath, fileContentFromString inputText )
                        , lastTextReceivedFromEditor = Just inputText
                        , elmFormat = Nothing
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
                    |> Json.Decode.decodeValue CompilationInterface.GenerateJsonConverters.jsonDecodeMessageFromMonacoEditor
            of
                Err decodeError ->
                    ( { stateBefore | decodeMessageFromMonacoEditorError = Just decodeError }, Cmd.none )

                Ok decodedMonacoEditorEvent ->
                    case decodedMonacoEditorEvent of
                        Frontend.MonacoEditor.DidChangeContentEvent content ->
                            stateBefore |> updateWorkspaceWithoutCmdToUpdateEditor updateConfig (UserInputChangeTextInEditor content)

                        Frontend.MonacoEditor.CompletedSetupEvent ->
                            ( { stateBefore | lastTextReceivedFromEditor = Nothing }, Cmd.none )

                        Frontend.MonacoEditor.EditorActionCloseEditorEvent ->
                            updateWorkspaceWithoutCmdToUpdateEditor updateConfig UserInputCloseEditor stateBefore

                        Frontend.MonacoEditor.EditorActionFormatDocumentEvent ->
                            updateWorkspaceWithoutCmdToUpdateEditor updateConfig UserInputFormat stateBefore

                        Frontend.MonacoEditor.EditorActionCompileEvent ->
                            updateWorkspaceWithoutCmdToUpdateEditor updateConfig UserInputCompile stateBefore

                        Frontend.MonacoEditor.EditorActionInspectSyntaxEvent ->
                            updateWorkspaceWithoutCmdToUpdateEditor updateConfig UserInputInspectSyntax stateBefore

                        Frontend.MonacoEditor.RequestCompletionItemsEvent requestCompletionItems ->
                            stateBefore
                                |> provideCompletionItems requestCompletionItems
                                |> Tuple.mapSecond provideCompletionItemsInMonacoEditorCmd

                        Frontend.MonacoEditor.RequestHoverEvent requestHoverEvent ->
                            stateBefore
                                |> provideHover requestHoverEvent
                                |> Tuple.mapSecond provideHoverInMonacoEditorCmd

                        Frontend.MonacoEditor.DidFocusEditorWidgetEvent ->
                            ( stateBefore, Cmd.none )

        UserInputFormat ->
            case elmFormatCmdFromState stateBefore of
                Nothing ->
                    ( stateBefore
                    , Cmd.none
                    )

                Just ( moduleText, elmFormatCmd ) ->
                    ( { stateBefore
                        | elmFormat = Just (ElmFormatInProgress { startTime = updateConfig.time, inputModuleText = moduleText })
                      }
                    , elmFormatCmd
                    )

        UserInputCancelFormatting ->
            ( { stateBefore | elmFormat = Nothing }, Cmd.none )

        UserInputCompile ->
            compileFileOpenedInEditor { stateBefore | viewEnlargedPane = Nothing }

        UserInputInspectSyntax ->
            syntaxInspectFileOpenedInEditor { stateBefore | viewEnlargedPane = Nothing }

        UserInputCloseEditor ->
            ( let
                editing =
                    stateBefore.editing
              in
              { stateBefore
                | editing = { editing | filePathOpenedInEditor = Nothing }
                , elmFormat = Nothing
              }
            , Cmd.none
            )

        BackendElmFormatResponseEvent formatResponseEvent ->
            case stateBefore.elmFormat of
                Just (ElmFormatInProgress formatStart) ->
                    ( if Just formatResponseEvent.filePath /= stateBefore.editing.filePathOpenedInEditor then
                        stateBefore

                      else
                        case formatResponseEvent.result |> Result.toMaybe |> Maybe.andThen .formattedText of
                            Nothing ->
                                stateBefore

                            Just formattedText ->
                                let
                                    formatResult =
                                        formatResponseEvent.result
                                            |> Result.map parseElmFormatResponse
                                in
                                { stateBefore
                                    | fileTree =
                                        stateBefore.fileTree
                                            |> FileTreeInWorkspace.setBlobAtPathInSortedFileTreeFromBytes
                                                ( formatResponseEvent.filePath
                                                , fileContentFromString formattedText
                                                )
                                    , elmFormat = Just (ElmFormatResult formatStart formatResult)
                                }
                    , Cmd.none
                    )

                _ ->
                    ( stateBefore, Cmd.none )

        BackendElmMakeResponseEvent requestIdentity httpResponse ->
            case stateBefore.compilation of
                Just compilationBefore ->
                    let
                        ( compilation, cmd ) =
                            updateCompilationForElmMakeRequestResponse
                                requestIdentity
                                httpResponse
                                compilationBefore
                    in
                    ( { stateBefore
                        | compilation = Just compilation
                        , elmFormat = Nothing
                      }
                    , Maybe.withDefault Cmd.none cmd
                    )

                _ ->
                    ( stateBefore, Cmd.none )

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


updateCompilationForElmMakeRequestResponse :
    ElmMakeRequestStructure
    -> Result (Http.Detailed.Error String) ElmMakeResponseStructure
    -> CompilationState
    -> ( CompilationState, Maybe (Cmd WorkspaceEventStructure) )
updateCompilationForElmMakeRequestResponse elmMakeRequest httpResponse compilationBefore =
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
    case compilationBefore.loweringLastIteration of
        LoweringComplete loweringComplete ->
            if elmMakeRequest == loweringComplete.elmMakeRequest then
                ( { compilationBefore
                    | loweringLastIteration =
                        LoweringComplete
                            { loweringComplete
                                | elmMakeResult = Just elmMakeResult
                            }
                  }
                , Nothing
                )

            else
                ( compilationBefore, Nothing )

        LoweringContinue loweringContinue ->
            if elmMakeRequest == loweringContinue.elmMakeRequest then
                case elmMakeResult of
                    Err elmMakeErr ->
                        ( { compilationBefore
                            | loweringLastIteration =
                                LoweringComplete
                                    { loweringResult = Err (DependencyLoweringError elmMakeErr)
                                    , elmMakeRequest = compilationBefore.origin.requestFromUser
                                    , elmMakeResult = Nothing
                                    }
                          }
                        , Nothing
                        )

                    Ok elmMakeOk ->
                        let
                            compilation =
                                continueLoweringWithResolvedDependency
                                    ( loweringContinue.forDependencyElmMakeRequest
                                    , elmMakeOk.response
                                    )
                                    compilationBefore
                        in
                        ( compilation
                        , Just (requestToBackendCmdFromCompilationInProgress compilation)
                        )

            else
                ( compilationBefore, Nothing )


parseElmFormatResponse : FrontendBackendInterface.FormatElmModuleTextResponseStructure -> Result FrontendBackendInterface.FormatElmModuleTextResponseStructure String
parseElmFormatResponse response =
    case response.formattedText of
        Nothing ->
            Err response

        Just formattedText ->
            if response.processOutput.exitCode == 0 then
                Ok formattedText

            else
                Err response


provideCompletionItems :
    Frontend.MonacoEditor.RequestCompletionItemsStruct
    -> WorkingProjectStateStructure
    -> ( WorkingProjectStateStructure, List Frontend.MonacoEditor.MonacoCompletionItem )
provideCompletionItems request stateBefore =
    case stateBefore.editing.filePathOpenedInEditor of
        Nothing ->
            ( stateBefore, [] )

        Just filePathOpenedInEditor ->
            updateAndGetFromLanguageService
                (LanguageService.provideCompletionItems
                    { filePathOpenedInEditor = filePathOpenedInEditor
                    , textUntilPosition = request.textUntilPosition
                    , cursorLineNumber = request.cursorLineNumber
                    }
                )
                stateBefore


provideHover :
    Frontend.MonacoEditor.RequestHoverStruct
    -> WorkingProjectStateStructure
    -> ( WorkingProjectStateStructure, List String )
provideHover request stateBefore =
    case stateBefore.editing.filePathOpenedInEditor of
        Nothing ->
            ( stateBefore, [] )

        Just filePathOpenedInEditor ->
            updateAndGetFromLanguageService
                (LanguageService.provideHover
                    { filePathOpenedInEditor = filePathOpenedInEditor
                    , positionLineNumber = request.positionLineNumber
                    , positionColumn = request.positionColumn
                    , lineText = request.lineText
                    }
                )
                stateBefore


updateAndGetFromLanguageService :
    (LanguageService.LanguageServiceState -> a)
    -> WorkingProjectStateStructure
    -> ( WorkingProjectStateStructure, a )
updateAndGetFromLanguageService getFromLangService stateBefore =
    let
        languageServiceState =
            LanguageService.updateLanguageServiceState stateBefore.fileTree stateBefore.languageServiceState
    in
    ( { stateBefore | languageServiceState = languageServiceState }
    , getFromLangService languageServiceState
    )


processEventUrlChanged : Url.Url -> State -> ( State, Cmd Event )
processEventUrlChanged url stateBefore =
    let
        projectStateExpectedCompositionHash =
            Frontend.ProjectStateInUrl.projectStateHashFromUrl url

        filePathToOpen =
            Frontend.ProjectStateInUrl.filePathToOpenFromUrl url
                |> Maybe.map (String.split "/" >> List.concatMap (String.split "\\"))

        projectWithMatchingStateHashAlreadyLoaded =
            case stateBefore.workspace of
                WorkspaceOk workingState ->
                    Just (Frontend.ProjectStateInUrl.projectStateCompositionHash (FileTreeInWorkspace.mapBlobsToBytes workingState.fileTree))
                        == projectStateExpectedCompositionHash

                _ ->
                    False

        continueWithDiffProjectState =
            updateToBeginLoadProjectState
                { projectStateExpectedCompositionHash = projectStateExpectedCompositionHash
                , filePathToOpen = filePathToOpen
                }
                stateBefore
    in
    case Frontend.ProjectStateInUrl.projectStateDescriptionFromUrl url of
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
                    Frontend.ProjectStateInUrl.LiteralProjectState fileTreeFromUrl ->
                        updateForLoadedProjectState
                            { expectedCompositionHash = projectStateExpectedCompositionHash
                            , filePathToOpen = filePathToOpen
                            }
                            (FileTreeInWorkspace.mapBlobsFromBytes fileTreeFromUrl)
                            ProjectState_2021_01.noDifference
                            stateBefore

                    Frontend.ProjectStateInUrl.LinkProjectState linkToProjectState ->
                        continueWithDiffProjectState
                            { base = linkToProjectState
                            , differenceFromBase = ProjectState_2021_01.noDifference
                            }

                    Frontend.ProjectStateInUrl.DiffProjectState_Version_2021_01 diffProjectState ->
                        continueWithDiffProjectState diffProjectState


updateToBeginLoadProjectState :
    { projectStateExpectedCompositionHash : Maybe String, filePathToOpen : Maybe (List String) }
    -> State
    -> ProjectState_2021_01.ProjectState
    -> ( State, Cmd Event )
updateToBeginLoadProjectState { projectStateExpectedCompositionHash, filePathToOpen } stateBefore projectStateDescription =
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


processEventBackendLoadFromGitResult : String -> Result (Http.Detailed.Error String) (Result String FrontendBackendInterface.LoadCompositionResponseStructure) -> State -> ( State, Cmd Event )
processEventBackendLoadFromGitResult urlIntoGitRepository result stateBeforeRememberingResult =
    let
        resultWithFileTreeAndCache =
            result
                |> Result.map
                    (Result.map
                        (\loadOk ->
                            { fileTree = fileTreeNodeFromListFileWithPath loadOk.filesAsFlatList
                            , compositionIdCache = loadOk.compositionId
                            , urlInCommit = loadOk.urlInCommit
                            }
                        )
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
                        case resultWithFileTreeAndCache |> Result.Extra.unpack (describeHttpError >> Err) identity of
                            Ok loadOk ->
                                updateForLoadedProjectState
                                    { expectedCompositionHash = projectStateLoadingFromLink.expectedCompositionHash
                                    , filePathToOpen = projectStateLoadingFromLink.filePathToOpen
                                    }
                                    loadOk.fileTree
                                    projectStateLoadingFromLink.projectStateDescription.differenceFromBase
                                    stateBefore

                            Err loadingError ->
                                ( { stateBefore | workspace = WorkspaceErr loadingError }
                                , Cmd.none
                                )

                    else
                        ( stateBefore, Cmd.none )

                _ ->
                    ( stateBefore, Cmd.none )


updateForLoadedProjectState :
    { expectedCompositionHash : Maybe String, filePathToOpen : Maybe (List String) }
    -> FileTreeInWorkspace.FileTreeNode
    -> ProjectState_2021_01.ProjectStateDifference
    -> State
    -> ( State, Cmd Event )
updateForLoadedProjectState config loadedBaseProjectState projectStateDiff stateBefore =
    ( (case
        FileTreeInWorkspace.applyProjectStateDifference_2021_01
            projectStateDiff
            (FileTreeInWorkspace.mapBlobsToBytes loadedBaseProjectState)
       of
        Err error ->
            Err ("Failed to apply difference model to compute project state: " ++ error)

        Ok composedProjectState ->
            let
                composedProjectStateHashBase16 =
                    Frontend.ProjectStateInUrl.projectStateCompositionHash composedProjectState

                continueIfHashOk =
                    { stateBefore
                        | workspace =
                            { fileTree = FileTreeInWorkspace.mapBlobsFromBytes composedProjectState
                            , filePathOpenedInEditor = config.filePathToOpen
                            }
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


fileTreeNodeFromListFileWithPath : List FrontendBackendInterface.FileWithPath -> FileTreeInWorkspace.FileTreeNode
fileTreeNodeFromListFileWithPath =
    List.map
        (\file ->
            ( file.path
            , file.contentBase64
                |> Base64.toBytes
                |> Maybe.withDefault ("Failed to decode from Base64" |> Bytes.Encode.string |> Bytes.Encode.encode)
            )
        )
        >> FileTreeInWorkspace.sortedFileTreeFromListOfBlobsAsBytes


elmFormatCmdFromState : WorkingProjectStateStructure -> Maybe ( String, Cmd WorkspaceEventStructure )
elmFormatCmdFromState state =
    case fileOpenedInEditorFromWorkspace state of
        Nothing ->
            Nothing

        Just ( filePath, fileContent ) ->
            case stringFromFileContent fileContent.asBytes of
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
                    ( fileContentString
                    , requestToApiCmd request
                        jsonDecoder
                        (Result.map Tuple.second >> (\result -> BackendElmFormatResponseEvent { filePath = filePath, result = result }))
                    )
                        |> Just


loadFromGitCmd : String -> Cmd Event
loadFromGitCmd urlIntoGitRepository =
    let
        backendResponseJsonDecoder backendResponse =
            case backendResponse of
                FrontendBackendInterface.LoadCompositionResponse loadComposition ->
                    Json.Decode.succeed (Ok loadComposition)

                FrontendBackendInterface.ErrorResponse error ->
                    Json.Decode.succeed (Err ("The server reported an error:\n" ++ error))

                _ ->
                    Json.Decode.fail "Unexpected response: Not a LoadCompositionResponse"
    in
    requestToApiCmd
        (FrontendBackendInterface.LoadCompositionRequest (String.trim urlIntoGitRepository))
        backendResponseJsonDecoder
        (Result.map Tuple.second >> BackendLoadFromGitResultEvent urlIntoGitRepository)


compileFileOpenedInEditor : WorkingProjectStateStructure -> ( WorkingProjectStateStructure, Cmd WorkspaceEventStructure )
compileFileOpenedInEditor stateBefore =
    case prepareCompileForFileOpenedInEditor stateBefore of
        Nothing ->
            ( stateBefore, Cmd.none )

        Just preparedCompilation ->
            let
                compilation =
                    preparedCompilation.compile ()

                requestToBackendCmd =
                    requestToBackendCmdFromCompilationInProgress compilation
            in
            ( { stateBefore
                | compilation = Just compilation
                , syntaxInspection = Nothing
              }
            , requestToBackendCmd
            )


syntaxInspectFileOpenedInEditor : WorkingProjectStateStructure -> ( WorkingProjectStateStructure, Cmd WorkspaceEventStructure )
syntaxInspectFileOpenedInEditor stateBefore =
    case fileOpenedInEditorFromWorkspace stateBefore of
        Nothing ->
            ( stateBefore, Cmd.none )

        Just ( _, fileOpenedInEditor ) ->
            let
                parseFileResult =
                    fileOpenedInEditor.asBytes
                        |> stringFromFileContent
                        |> Maybe.map
                            (\fileContentString ->
                                Ok ( fileContentString, CompileElmApp.parseElmModuleText fileContentString )
                            )
                        |> Maybe.withDefault (Err "Failed to decode file contents as string")
            in
            ( { stateBefore
                | compilation = Nothing
                , syntaxInspection =
                    Just { parseFileResult = parseFileResult }
              }
            , Cmd.none
            )


requestToBackendCmdFromCompilationInProgress : CompilationState -> Cmd WorkspaceEventStructure
requestToBackendCmdFromCompilationInProgress compilation =
    let
        jsonDecoder backendResponse =
            case backendResponse of
                FrontendBackendInterface.ElmMakeResponse elmMakeResponse ->
                    Json.Decode.succeed elmMakeResponse

                _ ->
                    Json.Decode.fail "Unexpected response"

        elmMakeRequest =
            case compilation.loweringLastIteration of
                LoweringContinue loweringContinue ->
                    loweringContinue.elmMakeRequest

                LoweringComplete loweringComplete ->
                    loweringComplete.elmMakeRequest
    in
    requestToApiCmd
        (FrontendBackendInterface.ElmMakeRequest elmMakeRequest)
        jsonDecoder
        (Result.map Tuple.second >> BackendElmMakeResponseEvent elmMakeRequest)


prepareCompileForFileOpenedInEditor :
    WorkingProjectStateStructure
    -> Maybe { requestFromUserIdentity : ElmMakeRequestStructure, compile : () -> CompilationState }
prepareCompileForFileOpenedInEditor workspace =
    case workspace.editing.filePathOpenedInEditor of
        Nothing ->
            Nothing

        Just entryPointFilePath ->
            let
                filesBeforeLowering =
                    workspace.fileTree
                        |> FileTree.flatListOfBlobsFromFileTreeNode

                directoryContainsElmJson directoryPath =
                    filesBeforeLowering |> List.map Tuple.first |> List.member (directoryPath ++ [ "elm.json" ])

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

                requestFromUserIdentity =
                    { files = mapFilesToRequestToBackendStructure .asBase64 filesBeforeLowering
                    , workingDirectoryPath = workingDirectoryPath
                    , entryPointFilePathFromWorkingDirectory = entryPointFilePath |> List.drop (List.length workingDirectoryPath)
                    , makeOptionDebug = workspace.enableInspectionOnCompile
                    , outputType = CompileElmApp.ElmMakeOutputTypeHtml
                    }

                compile () =
                    let
                        filesBeforeLoweringOnlyAsBytes =
                            filesBeforeLowering |> List.map (Tuple.mapSecond .asBytes)

                        loweredElmAppFromDependencies dependencies =
                            CompileElmApp.asCompletelyLoweredElmApp
                                CompileElmApp.defaultEntryPoints
                                { compilationInterfaceElmModuleNamePrefixes = [ "CompilationInterface" ]
                                , sourceFiles = Dict.fromList filesBeforeLoweringOnlyAsBytes
                                , dependencies = dependencies
                                , compilationRootFilePath = entryPointFilePath
                                , interfaceToHostRootModuleName = []
                                }
                                {-
                                   TODO: Instead of discarding the 'rootModuleEntryPointKind' here, adapt to the different kinds of entry points.
                                   In case the root builds a blob, we can get the bytes value and offer exporting to a file.
                                -}
                                |> Result.map .compiledFiles

                        compilationOrigin =
                            { requestFromUser = requestFromUserIdentity
                            , loweredElmAppFromDependencies = loweredElmAppFromDependencies
                            }

                        loweringIterationComplete =
                            continueLowering [] compilationOrigin
                    in
                    { origin = compilationOrigin
                    , loweringLastIteration = loweringIterationComplete
                    , dependenciesFromCompletedIterations = []
                    }
            in
            Just
                { requestFromUserIdentity = requestFromUserIdentity
                , compile = compile
                }


continueLoweringWithResolvedDependency :
    ( CompileElmApp.ElmMakeRequestStructure, ElmMakeResponseStructure )
    -> CompilationState
    -> CompilationState
continueLoweringWithResolvedDependency ( elmMakeDependency, lastIterationResponse ) compilationInProgress =
    let
        newDependencies =
            case lastIterationResponse.outputFileContentBase64 |> Maybe.andThen Base64.toBytes of
                Nothing ->
                    []

                Just outputFileContent ->
                    [ ( CompileElmApp.ElmMakeDependency elmMakeDependency, outputFileContent ) ]

        dependenciesFromCompletedIterations =
            compilationInProgress.dependenciesFromCompletedIterations ++ newDependencies

        loweringIterationComplete =
            compilationInProgress.origin
                |> continueLowering dependenciesFromCompletedIterations
    in
    { compilationInProgress
        | loweringLastIteration = loweringIterationComplete
        , dependenciesFromCompletedIterations = dependenciesFromCompletedIterations
    }


continueLowering :
    List ( CompileElmApp.DependencyKey, Bytes.Bytes )
    -> CompilationOrigin
    -> LoweringIterationComplete
continueLowering dependenciesFromCompletedIterations compilationOrigin =
    let
        base64FromBytes : Bytes.Bytes -> String
        base64FromBytes =
            Base64.fromBytes >> Maybe.withDefault "Error encoding in base64"
    in
    case
        compilationOrigin.loweredElmAppFromDependencies dependenciesFromCompletedIterations
    of
        Err loweringErrors ->
            let
                errorsMissingDependencyElmMake =
                    loweringErrors
                        |> List.filterMap
                            (\locatedError ->
                                case locatedError of
                                    CompileElmApp.LocatedInSourceFiles _ error ->
                                        case error of
                                            CompileElmApp.MissingDependencyError (CompileElmApp.ElmMakeDependency missingDependencyElmMake) ->
                                                Just missingDependencyElmMake

                                            _ ->
                                                Nothing
                            )
            in
            case List.head errorsMissingDependencyElmMake of
                Just missingDependencyElmMake ->
                    LoweringContinue
                        { elmMakeRequest =
                            { files =
                                missingDependencyElmMake.files
                                    |> Dict.toList
                                    |> mapFilesToRequestToBackendStructure base64FromBytes
                            , workingDirectoryPath = []
                            , entryPointFilePathFromWorkingDirectory = missingDependencyElmMake.entryPointFilePath
                            , makeOptionDebug = missingDependencyElmMake.enableDebug
                            , outputType = missingDependencyElmMake.outputType
                            }
                        , forDependencyElmMakeRequest = missingDependencyElmMake
                        }

                Nothing ->
                    LoweringComplete
                        { loweringResult = Err (LoweringError loweringErrors)
                        , elmMakeRequest = compilationOrigin.requestFromUser
                        , elmMakeResult = Nothing
                        }

        Ok completedLowering ->
            let
                requestFromUser =
                    compilationOrigin.requestFromUser

                files =
                    completedLowering
                        |> Dict.toList
                        |> List.filter (Tuple.first >> CompileElmApp.includeFilePathInElmMakeRequest)
            in
            LoweringComplete
                { loweringResult = Ok ()
                , elmMakeRequest = { requestFromUser | files = mapFilesToRequestToBackendStructure base64FromBytes files }
                , elmMakeResult = Nothing
                }


mapFilesToRequestToBackendStructure : (b -> a) -> List ( c, b ) -> List { path : c, contentBase64 : a }
mapFilesToRequestToBackendStructure mapContent =
    List.map (\( path, content ) -> { path = path, contentBase64 = mapContent content })


requestToApiCmd :
    FrontendBackendInterface.RequestStructure
    -> (FrontendBackendInterface.ResponseStructure -> Json.Decode.Decoder event)
    -> (Result (Http.Detailed.Error String) ( Http.Metadata, event ) -> mappedEvent)
    -> Cmd mappedEvent
requestToApiCmd request jsonDecoderSpecialization eventConstructor =
    let
        jsonDecoder =
            CompilationInterface.GenerateJsonConverters.jsonDecodeResponseStructure
                |> Json.Decode.andThen jsonDecoderSpecialization
    in
    Http.post
        { url = Url.Builder.absolute [ "api" ] []
        , body =
            Http.jsonBody
                (request |> CompilationInterface.GenerateJsonConverters.jsonEncodeRequestStructure)
        , expect = Http.Detailed.expectJson eventConstructor jsonDecoder
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
                                                    FileTree.BlobNode _ ->
                                                        Just (UserInputOpenFileInEditor (upperPath ++ [ nodeName ]))

                                                    FileTree.TreeNode _ ->
                                                        Nothing
                                        in
                                        viewFileTree
                                            { selectEventFromNode = selectEventFromFileTreeNode
                                            , iconFromFileName = Visuals.iconFromFileName
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
                                                |> Element.el [ Element.width (Element.px defaultFontSize) ]

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
                                                                |> Maybe.andThen Visuals.iconFromFileName
                                                                |> headerIconElementFromTypeAndColor

                                                        elementFromPathSegmentText appendDirectorySeparator segmentText =
                                                            [ Element.text segmentText
                                                            , if appendDirectorySeparator then
                                                                directorySeparatorIconElement

                                                              else
                                                                Element.none
                                                            ]
                                                                |> Element.row
                                                                    [ Element.spacing (defaultFontSize // 2)
                                                                    , Element.alpha 0.83
                                                                    ]
                                                    in
                                                    (directoryPathReversed |> List.reverse |> List.map (elementFromPathSegmentText True))
                                                        ++ [ [ fileIconElement, fileName |> elementFromPathSegmentText False ]
                                                                |> Element.row [ Element.spacing (defaultFontSize // 2) ]
                                                           ]
                                                        |> Element.row
                                                            [ Element.spacing (defaultFontSize // 2) ]

                                        closeEditorElement =
                                            Element.Input.button
                                                [ Element.mouseOver [ Element.Background.color (Element.rgba 1 1 1 0.2) ]
                                                , Element.padding 4
                                                , Element.scale 0.8
                                                , Element.centerY
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
                                                    , Element.htmlAttribute (HA.style "user-select" "none")
                                                    ]
                                            , [ buttonElement { label = "📄 Format", onPress = Just UserInputFormat }
                                              , buttonCompile
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

                                        editorModalOverlay =
                                            case workingState.elmFormat of
                                                Nothing ->
                                                    Nothing

                                                Just elmFormat ->
                                                    let
                                                        buttonCancelFormat =
                                                            buttonElement
                                                                { label = "Cancel formatting"
                                                                , onPress = Just UserInputCancelFormatting
                                                                }

                                                        continueWithHeadingAndContent config =
                                                            [ config.headingText
                                                                |> Element.text
                                                                |> Element.el (headingAttributes 3)
                                                            , config.mainContent
                                                                |> Element.el
                                                                    [ Element.padding defaultFontSize
                                                                    , Element.scrollbarY
                                                                    , Element.width Element.fill
                                                                    , Element.height Element.fill
                                                                    ]
                                                            , buttonCancelFormat |> Element.el [ Element.alignBottom, Element.alignLeft ]
                                                            ]
                                                                |> Element.column
                                                                    [ Element.spacing defaultFontSize
                                                                    , Element.width Element.fill
                                                                    , Element.height Element.fill
                                                                    ]

                                                        continueWithErrorText errorText =
                                                            continueWithHeadingAndContent
                                                                { headingText = "Formatting failed"
                                                                , mainContent =
                                                                    [ errorText |> dialogErrorElementFromDescription
                                                                    , [ "To see syntax errors in the code editor, use 'Compile'" |> Element.text
                                                                      , buttonCompile
                                                                      ]
                                                                        |> Element.wrappedRow [ Element.spacing defaultFontSize ]
                                                                    ]
                                                                        |> Element.column
                                                                            [ Element.spacing defaultFontSize
                                                                            , Element.width Element.fill
                                                                            , Element.height Element.fill
                                                                            ]
                                                                }
                                                    in
                                                    case elmFormat of
                                                        ElmFormatInProgress _ ->
                                                            continueWithHeadingAndContent
                                                                { headingText = "Formatting ..."
                                                                , mainContent = Element.none
                                                                }
                                                                |> Just

                                                        ElmFormatResult _ formatResult ->
                                                            case formatResult of
                                                                Err httpError ->
                                                                    [ "HTTP Error:"
                                                                    , describeHttpError httpError
                                                                    ]
                                                                        |> String.join "\n"
                                                                        |> continueWithErrorText
                                                                        |> Just

                                                                Ok (Err errorResponse) ->
                                                                    [ "elm-format reported an error:"
                                                                    , errorResponse.processOutput.standardError
                                                                    , errorResponse.processOutput.standardOutput
                                                                    ]
                                                                        |> String.join "\n"
                                                                        |> continueWithErrorText
                                                                        |> Just

                                                                Ok (Ok _) ->
                                                                    Nothing

                                        editorPaneContent =
                                            monacoEditorElement state
                                                |> Element.el
                                                    [ Element.transparent (editorModalOverlay /= Nothing)
                                                    , Element.width Element.fill
                                                    , Element.height Element.fill
                                                    ]
                                                |> Element.el
                                                    ((editorModalOverlay
                                                        |> Maybe.map
                                                            (Element.el
                                                                [ Element.padding defaultFontSize
                                                                , Element.width Element.fill
                                                                , Element.height Element.fill
                                                                ]
                                                                >> Element.inFront
                                                                >> List.singleton
                                                            )
                                                        |> Maybe.withDefault []
                                                     )
                                                        ++ [ Element.width Element.fill
                                                           , Element.height Element.fill
                                                           ]
                                                    )
                                    in
                                    { editorPaneHeader = headerElement
                                    , editorPaneContent = editorPaneContent
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
                    [ ("Failed to load project state: " ++ String.left 1000 projectStateError)
                        |> dialogErrorElementFromDescription
                        |> Element.el
                            [ Element.padding defaultFontSize
                            , Element.width Element.fill
                            , Element.height Element.fill
                            , Element.scrollbarY
                            ]
                    ]
                        |> Element.column
                            [ Element.width Element.fill
                            , Element.height Element.fill
                            ]

        logoElement =
            [ Visuals.elmEditorIconSvg "1.4em" |> Element.html |> Element.el []
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
                |> Element.row
                    [ Element.width Element.fill
                    , Element.height Element.fill

                    {-
                       2022-05-28 Observation: This "focusin" handler is not invoked when user clicks in one of the iframes (Monaco editor or output live test)
                    -}
                    , Element.htmlAttribute
                        (Html.Events.on "focusin" (Json.Decode.succeed UserInputFocusOutsideTitlebarMenu))
                    ]
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


focusInputUrlElementCmd : Cmd Event
focusInputUrlElementCmd =
    Browser.Dom.focus inputUrlElementId |> Task.attempt (\_ -> DiscardEvent)


titleBarMenubarElementId : String
titleBarMenubarElementId =
    "titlebar-menubar"


inputUrlElementId : String
inputUrlElementId =
    "input-url"


type alias FileTreeNodeViewModel event =
    { indentLevel : Int
    , label : String
    , selectEvent : Maybe event
    , icon : Maybe ( Visuals.Icon, String )
    }


sortFileTreeForExplorerView : FileTreeInWorkspace.FileTreeNode -> FileTreeInWorkspace.FileTreeNode
sortFileTreeForExplorerView node =
    case node of
        FileTree.BlobNode _ ->
            node

        FileTree.TreeNode tree ->
            tree
                |> List.map (Tuple.mapSecond sortFileTreeForExplorerView)
                |> List.sortBy
                    (\( _, child ) ->
                        if FileTree.isBlobNode child then
                            0

                        else
                            1
                    )
                |> FileTree.TreeNode


viewFileTree :
    { selectEventFromNode : List String -> ( String, FileTreeInWorkspace.FileTreeNode ) -> Maybe event
    , iconFromFileName : String -> Maybe ( Visuals.Icon, String )
    }
    -> FileTreeInWorkspace.FileTreeNode
    -> Element.Element event
viewFileTree configuration rootNode =
    case rootNode of
        FileTree.BlobNode _ ->
            Element.text "Error: Root node is a blob, not a tree"

        FileTree.TreeNode treeItems ->
            treeItems
                |> List.concatMap (buildFileTreeViewList configuration [])
                |> viewFileTreeList


buildFileTreeViewList :
    { selectEventFromNode : List String -> ( String, FileTreeInWorkspace.FileTreeNode ) -> Maybe event
    , iconFromFileName : String -> Maybe ( Visuals.Icon, String )
    }
    -> List String
    -> ( String, FileTreeInWorkspace.FileTreeNode )
    -> List (FileTreeNodeViewModel event)
buildFileTreeViewList configuration path ( currentNodeName, currentNodeContent ) =
    let
        icon =
            case currentNodeContent of
                FileTree.TreeNode _ ->
                    Just ( Visuals.DirectoryExpandedIcon, "white" )

                FileTree.BlobNode _ ->
                    configuration.iconFromFileName currentNodeName

        currentItem =
            { indentLevel = List.length path
            , label = currentNodeName
            , selectEvent = configuration.selectEventFromNode path ( currentNodeName, currentNodeContent )
            , icon = icon
            }
    in
    case currentNodeContent of
        FileTree.BlobNode _ ->
            [ currentItem ]

        FileTree.TreeNode tree ->
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
                    [ iconElement |> Element.el [ Element.width (Element.px defaultFontSize) ]
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
            20
    in
    Element.Input.button
        [ Element.Background.color (Element.rgb 0.2 0.2 0.2)
        , Element.mouseOver [ Element.Background.color (Element.rgb 0.3 0.3 0.3) ]
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


viewGetLinkToProjectDialog : GetLinkToProjectDialogState -> FileTreeInWorkspace.FileTreeNode -> PopupWindowAttributes Event
viewGetLinkToProjectDialog dialogState projectState =
    let
        linkElementFromUrl urlToProject =
            let
                maybeDependencyUrl =
                    case
                        urlToProject
                            |> Url.fromString
                            |> Maybe.andThen Frontend.ProjectStateInUrl.projectStateDescriptionFromUrl
                    of
                        Nothing ->
                            Nothing

                        Just (Err _) ->
                            Nothing

                        Just (Ok projectDescription) ->
                            case projectDescription of
                                Frontend.ProjectStateInUrl.LiteralProjectState _ ->
                                    Nothing

                                Frontend.ProjectStateInUrl.LinkProjectState link ->
                                    Just link

                                Frontend.ProjectStateInUrl.DiffProjectState_Version_2021_01 diffProjectState ->
                                    Just diffProjectState.base

                dependenciesDescriptionLines =
                    maybeDependencyUrl
                        |> Maybe.map
                            (\dependencyUrl ->
                                [ [ Element.text "The project state in this link depends on loading related data from the following URL: "
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
            [ [ "Use the link below to load the project's current state again later. This link is a fast way to share your project state with other people."
                    |> Element.text
              ]
                |> Element.paragraph []
            , linkElementFromUrl dialogState.urlToProject
            ]
                |> Element.column [ Element.spacing defaultFontSize ]
    in
    { title = "Get Link to Project for Bookmarking or Sharing"
    , titleIcon = Just FontAwesome.Solid.bookmark
    , guideParagraphItems =
        [ Element.text "Get a link to save or share your project's current state." ]
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
        userInputBeginLoadingDialogEvent =
            LoadFromGitBeginRequestEvent { urlIntoGitRepository = dialogState.urlIntoGitRepository }

        userInputBeginLoadingEvent =
            UserInputLoadFromGit userInputBeginLoadingDialogEvent

        userInputBeginLoadingCouldHaveEffect =
            updateForUserInputLoadFromGit { time = Time.millisToPosix 0 } userInputBeginLoadingDialogEvent dialogState
                /= ( dialogState, Cmd.none )

        urlInputElement =
            Element.Input.text
                [ Element.Background.color backgroundColor
                , onKeyDownEnter userInputBeginLoadingEvent
                , Element.htmlAttribute (HA.id inputUrlElementId)
                ]
                { onChange = \url -> UserInputLoadFromGit (LoadFromGitEnterUrlEvent { urlIntoGitRepository = url })
                , text = dialogState.urlIntoGitRepository
                , placeholder = Just (Element.Input.placeholder [] (Element.text "URL to tree in git repository"))
                , label = Element.Input.labelAbove [] (Element.text "URL to tree in git repository")
                }

        sendRequestButton =
            buttonElement
                { label = "Begin Loading"
                , onPress = Just userInputBeginLoadingEvent
                }

        inputOrProgressElement =
            case dialogState.request of
                Nothing ->
                    [ urlInputElement
                    , sendRequestButton
                        |> Element.el
                            [ Element.centerX
                            , elementTransparent (not userInputBeginLoadingCouldHaveEffect)
                            ]
                    ]
                        |> Element.column
                            [ Element.width Element.fill
                            , Element.spacing defaultFontSize
                            ]

                Just request ->
                    let
                        describeProgress =
                            case dialogState.loadCompositionResult of
                                Nothing ->
                                    "in progress..."

                                Just (Err _) ->
                                    "failed"

                                Just (Ok (Err _)) ->
                                    "failed"

                                Just (Ok (Ok _)) ->
                                    "completed"
                    in
                    [ Element.text "Loading from "
                    , linkElementFromUrlAndTextLabel { url = request.url, labelText = request.url }
                    , Element.text (" " ++ describeProgress)
                    ]
                        |> Element.paragraph []

        resultElement =
            dialogState.loadCompositionResult
                |> Maybe.map
                    (Result.Extra.unpack (describeHttpError >> Err) identity
                        >> Result.mapError (String.left 500)
                        >> Result.map FromGitProjectState
                        >> viewLoadOrImportDialogResultElement
                            dialogErrorElementFromDescription
                            UserInputLoadOrImportTakeProjectStateEvent
                    )
                |> Maybe.withDefault Element.none

        exampleUrl =
            "https://github.com/onlinegamemaker/making-online-games/tree/04f68edb04d9bc366f17f6123b189a6f577abb67/games-program-codes/simple-snake"
    in
    { title = "Load Project from Git Repository"
    , titleIcon = Just FontAwesome.Solid.cloudDownloadAlt
    , guideParagraphItems =
        [ Element.text "Load project files from a URL to a tree in a git repository. Here is an example of such a URL: "
        , linkElementFromUrlAndTextLabel { url = exampleUrl, labelText = exampleUrl }
        ]
    , contentElement =
        [ inputOrProgressElement
        , resultElement
        ]
            |> Element.column
                [ Element.width Element.fill
                , Element.spacing defaultFontSize
                ]
    }


viewExportToZipArchiveDialog : FileTreeInWorkspace.FileTreeNode -> PopupWindowAttributes Event
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
                    (Result.map FromZipArchiveProjectState
                        >> viewLoadOrImportDialogResultElement
                            dialogErrorElementFromDescription
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


elementToDisplayLoadFromGitError : Http.Detailed.Error String -> Element.Element msg
elementToDisplayLoadFromGitError =
    describeErrorLoadingContentsFromGit
        >> String.left 500
        >> dialogErrorElementFromDescription


dialogErrorElementFromDescription : String -> Element.Element e
dialogErrorElementFromDescription =
    Html.text
        >> Element.html
        >> Element.el [ Element.htmlAttribute (HA.style "white-space" "pre-wrap") ]
        >> List.singleton
        >> Element.paragraph [ Element.Font.color errorTextColor ]


projectSummaryElementForDialog : FileTreeInWorkspace.FileTreeNode -> Element.Element e
projectSummaryElementForDialog projectState =
    let
        projectFiles =
            FileTree.flatListOfBlobsFromFileTreeNode projectState

        projectStateHashBase16 =
            projectState
                |> FileTreeInWorkspace.mapBlobsToBytes
                |> Frontend.ProjectStateInUrl.projectStateCompositionHash
    in
    [ "The current project state has the hash code " ++ projectStateHashBase16
    , "It contains "
        ++ (projectFiles |> List.length |> String.fromInt)
        ++ " files with an aggregate size of "
        ++ (projectFiles |> List.map (Tuple.second >> .asBytes >> Bytes.width) |> List.sum |> String.fromInt)
        ++ " bytes."
    ]
        |> List.map (Element.text >> List.singleton >> Element.paragraph [])
        |> Element.textColumn []


viewLoadOrImportDialogResultElement :
    (err -> Element.Element e)
    -> (LoadOrImportProjectStateOrigin -> e)
    -> Result err LoadOrImportProjectStateOrigin
    -> Element.Element e
viewLoadOrImportDialogResultElement elementToDisplayFromError commitEvent loadCompositionResult =
    case loadCompositionResult of
        Err loadError ->
            elementToDisplayFromError loadError

        Ok loadOk ->
            let
                ( fileTree, compositionIdCache ) =
                    case loadOk of
                        FromZipArchiveProjectState fromZip ->
                            ( fromZip.fileTree, fromZip.compositionIdCache )

                        FromGitProjectState fromGit ->
                            ( fromGit.fileTree, fromGit.compositionIdCache )
            in
            [ [ ("Loaded composition "
                    ++ compositionIdCache
                    ++ " containing "
                    ++ (fileTree |> FileTree.flatListOfBlobsFromFileTreeNode |> List.length |> String.fromInt)
                    ++ " files:"
                )
                    |> Element.text
              ]
                |> Element.paragraph []
            , viewFileTree
                { selectEventFromNode = always (always Nothing)
                , iconFromFileName = Visuals.iconFromFileName
                }
                (sortFileTreeForExplorerView fileTree)
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
                , onPress = Just (commitEvent loadOk)
                }
                |> Element.el [ Element.centerX ]
            ]
                |> Element.column [ Element.spacing (defaultFontSize // 2) ]


describeErrorLoadingContentsFromGit : Http.Detailed.Error String -> String
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
            [ { icon = Visuals.ChatActionIcon, linkUrl = Just "https://github.com/elm-time/elm-time/discussions" }
            , { icon = Visuals.GitHubActionIcon, linkUrl = Just "https://github.com/elm-time/elm-time/tree/main/implement/example-apps/elm-editor" }
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
    case state.syntaxInspection of
        Just syntaxInspection ->
            let
                errorElement errorText =
                    [ Element.text "Error"
                    , monospaceTextWithScrollbarsElement errorText
                    ]
                        |> Element.column
                            [ Element.spacing defaultFontSize
                            , Element.Font.color errorTextColor
                            , Element.padding defaultFontSize
                            , Element.width Element.fill
                            , Element.height Element.fill
                            ]

                monospaceTextWithScrollbarsElement =
                    Html.text
                        >> Element.html
                        >> Element.el
                            [ Element.htmlAttribute (HA.style "white-space" "pre")
                            , Element.htmlAttribute attributeMonospaceFont
                            , Element.htmlAttribute (HA.style "line-height" "normal")
                            , Element.padding (defaultFontSize // 2)
                            ]
                        >> List.singleton
                        >> Element.column
                            [ Element.width Element.fill
                            , Element.height Element.fill
                            , Element.scrollbarX
                            , Element.scrollbarY
                            , Element.Background.color (Element.rgb 0.2 0.2 0.2)
                            ]

                mainContent =
                    case syntaxInspection.parseFileResult of
                        Err error ->
                            errorElement error

                        Ok ( fileContentString, Err parseErrors ) ->
                            parseErrors
                                |> List.map (CompileElmApp.parserDeadEndToString fileContentString)
                                |> String.join "\n\n"
                                |> errorElement

                        Ok ( _, Ok parsedFile ) ->
                            parsedFile
                                |> Elm.Syntax.File.encode
                                |> Json.Encode.encode 4
                                |> monospaceTextWithScrollbarsElement
                                |> List.singleton
                                |> Element.column
                                    [ Element.padding defaultFontSize
                                    , Element.width Element.fill
                                    , Element.height Element.fill
                                    ]
            in
            { mainContent = mainContent
            , header = Element.none
            }

        Nothing ->
            case state.compilation of
                Nothing ->
                    { mainContent =
                        if filePathOpenedInEditorFromWorkspace state == Nothing then
                            Element.none

                        else
                            [ "No compilation started. You can use the 'Compile' button to check program text for errors and see your app in action."
                                |> Element.text
                            ]
                                |> Element.paragraph [ Element.padding defaultFontSize ]
                    , header = Element.none
                    }

                Just compilation ->
                    let
                        continueWithInProgress () =
                            let
                                elmMakeRequestEntryPointFilePathAbs =
                                    compilation.origin.requestFromUser.workingDirectoryPath
                                        ++ compilation.origin.requestFromUser.entryPointFilePathFromWorkingDirectory
                            in
                            { mainContent =
                                [ Element.text
                                    ("Compiling module '"
                                        ++ String.join "/" elmMakeRequestEntryPointFilePathAbs
                                        ++ "' with inspection "
                                        ++ (if compilation.origin.requestFromUser.makeOptionDebug then
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
                    in
                    case compilation.loweringLastIteration of
                        LoweringComplete loweringComplete ->
                            case loweringComplete.elmMakeResult of
                                Nothing ->
                                    continueWithInProgress ()

                                Just elmMakeResult ->
                                    viewOutputPaneContentFromCompilationComplete
                                        state
                                        compilation
                                        loweringComplete
                                        elmMakeResult

                        LoweringContinue _ ->
                            continueWithInProgress ()


viewOutputPaneContentFromCompilationComplete :
    WorkingProjectStateStructure
    -> CompilationState
    -> LoweringCompleteStruct
    -> Result (Http.Detailed.Error String) ElmMakeResultStructure
    -> { mainContent : Element.Element WorkspaceEventStructure, header : Element.Element WorkspaceEventStructure }
viewOutputPaneContentFromCompilationComplete workspace compilation loweringComplete elmMakeResult =
    let
        elmMakeRequest =
            compilation.origin.requestFromUser

        elmMakeRequestFromCurrentState =
            prepareCompileForFileOpenedInEditor workspace

        currentFileContentIsStillSame =
            Just elmMakeRequest.files
                == (elmMakeRequestFromCurrentState |> Maybe.map (.requestFromUserIdentity >> .files))

        elmMakeRequestEntryPointFilePathAbs =
            elmMakeRequest.workingDirectoryPath
                ++ elmMakeRequest.entryPointFilePathFromWorkingDirectory

        warnAboutOutdatedCompilationText =
            if
                Just elmMakeRequestEntryPointFilePathAbs
                    /= filePathOpenedInEditorFromWorkspace workspace
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

        offerToggleInspection =
            case loweringComplete.loweringResult of
                Err _ ->
                    False

                Ok _ ->
                    case elmMakeResult of
                        Err _ ->
                            False

                        Ok elmMakeOk ->
                            elmMakeOk.compiledHtmlDocument /= Nothing

        ( toggleInspectionLabel, toggleInspectionEvent ) =
            if elmMakeRequest.makeOptionDebug then
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
                    if offerToggleInspection then
                        buttonElement { label = toggleInspectionLabel, onPress = Just toggleInspectionEvent }

                    else
                        Element.none

        outputElementFromPlainText outputText =
            [ outputText
                |> Html.text
                |> Element.html
            ]
                |> Element.paragraph
                    [ Element.htmlAttribute (HA.style "white-space" "pre-wrap")
                    , Element.htmlAttribute attributeMonospaceFont
                    ]

        elmMakeErrorElement elmMakeError =
            elmMakeError
                |> describeHttpError
                |> String.lines
                |> (++) [ "Error:" ]
                |> List.map (Element.text >> List.singleton >> Element.paragraph [])
                |> Element.textColumn [ Element.spacing 4, Element.padding defaultFontSize ]

        elmMakeResultElement =
            case elmMakeResult of
                Err elmMakeError ->
                    elmMakeErrorElement elmMakeError

                Ok elmMakeOk ->
                    let
                        continueWithProcessOutput () =
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
                                    , Element.padding (defaultFontSize // 2)
                                    ]
                    in
                    case elmMakeOk.compiledHtmlDocument of
                        Nothing ->
                            continueWithProcessOutput ()

                        Just compiledHtmlDocument ->
                            case loweringComplete.loweringResult of
                                Err _ ->
                                    continueWithProcessOutput ()

                                Ok _ ->
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

        compileResultElement =
            case loweringComplete.loweringResult of
                Err loweringError ->
                    let
                        loweringErrorElement =
                            case loweringError of
                                LoweringError failedLowering ->
                                    [ Element.paragraph []
                                        [ Element.text
                                            ("Failed to lower the program code files with "
                                                ++ String.fromInt (List.length failedLowering)
                                                ++ " errors:"
                                            )
                                        ]
                                    , failedLowering
                                        |> List.map (viewLoweringCompileError >> List.singleton >> Element.paragraph [])
                                        |> Element.column
                                            [ Element.spacing defaultFontSize
                                            , Element.width Element.fill
                                            ]
                                    , Element.paragraph []
                                        [ Element.text "Probably the response from 'elm make' below has more details on the error:"
                                        ]
                                    , elmMakeResultElement
                                    ]
                                        |> Element.column
                                            [ Element.spacing (defaultFontSize * 2)
                                            , Element.width Element.fill
                                            , Element.height Element.fill
                                            ]

                                DependencyLoweringError elmMakeError ->
                                    elmMakeErrorElement elmMakeError
                    in
                    loweringErrorElement
                        |> elementWithScrollbarY

                Ok _ ->
                    elmMakeResultElement
                        |> elementWithScrollbarY

        elementWithScrollbarY elementsToScroll =
            [ elementsToScroll
                |> Element.el
                    [ Element.width Element.fill
                    , Element.height Element.fill
                    , Element.scrollbarY
                    ]
            ]
                |> Element.row
                    [ Element.width Element.fill
                    , Element.height Element.fill

                    -- https://github.com/mdgriffith/elm-ui/issues/149#issuecomment-531480958
                    , Element.clip
                    , Element.htmlAttribute (HA.style "flex-shrink" "1")
                    ]
    in
    { mainContent = compileResultElement
    , header = warnAboutOutdatedOrOfferModifyCompilationElement
    }


viewLoweringCompileError : CompileElmApp.LocatedCompilationError -> Element.Element WorkspaceEventStructure
viewLoweringCompileError locatedLoweringError =
    case locatedLoweringError of
        CompileElmApp.LocatedInSourceFiles errorLocation loweringError ->
            let
                locationElement =
                    viewElmMakeErrorLocation
                        errorLocation.filePath
                        (compilationSyntaxRangeAsElmMakeReportRegion errorLocation.locationInModuleText)

                errorDescriptionElement =
                    -- Work around problem described at https://github.com/mdgriffith/elm-ui/issues/49#issuecomment-550229883
                    [ loweringError
                        |> loweringCompilationErrorDisplayText
                        |> Html.text
                        |> Element.html
                    ]
                        |> Element.paragraph
                            [ Element.htmlAttribute (HA.style "white-space" "pre-wrap")
                            , Element.htmlAttribute attributeMonospaceFont
                            ]
            in
            [ locationElement, errorDescriptionElement ]
                |> Element.column
                    [ Element.spacing (defaultFontSize // 2)
                    , Element.width Element.fill
                    ]


loweringCompilationErrorDisplayText : CompileElmApp.CompilationError -> String
loweringCompilationErrorDisplayText compilationError =
    case compilationError of
        CompileElmApp.MissingDependencyError missingDependency ->
            "Missing Dependency: "
                ++ (case missingDependency of
                        CompileElmApp.ElmMakeDependency elmMakeRequest ->
                            "Elm Make "
                                ++ (case elmMakeRequest.outputType of
                                        CompileElmApp.ElmMakeOutputTypeJs ->
                                            "javascript"

                                        CompileElmApp.ElmMakeOutputTypeHtml ->
                                            "html"
                                   )
                                ++ " from "
                                ++ String.join "/" elmMakeRequest.entryPointFilePath
                   )

        CompileElmApp.OtherCompilationError otherError ->
            otherError


viewElmMakeCompileError :
    FrontendBackendInterface.ElmMakeRequestStructure
    -> ElmMakeExecutableFile.ElmMakeReportCompileErrorStructure
    -> Element.Element WorkspaceEventStructure
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
                        , viewElmMakeErrorLocation displayPath elmMakeProblem.region
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


viewElmMakeErrorLocation : List String -> ElmMakeExecutableFile.ElmMakeReportRegion -> Element.Element WorkspaceEventStructure
viewElmMakeErrorLocation filePath regionInFile =
    (String.join "/" filePath
        ++ " - Line "
        ++ String.fromInt regionInFile.start.line
        ++ ", Column "
        ++ String.fromInt regionInFile.start.column
    )
        |> Element.text
        |> Element.el
            (Element.Events.onClick
                (UserInputRevealPositionInEditor
                    { filePath = filePath
                    , lineNumber = regionInFile.start.line
                    , column = regionInFile.start.column
                    }
                )
                :: elementLinkStyleAttributes
            )


compilationSyntaxRangeAsElmMakeReportRegion : Elm.Syntax.Range.Range -> ElmMakeExecutableFile.ElmMakeReportRegion
compilationSyntaxRangeAsElmMakeReportRegion range =
    { start = compilationSyntaxLocationAsElmMakeReportLocation range.start
    , end = compilationSyntaxLocationAsElmMakeReportLocation range.end
    }


compilationSyntaxLocationAsElmMakeReportLocation : Elm.Syntax.Range.Location -> ElmMakeExecutableFile.ElmMakeReportLocation
compilationSyntaxLocationAsElmMakeReportLocation location =
    { line = location.row, column = location.column }


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


editorDocumentMarkersFromFailedLowering :
    { compileRequest : ElmMakeRequestStructure, fileOpenedInEditor : ( List String, FileTreeInWorkspace.BlobNodeWithCache ) }
    -> List CompileElmApp.LocatedCompilationError
    -> List Frontend.MonacoEditor.EditorMarker
editorDocumentMarkersFromFailedLowering { compileRequest, fileOpenedInEditor } compileErrors =
    let
        filePathOpenedInEditor =
            Tuple.first fileOpenedInEditor

        fileOpenedInEditorBase64 =
            (Tuple.second fileOpenedInEditor).asBase64
    in
    case compileRequest.files |> List.filter (.path >> (==) filePathOpenedInEditor) |> List.head of
        Nothing ->
            []

        Just requestFile ->
            if requestFile.contentBase64 /= fileOpenedInEditorBase64 then
                []

            else
                compileErrors
                    |> List.filterMap
                        (\locatedCompilationError ->
                            case locatedCompilationError of
                                CompileElmApp.LocatedInSourceFiles location error ->
                                    if location.filePath == filePathOpenedInEditor then
                                        Just
                                            (editorDocumentMarkerFromLoweringCompileError
                                                ( compilationSyntaxRangeAsElmMakeReportRegion location.locationInModuleText
                                                , error
                                                )
                                            )

                                    else
                                        Nothing
                        )


editorDocumentMarkersFromElmMakeReport :
    { elmMakeRequest : ElmMakeRequestStructure, fileOpenedInEditor : ( List String, FileTreeInWorkspace.BlobNodeWithCache ) }
    -> Maybe (Result String ElmMakeExecutableFile.ElmMakeReportFromJson)
    -> List Frontend.MonacoEditor.EditorMarker
editorDocumentMarkersFromElmMakeReport { elmMakeRequest, fileOpenedInEditor } maybeReportFromJson =
    case maybeReportFromJson of
        Nothing ->
            []

        Just reportFromJson ->
            let
                filePathOpenedInEditor =
                    Tuple.first fileOpenedInEditor

                fileOpenedInEditorBase64 =
                    (Tuple.second fileOpenedInEditor).asBase64
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


editorDocumentMarkerFromLoweringCompileError : ( ElmMakeExecutableFile.ElmMakeReportRegion, CompileElmApp.CompilationError ) -> Frontend.MonacoEditor.EditorMarker
editorDocumentMarkerFromLoweringCompileError ( region, error ) =
    { message = loweringCompilationErrorDisplayText error
    , startLineNumber = region.start.line
    , startColumn = region.start.column
    , endLineNumber = region.end.line
    , endColumn = region.end.column
    , severity = Frontend.MonacoEditor.ErrorSeverity
    }


editorDocumentMarkerFromElmMakeProblem : ElmMakeExecutableFile.ElmMakeReportProblem -> Frontend.MonacoEditor.EditorMarker
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
    , severity = Frontend.MonacoEditor.ErrorSeverity
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


buttonCompile : Element.Element WorkspaceEventStructure
buttonCompile =
    buttonElement { label = "▶️ Compile", onPress = Just UserInputCompile }


buttonElement : { label : String, onPress : Maybe event } -> Element.Element event
buttonElement buttonConfig =
    Element.Input.button
        [ Element.Background.color buttonBackgroundColor.default
        , Element.mouseOver [ Element.Background.color buttonBackgroundColor.mouseOver ]
        , Element.paddingXY defaultFontSize (defaultFontSize // 2)
        ]
        { label = Element.text buttonConfig.label
        , onPress = buttonConfig.onPress
        }


buttonBackgroundColor : { default : Element.Color, mouseOver : Element.Color }
buttonBackgroundColor =
    { default = Element.rgb 0.055 0.388 0.612
    , mouseOver = Element.rgb 0.067 0.467 0.733
    }


titlebarMenuEntryButton : State -> TitlebarMenuEntry -> Element.Element Event
titlebarMenuEntryButton state menuEntry =
    let
        isOpen =
            state.popup == Just (TitlebarMenu menuEntry True)

        isHighlighted =
            state.popup == Just (TitlebarMenu menuEntry False) || isOpen

        greyFromLightness l =
            Element.rgb l l l

        entryBackgroundColor =
            if isHighlighted then
                greyFromLightness 0.3

            else
                greyFromLightness 0.2

        dropdownAttributes =
            if isOpen then
                [ titlebarMenuEntryDropdownContent state menuEntry
                    |> Element.below
                ]

            else
                []
    in
    Element.Input.button
        [ Element.Background.color entryBackgroundColor
        , Element.paddingXY defaultFontSize (defaultFontSize // 2)
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
                        UserInputLoadFromGitOpenDialog
                        (Just FontAwesome.Solid.cloudDownloadAlt)
                        "Load From Git Repository"
                        True
                    , titlebarMenuEntry
                        (UserInputImportProjectFromZipArchive ImportFromZipArchiveOpenDialog)
                        (Just FontAwesome.Solid.fileImport)
                        "Import From Zip Archive"
                        True
                    , titlebarMenuEntry
                        (UserInputGetLinkToProject { createDiffIfBaseAvailable = True })
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
                [ Element.mouseOver [ Element.Background.color (Element.rgb255 9 71 113) ] ]

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
    Frontend.MonacoEditor.SetValue
        >> CompilationInterface.GenerateJsonConverters.jsonEncodeMessageToMonacoEditor
        >> sendMessageToMonacoFrame


revealPositionInCenterInMonacoEditorCmd : { lineNumber : Int, column : Int } -> Cmd WorkspaceEventStructure
revealPositionInCenterInMonacoEditorCmd =
    Frontend.MonacoEditor.RevealPositionInCenter
        >> CompilationInterface.GenerateJsonConverters.jsonEncodeMessageToMonacoEditor
        >> sendMessageToMonacoFrame


setModelMarkersInMonacoEditorCmd : List Frontend.MonacoEditor.EditorMarker -> Cmd WorkspaceEventStructure
setModelMarkersInMonacoEditorCmd =
    Frontend.MonacoEditor.SetModelMarkers
        >> CompilationInterface.GenerateJsonConverters.jsonEncodeMessageToMonacoEditor
        >> sendMessageToMonacoFrame


provideCompletionItemsInMonacoEditorCmd : List Frontend.MonacoEditor.MonacoCompletionItem -> Cmd WorkspaceEventStructure
provideCompletionItemsInMonacoEditorCmd =
    Frontend.MonacoEditor.ProvideCompletionItemsEvent
        >> CompilationInterface.GenerateJsonConverters.jsonEncodeMessageToMonacoEditor
        >> sendMessageToMonacoFrame


provideHoverInMonacoEditorCmd : List String -> Cmd WorkspaceEventStructure
provideHoverInMonacoEditorCmd =
    Frontend.MonacoEditor.ProvideHoverEvent
        >> CompilationInterface.GenerateJsonConverters.jsonEncodeMessageToMonacoEditor
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


defaultProjectLink : String
defaultProjectLink =
    "https://github.com/onlinegamemaker/making-online-games/tree/04f68edb04d9bc366f17f6123b189a6f577abb67/games-program-codes/simple-snake"


initWorkspaceFromFileTreeAndFileSelection :
    { fileTree : FileTreeInWorkspace.FileTreeNode, filePathOpenedInEditor : Maybe (List String) }
    -> WorkingProjectStateStructure
initWorkspaceFromFileTreeAndFileSelection { fileTree, filePathOpenedInEditor } =
    { fileTree = fileTree
    , editing = { filePathOpenedInEditor = filePathOpenedInEditor }
    , decodeMessageFromMonacoEditorError = Nothing
    , lastTextReceivedFromEditor = Nothing
    , compilation = Nothing
    , syntaxInspection = Nothing
    , elmFormat = Nothing
    , viewEnlargedPane = Nothing
    , enableInspectionOnCompile = False
    , languageServiceState = LanguageService.initLanguageServiceState
    }


stringFromFileContent : Bytes.Bytes -> Maybe String
stringFromFileContent =
    Common.decodeBytesToString


fileContentFromString : String -> Bytes.Bytes
fileContentFromString =
    Bytes.Encode.string >> Bytes.Encode.encode


describeHttpError : Http.Detailed.Error String -> String
describeHttpError httpError =
    case httpError of
        Http.Detailed.BadUrl errorMessage ->
            "Bad Url: " ++ errorMessage

        Http.Detailed.Timeout ->
            "Timeout"

        Http.Detailed.NetworkError ->
            "Network Error"

        Http.Detailed.BadStatus metadata body ->
            [ "BadStatus: " ++ (metadata.statusCode |> String.fromInt)
            , "Response body: " ++ body
            ]
                |> String.join "\n"

        Http.Detailed.BadBody _ body errorMessage ->
            [ "BadPayload: " ++ errorMessage
            , "Response body: " ++ body
            ]
                |> String.join "\n"


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
    14


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


buildZipArchiveFromFileTree : FileTreeInWorkspace.FileTreeNode -> Zip.Zip
buildZipArchiveFromFileTree =
    FileTree.flatListOfBlobsFromFileTreeNode
        >> List.map
            (\( path, blobContent ) ->
                blobContent.asBytes
                    |> Zip.Entry.store
                        { path = String.join "/" path
                        , lastModified = ( Time.utc, Time.millisToPosix 0 )
                        , comment = Nothing
                        }
            )
        >> Zip.fromEntries


extractFileTreeFromZipArchive : Zip.Zip -> Result String FileTreeInWorkspace.FileTreeNode
extractFileTreeFromZipArchive =
    Zip.entries
        >> List.filter (Zip.Entry.isDirectory >> not)
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
        >> Result.map FileTreeInWorkspace.sortedFileTreeFromListOfBlobsAsBytes


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


errorTextColor : Element.Color
errorTextColor =
    Element.rgb 1 0.64 0
