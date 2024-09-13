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
import FontAwesome
import FontAwesome.Solid
import FontAwesome.Styles
import Frontend.BrowserApplicationInitWithTime as BrowserApplicationInitWithTime
import Frontend.ContainerHtml
import Frontend.FileEditor as FileEditor exposing (FileContentType(..))
import Frontend.MonacoEditor
import Frontend.Visuals as Visuals
import Frontend.WorkspaceStateInUrl
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
import LanguageServiceInterface
import List.Extra
import MonacoHtml
import Parser
import Result.Extra
import SHA256
import String.Extra
import Task
import Time
import Url
import Url.Builder
import WorkspaceState_2021_01
import Zip
import Zip.Entry


port sendMessageToMonacoFrame : Json.Encode.Value -> Cmd msg


port receiveMessageFromMonacoFrame : (Json.Encode.Value -> msg) -> Sub msg


port receiveMessageFromContainerHtml : (Json.Encode.Value -> msg) -> Sub msg


port sendRequestToLanguageService : Json.Encode.Value -> Cmd msg


port receiveResponseFromLanguageService : (Json.Encode.Value -> msg) -> Sub msg


type alias ElmMakeRequestStructure =
    FrontendBackendInterface.ElmMakeRequestStructure


type alias ElmMakeResponseStructure =
    FrontendBackendInterface.ElmMakeResponseStructure


type alias State =
    { navigationKey : Navigation.Key
    , url : Url.Url
    , time : Time.Posix
    , workspace : WorkspaceStateStruct
    , popup : Maybe PopupState
    , lastBackendLoadFromGitResult : Maybe ( String, Result (Http.Detailed.Error String) (Result String BackendLoadFromGitOkWithCache) )
    }


type WorkspaceStateStruct
    = WorkspaceActive WorkspaceActiveStruct
    | WorkspaceLoadingFromLink
        { workspaceStateDescription : WorkspaceState_2021_01.WorkspaceState
        , filePathToOpen : Maybe (List String)
        , expectedCompositionHash : Maybe String
        }
    | WorkspaceErr String


type alias WorkspaceActiveStruct =
    { fileTree : FileTreeInWorkspace.FileTreeNode
    , editing : { fileLocationOpenInEditor : Maybe ( List String, String ) }
    , decodeMessageFromMonacoEditorError : Maybe Json.Decode.Error
    , lastTextReceivedFromEditor : Maybe String
    , compilation : Maybe CompilationState
    , syntaxInspection : Maybe SyntaxInspectionState
    , elmFormat : Maybe ElmFormatState
    , viewEnlargedPane : Maybe WorkspacePane
    , enableInspectionOnCompile : Bool
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
    | UserInputLoadOrImportTakeWorkspaceStateEvent LoadOrImportWorkspaceStateOrigin
    | UserInputClosePopup
    | BackendLoadFromGitResultEvent String (Result (Http.Detailed.Error String) (Result String FrontendBackendInterface.LoadCompositionResponseStructure))
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | WorkspaceEvent WorkspaceEventStructure
    | UserInputGetLinkToWorkspace { createDiffIfBaseAvailable : Bool }
    | UserInputLoadedZipArchiveFile File.File
    | UserInputLoadedZipArchiveBytes Bytes.Bytes
    | UserInputImportWorkspaceFromZipArchive UserInputImportFromZipArchiveEventStructure
    | UserInputExportWorkspaceToZipArchive { sendDownloadCmd : Bool }
    | UserInputToggleTitleBarMenu TitlebarMenuEntry
    | UserInputMouseOverTitleBarMenu (Maybe TitlebarMenuEntry)
    | UserInputKeyDownEvent Keyboard.Event.KeyboardEvent
    | UserInputFocusOutsideTitlebarMenu
    | ReceiveMessageFromContainerHtmlEvent Json.Encode.Value
    | DiscardEvent


type WorkspaceEventStructure
    = MonacoEditorEvent Json.Decode.Value
    | UserInputChangeTextInEditor { uri : String } String
    | UserInputOpenFileInEditor ( List String, String )
    | UserInputFormat
    | UserInputCancelFormatting
    | UserInputCompile
    | UserInputInspectSyntax
    | UserInputCloseEditor
    | UserInputRevealPositionInEditor { fileLocation : ( List String, String ), lineNumber : Int, column : Int }
    | BackendElmFormatResponseEvent { fileLocation : ( List String, String ), result : Result (Http.Detailed.Error String) FrontendBackendInterface.FormatElmModuleTextResponseStructure }
    | BackendElmMakeResponseEvent ElmMakeRequestStructure (Result (Http.Detailed.Error String) ElmMakeResponseStructure)
    | UserInputSetEnlargedPane (Maybe WorkspacePane)
    | UserInputSetInspectionOnCompile Bool
    | ResponseFromLanguageServiceEvent Json.Encode.Value


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
    = GetLinkToWorkspaceDialog GetLinkToWorkspaceDialogState
    | LoadFromGitDialog LoadFromGitDialogState
    | ExportToZipArchiveDialog
    | ImportFromZipArchiveDialog ImportFromZipArchiveDialogState


type TitlebarMenuEntry
    = WorkspaceMenuEntry


type alias GetLinkToWorkspaceDialogState =
    { urlToWorkspace : String }


type alias LoadFromGitDialogState =
    { urlIntoGitRepository : String
    , request : Maybe { url : String, time : Time.Posix }
    , loadCompositionResult : Maybe (Result (Http.Detailed.Error String) (Result String BackendLoadFromGitOkWithCache))
    }


type alias ImportFromZipArchiveDialogState =
    { loadCompositionResult : Maybe (Result String ImportFromZipArchiveOk)
    }


type LoadOrImportWorkspaceStateOrigin
    = FromZipArchiveWorkspaceState ImportFromZipArchiveOk
    | FromGitWorkspaceState BackendLoadFromGitOkWithCache


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
    , receiveMessageFromContainerHtml ReceiveMessageFromContainerHtmlEvent
    , receiveResponseFromLanguageService (ResponseFromLanguageServiceEvent >> WorkspaceEvent)
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
            , updateBeginLoadDefaultIfWorkspaceErr
            ]


updateBeginLoadDefaultIfWorkspaceErr : State -> ( State, Cmd Event )
updateBeginLoadDefaultIfWorkspaceErr stateBefore =
    case stateBefore.workspace of
        WorkspaceErr _ ->
            updateToBeginLoadWorkspaceState
                { workspaceStateExpectedCompositionHash = Nothing
                , filePathToOpen = Just [ "src", "Main.elm" ]
                }
                stateBefore
                { base = defaultWorkspaceLink
                , differenceFromBase = WorkspaceState_2021_01.noDifference
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
                WorkspaceActive _ ->
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

        UserInputGetLinkToWorkspace generateLink ->
            case stateBefore.workspace of
                WorkspaceActive workspaceActive ->
                    let
                        urlToWorkspace =
                            setWorkspaceStateInUrlForBookmark
                                { createDiffIfBaseAvailable = generateLink.createDiffIfBaseAvailable }
                                workspaceActive
                                stateBefore
                                |> Url.toString

                        dialog =
                            { urlToWorkspace = urlToWorkspace }

                        cmd =
                            Navigation.pushUrl stateBefore.navigationKey urlToWorkspace
                    in
                    ( { stateBefore | popup = Just (ModalDialog (GetLinkToWorkspaceDialog dialog)) }, cmd )

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

        UserInputLoadOrImportTakeWorkspaceStateEvent origin ->
            let
                ( fileTree, urlToPush ) =
                    case origin of
                        FromZipArchiveWorkspaceState fromZip ->
                            ( fromZip.fileTree
                            , Url.Builder.absolute
                                (List.filter (String.isEmpty >> not) (String.split "/" stateBefore.url.path))
                                []
                            )

                        FromGitWorkspaceState fromGit ->
                            ( fromGit.fileTree
                            , stateBefore.url
                                |> Frontend.WorkspaceStateInUrl.setWorkspaceStateInUrl
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
                    , fileLocationOpenInEditor = Nothing
                    }
                        |> initWorkspaceFromFileTreeAndFileSelection
                        |> WorkspaceActive
              }
            , urlToPush |> Navigation.pushUrl stateBefore.navigationKey
            )

        UserInputExportWorkspaceToZipArchive { sendDownloadCmd } ->
            case stateBefore.workspace of
                WorkspaceActive workspaceActive ->
                    let
                        cmd =
                            if sendDownloadCmd then
                                let
                                    workspaceStateHash =
                                        FileTreeInWorkspace.compositionHashFromFileTreeNode workspaceActive.fileTree
                                in
                                workspaceActive.fileTree
                                    |> buildZipArchiveFromFileTree
                                    |> Zip.toBytes
                                    |> File.Download.bytes
                                        ("elm-app-" ++ SHA256.toHex workspaceStateHash ++ ".zip")
                                        "application/zip"

                            else
                                Cmd.none
                    in
                    ( { stateBefore | popup = Just (ModalDialog ExportToZipArchiveDialog) }, cmd )

                _ ->
                    ( stateBefore, Cmd.none )

        UserInputImportWorkspaceFromZipArchive ImportFromZipArchiveOpenDialog ->
            ( { stateBefore
                | popup = Just (ModalDialog (ImportFromZipArchiveDialog { loadCompositionResult = Nothing }))
              }
            , Cmd.none
            )

        UserInputImportWorkspaceFromZipArchive ImportFromZipArchiveSelectFile ->
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
                                        , compositionIdCache = Frontend.WorkspaceStateInUrl.workspaceStateCompositionHash (FileTreeInWorkspace.mapBlobsToBytes fileTree)
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
                WorkspaceActive workspaceBefore ->
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

                        setWorkspaceStateInUrlCmd =
                            if shouldUpdateUrl then
                                setWorkspaceStateInUrlForBookmark
                                    { createDiffIfBaseAvailable = True }
                                    workspaceBefore
                                    stateBefore
                                    |> Url.toString
                                    |> Navigation.pushUrl stateBefore.navigationKey

                            else
                                Cmd.none
                    in
                    ( { stateBefore | workspace = WorkspaceActive workspace }
                        |> mapForFocusOutsideTitlebarMenu
                    , [ Cmd.map WorkspaceEvent workspaceCmd
                      , setWorkspaceStateInUrlCmd
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

        ReceiveMessageFromContainerHtmlEvent messageJson ->
            case
                Json.Decode.decodeValue
                    CompilationInterface.GenerateJsonConverters.jsonDecodeMessageFromContainerHtml
                    messageJson
            of
                Err _ ->
                    ( stateBefore, Cmd.none )

                Ok message ->
                    case message of
                        Frontend.ContainerHtml.ClickedLinkInPreview clickedLink ->
                            updateUserClickedLinkInPreview clickedLink stateBefore

        DiscardEvent ->
            ( stateBefore, Cmd.none )


updateUserClickedLinkInPreview : { href : String } -> State -> ( State, Cmd Event )
updateUserClickedLinkInPreview { href } stateBefore =
    case Url.fromString href of
        Nothing ->
            ( stateBefore, Cmd.none )

        Just url ->
            if
                (url.host /= stateBefore.url.host)
                    && (String.toLower url.host /= "elm-editor.com")
            then
                ( stateBefore, Cmd.none )

            else
                case Frontend.WorkspaceStateInUrl.workspaceStateDescriptionFromUrl url of
                    Nothing ->
                        ( stateBefore, Cmd.none )

                    Just (Err _) ->
                        ( stateBefore, Cmd.none )

                    Just (Ok _) ->
                        processEventUrlChanged url stateBefore


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


setWorkspaceStateInUrlForBookmark : { createDiffIfBaseAvailable : Bool } -> WorkspaceActiveStruct -> State -> Url.Url
setWorkspaceStateInUrlForBookmark { createDiffIfBaseAvailable } workspaceActive state =
    let
        baseToUse =
            if not createDiffIfBaseAvailable then
                Nothing

            else
                state.lastBackendLoadFromGitResult
                    |> Maybe.andThen (Tuple.second >> Result.toMaybe)
                    |> Maybe.andThen Result.toMaybe

        filePathToOpen =
            case workspaceActive.editing.fileLocationOpenInEditor of
                Nothing ->
                    Nothing

                Just ( directoryPath, fileName ) ->
                    Just (directoryPath ++ [ fileName ])
    in
    state.url
        |> Frontend.WorkspaceStateInUrl.setWorkspaceStateInUrl
            workspaceActive.fileTree
            baseToUse
            { filePathToOpen = filePathToOpen }


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
    -> WorkspaceActiveStruct
    -> ( WorkspaceActiveStruct, Cmd WorkspaceEventStructure )
updateWorkspace updateConfig event stateBeforeApplyingEvent =
    let
        ( stateAfterApplyEvent, eventCmd ) =
            updateWorkspaceWithoutCmdToUpdateEditor updateConfig event stateBeforeApplyingEvent

        ( state, afterEventCmd ) =
            updateWorkspaceAfterEvent stateBeforeApplyingEvent stateAfterApplyEvent
    in
    ( state
    , Cmd.batch
        [ eventCmd
        , afterEventCmd
        ]
    )


updateWorkspaceAfterEvent :
    WorkspaceActiveStruct
    -> WorkspaceActiveStruct
    -> ( WorkspaceActiveStruct, Cmd WorkspaceEventStructure )
updateWorkspaceAfterEvent stateBeforeApplyEvent stateAfterEvent =
    case fileOpenedInEditorFromWorkspace stateAfterEvent of
        Nothing ->
            ( stateAfterEvent, Cmd.none )

        Just fileOpenInEditor ->
            let
                ( fileLocation, fileContent ) =
                    fileOpenInEditor
            in
            case stringFromFileContent fileContent.asBytes of
                Nothing ->
                    ( stateAfterEvent, Cmd.none )

                Just textForEditor ->
                    let
                        ( directoryPath, fileName ) =
                            fileLocation

                        fileLocationOpenedBeforeEvent =
                            fileLocationOpenInEditorFromWorkspace stateBeforeApplyEvent

                        fileLocationChanged =
                            fileLocationOpenedBeforeEvent /= Just fileLocation

                        compilationChanged =
                            stateBeforeApplyEvent.compilation /= stateAfterEvent.compilation

                        fileContentType : Maybe FileContentType
                        fileContentType =
                            FileEditor.fileContentTypeFromFileName fileName

                        setTextToEditorCmd : Maybe (Cmd WorkspaceEventStructure)
                        setTextToEditorCmd =
                            if Just textForEditor == state.lastTextReceivedFromEditor then
                                Nothing

                            else
                                Just
                                    (setContentInMonacoEditorCmd
                                        { text = textForEditor
                                        , language =
                                            Maybe.withDefault "txt"
                                                (Maybe.map monacoLanguageNameForFileContent fileContentType)
                                        , uri = monacoUriForFilePath directoryPath fileName
                                        }
                                    )

                        setModelMarkersToEditorCmd =
                            if not (compilationChanged || fileLocationChanged) then
                                Nothing

                            else
                                case stateAfterEvent.compilation of
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
                                                                        , fileOpenInEditor = fileOpenInEditor
                                                                        }

                                                    elmMakeMarkers =
                                                        loweringComplete.elmMakeResult
                                                            |> Maybe.andThen Result.toMaybe
                                                            |> Maybe.andThen .reportFromJson
                                                            |> editorDocumentMarkersFromElmMakeReport
                                                                { elmMakeRequest = loweringComplete.elmMakeRequest
                                                                , fileOpenInEditor = fileOpenInEditor
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

                        triggerCompileForFirstOpenedModule : Bool
                        triggerCompileForFirstOpenedModule =
                            (fileContentType == Just ElmContent)
                                && (stateAfterEvent.compilation == Nothing)
                                && (stateAfterEvent.syntaxInspection == Nothing)

                        ( state, compileCmd ) =
                            if triggerCompileForFirstOpenedModule then
                                compileFileOpenedInEditor stateAfterEvent

                            else
                                ( stateAfterEvent, Cmd.none )
                    in
                    ( state
                    , Cmd.batch
                        [ Maybe.withDefault Cmd.none setTextToEditorCmd
                        , Maybe.withDefault Cmd.none setModelMarkersToEditorCmd
                        , compileCmd
                        ]
                    )


monacoLanguageNameForFileContent : FileContentType -> String
monacoLanguageNameForFileContent fileContent =
    case fileContent of
        ElmContent ->
            "Elm"

        JsonContent ->
            "json"

        XmlContent ->
            "xml"

        MarkdownContent ->
            "markdown"

        HtmlContent ->
            "html"

        CssContent ->
            "css"


fileLocationOpenInEditorFromWorkspace : WorkspaceActiveStruct -> Maybe ( List String, String )
fileLocationOpenInEditorFromWorkspace =
    fileOpenedInEditorFromWorkspace >> Maybe.map Tuple.first


fileOpenedInEditorFromWorkspace :
    WorkspaceActiveStruct
    -> Maybe ( ( List String, String ), FileTreeInWorkspace.BlobNodeWithCache )
fileOpenedInEditorFromWorkspace workspaceActive =
    case workspaceActive.editing.fileLocationOpenInEditor of
        Nothing ->
            Nothing

        Just filePathOpenedInEditor ->
            let
                ( directoryPath, fileName ) =
                    filePathOpenedInEditor
            in
            case
                FileTree.getBlobAtPathFromFileTree
                    (directoryPath ++ [ fileName ])
                    workspaceActive.fileTree
            of
                Nothing ->
                    Nothing

                Just fileContent ->
                    Just ( filePathOpenedInEditor, fileContent )


updateWorkspaceWithoutCmdToUpdateEditor :
    { time : Time.Posix }
    -> WorkspaceEventStructure
    -> WorkspaceActiveStruct
    -> ( WorkspaceActiveStruct, Cmd WorkspaceEventStructure )
updateWorkspaceWithoutCmdToUpdateEditor updateConfig event stateBefore =
    case event of
        UserInputOpenFileInEditor fileLocation ->
            let
                ( directoryPath, fileName ) =
                    fileLocation
            in
            if FileTree.getBlobAtPathFromFileTree (directoryPath ++ [ fileName ]) stateBefore.fileTree == Nothing then
                ( stateBefore, Cmd.none )

            else
                ( let
                    editing =
                        stateBefore.editing
                  in
                  { stateBefore | editing = { editing | fileLocationOpenInEditor = Just fileLocation } }
                , Cmd.none
                )

        UserInputChangeTextInEditor editorScope inputText ->
            ( case stateBefore.editing.fileLocationOpenInEditor of
                Nothing ->
                    stateBefore

                Just ( directoryPath, fileName ) ->
                    let
                        filePath =
                            directoryPath ++ [ fileName ]

                        expectedUri =
                            monacoUriForFilePath directoryPath fileName
                    in
                    if editorScope.uri /= expectedUri then
                        stateBefore

                    else
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
            , if (stateBefore |> fileOpenedInEditorFromWorkspace |> Maybe.map Tuple.first) == Just revealPositionInEditor.fileLocation then
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
                            stateBefore
                                |> updateWorkspaceWithoutCmdToUpdateEditor
                                    updateConfig
                                    (UserInputChangeTextInEditor { uri = content.uri } content.textModelValue)

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
                            updateToProvideCompletionItemsRequest requestCompletionItems stateBefore

                        Frontend.MonacoEditor.RequestHoverEvent requestHoverEvent ->
                            updateToProvideHoverRequest requestHoverEvent stateBefore

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
                | editing = { editing | fileLocationOpenInEditor = Nothing }
                , elmFormat = Nothing
              }
            , Cmd.none
            )

        BackendElmFormatResponseEvent formatResponseEvent ->
            case stateBefore.elmFormat of
                Just (ElmFormatInProgress formatStart) ->
                    ( if Just formatResponseEvent.fileLocation /= stateBefore.editing.fileLocationOpenInEditor then
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

                                    ( directoryPath, fileName ) =
                                        formatResponseEvent.fileLocation

                                    filePath =
                                        directoryPath ++ [ fileName ]
                                in
                                { stateBefore
                                    | fileTree =
                                        stateBefore.fileTree
                                            |> FileTreeInWorkspace.setBlobAtPathInSortedFileTreeFromBytes
                                                ( filePath
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

        ResponseFromLanguageServiceEvent responseJson ->
            case
                Json.Decode.decodeValue
                    CompilationInterface.GenerateJsonConverters.jsonDecodeLanguageServiceResponse
                    responseJson
            of
                Err _ ->
                    ( stateBefore, Cmd.none )

                Ok responseWithId ->
                    case responseWithId.response of
                        Err _ ->
                            ( stateBefore, Cmd.none )

                        Ok response ->
                            let
                                cmd =
                                    case response of
                                        LanguageServiceInterface.ProvideHoverResponse provideHover ->
                                            provideHoverInMonacoEditorCmd provideHover

                                        LanguageServiceInterface.ProvideCompletionItemsResponse completionItems ->
                                            provideCompletionItemsInMonacoEditorCmd completionItems
                            in
                            ( stateBefore
                            , cmd
                            )


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


updateToProvideHoverRequest :
    Frontend.MonacoEditor.RequestHoverStruct
    -> WorkspaceActiveStruct
    -> ( WorkspaceActiveStruct, Cmd WorkspaceEventStructure )
updateToProvideHoverRequest request workspaceStateBefore =
    case workspaceStateBefore.editing.fileLocationOpenInEditor of
        Nothing ->
            ( workspaceStateBefore
            , Cmd.none
            )

        Just ( directoryPath, fileName ) ->
            updateForRequestToLanguageService
                (LanguageServiceInterface.ProvideHoverRequest
                    { filePathOpenedInEditor = directoryPath ++ [ fileName ]
                    , positionLineNumber = request.positionLineNumber
                    , positionColumn = request.positionColumn
                    , lineText = request.lineText
                    }
                )
                workspaceStateBefore


updateToProvideCompletionItemsRequest :
    Frontend.MonacoEditor.RequestCompletionItemsStruct
    -> WorkspaceActiveStruct
    -> ( WorkspaceActiveStruct, Cmd WorkspaceEventStructure )
updateToProvideCompletionItemsRequest request workspaceStateBefore =
    case workspaceStateBefore.editing.fileLocationOpenInEditor of
        Nothing ->
            ( workspaceStateBefore, Cmd.none )

        Just ( directoryPath, fileName ) ->
            updateForRequestToLanguageService
                (LanguageServiceInterface.ProvideCompletionItemsRequest
                    { filePathOpenedInEditor = directoryPath ++ [ fileName ]
                    , cursorLineNumber = request.cursorLineNumber
                    , textUntilPosition = request.textUntilPosition
                    }
                )
                workspaceStateBefore


updateForRequestToLanguageService :
    LanguageServiceInterface.Request
    -> WorkspaceActiveStruct
    -> ( WorkspaceActiveStruct, Cmd WorkspaceEventStructure )
updateForRequestToLanguageService request workspaceStateBefore =
    ( workspaceStateBefore
    , sendRequestToLanguageService
        (CompilationInterface.GenerateJsonConverters.jsonEncodeLanguageServiceRequestInWorkspace
            { id = ""
            , request =
                { workspace = workspaceForLangServiceRequest workspaceStateBefore
                , request = request
                }
            }
        )
    )


workspaceForLangServiceRequest : WorkspaceActiveStruct -> LanguageServiceInterface.FileTreeNode
workspaceForLangServiceRequest workspace =
    FileTree.mapBlobs
        (\{ asBytes, asBase64 } ->
            { asBase64 = asBase64
            , asText = Common.decodeBytesToString asBytes
            }
        )
        workspace.fileTree


processEventUrlChanged : Url.Url -> State -> ( State, Cmd Event )
processEventUrlChanged url stateBefore =
    let
        workspaceStateExpectedCompositionHash =
            Frontend.WorkspaceStateInUrl.workspaceStateHashFromUrl url

        filePathToOpen =
            Frontend.WorkspaceStateInUrl.filePathToOpenFromUrl url
                |> Maybe.map (String.split "/" >> List.concatMap (String.split "\\"))

        workspaceWithMatchingStateHashAlreadyLoaded () =
            case stateBefore.workspace of
                WorkspaceActive workspaceActive ->
                    Just
                        (Frontend.WorkspaceStateInUrl.workspaceStateCompositionHash
                            (FileTreeInWorkspace.mapBlobsToBytes workspaceActive.fileTree)
                        )
                        == workspaceStateExpectedCompositionHash

                _ ->
                    False

        continueWithDiffWorkspaceState =
            updateToBeginLoadWorkspaceState
                { workspaceStateExpectedCompositionHash = workspaceStateExpectedCompositionHash
                , filePathToOpen = filePathToOpen
                }
                stateBefore
    in
    case Frontend.WorkspaceStateInUrl.workspaceStateDescriptionFromUrl url of
        Nothing ->
            ( stateBefore, Cmd.none )

        Just (Err fromUrlError) ->
            ( { stateBefore
                | workspace = WorkspaceErr ("Failed to decode workspace state from URL: " ++ fromUrlError)
              }
            , Cmd.none
            )

        Just (Ok workspaceDescription) ->
            if workspaceWithMatchingStateHashAlreadyLoaded () then
                ( stateBefore, Cmd.none )

            else
                case workspaceDescription of
                    Frontend.WorkspaceStateInUrl.LiteralWorkspaceState fileTreeFromUrl ->
                        updateForLoadedWorkspaceState
                            { expectedCompositionHash = workspaceStateExpectedCompositionHash
                            , filePathToOpen = filePathToOpen
                            }
                            (FileTreeInWorkspace.mapBlobsFromBytes fileTreeFromUrl)
                            WorkspaceState_2021_01.noDifference
                            stateBefore

                    Frontend.WorkspaceStateInUrl.LinkWorkspaceState linkToWorkspaceState ->
                        continueWithDiffWorkspaceState
                            { base = linkToWorkspaceState
                            , differenceFromBase = WorkspaceState_2021_01.noDifference
                            }

                    Frontend.WorkspaceStateInUrl.DiffWorkspaceState_Version_2021_01 diffWorkspaceState ->
                        continueWithDiffWorkspaceState diffWorkspaceState


updateToBeginLoadWorkspaceState :
    { workspaceStateExpectedCompositionHash : Maybe String, filePathToOpen : Maybe (List String) }
    -> State
    -> WorkspaceState_2021_01.WorkspaceState
    -> ( State, Cmd Event )
updateToBeginLoadWorkspaceState { workspaceStateExpectedCompositionHash, filePathToOpen } stateBefore workspaceStateDescription =
    ( { stateBefore
        | workspace =
            WorkspaceLoadingFromLink
                { workspaceStateDescription = workspaceStateDescription
                , filePathToOpen = filePathToOpen
                , expectedCompositionHash = workspaceStateExpectedCompositionHash
                }
      }
    , loadFromGitCmd workspaceStateDescription.base
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
                WorkspaceLoadingFromLink workspaceLoadingFromLink ->
                    if urlIntoGitRepository == workspaceLoadingFromLink.workspaceStateDescription.base then
                        case resultWithFileTreeAndCache |> Result.Extra.unpack (describeHttpError >> Err) identity of
                            Ok loadOk ->
                                updateForLoadedWorkspaceState
                                    { expectedCompositionHash = workspaceLoadingFromLink.expectedCompositionHash
                                    , filePathToOpen = workspaceLoadingFromLink.filePathToOpen
                                    }
                                    loadOk.fileTree
                                    workspaceLoadingFromLink.workspaceStateDescription.differenceFromBase
                                    stateBefore

                            Err loadingError ->
                                ( { stateBefore | workspace = WorkspaceErr loadingError }
                                , Cmd.none
                                )

                    else
                        ( stateBefore, Cmd.none )

                _ ->
                    ( stateBefore, Cmd.none )


updateForLoadedWorkspaceState :
    { expectedCompositionHash : Maybe String, filePathToOpen : Maybe (List String) }
    -> FileTreeInWorkspace.FileTreeNode
    -> WorkspaceState_2021_01.WorkspaceStateDifference
    -> State
    -> ( State, Cmd Event )
updateForLoadedWorkspaceState config loadedBaseWorkspaceState workspaceStateDiff stateBefore =
    ( (case
        FileTreeInWorkspace.applyWorkspaceStateDifference_2021_01
            workspaceStateDiff
            (FileTreeInWorkspace.mapBlobsToBytes loadedBaseWorkspaceState)
       of
        Err error ->
            Err ("Failed to apply difference model to compute workspace state: " ++ error)

        Ok composedWorkspaceState ->
            let
                composedWorkspaceStateHashBase16 =
                    Frontend.WorkspaceStateInUrl.workspaceStateCompositionHash composedWorkspaceState

                fileLocationOpenInEditor =
                    case config.filePathToOpen of
                        Nothing ->
                            Nothing

                        Just filePathToOpen ->
                            case List.reverse filePathToOpen of
                                [] ->
                                    Nothing

                                fileName :: directoryPathReversed ->
                                    Just ( List.reverse directoryPathReversed, fileName )

                continueIfHashOk =
                    { stateBefore
                        | workspace =
                            { fileTree = FileTreeInWorkspace.mapBlobsFromBytes composedWorkspaceState
                            , fileLocationOpenInEditor = fileLocationOpenInEditor
                            }
                                |> initWorkspaceFromFileTreeAndFileSelection
                                |> WorkspaceActive
                    }
                        |> Ok
            in
            case config.expectedCompositionHash of
                Nothing ->
                    continueIfHashOk

                Just expectedCompositionHash ->
                    if composedWorkspaceStateHashBase16 == expectedCompositionHash then
                        continueIfHashOk

                    else
                        Err
                            ("Composed workspace state has hash "
                                ++ composedWorkspaceStateHashBase16
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


elmFormatCmdFromState : WorkspaceActiveStruct -> Maybe ( String, Cmd WorkspaceEventStructure )
elmFormatCmdFromState state =
    case fileOpenedInEditorFromWorkspace state of
        Nothing ->
            Nothing

        Just ( fileLocation, fileContent ) ->
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
                        (Result.map Tuple.second
                            >> (\result ->
                                    BackendElmFormatResponseEvent { fileLocation = fileLocation, result = result }
                               )
                        )
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


compileFileOpenedInEditor : WorkspaceActiveStruct -> ( WorkspaceActiveStruct, Cmd WorkspaceEventStructure )
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


syntaxInspectFileOpenedInEditor : WorkspaceActiveStruct -> ( WorkspaceActiveStruct, Cmd WorkspaceEventStructure )
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
    WorkspaceActiveStruct
    -> Maybe { requestFromUserIdentity : ElmMakeRequestStructure, compile : () -> CompilationState }
prepareCompileForFileOpenedInEditor workspace =
    case workspace.editing.fileLocationOpenInEditor of
        Nothing ->
            Nothing

        Just ( entryPointDirectoryPath, entryPointFileName ) ->
            let
                entryPointFilePath =
                    entryPointDirectoryPath ++ [ entryPointFileName ]

                filesBeforeLowering =
                    workspace.fileTree
                        |> FileTree.flatListOfBlobsFromFileTreeNode

                directoryContainsElmJson directoryPath =
                    filesBeforeLowering |> List.map Tuple.first |> List.member (directoryPath ++ [ "elm.json" ])

                workingDirectoryPath =
                    entryPointDirectoryPath
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
            [ Element.text "Loading workspace from "
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
            [ WorkspaceMenuEntry ]
                |> List.map (titlebarMenuEntryButton state)

        mainContent =
            case state.workspace of
                WorkspaceLoadingFromLink loadingWorkspaceStateFromLink ->
                    let
                        loadResult =
                            state.lastBackendLoadFromGitResult
                                |> Maybe.andThen
                                    (\( requestUrl, result ) ->
                                        if requestUrl == loadingWorkspaceStateFromLink.workspaceStateDescription.base then
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
                            if loadingWorkspaceStateFromLink.workspaceStateDescription.differenceFromBase == WorkspaceState_2021_01.noDifference then
                                loadingWorkspaceStateFromLink.expectedCompositionHash

                            else
                                Nothing
                    in
                    mainContentFromLoadingFromLink
                        { linkUrl = loadingWorkspaceStateFromLink.workspaceStateDescription.base
                        , progressOrResultElement = progressOrResultElement
                        , expectedCompositionHash = expectedCompositionHash
                        }

                WorkspaceActive workspaceActive ->
                    let
                        workspacePaneLayout paneProperties =
                            let
                                widthFillPortion =
                                    if workspaceActive.viewEnlargedPane == Just paneProperties.pane then
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
                              , toggleEnlargedPaneButton workspaceActive paneProperties.pane
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
                            case workspaceActive.editing.fileLocationOpenInEditor of
                                Nothing ->
                                    { editorPaneHeader =
                                        [ Element.text "Choose a file to open in the editor" ]
                                            |> Element.paragraph []
                                    , editorPaneContent =
                                        let
                                            selectEventFromFileTreeNode upperPath ( nodeName, nodeContent ) =
                                                case nodeContent of
                                                    FileTree.BlobNode _ ->
                                                        Just (UserInputOpenFileInEditor ( upperPath, nodeName ))

                                                    FileTree.TreeNode _ ->
                                                        Nothing
                                        in
                                        viewFileTree
                                            { selectEventFromNode = selectEventFromFileTreeNode
                                            , iconFromFileName = Visuals.iconFromFileName
                                            }
                                            (sortFileTreeForExplorerView workspaceActive.fileTree)
                                            |> Element.el
                                                [ Element.scrollbars
                                                , Element.width Element.fill
                                                , Element.height Element.fill
                                                , Element.padding defaultFontSize
                                                ]
                                    }

                                Just ( directoryPath, fileName ) ->
                                    let
                                        fileContentType : Maybe FileContentType
                                        fileContentType =
                                            FileEditor.fileContentTypeFromFileName fileName

                                        headerIconElementFromTypeAndColor maybeTypeAndColor =
                                            maybeTypeAndColor
                                                |> Maybe.map
                                                    (\( iconType, iconColor ) ->
                                                        Visuals.iconSvgElementFromIcon { color = iconColor } iconType
                                                    )
                                                |> Maybe.withDefault Element.none
                                                |> Element.el [ Element.width (Element.px defaultFontSize) ]

                                        filePathElement =
                                            let
                                                directorySeparatorIconElement =
                                                    headerIconElementFromTypeAndColor (Just ( Visuals.DirectoryCollapsedIcon, "white" ))

                                                fileIconElement =
                                                    fileName
                                                        |> Visuals.iconFromFileName
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
                                            (directoryPath |> List.map (elementFromPathSegmentText True))
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
                                                , Element.htmlAttribute (HA.title "Close")
                                                ]
                                                { label = headerIconElementFromTypeAndColor (Just ( Visuals.CloseEditorIcon, "white" ))
                                                , onPress = Just UserInputCloseEditor
                                                }

                                        buttonsElements : List (Element.Element WorkspaceEventStructure)
                                        buttonsElements =
                                            case fileContentType of
                                                Just ElmContent ->
                                                    [ buttonElement
                                                        { label =
                                                            Element.row
                                                                [ Element.spacing defaultFontSize
                                                                , Element.htmlAttribute (HA.title "Format")
                                                                ]
                                                                [ FontAwesome.Solid.indent
                                                                    |> FontAwesome.view
                                                                    |> Element.html
                                                                    |> Element.el []
                                                                , Element.text "Format"
                                                                ]
                                                        , onPress = Just UserInputFormat
                                                        }
                                                    , buttonCompile
                                                    ]

                                                _ ->
                                                    []

                                        headerElement =
                                            [ [ filePathElement, closeEditorElement ]
                                                |> Element.row
                                                    [ Element.width Element.fill
                                                    , Element.height Element.fill
                                                    , Element.spacing defaultFontSize
                                                    , Element.alignLeft
                                                    , Element.htmlAttribute (HA.style "user-select" "none")
                                                    ]
                                            , buttonsElements
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
                                            case workspaceActive.elmFormat of
                                                Nothing ->
                                                    Nothing

                                                Just elmFormat ->
                                                    let
                                                        buttonCancelFormat =
                                                            buttonElement
                                                                { label = Element.text "Cancel formatting"
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
                            viewOutputPaneContent workspaceActive
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

                WorkspaceErr workspaceStateError ->
                    [ ("Failed to load workspace state: " ++ String.left 1000 workspaceStateError)
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
                    , Element.height (Element.px 34)
                    , Element.Background.color (Element.rgb 0.09 0.09 0.09)
                    , Element.Font.color (Element.rgb 0.84 0.84 0.84)
                    ]

        popupWindowAttributes =
            case state.popup of
                Nothing ->
                    []

                Just (TitlebarMenu _ _) ->
                    []

                Just (ModalDialog (GetLinkToWorkspaceDialog dialog)) ->
                    case state.workspace of
                        WorkspaceActive workspaceActive ->
                            viewGetLinkToWorkspaceDialog dialog workspaceActive.fileTree
                                |> popupWindowElementAttributesFromAttributes

                        _ ->
                            []

                Just (ModalDialog (LoadFromGitDialog dialogState)) ->
                    viewLoadFromGitDialog dialogState
                        |> popupWindowElementAttributesFromAttributes

                Just (ModalDialog ExportToZipArchiveDialog) ->
                    case state.workspace of
                        WorkspaceActive workspaceActive ->
                            viewExportToZipArchiveDialog workspaceActive.fileTree
                                |> popupWindowElementAttributesFromAttributes

                        _ ->
                            []

                Just (ModalDialog (ImportFromZipArchiveDialog dialogState)) ->
                    viewImportFromZipArchiveDialog dialogState
                        |> popupWindowElementAttributesFromAttributes

        body =
            [ Element.html FontAwesome.Styles.css
            , titlebar
            , Element.none
                |> Element.el
                    [ Element.width Element.fill
                    , Element.height (Element.px 1)
                    , Element.Background.color (Element.rgb 0.17 0.17 0.17)
                    ]
            , [ activityBar
              , Element.none
                    |> Element.el
                        [ Element.width (Element.px 1)
                        , Element.height Element.fill
                        , Element.Background.color (Element.rgb 0.17 0.17 0.17)
                        ]
              , mainContent
              ]
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


toggleEnlargedPaneButton : WorkspaceActiveStruct -> WorkspacePane -> Element.Element WorkspaceEventStructure
toggleEnlargedPaneButton state pane =
    let
        ( icon, onPress, titleVerb ) =
            if state.viewEnlargedPane == Just pane then
                ( Visuals.ShrinkActionIcon
                , Nothing
                , "Shrink"
                )

            else if state.viewEnlargedPane == Nothing then
                ( Visuals.GrowActionIcon
                , Just pane
                , "Enlarge"
                )

            else
                ( Visuals.GrowActionIcon
                , Nothing
                , "Enlarge"
                )

        titleSubject =
            case pane of
                EditorPane ->
                    "Editor Pane"

                OutputPane ->
                    "Output Pane"

        title =
            titleVerb ++ " " ++ titleSubject

        iconSize =
            20
    in
    Element.Input.button
        [ Element.Background.color (Element.rgb 0.2 0.2 0.2)
        , Element.mouseOver [ Element.Background.color (Element.rgb 0.3 0.3 0.3) ]
        , Element.padding 4
        , Element.htmlAttribute (HA.title title)
        ]
        { label =
            Visuals.iconSvgElementFromIcon { color = "rgba(255,255,255,0.7)" } icon
                |> Element.el [ Element.width (Element.px iconSize), Element.height (Element.px iconSize) ]
        , onPress = Just (UserInputSetEnlargedPane onPress)
        }
        |> Element.el [ Element.alignRight ]


type alias PopupWindowAttributes event =
    { title : String
    , titleIcon : Maybe (FontAwesome.Icon FontAwesome.WithoutId)
    , guideParagraphItems : List (Element.Element event)
    , contentElement : Element.Element event
    }


viewGetLinkToWorkspaceDialog : GetLinkToWorkspaceDialogState -> FileTreeInWorkspace.FileTreeNode -> PopupWindowAttributes Event
viewGetLinkToWorkspaceDialog dialogState workspaceState =
    let
        linkElementFromUrl urlToWorkspace =
            let
                maybeDependencyUrl =
                    case
                        urlToWorkspace
                            |> Url.fromString
                            |> Maybe.andThen Frontend.WorkspaceStateInUrl.workspaceStateDescriptionFromUrl
                    of
                        Nothing ->
                            Nothing

                        Just (Err _) ->
                            Nothing

                        Just (Ok workspaceDescription) ->
                            case workspaceDescription of
                                Frontend.WorkspaceStateInUrl.LiteralWorkspaceState _ ->
                                    Nothing

                                Frontend.WorkspaceStateInUrl.LinkWorkspaceState link ->
                                    Just link

                                Frontend.WorkspaceStateInUrl.DiffWorkspaceState_Version_2021_01 diffWorkspaceState ->
                                    Just diffWorkspaceState.base

                dependenciesDescriptionLines =
                    maybeDependencyUrl
                        |> Maybe.map
                            (\dependencyUrl ->
                                [ [ Element.text "The workspace state in this link depends on loading related data from the following URL: "
                                  , linkElementFromUrlAndTextLabel { url = dependencyUrl, labelText = dependencyUrl }
                                  ]
                                    |> Element.paragraph []
                                ]
                            )
                        |> Maybe.withDefault []

                linkDescriptionLines =
                    Element.paragraph [] [ Element.text ("Length of this link: " ++ String.fromInt (String.length urlToWorkspace)) ]
                        :: dependenciesDescriptionLines
            in
            [ linkElementWithWrappedLabel
                { url = urlToWorkspace, labelText = urlToWorkspace }
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
            [ [ "Use the link below to load your workspace's current state again later. This link is a fast way to share your workspace with other people."
                    |> Element.text
              ]
                |> Element.paragraph []
            , linkElementFromUrl dialogState.urlToWorkspace
            ]
                |> Element.column [ Element.spacing defaultFontSize ]
    in
    { title = "Get Link to Workspace for Bookmarking or Sharing"
    , titleIcon = Just FontAwesome.Solid.bookmark
    , guideParagraphItems =
        [ Element.text "Get a link to save or share your workspace's current state." ]
    , contentElement =
        [ workspaceSummaryElementForDialog workspaceState
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
                { label = Element.text "Begin Loading"
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
                        >> Result.map FromGitWorkspaceState
                        >> viewLoadOrImportDialogResultElement
                            dialogErrorElementFromDescription
                            UserInputLoadOrImportTakeWorkspaceStateEvent
                    )
                |> Maybe.withDefault Element.none

        exampleUrl =
            "https://github.com/onlinegamemaker/making-online-games/tree/6838f7100dd86c8c8afcfe3efd553f8fa39c77ae/games-program-codes/simple-snake"
    in
    { title = "Load Workspace from Git Repository"
    , titleIcon = Just FontAwesome.Solid.cloudDownloadAlt
    , guideParagraphItems =
        [ Element.text "Load workspace files from a public git repository URL. Here is an example of such a URL: "
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
viewExportToZipArchiveDialog workspaceState =
    let
        buttonDownloadArchive =
            buttonElement
                { label = Element.text "Download Archive"
                , onPress = Just (UserInputExportWorkspaceToZipArchive { sendDownloadCmd = True })
                }
    in
    { title = "Export Workspace to Zip Archive"
    , titleIcon = Just FontAwesome.Solid.fileExport
    , guideParagraphItems =
        [ Element.text "Download a zip archive containing all files in your workspace. You can also use this archive with the 'Import from Zip Archive' function to load your workspace into the editor again later." ]
    , contentElement =
        [ workspaceSummaryElementForDialog workspaceState
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
                { label = Element.text "Select zip archive file"
                , onPress = Just (UserInputImportWorkspaceFromZipArchive ImportFromZipArchiveSelectFile)
                }
                |> Element.el [ Element.centerX ]

        resultElement =
            dialogState.loadCompositionResult
                |> Maybe.map
                    (Result.map FromZipArchiveWorkspaceState
                        >> viewLoadOrImportDialogResultElement
                            dialogErrorElementFromDescription
                            UserInputLoadOrImportTakeWorkspaceStateEvent
                    )
                |> Maybe.withDefault Element.none
    in
    { title = "Import Workspace from Zip Archive"
    , titleIcon = Just FontAwesome.Solid.fileImport
    , guideParagraphItems =
        [ Element.text "Load workspace files from a zip archive. Here you can select a zip archive file from your system to load as the workspace state." ]
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


workspaceSummaryElementForDialog : FileTreeInWorkspace.FileTreeNode -> Element.Element e
workspaceSummaryElementForDialog workspaceState =
    let
        workspaceFiles =
            FileTree.flatListOfBlobsFromFileTreeNode workspaceState

        workspaceStateHashBase16 =
            workspaceState
                |> FileTreeInWorkspace.mapBlobsToBytes
                |> Frontend.WorkspaceStateInUrl.workspaceStateCompositionHash
    in
    [ "The current workspace state has the hash code " ++ workspaceStateHashBase16
    , "It contains "
        ++ (workspaceFiles |> List.length |> String.fromInt)
        ++ " files with an aggregate size of "
        ++ (workspaceFiles |> List.map (Tuple.second >> .asBytes >> Bytes.width) |> List.sum |> String.fromInt)
        ++ " bytes."
    ]
        |> List.map (Element.text >> List.singleton >> Element.paragraph [])
        |> Element.textColumn []


viewLoadOrImportDialogResultElement :
    (err -> Element.Element e)
    -> (LoadOrImportWorkspaceStateOrigin -> e)
    -> Result err LoadOrImportWorkspaceStateOrigin
    -> Element.Element e
viewLoadOrImportDialogResultElement elementToDisplayFromError commitEvent loadCompositionResult =
    case loadCompositionResult of
        Err loadError ->
            elementToDisplayFromError loadError

        Ok loadOk ->
            let
                ( fileTree, compositionIdCache ) =
                    case loadOk of
                        FromZipArchiveWorkspaceState fromZip ->
                            ( fromZip.fileTree, fromZip.compositionIdCache )

                        FromGitWorkspaceState fromGit ->
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
                { label = Element.text "Set these files as workspace state"
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
    48


activityBar : Element.Element event
activityBar =
    let
        actionItemSpacing =
            4

        actionItemIconPadding =
            8

        bottomActionItems =
            [ { icon = Visuals.ChatActionIcon
              , linkUrl = Just "https://github.com/pine-vm/pine/discussions"
              , title = "Elm Editor Discussions"
              }
            , { icon = Visuals.GitHubActionIcon
              , linkUrl = Just "https://github.com/pine-vm/pine/tree/main/implement/example-apps/elm-editor"
              , title = "Elm Editor source code on GitHub"
              }
            ]

        actionItemWrapper itemConfig =
            let
                linkWrapper =
                    case itemConfig.linkUrl of
                        Nothing ->
                            identity

                        Just justLinkUrl ->
                            \linkLabel ->
                                Element.link
                                    [ Element.width Element.fill
                                    , Element.htmlAttribute (HA.title itemConfig.title)
                                    ]
                                    { url = justLinkUrl, label = linkLabel }
            in
            Visuals.iconSvgElementFromIcon { color = "white" } itemConfig.icon
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
            , Element.Background.color (Element.rgb 0.09 0.09 0.09)
            ]


popupWindowElementAttributesFromAttributes : PopupWindowAttributes Event -> List (Element.Attribute Event)
popupWindowElementAttributesFromAttributes { title, titleIcon, guideParagraphItems, contentElement } =
    [ [ [ titleIcon
            |> Maybe.map (FontAwesome.view >> Element.html >> Element.el [])
            |> Maybe.withDefault Element.none
        , title |> Element.text
        , Element.Input.button
            [ Element.mouseOver [ Element.Background.color (Element.rgba 1 1 1 0.2) ]
            , Element.padding 3
            , Element.alignRight
            , Element.htmlAttribute (HA.title "Close")
            ]
            { label =
                FontAwesome.Solid.xmark
                    |> FontAwesome.view
                    |> Element.html
                    |> Element.el []
            , onPress = Just UserInputClosePopup
            }
        ]
            |> Element.row
                (Element.spacing defaultFontSize
                    :: Element.width Element.fill
                    :: headingAttributes 3
                )
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
            , Element.Border.width 1
            , Element.Border.rounded 8
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
    WorkspaceActiveStruct
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
                        if fileLocationOpenInEditorFromWorkspace state == Nothing then
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
    WorkspaceActiveStruct
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

        warnAboutOutdatedCompilationText : Maybe String
        warnAboutOutdatedCompilationText =
            case fileLocationOpenInEditorFromWorkspace workspace of
                Nothing ->
                    Nothing

                Just ( directoryPath, fileName ) ->
                    let
                        filePathOpenedInEditor =
                            directoryPath ++ [ fileName ]
                    in
                    if elmMakeRequestEntryPointFilePathAbs /= filePathOpenedInEditor then
                        Just
                            ("Last compilation started for another file: '"
                                ++ String.join "/" elmMakeRequestEntryPointFilePathAbs
                                ++ "'"
                            )

                    else if currentFileContentIsStillSame then
                        Nothing

                    else
                        Just "File contents changed since compiling"

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

        ( ( toggleInspectionLabelIcon, toggleInspectionLabelText ), toggleInspectionEvent ) =
            if elmMakeRequest.makeOptionDebug then
                ( ( Nothing, "Disable Inspection" )
                , UserInputSetInspectionOnCompile False
                )

            else
                ( ( Just FontAwesome.Solid.magnifyingGlass, "Enable Inspection" )
                , UserInputSetInspectionOnCompile True
                )

        warnAboutOutdatedOrOfferModifyCompilationElement =
            case warnAboutOutdatedCompilationText of
                Just warnText ->
                    [ FontAwesome.Solid.triangleExclamation
                        |> FontAwesome.view
                        |> Element.html
                        |> Element.el []
                    , Element.text " "
                    , warnText
                        |> Element.text
                    ]
                        |> Element.paragraph
                            [ Element.padding (defaultFontSize // 2)
                            , Element.Background.color (Element.rgb 0.4 0.1 0)
                            , Element.width Element.fill
                            , Element.transparent (warnAboutOutdatedCompilationText == Nothing)
                            , Element.htmlAttribute (HA.style "user-select" "none")
                            ]

                Nothing ->
                    if offerToggleInspection then
                        buttonElement
                            { label =
                                Element.row
                                    [ Element.spacing defaultFontSize ]
                                    [ toggleInspectionLabelIcon
                                        |> Maybe.map
                                            (FontAwesome.view
                                                >> Element.html
                                                >> Element.el []
                                            )
                                        |> Maybe.withDefault Element.none
                                    , Element.text toggleInspectionLabelText
                                    ]
                            , onPress = Just toggleInspectionEvent
                            }

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

                        Just htmlDocumentFromElmMake ->
                            let
                                htmlDocumentForEmbedding =
                                    compileHtmlDocumentForEmbedding
                                        { htmlFromElmMake = htmlDocumentFromElmMake }
                            in
                            case loweringComplete.loweringResult of
                                Err _ ->
                                    continueWithProcessOutput ()

                                Ok _ ->
                                    Html.iframe
                                        [ HA.srcdoc htmlDocumentForEmbedding
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


monacoUriForFilePath : List String -> String -> String
monacoUriForFilePath directoryPath fileName =
    "file:///" ++ String.join "/" (directoryPath ++ [ fileName ])


compileHtmlDocumentForEmbedding : { htmlFromElmMake : String } -> String
compileHtmlDocumentForEmbedding { htmlFromElmMake } =
    let
        script =
            """
            <script>
            document.addEventListener('DOMContentLoaded', function() {
                document.body.addEventListener('click', function(e) {
                    if (e.target.tagName === 'A') {
                        e.preventDefault();
                        window.parent.postMessage({
                            type: 'clicked-link-in-iframe',
                            href: e.target.getAttribute('href')
                        }, '*'); // Be mindful of the security implications of '*'
                    }
                }, false);
            });
            </script>
            """

        closingBodyTag =
            "</body>"

        newBodyTag =
            String.join "\n" [ script, closingBodyTag ]
    in
    String.replace closingBodyTag newBodyTag htmlFromElmMake


viewLoweringCompileError : CompileElmApp.LocatedCompilationError -> Element.Element WorkspaceEventStructure
viewLoweringCompileError locatedLoweringError =
    case locatedLoweringError of
        CompileElmApp.LocatedInSourceFiles errorLocation loweringError ->
            case List.reverse errorLocation.filePath of
                [] ->
                    Element.none

                fileName :: directoryPathReversed ->
                    let
                        locationElement =
                            viewElmMakeErrorLocation
                                ( List.reverse directoryPathReversed, fileName )
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

                    displayPathLocation =
                        case List.reverse displayPath of
                            [] ->
                                ( [], "error parsing - empty path" )

                            fileName :: directoryPathReversed ->
                                ( List.reverse directoryPathReversed, fileName )

                    problemHeadingElement =
                        [ elmMakeProblem.title
                            |> elmEditorProblemDisplayTitleFromReportTitle
                            |> Element.text
                            |> Element.el [ Element.Font.bold ]
                        , viewElmMakeErrorLocation displayPathLocation elmMakeProblem.region
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


viewElmMakeErrorLocation : ( List String, String ) -> ElmMakeExecutableFile.ElmMakeReportRegion -> Element.Element WorkspaceEventStructure
viewElmMakeErrorLocation fileLocation regionInFile =
    let
        ( directoryPath, fileName ) =
            fileLocation

        filePath =
            directoryPath ++ [ fileName ]
    in
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
                    { fileLocation = fileLocation
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
    { compileRequest : ElmMakeRequestStructure
    , fileOpenInEditor : ( ( List String, String ), FileTreeInWorkspace.BlobNodeWithCache )
    }
    -> List CompileElmApp.LocatedCompilationError
    -> List Frontend.MonacoEditor.EditorMarker
editorDocumentMarkersFromFailedLowering { compileRequest, fileOpenInEditor } compileErrors =
    let
        ( directoryPath, fileName ) =
            Tuple.first fileOpenInEditor

        filePathOpenInEditor =
            directoryPath ++ [ fileName ]

        fileOpenedInEditorBase64 =
            (Tuple.second fileOpenInEditor).asBase64
    in
    case compileRequest.files |> List.filter (.path >> (==) filePathOpenInEditor) |> List.head of
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
                                    if location.filePath == filePathOpenInEditor then
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
    { elmMakeRequest : ElmMakeRequestStructure
    , fileOpenInEditor : ( ( List String, String ), FileTreeInWorkspace.BlobNodeWithCache )
    }
    -> Maybe (Result String ElmMakeExecutableFile.ElmMakeReportFromJson)
    -> List Frontend.MonacoEditor.EditorMarker
editorDocumentMarkersFromElmMakeReport { elmMakeRequest, fileOpenInEditor } maybeReportFromJson =
    case maybeReportFromJson of
        Nothing ->
            []

        Just reportFromJson ->
            let
                ( directoryPath, fileName ) =
                    Tuple.first fileOpenInEditor

                filePathOpenInEditor =
                    directoryPath ++ [ fileName ]

                fileOpenedInEditorBase64 =
                    (Tuple.second fileOpenInEditor).asBase64
            in
            case elmMakeRequest.files |> List.filter (.path >> (==) filePathOpenInEditor) |> List.head of
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
                                                    >> (==) filePathOpenInEditor
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
    buttonElement
        { label =
            Element.row
                [ Element.spacing defaultFontSize
                , Element.htmlAttribute (HA.title "Compile")
                ]
                [ FontAwesome.Solid.playCircle
                    |> FontAwesome.view
                    |> Element.html
                    |> Element.el []
                , Element.text "Compile"
                ]
        , onPress = Just UserInputCompile
        }


buttonElement : { label : Element.Element event, onPress : Maybe event } -> Element.Element event
buttonElement buttonConfig =
    Element.Input.button
        [ Element.Background.color buttonBackgroundColor.default
        , Element.mouseOver [ Element.Background.color buttonBackgroundColor.mouseOver ]
        , Element.paddingXY defaultFontSize (defaultFontSize // 2)
        , Element.Border.rounded 2
        ]
        { label = buttonConfig.label
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

        highlightColor =
            greyFromLightness 0.18

        entryBackgroundColor =
            if isHighlighted then
                highlightColor

            else
                Element.rgba 0 0 0 0

        dropdownAttributes =
            if isOpen then
                [ Element.below
                    (titlebarMenuEntryDropdownContent state menuEntry)
                ]

            else
                []
    in
    Element.Input.button
        [ Element.Background.color entryBackgroundColor
        , Element.paddingXY (defaultFontSize // 2) 5
        , Element.Events.onMouseEnter (UserInputMouseOverTitleBarMenu (Just menuEntry))
        , Element.Events.onMouseLeave (UserInputMouseOverTitleBarMenu Nothing)
        , Element.Border.rounded 4
        , Element.centerY
        , Element.mouseOver [ Element.Background.color highlightColor ]
        ]
        { label = Element.text (titlebarMenuEntryLabel menuEntry)
        , onPress = Just (UserInputToggleTitleBarMenu menuEntry)
        }
        |> Element.el dropdownAttributes


titlebarMenuEntryDropdownContent : State -> TitlebarMenuEntry -> Element.Element Event
titlebarMenuEntryDropdownContent state menuEntry =
    let
        canSaveWorkspace =
            case state.workspace of
                WorkspaceActive _ ->
                    True

                _ ->
                    False

        menuEntries =
            case menuEntry of
                WorkspaceMenuEntry ->
                    [ titlebarMenuEntry
                        UserInputLoadFromGitOpenDialog
                        (Just FontAwesome.Solid.cloudDownloadAlt)
                        "Load From Git Repository"
                        True
                    , titlebarMenuEntry
                        (UserInputImportWorkspaceFromZipArchive ImportFromZipArchiveOpenDialog)
                        (Just FontAwesome.Solid.fileImport)
                        "Import From Zip Archive"
                        True
                    , titlebarMenuEntry
                        (UserInputGetLinkToWorkspace { createDiffIfBaseAvailable = True })
                        (Just FontAwesome.Solid.bookmark)
                        "Get Link for Bookmarking or Sharing"
                        canSaveWorkspace
                    , titlebarMenuEntry
                        (UserInputExportWorkspaceToZipArchive { sendDownloadCmd = False })
                        (Just FontAwesome.Solid.fileExport)
                        "Export To Zip Archive"
                        canSaveWorkspace
                    ]
    in
    menuEntries
        |> Element.column
            [ Element.spacing 2
            , Element.paddingXY 4 4
            , Element.width Element.shrink
            ]
        |> Element.el
            [ Element.Background.color (Element.rgb 0.13 0.13 0.13)
            , Element.Border.color (Element.rgb 0.27 0.27 0.27)
            , Element.Border.solid
            , Element.Border.width 1
            , Element.Border.rounded 6
            ]


titlebarMenuEntry : Event -> Maybe (FontAwesome.Icon FontAwesome.WithoutId) -> String -> Bool -> Element.Element Event
titlebarMenuEntry onPressEventIfEnabled maybeIcon label isEnabled =
    let
        mouseOverAttributes =
            if isEnabled then
                [ Element.mouseOver [ Element.Background.color (Element.rgb255 4 57 94) ] ]

            else
                []

        opacity =
            if isEnabled then
                1

            else
                0.5

        cursor =
            if isEnabled then
                "pointer"

            else
                "default"
    in
    Element.Input.button
        [ Element.width Element.fill
        , Element.paddingXY defaultFontSize 4
        , Element.htmlAttribute (HA.style "cursor" cursor)
        ]
        { label =
            [ maybeIcon
                |> Maybe.map
                    (FontAwesome.view
                        >> Element.html
                        >> Element.el [ Element.centerX ]
                    )
                |> Maybe.withDefault Element.none
                |> Element.el [ Element.width (Element.px (defaultFontSize * 6 // 5)) ]
            , Element.text label
            ]
                |> Element.row
                    [ Element.spacing (defaultFontSize // 2)
                    , Element.alpha opacity
                    ]
        , onPress =
            if isEnabled then
                Just onPressEventIfEnabled

            else
                Nothing
        }
        |> Element.el
            (Element.width Element.fill
                :: Element.Border.rounded 4
                :: mouseOverAttributes
            )


titlebarMenuEntryLabel : TitlebarMenuEntry -> String
titlebarMenuEntryLabel menuEntry =
    case menuEntry of
        WorkspaceMenuEntry ->
            "Workspace"


setContentInMonacoEditorCmd : { text : String, language : String, uri : String } -> Cmd WorkspaceEventStructure
setContentInMonacoEditorCmd content =
    Frontend.MonacoEditor.SetContent
        { value = content.text
        , language = content.language
        , uri = content.uri
        }
        |> CompilationInterface.GenerateJsonConverters.jsonEncodeMessageToMonacoEditor
        |> sendMessageToMonacoFrame


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
        [ HA.srcdoc MonacoHtml.monacoHtmlDocument
        , HA.id "monaco-iframe"
        , HA.style "width" "100%"
        , HA.style "height" "100%"
        , HA.style "border" "0"
        ]
        []
        |> Element.html


defaultWorkspaceLink : String
defaultWorkspaceLink =
    "https://github.com/onlinegamemaker/making-online-games/tree/6838f7100dd86c8c8afcfe3efd553f8fa39c77ae/implement/landing-app"


initWorkspaceFromFileTreeAndFileSelection :
    { fileTree : FileTreeInWorkspace.FileTreeNode, fileLocationOpenInEditor : Maybe ( List String, String ) }
    -> WorkspaceActiveStruct
initWorkspaceFromFileTreeAndFileSelection { fileTree, fileLocationOpenInEditor } =
    { fileTree = fileTree
    , editing = { fileLocationOpenInEditor = fileLocationOpenInEditor }
    , decodeMessageFromMonacoEditorError = Nothing
    , lastTextReceivedFromEditor = Nothing
    , compilation = Nothing
    , syntaxInspection = Nothing
    , elmFormat = Nothing
    , viewEnlargedPane = Nothing
    , enableInspectionOnCompile = False
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
    Element.rgb 0.12 0.12 0.12


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
