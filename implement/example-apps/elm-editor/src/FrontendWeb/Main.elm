port module FrontendWeb.Main exposing (Event(..), State, defaultProject, init, main, receiveMessageFromMonacoFrame, sendMessageToMonacoFrame, update, view)

import Base64
import Browser
import Browser.Navigation as Navigation
import Bytes
import Bytes.Decode
import Bytes.Encode
import Common
import Dict
import Element
import Element.Background
import Element.Border
import Element.Events
import Element.Font
import Element.Input
import Element.Region
import ElmFullstackCompilerInterface.GenerateJsonCoders
import ElmFullstackCompilerInterface.SourceFiles
import FrontendBackendInterface
import FrontendWeb.MonacoEditor
import FrontendWeb.ProjectStateInUrl
import Html
import Html.Attributes as HA
import Html.Events
import Http
import Json.Decode
import Json.Encode
import Maybe.Extra
import ProjectState
import ProjectState_2021_01
import Result.Extra
import Svg
import Svg.Attributes
import Time
import Url
import Url.Builder


port sendMessageToMonacoFrame : Json.Encode.Value -> Cmd msg


port receiveMessageFromMonacoFrame : (Json.Encode.Value -> msg) -> Sub msg


type alias ElmMakeRequestStructure =
    { requestToBackend : FrontendBackendInterface.ElmMakeRequestStructure
    , entryPointFilePath : List String
    }


type alias ElmMakeResponseStructure =
    FrontendBackendInterface.ElmMakeResponseStructure


type alias State =
    { navigationKey : Navigation.Key
    , url : Url.Url
    , time : Maybe Time.Posix
    , workspace : WorkspaceStateStructure
    , decodeMessageFromMonacoEditorError : Maybe Json.Decode.Error
    , lastTextReceivedFromEditor : Maybe String
    , lastElmMakeRequest : Maybe { time : Time.Posix, request : ElmMakeRequestStructure }
    , elmMakeResult : Maybe ( ElmMakeRequestStructure, Result Http.Error ElmMakeResultStructure )
    , elmFormatResult : Maybe (Result Http.Error FrontendBackendInterface.FormatElmModuleTextResponseStructure)
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
    }


type alias ElmMakeResultStructure =
    { response : ElmMakeResponseStructure
    , compiledHtmlDocument : Maybe String
    }


type Event
    = UserInputChangeTextInEditor String
    | MonacoEditorEvent Json.Decode.Value
    | TimeHasArrived Time.Posix
    | UserInputOpenFileInEditor (List String)
    | UserInputFormat
    | UserInputCompile
    | UserInputSave (Maybe { createDiffIfBaseAvailable : Bool })
    | UserInputLoadFromGit UserInputLoadFromGitEventStructure
    | UserInputCloseModalDialog
    | BackendElmFormatResponseEvent { filePath : List String, result : Result Http.Error FrontendBackendInterface.FormatElmModuleTextResponseStructure }
    | BackendElmMakeResponseEvent ElmMakeRequestStructure (Result Http.Error ElmMakeResponseStructure)
    | BackendLoadFromGitResultEvent String (Result Http.Error FrontendBackendInterface.LoadCompositionResponseStructure)
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | DiscardEvent


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


main : Program () State Event
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }


subscriptions : State -> Sub Event
subscriptions _ =
    [ receiveMessageFromMonacoFrame MonacoEditorEvent
    , Time.every 500 TimeHasArrived
    ]
        |> Sub.batch


init : () -> Url.Url -> Navigation.Key -> ( State, Cmd Event )
init _ url navigationKey =
    let
        ( model, urlChangeCmd ) =
            { navigationKey = navigationKey
            , url = url
            , time = Nothing
            , workspace = WorkspaceOk defaultProject
            , decodeMessageFromMonacoEditorError = Nothing
            , lastTextReceivedFromEditor = Nothing
            , lastElmMakeRequest = Nothing
            , elmMakeResult = Nothing
            , elmFormatResult = Nothing
            , modalDialog = Nothing
            , lastBackendLoadFromGitResult = Nothing
            }
                |> update (UrlChange url)
    in
    ( model, urlChangeCmd )


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    let
        ( stateBeforeConsiderCompile, cmd ) =
            updateWithoutCmdToUpdateEditor event stateBefore

        textForEditor =
            stateBeforeConsiderCompile.workspace
                |> mapWorkspaceIfOk
                    (\workspace ->
                        case workspace.editing.filePathOpenedInEditor of
                            Nothing ->
                                Nothing

                            Just filePath ->
                                workspace.fileTree |> ProjectState.getBlobAtPathFromFileTree filePath
                    )
                |> Maybe.Extra.join
                |> Maybe.andThen stringFromFileContent
                |> Maybe.withDefault "Failed to map file content to string."

        setTextToEditorCmd =
            if Just textForEditor == state.lastTextReceivedFromEditor then
                Cmd.none

            else
                setTextInMonacoEditorCmd textForEditor

        triggerCompile =
            case filePathOpenedInEditorFromState stateBeforeConsiderCompile of
                Nothing ->
                    False

                Just filePathOpenedInEditor ->
                    Just filePathOpenedInEditor /= filePathOpenedInEditorFromState stateBefore

        ( state, compileCmd ) =
            if triggerCompile then
                userInputCompileFileOpenedInEditor stateBeforeConsiderCompile

            else
                ( stateBeforeConsiderCompile, Cmd.none )
    in
    ( state, Cmd.batch [ cmd, setTextToEditorCmd, compileCmd ] )


filePathOpenedInEditorFromState : State -> Maybe (List String)
filePathOpenedInEditorFromState state =
    case state.workspace of
        WorkspaceOk workingState ->
            workingState.editing.filePathOpenedInEditor

        _ ->
            Nothing


updateWithoutCmdToUpdateEditor : Event -> State -> ( State, Cmd Event )
updateWithoutCmdToUpdateEditor event stateBefore =
    case event of
        UserInputOpenFileInEditor filePath ->
            ( stateBefore
                |> updateWorkspaceIfOk
                    (\workspace ->
                        let
                            editing =
                                workspace.editing
                        in
                        { workspace | editing = { editing | filePathOpenedInEditor = Just filePath } }
                    )
            , Cmd.none
            )

        UserInputChangeTextInEditor inputText ->
            ( { stateBefore | lastTextReceivedFromEditor = Just inputText }
                |> updateWorkspaceIfOk
                    (\workspace ->
                        case workspace.editing.filePathOpenedInEditor of
                            Nothing ->
                                workspace

                            Just filePath ->
                                { workspace
                                    | fileTree =
                                        workspace.fileTree
                                            |> ProjectState.setBlobAtPathInSortedFileTree ( filePath, fileContentFromString inputText )
                                }
                    )
            , Cmd.none
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
                            stateBefore |> update (UserInputChangeTextInEditor content)

                        FrontendWeb.MonacoEditor.CompletedSetupEvent ->
                            ( { stateBefore | lastTextReceivedFromEditor = Nothing }, Cmd.none )

                        FrontendWeb.MonacoEditor.EditorActionCloseFileEvent ->
                            ( stateBefore
                                |> updateWorkspaceIfOk
                                    (\workspace ->
                                        let
                                            editing =
                                                workspace.editing
                                        in
                                        { workspace | editing = { editing | filePathOpenedInEditor = Nothing } }
                                    )
                            , Cmd.none
                            )

                        FrontendWeb.MonacoEditor.EditorActionFormatDocumentEvent ->
                            update UserInputFormat stateBefore

                        FrontendWeb.MonacoEditor.EditorActionCompileEvent ->
                            userInputCompileFileOpenedInEditor stateBefore

        TimeHasArrived time ->
            ( { stateBefore | time = Just time }, Cmd.none )

        UserInputFormat ->
            ( stateBefore, elmFormatCmd stateBefore )

        UserInputCompile ->
            userInputCompileFileOpenedInEditor stateBefore

        BackendElmFormatResponseEvent formatResponseEvent ->
            ( { stateBefore | elmFormatResult = Just formatResponseEvent.result }
                |> updateWorkspaceIfOk
                    (\workspace ->
                        if Just formatResponseEvent.filePath /= workspace.editing.filePathOpenedInEditor then
                            workspace

                        else
                            case formatResponseEvent.result |> Result.toMaybe |> Maybe.andThen .formattedText of
                                Nothing ->
                                    workspace

                                Just formattedText ->
                                    { workspace
                                        | fileTree =
                                            workspace.fileTree
                                                |> ProjectState.setBlobAtPathInSortedFileTree
                                                    ( formatResponseEvent.filePath
                                                    , fileContentFromString formattedText
                                                    )
                                    }
                    )
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
                                in
                                { response = elmMakeResponse
                                , compiledHtmlDocument = compiledHtmlDocument
                                }
                            )
            in
            ( { stateBefore | elmMakeResult = Just ( elmMakeRequest, elmMakeResult ) }
            , Cmd.none
            )

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
                            , requestTime = stateBefore.time
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
                                    WorkspaceOk
                                        { fileTree = fileTreeNodeFromListFileWithPath loadOk.filesAsFlatList
                                        , editing = { filePathOpenedInEditor = Nothing }
                                        }
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

        DiscardEvent ->
            ( stateBefore, Cmd.none )


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
            if projectWithMatchingStateHashAlreadyLoaded then
                ( stateBefore, Cmd.none )

            else
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

        Just (Ok (FrontendWeb.ProjectStateInUrl.LiteralProjectState projectState)) ->
            updateForLoadedProjectState
                { expectedCompositionHash = projectStateExpectedCompositionHash
                , filePathToOpen = filePathToOpen
                }
                projectState
                ProjectState_2021_01.noDifference
                stateBefore

        Just (Ok (FrontendWeb.ProjectStateInUrl.LinkProjectState linkToProjectState)) ->
            continueWithDiffProjectState
                { base = linkToProjectState
                , differenceFromBase = ProjectState_2021_01.noDifference
                }

        Just (Ok (FrontendWeb.ProjectStateInUrl.DiffProjectState diffProjectState)) ->
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
                            WorkspaceOk
                                { fileTree = composedProjectState, editing = { filePathOpenedInEditor = config.filePathToOpen } }
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


elmFormatCmd : State -> Cmd Event
elmFormatCmd state =
    state.workspace
        |> mapWorkspaceIfOk
            (\workspace ->
                case workspace.editing.filePathOpenedInEditor of
                    Nothing ->
                        Cmd.none

                    Just filePath ->
                        case
                            workspace.fileTree
                                |> ProjectState.getBlobAtPathFromFileTree filePath
                                |> Maybe.andThen stringFromFileContent
                        of
                            Nothing ->
                                Cmd.none

                            Just fileContent ->
                                let
                                    request =
                                        fileContent |> FrontendBackendInterface.FormatElmModuleTextRequest

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
            )
        |> Maybe.withDefault Cmd.none


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


userInputCompileFileOpenedInEditor : State -> ( State, Cmd Event )
userInputCompileFileOpenedInEditor stateBefore =
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
                | lastElmMakeRequest =
                    stateBefore.time |> Maybe.map (\time -> { time = time, request = elmMakeRequest })
                , elmMakeResult = Nothing
              }
            , requestToApiCmd
                (FrontendBackendInterface.ElmMakeRequest elmMakeRequest.requestToBackend)
                jsonDecoder
                (BackendElmMakeResponseEvent elmMakeRequest)
            )


elmMakeRequestForFileOpenedInEditor : State -> Maybe ElmMakeRequestStructure
elmMakeRequestForFileOpenedInEditor state =
    state.workspace
        |> mapWorkspaceIfOk
            (\workspace ->
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
                            { requestToBackend =
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
                            , entryPointFilePath = entryPointFilePath
                            }
            )
        >> Maybe.Extra.join


requestToApiCmd :
    FrontendBackendInterface.RequestStructure
    -> (FrontendBackendInterface.ResponseStructure -> Json.Decode.Decoder event)
    -> (Result Http.Error event -> Event)
    -> Cmd Event
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


priorityToOfferToOpenFileInEditor : ( List String, String ) -> Maybe Int
priorityToOfferToOpenFileInEditor ( filePath, _ ) =
    if filePath == [ "elm.json" ] then
        Just 0

    else if filePath |> List.reverse |> List.head |> Maybe.map (String.endsWith ".elm") |> Maybe.withDefault False then
        Just 1

    else
        Nothing


sortFilesIntoPrioritiesToOfferToOpenFileInEditor : List ( List String, Bytes.Bytes ) -> List ( List String, String )
sortFilesIntoPrioritiesToOfferToOpenFileInEditor projectFiles =
    projectFiles
        |> List.filterMap
            (\( filePath, fileContent ) ->
                case stringFromFileContent fileContent of
                    Nothing ->
                        Nothing

                    Just fileContentString ->
                        case priorityToOfferToOpenFileInEditor ( filePath, fileContentString ) of
                            Nothing ->
                                Nothing

                            Just priority ->
                                Just ( priority, ( filePath, fileContentString ) )
            )
        |> List.sortBy (Tuple.first >> negate)
        |> List.map Tuple.second


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

        mainContentFromNavigationButtonElementsAndContent config =
            [ config.buttonElements
                |> Element.row
                    [ Element.spacing defaultFontSize
                    , Element.padding (defaultFontSize // 2)
                    ]
            , config.contentElement
            ]
                |> Element.column
                    [ Element.spacing (defaultFontSize // 2)
                    , Element.width (Element.fillPortion 4)
                    , Element.height Element.fill
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
                    case workingState.editing.filePathOpenedInEditor of
                        Nothing ->
                            let
                                projectFiles =
                                    workingState.fileTree |> ProjectState.flatListOfBlobsFromFileTreeNode

                                filesToOfferToOpenInEditor =
                                    projectFiles |> sortFilesIntoPrioritiesToOfferToOpenFileInEditor

                                filesToOfferToOpenInEditorNames =
                                    filesToOfferToOpenInEditor |> List.map Tuple.first

                                otherFilesList =
                                    case
                                        filesToOfferToOpenInEditorNames
                                            |> List.foldl Dict.remove (Dict.fromList projectFiles)
                                            |> Dict.toList
                                    of
                                        [] ->
                                            Element.none

                                        otherFilesInTheProject ->
                                            [ Element.text ("There are " ++ (String.fromInt (List.length otherFilesInTheProject) ++ " other files in this project:"))
                                            , otherFilesInTheProject
                                                |> List.map (\( filePath, _ ) -> Element.text (String.join "/" filePath))
                                                |> Element.column [ Element.spacing 4, Element.padding 8 ]
                                            ]
                                                |> Element.column []

                                chooseElmFileElement =
                                    case filesToOfferToOpenInEditor of
                                        [] ->
                                            Element.text "Did not find any .elm file in this project."

                                        _ ->
                                            [ Element.text "Choose one of the files in the project to open in the editor:"
                                            , filesToOfferToOpenInEditor
                                                |> List.map
                                                    (\( filePath, _ ) ->
                                                        Element.Input.button
                                                            [ Element.mouseOver [ Element.Background.color (Element.rgb 0 0.5 0.8) ]
                                                            ]
                                                            { label = Element.text (String.join "/" filePath)
                                                            , onPress = Just (UserInputOpenFileInEditor filePath)
                                                            }
                                                    )
                                                |> Element.column [ Element.spacing 4, Element.padding 8 ]
                                            ]
                                                |> Element.column []
                            in
                            mainContentFromNavigationButtonElementsAndContent
                                { buttonElements = [ saveButton, loadFromGitOpenDialogButton ]
                                , contentElement =
                                    [ chooseElmFileElement, otherFilesList ]
                                        |> Element.column
                                            [ Element.spacing defaultFontSize
                                            , Element.centerX
                                            , Element.centerY
                                            ]
                                }

                        Just filePathOpenedInEditor ->
                            viewWhenEditorOpen filePathOpenedInEditor state

                WorkspaceErr projectStateError ->
                    mainContentFromNavigationButtonElementsAndContent
                        { buttonElements = [ loadFromGitOpenDialogButton ]
                        , contentElement =
                            [ ("Failed to load project state: "
                                ++ (projectStateError |> String.left 500)
                              )
                                |> Element.text
                            ]
                                |> Element.paragraph
                                    [ Element.Font.color (Element.rgb 1 0.64 0)
                                    , Element.padding defaultFontSize
                                    , Element.width Element.fill
                                    ]
                        }

        popupAttributes =
            case state.modalDialog of
                Nothing ->
                    []

                Just (SaveOrShareDialog saveOrShareDialog) ->
                    case state.workspace of
                        WorkspaceOk workingState ->
                            viewSaveOrShareDialog saveOrShareDialog workingState.fileTree
                                |> popupAttributesFromProperties

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
                    popupAttributesFromProperties
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


type alias PopupAttributes =
    { title : String
    , guide : String
    , contentElement : Element.Element Event
    }


viewSaveOrShareDialog : SaveOrShareDialogState -> ProjectState.FileTreeNode -> PopupAttributes
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


updateWorkspaceIfOk : (WorkingProjectStateStructure -> WorkingProjectStateStructure) -> State -> State
updateWorkspaceIfOk map stateBefore =
    stateBefore.workspace
        |> mapWorkspaceIfOk map
        |> Maybe.map (\workspace -> { stateBefore | workspace = WorkspaceOk workspace })
        |> Maybe.withDefault stateBefore


mapWorkspaceIfOk : (WorkingProjectStateStructure -> a) -> WorkspaceStateStructure -> Maybe a
mapWorkspaceIfOk map projectState =
    case projectState of
        WorkspaceOk workspace ->
            Just (map workspace)

        _ ->
            Nothing


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
            [ { icon = ChatActionIcon, linkUrl = Just "https://github.com/elm-fullstack/elm-fullstack/discussions" }
            , { icon = GitHubActionIcon, linkUrl = Just "https://github.com/elm-fullstack/elm-fullstack/tree/main/implement/example-apps/elm-editor" }
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
            actionIconSvgElementFromIcon icon
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


type ActionIcon
    = ChatActionIcon
    | GitHubActionIcon


actionIconSvgElementFromIcon : ActionIcon -> Element.Element event
actionIconSvgElementFromIcon iconType =
    let
        pathsElements =
            actionIconSvgPathsData iconType
                |> List.map
                    (\pathInfo ->
                        let
                            fillAttributes =
                                if pathInfo.fillNone then
                                    [ Svg.Attributes.fill "none" ]

                                else
                                    []
                        in
                        Svg.path (Svg.Attributes.d pathInfo.pathData :: fillAttributes) []
                    )
    in
    Svg.svg
        [ Svg.Attributes.viewBox ([ 0, 0, 24, 24 ] |> List.map String.fromInt |> String.join " ")
        , Svg.Attributes.fill "white"
        ]
        pathsElements
        |> Element.html


actionIconSvgPathsData : ActionIcon -> List { pathData : String, fillNone : Bool }
actionIconSvgPathsData icon =
    case icon of
        ChatActionIcon ->
            -- https://github.com/google/material-design-icons/tree/96206ade0e8325ac4c4ce9d49dc4ef85241689e1/src/communication/chat_bubble
            [ { pathData = "M20 2H4c-1.1 0-2 .9-2 2v18l4-4h14c1.1 0 2-.9 2-2V4c0-1.1-.9-2-2-2z"
              , fillNone = False
              }
            ]

        GitHubActionIcon ->
            -- https://github.com/microsoft/vscode-codicons/tree/e1155a851abafe070be17d36996474f4f374741f/src/icons
            [ { pathData = "M12 1a11 11 0 1 0 0 22 11 11 0 0 0 0-22zm2.9 19.968h-.086a.471.471 0 0 1-.35-.129.471.471 0 0 1-.129-.34v-1.29c.006-.428.01-.86.01-1.297a3.385 3.385 0 0 0-.139-.943 1.679 1.679 0 0 0-.496-.802 7.34 7.34 0 0 0 1.868-.432 3.715 3.715 0 0 0 1.344-.883c.373-.392.65-.864.81-1.381.196-.632.289-1.29.276-1.952a3.797 3.797 0 0 0-.24-1.353 3.569 3.569 0 0 0-.727-1.177c.068-.172.118-.351.148-.534a3.286 3.286 0 0 0-.036-1.262 4.87 4.87 0 0 0-.203-.7.269.269 0 0 0-.102-.018h-.1c-.21.002-.419.037-.618.102-.22.064-.436.144-.645.239a5.97 5.97 0 0 0-.606.314 9.992 9.992 0 0 0-.525.332 8.78 8.78 0 0 0-4.714 0 12.367 12.367 0 0 0-.525-.332 5.52 5.52 0 0 0-.616-.314 4.14 4.14 0 0 0-.646-.239 2.02 2.02 0 0 0-.607-.102h-.1a.266.266 0 0 0-.1.019 5.356 5.356 0 0 0-.213.699 3.441 3.441 0 0 0-.027 1.262c.03.183.079.362.147.534a3.565 3.565 0 0 0-.726 1.177 3.797 3.797 0 0 0-.24 1.353 6.298 6.298 0 0 0 .266 1.942c.167.517.443.992.811 1.391.38.386.838.687 1.344.883.598.23 1.225.377 1.863.437-.178.161-.32.36-.414.58-.09.219-.153.448-.184.682a2.524 2.524 0 0 1-1.077.248 1.639 1.639 0 0 1-.976-.276 2.661 2.661 0 0 1-.69-.755 2.914 2.914 0 0 0-.267-.35 2.459 2.459 0 0 0-.34-.314 1.687 1.687 0 0 0-.397-.22 1.1 1.1 0 0 0-.441-.093.942.942 0 0 0-.11.01c-.05 0-.1.006-.148.018a.376.376 0 0 0-.12.055.107.107 0 0 0-.054.091.304.304 0 0 0 .129.222c.084.068.155.12.212.157l.026.019c.123.094.24.196.35.305.104.09.197.192.276.303.083.108.154.226.212.349.067.123.138.264.212.424.172.434.478.802.874 1.05.415.223.882.334 1.353.322.16 0 .32-.01.48-.028.156-.025.313-.052.47-.083v1.598a.459.459 0 0 1-.488.477h-.057a9.428 9.428 0 1 1 5.797 0v.005z"
              , fillNone = False
              }
            ]


popupAttributesFromProperties : { title : String, guide : String, contentElement : Element.Element Event } -> List (Element.Attribute Event)
popupAttributesFromProperties { title, guide, contentElement } =
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


viewWhenEditorOpen : List String -> State -> Element.Element Event
viewWhenEditorOpen filePathOpenedInEditor state =
    let
        elmMakeResultForFileOpenedInEditor =
            state.elmMakeResult
                |> Maybe.andThen
                    (\elmMakeRequestAndResult ->
                        if (elmMakeRequestAndResult |> Tuple.first |> .entryPointFilePath) == filePathOpenedInEditor then
                            Just elmMakeRequestAndResult

                        else
                            Nothing
                    )

        resultElement =
            case elmMakeResultForFileOpenedInEditor of
                Nothing ->
                    case state.lastElmMakeRequest of
                        Nothing ->
                            [ "No compilation started so far. You can use the 'Compile' button to check program text for errors and see your app in action."
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
                                    Just elmMakeRequest.requestToBackend.files
                                        == (elmMakeRequestFromCurrentState |> Maybe.map (.requestToBackend >> .files))

                                warningFileContentChangedElement =
                                    "⚠️ The file contents changed since compiling"
                                        |> Element.text
                                        |> Element.el
                                            [ Element.transparent currentFileContentIsStillSame
                                            , Element.padding (defaultFontSize // 2)
                                            ]

                                compileResultElement =
                                    case elmMakeOk.compiledHtmlDocument of
                                        Nothing ->
                                            [ ( "standard error", elmMakeOk.response.processOutput.standardError )
                                            , ( "standard output", elmMakeOk.response.processOutput.standardOutput )
                                            ]
                                                |> List.map
                                                    (\( channel, output ) ->
                                                        [ channel |> Element.text |> Element.el (headingAttributes 3)
                                                        , [ Html.text output
                                                                |> Element.html
                                                                |> Element.el [ Element.htmlAttribute (HA.style "white-space" "pre-wrap") ]
                                                          ]
                                                            |> Element.paragraph
                                                                [ Element.htmlAttribute attributeMonospaceFont ]
                                                            |> indentOneLevel
                                                        ]
                                                            |> Element.column
                                                                [ Element.spacing (defaultFontSize // 2)
                                                                , Element.width Element.fill
                                                                ]
                                                    )
                                                |> Element.column
                                                    [ Element.spacing defaultFontSize
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
                            [ warningFileContentChangedElement, compileResultElement ]
                                |> Element.column
                                    [ Element.spacing (defaultFontSize // 2)
                                    , Element.width Element.fill
                                    , Element.height Element.fill
                                    ]

        formatButton =
            buttonElement { label = "📄 Format", onPress = Just UserInputFormat }

        compileButton =
            buttonElement { label = "▶️ Compile", onPress = Just UserInputCompile }
    in
    [ [ [ saveButton, formatButton ]
            |> Element.row
                [ Element.spacing defaultFontSize
                , Element.padding (defaultFontSize // 2)
                ]
      , monacoEditorElement state
      ]
        |> Element.column
            [ Element.spacing (defaultFontSize // 2)
            , Element.width (Element.fillPortion 4)
            , Element.height Element.fill
            ]
    , [ [ compileButton ] |> Element.row [ Element.padding (defaultFontSize // 2) ]
      , resultElement
            |> Element.el
                [ Element.width Element.fill
                , Element.height Element.fill

                -- https://github.com/mdgriffith/elm-ui/issues/149#issuecomment-531480958
                , Element.clip
                , Element.htmlAttribute (HA.style "flex-shrink" "1")
                ]
      ]
        |> Element.column
            [ Element.width (Element.fillPortion 4)
            , Element.height Element.fill
            ]
    ]
        |> Element.row [ Element.width Element.fill, Element.height Element.fill ]


buttonElement : { label : String, onPress : Maybe Event } -> Element.Element Event
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


setTextInMonacoEditorCmd : String -> Cmd Event
setTextInMonacoEditorCmd =
    FrontendWeb.MonacoEditor.SetValue
        >> ElmFullstackCompilerInterface.GenerateJsonCoders.jsonEncodeMessageToMonacoEditor
        >> sendMessageToMonacoFrame


monacoEditorElement : State -> Element.Element Event
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


defaultProject : WorkingProjectStateStructure
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
    , editing = { filePathOpenedInEditor = Just [ "src", "Main.elm" ] }
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
        [ -- https://github.com/mdgriffith/elm-ui/issues/158#issuecomment-624231895
          Element.Border.widthEach { bottom = 1, left = 0, top = 0, right = 0 }
        , Element.Border.color <| Element.rgba 0 0 0 0
        , Element.mouseOver [ Element.Border.color <| Element.rgba 0.7 0.7 1 0.5 ]
        ]
        { url = url
        , label = labelText |> Element.text
        }
