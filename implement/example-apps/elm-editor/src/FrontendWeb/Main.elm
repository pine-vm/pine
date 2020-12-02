port module FrontendWeb.Main exposing (Event(..), State, init, main, receiveMessageFromMonacoFrame, sendMessageToMonacoFrame, update, view)

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
import ProjectState
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
    , time : Maybe Time.Posix
    , projectFiles : Dict.Dict (List String) String
    , fileInEditor : Maybe ( List String, String )
    , decodeMessageFromMonacoEditorError : Maybe Json.Decode.Error
    , lastElmMakeRequest : Maybe { time : Time.Posix, request : ElmMakeRequestStructure }
    , elmMakeResult : Maybe (Result Http.Error ElmMakeResultStructure)
    , elmFormatResult : Maybe (Result Http.Error FrontendBackendInterface.FormatElmModuleTextResponseStructure)
    , saveOrShareDialog : Maybe SaveOrShareDialog
    }


type alias ElmMakeResultStructure =
    { request : ElmMakeRequestStructure
    , response : ElmMakeResponseStructure
    , compiledHtmlDocument : Maybe String
    }


type Event
    = UserInputChangeTextInEditor String
    | MonacoEditorEvent Json.Decode.Value
    | TimeHasArrived Time.Posix
    | UserInputOpenFileInEditor (List String)
    | UserInputFormat
    | UserInputCompile
    | UserInputSave Bool
    | UserInputCloseModalDialog
    | BackendElmFormatResponseEvent { filePath : List String, result : Result Http.Error FrontendBackendInterface.FormatElmModuleTextResponseStructure }
    | BackendElmMakeResponseEvent ElmMakeRequestStructure (Result Http.Error ElmMakeResponseStructure)
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url


type alias SaveOrShareDialog =
    { urlToProject : Maybe String }


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
subscriptions state =
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
            , projectFiles = Dict.empty
            , fileInEditor = Nothing
            , decodeMessageFromMonacoEditorError = Nothing
            , lastElmMakeRequest = Nothing
            , elmMakeResult = Nothing
            , elmFormatResult = Nothing
            , saveOrShareDialog = Nothing
            }
                |> loadProject defaultProject
                |> update (UrlChange url)
    in
    ( model, urlChangeCmd )


loadProject : ProjectState.ProjectState -> State -> State
loadProject project state =
    let
        projectFiles =
            project
                |> List.map (\file -> ( file.filePath, file.fileContentText ))

        fileInEditor =
            case projectFiles |> List.filter offerToOpenFileInEditor of
                [ singleMatch ] ->
                    Just singleMatch

                _ ->
                    Nothing
    in
    { state | projectFiles = projectFiles |> Dict.fromList, fileInEditor = fileInEditor }


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    case event of
        UserInputOpenFileInEditor filePath ->
            case stateBefore.projectFiles |> Dict.get filePath of
                Nothing ->
                    ( stateBefore, Cmd.none )

                Just fileContent ->
                    ( { stateBefore | fileInEditor = Just ( filePath, fileContent ) }
                    , setTextInMonacoEditorCmd fileContent
                    )

        UserInputChangeTextInEditor inputElmCode ->
            case stateBefore.fileInEditor of
                Nothing ->
                    ( stateBefore, Cmd.none )

                Just ( filePath, _ ) ->
                    let
                        fileInEditor =
                            ( filePath, inputElmCode )

                        projectFiles =
                            stateBefore.projectFiles
                                |> Dict.insert filePath inputElmCode
                    in
                    ( { stateBefore | projectFiles = projectFiles, fileInEditor = Just fileInEditor }
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
                            ( stateBefore
                            , stateBefore.fileInEditor
                                |> Maybe.map (Tuple.second >> setTextInMonacoEditorCmd)
                                |> Maybe.withDefault Cmd.none
                            )

        TimeHasArrived time ->
            ( { stateBefore | time = Just time }, Cmd.none )

        UserInputFormat ->
            ( stateBefore, elmFormatCmd stateBefore )

        UserInputCompile ->
            userInputCompile stateBefore

        BackendElmFormatResponseEvent formatResponseEvent ->
            if Just formatResponseEvent.filePath /= Maybe.map Tuple.first stateBefore.fileInEditor then
                ( stateBefore, Cmd.none )

            else
                case formatResponseEvent.result |> Result.toMaybe |> Maybe.andThen .formattedText of
                    Nothing ->
                        ( stateBefore, Cmd.none )

                    Just formattedText ->
                        let
                            projectFiles =
                                stateBefore.projectFiles
                                    |> Dict.insert formatResponseEvent.filePath formattedText

                            fileInEditor =
                                ( formatResponseEvent.filePath, formattedText )
                        in
                        ( { stateBefore
                            | elmFormatResult = Just formatResponseEvent.result
                            , projectFiles = projectFiles
                            , fileInEditor = Just fileInEditor
                          }
                        , setTextInMonacoEditorCmd (Tuple.second fileInEditor)
                        )

        BackendElmMakeResponseEvent elmMakeRequest httpResponse ->
            let
                elmMakeResult =
                    httpResponse
                        |> Result.map
                            (\elmMakeResponse ->
                                let
                                    compiledHtmlDocument =
                                        case
                                            elmMakeResponse.files
                                                |> List.filter (.path >> List.reverse >> List.head >> (==) (Just elmMakeOutputFileName))
                                                |> List.head
                                        of
                                            Nothing ->
                                                Nothing

                                            Just newFile ->
                                                newFile.contentBase64
                                                    |> Common.decodeBase64ToString
                                                    |> Maybe.withDefault ("Error decoding base64: " ++ newFile.contentBase64)
                                                    |> Just
                                in
                                { request = elmMakeRequest
                                , response = elmMakeResponse
                                , compiledHtmlDocument = compiledHtmlDocument
                                }
                            )
            in
            ( { stateBefore | elmMakeResult = Just elmMakeResult }, Cmd.none )

        UrlChange url ->
            let
                state =
                    case FrontendWeb.ProjectStateInUrl.projectStateFromUrl url |> Maybe.andThen Result.toMaybe of
                        Nothing ->
                            stateBefore

                        Just project ->
                            stateBefore |> loadProject project
            in
            ( state
            , Cmd.none
            )

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( stateBefore, Navigation.pushUrl stateBefore.navigationKey (Url.toString url) )

                Browser.External url ->
                    ( stateBefore, Navigation.load url )

        UserInputSave generateLink ->
            let
                dialogBefore =
                    Maybe.withDefault { urlToProject = Nothing } stateBefore.saveOrShareDialog

                ( dialog, cmd ) =
                    if generateLink then
                        let
                            projectState =
                                stateBefore.projectFiles
                                    |> Dict.toList
                                    |> List.map
                                        (\( filePath, fileContentText ) ->
                                            { filePath = filePath, fileContentText = fileContentText }
                                        )

                            url =
                                stateBefore.url
                                    |> FrontendWeb.ProjectStateInUrl.setProjectStateInUrl projectState
                                    |> Url.toString
                        in
                        ( { dialogBefore | urlToProject = Just url }
                        , Navigation.replaceUrl stateBefore.navigationKey url
                        )

                    else
                        ( dialogBefore, Cmd.none )
            in
            ( { stateBefore | saveOrShareDialog = Just dialog }, cmd )

        UserInputCloseModalDialog ->
            ( { stateBefore | saveOrShareDialog = Nothing }, Cmd.none )


elmFormatCmd : State -> Cmd Event
elmFormatCmd state =
    case state.fileInEditor of
        Nothing ->
            Cmd.none

        Just ( filePath, fileContent ) ->
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


userInputCompile : State -> ( State, Cmd Event )
userInputCompile stateBefore =
    case stateBefore.fileInEditor of
        Nothing ->
            ( stateBefore, Cmd.none )

        Just ( filePath, _ ) ->
            let
                base64FromString : String -> String
                base64FromString =
                    Bytes.Encode.string
                        >> Bytes.Encode.encode
                        >> Base64.fromBytes
                        >> Maybe.withDefault "Error encoding in base64"

                entryPointFilePath =
                    filePath

                elmMakeRequest =
                    { commandLineArguments = "make " ++ (entryPointFilePath |> String.join "/") ++ " --output=" ++ elmMakeOutputFileName
                    , files =
                        stateBefore.projectFiles
                            |> Dict.toList
                            |> List.map
                                (\( path, content ) ->
                                    { path = path
                                    , contentBase64 = base64FromString content
                                    }
                                )
                    }

                request =
                    elmMakeRequest |> FrontendBackendInterface.ElmMakeRequest

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
            , requestToApiCmd request jsonDecoder (BackendElmMakeResponseEvent elmMakeRequest)
            )


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


elmMakeOutputFileName : String
elmMakeOutputFileName =
    "elm-make-output.html"


offerToOpenFileInEditor : ( List String, String ) -> Bool
offerToOpenFileInEditor =
    Tuple.first >> List.reverse >> List.head >> Maybe.map (String.endsWith ".elm") >> Maybe.withDefault False


view : State -> Browser.Document Event
view state =
    let
        mainContent =
            case state.fileInEditor of
                Nothing ->
                    let
                        projectFiles =
                            state.projectFiles |> Dict.toList

                        otherFilesList =
                            case projectFiles |> List.filter (offerToOpenFileInEditor >> not) of
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
                            case projectFiles |> List.filter offerToOpenFileInEditor of
                                [] ->
                                    Element.text "Did not find any .elm file in this project."

                                elmFilesInTheProject ->
                                    [ Element.text "Choose one of the files in the project to open in the editor:"
                                    , elmFilesInTheProject
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
                    [ [ saveButton ]
                        |> Element.row
                            [ Element.spacing defaultFontSize
                            , Element.padding (defaultFontSize // 2)
                            ]
                    , [ chooseElmFileElement, otherFilesList ]
                        |> Element.column
                            [ Element.spacing defaultFontSize
                            , Element.centerX
                            , Element.centerY
                            ]
                    ]
                        |> Element.column
                            [ Element.spacing (defaultFontSize // 2)
                            , Element.width (Element.fillPortion 4)
                            , Element.height Element.fill
                            ]

                Just _ ->
                    viewWhenEditorOpen state

        popupAttributes =
            case state.saveOrShareDialog of
                Nothing ->
                    []

                Just saveOrShareDialog ->
                    let
                        buttonGenerateUrl =
                            buttonElement { label = "Generate link to project", onPress = Just (UserInputSave True) }

                        urlElement =
                            case saveOrShareDialog.urlToProject of
                                Nothing ->
                                    Element.el [ Element.transparent True ] (Element.html (htmlOfferingTextToCopy ""))

                                Just urlToProject ->
                                    Element.html (htmlOfferingTextToCopy urlToProject)

                        popup =
                            [ buttonGenerateUrl |> Element.el [ Element.centerX ]
                            , urlElement |> Element.el [ Element.width (Element.px 500) ]
                            ]
                                |> Element.column
                                    [ Element.spacing defaultFontSize
                                    , Element.padding defaultFontSize
                                    ]
                                |> Element.el
                                    [ Element.Background.color (Element.rgb 0 0 0)
                                    , Element.Border.color (Element.rgb 0.8 0.8 0.8)
                                    , Element.Border.width 2
                                    , Element.htmlAttribute
                                        (Html.Events.custom "click"
                                            (Json.Decode.succeed
                                                { message = UserInputSave False
                                                , stopPropagation = True
                                                , preventDefault = False
                                                }
                                            )
                                        )
                                    ]
                    in
                    [ Element.el
                        [ Element.Background.color (Element.rgba 0 0 0 0.3)
                        , Element.width Element.fill
                        , Element.height Element.fill
                        , Element.Events.onClick UserInputCloseModalDialog
                        , Element.htmlAttribute (HA.style "backdrop-filter" "blur(1px)")
                        , Element.inFront (Element.el [ Element.centerX, Element.centerY ] popup)
                        ]
                        Element.none
                        |> Element.inFront
                    ]

        body =
            Element.layout
                ([ Element.Font.family (rootFontFamily |> List.map Element.Font.typeface)
                 , Element.Font.size defaultFontSize
                 , Element.Font.color (Element.rgb 0.95 0.95 0.95)
                 , Element.Background.color backgroundColor
                 , Element.width Element.fill
                 ]
                    ++ popupAttributes
                )
                mainContent
    in
    { title = "Elm Editor", body = [ body ] }


viewWhenEditorOpen : State -> Element.Element Event
viewWhenEditorOpen state =
    let
        resultElement =
            case state.lastElmMakeRequest of
                Nothing ->
                    [ "No compilation started so far. You can use the 'Compile' button to check program text for errors and see your app in action."
                        |> Element.text
                    ]
                        |> Element.paragraph [ Element.padding defaultFontSize ]

                Just lastElmMakeRequest ->
                    case state.elmMakeResult of
                        Nothing ->
                            Element.text "Compiling..." |> Element.el [ Element.padding defaultFontSize ]

                        Just (Err elmMakeError) ->
                            ("Error: " ++ describeHttpError elmMakeError) |> Element.text

                        Just (Ok elmMakeOk) ->
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
                                        , HA.style "height" "100%"
                                        ]
                                        []
                                        |> Element.html

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
    buttonElement { label = "💾 Save", onPress = Just (UserInputSave False) }


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


defaultProject : ProjectState.ProjectState
defaultProject =
    [ { filePath = [ "src", "Main.elm" ]
      , fileContentText =
            ElmFullstackCompilerInterface.SourceFiles.file____default_app_src_Main_elm
                |> decodeBytesToString
                |> Maybe.withDefault "Failed to decode file"
      }
    , { filePath = [ "elm.json" ]
      , fileContentText =
            ElmFullstackCompilerInterface.SourceFiles.file____default_app_elm_json
                |> decodeBytesToString
                |> Maybe.withDefault "Failed to decode file"
      }
    ]


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))


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
