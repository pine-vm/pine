port module FrontendWeb.Main exposing (Event(..), State, init, main, receiveMessageFromMonacoFrame, sendMessageToMonacoFrame, update, view)

import Base64
import Browser
import Browser.Navigation as Navigation
import Bytes.Encode
import Common
import Element
import Element.Background
import Element.Border
import Element.Font
import Element.Input
import ElmFullstackCompilerInterface.GenerateJsonCoders
import FrontendBackendInterface
import FrontendWeb.MonacoEditor
import Html
import Html.Attributes as HA
import Http
import Json.Decode
import Json.Encode
import Url
import Url.Builder


port sendMessageToMonacoFrame : Json.Encode.Value -> Cmd msg


port receiveMessageFromMonacoFrame : (Json.Encode.Value -> msg) -> Sub msg


type alias ElmMakeResponseStructure =
    FrontendBackendInterface.ElmMakeResponseStructure


type alias State =
    { navigationKey : Navigation.Key
    , editorElmCode : String
    , decodeMessageFromMonacoEditorError : Maybe Json.Decode.Error
    , elmMakeResult : Maybe (Result Http.Error ElmMakeResultStructure)
    , elmFormatResult : Maybe (Result Http.Error FrontendBackendInterface.FormatElmModuleTextResponseStructure)
    }


type alias ElmMakeResultStructure =
    { response : ElmMakeResponseStructure
    , compiledHtmlDocument : Maybe String
    }


type Event
    = UserInputElmCode String
    | MonacoEditorEvent Json.Decode.Value
    | UserInputFormat
    | UserInputCompile
    | BackendElmFormatResponseEvent (Result Http.Error FrontendBackendInterface.FormatElmModuleTextResponseStructure)
    | BackendElmMakeResponseEvent (Result Http.Error FrontendBackendInterface.ElmMakeResponseStructure)
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url


main : Program () State Event
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = always (receiveMessageFromMonacoFrame MonacoEditorEvent)
        , view = view
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }


init : () -> Url.Url -> Navigation.Key -> ( State, Cmd Event )
init _ url navigationKey =
    let
        ( model, urlChangeCmd ) =
            { navigationKey = navigationKey
            , editorElmCode = initElmCode
            , decodeMessageFromMonacoEditorError = Nothing
            , elmMakeResult = Nothing
            , elmFormatResult = Nothing
            }
                |> update (UrlChange url)
    in
    ( model, urlChangeCmd )


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    case event of
        UserInputElmCode inputElmCode ->
            ( { stateBefore | editorElmCode = inputElmCode }
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
                            stateBefore |> update (UserInputElmCode content)

                        FrontendWeb.MonacoEditor.CompletedSetupEvent ->
                            ( stateBefore, stateBefore.editorElmCode |> setTextInMonacoEditorCmd )

        UserInputFormat ->
            ( stateBefore, elmFormatCmd stateBefore )

        UserInputCompile ->
            ( stateBefore, elmMakeCmd stateBefore )

        BackendElmFormatResponseEvent httpResponse ->
            let
                editorElmCode =
                    httpResponse |> Result.toMaybe |> Maybe.andThen .formattedText |> Maybe.withDefault stateBefore.editorElmCode
            in
            ( { stateBefore | elmFormatResult = Just httpResponse, editorElmCode = editorElmCode }
            , setTextInMonacoEditorCmd editorElmCode
            )

        BackendElmMakeResponseEvent httpResponse ->
            let
                elmMakeResult =
                    httpResponse
                        |> Result.map
                            (\responseOk ->
                                let
                                    compiledHtmlDocument =
                                        case
                                            responseOk.files
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
                                { response = responseOk, compiledHtmlDocument = compiledHtmlDocument }
                            )
            in
            ( { stateBefore | elmMakeResult = Just elmMakeResult }
            , Cmd.none
            )

        UrlChange _ ->
            ( stateBefore, Cmd.none )

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( stateBefore, Navigation.pushUrl stateBefore.navigationKey (Url.toString url) )

                Browser.External url ->
                    ( stateBefore, Navigation.load url )


elmFormatCmd : State -> Cmd Event
elmFormatCmd state =
    let
        request =
            state.editorElmCode |> FrontendBackendInterface.FormatElmModuleTextRequest

        jsonDecoder backendResponse =
            case backendResponse of
                FrontendBackendInterface.FormatElmModuleTextResponse formatResponse ->
                    Json.Decode.succeed formatResponse

                _ ->
                    Json.Decode.fail "Unexpected response"
    in
    requestToApiCmd request jsonDecoder BackendElmFormatResponseEvent


elmMakeCmd : State -> Cmd Event
elmMakeCmd state =
    let
        base64FromString : String -> String
        base64FromString =
            Bytes.Encode.string
                >> Bytes.Encode.encode
                >> Base64.fromBytes
                >> Maybe.withDefault "Error encoding in base64"

        entryPointFilePath =
            [ "src", "Main.elm" ]

        elmMakeRequest =
            { commandLineArguments = "make " ++ (entryPointFilePath |> String.join "/") ++ " --output=" ++ elmMakeOutputFileName
            , files =
                [ ( [ "elm.json" ]
                  , initElmJson
                  )
                , ( entryPointFilePath
                  , state.editorElmCode
                  )
                ]
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
    requestToApiCmd request jsonDecoder BackendElmMakeResponseEvent


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


view : State -> Browser.Document Event
view state =
    let
        resultElement =
            case state.elmMakeResult of
                Nothing ->
                    Element.text "No compilation so far" |> Element.el [ Element.padding defaultFontSize ]

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
                                        [ channel |> Element.text
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
                                    , Element.scrollbarY
                                    ]

                        Just compiledHtmlDocument ->
                            Html.iframe
                                [ HA.srcdoc compiledHtmlDocument
                                , HA.style "height" "100%"
                                ]
                                []
                                |> Element.html

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

        formatButton =
            buttonElement { label = "📄 Format", onPress = Just UserInputFormat }

        compileButton =
            buttonElement { label = "▶️ Compile", onPress = Just UserInputCompile }

        body =
            [ [ [ formatButton ]
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
                    |> Element.el [ Element.width Element.fill, Element.height Element.fill ]
              ]
                |> Element.column
                    [ Element.width (Element.fillPortion 4)
                    , Element.height Element.fill
                    ]
            ]
                |> Element.row [ Element.width Element.fill, Element.height Element.fill ]
                |> Element.layout
                    [ Element.Font.family (rootFontFamily |> List.map Element.Font.typeface)
                    , Element.Font.size defaultFontSize
                    , Element.Font.color (Element.rgb 0.95 0.95 0.95)
                    , Element.Background.color backgroundColor
                    , Element.width Element.fill
                    ]
    in
    { title = "Elm Editor", body = [ body ] }


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


initElmCode : String
initElmCode =
    """
module Main exposing (..)


import Html
import Html.Attributes


main =
    [ Html.text "Hello World!" ]
    |> Html.div [ Html.Attributes.style "color" "whitesmoke" ]

"""


initElmJson : String
initElmJson =
    """
{
    "type": "application",
    "source-directories": [
        "src"
    ],
    "elm-version": "0.19.1",
    "dependencies": {
        "direct": {
            "danfishgold/base64-bytes": "1.0.3",
            "elm/browser": "1.0.2",
            "elm/bytes": "1.0.8",
            "elm/core": "1.0.5",
            "elm/html": "1.0.0",
            "elm/http": "2.0.0",
            "elm/json": "1.1.3",
            "elm/url": "1.0.0",
            "mdgriffith/elm-ui": "1.1.8"
        },
        "indirect": {
            "elm/file": "1.0.5",
            "elm/time": "1.0.0",
            "elm/virtual-dom": "1.0.2"
        }
    },
    "test-dependencies": {
        "direct": {},
        "indirect": {
            "elm/parser": "1.1.0",
            "elm/regex": "1.0.0"
        }
    }
}
"""


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
