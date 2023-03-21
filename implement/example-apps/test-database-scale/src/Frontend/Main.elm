module Frontend.Main exposing (Event(..), State, init, main, update, view)

import Browser
import Browser.Navigation as Navigation
import CompilationInterface.GenerateJsonConverters
import Dict
import Element
import Element.Background
import Element.Font
import Element.Input
import Element.Region
import FrontendBackendInterface
import Html
import Http
import Random
import Random.Char
import Random.String
import Url


batchConfigDefault : BatchConfig
batchConfigDefault =
    { count = 1000, elementSize = 5000 }


main : Program () State Event
main =
    Browser.application
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view
        , onUrlRequest = UrlRequest
        , onUrlChange = UrlChange
        }


type alias State =
    { navigationKey : Navigation.Key
    , getDirectoryResult : Maybe (Result Http.Error FrontendBackendInterface.GetDirectoryResponse)
    , batchConfig : BatchConfig
    }


type alias BatchConfig =
    { count : Int, elementSize : Int }


type Event
    = UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | UserInputGetDirectory
    | UserInputConfigureBatch BatchConfig
    | UserInputAppendBatch BatchConfig
    | BackendResponse BackendResponseStructure
    | TaskPostEntry Int String


type BackendResponseStructure
    = GetDirectoryResult (Result Http.Error FrontendBackendInterface.GetDirectoryResponse)
    | PostEntryResult (Result Http.Error ())


init : () -> Url.Url -> Navigation.Key -> ( State, Cmd Event )
init _ _ navigationKey =
    ( { navigationKey = navigationKey
      , getDirectoryResult = Nothing
      , batchConfig = batchConfigDefault
      }
    , Cmd.none
    )


cmdRequestGetDirectory : Cmd Event
cmdRequestGetDirectory =
    Http.get
        { url = "api/entry"
        , expect =
            Http.expectJson
                (GetDirectoryResult >> BackendResponse)
                CompilationInterface.GenerateJsonConverters.jsonDecodeGetDirectoryResponse
        }


cmdRequestPostEntry : Int -> String -> Cmd Event
cmdRequestPostEntry entryId entryContent =
    Http.post
        { url = "api/entry/" ++ String.fromInt entryId
        , body = Http.stringBody "text" entryContent
        , expect = Http.expectWhatever (PostEntryResult >> BackendResponse)
        }


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    case event of
        UrlChange _ ->
            ( stateBefore
            , Cmd.none
            )

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( stateBefore, Navigation.pushUrl stateBefore.navigationKey (Url.toString url) )

                Browser.External url ->
                    ( stateBefore, Navigation.load url )

        BackendResponse (GetDirectoryResult result) ->
            ( { stateBefore | getDirectoryResult = Just result }
            , Cmd.none
            )

        BackendResponse (PostEntryResult _) ->
            ( stateBefore
            , Cmd.none
            )

        UserInputGetDirectory ->
            ( stateBefore
            , cmdRequestGetDirectory
            )

        UserInputConfigureBatch batchConfig ->
            ( { stateBefore | batchConfig = batchConfig }
            , Cmd.none
            )

        UserInputAppendBatch batchConfig ->
            let
                lastEntryId =
                    stateBefore.getDirectoryResult
                        |> Maybe.andThen Result.toMaybe
                        |> Maybe.andThen (Dict.keys >> List.maximum)

                postEntryCmds =
                    List.range 0 (batchConfig.count - 1)
                        |> List.map
                            (\entryIdOffset ->
                                let
                                    entryId =
                                        Maybe.withDefault 0 lastEntryId + 1 + entryIdOffset

                                    randomGenerator =
                                        Random.String.string batchConfig.elementSize (Random.Char.char 32 126)
                                in
                                Random.generate (TaskPostEntry entryId) randomGenerator
                            )
            in
            ( stateBefore
            , Cmd.batch postEntryCmds
            )

        TaskPostEntry entryId entryContent ->
            ( stateBefore
            , cmdRequestPostEntry entryId entryContent
            )


view : State -> Browser.Document Event
view state =
    let
        body =
            [ globalStylesHtmlElement
                |> Element.html
            , viewDirectory state
            , Element.Input.button buttonStyle
                { onPress = Just UserInputGetDirectory
                , label = Element.text "Get Directory"
                }
            , viewFormConfigureAndAppendBatch state
            ]
                |> Element.column [ Element.spacing 20 ]
                |> Element.layout []
    in
    { title = "Test Database Scale"
    , body = [ body ]
    }


viewDirectory : State -> Element.Element Event
viewDirectory state =
    case state.getDirectoryResult of
        Nothing ->
            Element.text "Directory not fetched yet."

        Just getDirectoryResult ->
            case getDirectoryResult of
                Err httpErr ->
                    Element.text (describeHttpError httpErr)

                Ok directory ->
                    let
                        aggregateSize =
                            directory |> Dict.values |> List.map .length |> List.sum
                    in
                    ("Directory contains "
                        ++ (directory |> Dict.size |> String.fromInt)
                        ++ " items with an aggregate size of "
                        ++ (aggregateSize |> String.fromInt)
                    )
                        |> Element.text


viewFormConfigureAndAppendBatch : State -> Element.Element Event
viewFormConfigureAndAppendBatch state =
    [ Element.text "Configure Batch"
        |> Element.el [ Element.Region.heading 3 ]
    , viewFormConfigureBatch state
    , viewFormAppendBatch state
    ]
        |> Element.column [ Element.spacing 10 ]


viewFormConfigureBatch : State -> Element.Element Event
viewFormConfigureBatch state =
    let
        batchConfig =
            state.batchConfig
    in
    [ [ Element.text "count"
      , Element.Input.text []
            { onChange =
                \countText ->
                    UserInputConfigureBatch
                        { batchConfig
                            | count = countText |> String.toInt |> Maybe.withDefault batchConfig.count
                        }
            , text = String.fromInt batchConfig.count
            , placeholder = Nothing
            , label = Element.Input.labelHidden "batch count"
            }
      ]
        |> Element.row [ Element.spacing 10 ]
    , [ Element.text "element size"
      , Element.Input.text []
            { onChange =
                \sizeText ->
                    UserInputConfigureBatch
                        { batchConfig
                            | elementSize = sizeText |> String.toInt |> Maybe.withDefault batchConfig.elementSize
                        }
            , text = String.fromInt batchConfig.elementSize
            , placeholder = Nothing
            , label = Element.Input.labelHidden "element size"
            }
      ]
        |> Element.row [ Element.spacing 10 ]
    ]
        |> Element.column []


viewFormAppendBatch : State -> Element.Element Event
viewFormAppendBatch state =
    Element.Input.button buttonStyle
        { onPress = Just (UserInputAppendBatch state.batchConfig)
        , label = Element.text "Append Batch"
        }


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


buttonStyle : List (Element.Attribute a)
buttonStyle =
    [ Element.Background.color (Element.rgb255 14 99 156)
    , Element.mouseOver
        [ Element.Background.color (Element.rgb255 17 119 187) ]
    , Element.Font.color (Element.rgb 1 1 1)
    , Element.paddingXY 10 5
    ]


globalStylesHtmlElement : Html.Html a
globalStylesHtmlElement =
    """
body {
font-family: 'Segoe UI', Tahoma, Geneva, Verdana, sans-serif;
margin: 1em;
}
"""
        |> Html.text
        |> List.singleton
        |> Html.node "style" []
