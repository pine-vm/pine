module Frontend.Main exposing (Event(..), State, init, main, update, view)

import Browser
import Browser.Navigation as Navigation
import CompilationInterface.GenerateJsonCoders
import Dict
import FrontendBackendInterface
import Html
import Html.Events
import Http
import Random
import Random.Char
import Random.String
import Url


batchConfigDefault : BatchConfig
batchConfigDefault =
    { count = 1000, size = 5000 }


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
    }


type alias BatchConfig =
    { count : Int, size : Int }


type Event
    = UrlRequest Browser.UrlRequest
    | UrlChange Url.Url
    | UserInputGetDirectory
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
                CompilationInterface.GenerateJsonCoders.jsonDecodeGetDirectoryResponse
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
                                        Random.String.string batchConfig.size (Random.Char.char 32 126)
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
            , Html.div []
                [ viewDirectory state ]
            , Html.div []
                [ Html.button [ Html.Events.onClick UserInputGetDirectory ] [ Html.text "Get Directory" ] ]
            , Html.div []
                [ viewFormAppend state ]
            ]
    in
    { title = "Test Database Scale"
    , body = body
    }


viewDirectory : State -> Html.Html Event
viewDirectory state =
    case state.getDirectoryResult of
        Nothing ->
            Html.text "Directory not fetched yet."

        Just getDirectoryResult ->
            case getDirectoryResult of
                Err httpErr ->
                    Html.text (describeHttpError httpErr)

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
                        |> Html.text


viewFormAppend : State -> Html.Html Event
viewFormAppend _ =
    Html.button [ Html.Events.onClick (UserInputAppendBatch batchConfigDefault) ] [ Html.text "Append Batch" ]


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
