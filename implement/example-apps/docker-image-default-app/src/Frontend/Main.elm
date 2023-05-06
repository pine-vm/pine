module Frontend.Main exposing (Event(..), State, init, main, update, view)

import Browser
import Browser.Navigation as Navigation
import CompilationInterface.SourceFiles
import Html
import Html.Attributes as HA
import Html.Events
import Http
import Json.Decode
import Markdown.Parser
import Result.Extra
import Url


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
    { selectedPage : Page
    , navigationKey : Navigation.Key
    }


type OfferedPage
    = NavigateToHome
    | NavigateToAbout


type Page
    = Home PageState
    | About PageState
    | NotFound String


type PageState
    = Failure
    | Loading
    | Success String


init : () -> Url.Url -> Navigation.Key -> ( State, Cmd Event )
init _ url navigationKey =
    { selectedPage = NotFound "", navigationKey = navigationKey }
        |> update (UrlChange url)


type Event
    = BackendResponse { requestPage : OfferedPage, result : Result Http.Error String }
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url


apiRequestCmdFromOfferedPage : OfferedPage -> Cmd Event
apiRequestCmdFromOfferedPage page =
    let
        path =
            case page of
                NavigateToHome ->
                    "api/home"

                NavigateToAbout ->
                    "api/about"
    in
    Http.get
        { url = path
        , expect = Http.expectString (\result -> BackendResponse { requestPage = page, result = result })
        }


apiRequestCmdFromSelectedPage : Page -> Maybe (Cmd Event)
apiRequestCmdFromSelectedPage =
    offeredPageFromSelectedPage >> Maybe.map apiRequestCmdFromOfferedPage


offeredPageFromSelectedPage : Page -> Maybe OfferedPage
offeredPageFromSelectedPage selectedPage =
    case selectedPage of
        Home _ ->
            Just NavigateToHome

        About _ ->
            Just NavigateToAbout

        NotFound _ ->
            Nothing


update : Event -> State -> ( State, Cmd Event )
update event stateBefore =
    case event of
        BackendResponse { requestPage, result } ->
            let
                selectedPage =
                    case stateBefore.selectedPage of
                        Home _ ->
                            if requestPage /= NavigateToHome then
                                stateBefore.selectedPage

                            else
                                let
                                    pageState =
                                        case result of
                                            Ok fullText ->
                                                Success fullText

                                            Err _ ->
                                                Failure
                                in
                                Home pageState

                        About _ ->
                            if requestPage /= NavigateToAbout then
                                stateBefore.selectedPage

                            else
                                let
                                    pageState =
                                        case result of
                                            Ok fullText ->
                                                Success fullText

                                            Err _ ->
                                                Failure
                                in
                                About pageState

                        NotFound _ ->
                            stateBefore.selectedPage
            in
            ( { stateBefore | selectedPage = selectedPage }, Cmd.none )

        UrlChange url ->
            let
                selectedPage =
                    case url.path of
                        "" ->
                            Home Loading

                        "/" ->
                            Home Loading

                        "/home" ->
                            Home Loading

                        "/about" ->
                            About Loading

                        other ->
                            NotFound other
            in
            ( { stateBefore | selectedPage = selectedPage }
            , selectedPage |> apiRequestCmdFromSelectedPage |> Maybe.withDefault Cmd.none
            )

        UrlRequest urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( stateBefore, Navigation.pushUrl stateBefore.navigationKey (Url.toString url) )

                Browser.External url ->
                    ( stateBefore, Navigation.load url )


view : State -> Browser.Document Event
view state =
    let
        pageContentFromPageState pageState =
            case pageState of
                Failure ->
                    Html.text "HTTP request failed."

                Loading ->
                    Html.text "Loading..."

                Success fullText ->
                    [ [ "I received the following content from the backend:" |> Html.text ]
                        |> Html.span []
                    , Html.pre [ HA.style "white-space" "pre-wrap" ] [ Html.text fullText ]
                    ]
                        |> Html.div []

        ( pageHeading, selectedPageContent ) =
            case state.selectedPage of
                Home pageState ->
                    ( "Home", pageContentFromPageState pageState )

                About pageState ->
                    ( "About", pageContentFromPageState pageState )

                NotFound url ->
                    ( "Not Found", ("I found nothing at " ++ url) |> Html.text )

        body =
            [ globalStylesHtmlElement
            , viewNavigation
            , [ pageHeading |> Html.text ] |> Html.h1 []
            , viewGuide
            , selectedPageContent
            ]
    in
    { title = pageHeading, body = body }


viewGuide : Html.Html event
viewGuide =
    CompilationInterface.SourceFiles.file____README_md.utf8
        |> Markdown.Parser.parse
        |> Result.mapError (List.map Markdown.Parser.deadEndToString >> String.join "\n")
        |> Result.andThen (Markdown.Parser.render Markdown.Parser.defaultHtmlRenderer)
        |> Result.map (Html.div [])
        |> Result.Extra.extract Html.text


viewNavigation : Html.Html Event
viewNavigation =
    [ NavigateToHome, NavigateToAbout ]
        |> List.map
            (\offeredPage ->
                [ [ offeredPage |> titleAndUrlFromOfferedPage |> Tuple.first |> Html.text ] |> htmlLinkToPage offeredPage ]
                    |> Html.span [ HA.style "margin" "4pt" ]
            )
        |> Html.div []


htmlLinkToPage : OfferedPage -> (List (Html.Html Event) -> Html.Html Event)
htmlLinkToPage page =
    let
        url =
            page |> titleAndUrlFromOfferedPage |> Tuple.second

        inputAttributes =
            case url |> Url.fromString of
                Nothing ->
                    []

                Just parsedUrl ->
                    [ Html.Events.custom
                        "click"
                        (Json.Decode.succeed
                            { stopPropagation = True
                            , preventDefault = True
                            , message = UrlChange parsedUrl
                            }
                        )
                    ]
    in
    Html.a
        (HA.href url :: inputAttributes)


titleAndUrlFromOfferedPage : OfferedPage -> ( String, String )
titleAndUrlFromOfferedPage offeredPage =
    case offeredPage of
        NavigateToHome ->
            ( "Home", "/home" )

        NavigateToAbout ->
            ( "About", "/about" )


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
