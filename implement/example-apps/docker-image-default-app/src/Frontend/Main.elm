module Frontend.Main exposing (Event(..), State, init, main, update, view)

import Browser
import Browser.Navigation as Navigation
import CompilationInterface.SourceFiles
import Element
import Element.Font
import Element.Region
import Frontend.Markdown
import Frontend.Visuals
import Html
import Html.Attributes as HA
import Html.Events
import Http
import Json.Decode
import Result.Extra
import Url


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
                    Element.text "HTTP request failed."

                Loading ->
                    Element.text "Loading..."

                Success fullText ->
                    Html.div []
                        [ Html.span
                            []
                            [ Html.text "I received the following content from the backend:" ]
                        , Html.pre
                            [ HA.style "white-space" "pre-wrap" ]
                            [ Html.text fullText ]
                        ]
                        |> Element.html

        ( pageHeading, selectedPageContent ) =
            case state.selectedPage of
                Home pageState ->
                    ( "Home", pageContentFromPageState pageState )

                About pageState ->
                    ( "About", pageContentFromPageState pageState )

                NotFound url ->
                    ( "Not Found"
                    , Element.text ("I found nothing at " ++ url)
                    )

        body =
            [ Frontend.Visuals.globalCssStyleHtmlElement
            , Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.padding 16
                , Element.spacing 16
                ]
                [ Element.el
                    [ Element.padding 8 ]
                    viewNavigation
                , Element.el
                    [ Element.Region.heading 1
                    , Element.Font.size 24
                    ]
                    (Element.text pageHeading)
                , viewGuide
                , selectedPageContent
                ]
                |> Element.layout
                    [ Element.Font.size 16
                    , Element.Font.family
                        [ Element.Font.typeface "Helvetica"
                        , Element.Font.sansSerif
                        ]
                    ]
            ]
    in
    { title = pageHeading, body = body }


viewGuide : Element.Element event
viewGuide =
    CompilationInterface.SourceFiles.file____README_md.utf8
        |> Frontend.Markdown.viewViaDefaultHtmlRenderer { containerClassName = "markdown-body" }
        |> Result.map (Element.column [ Element.width Element.fill ])
        |> Result.Extra.extract Element.text


viewNavigation : Element.Element Event
viewNavigation =
    [ NavigateToHome, NavigateToAbout ]
        |> List.map
            (\offeredPage ->
                [ [ offeredPage
                        |> titleAndUrlFromOfferedPage
                        |> Tuple.first
                        |> Html.text
                  ]
                    |> htmlLinkToPage offeredPage
                ]
                    |> Html.span [ HA.style "margin" "4pt" ]
            )
        |> Html.div []
        |> Element.html


htmlLinkToPage : OfferedPage -> (List (Html.Html Event) -> Html.Html Event)
htmlLinkToPage page =
    let
        ( _, url ) =
            titleAndUrlFromOfferedPage page

        inputAttributes =
            case Url.fromString url of
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
