module Main exposing (Msg(..), State, init, main, update, view)

import Browser
import Browser.Navigation as Navigation
import Html
import Html.Attributes as HA
import Html.Events
import Http
import Json.Decode
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


init : () -> Url.Url -> Navigation.Key -> ( State, Cmd Msg )
init _ url navigationKey =
    { selectedPage = NotFound "", navigationKey = navigationKey }
        |> update (UrlChange url)


type Msg
    = ServerResponse { requestPage : OfferedPage, result : Result Http.Error String }
    | UrlRequest Browser.UrlRequest
    | UrlChange Url.Url


apiRequestCmdFromOfferedPage : OfferedPage -> Cmd Msg
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
        , expect = Http.expectString (\result -> ServerResponse { requestPage = page, result = result })
        }


apiRequestCmdFromSelectedPage : Page -> Maybe (Cmd Msg)
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


update : Msg -> State -> ( State, Cmd Msg )
update msg stateBefore =
    case msg of
        ServerResponse { requestPage, result } ->
            let
                selectedPage =
                    case stateBefore.selectedPage of
                        Home pageStateBefore ->
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

                        About pageStateBefore ->
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


view : State -> Browser.Document Msg
view state =
    let
        pageContentFromPageState pageState =
            case pageState of
                Failure ->
                    Html.text "HTTP request failed."

                Loading ->
                    Html.text "Loading..."

                Success fullText ->
                    [ [ "I received the following content from the server:" |> Html.text ]
                        |> Html.span []
                    , Html.pre [] [ Html.text fullText ]
                    ]
                        |> Html.div []

        ( pageHeading, pageContent ) =
            case state.selectedPage of
                Home pageState ->
                    ( "Home", pageContentFromPageState pageState )

                About pageState ->
                    ( "About", pageContentFromPageState pageState )

                NotFound url ->
                    ( "Not Found", ("I found nothing at " ++ url) |> Html.text )

        body =
            [ viewNavigation
            , [ pageHeading |> Html.text ] |> Html.h1 []
            , pageContent
            ]
    in
    { title = pageHeading, body = body }


viewNavigation : Html.Html Msg
viewNavigation =
    [ NavigateToHome, NavigateToAbout ]
        |> List.map
            (\offeredPage ->
                [ [ offeredPage |> titleAndUrlFromOfferedPage |> Tuple.first |> Html.text ] |> htmlLinkToPage offeredPage ]
                    |> Html.span [ HA.style "margin" "4pt" ]
            )
        |> Html.div []


htmlLinkToPage : OfferedPage -> (List (Html.Html Msg) -> Html.Html Msg)
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
        ([ HA.href url ] ++ inputAttributes)


titleAndUrlFromOfferedPage : OfferedPage -> ( String, String )
titleAndUrlFromOfferedPage offeredPage =
    case offeredPage of
        NavigateToHome ->
            ( "Home", "/home" )

        NavigateToAbout ->
            ( "About", "/about" )
