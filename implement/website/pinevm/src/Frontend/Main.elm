module Frontend.Main exposing (main)

import Browser
import Browser.Dom
import Browser.Events
import Browser.Navigation
import Color
import Element
import Element.Background
import Element.Font
import FontAwesome
import FontAwesome.Brands
import FontAwesome.Styles
import Frontend.Page.Download
import Frontend.Page.Home
import Frontend.Page.News
import Frontend.PineLogo as PineLogo
import Frontend.Visuals as Visuals
import Html.Attributes
import Result.Extra
import Task
import Url
import Url.Parser


type NavigationLink
    = PageNavigationLink Page
    | ExternalNavigationLink { label : String, url : String }


type Page
    = HomePage
    | DownloadPage
    | NewsPage


type Event
    = UserInputNavigateToPageEvent Page
    | OnUrlChangeEvent Url.Url
    | OnUrlRequestEvent Browser.UrlRequest
    | BrowserOnResizeEvent WindowSize
    | NopEvent


type alias State =
    { navigationKey : Browser.Navigation.Key
    , selectedPage : Page
    , windowSize : WindowSize
    }


type alias WindowSize =
    { width : Int, height : Int }


linkToSourceCodeRepository : String
linkToSourceCodeRepository =
    "https://github.com/pine-vm/pine"


topNavigationElements : List NavigationLink
topNavigationElements =
    [ PageNavigationLink HomePage
    , ExternalNavigationLink { label = "Learn", url = "https://github.com/pine-vm/pine/tree/main/guide" }
    , ExternalNavigationLink { label = "Playground", url = "https://elm-editor.com" }
    , PageNavigationLink DownloadPage
    , PageNavigationLink NewsPage
    ]


main : Program () State Event
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = OnUrlRequestEvent
        , onUrlChange = OnUrlChangeEvent
        }


init : () -> Url.Url -> Browser.Navigation.Key -> ( State, Cmd.Cmd Event )
init _ url navigationKey =
    let
        stateBeforeUrl =
            { navigationKey = navigationKey
            , selectedPage = HomePage
            , windowSize = { width = 0, height = 0 }
            }

        getViewportCmd =
            Browser.Dom.getViewport
                |> Task.attempt
                    (Result.Extra.unpack
                        (always NopEvent)
                        (\viewport ->
                            BrowserOnResizeEvent
                                { width = round viewport.viewport.width
                                , height = round viewport.viewport.height
                                }
                        )
                    )
    in
    stateBeforeUrl
        |> updateOnUrlChange url
        |> Tuple.mapSecond (List.singleton >> (::) getViewportCmd >> Cmd.batch)


update : Event -> State -> ( State, Cmd.Cmd Event )
update event stateBefore =
    case event of
        UserInputNavigateToPageEvent page ->
            ( { stateBefore | selectedPage = page }
            , Cmd.none
            )

        OnUrlRequestEvent urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( stateBefore
                    , Browser.Navigation.pushUrl stateBefore.navigationKey (url |> Url.toString)
                    )

                Browser.External url ->
                    ( stateBefore, Browser.Navigation.load url )

        OnUrlChangeEvent url ->
            updateOnUrlChange url stateBefore

        BrowserOnResizeEvent size ->
            ( { stateBefore | windowSize = size }, Cmd.none )

        NopEvent ->
            ( stateBefore, Cmd.none )


updateOnUrlChange : Url.Url -> State -> ( State, Cmd.Cmd Event )
updateOnUrlChange url stateBefore =
    let
        pageFromUrl =
            Url.Parser.parse routeParser url
    in
    ( { stateBefore | selectedPage = pageFromUrl |> Maybe.withDefault stateBefore.selectedPage }
    , Cmd.none
    )


subscriptions : State -> Sub Event
subscriptions _ =
    Browser.Events.onResize (\width height -> BrowserOnResizeEvent { width = width, height = height })


routeParser : Url.Parser.Parser (Page -> a) a
routeParser =
    let
        fromDictionary =
            routes
                |> List.map (\( name, page ) -> Url.Parser.map page (Url.Parser.s name))
    in
    [ fromDictionary
    , [ Url.Parser.map HomePage Url.Parser.top
      ]
    ]
        |> List.concat
        |> Url.Parser.oneOf


routes : List ( String, Page )
routes =
    [ ( "home", HomePage )
    , ( "download", DownloadPage )
    , ( "downloads", DownloadPage )
    , ( "news", NewsPage )
    ]


view : State -> Browser.Document Event
view state =
    let
        title =
            if state.selectedPage == HomePage then
                "Pine — Run Elm Everywhere"

            else
                String.join " — " [ titleFromPage state.selectedPage, "Pine" ]

        pageHeadingElement =
            if state.selectedPage == HomePage then
                Element.none

            else
                Element.text (titleFromPage state.selectedPage)
                    |> Element.el (Element.centerX :: Visuals.headingAttributes 1)

        device =
            Element.classifyDevice state.windowSize
    in
    { title = title
    , body =
        [ [ Element.html Visuals.globalCssStyleHtmlElement
          , Element.html FontAwesome.Styles.css
          , header
          , [ pageHeadingElement
            , viewPageMainContent state.selectedPage device
            ]
                |> Element.column
                    [ Element.paddingXY Visuals.defaultFontSize 0
                    , Element.spacing (Visuals.defaultFontSize * 2)
                    , Element.width Element.fill
                    , Element.height Element.fill
                    ]
                |> Element.el
                    [ Element.width (Element.maximum mainContentMaxWidth Element.fill)
                    , Element.height Element.fill
                    , Element.centerX
                    ]
          , viewFooter device
                |> Element.el
                    [ Element.paddingXY 0 40
                    , Element.width Element.fill
                    ]
          ]
            |> Element.column
                [ Element.width Element.fill
                , Element.height Element.fill
                , Element.spacing (Visuals.defaultFontSize * 2)
                ]
            |> Element.layout
                [ Element.Font.family (Visuals.rootFontFamily |> List.map Element.Font.typeface)
                , Element.Font.size Visuals.defaultFontSize
                , Element.Font.color Visuals.defaultFontColor
                , Element.Background.color Visuals.backgroundColor
                , Element.width Element.fill

                -- , Element.height Element.fill
                ]
        ]
    }


viewPageMainContent : Page -> Element.Device -> Element.Element e
viewPageMainContent page device =
    case page of
        HomePage ->
            viewHomePage

        DownloadPage ->
            Frontend.Page.Download.view device

        NewsPage ->
            Frontend.Page.News.view device


viewHomePage : Element.Element e
viewHomePage =
    Frontend.Page.Home.view


header : Element.Element e
header =
    let
        logoElement =
            Element.link
                [ Element.pointer ]
                { url = "https://pine-vm.org"
                , label =
                    [ [ PineLogo.pineLogoSvg
                            { strokeThicknessMikro = 30 }
                            { fill = \lightness -> Color.toCssString (Color.hsl 0.555 1 (toFloat lightness / 1000))
                            , svgAttributes = [ Html.Attributes.style "height" "2.4em" ]
                            }
                            |> Element.html
                      ]
                        |> Element.row []
                    ]
                        |> Element.row
                            [ Element.spacing Visuals.defaultFontSize
                            , Element.htmlAttribute (Html.Attributes.style "letter-spacing" "3px")
                            , Element.mouseOver [ Element.alpha 0.8 ]
                            ]
                }

        navigationButtonFromDestination : NavigationLink -> Element.Element msg
        navigationButtonFromDestination destination =
            let
                commonAttributes =
                    [ Element.Font.bold
                    , Element.Font.color (Element.rgb 1 1 1)
                    , Element.padding (Visuals.defaultFontSize // 2)
                    , Element.mouseOver
                        [ Element.Font.color (Element.rgb 0.8 0.8 0.8)
                        ]
                    ]
            in
            case destination of
                PageNavigationLink page ->
                    linkToPage
                        commonAttributes
                        { page = page
                        , label = Element.text (titleFromPage page)
                        }

                ExternalNavigationLink { label, url } ->
                    Element.link
                        commonAttributes
                        { url = url
                        , label = Element.text label
                        }
    in
    [ [ logoElement
            |> Element.el
                [ Element.centerX
                , Element.padding 7
                ]
      ]
        |> Element.row
            [ Element.width Element.fill
            ]
    , Element.wrappedRow
        [ Element.spacing Visuals.defaultFontSize
        , Element.padding (Visuals.defaultFontSize * 2)
        , Element.centerX
        , Element.htmlAttribute (Html.Attributes.style "letter-spacing" "2px")
        ]
        (topNavigationElements |> List.map navigationButtonFromDestination)
    ]
        |> Element.column
            [ Element.width Element.fill
            , Element.Background.color Visuals.headerBackgroundColor
            , Element.paddingXY (Visuals.defaultFontSize * 2) 0
            ]


viewFooter : Element.Device -> Element.Element e
viewFooter _ =
    [ [ Visuals.linkElementFromUrlAndLabel
            { url = "https://github.com/pine-vm/pine/tree/main/implement/website/pinevm"
            , labelElement = Element.text " Site Source"
            , newTabLink = False
            }
      ]
        |> Element.wrappedRow
            [ Element.width Element.fill
            , Element.spacing Visuals.defaultFontSize
            , Element.padding (Visuals.defaultFontSize * 2)
            ]
    , [ weblinksElement
      ]
        |> Element.row
            [ Element.centerX
            , Element.spacing (Visuals.defaultFontSize * 2)
            ]
    ]
        |> Element.column
            [ Element.width Element.fill
            , Element.spacing Visuals.defaultFontSize
            ]
        |> Element.el
            [ Element.width (Element.maximum mainContentMaxWidth Element.fill)
            , Element.centerX
            ]
        |> Element.el
            [ Element.width Element.fill
            , Element.Background.color Visuals.footerBackgroundColor
            , Element.alignBottom
            ]


weblinksElement : Element.Element e
weblinksElement =
    [ ( FontAwesome.Brands.github, linkToSourceCodeRepository )
    , ( FontAwesome.Brands.discord, "https://discord.gg/UEYUj3MXZf" )
    , ( FontAwesome.Brands.bluesky, "https://bsky.app/profile/pinevm.bsky.social" )
    , ( FontAwesome.Brands.twitter, "https://twitter.com/PineVM" )
    ]
        |> List.map
            (\( icon, url ) ->
                Element.link
                    [ Element.pointer
                    , Element.Font.color (Element.rgb 0 0 0)
                    , Element.alpha 0.6
                    , Element.mouseOver
                        [ Element.alpha 1
                        ]
                    ]
                    { url = url
                    , label =
                        icon
                            |> FontAwesome.view
                            |> Element.html
                            |> Element.el
                                [ Element.Font.size (Visuals.defaultFontSize * 2)
                                , Element.padding 4
                                ]
                    }
            )
        |> Element.row
            [ Element.spacing Visuals.defaultFontSize
            ]


titleFromPage : Page -> String
titleFromPage page =
    case page of
        HomePage ->
            "Home"

        DownloadPage ->
            "Download"

        NewsPage ->
            "News"


linkToPage : List (Element.Attribute e) -> { label : Element.Element e, page : Page } -> Element.Element e
linkToPage attributes { label, page } =
    Element.link attributes
        { label = label
        , url = urlToPage page |> Maybe.withDefault ""
        }


urlToPage : Page -> Maybe String
urlToPage page =
    routes
        |> List.filter (Tuple.second >> (==) page)
        |> List.head
        |> Maybe.map Tuple.first


mainContentMaxWidth : Int
mainContentMaxWidth =
    1080
