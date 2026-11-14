module Application exposing (main)

import Browser
import Browser.Navigation as Navigation
import Html exposing (a, div, text)
import Html.Attributes exposing (href)
import Url exposing (Url)


type alias Model =
    { url : Url
    , key : Navigation.Key
    }


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url


main : Program () Model Msg
main =
    Browser.application
        { init = init
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


init : () -> Url -> Navigation.Key -> ( Model, Cmd Msg )
init () url key =
    ( { url = url, key = key }, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked request ->
            case request of
                Browser.Internal url ->
                    ( model, Navigation.pushUrl model.key (Url.toString url) )

                Browser.External address ->
                    ( model, Navigation.load address )

        UrlChanged url ->
            ( { model | url = url }, Cmd.none )


view : Model -> Browser.Document Msg
view model =
    { title = "Browser application"
    , body =
        [ div []
            [ text (Url.toString model.url)
            , a [ href "/next" ] [ text "Internal link" ]
            , a [ href "https://elm-lang.org/" ] [ text "External link" ]
            ]
        ]
    }
