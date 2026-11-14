module Aliased exposing (main)

import Browser
import Html


type alias Flags =
    ()


type alias App flags =
    Program flags () ()


main : App Flags
main =
    Browser.element
        { init = \() -> ( (), Cmd.none )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = \_ -> Html.text "aliases"
        }
