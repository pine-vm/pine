module Sandbox exposing (main)

import Browser
import Html exposing (Html, button, text)
import Html.Events exposing (onClick)


type Msg
    = Increment


main : Program () Int Msg
main =
    Browser.sandbox
        { init = 0
        , update = update
        , view = view
        }


update : Msg -> Int -> Int
update Increment model =
    model + 1


view : Int -> Html Msg
view model =
    button [ onClick Increment ] [ text (String.fromInt model) ]
