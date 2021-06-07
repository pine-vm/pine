module Main exposing (..)

-- This app implements an interactive counter. It contains two buttons to increment and decrement the number shown between the buttons.
--
-- For a list of other example projects, see https://github.com/onlinegamemaker/making-online-games

import Browser
import Html
import Html.Attributes
import Html.Events


main : Program () State Event
main =
    Browser.sandbox { init = init, update = update, view = view }


type alias State =
    Int


type Event
    = Increment
    | Decrement


init : State
init =
    0


update : Event -> State -> State
update event state =
    case event of
        Increment ->
            state + 1

        Decrement ->
            state - 1


view : State -> Html.Html Event
view state =
    Html.div
        [ Html.Attributes.style "background" "#222"
        , Html.Attributes.style "color" "whitesmoke"
        ]
        [ Html.button [ Html.Events.onClick Decrement ] [ Html.text "-" ]
        , Html.div [] [ Html.text (String.fromInt state) ]
        , Html.button [ Html.Events.onClick Increment ] [ Html.text "+" ]
        ]
