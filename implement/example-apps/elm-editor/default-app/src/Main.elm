module Main exposing (..)

-- Press buttons to increment and decrement a counter.
--
-- Read how it works:
--   https://guide.elm-lang.org/architecture/buttons.html
--

import Browser
import Html
import Html.Attributes
import Html.Events


main =
    Browser.sandbox { init = init, update = update, view = view }


type alias State =
    Int


init : State
init =
    0


type Event
    = Increment
    | Decrement


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
