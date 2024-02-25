module Main exposing (..)

{-
   This app shows two buttons to increment and decrement an integer in the app state.

   For more examples, including complete games, see <https://github.com/onlinegamemaker/making-online-games>
-}

import Browser
import Html
import Html.Attributes
import Html.Events


type alias State =
    Int


type Event
    = Increment
    | Decrement


main : Program () State Event
main =
    Browser.sandbox { init = init, update = update, view = view }


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
        [ Html.Attributes.style "background" "#333"
        , Html.Attributes.style "color" "whitesmoke"
        , Html.Attributes.style "font-size" "200%"
        , Html.Attributes.style "display" "grid"
        , Html.Attributes.style "place-content" "center"
        , Html.Attributes.style "width" "100vw"
        , Html.Attributes.style "height" "100vh"
        , Html.Attributes.style "overflow" "hidden"
        ]
        [ Html.button
            [ Html.Events.onClick Decrement
            , Html.Attributes.style "font-weight" "bold"
            , Html.Attributes.style "font-size" "150%"
            ]
            [ Html.text "-" ]
        , Html.div
            [ Html.Attributes.style "display" "inline-block"
            , Html.Attributes.style "padding" "1em 1em"
            ]
            [ Html.text (String.fromInt state) ]
        , Html.button
            [ Html.Events.onClick Increment
            , Html.Attributes.style "font-weight" "bold"
            , Html.Attributes.style "font-size" "150%"
            ]
            [ Html.text "+" ]
        ]
