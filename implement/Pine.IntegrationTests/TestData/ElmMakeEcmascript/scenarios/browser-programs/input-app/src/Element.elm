module Element exposing (main)

import Browser
import Html exposing (Html, button, text)
import Html.Events exposing (onClick)


type Msg
    = Increment


main : Program () Int Msg
main =
    Browser.element
        { init = \() -> ( 0, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Int -> ( Int, Cmd Msg )
update Increment model =
    ( model + 1, Cmd.none )


view : Int -> Html Msg
view model =
    button [ onClick Increment ] [ text (String.fromInt model) ]
