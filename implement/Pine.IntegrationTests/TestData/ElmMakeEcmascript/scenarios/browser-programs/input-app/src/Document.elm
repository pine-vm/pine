module Document exposing (main)

import Browser
import Html exposing (button, text)
import Html.Events exposing (onClick)


type Msg
    = Increment


main : Program () Int Msg
main =
    Browser.document
        { init = \() -> ( 0, Cmd.none )
        , update = update
        , view = view
        , subscriptions = \_ -> Sub.none
        }


update : Msg -> Int -> ( Int, Cmd Msg )
update Increment model =
    ( model + 1, Cmd.none )


view : Int -> Browser.Document Msg
view model =
    { title = "Counter " ++ String.fromInt model
    , body = [ button [ onClick Increment ] [ text (String.fromInt model) ] ]
    }
