module ImportedMetadata exposing (main)

import AppTypes
import Browser
import Html


type alias Payload a =
    { value : a, entries : List a, pair : ( a, a ) }


type Msg
    = ReceiveImported (AppTypes.Payload Int)
    | ReceiveLocal (Payload Int)
    | Nested (Maybe Msg)


main : AppTypes.App Msg
main =
    Browser.element
        { init = \() -> ( (), Cmd.none )
        , update = \_ model -> ( model, Cmd.none )
        , subscriptions = \_ -> Sub.none
        , view = \_ -> Html.text "imported alias"
        }
