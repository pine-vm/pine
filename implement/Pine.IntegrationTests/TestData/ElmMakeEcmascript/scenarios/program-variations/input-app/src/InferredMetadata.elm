module InferredMetadata exposing (main)

import Browser
import Html


type Msg
    = Increment


main =
    Browser.sandbox
        { init = 0
        , update = update
        , view = \_ -> Html.text "inferred"
        }


update : Msg -> Int -> Int
update _ model =
    model + 1
