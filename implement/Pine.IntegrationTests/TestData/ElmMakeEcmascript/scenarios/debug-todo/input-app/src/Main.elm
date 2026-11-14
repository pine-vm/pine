module Main exposing (main)

import Html


type Box
    = Box Int


main =
    Html.text (String.fromInt (value (Box 7)))


value (Box n) =
    if n > 0 then
        n

    else
        Debug.todo "unreachable"


unused =
    Debug.log "dead" 3
