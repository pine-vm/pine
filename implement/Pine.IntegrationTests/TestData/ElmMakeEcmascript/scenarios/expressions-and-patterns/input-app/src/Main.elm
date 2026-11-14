module Main exposing (main)

import Coverage
import Html exposing (Html)


main : Html msg
main =
    Html.text (String.join "\n" Coverage.results)
