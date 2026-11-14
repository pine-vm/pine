module Main exposing (main)

import Html exposing (Html)
import Optimizations


main : Html msg
main =
    Html.text (String.join "\n" Optimizations.results)
