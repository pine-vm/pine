module Main exposing (main)
import Html
import A
import B
main = Html.text (A.value ++ B.value)
