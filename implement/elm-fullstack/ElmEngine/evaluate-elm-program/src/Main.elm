module Main exposing (..)

import Elm.Syntax.File
import ElmEvaluation
import Parser
import Platform


parseElmModuleTextToJson : String -> String
parseElmModuleTextToJson =
    ElmEvaluation.parseElmModuleTextToJson


parseElmModuleText : String -> Result (List Parser.DeadEnd) Elm.Syntax.File.File
parseElmModuleText =
    ElmEvaluation.parseElmModuleText


{-| Support function-level dead code elimination (<https://elm-lang.org/blog/small-assets-without-the-headache>)
Elm code needed to inform the Elm compiler about our entry points.
-}
main : Program Int () String
main =
    Platform.worker
        { init = \_ -> ( (), Cmd.none )
        , update =
            \event stateBefore ->
                ( parseElmModuleTextToJson "" |> always stateBefore, Cmd.none )
        , subscriptions = \_ -> Sub.none
        }
