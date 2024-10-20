module Elm.Parser.CombineTestUtil exposing (parse, parseWithFailure, parseWithState)

import Combine exposing (Parser)
import Elm.Parser.State exposing (State, emptyState)
import Parser exposing (DeadEnd)


parseWithState : String -> Parser State a -> Maybe ( State, a )
parseWithState s p =
    Combine.runParser (p |> Combine.ignore Combine.end) emptyState s
        |> Result.toMaybe


parse : String -> Parser State a -> Maybe a
parse s p =
    parseWithState s p
        |> Maybe.map Tuple.second


parseWithFailure : String -> Parser State a -> Result (List DeadEnd) a
parseWithFailure s p =
    Combine.runParser (p |> Combine.ignore Combine.end) emptyState s
        |> Result.map Tuple.second
