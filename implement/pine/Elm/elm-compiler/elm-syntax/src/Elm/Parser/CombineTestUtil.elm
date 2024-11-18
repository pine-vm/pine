module Elm.Parser.CombineTestUtil exposing (parse, parseWithFailure, parseWithState)

import Combine exposing (Parser)
import Elm.Parser.State exposing (State)
import Parser exposing (DeadEnd)


parseWithState : String -> Parser State a -> Maybe ( State, a )
parseWithState s p =
    case Combine.runParser (p |> Combine.ignore Combine.end) Elm.Parser.State.emptyState s of
        Ok ok ->
            Just ok

        Err _ ->
            Nothing


parse : String -> Parser State a -> Maybe a
parse s p =
    case parseWithState s p of
        Nothing ->
            Nothing

        Just ( _, r ) ->
            Just r


parseWithFailure : String -> Parser State a -> Result (List DeadEnd) a
parseWithFailure s p =
    Combine.runParser (p |> Combine.ignore Combine.end) Elm.Parser.State.emptyState s
        |> Result.map Tuple.second
