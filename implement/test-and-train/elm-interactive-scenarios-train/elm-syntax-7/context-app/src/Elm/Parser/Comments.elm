module Elm.Parser.Comments exposing (multilineComment, singleLineComment)

import Combine exposing (Parser, modifyState, string, succeed)
import Elm.Parser.Node
import Elm.Parser.State exposing (State, addComment)
import Elm.Parser.Whitespace exposing (untilNewlineToken)
import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing (Nestable(..))


addCommentToState : Parser State (Node String) -> Parser State ()
addCommentToState p =
    p |> Combine.andThen (\pair -> modifyState (addComment pair))


parseComment : Parser State String -> Parser State ()
parseComment commentParser =
    Elm.Parser.Node.parser commentParser |> addCommentToState


singleLineComment : Parser State ()
singleLineComment =
    parseComment
        (succeed (++)
            |> Combine.keep (string "--")
            |> Combine.keep untilNewlineToken
        )


multilineCommentInner : Parser State String
multilineCommentInner =
    Core.getChompedString (Core.multiComment "{-" "-}" Nestable)
        |> Combine.fromCore


multilineComment : Parser State ()
multilineComment =
    parseComment multilineCommentInner
