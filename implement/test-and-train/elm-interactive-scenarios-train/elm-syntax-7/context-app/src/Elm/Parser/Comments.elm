module Elm.Parser.Comments exposing (multilineComment, singleLineComment)

import Combine exposing (Parser)
import Elm.Parser.Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Whitespace
import Elm.Syntax.Node exposing (Node)
import Parser as Core exposing (Nestable(..))


addCommentToState : Parser State (Node String) -> Parser State ()
addCommentToState p =
    p |> Combine.andThen (\pair -> Combine.modifyState (Elm.Parser.State.addComment pair))


parseComment : Parser State String -> Parser State ()
parseComment commentParser =
    Elm.Parser.Node.parser commentParser |> addCommentToState


singleLineComment : Parser State ()
singleLineComment =
    parseComment
        (Combine.succeed (\a b -> String.concat [ a, b ])
            |> Combine.keep (Combine.string "--")
            |> Combine.keep Elm.Parser.Whitespace.untilNewlineToken
        )


multilineCommentInner : Parser State String
multilineCommentInner =
    Core.getChompedString (Core.multiComment "{-" "-}" Nestable)
        |> Combine.fromCore


multilineComment : Parser State ()
multilineComment =
    parseComment multilineCommentInner
