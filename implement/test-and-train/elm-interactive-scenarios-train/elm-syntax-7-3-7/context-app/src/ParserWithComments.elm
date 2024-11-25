module ParserWithComments exposing
    ( Comments
    , WithComments
    , many
    , manyWithoutReverse
    , until
    )

import Elm.Syntax.Node exposing (Node)
import ParserFast exposing (Parser)
import Rope exposing (Rope)


type alias WithComments res =
    { comments : Comments, syntax : res }


type alias Comments =
    Rope (Node String)


until : Parser () -> Parser (WithComments a) -> Parser (WithComments (List a))
until end element =
    ParserFast.loopUntil
        end
        element
        ( Rope.empty, [] )
        (\pResult ( commentsSoFar, itemsSoFar ) ->
            ( commentsSoFar |> Rope.prependTo pResult.comments
            , pResult.syntax :: itemsSoFar
            )
        )
        (\( commentsSoFar, itemsSoFar ) ->
            { comments = commentsSoFar
            , syntax = List.reverse itemsSoFar
            }
        )


many : Parser (WithComments a) -> Parser (WithComments (List a))
many p =
    ParserFast.loopWhileSucceeds p
        ( Rope.empty, [] )
        (\pResult ( commentsSoFar, itemsSoFar ) ->
            ( commentsSoFar |> Rope.prependTo pResult.comments
            , pResult.syntax :: itemsSoFar
            )
        )
        (\( commentsSoFar, itemsSoFar ) ->
            { comments = commentsSoFar
            , syntax = List.reverse itemsSoFar
            }
        )


{-| Same as [`many`](#many), except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.

Mind you the comments will be reversed either way

-}
manyWithoutReverse : Parser (WithComments a) -> Parser (WithComments (List a))
manyWithoutReverse p =
    ParserFast.loopWhileSucceeds p
        ( Rope.empty, [] )
        (\pResult ( commentsSoFar, itemsSoFar ) ->
            ( commentsSoFar |> Rope.prependTo pResult.comments
            , pResult.syntax :: itemsSoFar
            )
        )
        (\( commentsSoFar, itemsSoFar ) ->
            { comments = commentsSoFar
            , syntax = itemsSoFar
            }
        )
