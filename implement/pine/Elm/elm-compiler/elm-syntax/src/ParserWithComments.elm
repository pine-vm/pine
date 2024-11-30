module ParserWithComments exposing
    ( Comments
    , WithComments(..)
    , many
    , manyWithoutReverse
    , until
    )

import Elm.Syntax.Node exposing (Node)
import ParserFast exposing (Parser)
import Rope exposing (Rope)


type WithComments res
    = WithComments Comments res


type alias Comments =
    Rope (Node String)


until : Parser () -> Parser (WithComments a) -> Parser (WithComments (List a))
until end element =
    ParserFast.loopUntil
        end
        element
        ( Rope.empty, [] )
        (\(WithComments comments syntax) ( commentsSoFar, itemsSoFar ) ->
            ( commentsSoFar |> Rope.prependTo comments
            , syntax :: itemsSoFar
            )
        )
        (\( commentsSoFar, itemsSoFar ) ->
            WithComments
                commentsSoFar
                (List.reverse itemsSoFar)
        )


many : Parser (WithComments a) -> Parser (WithComments (List a))
many p =
    ParserFast.loopWhileSucceeds p
        ( Rope.empty, [] )
        (\(WithComments comments syntax) ( commentsSoFar, itemsSoFar ) ->
            ( commentsSoFar |> Rope.prependTo comments
            , syntax :: itemsSoFar
            )
        )
        (\( commentsSoFar, itemsSoFar ) ->
            WithComments
                commentsSoFar
                (List.reverse itemsSoFar)
        )


{-| Same as [`many`](#many), except that it doesn't reverse the list.
This can be useful if you need to access the range of the last item.

Mind you the comments will be reversed either way

-}
manyWithoutReverse : Parser (WithComments a) -> Parser (WithComments (List a))
manyWithoutReverse p =
    ParserFast.loopWhileSucceeds p
        ( Rope.empty, [] )
        (\(WithComments comments syntax) ( commentsSoFar, itemsSoFar ) ->
            ( commentsSoFar |> Rope.prependTo comments
            , syntax :: itemsSoFar
            )
        )
        (\( commentsSoFar, itemsSoFar ) ->
            WithComments
                commentsSoFar
                itemsSoFar
        )
