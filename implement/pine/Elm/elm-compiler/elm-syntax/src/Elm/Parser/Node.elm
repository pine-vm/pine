module Elm.Parser.Node exposing (parser)

import Combine exposing (Parser, succeed, withLocation)
import Elm.Parser.State exposing (State)
import Elm.Syntax.Node exposing (Node(..))


parser : Parser State a -> Parser State (Node a)
parser p =
    withLocation
        (\start ->
            succeed (\v r -> Node r v)
                |> Combine.keep p
                |> Combine.keep
                    (withLocation
                        (\end ->
                            succeed
                                { start = start
                                , end = end
                                }
                        )
                    )
        )
