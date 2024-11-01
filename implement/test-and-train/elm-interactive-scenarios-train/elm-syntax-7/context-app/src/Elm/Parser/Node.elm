module Elm.Parser.Node exposing (parser)

import Combine exposing (Parser)
import Elm.Parser.State exposing (State)
import Elm.Syntax.Node exposing (Node(..))


parser : Parser State a -> Parser State (Node a)
parser p =
    Combine.withLocation
        (\start ->
            Combine.succeed (\v r -> Node r v)
                |> Combine.keep p
                |> Combine.keep
                    (Combine.withLocation
                        (\end ->
                            Combine.succeed
                                { start = start
                                , end = end
                                }
                        )
                    )
        )
