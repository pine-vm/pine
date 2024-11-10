module Elm.Parser.Ranges exposing (withRange)

import Combine exposing (Parser)
import Elm.Parser.State exposing (State)
import Elm.Syntax.Range exposing (Range)


withRange : Parser State (Range -> a) -> Parser State a
withRange p =
    Combine.withLocation
        (\start ->
            p
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
