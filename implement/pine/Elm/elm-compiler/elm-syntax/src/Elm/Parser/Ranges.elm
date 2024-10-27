module Elm.Parser.Ranges exposing (withRange)

import Combine exposing (Parser, succeed, withLocation)
import Elm.Parser.State exposing (State)
import Elm.Syntax.Range exposing (Range)


withRange : Parser State (Range -> a) -> Parser State a
withRange p =
    withLocation
        (\start ->
            p
                |> Combine.keep
                    (withLocation
                        (\end ->
                            succeed <|
                                { start = start
                                , end = end
                                }
                        )
                    )
        )
