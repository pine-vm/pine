module Elm.Parser exposing
    ( parseToFile
    , parse
    )

{-|

@docs parseToFile
@docs parse

-}

import Elm.Internal.RawFile as InternalRawFile
import Elm.Parser.File exposing (file)
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.File exposing (File)
import Parser
import ParserFast


{-| **@deprecated** Use [`parseToFile`](#parseToFile) instead, which is simpler and doesn't require post-processing.
Since v7.3.3, post-processing is unnecessary.

Parse some text as if it is an Elm source file.
When parsing fails, the result will contain a list of errors indicating what went wrong (and/or where).
If it succeeds, you will get a `RawFile`.
This `RawFile` will require some post-processing to properly setup documentation and ensure that operator precedence is applied correctly (based on dependencies).
To process a `RawFile`, check out the `Processing` module.

-}
parse : String -> Result (List Parser.DeadEnd) RawFile
parse input =
    parseToFile input
        |> Result.map InternalRawFile.fromFile


{-| Parse some text as if it is an Elm source file.
When parsing fails, the result will contain a list of errors indicating what went wrong (and/or where).
-}
parseToFile : String -> Result (List Parser.DeadEnd) File
parseToFile input =
    ParserFast.run file input
