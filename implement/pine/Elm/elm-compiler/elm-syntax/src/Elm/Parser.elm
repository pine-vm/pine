module Elm.Parser exposing
    ( parseToFile
    , parse
    )

{-|

@docs parseToFile
@docs parse

-}

import Combine exposing (Parser, end)
import Elm.Internal.RawFile as InternalRawFile
import Elm.Parser.File exposing (file)
import Elm.Parser.State exposing (State, emptyState)
import Elm.Processing as Processing
import Elm.RawFile exposing (RawFile)
import Elm.Syntax.File exposing (File)
import Parser exposing (DeadEnd)


{-| **@deprecated** Use [`parseToFile`](#parseToFile) instead, which is simpler and doesn't require post-processing.

Parse some text as if it is an Elm source file.
When parsing fails, the result will contain a list of errors indicating what went wrong (and/or where).
If it succeeds, you will get a `RawFile`.
This `RawFile` will require some post-processing to properly setup documentation and ensure that operator precedence is applied correctly (based on dependencies).
To process a `RawFile`, check out the `Processing` module.

-}
parse : String -> Result (List DeadEnd) RawFile
parse input =
    parseToFile input
        |> Result.map InternalRawFile.fromFile


{-| Parse some text as if it is an Elm source file.
When parsing fails, the result will contain a list of errors indicating what went wrong (and/or where).
-}
parseToFile : String -> Result (List DeadEnd) File
parseToFile input =
    -- A single line is added for unfinished ranges produced by `parser-combinators` on the last line.
    case Combine.runParser (withEnd file) emptyState (input ++ "\n") of
        Ok ( _, r ) ->
            Ok (Processing.process Processing.init (InternalRawFile.fromFile r))

        Err s ->
            Err s


withEnd : Parser State File -> Parser State File
withEnd p =
    p |> Combine.ignore end
