module Elm.Syntax.Encode.Range exposing (..)

import Elm.Syntax.Range exposing (Location, Range)
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Construct an empty range
-}
empty : Range
empty =
    { start = { row = 0, column = 0 }
    , end = { row = 0, column = 0 }
    }


{-| **@deprecated** Use [`empty`](#empty) instead. It does the same thing but the name is more Elm-y.

Construct an empty range

-}
emptyRange : Range
emptyRange =
    empty


{-| Encode a range
-}
encode : Range -> Value
encode { start, end } =
    JE.list JE.int
        [ start.row
        , start.column
        , end.row
        , end.column
        ]


{-| Decode a range
-}
decoder : Decoder Range
decoder =
    JD.list JD.int
        |> JD.andThen fromList


fromList : List Int -> Decoder Range
fromList input =
    case input of
        [ a, b, c, d ] ->
            JD.succeed
                { start = { row = a, column = b }
                , end = { row = c, column = d }
                }

        _ ->
            JD.fail "Invalid input list"


{-| Compute the largest area of a list of ranges.
-}
combine : List Range -> Range
combine ranges =
    case ranges of
        [] ->
            empty

        head :: tail ->
            combineHelp tail head.start head.end


combineHelp : List Range -> Location -> Location -> Range
combineHelp ranges previousStart previousEnd =
    case ranges of
        [] ->
            { start = previousStart, end = previousEnd }

        { start, end } :: rest ->
            let
                newStart : Location
                newStart =
                    case compareLocations start previousStart of
                        LT ->
                            start

                        _ ->
                            previousStart

                newEnd : Location
                newEnd =
                    case compareLocations end previousEnd of
                        GT ->
                            end

                        _ ->
                            previousEnd
            in
            combineHelp rest newStart newEnd


{-| Compare the position of two Ranges.
-}
compare : Range -> Range -> Order
compare left right =
    case compareLocations left.start right.start of
        EQ ->
            compareLocations left.end right.end

        order ->
            order


{-| Compare two Locations.
-}
compareLocations : Location -> Location -> Order
compareLocations left right =
    if left.row < right.row then
        LT

    else if left.row > right.row then
        GT

    else
        Basics.compare left.column right.column
