module Elm.Syntax.Range exposing
    ( Range, Location
    , empty, combine
    , compare, compareLocations
    , emptyRange
    )

{-|


## Types

@docs Range, Location


## Functions

@docs empty, combine


## Comparison

See also [Basics.compare](https://package.elm-lang.org/packages/elm/core/latest/Basics#compare).

@docs compare, compareLocations


## Serialization

@docs encode, decoder


## Deprecated

@docs emptyRange

-}


{-| Source location
-}
type alias Location =
    { row : Int
    , column : Int
    }


{-| Range for a piece of code with a start and end
-}
type alias Range =
    { start : Location
    , end : Location
    }


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
