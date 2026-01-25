module Array exposing (..)

import Basics
import Bitwise
import List
import Maybe exposing (Maybe(..))
import Tuple


empty =
    []


isEmpty : Array a -> Bool
isEmpty array =
    Pine_kernel.equal [ array, [] ]


length : Array a -> Int
length array =
    Pine_kernel.length array


repeat : Int -> a -> Array a
repeat n value =
    List.repeat n value


get : Int -> Array a -> Maybe a
get index array =
    if Pine_kernel.int_is_sorted_asc [ 0, index ] then
        List.head (List.drop index array)

    else
        Nothing


set : Int -> a -> Array a -> Array a
set index value array =
    if
        Pine_kernel.negate (Pine_kernel.int_is_sorted_asc [ 0, index ])
            || Pine_kernel.int_is_sorted_asc [ Pine_kernel.length array, index ]
    then
        array

    else
        Pine_kernel.concat
            [ Pine_kernel.take [ index, array ]
            , [ value ]
            , Pine_kernel.skip [ index + 1, array ]
            ]


push : a -> Array a -> Array a
push element array =
    Pine_kernel.concat [ array, [ element ] ]


append : Array a -> Array a -> Array a
append first second =
    Pine_kernel.concat [ first, second ]


fromList : List a -> Array a
fromList list =
    list


toList : Array a -> List a
toList array =
    array


map : (a -> b) -> Array a -> Array b
map mapItem array =
    List.map mapItem array


indexedMap : (Int -> a -> b) -> Array a -> Array b
indexedMap mapItem array =
    List.indexedMap mapItem array


foldr : (a -> b -> b) -> b -> Array a -> b
foldr foldItem seed array =
    List.foldr foldItem seed array


foldl : (a -> b -> b) -> b -> Array a -> b
foldl foldItem seed array =
    List.foldl foldItem seed array


filter : (a -> Bool) -> Array a -> Array a
filter filterItem array =
    List.filter filterItem array


initialize : Int -> (Int -> a) -> Array a
initialize n init =
    List.map
        init
        (List.range
            0
            (Pine_kernel.int_add [ n, -1 ])
        )


slice : Int -> Int -> Array a -> Array a
slice start end array =
    let
        sourceLength =
            Pine_kernel.length array

        startNormalized =
            if Pine_kernel.int_is_sorted_asc [ 0, start ] then
                start

            else
                Pine_kernel.int_add
                    [ sourceLength
                    , start
                    ]

        endNormalized =
            if Pine_kernel.int_is_sorted_asc [ 0, end ] then
                end

            else
                Pine_kernel.int_add
                    [ sourceLength
                    , end
                    ]

        takeCount =
            Pine_kernel.int_add
                [ endNormalized
                , Pine_kernel.int_mul [ -1, startNormalized ]
                ]
    in
    Pine_kernel.take
        [ takeCount
        , Pine_kernel.skip
            [ startNormalized
            , array
            ]
        ]
