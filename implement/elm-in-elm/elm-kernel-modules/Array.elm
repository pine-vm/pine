module Array exposing (..)


type alias Array a =
    List a


empty =
    []


isEmpty : Array a -> Bool
isEmpty array =
    Pine_builtin.equal [ array, [] ]


length : Array a -> Int
length array =
    Pine_builtin.length array


repeat : Int -> a -> Array a
repeat n value =
    List.repeat n value


get : Int -> Array a -> Maybe a
get index array =
    if Pine_builtin.int_is_sorted_asc [ 0, index ] then
        List.head (List.drop index array)

    else
        Nothing


set : Int -> a -> Array a -> Array a
set index value array =
    if
        Pine_builtin.negate (Pine_builtin.int_is_sorted_asc [ 0, index ])
            || Pine_builtin.int_is_sorted_asc [ Pine_builtin.length array, index ]
    then
        array

    else
        Pine_builtin.concat
            [ Pine_builtin.take [ index, array ]
            , [ value ]
            , Pine_builtin.skip [ index + 1, array ]
            ]


push : a -> Array a -> Array a
push element array =
    Pine_builtin.concat [ array, [ element ] ]


append : Array a -> Array a -> Array a
append first second =
    Pine_builtin.concat [ first, second ]


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
            (Pine_builtin.int_add [ n, -1 ])
        )


slice : Int -> Int -> Array a -> Array a
slice start end array =
    let
        sourceLength =
            Pine_builtin.length array

        startNormalized =
            if Pine_builtin.int_is_sorted_asc [ 0, start ] then
                start

            else
                Pine_builtin.int_add
                    [ sourceLength
                    , start
                    ]

        endNormalized =
            if Pine_builtin.int_is_sorted_asc [ 0, end ] then
                end

            else
                Pine_builtin.int_add
                    [ sourceLength
                    , end
                    ]

        takeCount =
            Pine_builtin.int_add
                [ endNormalized
                , Pine_builtin.int_mul [ -1, startNormalized ]
                ]
    in
    Pine_builtin.take
        [ takeCount
        , Pine_builtin.skip
            [ startNormalized
            , array
            ]
        ]
