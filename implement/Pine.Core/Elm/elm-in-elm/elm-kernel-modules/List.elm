module List exposing
    ( (::)
    , all
    , any
    , append
    , concat
    , concatMap
    , drop
    , filter
    , filterMap
    , foldl
    , foldr
    , head
    , indexedMap
    , intersperse
    , isEmpty
    , length
    , map
    , map2
    , map3
    , map4
    , map5
    , maximum
    , member
    , minimum
    , partition
    , product
    , range
    , repeat
    , reverse
    , singleton
    , sort
    , sortBy
    , sortWith
    , sum
    , tail
    , take
    , unzip
    )


infix right 5 (::) = cons


singleton : a -> List a
singleton value =
    [ value ]


repeat : Int -> a -> List a
repeat n value =
    repeatHelp [] n value


repeatHelp : List a -> Int -> a -> List a
repeatHelp result n value =
    if Pine_kernel.int_is_sorted_asc [ n, 0 ] then
        result

    else
        repeatHelp (cons value result) (n - 1) value


range : Int -> Int -> List Int
range lo hi =
    rangeHelp lo hi []


rangeHelp : Int -> Int -> List Int -> List Int
rangeHelp lo hi list =
    if Pine_kernel.int_is_sorted_asc [ lo, hi ] then
        rangeHelp lo (hi - 1) (cons hi list)

    else
        list


cons : a -> List a -> List a
cons element list =
    Pine_kernel.concat [ [ element ], list ]


map : (a -> b) -> List a -> List b
map f xs =
    mapHelp f xs []


mapHelp : (a -> b) -> List a -> List b -> List b
mapHelp f remaining acc =
    case remaining of
        x :: xs ->
            mapHelp f xs (Pine_kernel.concat [ [ f x ], acc ])

        _ ->
            Pine_kernel.reverse acc


indexedMap : (Int -> a -> b) -> List a -> List b
indexedMap f xs =
    indexedMapHelp f 0 xs []


indexedMapHelp : (Int -> a -> b) -> Int -> List a -> List b -> List b
indexedMapHelp f index xs acc =
    case xs of
        x :: following ->
            indexedMapHelp
                f
                (index + 1)
                following
                (Pine_kernel.concat [ [ f index x ], acc ])

        _ ->
            Pine_kernel.reverse acc


foldl : (a -> b -> b) -> b -> List a -> b
foldl func acc list_in_foldl =
    case list_in_foldl of
        x :: xs ->
            foldl func (func x acc) xs

        _ ->
            acc


foldr : (a -> b -> b) -> b -> List a -> b
foldr func acc list =
    foldl func
        acc
        (Pine_kernel.reverse list)


filter : (a -> Bool) -> List a -> List a
filter isGood list =
    filterHelp isGood list []


filterHelp : (a -> Bool) -> List a -> List a -> List a
filterHelp isGood list acc =
    case list of
        [] ->
            Pine_kernel.reverse acc

        x :: xs ->
            if isGood x then
                filterHelp isGood xs (Pine_kernel.concat [ [ x ], acc ])

            else
                filterHelp isGood xs acc


filterMap : (a -> Maybe b) -> List a -> List b
filterMap f xs =
    filterMapHelp f xs []


filterMapHelp : (a -> Maybe b) -> List a -> List b -> List b
filterMapHelp f xs acc =
    case xs of
        [] ->
            Pine_kernel.reverse acc

        x :: remaining ->
            case f x of
                Just value ->
                    filterMapHelp f remaining (Pine_kernel.concat [ [ value ], acc ])

                Nothing ->
                    filterMapHelp f remaining acc


maybeCons : (a -> Maybe b) -> a -> List b -> List b
maybeCons f mx xs =
    case f mx of
        Just x ->
            cons x xs

        Nothing ->
            xs


length : List a -> Int
length list =
    Pine_kernel.length list


reverse : List a -> List a
reverse list =
    Pine_kernel.reverse list


member : a -> List a -> Bool
member x xs =
    case xs of
        [] ->
            False

        y :: ys ->
            if x == y then
                True

            else
                member x ys


all : (a -> Bool) -> List a -> Bool
all isOkay list =
    case list of
        [] ->
            True

        x :: xs ->
            if isOkay x then
                all isOkay xs

            else
                False


any : (a -> Bool) -> List a -> Bool
any isOkay list =
    case list of
        [] ->
            False

        x :: xs ->
            if isOkay x then
                True

            else
                any isOkay xs


minimum : List comparable -> Maybe comparable
minimum list =
    case list of
        x :: xs ->
            Just (foldl min x xs)

        _ ->
            Nothing


maximum : List comparable -> Maybe comparable
maximum list =
    case list of
        x :: xs ->
            Just (foldl max x xs)

        _ ->
            Nothing


sum : List number -> number
sum numbers =
    foldl (\x acc -> x + acc) 0 numbers


product : List number -> number
product numbers =
    foldl (\x acc -> x * acc) 1 numbers


append : List a -> List a -> List a
append xs ys =
    Pine_kernel.concat [ xs, ys ]


concat : List (List a) -> List a
concat lists =
    Pine_kernel.concat lists


concatMap : (a -> List b) -> List a -> List b
concatMap f list =
    case list of
        x :: xs ->
            Pine_kernel.concat [ f x, concatMap f xs ]

        _ ->
            []


intersperse : a -> List a -> List a
intersperse sep xs =
    intersperseHelp
        (Pine_kernel.take [ 1, xs ])
        1
        sep
        xs


intersperseHelp : List a -> Int -> a -> List a -> List a
intersperseHelp acc offset sep xs =
    case Pine_kernel.take [ 1, Pine_kernel.skip [ offset, xs ] ] of
        [ x ] ->
            intersperseHelp
                (Pine_kernel.concat [ acc, [ sep, x ] ])
                (offset + 1)
                sep
                xs

        _ ->
            acc


map2 : (a -> b -> result) -> List a -> List b -> List result
map2 mapItems listA listB =
    case ( listA, listB ) of
        ( x :: xs, y :: ys ) ->
            cons (mapItems x y) (map2 mapItems xs ys)

        _ ->
            []


map3 : (a -> b -> c -> result) -> List a -> List b -> List c -> List result
map3 mapItems listA listB listC =
    case ( listA, listB, listC ) of
        ( x :: xs, y :: ys, z :: zs ) ->
            cons (mapItems x y z) (map3 mapItems xs ys zs)

        _ ->
            []


map4 : (a -> b -> c -> d -> result) -> List a -> List b -> List c -> List d -> List result
map4 mapItems listA listB listC listD =
    case ( listA, listB ) of
        ( x :: xs, y :: ys ) ->
            case ( listC, listD ) of
                ( z :: zs, w :: ws ) ->
                    cons (mapItems x y z w) (map4 mapItems xs ys zs ws)

                _ ->
                    []

        _ ->
            []


map5 : (a -> b -> c -> d -> e -> result) -> List a -> List b -> List c -> List d -> List e -> List result
map5 mapItems listA listB listC listD listE =
    case ( listA, listB ) of
        ( x :: xs, y :: ys ) ->
            case ( listC, listD ) of
                ( z :: zs, w :: ws ) ->
                    case listE of
                        v :: vs ->
                            cons (mapItems x y z w v) (map5 mapItems xs ys zs ws vs)

                        _ ->
                            []

                _ ->
                    []

        _ ->
            []


isEmpty : List a -> Bool
isEmpty xs =
    if Pine_kernel.equal [ Pine_kernel.length xs, 0 ] then
        True

    else
        False


head : List a -> Maybe a
head list =
    case list of
        x :: xs ->
            Just x

        _ ->
            Nothing


tail : List a -> Maybe (List a)
tail list =
    case list of
        x :: xs ->
            Just xs

        _ ->
            Nothing


take : Int -> List a -> List a
take n list =
    Pine_kernel.take [ n, list ]


drop : Int -> List a -> List a
drop n list =
    Pine_kernel.skip [ n, list ]


partition : (a -> Bool) -> List a -> ( List a, List a )
partition pred list =
    let
        step x ( trues, falses ) =
            if pred x then
                ( cons x trues, falses )

            else
                ( trues, cons x falses )
    in
    foldr step ( [], [] ) list


unzip : List ( a, b ) -> ( List a, List b )
unzip pairs =
    let
        step ( x, y ) ( xs, ys ) =
            ( cons x xs, cons y ys )
    in
    foldr step ( [], [] ) pairs


sort : List comparable -> List comparable
sort list =
    sortWith
        (\x y -> Basics.compare x y)
        list


sortBy : (a -> comparable) -> List a -> List a
sortBy toComparable list =
    sortWith
        (\x y -> Basics.compare (toComparable x) (toComparable y))
        list


sortWith : (a -> a -> Order) -> List a -> List a
sortWith compareFunc list =
    case list of
        [] ->
            list

        [ _ ] ->
            list

        _ ->
            let
                ( left, right ) =
                    sortWithSplit list
            in
            sortWithMerge
                (sortWith compareFunc left)
                (sortWith compareFunc right)
                compareFunc


sortWithSplit : List a -> ( List a, List a )
sortWithSplit list =
    let
        middleIndex =
            Pine_kernel.length list // 2
    in
    ( Pine_kernel.take [ middleIndex, list ]
    , Pine_kernel.skip [ middleIndex, list ]
    )


sortWithMerge : List a -> List a -> (a -> a -> Order) -> List a
sortWithMerge left right compareFunc =
    case ( left, right ) of
        ( [], _ ) ->
            right

        ( _, [] ) ->
            left

        ( x :: xs, y :: ys ) ->
            case compareFunc x y of
                GT ->
                    Pine_kernel.concat [ [ y ], sortWithMerge left ys compareFunc ]

                _ ->
                    Pine_kernel.concat [ [ x ], sortWithMerge xs right compareFunc ]
