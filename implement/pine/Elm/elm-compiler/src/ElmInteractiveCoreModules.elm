module ElmInteractiveCoreModules exposing (..)


elmCoreModulesTexts : List String
elmCoreModulesTexts =
    [ """
module Basics exposing
  ( Int, Float
  , (+), (-), (*), (/), (//), (^)
  , toFloat, round, floor, ceiling, truncate
  , (==), (/=)
  , (<), (>), (<=), (>=), max, min, compare, Order(..)
  , Bool(..), not, (&&), (||), xor
  , (++)
  , modBy, remainderBy, negate, abs, clamp, sqrt, logBase, e
  , pi, cos, sin, tan, acos, asin, atan, atan2
  , degrees, radians, turns
  , toPolar, fromPolar
  , isNaN, isInfinite
  , identity, always, (<|), (|>), (<<), (>>), Never, never
  )


infix right 0 (<|) = apL
infix left  0 (|>) = apR
infix right 2 (||) = or
infix right 3 (&&) = and
infix non   4 (==) = eq
infix non   4 (/=) = neq
infix non   4 (<)  = lt
infix non   4 (>)  = gt
infix non   4 (<=) = le
infix non   4 (>=) = ge
infix right 5 (++) = append
infix left  6 (+)  = add
infix left  6 (-)  = sub
infix left  7 (*)  = mul
infix left  7 (//) = idiv
infix right 8 (^)  = pow
infix left  9 (<<) = composeL
infix right 9 (>>) = composeR


type Bool = True | False


type String
    = String Int

    -- We need another tag to prevent the compiler from assuming that the condition for tag 'String' is always true.
    | AnyOtherKind_String


type Elm_Float
    = Elm_Float Int Int

    -- We need another tag to prevent the compiler from assuming that the condition for tag 'String' is always true.
    | AnyOtherKind_Float


{-| Represents the relative ordering of two things.
The relations are less than, equal to, and greater than.
-}
type Order = LT | EQ | GT


eq : a -> a -> Bool
eq a b =
    if Pine_kernel.equal [ a, b ] then
        True

    else
        case ( a, b ) of
            ( Elm_Float numA denomA, intB ) ->
                if Pine_kernel.equal [ numA, intB ] then
                    Pine_kernel.equal [ denomA, 1 ]

                else
                    False

            ( intA, Elm_Float numB denomB ) ->
                if Pine_kernel.equal [ intA, numB ] then
                    Pine_kernel.equal [ denomB, 1 ]

                else
                    False

            _ ->
                if isPineBlob a then
                    False

                else if Pine_kernel.equal [ Pine_kernel.length a, Pine_kernel.length b ] then
                    case a of
                        String _ ->
                            False

                        RBNode_elm_builtin _ _ _ _ _ ->
                            Pine_kernel.equal [ dictToList a, dictToList b ]

                        Set_elm_builtin dictA ->
                            let
                                (Set_elm_builtin dictB) =
                                    b
                            in
                            Pine_kernel.equal [ dictKeys dictA, dictKeys dictB ]

                        _ ->
                            listsEqualRecursive a b

                else
                    False


listsEqualRecursive : List comparable -> List comparable -> Bool
listsEqualRecursive listA listB =
    if Pine_kernel.equal [ listA, [] ] then
        True

    else
        if eq (Pine_kernel.head listA) (Pine_kernel.head listB) then
            listsEqualRecursive
                (Pine_kernel.skip [ 1, listA ])
                (Pine_kernel.skip [ 1, listB ])

        else
            False


dictToList : Dict k v -> List (k,v)
dictToList dict =
  case dict of
    RBEmpty_elm_builtin ->
      []

    RBNode_elm_builtin _ key value left right ->
      Pine_kernel.concat [ dictToList left, [ (key, value) ], dictToList right ]


dictKeys : Dict k v -> List k
dictKeys dict =
  case dict of
    RBEmpty_elm_builtin ->
      []

    RBNode_elm_builtin _ key value left right ->
      Pine_kernel.concat [ dictKeys left, [ key ], dictKeys right ]


neq : a -> a -> Bool
neq a b =
    not (eq a b)


add : number -> number -> number
add a b =
    Pine_kernel.int_add [ a, b ]


sub : number -> number -> number
sub a b =
    Pine_kernel.int_add [ a, Pine_kernel.negate b ]


mul : number -> number -> number
mul a b =
    case ( a, b ) of
        ( Elm_Float numA denomA, Elm_Float numB denomB ) ->
            let
                newNumerator =
                    Pine_kernel.int_mul [ numA, numB ]

                newDenominator =
                    Pine_kernel.int_mul [ denomA, denomB ]
            in
            simplifyFraction (Elm_Float newNumerator newDenominator)

        ( Elm_Float numA denomA, intB ) ->
            let
                newNumerator =
                    Pine_kernel.int_mul [ numA, intB ]
            in
            simplifyFraction (Elm_Float newNumerator denomA)


        ( intA, Elm_Float numB denomB ) ->
            let
                newNumerator =
                    Pine_kernel.int_mul [ intA, numB ]
            in
            simplifyFraction (Elm_Float newNumerator denomB)


        _ ->
            Pine_kernel.int_mul [ a, b ]


idiv : Int -> Int -> Int
idiv dividend divisor =
    if Pine_kernel.equal [ divisor, 0 ] then
        0

    else
        let
            ( dividendNegative, absDividend ) =
                if Pine_kernel.int_is_sorted_asc [ 0, dividend ] then
                    ( False, dividend )

                else
                    ( True, -dividend )

            ( divisorNegative, absDivisor ) =
                if Pine_kernel.int_is_sorted_asc [ 0, divisor ] then
                    ( False, divisor )

                else
                    ( True, -divisor )

            absQuotient : Int
            absQuotient =
                idivHelper absDividend absDivisor 0
        in
        if Pine_kernel.equal [ dividendNegative, divisorNegative ] then
            absQuotient

        else
            Pine_kernel.negate absQuotient


idivHelper : Int -> Int -> Int -> Int
idivHelper dividend divisor quotient =
    let
        scaledDivisor : Int
        scaledDivisor =
            Pine_kernel.int_mul [ divisor, 16 ]
    in
    if Pine_kernel.int_is_sorted_asc [ scaledDivisor, dividend ] then
        let
            scaledQuotient : Int
            scaledQuotient =
                idivHelper
                    dividend
                    scaledDivisor
                    0

            scaledQuotientSum : Int
            scaledQuotientSum =
                Pine_kernel.int_mul [ scaledQuotient, 16 ]

            remainder : Int
            remainder =
                Pine_kernel.int_add
                    [ dividend
                    , Pine_kernel.negate (Pine_kernel.int_mul [ scaledQuotient, scaledDivisor ])
                    ]

            remainderQuotient : Int
            remainderQuotient =
                idivHelper remainder divisor 0
        in
        Pine_kernel.int_add [ scaledQuotientSum, remainderQuotient ]

    else if Pine_kernel.int_is_sorted_asc [ divisor, dividend ] then
        idivHelper
            (Pine_kernel.int_add [ dividend, Pine_kernel.negate divisor ])
            divisor
            (Pine_kernel.int_add [ quotient, 1 ])

    else
        quotient


simplifyFraction : Float -> number
simplifyFraction (Elm_Float numerator denominator) =
    let
        gcdValue =
            gcd (abs numerator) (abs denominator)

        simplifiedNumerator =
            idiv numerator gcdValue

        simplifiedDenominator =
            idiv denominator gcdValue
    in
    if Pine_kernel.equal [ simplifiedDenominator, 1 ] then
        simplifiedNumerator

    else
        Elm_Float simplifiedNumerator simplifiedDenominator


gcd : Int -> Int -> Int
gcd a b =
    if Pine_kernel.equal [ b, 0 ] then
        a

    else
        gcd b (modBy b a)


pow : Int -> Int -> Int
pow base exponent =
    if Pine_kernel.int_is_sorted_asc [ exponent, 0 ] then
        1

    else
        powHelper base exponent 1


powHelper : Int -> Int -> Int -> Int
powHelper base exponent accumulator =
    if Pine_kernel.equal [ exponent, 0 ] then
        accumulator

    else
        powHelper base (Pine_kernel.int_add [ exponent, -1 ]) (Pine_kernel.int_mul [ base, accumulator ])


and : Bool -> Bool -> Bool
and a b =
    if a then
        b
    else
        False


or : Bool -> Bool -> Bool
or a b =
    if a then
        True
    else
        b


append : appendable -> appendable -> appendable
append a b =
    case (a, b) of
    (String stringA, String stringB) ->
        String (Pine_kernel.concat [ stringA, stringB ])
    _ -> Pine_kernel.concat [ a, b ]


lt : comparable -> comparable -> Bool
lt a b =
    Pine_kernel.equal [ compare a b, LT ]


gt : comparable -> comparable -> Bool
gt a b =
    Pine_kernel.equal [ compare a b, GT ]


le : comparable -> comparable -> Bool
le a b =
    if Pine_kernel.equal [ a, b ] then
        True

    else
        Pine_kernel.equal [ compare a b, LT ]


ge : comparable -> comparable -> Bool
ge a b =
    if Pine_kernel.equal [ a, b ] then
        True

    else
        Pine_kernel.equal [ compare a b, GT ]


{-| Find the smaller of two comparables.

    min 42 12345678 == 42
    min "abc" "xyz" == "abc"
-}
min : comparable -> comparable -> comparable
min x y =
    if lt x y then x else y


{-| Find the larger of two comparables.

    max 42 12345678 == 12345678
    max "abc" "xyz" == "xyz"
-}
max : comparable -> comparable -> comparable
max x y =
    if gt x y then x else y


apR : a -> (a -> b) -> b
apR x f =
    f x


apL : (a -> b) -> a -> b
apL f x =
    f x


composeL : (b -> c) -> (a -> b) -> (a -> c)
composeL g f x =
    g (f x)


composeR : (a -> b) -> (b -> c) -> (a -> c)
composeR f g x =
    g (f x)


identity : a -> a
identity x =
    x


always : a -> b -> a
always a _ =
    a


not : Bool -> Bool
not bool =
    Pine_kernel.equal [ bool, False ]


{-| Compare any two comparable values. Comparable values include `String`,
`Char`, `Int`, `Float`, or a list or tuple containing comparable values. These
are also the only values that work as `Dict` keys or `Set` members.

    compare 3 4 == LT
    compare 4 4 == EQ
    compare 5 4 == GT
-}
compare : comparable -> comparable -> Order
compare a b =
    if Pine_kernel.equal [ a, b ] then
        EQ

    else
        case ( a, b ) of
            ( String stringA, String stringB ) ->
                compareStrings 0 stringA stringB

            ( Elm_Float numA denomA, Elm_Float numB denomB ) ->
                let
                    leftProduct =
                        Pine_kernel.int_mul [ numA, denomB ]

                    rightProduct =
                        Pine_kernel.int_mul [ numB, denomA ]
                in
                if Pine_kernel.equal [ leftProduct, rightProduct ] then
                    EQ

                else if Pine_kernel.int_is_sorted_asc [ leftProduct, rightProduct ] then
                    LT

                else
                    GT

            ( Elm_Float numA denomA, intB ) ->
                let
                    leftProduct =
                        numA

                    rightProduct =
                        Pine_kernel.int_mul [ denomA, intB ]
                in
                if Pine_kernel.equal [ leftProduct, rightProduct ] then
                    EQ

                else if Pine_kernel.int_is_sorted_asc [ leftProduct, rightProduct ] then
                    LT

                else
                    GT


            ( intA, Elm_Float numB denomB ) ->
                let
                    leftProduct =
                        Pine_kernel.int_mul [ intA, denomB ]

                    rightProduct =
                        numB
                in
                if Pine_kernel.equal [ leftProduct, rightProduct ] then
                    EQ

                else if Pine_kernel.int_is_sorted_asc [ leftProduct, rightProduct ] then
                    LT

                else
                    GT

            _ ->
                if isPineList a then
                    compareList a b

                else if Pine_kernel.int_is_sorted_asc [ a, b ] then
                    LT

                else
                    GT


compareList : List comparable -> List comparable -> Order
compareList listA listB =
    case listA of
        [] ->
            case listB of
                [] ->
                    EQ

                _ ->
                    LT

        headA :: tailA ->
            case listB of
                [] ->
                    GT

                headB :: tailB ->
                    let
                        headOrder =
                            compare headA headB
                    in
                    if Pine_kernel.equal [ headOrder, EQ ] then
                        compareList tailA tailB

                    else
                        headOrder


compareStrings : Int -> Int -> Int -> Order
compareStrings offset stringA stringB =
    let
        charA =
            Pine_kernel.take
                [ 4
                , Pine_kernel.skip [ offset, stringA ]
                ]

        charB =
            Pine_kernel.take
                [ 4
                , Pine_kernel.skip [ offset, stringB ]
                ]
    in
    if Pine_kernel.equal [ Pine_kernel.length charA, 0 ] then
        if Pine_kernel.equal [ Pine_kernel.length charB, 0 ] then
            EQ

        else
            LT

    else if Pine_kernel.equal [ Pine_kernel.length charB, 0 ] then
        GT

    else if Pine_kernel.equal [ charA, charB ] then
        compareStrings
            (Pine_kernel.int_add [ offset, 4 ])
            stringA
            stringB

    else if
        -- Pine_kernel.int_is_sorted_asc only works with signed integers. Therefore prepend sign to each character.
        Pine_kernel.int_is_sorted_asc
            [ Pine_kernel.concat [ 0, charA ]
            , Pine_kernel.concat [ 0, charB ]
            ]
    then
        LT

    else
        GT


modBy : Int -> Int -> Int
modBy divisor dividend =
    let
        remainder = remainderBy divisor dividend
    in
        if Pine_kernel.int_is_sorted_asc [ 0, remainder ] then
            remainder

        else
            Pine_kernel.int_add [ remainder, divisor ]


remainderBy : Int -> Int -> Int
remainderBy divisor dividend =
    Pine_kernel.int_add
        [ dividend
        , Pine_kernel.negate (Pine_kernel.int_mul [ divisor, (idiv dividend divisor)])
        ]


{-| Negate a number.

    negate 42 == -42
    negate -42 == 42
    negate 0 == 0
-}
negate : number -> number
negate n =
    case n of
        Elm_Float numerator denominator ->
            Elm_Float (Pine_kernel.negate numerator) denominator

        _ ->
            Pine_kernel.negate n


{-| Get the [absolute value][abs] of a number.

    abs 16   == 16
    abs -4   == 4
    abs -8.5 == 8.5
    abs 3.14 == 3.14

[abs]: https://en.wikipedia.org/wiki/Absolute_value
-}
abs : number -> number
abs n =
    if Pine_kernel.int_is_sorted_asc [ 0, n ] then
        n
    else
        Pine_kernel.negate n


{-| Clamps a number within a given range. With the expression
`clamp 100 200 x` the results are as follows:

    100     if x < 100
     x      if 100 <= x < 200
    200     if 200 <= x
-}
clamp : number -> number -> number -> number
clamp low high number =
  if lt number low then
    low
  else if gt number high then
    high
  else
    number


isPineList a =
    Pine_kernel.equal [ Pine_kernel.take [ 0, a ], [] ]


isPineBlob a =
    Pine_kernel.equal [ Pine_kernel.take [ 0, a ], Pine_kernel.take [ 0, 0 ] ]


toFloat : number -> Float
toFloat number =
    case number of
        Elm_Float _ _ ->
            number

        _ ->
            Elm_Float number 1


floor : Float -> Int
floor number =
    case number of
        Elm_Float numerator denom ->
            if Pine_kernel.int_is_sorted_asc [ 0, numerator ] then
                ratioFloor numerator denom

            else
                Pine_kernel.negate
                    (ratioFloor (Pine_kernel.negate numerator) denom)

        _ ->
            number


ratioFloor : Int -> Int -> Int
ratioFloor numerator denom =
    let
        (multiplier, denomProd) =
            findMultiplierToDecimal 1 denom
    in
    idiv
        (Pine_kernel.int_mul [ numerator, multiplier ])
        denomProd


findMultiplierToDecimal : Int -> Int -> (Int, Int, Int)
findMultiplierToDecimal factor denom =
    let
        denomProd =
            Pine_kernel.int_mul [ denom, factor ]

        lowerPowerOfTen =
            findLowerPowerOfTen denomProd
    in
    if Pine_kernel.equal [ pow 10 lowerPowerOfTen, denomProd ] then
        (factor, denomProd)

    else
        findMultiplierToDecimal
            (Pine_kernel.int_add [ factor, 1 ])
            denom


findLowerPowerOfTen : Int -> Int
findLowerPowerOfTen int =
    if Pine_kernel.int_is_sorted_asc [ int, 9 ] then
        0

    else
        Pine_kernel.int_add [ findLowerPowerOfTen (idiv int 10), 1 ]


isNaN : Float -> Bool
isNaN number =
    case number of
        Elm_Float numerator denom ->
            if Pine_kernel.equal [ denom, 0 ] then
                Pine_kernel.equal [ numerator, 0 ]

            else
                False

        _ ->
            False

"""
    , """
module Tuple exposing
  ( pair
  , first, second
  , mapFirst, mapSecond, mapBoth
  )


pair : a -> b -> (a, b)
pair a b =
  (a, b)


first : (a, b) -> a
first (x,_) =
    x


second : (a, b) -> b
second (_,y) =
    y


mapFirst : (a -> x) -> (a, b) -> (x, b)
mapFirst func (x,y) =
    (func x, y)


mapSecond : (b -> y) -> (a, b) -> (a, y)
mapSecond func (x,y) =
    (x, func y)


mapBoth : (a -> x) -> (b -> y) -> (a, b) -> (x, y)
mapBoth funcA funcB (x,y) =
    ( funcA x, funcB y )

"""
    , """
module Maybe exposing (..)


type Maybe a
    = Just a
    | Nothing


withDefault : a -> Maybe a -> a
withDefault default maybe =
    case maybe of
        Just value -> value
        Nothing -> default


map : (a -> b) -> Maybe a -> Maybe b
map f maybe =
    case maybe of
        Just value ->
            Just (f value)

        Nothing ->
            Nothing


map2 : (a -> b -> value) -> Maybe a -> Maybe b -> Maybe value
map2 func ma mb =
    case ma of
        Nothing ->
            Nothing

        Just a ->
            case mb of
                Nothing ->
                    Nothing

                Just b ->
                    Just (func a b)


andThen : (a -> Maybe b) -> Maybe a -> Maybe b
andThen callback maybeValue =
    case maybeValue of
        Just value ->
            callback value

        Nothing ->
            Nothing

"""
    , -- https://github.com/elm/core/blob/84f38891468e8e153fc85a9b63bdafd81b24664e/src/List.elm
      """
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

import Basics
import Maybe exposing (Maybe(..))


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
        [] ->
            Pine_kernel.reverse acc

        x :: xs ->
            mapHelp f xs (Pine_kernel.concat [ [ f x ], acc ])


indexedMap : (Int -> a -> b) -> List a -> List b
indexedMap f xs =
    indexedMapHelp f 0 xs []


indexedMapHelp : (Int -> a -> b) -> Int -> List a -> List b -> List b
indexedMapHelp f index xs acc =
    case xs of
        [] ->
            Pine_kernel.reverse acc

        x :: following ->
            indexedMapHelp
                f
                (index + 1)
                following
                (Pine_kernel.concat [ [ f index x ], acc ])


foldl : (a -> b -> b) -> b -> List a -> b
foldl func acc list_in_foldl =
    case list_in_foldl of
        [] ->
            acc

        x :: xs ->
            foldl func (func x acc) xs


foldr : (a -> b -> b) -> b -> List a -> b
foldr func acc list =
    foldl func acc (reverse list)


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
    foldl (\\x acc -> x + acc) 0 numbers


append : List a -> List a -> List a
append xs ys =
    Pine_kernel.concat [ xs, ys ]


concat : List (List a) -> List a
concat lists =
    Pine_kernel.concat lists


concatMap : (a -> List b) -> List a -> List b
concatMap f list =
    case list of
        [] ->
            []

        x :: xs ->
            Pine_kernel.concat [ f x, concatMap f xs ]


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
        ( [], _ ) ->
            []

        ( _, [] ) ->
            []

        ( x :: xs, y :: ys ) ->
            cons (mapItems x y) (map2 mapItems xs ys)


map3 : (a -> b -> c -> result) -> List a -> List b -> List c -> List result
map3 mapItems listA listB listC =
    case ( listA, listB, listC ) of
        ( [], _, _ ) ->
            []

        ( _, [], _ ) ->
            []

        ( _, _, [] ) ->
            []

        ( x :: xs, y :: ys, z :: zs ) ->
            cons (mapItems x y z) (map3 mapItems xs ys zs)


isEmpty : List a -> Bool
isEmpty xs =
    case xs of
        [] ->
            True

        _ ->
            False


head : List a -> Maybe a
head list =
    case list of
        x :: xs ->
            Just x

        [] ->
            Nothing


tail : List a -> Maybe (List a)
tail list =
    case list of
        x :: xs ->
            Just xs

        [] ->
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


sort : List comparable -> List comparable
sort list =
    sortWith
        (\\x y -> Basics.compare x y)
        list


sortBy : (a -> comparable) -> List a -> List a
sortBy toComparable list =
    sortWith
        (\\x y -> Basics.compare (toComparable x) (toComparable y))
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

"""
    , """
module Char exposing (..)

import Basics exposing ((&&), (<=), (>=), (||), Bool, Int)


type alias Char =
    Int


toCode : Char -> Int
toCode char =
    Pine_kernel.int_add
        [ -- Add the sign prefix byte
          Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]

        -- Use kernel function 'add' to ensure canonical form
        , 0
        ]


fromCode : Int -> Char
fromCode code =
    -- Remove the sign prefix byte
    Pine_kernel.reverse
        (Pine_kernel.take
            [ 4
            , Pine_kernel.concat
                [ Pine_kernel.reverse (Pine_kernel.skip [ 1, code ])
                , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                ]
            ]
        )


{-| Detect digits `0123456789`

    isDigit '0' == True

    isDigit '1'
        == True
        ... isDigit '9'
        == True

    isDigit 'a' == False

    isDigit 'b' == False

    isDigit 'A' == False

-}
isDigit : Char -> Bool
isDigit char =
    let
        code : Int
        code =
            Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]
    in
    Pine_kernel.int_is_sorted_asc [ 0x30, code, 0x39 ]


{-| Detect octal digits `01234567`

    isOctDigit '0' == True

    isOctDigit '1'
        == True
        ... isOctDigit '7'
        == True

    isOctDigit '8' == False

    isOctDigit 'a' == False

    isOctDigit 'A' == False

-}
isOctDigit : Char -> Bool
isOctDigit char =
    let
        code : Int
        code =
            Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]
    in
    Pine_kernel.int_is_sorted_asc [ 0x30, code, 0x37 ]


{-| Detect hexadecimal digits `0123456789abcdefABCDEF`
-}
isHexDigit : Char -> Bool
isHexDigit char =
    let
        code : Int
        code =
            Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]
    in
    Pine_kernel.int_is_sorted_asc [ 0x30, code, 0x39 ]
        || Pine_kernel.int_is_sorted_asc [ 0x41, code, 0x46 ]
        || Pine_kernel.int_is_sorted_asc [ 0x61, code, 0x66 ]


isUpper : Char -> Bool
isUpper char =
    let
        code : Int
        code =
            Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]
    in
    Pine_kernel.int_is_sorted_asc [ 0x41, code, 0x5A ]


isLower : Char -> Bool
isLower char =
    let
        code : Int
        code =
            Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]
    in
    Pine_kernel.int_is_sorted_asc [ 0x61, code, 0x7A ]


isAlpha : Char -> Bool
isAlpha char =
    let
        code : Int
        code =
            Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]
    in
    if Pine_kernel.int_is_sorted_asc [ 0x41, code, 0x5A ] then
        True

    else
        Pine_kernel.int_is_sorted_asc [ 0x61, code, 0x7A ]


isAlphaNum : Char -> Bool
isAlphaNum char =
    let
        code =
            Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]
    in
    if Pine_kernel.int_is_sorted_asc [ 0x41, code, 0x5A ] then
        True

    else if Pine_kernel.int_is_sorted_asc [ 0x61, code, 0x7A ] then
        True

    else
        Pine_kernel.int_is_sorted_asc [ 0x30, code, 0x39 ]


toUpper : Char -> Char
toUpper char =
    let
        code =
            Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]
    in
    if Pine_kernel.int_is_sorted_asc [ 0x61, code, 0x7A ] then
        Pine_kernel.concat
            [ Pine_kernel.take [ 3, Pine_kernel.skip [ 2, 0x0000000100000000 ] ]
            , Pine_kernel.skip [ 1, Pine_kernel.int_add [ code, -0x20 ] ]
            ]

    else
        char


toLower : Char -> Char
toLower char =
    let
        code =
            Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]
    in
    if Pine_kernel.int_is_sorted_asc [ 0x41, code, 0x5A ] then
        Pine_kernel.concat
            [ Pine_kernel.take [ 3, Pine_kernel.skip [ 2, 0x0000000100000000 ] ]
            , Pine_kernel.skip [ 1, Pine_kernel.int_add [ code, 0x20 ] ]
            ]

    else
        char

"""
    , """
module String exposing
    ( String
    , all
    , any
    , append
    , concat
    , cons
    , contains
    , dropLeft
    , dropRight
    , endsWith
    , filter
    , foldl
    , foldr
    , fromChar
    , fromFloat
    , fromInt
    , fromList
    , indexes
    , indices
    , isEmpty
    , join
    , left
    , length
    , lines
    , map
    , pad
    , padLeft
    , padRight
    , repeat
    , replace
    , reverse
    , right
    , slice
    , split
    , startsWith
    , toFloat
    , toInt
    , toList
    , toListRecursive
    , toLower
    , toUpper
    , trim
    , trimLeft
    , trimRight
    , uncons
    , words
    )

import Basics
import Char
import List exposing ((::))
import Maybe exposing (Maybe)
import Tuple


type String
    = String (List Char.Char)


type Elm_Float
    = Elm_Float Int Int
      -- We need another tag to prevent the compiler from assuming that the condition for tag 'String' is always true.
    | AnyOtherKind_Float


toList : String -> List Char
toList (String charsBlob) =
    toListRecursive
        0
        []
        charsBlob


toListRecursive : Int -> List Char -> Int -> List Char
toListRecursive offset list blob =
    let
        nextChar =
            Pine_kernel.take
                [ 4
                , Pine_kernel.skip [ offset, blob ]
                ]
    in
    if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
        list

    else
        toListRecursive
            (Pine_kernel.int_add [ offset, 4 ])
            (Pine_kernel.concat [ list, [ nextChar ] ])
            blob


fromList : List Char -> String
fromList chars =
    String (Pine_kernel.concat chars)


fromChar : Char -> String
fromChar char =
    String char


cons : Char -> String -> String
cons char (String string) =
    String (Pine_kernel.concat [ char, string ])


uncons : String -> Maybe ( Char, String )
uncons (String chars) =
    if Pine_kernel.equal [ Pine_kernel.length chars, 0 ] then
        Nothing

    else
        Just ( Pine_kernel.take [ 4, chars ], String (Pine_kernel.skip [ 4, chars ]) )


isEmpty : String -> Bool
isEmpty (String chars) =
    Pine_kernel.equal
        [ Pine_kernel.length chars, 0 ]


length : String -> Int
length (String chars) =
    Pine_kernel.concat
        [ Pine_kernel.take [ 1, 0 ]
        , Pine_kernel.bit_shift_right
            [ 2
            , Pine_kernel.skip [ 1, Pine_kernel.length chars ]
            ]
        ]


reverse : String -> String
reverse string =
    fromList (List.reverse (toList string))


foldl : (Char -> b -> b) -> b -> String -> b
foldl func acc string =
    foldlChars
        func
        acc
        (toList string)


foldlChars : (Char -> b -> b) -> b -> List Char -> b
foldlChars func acc chars =
    let
        nextChar =
            Pine_kernel.head chars
    in
    if Pine_kernel.equal [ nextChar, [] ] then
        acc

    else
        foldlChars func (func nextChar acc) (Pine_kernel.skip [ 1, chars ])


foldr : (Char -> b -> b) -> b -> String -> b
foldr func acc string =
    foldlChars
        func
        acc
        (List.reverse (toList string))


map : (Char -> Char) -> String -> String
map func string =
    fromList (List.map func (toList string))


repeat : Int -> String -> String
repeat n (String chars) =
    String (Pine_kernel.concat (List.repeat n chars))


replace : String -> String -> String -> String
replace before after string =
    join after (split before string)


append : String -> String -> String
append (String a) (String b) =
    String (Pine_kernel.concat [ a, b ])


concat : List String -> String
concat strings =
    let        
        charsBlobs =
            List.map
                (\\(String chars) -> chars)
                strings
    in
    String (Pine_kernel.concat charsBlobs)


split : String -> String -> List String
split (String sep) ((String stringBytes) as string) =
    if Pine_kernel.equal [ Pine_kernel.length sep, 0 ] then
        List.map fromChar (toList string)

    else
        splitHelperOnBlob 0 [] 0 sep stringBytes


splitHelperOnBlob : Int -> List String -> Int -> Int -> Int -> List String
splitHelperOnBlob offset collected lastStart sepBytes stringBytes =
    let
        sliceBytes : Int
        sliceBytes =
            Pine_kernel.take
                [ Pine_kernel.length sepBytes
                , Pine_kernel.skip [ offset, stringBytes ]
                ]
    in
    if Pine_kernel.equal [ sliceBytes, sepBytes ] then
        let
            separatedSliceLength : Int
            separatedSliceLength =
                Pine_kernel.int_add
                    [ offset
                    , Pine_kernel.int_mul [ -1, lastStart ]
                    ]

            separatedSlice : Int
            separatedSlice =
                Pine_kernel.take
                    [ separatedSliceLength
                    , Pine_kernel.skip [ lastStart, stringBytes ]
                    ]
        in
        splitHelperOnBlob
            (Pine_kernel.int_add [ offset, Pine_kernel.length sepBytes ])
            (Pine_kernel.concat [ collected, [ String separatedSlice ] ])
            (Pine_kernel.int_add [ offset, Pine_kernel.length sepBytes ])
            sepBytes
            stringBytes

    else if Pine_kernel.equal [ Pine_kernel.length sliceBytes, 0 ] then
        let
            separatedSlice : Int
            separatedSlice =
                Pine_kernel.skip [ lastStart, stringBytes ]
        in
        Pine_kernel.concat [ collected, [ String separatedSlice ] ]

    else
        splitHelperOnBlob
            (Pine_kernel.int_add [ offset, 4 ])
            collected
            lastStart
            sepBytes
            stringBytes


join : String -> List String -> String
join sepList chunks =
    let
        charsLists =
            List.intersperse
                (toList sepList)
                (List.map toList chunks)
    in
    String (Pine_kernel.concat (Pine_kernel.concat charsLists))


slice : Int -> Int -> String -> String
slice start end (String charsBlob) =
    if Pine_kernel.int_is_sorted_asc [ 0, start, end ] then
        let
            sliceLength : Int
            sliceLength =
                Pine_kernel.int_add [ end, Pine_kernel.int_mul [ -1, start ] ]
        in
        String
            (Pine_kernel.take
                [ Pine_kernel.int_mul [ sliceLength, 4 ]
                , Pine_kernel.skip
                    [ Pine_kernel.int_mul [ start, 4 ]
                    , charsBlob
                    ]
                ]
            )

    else
        let
            absoluteIndex relativeIndex =
                {-
                   Instead of using integer comparison together with the literal 0,
                   check the first byte if the sign is negative.
                -}
                if
                    Pine_kernel.equal
                        [ Pine_kernel.take [ 1, relativeIndex ]
                        , Pine_kernel.take [ 1, -1 ]
                        ]
                then
                    Pine_kernel.int_add [ relativeIndex, Pine_kernel.length charsBlob ]

                else
                    relativeIndex

            absoluteStart : Int
            absoluteStart =
                absoluteIndex
                    (Pine_kernel.int_mul [ start, 4 ])

            sliceLength : Int
            sliceLength =
                Pine_kernel.int_add
                    [ absoluteIndex (Pine_kernel.int_mul [ end, 4 ])
                    , Pine_kernel.int_mul [ -1, absoluteStart ]
                    ]
        in
        String
            (Pine_kernel.take
                [ sliceLength
                , Pine_kernel.skip [ absoluteStart, charsBlob ]
                ]
            )


left : Int -> String -> String
left n (String chars) =
    String
        (Pine_kernel.take
            [ Pine_kernel.int_mul [ n, 4 ]
            , chars
            ]
        )


right : Int -> String -> String
right n string =
    if Pine_kernel.int_is_sorted_asc [ n, 0 ] then
        ""

    else
        slice -n (length string) string


dropLeft : Int -> String -> String
dropLeft n (String chars) =
    String
        (Pine_kernel.skip
            [ Pine_kernel.int_mul [ n, 4 ]
            , chars
            ]
        )


dropRight : Int -> String -> String
dropRight n string =
    if Pine_kernel.int_is_sorted_asc [ n, 0 ] then
        string

    else
        slice 0 -n string


contains : String -> String -> Bool
contains (String patternList) (String stringList) =
    if Pine_kernel.equal [ patternList, [] ] then
        True

    else
        containsOnBlob 0 patternList stringList


containsOnBlob : Int -> Int -> Int -> Bool
containsOnBlob offset patternBytes stringBytes =
    let
        stringSlice =
            Pine_kernel.take
                [ Pine_kernel.length patternBytes
                , Pine_kernel.skip [ offset, stringBytes ]
                ]
    in
    if Pine_kernel.equal [ stringSlice, patternBytes ] then
        True

    else if Pine_kernel.equal [ Pine_kernel.length stringSlice, 0 ] then
        False

    else
        containsOnBlob
            (Pine_kernel.int_add [ offset, 4 ])
            patternBytes
            stringBytes


startsWith : String -> String -> Bool
startsWith (String patternList) (String stringList) =
    Pine_kernel.equal
        [ Pine_kernel.take [ Pine_kernel.length patternList, stringList ]
        , patternList
        ]


endsWith : String -> String -> Bool
endsWith pattern string =
    Pine_kernel.equal
        [ right (length pattern) string
        , pattern
        ]


toInt : String -> Maybe Int
toInt (String chars) =
    parseInt chars


fromInt : Int -> String
fromInt int =
    String (Pine_kernel.concat (fromIntAsList int))


parseInt : Int -> Maybe Int
parseInt src =
    let
        nextChar =
            Pine_kernel.take
                [ 4
                , src
                ]
    in
    case nextChar of
        '-' ->
            case parseUnsignedInt src 4 of
                Just unsignedVal ->
                    Just -unsignedVal

                Nothing ->
                    Nothing

        '+' ->
            parseUnsignedInt src 4

        _ ->
            -- If no minus sign, parse the rest as an unsigned integer
            parseUnsignedInt src 0


parseUnsignedInt : Int -> Int -> Maybe Int
parseUnsignedInt src offset0 =
    case Pine_kernel.take [ 4, Pine_kernel.skip [ offset0, src ] ] of
        '0' ->
            parseUnsignedIntRec 0 src (offset0 + 4)

        '1' ->
            parseUnsignedIntRec 1 src (offset0 + 4)

        '2' ->
            parseUnsignedIntRec 2 src (offset0 + 4)

        '3' ->
            parseUnsignedIntRec 3 src (offset0 + 4)

        '4' ->
            parseUnsignedIntRec 4 src (offset0 + 4)

        '5' ->
            parseUnsignedIntRec 5 src (offset0 + 4)

        '6' ->
            parseUnsignedIntRec 6 src (offset0 + 4)

        '7' ->
            parseUnsignedIntRec 7 src (offset0 + 4)

        '8' ->
            parseUnsignedIntRec 8 src (offset0 + 4)

        '9' ->
            parseUnsignedIntRec 9 src (offset0 + 4)

        _ ->
            Nothing


parseUnsignedIntRec : Int -> Int -> Int -> Maybe Int
parseUnsignedIntRec upper src offset0 =
    let
        nextChar =
            Pine_kernel.take [ 4, Pine_kernel.skip [ offset0, src ] ]
    in
    if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
        Just upper

    else if Pine_kernel.equal [ nextChar, '0' ] then
        parseUnsignedIntRec (upper * 10) src (offset0 + 4)

    else if Pine_kernel.equal [ nextChar, '1' ] then
        parseUnsignedIntRec (upper * 10 + 1) src (offset0 + 4)

    else if Pine_kernel.equal [ nextChar, '2' ] then
        parseUnsignedIntRec (upper * 10 + 2) src (offset0 + 4)

    else if Pine_kernel.equal [ nextChar, '3' ] then
        parseUnsignedIntRec (upper * 10 + 3) src (offset0 + 4)

    else if Pine_kernel.equal [ nextChar, '4' ] then
        parseUnsignedIntRec (upper * 10 + 4) src (offset0 + 4)

    else if Pine_kernel.equal [ nextChar, '5' ] then
        parseUnsignedIntRec (upper * 10 + 5) src (offset0 + 4)

    else if Pine_kernel.equal [ nextChar, '6' ] then
        parseUnsignedIntRec (upper * 10 + 6) src (offset0 + 4)

    else if Pine_kernel.equal [ nextChar, '7' ] then
        parseUnsignedIntRec (upper * 10 + 7) src (offset0 + 4)

    else if Pine_kernel.equal [ nextChar, '8' ] then
        parseUnsignedIntRec (upper * 10 + 8) src (offset0 + 4)

    else if Pine_kernel.equal [ nextChar, '9' ] then
        parseUnsignedIntRec (upper * 10 + 9) src (offset0 + 4)

    else
        Nothing


fromIntAsList : Int -> List Char
fromIntAsList int =
    if Pine_kernel.int_is_sorted_asc [ 0, int ] then
        fromUnsignedIntAsList int

    else
        Pine_kernel.concat [ [ '-' ], fromUnsignedIntAsList -int ]


fromUnsignedIntAsList : Int -> List Char
fromUnsignedIntAsList int =
    fromUnsignedIntAsListHelper int []


fromUnsignedIntAsListHelper : Int -> List Char -> List Char
fromUnsignedIntAsListHelper int lowerDigits =
    if Pine_kernel.int_is_sorted_asc [ int, 0 ] then
        if lowerDigits == [] then
            [ '0' ]

        else
            lowerDigits

    else
        let
            upperDigitsValue : Int
            upperDigitsValue =
                int // 10

            digitChar =
                unsafeDigitCharacterFromValue
                    (Pine_kernel.int_add
                        [ int
                        , Pine_kernel.int_mul [ upperDigitsValue, -10 ]
                        ]
                    )
        in
        fromUnsignedIntAsListHelper upperDigitsValue (digitChar :: lowerDigits)


unsafeDigitCharacterFromValue : Int -> Char
unsafeDigitCharacterFromValue digitValue =
    case digitValue of
        0 ->
            '0'

        1 ->
            '1'

        2 ->
            '2'

        3 ->
            '3'

        4 ->
            '4'

        5 ->
            '5'

        6 ->
            '6'

        7 ->
            '7'

        8 ->
            '8'

        9 ->
            '9'

        _ ->
            unsafeDigitCharacterFromValue digitValue


trim : String -> String
trim (String chars) =
    let
        leftTrimmedCount : Int
        leftTrimmedCount =
            trimLeftCountBytesTrimmed 0 chars

        rightRemainingLength : Int
        rightRemainingLength =
            trimRightCountBytesRemaining
                (Pine_kernel.length chars)
                chars
    in
    String
        (Pine_kernel.skip
            [ leftTrimmedCount
            , Pine_kernel.take
                [ rightRemainingLength
                , chars
                ]
            ]
        )


trimLeft : String -> String
trimLeft (String chars) =
    let
        trimmedCount : Int
        trimmedCount =
            trimLeftCountBytesTrimmed 0 chars
    in
    String
        (Pine_kernel.skip
            [ trimmedCount
            , chars
            ]
        )


trimRight : String -> String
trimRight (String chars) =
    let
        remainingLength : Int
        remainingLength =
            trimRightCountBytesRemaining
                (Pine_kernel.length chars)
                chars
    in
    String
        (Pine_kernel.take
            [ remainingLength
            , chars
            ]
        )


trimLeftCountBytesTrimmed : Int -> Int -> Int
trimLeftCountBytesTrimmed offset charsBytes =
    let
        nextCharBytes =
            Pine_kernel.take
                [ 4
                , Pine_kernel.skip [ offset, charsBytes ]
                ]
    in
    if Pine_kernel.equal [ Pine_kernel.length nextCharBytes, 0 ] then
        offset

    else if isCharRemovedOnTrim nextCharBytes then
        trimLeftCountBytesTrimmed
            (Pine_kernel.int_add [ offset, 4 ])
            charsBytes

    else
        offset


trimRightCountBytesRemaining : Int -> Int -> Int
trimRightCountBytesRemaining remainingLength charsBytes =
    if Pine_kernel.equal [ remainingLength, 0 ] then
        0

    else
        let
            char =
                Pine_kernel.take
                    [ 4
                    , Pine_kernel.skip
                        [ Pine_kernel.int_add [ remainingLength, -4 ]
                        , charsBytes
                        ]
                    ]
        in
        if isCharRemovedOnTrim char then
            trimRightCountBytesRemaining
                (Pine_kernel.int_add [ remainingLength, -4 ])
                charsBytes

        else
            remainingLength


isCharRemovedOnTrim : Char -> Bool
isCharRemovedOnTrim char =
    if Pine_kernel.equal [ char, ' ' ] then
        True

    else if Pine_kernel.equal [ char, '\\t' ] then
        True

    else if Pine_kernel.equal [ char, '\\n' ] then
        True

    else if Pine_kernel.equal [ char, '\\u{000D}' ] then
        True

    else
        False


dropWhileList : (Char -> Bool) -> List Char -> List Char
dropWhileList predicate stringList =
    case stringList of
        [] ->
            []

        char :: rest ->
            if predicate char then
                dropWhileList predicate rest

            else
                stringList


padLeft : Int -> Char -> String -> String
padLeft n char ((String charsBytes) as string) =
    let
        stringLength : Int
        stringLength =
            length string
    
        paddingLength : Int
        paddingLength =
            Pine_kernel.int_add [ n, -stringLength ]
    in
    if Pine_kernel.int_is_sorted_asc [ paddingLength, 0 ] then
        string
    else
        String
            (Pine_kernel.concat
                [ Pine_kernel.concat (List.repeat paddingLength char)
                , charsBytes
                ]
            )


lines : String -> List String
lines (String chars) =
    linesHelper 0 [] 0 chars


linesHelper : Int -> List String -> Int -> Int -> List String
linesHelper currentLineStart currentLines offset charsBytes =
    let
        nextChar =
            Pine_kernel.take
                [ 4
                , Pine_kernel.skip [ offset, charsBytes ]
                ]

        nextTwoChars =
            Pine_kernel.take
                [ 8
                , Pine_kernel.skip [ offset, charsBytes ]
                ]
    in
    if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]
        in
        Pine_kernel.concat
            [ currentLines
            , [ String (Pine_kernel.skip [ currentLineStart, charsBytes ]) ]
            ]

    else if Pine_kernel.equal [ nextTwoChars, Pine_kernel.concat [ '\u{000D}', '
' ] ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]

            currentLineChars : Int
            currentLineChars =
                Pine_kernel.take
                    [ currentLineLength
                    , Pine_kernel.skip [ currentLineStart, charsBytes ]
                    ]
        in
        linesHelper
            (Pine_kernel.int_add [ offset, 8 ])
            (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_kernel.int_add [ offset, 8 ])
            charsBytes

    else if Pine_kernel.equal [ nextTwoChars, Pine_kernel.concat [ '
', '\u{000D}' ] ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]

            currentLineChars : Int
            currentLineChars =
                Pine_kernel.take
                    [ currentLineLength
                    , Pine_kernel.skip [ currentLineStart, charsBytes ]
                    ]
        in
        linesHelper
            (Pine_kernel.int_add [ offset, 8 ])
            (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_kernel.int_add [ offset, 8 ])
            charsBytes

    else if Pine_kernel.equal [ nextChar, '
' ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]

            currentLineChars : List Char
            currentLineChars =
                Pine_kernel.take
                    [ currentLineLength
                    , Pine_kernel.skip [ currentLineStart, charsBytes ]
                    ]
        in
        linesHelper
            (Pine_kernel.int_add [ offset, 4 ])
            (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_kernel.int_add [ offset, 4 ])
            charsBytes

    else if Pine_kernel.equal [ nextChar, '\u{000D}' ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]

            currentLineChars : List Char
            currentLineChars =
                Pine_kernel.take
                    [ currentLineLength
                    , Pine_kernel.skip [ currentLineStart, charsBytes ]
                    ]
        in
        linesHelper
            (Pine_kernel.int_add [ offset, 4 ])
            (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_kernel.int_add [ offset, 4 ])
            charsBytes

    else
        linesHelper
            currentLineStart
            currentLines
            (Pine_kernel.int_add [ offset, 4 ])
            charsBytes


words : String -> List String
words string =
    wordsHelper 0 [] 0 (toList string)


wordsHelper : Int -> List String -> Int -> List Char -> List String
wordsHelper currentWordStart currentWords offset chars =
    let
        nextChar =
            Pine_kernel.head (Pine_kernel.skip [ offset, chars ])
    in
    if Pine_kernel.equal [ nextChar, [] ] then
        let
            currentWordLength : Int
            currentWordLength =
                Pine_kernel.int_add
                    [ offset
                    , Pine_kernel.int_mul [ currentWordStart, -1 ]
                    ]

            currentWordChars : List Char
            currentWordChars =
                Pine_kernel.take
                    [ currentWordLength
                    , Pine_kernel.skip [ currentWordStart, chars ]
                    ]
        in
        if Pine_kernel.equal [ currentWordChars, [] ] then
            currentWords

        else
            Pine_kernel.concat
                [ currentWords
                , [ String currentWordChars ]
                ]

    else
        let
            currentIsBreak : Bool
            currentIsBreak =
                isCharRemovedOnTrim nextChar
        in
        if currentIsBreak then
            let
                currentWordLength : Int
                currentWordLength =
                    Pine_kernel.int_add
                        [ offset
                        , Pine_kernel.int_mul [ currentWordStart, -1 ]
                        ]

                currentWordChars : List Char
                currentWordChars =
                    Pine_kernel.take
                        [ currentWordLength
                        , Pine_kernel.skip [ currentWordStart, chars ]
                        ]
            in
            if Pine_kernel.equal [ currentWordChars, [] ] then
                wordsHelper
                    (Pine_kernel.int_add [ offset, 1 ])
                    currentWords
                    (Pine_kernel.int_add [ offset, 1 ])
                    chars

            else
                wordsHelper
                    (Pine_kernel.int_add [ offset, 1 ])
                    (Pine_kernel.concat [ currentWords, [ String currentWordChars ] ])
                    (Pine_kernel.int_add [ offset, 1 ])
                    chars

        else
            wordsHelper
                currentWordStart
                currentWords
                (Pine_kernel.int_add [ offset, 1 ])
                chars


toFloat : String -> Maybe Float
toFloat (String charsBlob) =
    let
        firstChar =
            Pine_kernel.take [ 4, charsBlob ]
    in
    if Pine_kernel.equal [ Pine_kernel.length firstChar, 0 ] then
        Nothing

    else if Pine_kernel.equal [ firstChar, '-' ] then
        case toRationalComponentsLessSign (Pine_kernel.skip [ 4, charsBlob ]) of
            Nothing ->
                Nothing

            Just (Elm_Float numAbs denom) ->
                let
                    numSigned : Int
                    numSigned =
                        Pine_kernel.int_mul [ -1, numAbs ]
                in
                Just (Elm_Float numSigned denom)

    else
        case toRationalComponentsLessSign charsBlob of
            Nothing ->
                Nothing

            Just (Elm_Float numAbs denom) ->
                Just (Elm_Float numAbs denom)


fromFloat : Float -> String
fromFloat float =
    case float of
        Elm_Float numerator denom ->
            fromFloatDecimal 16 ( numerator, denom )

        int ->
            fromInt int


fromFloatDecimal : Int -> ( Int, Int ) -> String
fromFloatDecimal decimalPlacesMax ( numerator, denom ) =
    case denom of
        1 ->
            fromInt numerator

        0 ->
            if
                Pine_kernel.equal
                    [ Pine_kernel.take [ 1, numerator ]
                    , Pine_kernel.take [ 1, -1 ]
                    ]
            then
                "-Infinity"

            else
                "Infinity"

        _ ->
            let
                isNegative : Bool
                isNegative =
                    Pine_kernel.equal
                        [ Pine_kernel.take [ 1, numerator ]
                        , Pine_kernel.take [ 1, -1 ]
                        ]

                ( signStr, absNum ) =
                    if isNegative then
                        ( [ '-' ]
                        , Pine_kernel.int_mul [ -1, numerator ]
                        )

                    else
                        ( []
                        , numerator
                        )

                intPart : Int
                intPart =
                    absNum // denom

                remainder : Int
                remainder =
                    modBy denom absNum
            in
            if Pine_kernel.equal [ remainder, 0 ] || Pine_kernel.equal [ decimalPlacesMax, 0 ] then
                -- No remainder OR no decimal places requested
                String
                    (Pine_kernel.concat
                        [ signStr
                        , fromIntAsList intPart
                        ]
                    )

            else
                -- 3) Scale and round remainder to get fractional part
                let
                    scale : Int
                    scale =
                        intPow 1 10 decimalPlacesMax

                    scaledVal : Int
                    scaledVal =
                        Pine_kernel.int_mul [ remainder, scale ]

                    scaledInt : Int
                    scaledInt =
                        scaledVal // denom

                    leftover : Int
                    leftover =
                        modBy denom scaledVal

                    -- 4) ROUND HALF-UP:
                    scaledIntRounded : Int
                    scaledIntRounded =
                        if Pine_kernel.int_is_sorted_asc [ denom, Pine_kernel.int_mul [ leftover, 2 ] ] then
                            Pine_kernel.int_add [ 1, scaledInt ]

                        else
                            scaledInt

                    scaledStr : List Char
                    scaledStr =
                        fromIntAsList scaledIntRounded

                    -- If scaledIntRounded >= scale (e.g. 100 when scale=100),
                    -- that means we "overflowed" into the next integer. For instance:
                    -- fromFloatDecimal 2 (Elm_Float 999 100)
                    -- might round from "9.99" to "10.00".
                    overflowed : Bool
                    overflowed =
                        Pine_kernel.int_is_sorted_asc [ scale, scaledIntRounded ]

                    ( newIntPart, fractionDigits ) =
                        if overflowed then
                            -- increment integer part, fraction becomes e.g. "000"
                            let
                                incremented : Int
                                incremented =
                                    Pine_kernel.int_add [ intPart, 1 ]

                                -- e.g. scaledStr = "100" => dropLeft 1 => "00"
                                -- If scaledStr was "1000" => dropLeft 1 => "000"
                                fractionNoSign : List Char
                                fractionNoSign =
                                    Pine_kernel.skip [ 1, scaledStr ]
                            in
                            ( incremented, fractionNoSign )

                        else
                            -- If not overflowed, we may need zero‐padding to the left.
                            let
                                neededZeros : Int
                                neededZeros =
                                    Pine_kernel.int_add
                                        [ decimalPlacesMax
                                        , Pine_kernel.int_mul [ -1, Pine_kernel.length scaledStr ]
                                        ]

                                fractionNoSign : List Char
                                fractionNoSign =
                                    Pine_kernel.concat
                                        [ List.repeat neededZeros '0'
                                        , scaledStr
                                        ]
                            in
                            ( intPart, fractionNoSign )

                    -- Now remove trailing zeros from fractionDigits.
                    trimmedFraction : List Char
                    trimmedFraction =
                        removeTrailingZeros fractionDigits
                in
                if trimmedFraction == [] then
                    -- Entire fractional part was zeros, so just show an integer.
                    String
                        (Pine_kernel.concat
                            [ signStr
                            , fromIntAsList newIntPart
                            ]
                        )

                else
                    String
                        (Pine_kernel.concat
                            [ signStr
                            , fromIntAsList newIntPart
                            , [ '.' ]
                            , trimmedFraction
                            ]
                        )


removeTrailingZeros : List Char -> List Char
removeTrailingZeros chars =
    removeTrailingZerosHelper (Pine_kernel.length chars) chars


removeTrailingZerosHelper : Int -> List Char -> List Char
removeTrailingZerosHelper offset chars =
    if Pine_kernel.equal [ offset, 0 ] then
        chars

    else
        let
            nextOffset : Int
            nextOffset =
                Pine_kernel.int_add [ offset, -1 ]
        in
        case
            Pine_kernel.take
                [ 1
                , Pine_kernel.skip [ nextOffset, chars ]
                ]
        of
            [ '0' ] ->
                removeTrailingZerosHelper nextOffset chars

            _ ->
                Pine_kernel.take [ offset, chars ]


intPow : Int -> Int -> Int -> Int
intPow acc base exponent =
    if Pine_kernel.int_is_sorted_asc [ exponent, 0 ] then
        acc

    else
        intPow
            (Pine_kernel.int_mul [ acc, base ])
            base
            (Pine_kernel.int_add [ exponent, -1 ])


toRationalComponentsLessSign : Int -> Maybe ( Int, Int )
toRationalComponentsLessSign charsBlob =
    case splitHelperOnBlob 0 [] 0 '.' charsBlob of
        [] ->
            Nothing

        [ String whole ] ->
            case parseUnsignedInt whole 0 of
                Nothing ->
                    Nothing

                Just numerator ->
                    Just (Elm_Float numerator 1)

        [ String beforeSep, String afterSep ] ->
            if Pine_kernel.equal [ Pine_kernel.length afterSep, 0 ] then
                if Pine_kernel.equal [ Pine_kernel.length beforeSep, 0 ] then
                    Nothing

                else
                    case parseUnsignedIntRec 0 beforeSep 0 of
                        Nothing ->
                            Nothing

                        Just beforeSepInt ->
                            Just (Elm_Float beforeSepInt 1)

            else
                case parseUnsignedIntRec 0 beforeSep 0 of
                    Nothing ->
                        Nothing

                    Just beforeSepInt ->
                        case parseUnsignedIntRec 0 afterSep 0 of
                            Nothing ->
                                Nothing

                            Just afterSepInt ->
                                let
                                    denom : Int
                                    denom =
                                        case Pine_kernel.length afterSep of
                                            4 ->
                                                10

                                            8 ->
                                                100

                                            12 ->
                                                1000

                                            16 ->
                                                10000

                                            20 ->
                                                100000

                                            24 ->
                                                1000000

                                            28 ->
                                                10000000

                                            32 ->
                                                100000000

                                            36 ->
                                                1000000000

                                            40 ->
                                                10000000000

                                            _ ->
                                                1

                                    numerator =
                                        Pine_kernel.int_add
                                            [ Pine_kernel.int_mul [ beforeSepInt, denom ], afterSepInt ]
                                in
                                Just (Elm_Float numerator denom)

        _ ->
            Nothing


any : (Char -> Bool) -> String -> Bool
any predicate (String chars) =
    charsAny 0 predicate chars


charsAny : Int -> (Char -> Bool) -> Int -> Bool
charsAny offset predicate charsBytes =
    let
        char =
            Pine_kernel.take [ 4, Pine_kernel.skip [ offset, charsBytes ] ]
    in
    if Pine_kernel.equal [ Pine_kernel.length char, 0 ] then
        False

    else if predicate char then
        True

    else
        charsAny
            (Pine_kernel.int_add [ offset, 4 ])
            predicate
            charsBytes


indexes : String -> String -> List Int
indexes (String pattern) (String string) =
    indexesHelper 0 [] pattern string


indexesHelper : Int -> List Int -> Int -> Int -> List Int
indexesHelper offset currentIndexes pattern string =
    let
        stringSlice =
            Pine_kernel.take
                [ Pine_kernel.length pattern
                , Pine_kernel.skip
                    [ Pine_kernel.int_mul [ offset, 4 ]
                    , string
                    ]
                ]
    in
    if Pine_kernel.equal [ Pine_kernel.length stringSlice, 0 ] then
        currentIndexes

    else if
        Pine_kernel.equal
            [ stringSlice
            , pattern
            ]
    then
        indexesHelper
            (Pine_kernel.int_add [ offset, 1 ])
            (Pine_kernel.concat [ currentIndexes, [ offset ] ])
            pattern
            string

    else
        indexesHelper
            (Pine_kernel.int_add [ offset, 1 ])
            currentIndexes
            pattern
            string


indices : String -> String -> List Int
indices pattern string =
    indexes pattern string


toUpper : String -> String
toUpper string =
    fromList (List.map Char.toUpper (toList string))


toLower : String -> String
toLower string =
    fromList (List.map Char.toLower (toList string))


"""
    , """
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
    if Pine_kernel.int_is_sorted_asc [ 0, index ]
    then
        List.head (List.drop index array)

    else
        Nothing


set : Int -> a -> Array a -> Array a
set index value array =
    if Pine_kernel.negate (Pine_kernel.int_is_sorted_asc [ 0, index ]) ||
        (Pine_kernel.int_is_sorted_asc [ Pine_kernel.length array, index ])
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

"""
    , """
module Result exposing
  ( Result(..)
  , withDefault
  , map, map2, map3, map4, map5
  , andThen
  , toMaybe, fromMaybe, mapError
  )

{-| A `Result` is the result of a computation that may fail. This is a great
way to manage errors in Elm.

# Type and Constructors
@docs Result

# Mapping
@docs map, map2, map3, map4, map5

# Chaining
@docs andThen

# Handling Errors
@docs withDefault, toMaybe, fromMaybe, mapError
-}

import Basics exposing ( Bool(..) )
import Maybe exposing ( Maybe(..) )


{-| A `Result` is either `Ok` meaning the computation succeeded, or it is an
`Err` meaning that there was some failure.
-}
type Result error value
    = Ok value
    | Err error


{-| If the result is `Ok` return the value, but if the result is an `Err` then
return a given default value. The following examples try to parse integers.

    Result.withDefault 0 (Ok 123)   == 123
    Result.withDefault 0 (Err "no") == 0
-}
withDefault : a -> Result x a -> a
withDefault def result =
  case result of
    Ok a ->
        a

    Err _ ->
        def


{-| Apply a function to a result. If the result is `Ok`, it will be converted.
If the result is an `Err`, the same error value will propagate through.

    map sqrt (Ok 4.0)          == Ok 2.0
    map sqrt (Err "bad input") == Err "bad input"
-}
map : (a -> value) -> Result x a -> Result x value
map func ra =
  case ra of
    Ok a ->
      Ok (func a)

    Err e ->
      Err e


{-| Apply a function if both results are `Ok`. If not, the first `Err` will
propagate through.

    map2 max (Ok 42)   (Ok 13)   == Ok 42
    map2 max (Err "x") (Ok 13)   == Err "x"
    map2 max (Ok 42)   (Err "y") == Err "y"
    map2 max (Err "x") (Err "y") == Err "x"

This can be useful if you have two computations that may fail, and you want
to put them together quickly.
-}
map2 : (a -> b -> value) -> Result x a -> Result x b -> Result x value
map2 func ra rb =
  case ra of
    Err x ->
      Err x

    Ok a ->
      case rb of
        Err x ->
          Err x

        Ok b ->
          Ok (func a b)


{-|-}
map3 : (a -> b -> c -> value) -> Result x a -> Result x b -> Result x c -> Result x value
map3 func ra rb rc =
  case ra of
    Err x ->
      Err x

    Ok a ->
      case rb of
        Err x ->
          Err x

        Ok b ->
          case rc of
            Err x ->
              Err x

            Ok c ->
              Ok (func a b c)


{-|-}
map4 : (a -> b -> c -> d -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x value
map4 func ra rb rc rd =
  case ra of
    Err x ->
      Err x

    Ok a ->
      case rb of
        Err x ->
          Err x

        Ok b ->
          case rc of
            Err x ->
              Err x

            Ok c ->
              case rd of
                Err x ->
                  Err x

                Ok d ->
                  Ok (func a b c d)


{-|-}
map5 : (a -> b -> c -> d -> e -> value) -> Result x a -> Result x b -> Result x c -> Result x d -> Result x e -> Result x value
map5 func ra rb rc rd re =
  case ra of
    Err x ->
      Err x

    Ok a ->
      case rb of
        Err x ->
          Err x

        Ok b ->
          case rc of
            Err x ->
              Err x

            Ok c ->
              case rd of
                Err x ->
                  Err x

                Ok d ->
                  case re of
                    Err x ->
                      Err x

                    Ok e ->
                      Ok (func a b c d e)


{-| Chain together a sequence of computations that may fail. It is helpful
to see its definition:

    andThen : (a -> Result e b) -> Result e a -> Result e b
    andThen callback result =
        case result of
          Ok value -> callback value
          Err msg -> Err msg

This means we only continue with the callback if things are going well. For
example, say you need to use (`toInt : String -> Result String Int`) to parse
a month and make sure it is between 1 and 12:

    toValidMonth : Int -> Result String Int
    toValidMonth month =
        if month >= 1 && month <= 12
            then Ok month
            else Err "months must be between 1 and 12"

    toMonth : String -> Result String Int
    toMonth rawString =
        toInt rawString
          |> andThen toValidMonth

    -- toMonth "4" == Ok 4
    -- toMonth "9" == Ok 9
    -- toMonth "a" == Err "cannot parse to an Int"
    -- toMonth "0" == Err "months must be between 1 and 12"

This allows us to come out of a chain of operations with quite a specific error
message. It is often best to create a custom type that explicitly represents
the exact ways your computation may fail. This way it is easy to handle in your
code.
-}
andThen : (a -> Result x b) -> Result x a -> Result x b
andThen callback result =
    case result of
      Ok value ->
        callback value

      Err msg ->
        Err msg


{-| Transform an `Err` value. For example, say the errors we get have too much
information:

    parseInt : String -> Result ParseError Int

    type alias ParseError =
        { message : String
        , code : Int
        , position : (Int,Int)
        }

    mapError .message (parseInt "123") == Ok 123
    mapError .message (parseInt "abc") == Err "char 'a' is not a number"
-}
mapError : (x -> y) -> Result x a -> Result y a
mapError f result =
    case result of
      Ok v ->
        Ok v

      Err e ->
        Err (f e)


{-| Convert to a simpler `Maybe` if the actual error message is not needed or
you need to interact with some code that primarily uses maybes.

    parseInt : String -> Result ParseError Int

    maybeParseInt : String -> Maybe Int
    maybeParseInt string =
        toMaybe (parseInt string)
-}
toMaybe : Result x a -> Maybe a
toMaybe result =
    case result of
      Ok  v -> Just v
      Err _ -> Nothing


{-| Convert from a simple `Maybe` to interact with some code that primarily
uses `Results`.

    parseInt : String -> Maybe Int

    resultParseInt : String -> Result String Int
    resultParseInt string =
        fromMaybe ("error parsing string: " ++ toString string) (parseInt string)
-}
fromMaybe : x -> Maybe a -> Result x a
fromMaybe err maybe =
    case maybe of
      Just v  -> Ok v
      Nothing -> Err err


"""
    , """
module Dict exposing
  ( Dict
  , empty, singleton, insert, update, remove
  , isEmpty, member, get, size
  , keys, values, toList, fromList
  , map, foldl, foldr, filter, partition
  , union, intersect, diff, merge
  )

{-| A dictionary mapping unique keys to values. The keys can be any comparable
type. This includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or
lists of comparable types.

Insert, remove, and query operations all take *O(log n)* time.

# Dictionaries
@docs Dict

# Build
@docs empty, singleton, insert, update, remove

# Query
@docs isEmpty, member, get, size

# Lists
@docs keys, values, toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

# Combine
@docs union, intersect, diff, merge

-}


import Basics
import Maybe exposing (..)
import List exposing (..)



-- DICTIONARIES


-- The color of a node. Leaves are considered Black.
type NColor
    = Red
    | Black


{-| A dictionary of keys and values. So a `Dict String User` is a dictionary
that lets you look up a `String` (such as user names) and find the associated
`User`.

    import Dict exposing (Dict)

    users : Dict String User
    users =
      Dict.fromList
        [ ("Alice", User "Alice" 28 1.65)
        , ("Bob"  , User "Bob"   19 1.82)
        , ("Chuck", User "Chuck" 33 1.75)
        ]

    type alias User =
      { name : String
      , age : Int
      , height : Float
      }
-}
type Dict k v
    = RBNode_elm_builtin NColor k v (Dict k v) (Dict k v)
    | RBEmpty_elm_builtin


{-| Create an empty dictionary. -}
empty : Dict k v
empty =
  RBEmpty_elm_builtin


{-| Get the value associated with a key. If the key is not found, return
`Nothing`. This is useful when you are not sure if a key will be in the
dictionary.

    animals = fromList [ ("Tom", Cat), ("Jerry", Mouse) ]

    get "Tom"   animals == Just Cat
    get "Jerry" animals == Just Mouse
    get "Spike" animals == Nothing

-}
get : comparable -> Dict comparable v -> Maybe v
get targetKey dict =
  case dict of
    RBEmpty_elm_builtin ->
      Nothing

    RBNode_elm_builtin _ key value left right ->
      getAfterCompare
        (compare targetKey key)
        targetKey
        value
        left
        right


getAfterCompare : Order -> comparable -> Dict comparable v -> Dict comparable v -> Maybe v
getAfterCompare order targetKey value left right =
    case order of
        LT ->
            get targetKey left

        EQ ->
            Just value

        GT ->
            get targetKey right


{-| Determine if a key is in a dictionary. -}
member : comparable -> Dict comparable v -> Bool
member key dict =
  case get key dict of
    Just _ ->
      True

    Nothing ->
      False


{-| Determine the number of key-value pairs in the dictionary. -}
size : Dict k v -> Int
size dict =
  sizeHelp 0 dict


sizeHelp : Int -> Dict k v -> Int
sizeHelp n dict =
  case dict of
    RBEmpty_elm_builtin ->
      n

    RBNode_elm_builtin _ _ _ left right ->
      sizeHelp (sizeHelp (n+1) right) left


{-| Determine if a dictionary is empty.

    isEmpty empty == True
-}
isEmpty : Dict k v -> Bool
isEmpty dict =
  case dict of
    RBEmpty_elm_builtin ->
      True

    RBNode_elm_builtin _ _ _ _ _ ->
      False


{-| Insert a key-value pair into a dictionary. Replaces value when there is
a collision. -}
insert : comparable -> v -> Dict comparable v -> Dict comparable v
insert key value dict =
  -- Root node is always Black
  case insertHelp key value dict of
    RBNode_elm_builtin Red k v l r ->
      RBNode_elm_builtin Black k v l r

    x ->
      x


insertHelp : comparable -> v -> Dict comparable v -> Dict comparable v
insertHelp key value dict =
  case dict of
    RBEmpty_elm_builtin ->
      -- New nodes are always red. If it violates the rules, it will be fixed
      -- when balancing.
      RBNode_elm_builtin Red key value RBEmpty_elm_builtin RBEmpty_elm_builtin

    RBNode_elm_builtin nColor nKey nValue nLeft nRight ->
      case compare key nKey of
        LT ->
          balance nColor nKey nValue (insertHelp key value nLeft) nRight

        EQ ->
          RBNode_elm_builtin nColor nKey value nLeft nRight

        GT ->
          balance nColor nKey nValue nLeft (insertHelp key value nRight)


balance : NColor -> k -> v -> Dict k v -> Dict k v -> Dict k v
balance color key value left right =
  case right of
    RBNode_elm_builtin Red rK rV rLeft rRight ->
      case left of
        RBNode_elm_builtin Red lK lV lLeft lRight ->
          RBNode_elm_builtin
            Red
            key
            value
            (RBNode_elm_builtin Black lK lV lLeft lRight)
            (RBNode_elm_builtin Black rK rV rLeft rRight)

        _ ->
          RBNode_elm_builtin color rK rV (RBNode_elm_builtin Red key value left rLeft) rRight

    _ ->
      case left of
        RBNode_elm_builtin Red lK lV (RBNode_elm_builtin Red llK llV llLeft llRight) lRight ->
          RBNode_elm_builtin
            Red
            lK
            lV
            (RBNode_elm_builtin Black llK llV llLeft llRight)
            (RBNode_elm_builtin Black key value lRight right)

        _ ->
          RBNode_elm_builtin color key value left right


{-| Remove a key-value pair from a dictionary. If the key is not found,
no changes are made. -}
remove : comparable -> Dict comparable v -> Dict comparable v
remove key dict =
  -- Root node is always Black
  case removeHelp key dict of
    RBNode_elm_builtin Red k v l r ->
      RBNode_elm_builtin Black k v l r

    x ->
      x


{-| The easiest thing to remove from the tree, is a red node. However, when searching for the
node to remove, we have no way of knowing if it will be red or not. This remove implementation
makes sure that the bottom node is red by moving red colors down the tree through rotation
and color flips. Any violations this will cause, can easily be fixed by balancing on the way
up again.
-}
removeHelp : comparable -> Dict comparable v -> Dict comparable v
removeHelp targetKey dict =
  case dict of
    RBEmpty_elm_builtin ->
      RBEmpty_elm_builtin

    RBNode_elm_builtin color key value left right ->
      if targetKey < key then
        case left of
          RBNode_elm_builtin Black _ _ lLeft _ ->
            case lLeft of
              RBNode_elm_builtin Red _ _ _ _ ->
                RBNode_elm_builtin color key value (removeHelp targetKey left) right

              _ ->
                case moveRedLeft dict of
                  RBNode_elm_builtin nColor nKey nValue nLeft nRight ->
                    balance nColor nKey nValue (removeHelp targetKey nLeft) nRight

                  RBEmpty_elm_builtin ->
                    RBEmpty_elm_builtin

          _ ->
            RBNode_elm_builtin color key value (removeHelp targetKey left) right
      else
        removeHelpEQGT targetKey (removeHelpPrepEQGT targetKey dict color key value left right)


removeHelpPrepEQGT : comparable -> Dict comparable v -> NColor -> comparable -> v -> Dict comparable v -> Dict comparable v -> Dict comparable v
removeHelpPrepEQGT targetKey dict color key value left right =
  case left of
    RBNode_elm_builtin Red lK lV lLeft lRight ->
      RBNode_elm_builtin
        color
        lK
        lV
        lLeft
        (RBNode_elm_builtin Red key value lRight right)

    _ ->
      case right of
        RBNode_elm_builtin Black _ _ (RBNode_elm_builtin Black _ _ _ _) _ ->
          moveRedRight dict

        RBNode_elm_builtin Black _ _ RBEmpty_elm_builtin _ ->
          moveRedRight dict

        _ ->
          dict


{-| When we find the node we are looking for, we can remove by replacing the key-value
pair with the key-value pair of the left-most node on the right side (the closest pair).
-}
removeHelpEQGT : comparable -> Dict comparable v -> Dict comparable v
removeHelpEQGT targetKey dict =
  case dict of
    RBNode_elm_builtin color key value left right ->
      if targetKey == key then
        case getMin right of
          RBNode_elm_builtin _ minKey minValue _ _ ->
            balance color minKey minValue left (removeMin right)

          RBEmpty_elm_builtin ->
            RBEmpty_elm_builtin
      else
        balance color key value left (removeHelp targetKey right)

    RBEmpty_elm_builtin ->
      RBEmpty_elm_builtin


getMin : Dict k v -> Dict k v
getMin dict =
  case dict of
    RBNode_elm_builtin _ _ _ ((RBNode_elm_builtin _ _ _ _ _) as left) _ ->
      getMin left

    _ ->
      dict


removeMin : Dict k v -> Dict k v
removeMin dict =
  case dict of
    RBNode_elm_builtin color key value ((RBNode_elm_builtin lColor _ _ lLeft _) as left) right ->
      case lColor of
        Black ->
          case lLeft of
            RBNode_elm_builtin Red _ _ _ _ ->
              RBNode_elm_builtin color key value (removeMin left) right

            _ ->
              case moveRedLeft dict of
                RBNode_elm_builtin nColor nKey nValue nLeft nRight ->
                  balance nColor nKey nValue (removeMin nLeft) nRight

                RBEmpty_elm_builtin ->
                  RBEmpty_elm_builtin

        _ ->
          RBNode_elm_builtin color key value (removeMin left) right

    _ ->
      RBEmpty_elm_builtin


moveRedLeft : Dict k v -> Dict k v
moveRedLeft dict =
  case dict of
    RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV lLeft lRight) (RBNode_elm_builtin rClr rK rV ((RBNode_elm_builtin Red rlK rlV rlL rlR) as rLeft) rRight) ->
      RBNode_elm_builtin
        Red
        rlK
        rlV
        (RBNode_elm_builtin Black k v (RBNode_elm_builtin Red lK lV lLeft lRight) rlL)
        (RBNode_elm_builtin Black rK rV rlR rRight)

    RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV lLeft lRight) (RBNode_elm_builtin rClr rK rV rLeft rRight) ->
      case clr of
        Black ->
          RBNode_elm_builtin
            Black
            k
            v
            (RBNode_elm_builtin Red lK lV lLeft lRight)
            (RBNode_elm_builtin Red rK rV rLeft rRight)

        Red ->
          RBNode_elm_builtin
            Black
            k
            v
            (RBNode_elm_builtin Red lK lV lLeft lRight)
            (RBNode_elm_builtin Red rK rV rLeft rRight)

    _ ->
      dict


moveRedRight : Dict k v -> Dict k v
moveRedRight dict =
  case dict of
    RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV (RBNode_elm_builtin Red llK llV llLeft llRight) lRight) (RBNode_elm_builtin rClr rK rV rLeft rRight) ->
      RBNode_elm_builtin
        Red
        lK
        lV
        (RBNode_elm_builtin Black llK llV llLeft llRight)
        (RBNode_elm_builtin Black k v lRight (RBNode_elm_builtin Red rK rV rLeft rRight))

    RBNode_elm_builtin clr k v (RBNode_elm_builtin lClr lK lV lLeft lRight) (RBNode_elm_builtin rClr rK rV rLeft rRight) ->
      case clr of
        Black ->
          RBNode_elm_builtin
            Black
            k
            v
            (RBNode_elm_builtin Red lK lV lLeft lRight)
            (RBNode_elm_builtin Red rK rV rLeft rRight)

        Red ->
          RBNode_elm_builtin
            Black
            k
            v
            (RBNode_elm_builtin Red lK lV lLeft lRight)
            (RBNode_elm_builtin Red rK rV rLeft rRight)

    _ ->
      dict


{-| Update the value of a dictionary for a specific key with a given function. -}
update : comparable -> (Maybe v -> Maybe v) -> Dict comparable v -> Dict comparable v
update targetKey alter dictionary =
  case alter (get targetKey dictionary) of
    Just value ->
      insert targetKey value dictionary

    Nothing ->
      remove targetKey dictionary


{-| Create a dictionary with one key-value pair. -}
singleton : comparable -> v -> Dict comparable v
singleton key value =
  -- Root node is always Black
  RBNode_elm_builtin Black key value RBEmpty_elm_builtin RBEmpty_elm_builtin


-- COMBINE


{-| Combine two dictionaries. If there is a collision, preference is given
to the first dictionary.
-}
union : Dict comparable v -> Dict comparable v -> Dict comparable v
union t1 t2 =
    foldl (\\k v t -> insert k v t) t2 t1


{-| Keep a key-value pair when its key appears in the second dictionary.
Preference is given to values in the first dictionary.
-}
intersect : Dict comparable v -> Dict comparable v -> Dict comparable v
intersect t1 t2 =
  filter (\\k _ -> member k t2) t1


{-| Keep a key-value pair when its key does not appear in the second dictionary.
-}
diff : Dict comparable a -> Dict comparable b -> Dict comparable a
diff t1 t2 =
  foldl (\\k v t -> remove k t) t1 t2


{-| Apply a function to all values in a dictionary.
-}
map : (k -> a -> b) -> Dict k a -> Dict k b
map func dict =
  case dict of
    RBEmpty_elm_builtin ->
      RBEmpty_elm_builtin

    RBNode_elm_builtin color key value left right ->
      RBNode_elm_builtin color key (func key value) (map func left) (map func right)


{-| Fold over the key-value pairs in a dictionary from lowest key to highest key.

    import Dict exposing (Dict)

    getAges : Dict String User -> List String
    getAges users =
      Dict.foldl addAge [] users

    addAge : String -> User -> List String -> List String
    addAge _ user ages =
      user.age :: ages

    -- getAges users == [33,19,28]
-}
foldl : (k -> v -> b -> b) -> b -> Dict k v -> b
foldl func acc dict =
  case dict of
    RBEmpty_elm_builtin ->
      acc

    RBNode_elm_builtin _ key value left right ->
      foldl func (func key value (foldl func acc left)) right


{-| Fold over the key-value pairs in a dictionary from highest key to lowest key.

    import Dict exposing (Dict)

    getAges : Dict String User -> List String
    getAges users =
      Dict.foldr addAge [] users

    addAge : String -> User -> List String -> List String
    addAge _ user ages =
      user.age :: ages

    -- getAges users == [28,19,33]
-}
foldr : (k -> v -> b -> b) -> b -> Dict k v -> b
foldr func acc t =
  case t of
    RBEmpty_elm_builtin ->
      acc

    RBNode_elm_builtin _ key value left right ->
      foldr func (func key value (foldr func acc right)) left


{-| Keep only the key-value pairs that pass the given test. -}
filter : (comparable -> v -> Bool) -> Dict comparable v -> Dict comparable v
filter isGood dict =
  foldl (\\k v d -> if isGood k v then insert k v d else d) empty dict


{-| Partition a dictionary according to some test. The first dictionary
contains all key-value pairs which passed the test, and the second contains
the pairs that did not.
-}
partition : (comparable -> v -> Bool) -> Dict comparable v -> (Dict comparable v, Dict comparable v)
partition isGood dict =
  let
    add key value (t1, t2) =
      if isGood key value then
        (insert key value t1, t2)

      else
        (t1, insert key value t2)
  in
    foldl add (empty, empty) dict


-- LISTS


{-| Get all of the keys in a dictionary, sorted from lowest to highest.

    keys (fromList [(0,"Alice"),(1,"Bob")]) == [0,1]
-}
keys : Dict k v -> List k
keys dict =
  case dict of
    RBEmpty_elm_builtin ->
      []

    RBNode_elm_builtin _ key value left right ->
      Pine_kernel.concat [ keys left, [ key ], keys right ]


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]
-}
values : Dict k v -> List v
values dict =
  case dict of
    RBEmpty_elm_builtin ->
      []

    RBNode_elm_builtin _ key value left right ->
      Pine_kernel.concat [ values left, [ value ], values right ]


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys. -}
toList : Dict k v -> List (k,v)
toList dict =
  case dict of
    RBEmpty_elm_builtin ->
      []

    RBNode_elm_builtin _ key value left right ->
      Pine_kernel.concat [ toList left, [ (key, value) ], toList right ]


{-| Convert an association list into a dictionary. -}
fromList : List (comparable, v) -> Dict comparable v
fromList assocs =
    insertFromList assocs empty


insertFromList : List (comparable, v) -> Dict comparable v -> Dict comparable v
insertFromList assocs dict =
    case assocs of
    [] ->
        dict

    (key, value) :: remaining ->
        insertFromList remaining (insert key value dict)


"""
    , """
module Set exposing
  ( Set
  , empty, singleton, insert, remove
  , isEmpty, member, size
  , union, intersect, diff
  , toList, fromList
  , map, foldl, foldr, filter, partition
  )

{-| A set of unique values. The values can be any comparable type. This
includes `Int`, `Float`, `Time`, `Char`, `String`, and tuples or lists
of comparable types.

Insert, remove, and query operations all take *O(log n)* time.

# Sets
@docs Set

# Build
@docs empty, singleton, insert, remove

# Query
@docs isEmpty, member, size

# Combine
@docs union, intersect, diff

# Lists
@docs toList, fromList

# Transform
@docs map, foldl, foldr, filter, partition

-}

import Basics exposing (Bool, Int)
import Dict
import List exposing ((::))
import Maybe exposing (Maybe(..))


{-| Represents a set of unique values. So `(Set Int)` is a set of integers and
`(Set String)` is a set of strings.
-}
type Set t =
  Set_elm_builtin (Dict.Dict t ())


{-| Create an empty set.
-}
empty : Set a
empty =
  Set_elm_builtin Dict.empty


{-| Create a set with one value.
-}
singleton : comparable -> Set comparable
singleton key =
  Set_elm_builtin (Dict.singleton key ())


{-| Insert a value into a set.
-}
insert : comparable -> Set comparable -> Set comparable
insert key (Set_elm_builtin dict) =
  Set_elm_builtin (Dict.insert key () dict)


{-| Remove a value from a set. If the value is not found, no changes are made.
-}
remove : comparable -> Set comparable -> Set comparable
remove key (Set_elm_builtin dict) =
  Set_elm_builtin (Dict.remove key dict)


{-| Determine if a set is empty.
-}
isEmpty : Set a -> Bool
isEmpty (Set_elm_builtin dict) =
  Dict.isEmpty dict


{-| Determine if a value is in a set.
-}
member : comparable -> Set comparable -> Bool
member key (Set_elm_builtin dict) =
  Dict.member key dict


{-| Determine the number of elements in a set.
-}
size : Set a -> Int
size (Set_elm_builtin dict) =
  Dict.size dict


{-| Get the union of two sets. Keep all values.
-}
union : Set comparable -> Set comparable -> Set comparable
union (Set_elm_builtin dict1) (Set_elm_builtin dict2) =
  Set_elm_builtin (Dict.union dict1 dict2)


{-| Get the intersection of two sets. Keeps values that appear in both sets.
-}
intersect : Set comparable -> Set comparable -> Set comparable
intersect (Set_elm_builtin dict1) (Set_elm_builtin dict2) =
  Set_elm_builtin (Dict.intersect dict1 dict2)


{-| Get the difference between the first set and the second. Keeps values
that do not appear in the second set.
-}
diff : Set comparable -> Set comparable -> Set comparable
diff (Set_elm_builtin dict1) (Set_elm_builtin dict2) =
  Set_elm_builtin (Dict.diff dict1 dict2)


{-| Convert a set into a list, sorted from lowest to highest.
-}
toList : Set a -> List a
toList (Set_elm_builtin dict) =
  Dict.keys dict


{-| Convert a list into a set, removing any duplicates.
-}
fromList : List comparable -> Set comparable
fromList list =
    insertFromList list empty


insertFromList : List comparable -> Set comparable -> Set comparable
insertFromList list (Set_elm_builtin dict) =
    case list of
    next :: following ->
        insertFromList
            following
            (Set_elm_builtin (Dict.insert next () dict))

    [] ->
        (Set_elm_builtin dict)


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> b -> b) -> b -> Set a -> b
foldl func initialState (Set_elm_builtin dict) =
    List.foldl (\\key state -> func key state) initialState (Dict.keys dict)


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> b -> b) -> b -> Set a -> b
foldr func initialState (Set_elm_builtin dict) =
    List.foldr (\\key state -> func key state) initialState (Dict.keys dict)


{-| Map a function onto a set, creating a new set with no duplicates.
-}
map : (comparable -> comparable2) -> Set comparable -> Set comparable2
map func set =
  fromList (foldl (\\x xs -> func x :: xs) [] set)


{-| Only keep elements that pass the given test.

    import Set exposing (Set)

    numbers : Set Int
    numbers =
      Set.fromList [-2,-1,0,1,2]

    positives : Set Int
    positives =
      Set.filter (\\x -> x > 0) numbers

    -- positives == Set.fromList [1,2]
-}
filter : (comparable -> Bool) -> Set comparable -> Set comparable
filter isGood (Set_elm_builtin dict) =
  Set_elm_builtin (Dict.filter (\\key _ -> isGood key) dict)


{-| Create two new sets. The first contains all the elements that passed the
given test, and the second contains all the elements that did not.
-}
partition : (comparable -> Bool) -> Set comparable -> (Set comparable, Set comparable)
partition isGood (Set_elm_builtin dict) =
  let
    (dict1, dict2) =
      Dict.partition (\\key _ -> isGood key) dict
  in
    (Set_elm_builtin dict1, Set_elm_builtin dict2)

"""
    , """
module Bitwise exposing
    ( and
    , complement
    , or
    , shiftLeftBy
    , shiftRightBy
    , shiftRightZfBy
    , xor
    )

{-
   Functions in the 'Bitwise' module emulate limits of JavaScript bitwise operations for backwards-compatibility.

   To provide an Elm core library that is backward-compatible with libraries and apps implemented for
   legacy platforms, simulate mapping from integer to two's complement and wrapping to 32-bit.
-}


and : Int -> Int -> Int
and a b =
    let
        bytesA =
            if
                Pine_kernel.int_is_sorted_asc
                    [ 0
                    , a
                    ]
            then
                Pine_kernel.bit_and
                    [ Pine_kernel.skip [ 1, a ]
                    , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_kernel.int_is_sorted_asc
                    [ -0x80000000
                    , a
                    ]
            then
                Pine_kernel.bit_not
                    (Pine_kernel.bit_or
                        [ Pine_kernel.skip
                            [ 1
                            , Pine_kernel.int_add [ a, 1 ]
                            ]
                        , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_kernel.bit_not
                    (Pine_kernel.bit_and
                        [ Pine_kernel.int_add [ a, 1 ]
                        , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        bytesB =
            if
                Pine_kernel.int_is_sorted_asc
                    [ 0
                    , b
                    ]
            then
                Pine_kernel.bit_and
                    [ Pine_kernel.skip [ 1, b ]
                    , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_kernel.int_is_sorted_asc
                    [ -0x80000000
                    , b
                    ]
            then
                Pine_kernel.bit_not
                    (Pine_kernel.bit_or
                        [ Pine_kernel.skip
                            [ 1
                            , Pine_kernel.int_add [ b, 1 ]
                            ]
                        , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_kernel.bit_not
                    (Pine_kernel.bit_and
                        [ Pine_kernel.int_add [ b, 1 ]
                        , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        combined =
            Pine_kernel.bit_and
                [ bytesA
                , bytesB
                ]
    in
    if
        Pine_kernel.equal
            [ Pine_kernel.bit_and
                [ combined
                , Pine_kernel.skip [ 1, 0x80000000 ]
                ]
            , Pine_kernel.skip [ 1, 0x80000000 ]
            ]
    then
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ combined
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , -0x80000000
            ]

    else
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ combined
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , 0
            ]


or : Int -> Int -> Int
or a b =
    let
        bytesA =
            if
                Pine_kernel.int_is_sorted_asc
                    [ 0
                    , a
                    ]
            then
                Pine_kernel.bit_and
                    [ Pine_kernel.skip [ 1, a ]
                    , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_kernel.int_is_sorted_asc
                    [ -0x80000000
                    , a
                    ]
            then
                Pine_kernel.bit_not
                    (Pine_kernel.bit_or
                        [ Pine_kernel.skip
                            [ 1
                            , Pine_kernel.int_add [ a, 1 ]
                            ]
                        , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_kernel.bit_not
                    (Pine_kernel.bit_and
                        [ Pine_kernel.int_add [ a, 1 ]
                        , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        bytesB =
            if
                Pine_kernel.int_is_sorted_asc
                    [ 0
                    , b
                    ]
            then
                Pine_kernel.bit_and
                    [ Pine_kernel.skip [ 1, b ]
                    , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_kernel.int_is_sorted_asc
                    [ -0x80000000
                    , b
                    ]
            then
                Pine_kernel.bit_not
                    (Pine_kernel.bit_or
                        [ Pine_kernel.skip
                            [ 1
                            , Pine_kernel.int_add [ b, 1 ]
                            ]
                        , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_kernel.bit_not
                    (Pine_kernel.bit_and
                        [ Pine_kernel.int_add [ b, 1 ]
                        , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        combined =
            Pine_kernel.bit_or
                [ bytesA
                , bytesB
                ]
    in
    if
        Pine_kernel.equal
            [ Pine_kernel.bit_and
                [ combined
                , Pine_kernel.skip [ 1, 0x80000000 ]
                ]
            , Pine_kernel.skip [ 1, 0x80000000 ]
            ]
    then
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ combined
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , -0x80000000
            ]

    else
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ combined
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , 0
            ]


xor : Int -> Int -> Int
xor a b =
    let
        bytesA =
            if
                Pine_kernel.int_is_sorted_asc
                    [ 0
                    , a
                    ]
            then
                Pine_kernel.bit_and
                    [ Pine_kernel.skip [ 1, a ]
                    , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_kernel.int_is_sorted_asc
                    [ -0x80000000
                    , a
                    ]
            then
                Pine_kernel.bit_not
                    (Pine_kernel.bit_or
                        [ Pine_kernel.skip
                            [ 1
                            , Pine_kernel.int_add [ a, 1 ]
                            ]
                        , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_kernel.bit_not
                    (Pine_kernel.bit_and
                        [ Pine_kernel.int_add [ a, 1 ]
                        , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        bytesB =
            if
                Pine_kernel.int_is_sorted_asc
                    [ 0
                    , b
                    ]
            then
                Pine_kernel.bit_and
                    [ Pine_kernel.skip [ 1, b ]
                    , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                    ]

            else if
                Pine_kernel.int_is_sorted_asc
                    [ -0x80000000
                    , b
                    ]
            then
                Pine_kernel.bit_not
                    (Pine_kernel.bit_or
                        [ Pine_kernel.skip
                            [ 1
                            , Pine_kernel.int_add [ b, 1 ]
                            ]
                        , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                        ]
                    )

            else
                Pine_kernel.bit_not
                    (Pine_kernel.bit_and
                        [ Pine_kernel.int_add [ b, 1 ]
                        , Pine_kernel.skip [ 1, 0xFFFFFFFF ]
                        ]
                    )

        combined =
            Pine_kernel.bit_xor
                [ bytesA
                , bytesB
                ]
    in
    if
        Pine_kernel.equal
            [ Pine_kernel.bit_and
                [ combined
                , Pine_kernel.skip [ 1, 0x80000000 ]
                ]
            , Pine_kernel.skip [ 1, 0x80000000 ]
            ]
    then
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ combined
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , -0x80000000
            ]

    else
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ combined
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , 0
            ]


complement : Int -> Int
complement asInt =
    if
        Pine_kernel.int_is_sorted_asc
            [ -0x80000000
            , asInt
            , 0x7FFFFFFF
            ]
    then
        Pine_kernel.int_add
            [ Pine_kernel.int_mul [ -1, asInt ]
            , -1
            ]

    else if
        Pine_kernel.equal
            [ Pine_kernel.bit_and
                [ asInt
                , Pine_kernel.skip [ 1, 0x80000000 ]
                ]
            , Pine_kernel.skip [ 1, 0x80000000 ]
            ]
    then
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, -11 ]
                , Pine_kernel.bit_and
                    [ asInt
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , 0x7FFFFFFF
            ]

    else
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ asInt
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , -1
            ]


shiftLeftBy : Int -> Int -> Int
shiftLeftBy offset asInt =
    let
        withPadding =
            if
                Pine_kernel.int_is_sorted_asc
                    [ -0x80000000
                    , asInt
                    , -1
                    ]
            then
                Pine_kernel.bit_or
                    [ Pine_kernel.int_add
                        [ asInt
                        , 0x80000000
                        ]
                    , Pine_kernel.skip [ 1, 0x80000000 ]
                    ]

            else
                Pine_kernel.bit_or
                    [ Pine_kernel.skip [ 1, asInt ]
                    , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                    ]

        beforeTruncate =
            Pine_kernel.bit_shift_left
                [ offset
                , withPadding
                ]
    in
    if
        Pine_kernel.equal
            [ Pine_kernel.bit_and
                [ beforeTruncate
                , Pine_kernel.skip [ 1, 0x80000000 ]
                ]
            , Pine_kernel.skip [ 1, 0x80000000 ]
            ]
    then
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ beforeTruncate
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , -0x80000000
            ]

    else
        Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 11 ]
                , Pine_kernel.bit_and
                    [ beforeTruncate
                    , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                    ]
                ]
            , 0
            ]


shiftRightBy : Int -> Int -> Int
shiftRightBy offset asInt =
    if
        Pine_kernel.int_is_sorted_asc
            [ -0x80000000
            , asInt
            , 0x7FFFFFFF
            ]
    then
        if
            Pine_kernel.int_is_sorted_asc
                [ 0
                , asInt
                ]
        then
            let
                lessSign =
                    Pine_kernel.skip
                        [ 1
                        , Pine_kernel.int_add [ asInt, 0 ]
                        ]

                beforeTruncate =
                    Pine_kernel.bit_shift_right
                        [ offset
                        , lessSign
                        ]
            in
            Pine_kernel.int_add
                [ Pine_kernel.concat
                    [ Pine_kernel.take [ 1, 0 ]
                    , beforeTruncate
                    ]
                , 0
                ]

        else
            let
                lessSign =
                    Pine_kernel.skip
                        [ 1
                        , Pine_kernel.int_add [ asInt, -1 ]
                        ]

                beforeTruncate =
                    Pine_kernel.bit_shift_right
                        [ offset
                        , lessSign
                        ]
            in
            Pine_kernel.int_add
                [ Pine_kernel.concat
                    [ Pine_kernel.take [ 1, -1 ]
                    , beforeTruncate
                    ]
                , 0
                ]

    else
        let
            asInt32 =
                if
                    Pine_kernel.equal
                        [ Pine_kernel.bit_and
                            [ asInt
                            , Pine_kernel.skip [ 1, 0x80000000 ]
                            ]
                        , Pine_kernel.skip [ 1, 0x80000000 ]
                        ]
                then
                    Pine_kernel.int_add
                        [ Pine_kernel.concat
                            [ Pine_kernel.take [ 1, 11 ]
                            , Pine_kernel.bit_and
                                [ asInt
                                , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                                ]
                            ]
                        , -0x80000000
                        ]

                else
                    Pine_kernel.int_add
                        [ Pine_kernel.concat
                            [ Pine_kernel.take [ 1, -11 ]
                            , Pine_kernel.bit_and
                                [ asInt
                                , Pine_kernel.skip [ 1, 0x7FFFFFFF ]
                                ]
                            ]
                        , -1
                        ]
        in
        if
            Pine_kernel.int_is_sorted_asc
                [ 0
                , asInt32
                ]
        then
            let
                lessSign =
                    Pine_kernel.skip
                        [ 1
                        , Pine_kernel.int_add [ asInt32, 0 ]
                        ]

                beforeTruncate =
                    Pine_kernel.bit_shift_right
                        [ offset
                        , lessSign
                        ]
            in
            Pine_kernel.int_add
                [ Pine_kernel.concat
                    [ Pine_kernel.take [ 1, 0 ]
                    , beforeTruncate
                    ]
                , 0
                ]

        else
            let
                lessSign =
                    Pine_kernel.skip
                        [ 1
                        , Pine_kernel.int_add [ asInt32, -1 ]
                        ]

                beforeTruncate =
                    Pine_kernel.bit_shift_right
                        [ offset
                        , lessSign
                        ]
            in
            Pine_kernel.int_add
                [ Pine_kernel.concat
                    [ Pine_kernel.take [ 1, -1 ]
                    , beforeTruncate
                    ]
                , 0
                ]


shiftRightZfBy : Int -> Int -> Int
shiftRightZfBy offset bytes =
    let
        sign =
            Pine_kernel.take [ 1, bytes ]
    in
    if
        Pine_kernel.equal
            [ sign
            , Pine_kernel.take [ 1, 0 ]
            ]
    then
        let
            beforeTruncate =
                Pine_kernel.bit_shift_right
                    [ offset
                    , Pine_kernel.skip [ 1, bytes ]
                    ]
        in
        Pine_kernel.concat
            [ sign
            , trimLeadingZeros
                (Pine_kernel.reverse
                    (Pine_kernel.take
                        [ 4
                        , Pine_kernel.reverse beforeTruncate
                        ]
                    )
                )
            ]

    else
        let
            fromTwosComplement32 =
                Pine_kernel.bit_not
                    (Pine_kernel.reverse
                        (Pine_kernel.take
                            [ 4
                            , Pine_kernel.concat
                                [ Pine_kernel.reverse
                                    (Pine_kernel.skip
                                        [ 1
                                        , Pine_kernel.int_add
                                            [ bytes
                                            , 1
                                            ]
                                        ]
                                    )
                                , Pine_kernel.skip [ 2, 0x0000000100000000 ]
                                ]
                            ]
                        )
                    )

            beforeTruncate =
                Pine_kernel.bit_shift_right
                    [ offset
                    , fromTwosComplement32
                    ]
        in
        Pine_kernel.concat
            [ Pine_kernel.take [ 1, 0 ]
            , trimLeadingZeros beforeTruncate
            ]


trimLeadingZeros : Int -> Int
trimLeadingZeros bytes =
    Pine_kernel.skip
        [ 1
        , Pine_kernel.int_add
            [ Pine_kernel.concat
                [ Pine_kernel.take [ 1, 0 ]
                , bytes
                ]
            , 0
            ]
        ]

"""
    ]
