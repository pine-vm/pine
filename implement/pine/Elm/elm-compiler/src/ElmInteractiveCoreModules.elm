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
    = String (List Char.Char)

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
                compareStrings stringA stringB

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


compareStrings : List Char -> List Char -> Order
compareStrings stringA stringB =
    case stringA of
        [] ->
            case stringB of
                [] ->
                    EQ

                _ ->
                    LT

        charA :: tailA ->
            case stringB of
                [] ->
                    GT

                charB :: tailB ->
                    if Pine_kernel.equal [ charA, charB ] then
                        compareStrings tailA tailB

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
  ( singleton, repeat, range, (::)
  , map, indexedMap, foldl, foldr, filter, filterMap
  , length, reverse, member, all, any, maximum, minimum, sum, product
  , append, concat, concatMap, intersperse, map2, map3, map4, map5
  , sort, sortBy, sortWith
  , isEmpty, head, tail, take, drop, partition, unzip
  )


import Basics
import Maybe exposing (Maybe(..))


infix right 5 (::) = cons


singleton : a -> List a
singleton value =
    [value]


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
    case xs of
        [] ->
            []

        hd :: tl ->
            let
                step x rest =
                    cons sep (cons x rest)

                spersed =
                    foldr step [] tl
            in
            cons hd spersed


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
        middleIndex = (Pine_kernel.length list) // 2
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


import Basics exposing (Bool, Int, (&&), (||), (>=), (<=))


type alias Char = Int


toCode : Char -> Int
toCode char =
    -- Add the sign prefix byte
    Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]


fromCode : Int -> Char
fromCode code =
    -- Remove the sign prefix byte
    Pine_kernel.skip [ 1, code ]


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
        code =
            toCode char
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
        code =
            toCode char
    in
    Pine_kernel.int_is_sorted_asc [ 0x30, code, 0x37 ]


{-| Detect hexadecimal digits `0123456789abcdefABCDEF`
-}
isHexDigit : Char -> Bool
isHexDigit char =
    let
        code =
            toCode char
    in
    (Pine_kernel.int_is_sorted_asc [ 0x30, code, 0x39 ])
        || (Pine_kernel.int_is_sorted_asc [ 0x41, code, 0x46 ])
        || (Pine_kernel.int_is_sorted_asc [ 0x61, code, 0x66 ])


isUpper : Char -> Bool
isUpper char =
    let
        code =
            toCode char
    in
    Pine_kernel.int_is_sorted_asc [ 0x41, code, 0x5A ]


isLower : Char -> Bool
isLower char =
    let
        code =
            toCode char
    in
    Pine_kernel.int_is_sorted_asc [ 0x61, code, 0x7A ]


isAlpha : Char -> Bool
isAlpha char =
    let
        code =
            toCode char
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
        Pine_kernel.skip [ 1, Pine_kernel.int_add [ code, -0x20 ] ]

    else
        char


toLower : Char -> Char
toLower char =
    let
        code =
            Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], char ]
    in
    if Pine_kernel.int_is_sorted_asc [ 0x41, code, 0x5A ] then
        Pine_kernel.skip [ 1, Pine_kernel.int_add [ code, 0x20 ] ]

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
toList (String chars) =
    chars


fromList : List Char -> String
fromList chars =
    String chars


fromChar : Char -> String
fromChar char =
    String [ char ]


cons : Char -> String -> String
cons char (String string) =
    String (char :: string)


uncons : String -> Maybe ( Char, String )
uncons (String chars) =
    case chars of
        [] ->
            Nothing

        first :: rest ->
            Just ( first, String rest )


isEmpty : String -> Bool
isEmpty (String chars) =
    Pine_kernel.equal [ chars, [] ]


length : String -> Int
length (String chars) =
    Pine_kernel.length chars


reverse : String -> String
reverse (String chars) =
    String (Pine_kernel.reverse chars)


foldl : (Char -> b -> b) -> b -> String -> b
foldl func acc (String chars) =
    foldlChars func acc chars


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
foldr func acc (String chars) =
    foldlChars func acc (Pine_kernel.reverse chars)


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
    join "" strings


split : String -> String -> List String
split (String sep) (String string) =
    if Pine_kernel.equal [ sep, [] ] then
        List.map fromChar string

    else
        splitHelperOnList 0 [] 0 sep string


splitHelperOnList : Int -> List String -> Int -> List Char -> List Char -> List String
splitHelperOnList offset collected lastStart sep string =
    let
        slice : List Char
        slice =
            Pine_kernel.take
                [ Pine_kernel.length sep
                , Pine_kernel.skip [ offset, string ]
                ]
    in
    if Pine_kernel.equal [ slice, sep ] then
        let
            separatedSliceLength : Int
            separatedSliceLength =
                Pine_kernel.int_add
                    [ offset
                    , Pine_kernel.int_mul [ -1, lastStart ]
                    ]

            separatedSlice : List Char
            separatedSlice =
                Pine_kernel.take
                    [ separatedSliceLength
                    , Pine_kernel.skip [ lastStart, string ]
                    ]
        in
        splitHelperOnList
            (Pine_kernel.int_add [ offset, Pine_kernel.length sep ])
            (Pine_kernel.concat [ collected, [ String separatedSlice ] ])
            (Pine_kernel.int_add [ offset, Pine_kernel.length sep ])
            sep
            string

    else if Pine_kernel.equal [ slice, [] ] then
        let
            separatedSlice : List Char
            separatedSlice =
                Pine_kernel.skip [ lastStart, string ]
        in
        Pine_kernel.concat [ collected, [ String separatedSlice ] ]

    else
        splitHelperOnList
            (Pine_kernel.int_add [ offset, 1 ])
            collected
            lastStart
            sep
            string


join : String -> List String -> String
join (String sepList) chunks =
    String (joinOnList sepList chunks)


joinOnList : List Char -> List String -> List Char
joinOnList sep chunks =
    case chunks of
        [] ->
            []

        (String nextChunk) :: remaining ->
            if remaining == [] then
                nextChunk

            else
                Pine_kernel.concat [ nextChunk, sep, joinOnList sep remaining ]


slice : Int -> Int -> String -> String
slice start end (String chars) =
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
                Pine_kernel.int_add [ relativeIndex, Pine_kernel.length chars ]

            else
                relativeIndex

        absoluteStart : Int
        absoluteStart =
            absoluteIndex start

        sliceLength : Int
        sliceLength =
            Pine_kernel.int_add
                [ absoluteIndex end
                , Pine_kernel.int_mul [ -1, absoluteStart ]
                ]
    in
    String
        (Pine_kernel.take
            [ sliceLength
            , Pine_kernel.skip [ absoluteStart, chars ]
            ]
        )


left : Int -> String -> String
left n (String chars) =
    String (List.take n chars)


right : Int -> String -> String
right n string =
    if Pine_kernel.int_is_sorted_asc [ n, 0 ] then
        ""

    else
        slice -n (length string) string


dropLeft : Int -> String -> String
dropLeft n (String chars) =
    String (List.drop n chars)


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
        containsOnList patternList stringList


containsOnList : List Char -> List Char -> Bool
containsOnList pattern string =
    if
        Pine_kernel.equal
            [ Pine_kernel.take [ Pine_kernel.length pattern, string ]
            , pattern
            ]
    then
        True

    else if Pine_kernel.int_is_sorted_asc [ Pine_kernel.length string, Pine_kernel.length pattern ] then
        False

    else
        containsOnList pattern (Pine_kernel.skip [ 1, string ])


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
    toIntFromList chars


fromInt : Int -> String
fromInt int =
    String (fromIntAsList int)


toIntFromList : List Char -> Maybe Int
toIntFromList stringAsList =
    let
        firstChar =
            Pine_kernel.head stringAsList
    in
    if Pine_kernel.equal [ firstChar, [] ] then
        Nothing

    else
        let
            ( valueString, signMultiplier ) =
                case firstChar of
                    '-' ->
                        ( Pine_kernel.skip [ 1, stringAsList ], -1 )

                    '+' ->
                        ( Pine_kernel.skip [ 1, stringAsList ], 1 )

                    _ ->
                        ( stringAsList, 1 )
        in
        if Pine_kernel.equal [ valueString, [] ] then
            Nothing

        else
            case toUnsignedIntFromList 0 valueString of
                Just unsigned ->
                    Just (Pine_kernel.int_mul [ signMultiplier, unsigned ])

                Nothing ->
                    Nothing


toUnsignedIntFromList : Int -> List Char -> Maybe Int
toUnsignedIntFromList upper chars =
    let
        char =
            Pine_kernel.head chars
    in
    if Pine_kernel.equal [ char, [] ] then
        Just upper

    else
        case digitValueFromChar char of
            Nothing ->
                Nothing

            Just digitValue ->
                toUnsignedIntFromList
                    (Pine_kernel.int_add [ digitValue, Pine_kernel.int_mul [ upper, 10 ] ])
                    (Pine_kernel.skip [ 1, chars ])


digitValueFromChar : Char -> Maybe Int
digitValueFromChar char =
    case char of
        '0' ->
            Just 0

        '1' ->
            Just 1

        '2' ->
            Just 2

        '3' ->
            Just 3

        '4' ->
            Just 4

        '5' ->
            Just 5

        '6' ->
            Just 6

        '7' ->
            Just 7

        '8' ->
            Just 8

        '9' ->
            Just 9

        _ ->
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
    String
        (dropWhileList isCharRemovedOnTrim
            (List.reverse (dropWhileList isCharRemovedOnTrim (List.reverse chars)))
        )


trimLeft : String -> String
trimLeft (String chars) =
    String (dropWhileList isCharRemovedOnTrim chars)


trimRight : String -> String
trimRight (String chars) =
    String (List.reverse (dropWhileList isCharRemovedOnTrim (List.reverse chars)))


isCharRemovedOnTrim : Char -> Bool
isCharRemovedOnTrim char =
    if Pine_kernel.equal [ char, ' ' ] then
        True

    else if Pine_kernel.equal [ char, '\\t' ] then
        True

    else if Pine_kernel.equal [ char, '\\n' ] then
        True

    else if Pine_kernel.equal [ char, '\u{000D}' ] then
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
padLeft n char (String list) =
    String
        (Pine_kernel.concat [ List.repeat (n - Pine_kernel.length list) char, list ])


lines : String -> List String
lines (String chars) =
    linesHelper 0 [] 0 chars


linesHelper : Int -> List String -> Int -> List Char -> List String
linesHelper currentLineStart currentLines offset chars =
    let
        nextChar =
            Pine_kernel.head (Pine_kernel.skip [ offset, chars ])

        nextTwoChars =
            Pine_kernel.take [ 2, Pine_kernel.skip [ offset, chars ] ]
    in
    if Pine_kernel.equal [ nextChar, [] ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]

            currentLineChars : List Char
            currentLineChars =
                Pine_kernel.take
                    [ currentLineLength
                    , Pine_kernel.skip [ currentLineStart, chars ]
                    ]
        in
        Pine_kernel.concat
            [ currentLines
            , [ String (Pine_kernel.skip [ currentLineStart, chars ]) ]
            ]

    else if Pine_kernel.equal [ nextTwoChars, [ '\\u{000D}', '\\n' ] ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]

            currentLineChars : List Char
            currentLineChars =
                Pine_kernel.take
                    [ currentLineLength
                    , Pine_kernel.skip [ currentLineStart, chars ]
                    ]
        in
        linesHelper
            (Pine_kernel.int_add [ offset, 2 ])
            (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_kernel.int_add [ offset, 2 ])
            chars

    else if Pine_kernel.equal [ nextTwoChars, [ '\\n', '\\u{000D}' ] ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]

            currentLineChars : List Char
            currentLineChars =
                Pine_kernel.take
                    [ currentLineLength
                    , Pine_kernel.skip [ currentLineStart, chars ]
                    ]
        in
        linesHelper
            (Pine_kernel.int_add [ offset, 2 ])
            (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_kernel.int_add [ offset, 2 ])
            chars

    else if Pine_kernel.equal [ nextChar, '\\n' ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]

            currentLineChars : List Char
            currentLineChars =
                Pine_kernel.take
                    [ currentLineLength
                    , Pine_kernel.skip [ currentLineStart, chars ]
                    ]
        in
        linesHelper
            (Pine_kernel.int_add [ offset, 1 ])
            (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_kernel.int_add [ offset, 1 ])
            chars

    else if Pine_kernel.equal [ nextChar, '\\u{000D}' ] then
        let
            currentLineLength =
                Pine_kernel.int_add [ offset, -currentLineStart ]

            currentLineChars : List Char
            currentLineChars =
                Pine_kernel.take
                    [ currentLineLength
                    , Pine_kernel.skip [ currentLineStart, chars ]
                    ]
        in
        linesHelper
            (Pine_kernel.int_add [ offset, 1 ])
            (Pine_kernel.concat [ currentLines, [ String currentLineChars ] ])
            (Pine_kernel.int_add [ offset, 1 ])
            chars

    else
        linesHelper
            currentLineStart
            currentLines
            (Pine_kernel.int_add [ offset, 1 ])
            chars


toFloat : String -> Maybe Float
toFloat (String chars) =
    let
        firstChar =
            Pine_kernel.head chars
    in
    if Pine_kernel.equal [ firstChar, [] ] then
        Nothing

    else if Pine_kernel.equal [ firstChar, '-' ] then
        case toRationalComponentsLessSign (Pine_kernel.skip [ 1, chars ]) of
            Nothing ->
                Nothing

            Just (Elm_Float numAbs denom) ->
                let
                    numSigned =
                        if Pine_kernel.equal [ numAbs, 0 ] then
                            0

                        else
                            Pine_kernel.int_mul [ -1, numAbs ]
                in
                Just (Elm_Float numSigned denom)

    else
        case toRationalComponentsLessSign chars of
            Nothing ->
                Nothing

            Just (Elm_Float numAbs denom) ->
                Just (Elm_Float numAbs denom)


toRationalComponentsLessSign : List Char -> Maybe ( Int, Int )
toRationalComponentsLessSign chars =
    case splitHelperOnList 0 [] 0 [ '.' ] chars of
        [] ->
            Nothing

        [ String whole ] ->
            case toUnsignedIntFromList 0 whole of
                Nothing ->
                    Nothing

                Just numerator ->
                    Just (Elm_Float numerator 1)

        [ String beforeSep, String afterSep ] ->
            if Pine_kernel.equal [ afterSep, [] ] then
                if Pine_kernel.equal [ beforeSep, [] ] then
                    Nothing

                else
                    case toUnsignedIntFromList 0 beforeSep of
                        Nothing ->
                            Nothing

                        Just beforeSepInt ->
                            Just (Elm_Float beforeSepInt 1)

            else
                case toUnsignedIntFromList 0 beforeSep of
                    Nothing ->
                        Nothing

                    Just beforeSepInt ->
                        case toUnsignedIntFromList 0 afterSep of
                            Nothing ->
                                Nothing

                            Just afterSepInt ->
                                let
                                    denom =
                                        Basics.pow 10 (Pine_kernel.length afterSep)

                                    numerator =
                                        Pine_kernel.int_add
                                            [ Pine_kernel.int_mul [ beforeSepInt, denom ], afterSepInt ]
                                in
                                Just (Elm_Float numerator denom)

        _ ->
            Nothing


any : (Char -> Bool) -> String -> Bool
any predicate (String chars) =
    charsAny predicate chars


charsAny : (Char -> Bool) -> List Char -> Bool
charsAny predicate chars =
    case chars of
        [] ->
            False

        char :: rest ->
            if predicate char then
                True

            else
                charsAny predicate rest


toUpper : String -> String
toUpper (String chars) =
    String (List.map Char.toUpper chars)


toLower : String -> String
toLower (String chars) =
    String (List.map Char.toLower chars)


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


foldr : (a -> b -> b) -> b -> Array a -> b
foldr foldItem seed array =
    List.foldr foldItem seed array


foldl : (a -> b -> b) -> b -> Array a -> b
foldl foldItem seed array =
    List.foldl foldItem seed array

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
  ( and, or, xor, complement
  , shiftLeftBy, shiftRightBy, shiftRightZfBy
  )

{-
   Emulate limits of JavaScript bitwise operations for backwards-compat

   To provide an Elm core library that is backward-compatible with libraries and apps implemented for
   classic environments, limit the functions in the `Bitwise` module to 32-bit.
-}


and : Int -> Int -> Int
and a b =
    Pine_kernel.concat
        [ Pine_kernel.take [ 1, 0 ]
        , Pine_kernel.reverse
            (Pine_kernel.take
                [ 4
                , Pine_kernel.reverse
                    (Pine_kernel.bit_and
                        [ Pine_kernel.skip [ 1, a ]
                        , Pine_kernel.skip [ 1, b ]
                        ]
                    )
                ]
            )
        ]


or : Int -> Int -> Int
or a b =
    Pine_kernel.concat
        [ Pine_kernel.take [ 1, 0 ]
        , Pine_kernel.reverse
            (Pine_kernel.take
                [ 4
                , Pine_kernel.reverse
                    (Pine_kernel.bit_or
                        [ Pine_kernel.skip [ 1, a ]
                        , Pine_kernel.skip [ 1, b ]
                        ]
                    )
                ]
            )
        ]


xor : Int -> Int -> Int
xor a b =
    Pine_kernel.concat
        [ Pine_kernel.take [ 1, 0 ]
        , Pine_kernel.reverse
            (Pine_kernel.take
                [ 4
                , Pine_kernel.reverse
                    (Pine_kernel.bit_xor
                        [ Pine_kernel.skip [ 1, a ]
                        , Pine_kernel.skip [ 1, b ]
                        ]
                    )
                ]
            )
        ]


complement : Int -> Int
complement a =
    Pine_kernel.concat
        [ Pine_kernel.take [ 1, 0 ]
        , Pine_kernel.bit_not (Pine_kernel.skip [ 1, a ])
        ]


shiftLeftBy : Int -> Int -> Int
shiftLeftBy offset bytes =
    let
        sign =
            Pine_kernel.take [ 1, bytes ]

        withExtension =
            Pine_kernel.concat
                [ Pine_kernel.skip [ 1, 0 ]
                , Pine_kernel.skip [ 1, 0 ]
                , Pine_kernel.skip [ 1, 0 ]
                , Pine_kernel.skip [ 1, bytes ]
                ]

        beforeTruncate =
            Pine_kernel.bit_shift_left
                [ offset
                , withExtension
                ]
    in
    Pine_kernel.concat
        [ sign
        , truncateLeadingZeros
            (Pine_kernel.reverse
                (Pine_kernel.take
                    [ 4
                    , Pine_kernel.reverse beforeTruncate
                    ]
                )
            )
        ]


shiftRightBy : Int -> Int -> Int
shiftRightBy offset bytes =
    let
        sign =
            Pine_kernel.take [ 1, bytes ]

        beforeTruncate =
            Pine_kernel.bit_shift_right
                [ offset
                , Pine_kernel.skip [ 1, bytes ]
                ]
    in
    Pine_kernel.concat
        [ sign
        , truncateLeadingZeros beforeTruncate
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
            , truncateLeadingZeros
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
            , truncateLeadingZeros beforeTruncate
            ]


truncateLeadingZeros : Int -> Int
truncateLeadingZeros bytes =
    if
        Pine_kernel.equal
            [ Pine_kernel.length bytes
            , 1
            ]
    then
        bytes

    else if
        Pine_kernel.equal
            [ Pine_kernel.take [ 1, bytes ]
            , Pine_kernel.skip [ 1, 0 ]
            ]
    then
        truncateLeadingZeros (Pine_kernel.skip [ 1, bytes ])

    else
        bytes


"""
    ]
