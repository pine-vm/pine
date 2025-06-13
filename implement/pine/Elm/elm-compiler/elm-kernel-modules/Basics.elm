module Basics exposing
    ( (&&)
    , (*)
    , (+)
    , (++)
    , (-)
    , (/)
    , (//)
    , (/=)
    , (<)
    , (<<)
    , (<=)
    , (<|)
    , (==)
    , (>)
    , (>=)
    , (>>)
    , (^)
    , (|>)
    , (||)
    , Bool(..)
    , Float
    , Int
    , Never
    , Order(..)
    , abs
    , acos
    , always
    , asin
    , atan
    , atan2
    , ceiling
    , clamp
    , compare
    , cos
    , degrees
    , e
    , floor
    , fromPolar
    , identity
    , isInfinite
    , isNaN
    , logBase
    , max
    , min
    , modBy
    , negate
    , never
    , not
    , pi
    , radians
    , remainderBy
    , round
    , sin
    , sqrt
    , tan
    , toFloat
    , toPolar
    , truncate
    , turns
    , xor
    )


infix right 0 (<|) = apL
infix left  0 (|>) = apR
infix right 2 (||) = or
infix right 3 (&&) = and
infix non   4 (==) = eq
infix non   4 (/=) = neq
infix non   4 (<) = lt
infix non   4 (>) = gt
infix non   4 (<=) = le
infix non   4 (>=) = ge
infix right 5 (++) = append
infix left  6 (+) = add
infix left  6 (-) = sub
infix left  7 (*) = mul
infix left  7 (//) = idiv
infix right 8 (^) = pow
infix left  9 (<<) = composeL
infix right 9 (>>) = composeR


type Bool
    = True
    | False


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
type Order
    = LT
    | EQ
    | GT


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

    else if eq (Pine_kernel.head listA) (Pine_kernel.head listB) then
        listsEqualRecursive
            (Pine_kernel.skip [ 1, listA ])
            (Pine_kernel.skip [ 1, listB ])

    else
        False


dictToList : Dict k v -> List ( k, v )
dictToList dict =
    case dict of
        RBEmpty_elm_builtin ->
            []

        RBNode_elm_builtin _ key value left right ->
            Pine_kernel.concat [ dictToList left, [ ( key, value ) ], dictToList right ]


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

            absQuotient =
                idivHelper absDividend absDivisor 0
        in
        if Pine_kernel.equal [ dividendNegative, divisorNegative ] then
            absQuotient

        else
            -absQuotient


idivHelper : Int -> Int -> Int -> Int
idivHelper dividend divisor quotient =
    let
        scaledDivisor =
            mul divisor 16
    in
    if Pine_kernel.int_is_sorted_asc [ scaledDivisor, dividend ] then
        let
            scaledQuotient =
                idivHelper
                    dividend
                    scaledDivisor
                    0

            scaledQuotientSum =
                mul scaledQuotient 16

            remainder =
                sub dividend (mul scaledQuotient scaledDivisor)

            remainderQuotient =
                idivHelper remainder divisor 0
        in
        add scaledQuotientSum remainderQuotient

    else if Pine_kernel.int_is_sorted_asc [ divisor, dividend ] then
        idivHelper
            (sub dividend divisor)
            divisor
            (add quotient 1)

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
    case ( a, b ) of
        ( String stringA, String stringB ) ->
            String (Pine_kernel.concat [ stringA, stringB ])

        _ ->
            Pine_kernel.concat [ a, b ]


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
    if lt x y then
        x

    else
        y


{-| Find the larger of two comparables.

    max 42 12345678 == 12345678

    max "abc" "xyz" == "xyz"

-}
max : comparable -> comparable -> comparable
max x y =
    if gt x y then
        x

    else
        y


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
        remainder =
            remainderBy divisor dividend
    in
    if Pine_kernel.int_is_sorted_asc [ 0, remainder ] then
        remainder

    else
        Pine_kernel.int_add [ remainder, divisor ]


remainderBy : Int -> Int -> Int
remainderBy divisor dividend =
    Pine_kernel.int_add
        [ dividend
        , Pine_kernel.negate (Pine_kernel.int_mul [ divisor, idiv dividend divisor ])
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

    abs 16 == 16

    abs -4 == 4

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
        ( multiplier, denomProd ) =
            findMultiplierToDecimal 1 denom
    in
    idiv
        (Pine_kernel.int_mul [ numerator, multiplier ])
        denomProd


findMultiplierToDecimal : Int -> Int -> ( Int, Int, Int )
findMultiplierToDecimal factor denom =
    let
        denomProd =
            Pine_kernel.int_mul [ denom, factor ]

        lowerPowerOfTen =
            findLowerPowerOfTen denomProd
    in
    if Pine_kernel.equal [ pow 10 lowerPowerOfTen, denomProd ] then
        ( factor, denomProd )

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
