module BigInt exposing
    ( BigInt
    , fromInt, fromIntString, toString
    , add, sub, mul, div, modBy, divmod, pow
    , abs, negate
    , compare, gt, gte, lt, lte, max, min
    , isEven, isOdd
    )

{-| Infinite digits integers

@docs BigInt


# From/To

@docs fromInt, fromIntString, fromHexString, toString, toHexString


# Operations

@docs add, sub, mul, div, modBy, divmod, pow


# Sign

@docs abs, negate


# Comparison

@docs compare, gt, gte, lt, lte, max, min


# Misc

@docs isEven, isOdd

-}

import Basics
import Maybe exposing (Maybe)


{-| This variant of the BigInt module depends on the default integer type in environments where the
Elm core libraries are based on the Pine kernel.
Since integer arithmetic in Pine has no limits, we don't need to do any composition here.
-}
type alias BigInt =
    Int


{-| Makes a BigInt from an Int
-}
fromInt : Int -> BigInt
fromInt x =
    x


{-| Makes a BigInt from an integer string, positive or negative

    fromIntString "123" == Just (BigInt.Pos ...)
    fromIntString "-123" == Just (BigInt.Neg ...)
    fromIntString "" == Nothing
    fromIntString "this is not a number :P" == Nothing

-}
fromIntString : String -> Maybe BigInt
fromIntString x =
    String.toInt x


{-| Adds two BigInts
-}
add : BigInt -> BigInt -> BigInt
add a b =
    a + b


{-| Changes the sign of an BigInt
-}
negate : BigInt -> BigInt
negate bigInt =
    -bigInt


{-| Absolute value
-}
abs : BigInt -> BigInt
abs bigInt =
    Basics.abs bigInt


{-| Substracts the second BigInt from the first
-}
sub : BigInt -> BigInt -> BigInt
sub a b =
    add a (negate b)


{-| Multiplies two BigInts
-}
mul : BigInt -> BigInt -> BigInt
mul int1 int2 =
    int1 * int2


{-| Compares two BigInts
-}
compare : BigInt -> BigInt -> Order
compare int1 int2 =
    Basics.compare int1 int2


{-| Less than
-}
lt : BigInt -> BigInt -> Bool
lt x y =
    compare x y == LT


{-| Greater than
-}
gt : BigInt -> BigInt -> Bool
gt x y =
    compare x y == GT


{-| Greater than or equals
-}
gte : BigInt -> BigInt -> Bool
gte x y =
    not (lt x y)


{-| Less than or equals
-}
lte : BigInt -> BigInt -> Bool
lte x y =
    not (gt x y)


{-| Returns the largest of two BigInts
-}
max : BigInt -> BigInt -> BigInt
max x y =
    if lt x y then
        y

    else
        x


{-| Returns the smallest of two BigInts
-}
min : BigInt -> BigInt -> BigInt
min x y =
    if gt x y then
        y

    else
        x


{-| Convert the BigInt to an integer string
-}
toString : BigInt -> String
toString bigInt =
    String.fromInt bigInt


{-| BigInt division. Produces 0 when dividing by 0 (like (//)).
-}
div : BigInt -> BigInt -> BigInt
div num den =
    divmod num den
        |> Maybe.map Tuple.first
        |> Maybe.withDefault zero


{-| Modulus.

    modBy (BigInt.fromInt 3) (BigInt.fromInt 3)

-}
modBy : BigInt -> BigInt -> Maybe BigInt
modBy modulus x =
    divmod x modulus |> Maybe.map Tuple.second


{-| Square.
-}
square : BigInt -> BigInt
square num =
    mul num num


{-| Parity Check - Even.
-}
isEven : BigInt -> Bool
isEven num =
    Basics.modBy 2 num == 0


{-| Parity Check - Odd.
-}
isOdd : BigInt -> Bool
isOdd num =
    not (isEven num)


{-| Power/Exponentiation.
-}
pow : BigInt -> BigInt -> BigInt
pow base exp =
    powHelp one base exp


{-| Power helper, for sake of tail-recursion.
-}
powHelp : BigInt -> BigInt -> BigInt -> BigInt
powHelp work num exp =
    if exp == 0 then
        one

    else if exp < 0 then
        0

    else if exp == one then
        mul work num

    else if isEven exp then
        powHelp work (square num) (div exp two)

    else
        powHelp (mul num work) (square num) (div (sub exp one) two)


{-| Division and modulus
-}
divmod : BigInt -> BigInt -> Maybe ( BigInt, BigInt )
divmod num den =
    if den == zero then
        Nothing

    else
        let
            quotient =
                num // den
        in
        Just
            ( quotient
            , sub num (quotient * den)
            )


zero : BigInt
zero =
    fromInt 0


one : BigInt
one =
    fromInt 1


two : BigInt
two =
    fromInt 2
