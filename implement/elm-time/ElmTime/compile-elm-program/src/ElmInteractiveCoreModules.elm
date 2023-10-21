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
infix left  9 (<<) = composeL
infix right 9 (>>) = composeR


type Bool = True | False


type String
    = String (List Char.Char)


{-| Represents the relative ordering of two things.
The relations are less than, equal to, and greater than.
-}
type Order = LT | EQ | GT


eq : a -> a -> Bool
eq a b =
    Pine_kernel.equal [ a, b ]


neq : a -> a -> Bool
neq a b =
    Pine_kernel.negate (Pine_kernel.equal [ a, b ])


add : number -> number -> number
add a b =
    Pine_kernel.add_int [ a, b ]


sub : number -> number -> number
sub a b =
    Pine_kernel.sub_int [ a, b ]


mul : number -> number -> number
mul a b =
    Pine_kernel.mul_int [ a, b ]


idiv : number -> number -> number
idiv a b =
    Pine_kernel.div_int [ a, b ]


and : Bool -> Bool -> Bool
and a b =
    Pine_kernel.logical_and [ a, b ]


or : Bool -> Bool -> Bool
or a b =
    Pine_kernel.logical_or [ a, b ]


append : appendable -> appendable -> appendable
append a b =
    case (a, b) of
    (String stringA, String stringB) ->
        String (Pine_kernel.concat [ stringA, stringB ])
    _ -> Pine_kernel.concat [ a, b ]


lt : comparable -> comparable -> Bool
lt a b =
    Pine_kernel.logical_and
    [ (le a b)
    , Pine_kernel.negate (Pine_kernel.equal [a, b])
    ]


gt : comparable -> comparable -> Bool
gt a b =
    Pine_kernel.logical_and
    [ (ge a b)
    , Pine_kernel.negate (Pine_kernel.equal [a, b])
    ]


le : comparable -> comparable -> Bool
le a b =
    Pine_kernel.is_sorted_ascending_int [a, b]


ge : comparable -> comparable -> Bool
ge a b =
    Pine_kernel.is_sorted_ascending_int [b, a]


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
    if Pine_kernel.equal [ bool, True ] then
        False
    else
        True


{-| Compare any two comparable values. Comparable values include `String`,
`Char`, `Int`, `Float`, or a list or tuple containing comparable values. These
are also the only values that work as `Dict` keys or `Set` members.

    compare 3 4 == LT
    compare 4 4 == EQ
    compare 5 4 == GT
-}
compare : comparable -> comparable -> Order
compare a b =
    if eq a b
    then
        EQ
    else
        case (a, b) of
        (String stringA, String stringB) ->
            compareList
                (stringCharsToSignedInts stringA)
                (stringCharsToSignedInts stringB)

        _ ->
            if isPineList a
            then
                compareList a b
            else if le a b
            then
                LT
            else
                GT


compareList : List comparable -> List comparable -> Order
compareList listA listB =
    case (listA, listB) of
    (headA :: tailA, headB :: tailB) ->
        let
            headOrder =
                compare headA headB
        in
            if eq headOrder EQ
            then
                compareList tailA tailB
            else
                headOrder

    _ ->
        compare (Pine_kernel.length listA) (Pine_kernel.length listB)


stringCharsToSignedInts : List Char -> List Int
stringCharsToSignedInts chars =
    case chars of
    head :: tail ->
        -- Add the sign prefix byte
        Pine_kernel.concat
            [ [ Pine_kernel.concat [ Pine_kernel.take [ 1, 0 ], head ] ]
            , stringCharsToSignedInts tail
            ]
    _ ->
        []


modBy : Int -> Int -> Int
modBy divisor dividend =
    let
        remainder = remainderBy divisor dividend
    in
        if lt remainder 0 then
            add remainder divisor
        else
            remainder


remainderBy : Int -> Int -> Int
remainderBy divisor dividend =
    sub dividend (mul divisor (idiv dividend divisor))


{-| Negate a number.

    negate 42 == -42
    negate -42 == 42
    negate 0 == 0
-}
negate : number -> number
negate n =
  -n


{-| Get the [absolute value][abs] of a number.

    abs 16   == 16
    abs -4   == 4
    abs -8.5 == 8.5
    abs 3.14 == 3.14

[abs]: https://en.wikipedia.org/wiki/Absolute_value
-}
abs : number -> number
abs n =
  if lt n 0 then -n else n


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


import Basics exposing (..)
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
    if n <= 0 then
        result
    else
        repeatHelp (cons value result) (n - 1) value


range : Int -> Int -> List Int
range lo hi =
    rangeHelp lo hi []


rangeHelp : Int -> Int -> List Int -> List Int
rangeHelp lo hi list =
    if lo <= hi then
        rangeHelp lo (hi - 1) (cons hi list)
    else
        list


cons : a -> List a -> List a
cons element list =
    Pine_kernel.concat [ [ element ], list ]


map : (a -> b) -> List a -> List b
map f xs =
    foldr (\\x acc -> cons (f x) acc ) [] xs


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
    foldr (\\x xs -> if isGood x then cons x xs else xs) [] list


length : List a -> Int
length list =
    Pine_kernel.length list


reverse : List a -> List a
reverse list =
    Pine_kernel.reverse list


member : a -> List a -> Bool
member x xs =
    any ((==) x) xs


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


append : List a -> List a -> List a
append xs ys =
    concat [ xs, ys ]


concat : List (List a) -> List a
concat lists =
    Pine_kernel.concat lists


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
    (code <= 0x39) && (0x30 <= code)


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
    code <= 0x37 && 0x30 <= code


{-| Detect hexadecimal digits `0123456789abcdefABCDEF`
-}
isHexDigit : Char -> Bool
isHexDigit char =
    let
        code =
            toCode char
    in
    (0x30 <= code && code <= 0x39)
        || (0x41 <= code && code <= 0x46)
        || (0x61 <= code && code <= 0x66)

"""
    , """
module String exposing
  ( String
  , isEmpty, length, reverse, repeat, replace
  , append, concat, split, join, words, lines
  , slice, left, right, dropLeft, dropRight
  , contains, startsWith, endsWith, indexes, indices
  , toInt, fromInt
  , toFloat, fromFloat
  , fromChar, cons, uncons
  , toList, fromList
  , toUpper, toLower, pad, padLeft, padRight, trim, trimLeft, trimRight
  , map, filter, foldl, foldr, any, all
  )


import Basics exposing (..)
import Char
import List exposing ((::))
import Maybe exposing (Maybe)
import Tuple


type String
    = String (List Char.Char)


toList : String -> List Char
toList string =
    case string of
    String list -> list


fromList : List Char -> String
fromList =
    String


fromChar : Char -> String
fromChar char =
    String [ char ]


cons : Char -> String -> String
cons char string =
    fromList (char :: toList string)


uncons : String -> Maybe ( Char, String )
uncons string =
    case toList string of
        [] ->
            Nothing

        first :: rest ->
            Just ( first, fromList rest )


isEmpty : String -> Bool
isEmpty string =
    string == ""


length : String -> Int
length =
    toList >> List.length


reverse : String -> String
reverse =
    toList >> List.reverse >> fromList


repeat : Int -> String -> String
repeat n =
    toList >> List.repeat n >> List.concat >> fromList


replace : String -> String -> String -> String
replace before after string =
    join after (split before string)


append : String -> String -> String
append a b =
    fromList (Pine_kernel.concat [ toList a, toList b ])


concat : List String -> String
concat strings =
    join "" strings


split : String -> String -> List String
split sep string =
    if sep == "" then
        List.map fromChar (toList string)

    else splitHelperOnList [] (toList sep) (toList string)


splitHelperOnList : List Char -> List Char -> List Char -> List String
splitHelperOnList current sep string =
    if string == [] then
        [ fromList current ]

    else if sep == (List.take (List.length sep) string) then
        [ fromList current ] ++ splitHelperOnList [] sep (List.drop (List.length sep) string)

    else
        splitHelperOnList (current ++ List.take 1 string) sep (List.drop 1 string)


join : String -> List String -> String
join sep chunks =
    fromList (joinOnList (toList sep) (List.map toList chunks))


joinOnList : List Char -> List (List Char) -> List Char
joinOnList sep chunks =
    case chunks of
        [] ->
            []

        nextChunk :: remaining ->
            if remaining == []
            then
                nextChunk
            else
                nextChunk ++ sep ++ joinOnList sep remaining


slice : Int -> Int -> String -> String
slice start end string =
    let
        absoluteIndex relativeIndex =
            if relativeIndex < 0 then
                relativeIndex + length string

            else
                relativeIndex

        absoluteStart =
            absoluteIndex start
    in
    fromList (List.take (absoluteIndex end - absoluteStart) (List.drop absoluteStart (toList string)))


left : Int -> String -> String
left n string =
    fromList (List.take n (toList string))


right : Int -> String -> String
right n string =
    if n < 1 then
        ""
    else
        slice -n (length string) string


dropLeft : Int -> String -> String
dropLeft n string =
    fromList (List.drop n (toList string))


dropRight : Int -> String -> String
dropRight n string =
    if n < 1 then
        string
    else
        slice 0 -n string


contains : String -> String -> Bool
contains pattern string =
    if startsWith pattern string then
        True
    else
        if length pattern < length string then
            contains pattern (dropLeft 1 string)
        else
            False


startsWith : String -> String -> Bool
startsWith pattern string =
    left (length pattern) string == pattern


endsWith : String -> String -> Bool
endsWith pattern string =
    right (length pattern) string == pattern


toInt : String -> Maybe Int
toInt string =
    toIntFromList (toList string)


fromInt : Int -> String
fromInt int =
    fromList (fromIntAsList int)


toIntFromList : List Char -> Maybe Int
toIntFromList stringAsList =
    case stringAsList of
        [] ->
            Nothing

        firstChar :: lessFirstChar ->
            let
                ( valueString, signMultiplier ) =
                    case firstChar of
                        '-' ->
                            ( lessFirstChar, -1 )

                        '+' ->
                            ( lessFirstChar, 1 )

                        _ ->
                            ( stringAsList, 1 )
            in
            Maybe.map ((*) signMultiplier)
                (toUnsignedIntFromList valueString)


toUnsignedIntFromList : List Char -> Maybe Int
toUnsignedIntFromList string =
    let
        digitValueFromCharacter char =
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

    in
    case string of
        [] ->
            Nothing

        digits ->
            List.foldl
                (\\maybeDigitValue maybeAggregate ->
                    case (maybeDigitValue, maybeAggregate) of
                        (Just digitValue, Just aggregate) ->
                            Just (aggregate * 10 + digitValue)
                        
                        _ ->
                            Nothing
                )
                (Just 0)
                (List.map digitValueFromCharacter digits)


fromIntAsList : Int -> List Char
fromIntAsList int =
    if int < 0 then
        [ '-' ] ++ fromUnsignedIntAsList -int

    else
        fromUnsignedIntAsList int


fromUnsignedIntAsList : Int -> List Char
fromUnsignedIntAsList int =
    let
        digitCharacterFromValue digitValue =
            case digitValue of
                0 ->
                    Just '0'

                1 ->
                    Just '1'

                2 ->
                    Just '2'

                3 ->
                    Just '3'

                4 ->
                    Just '4'

                5 ->
                    Just '5'

                6 ->
                    Just '6'

                7 ->
                    Just '7'

                8 ->
                    Just '8'

                9 ->
                    Just '9'

                _ ->
                    Nothing                

        upperDigitsValue =
            int // 10

        lastDigitValue =
            int - (upperDigitsValue * 10)

        upperDigitsString =
            if upperDigitsValue < 1 then
                []

            else
                fromUnsignedIntAsList upperDigitsValue

        lastDigitChar =
            digitCharacterFromValue lastDigitValue
    in
    upperDigitsString ++ [ Maybe.withDefault 'e' lastDigitChar ]


trim : String -> String
trim str =
    fromList (dropWhileList isCharRemovedOnTrim (List.reverse (dropWhileList isCharRemovedOnTrim (List.reverse (toList str)))))


trimLeft : String -> String
trimLeft str =
    fromList (dropWhileList isCharRemovedOnTrim (toList str))


trimRight : String -> String
trimRight str =
    fromList (List.reverse (dropWhileList isCharRemovedOnTrim (List.reverse (toList str))))


isCharRemovedOnTrim : Char -> Bool
isCharRemovedOnTrim char =
    Char.toCode char <= 32


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

"""
    , """
module Array exposing (..)


import Basics exposing (..)
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
    if index < 0
    then
        Nothing
    else
        List.head (List.drop index array)


set : Int -> a -> Array a -> Array a
set index value array =
    if (index < 0) || (index >= (Pine_kernel.length array))
    then
        array
    else
        Pine_kernel.concat [ Pine_kernel.take [ index, array ], [ value ], Pine_kernel.skip [ index + 1, array ] ]


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
map =
    List.map


foldr : (a -> b -> b) -> b -> Array a -> b
foldr =
    List.foldr


foldl : (a -> b -> b) -> b -> Array a -> b
foldl =
    List.foldl

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


import Basics exposing (..)
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
      case compare targetKey key of
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
  foldl insert t2 t1


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
  foldr (\\key value keyList -> key :: keyList) [] dict


{-| Get all of the values in a dictionary, in the order of their keys.

    values (fromList [(0,"Alice"),(1,"Bob")]) == ["Alice", "Bob"]
-}
values : Dict k v -> List v
values dict =
  foldr (\\key value valueList -> value :: valueList) [] dict


{-| Convert a dictionary into an association list of key-value pairs, sorted by keys. -}
toList : Dict k v -> List (k,v)
toList dict =
  foldr (\\key value list -> (key,value) :: list) [] dict


{-| Convert an association list into a dictionary. -}
fromList : List (comparable,v) -> Dict comparable v
fromList assocs =
  List.foldl (\\(key,value) dict -> insert key value dict) empty assocs

"""
    ]
