module ElmInteractiveCoreModules exposing (..)


elmCoreModulesTexts : List String
elmCoreModulesTexts =
    [ """
module Basics exposing (..)


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
    Pine_kernel.logical_not (Pine_kernel.equal [ a, b ])


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
append a_in_append b_in_append =
    case a_in_append of
    String stringA ->
        case b_in_append of
        String stringB ->
            String (Pine_kernel.concat [ stringA, stringB ])
        _ -> Pine_kernel.concat [ a_in_append, b_in_append ]
    _ -> Pine_kernel.concat [ a_in_append, b_in_append ]


lt : comparable -> comparable -> Bool
lt a b =
    Pine_kernel.logical_and
    [ (le a b)
    , Pine_kernel.logical_not (Pine_kernel.equal [a, b])
    ]


gt : comparable -> comparable -> Bool
gt a b =
    Pine_kernel.logical_and
    [ (ge a b)
    , Pine_kernel.logical_not (Pine_kernel.equal [a, b])
    ]


le : comparable -> comparable -> Bool
le a b =
    Pine_kernel.is_sorted_ascending_int [a, b]


ge : comparable -> comparable -> Bool
ge a b =
    Pine_kernel.is_sorted_ascending_int [b, a]


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
    if Pine_kernel.equal [ a, b ]
    then
        EQ
    else
        case a of
        String stringA ->
            case b of
            String stringB ->
                compareList
                    (stringCharsToSignedInts stringA)
                    (stringCharsToSignedInts stringB)

            _ ->
                compareIgnoringString a b

        _ -> compareIgnoringString a b


compareIgnoringString : comparable -> comparable -> Order
compareIgnoringString a b =
    if isPineList a
    then
        compareList a b
    else if Pine_kernel.is_sorted_ascending_int [ a, b ]
    then
        LT
    else
        GT


compareList : List comparable -> List comparable -> Order
compareList listA listB =
    let
        compareListEmpty =
            compare (Pine_kernel.length listA) (Pine_kernel.length listB)
    in
    case listA of
    headA :: tailA ->
        case listB of
        headB :: tailB ->
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
            compareListEmpty

    _ ->
        compareListEmpty


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
module List exposing (..)


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

"""
    , """
module String exposing (..)

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
                (\\maybeDigitValue ->
                    Maybe.andThen
                        (\\aggregate ->
                            case maybeDigitValue of
                                Nothing ->
                                    Nothing

                                Just digitValue ->
                                    Just (aggregate * 10 + digitValue)
                        )
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
module Bytes exposing (..)


type Bytes
    = Bytes (List Int)


width : Bytes -> Int
width bytes =
    case bytes of
    Bytes list -> Pine_kernel.length list


type Endianness = LE | BE

"""
    , """
module Bytes.Encode exposing (..)


import Bytes


type Encoder
  = U8 Int
  | U16 Endianness Int
  | U32 Endianness Int
  | SequenceEncoder (List Encoder)
  | BytesEncoder Bytes.Bytes


encode : Encoder -> Bytes.Bytes
encode builder =
    Bytes.Bytes (encodeBlob builder)


encodeBlob : Encoder -> List Int
encodeBlob builder =
  case builder of
    U8    n ->
        Pine_kernel.take [ 1, (Pine_kernel.reverse n) ]

    U16 e n ->
        let
            littleEndian =
                Pine_kernel.take [ 2, (Pine_kernel.reverse (Pine_kernel.skip [ 1, n ])) ]
        in
        if (e == Bytes.LE)
        then littleEndian
        else Pine_kernel.reverse littleEndian

    U32 e n ->
        let
            littleEndian =
                Pine_kernel.take [ 4, (Pine_kernel.reverse (Pine_kernel.skip [ 1, n ])) ]
        in
        if (e == Bytes.LE)
        then littleEndian
        else Pine_kernel.reverse littleEndian

    SequenceEncoder bs ->
        Pine_kernel.concat (List.map encodeBlob bs)

    BytesEncoder bs ->
        case bs of
        Bytes.Bytes blob -> blob


-- INTEGERS


{-| Encode integers from `0` to `255` in one byte.
-}
unsignedInt8 : Int -> Encoder
unsignedInt8 =
  U8


{-| Encode integers from `0` to `65535` in two bytes.
-}
unsignedInt16 : Endianness -> Int -> Encoder
unsignedInt16 =
  U16


{-| Encode integers from `0` to `4294967295` in four bytes.
-}
unsignedInt32 : Endianness -> Int -> Encoder
unsignedInt32 =
  U32


bytes : Bytes.Bytes -> Encoder
bytes =
  Bytes.Bytes


sequence : List Encoder -> Encoder
sequence builders =
  SequenceEncoder builders

"""
    ]
