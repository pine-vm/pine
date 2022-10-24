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
    Pine_kernel.equal [ Pine_kernel.sort_int [a, b], [a, b] ]


ge : comparable -> comparable -> Bool
ge a b =
    Pine_kernel.equal [ Pine_kernel.sort_int [b, a], [b, a] ]


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
    foldr (\\x acc -> cons (f x) acc) [] xs


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
    any (\\a -> a == x) xs


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
    Pine_kernel.concat [ toList a, toList b ]


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
toInt =
    toIntDecimal


toIntDecimal : String -> Maybe Int
toIntDecimal =
    toIntFromDigitsChars digitCharactersDecimal


fromInt : Int -> String
fromInt =
    fromIntDecimal


fromIntDecimal : Int -> String
fromIntDecimal int =
    fromList (fromIntFromDigitsChars digitCharactersDecimal int)


digitCharactersDecimal : List ( Char, Int )
digitCharactersDecimal =
    [ ( '0', 0 )
    , ( '1', 1 )
    , ( '2', 2 )
    , ( '3', 3 )
    , ( '4', 4 )
    , ( '5', 5 )
    , ( '6', 6 )
    , ( '7', 7 )
    , ( '8', 8 )
    , ( '9', 9 )
    ]


toIntFromDigitsChars : List ( Char, Int ) -> String -> Maybe Int
toIntFromDigitsChars digitsCharacters string =
    case toList string of
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
                            ( toList string, 1 )
            in
            Maybe.map (\\value -> value * signMultiplier)
                (toUnsignedIntFromDigitsChars digitsCharacters valueString)


toUnsignedIntFromDigitsChars : List ( Char, Int ) -> List Char -> Maybe Int
toUnsignedIntFromDigitsChars digitsCharacters string =
    let
        digitValueFromCharacter char =
            Maybe.map Tuple.second (List.head (List.filter (\\(c, _) -> c == char) digitsCharacters))
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
                                    Just (aggregate * List.length digitsCharacters + digitValue)
                        )
                )
                (Just 0)
                (List.map digitValueFromCharacter digits)


fromIntFromDigitsChars : List ( Char, Int ) -> Int -> List Char
fromIntFromDigitsChars digitsCharacters int =
    if int < 0 then
        [ '-' ] ++ fromIntFromDigitsChars digitsCharacters -int

    else
        let
            digitCharacterFromValue digitValue =
                Maybe.map Tuple.first (List.head (List.filter (\\( _, c ) -> c == digitValue) digitsCharacters))

            upperDigitsValue =
                int // 10

            lastDigitValue =
                int - (upperDigitsValue * 10)

            upperDigitsString =
                if upperDigitsValue < 1 then
                    []

                else
                    fromIntFromDigitsChars digitsCharacters upperDigitsValue
        in
        upperDigitsString ++ [ Maybe.withDefault 'e' (digitCharacterFromValue lastDigitValue) ]

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
