module ElmInteractiveKernelModules exposing (..)


elmKernelModulesTexts : List String
elmKernelModulesTexts =
    [ """
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
  List.foldl insert empty list


{-| Fold over the values in a set, in order from lowest to highest.
-}
foldl : (a -> b -> b) -> b -> Set a -> b
foldl func initialState (Set_elm_builtin dict) =
  Dict.foldl (\\key _ state -> func key state) initialState dict


{-| Fold over the values in a set, in order from highest to lowest.
-}
foldr : (a -> b -> b) -> b -> Set a -> b
foldr func initialState (Set_elm_builtin dict) =
  Dict.foldr (\\key _ state -> func key state) initialState dict


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
    , """
module Json.Encode exposing (..)


type Value
    = NullValue
    | BoolValue Bool
    | IntValue Int
    | StringValue String
    | ArrayValue (List Value)
    | ObjectValue (List ( String, Value ))


null : Value
null =
    NullValue


bool : Bool -> Value
bool =
    BoolValue


int : Int -> Value
int =
    IntValue


string : String -> Value
string =
    StringValue


list : (item -> Value) -> List item -> Value
list encodeItem items =
    ArrayValue (List.map encodeItem items)


object : List ( String, Value ) -> Value
object =
    ObjectValue


encode : Int -> Value -> String
encode indent value =
    case value of
        NullValue ->
            "null"

        BoolValue boolVal ->
            if boolVal then
                "true"

            else
                "false"

        IntValue intVal ->
            String.fromInt intVal

        StringValue stringVal ->
            "\\"" ++ escapeString stringVal ++ "\\""

        ArrayValue values ->
            "[" ++ String.join "," (List.map (encode indent) values) ++ "]"

        ObjectValue fields ->
            "{" ++ String.join "," (List.map (encodeField indent) fields) ++ "}"


encodeField : Int -> ( String, Value ) -> String
encodeField indent ( key, value ) =
    "\\"" ++ escapeString key ++ "\\":" ++ encode indent value


escapeString : String -> String
escapeString stringVal =
    String.join "" (List.map escapeChar (String.toList stringVal))


escapeChar : Char -> String
escapeChar char =
    case Char.toCode char of
        8 ->
            "\\\\b"

        9 ->
            "\\\\t"

        10 ->
            "\\\\n"

        12 ->
            "\\\\f"

        13 ->
            "\\\\r"

        34 ->
            "\\\\\\""

        92 ->
            "\\\\\\\\"

        _ ->
            String.fromChar char

"""
    , """
module Json.Decode exposing (..)


import Json.Encode exposing (Value(..))


{-| A structured error describing exactly how the decoder failed. You can use
this to create more elaborate visualizations of a decoder problem. For example,
you could show the entire JSON object and show the part causing the failure in
red.
-}
type Error
    = Field String Error
    | Index Int Error
    | OneOf (List Error)
    | Failure String Value


type alias Decoder a =
    Value -> Result Error a


temporaryStubErrorNotImplemented : Value -> Error
temporaryStubErrorNotImplemented =
    Failure "Json.Decode error not implemented yet"


decodeValue : Decoder a -> Value -> Result Error a
decodeValue decoder jsonValue =
    decoder jsonValue


{-| Do not do anything with a JSON value, just bring it into Elm as a `Value`.
This can be useful if you have particularly complex data that you would like to
deal with later. Or if you are going to send it out a port and do not care
about its structure.
-}
value : Decoder Value
value =
    Ok


{-| Decode a `null` value into some Elm value.

    decodeString (null False) "null" == Ok False
    decodeString (null 42) "null"    == Ok 42
    decodeString (null 42) "42"      == Err ..
    decodeString (null 42) "false"   == Err ..

So if you ever see a `null`, this will return whatever value you specified.

-}
null : a -> Decoder a
null ok jsonValue =
    if jsonValue == NullValue then
        Ok ok

    else
        Err (temporaryStubErrorNotImplemented jsonValue)


bool : Decoder Bool
bool jsonValue =
    case jsonValue of
        BoolValue boolVal ->
            Ok boolVal

        _ ->
            Err (temporaryStubErrorNotImplemented jsonValue)


int : Decoder Int
int jsonValue =
    case jsonValue of
        IntValue intVal ->
            Ok intVal

        _ ->
            Err (temporaryStubErrorNotImplemented jsonValue)


string : Decoder String
string jsonValue =
    case jsonValue of
        StringValue stringVal ->
            Ok stringVal

        _ ->
            Err (temporaryStubErrorNotImplemented jsonValue)


{-| Ignore the JSON and produce a certain Elm value.

    decodeString (succeed 42) "true"    == Ok 42
    decodeString (succeed 42) "[1,2,3]" == Ok 42
    decodeString (succeed 42) "hello"   == Err ... -- this is not a valid JSON string

This is handy when used with `oneOf` or `andThen`.

-}
succeed : a -> Decoder a
succeed success _ =
    Ok success


{-| Ignore the JSON and make the decoder fail. This is handy when used with
`oneOf` or `andThen` where you want to give a custom error message in some
case.

See the [`andThen`](#andThen) docs for an example.

-}
fail : String -> Decoder a
fail error jsonValue =
    Err (Failure error jsonValue)


{-| Parse the given string into a JSON value and then run the `Decoder` on it.
This will fail if the string is not well-formed JSON or if the `Decoder`
fails for some reason.

    decodeString int "4"     == Ok 4
    decodeString int "1 + 2" == Err ...

-}
decodeString : Decoder a -> String -> Result Error a
decodeString decoder jsonString =
    case parseJsonString jsonString of
        Err parseErr ->
            Err (Failure ("Bad JSON: " ++ parseErr) NullValue)

        Ok jsonValue ->
            decodeValue decoder jsonValue


parseJsonString : String -> Result String Value
parseJsonString jsonString =
    case parseValue (String.toList (String.trim jsonString)) of
        ( Ok ok, consumed ) ->
            if consumed < String.length jsonString then
                Err ("Unexpected string at the end: " ++ String.dropLeft consumed jsonString)

            else
                Ok ok

        ( Err err, _ ) ->
            Err err


type alias Parser a =
    List Char -> ( Result String a, Int )


parseValue : Parser Value
parseValue str =
    if str == [] then
        ( Err "Unexpected end of input", 0 )

    else if listStartsWith [ 'n', 'u', 'l', 'l' ] str then
        ( Ok NullValue, 4 )

    else if listStartsWith [ 't', 'r', 'u', 'e' ] str then
        ( Ok (BoolValue True), 4 )

    else if listStartsWith [ 'f', 'a', 'l', 's', 'e' ] str then
        ( Ok (BoolValue False), 5 )

    else if listStartsWith [ '"' ] str then
        parseString (List.drop 1 str)
            |> Tuple.mapFirst (Result.map (String.fromList >> StringValue))
            |> Tuple.mapSecond ((+) 1)

    else if listStartsWith [ '[' ] str then
        parseArray (List.drop 1 str)
            |> Tuple.mapFirst (Result.map ArrayValue)
            |> Tuple.mapSecond ((+) 1)

    else if listStartsWith [ '{' ] str then
        parseObject (List.drop 1 str)
            |> Tuple.mapFirst (Result.map ObjectValue)
            |> Tuple.mapSecond ((+) 1)

    else
        parseInt str
            |> Tuple.mapFirst (Result.map IntValue)


parseString : Parser (List Char)
parseString str =
    case str of
        [] ->
            ( Err "Unexpected end of input while parsing string", 0 )

        nextChar :: following ->
            case nextChar of
                '"' ->
                    ( Ok [], 1 )

                _ ->
                    parseString following
                        |> Tuple.mapFirst (Result.map ((::) nextChar))
                        |> Tuple.mapSecond ((+) 1)


parseInt : Parser Int
parseInt str =
    case str of
        '-' :: rest ->
            let
                ( result, offset ) =
                    parseUnsignedInt rest
            in
            case result of
                Ok intAbsolute ->
                    ( Ok -intAbsolute
                    , offset + 1
                    )

                Err err ->
                    ( Err err
                    , offset + 1
                    )

        _ ->
            parseUnsignedInt str


parseUnsignedInt : Parser Int
parseUnsignedInt str =
    parseUnsignedIntHelp ( 0, Nothing ) str


parseUnsignedIntHelp : ( Int, Maybe Int ) -> Parser Int
parseUnsignedIntHelp previousDigits str =
    let
        ( previousDigitsCount, maybePreviusDigitsValue ) =
            previousDigits

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

        complete =
            ( Result.fromMaybe "No digits found" maybePreviusDigitsValue
            , previousDigitsCount
            )
    in
    case str of
        [] ->
            complete

        currentChar :: followingChars ->
            case digitValueFromCharacter currentChar of
                Just digitValue ->
                    parseUnsignedIntHelp
                        ( previousDigitsCount + 1
                        , Just (Maybe.withDefault 0 maybePreviusDigitsValue * 10 + digitValue)
                        )
                        followingChars

                Nothing ->
                    complete


countCharsWhile : (Char -> Bool) -> List Char -> Int
countCharsWhile predicate str =
    case str of
        [] ->
            0

        char :: rest ->
            if predicate char then
                countCharsWhile predicate rest + 1

            else
                0


parseArray : Parser (List Value)
parseArray str =
    parseArrayHelper [] str
        |> Tuple.mapFirst (Result.map List.reverse)


parseArrayHelper : List Value -> Parser (List Value)
parseArrayHelper previousItems str =
    let
        strTrimmed =
            dropWhileList isCharRemovedOnTrim str

        trimmedLength =
            List.length str - List.length strTrimmed
    in
    case strTrimmed of
        [] ->
            ( Err "Unexpected end of input while parsing array"
            , 0
            )

        ']' :: _ ->
            ( Ok previousItems
            , trimmedLength + 1
            )

        _ ->
            parseArrayHelperTrimmedNotEmpty previousItems strTrimmed
                |> Tuple.mapSecond ((+) trimmedLength)


parseArrayHelperTrimmedNotEmpty : List Value -> Parser (List Value)
parseArrayHelperTrimmedNotEmpty previousItems strTrimmed =
    let
        ( valueResult, itemLength ) =
            parseValue strTrimmed
    in
    case valueResult of
        Err err ->
            ( Err err, itemLength )

        Ok itemValue ->
            let
                strAfterItem =
                    List.drop itemLength strTrimmed

                strAfterValueTrimmed =
                    dropWhileList isCharRemovedOnTrim strAfterItem

                trimmedLength =
                    List.length strAfterItem - List.length strAfterValueTrimmed
            in
            case strAfterValueTrimmed of
                ',' :: afterComma ->
                    parseArrayHelper (itemValue :: previousItems) afterComma
                        |> Tuple.mapSecond ((+) (itemLength + trimmedLength + 1))

                _ ->
                    parseArrayHelper (itemValue :: previousItems) strAfterValueTrimmed
                        |> Tuple.mapSecond ((+) (itemLength + trimmedLength))


parseObject : Parser (List ( String, Value ))
parseObject str =
    -- Object parsing logic goes here
    -- Similar to `parseArray`, but also needs to handle the colon (:) separating keys and values.
    ( Err "Object parsing not implemented", 0 )


listStartsWith : List a -> List a -> Bool
listStartsWith prefix list =
    List.take (List.length prefix) list == prefix


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


isCharRemovedOnTrim : Char -> Bool
isCharRemovedOnTrim char =
    Char.toCode char <= 32


"""
    ]
