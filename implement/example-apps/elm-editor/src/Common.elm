module Common exposing (..)

import Base64
import Bytes
import Bytes.Decode
import Dict


decodeBytesToString : Bytes.Bytes -> Maybe String
decodeBytesToString bytes =
    bytes |> Bytes.Decode.decode (Bytes.Decode.string (bytes |> Bytes.width))


decodeBase64ToString : String -> Maybe String
decodeBase64ToString =
    Base64.toBytes >> Maybe.andThen decodeBytesToString


faviconPath : String
faviconPath =
    "favicon.svg"


commonPrefixLength : List a -> List a -> Int
commonPrefixLength listA listB =
    case listA of
        [] ->
            0

        a :: restA ->
            case listB of
                [] ->
                    0

                b :: restB ->
                    if a == b then
                        1 + commonPrefixLength restA restB

                    else
                        0


listFind : (a -> Bool) -> List a -> Maybe a
listFind predicate list =
    case list of
        [] ->
            Nothing

        first :: second :: rest ->
            if predicate first then
                Just first

            else if predicate second then
                Just second

            else
                listFind predicate rest

        first :: rest ->
            if predicate first then
                Just first

            else
                listFind predicate rest


assocListGet : key -> List ( key, value ) -> Maybe value
assocListGet key list =
    case list of
        [] ->
            Nothing

        ( firstKey, firstValue ) :: rest ->
            if firstKey == key then
                Just firstValue

            else
                assocListGet key rest


listMapFind : (a -> Maybe b) -> List a -> Maybe b
listMapFind mapItem list =
    case list of
        [] ->
            Nothing

        item :: rest ->
            case mapItem item of
                Just itemOk ->
                    Just itemOk

                Nothing ->
                    listMapFind mapItem rest


assocListGetWithIndex : key -> List ( key, value ) -> Maybe ( Int, value )
assocListGetWithIndex key list =
    assocListGetWithIndexHelper 0 key list


assocListGetWithIndexHelper : Int -> key -> List ( key, value ) -> Maybe ( Int, value )
assocListGetWithIndexHelper index key list =
    case list of
        [] ->
            Nothing

        ( nextKey, nextValue ) :: tail ->
            if nextKey == key then
                Just ( index, nextValue )

            else
                assocListGetWithIndexHelper (index + 1) key tail


resultListMapCombine :
    (item -> Result err itemOk)
    -> List item
    -> Result err (List itemOk)
resultListMapCombine mapItem list =
    resultListMapCombineHelper [] mapItem list


resultListMapCombineHelper :
    List itemOk
    -> (item -> Result err itemOk)
    -> List item
    -> Result err (List itemOk)
resultListMapCombineHelper completeList mapItem sourceList =
    case sourceList of
        [] ->
            Ok (List.reverse completeList)

        item :: tail ->
            case mapItem item of
                Ok itemOk ->
                    resultListMapCombineHelper
                        (itemOk :: completeList)
                        mapItem
                        tail

                Err err ->
                    Err err


resultDictMapCombine :
    (( comparable, value ) -> Result err valueOk)
    -> Dict.Dict comparable value
    -> Result err (Dict.Dict comparable valueOk)
resultDictMapCombine mapItem dict =
    Dict.foldl
        (\key value result ->
            case result of
                Ok resultDict ->
                    case mapItem ( key, value ) of
                        Ok valueOk ->
                            Ok (Dict.insert key valueOk resultDict)

                        Err err ->
                            Err err

                Err err ->
                    Err err
        )
        (Ok Dict.empty)
        dict


resultListIndexedMapCombine :
    (( Int, item ) -> Result err itemOk)
    -> List item
    -> Result err (List itemOk)
resultListIndexedMapCombine mapItem list =
    resultListIndexedMapCombineHelper 0 [] mapItem list


resultListIndexedMapCombineHelper :
    Int
    -> List itemOk
    -> (( Int, item ) -> Result err itemOk)
    -> List item
    -> Result err (List itemOk)
resultListIndexedMapCombineHelper index completeList mapItem sourceList =
    case sourceList of
        [] ->
            Ok (List.reverse completeList)

        item :: tail ->
            case mapItem ( index, item ) of
                Ok itemOk ->
                    resultListIndexedMapCombineHelper
                        (index + 1)
                        (itemOk :: completeList)
                        mapItem
                        tail

                Err err ->
                    Err err


{-| Remove duplicate values, keeping the first instance of each element which appears more than once.

    unique [ 0, 1, 1, 0, 1 ]
    --> [ 0, 1 ]

-}
listUnique : List a -> List a
listUnique list =
    listUniqueHelp list []


listUniqueHelp : List a -> List a -> List a
listUniqueHelp remaining accumulator =
    case remaining of
        [] ->
            List.reverse accumulator

        first :: rest ->
            if List.member first accumulator then
                listUniqueHelp rest accumulator

            else
                listUniqueHelp rest (first :: accumulator)


listCount : (a -> Bool) -> List a -> Int
listCount predicate list =
    case list of
        [] ->
            0

        first :: rest ->
            if predicate first then
                1 + listCount predicate rest

            else
                listCount predicate rest
