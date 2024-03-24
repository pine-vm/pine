module Common exposing (..)

import Dict


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


listFindWithIndex : (a -> Bool) -> List a -> Maybe ( Int, a )
listFindWithIndex predicate list =
    listFindWithIndexHelper 0 predicate list


listFindWithIndexHelper : Int -> (a -> Bool) -> List a -> Maybe ( Int, a )
listFindWithIndexHelper index predicate list =
    case list of
        [] ->
            Nothing

        a :: tail ->
            if predicate a then
                Just ( index, a )

            else
                listFindWithIndexHelper (index + 1) predicate tail


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
    (comparable -> value -> Result err valueOk)
    -> Dict.Dict comparable value
    -> Result err (Dict.Dict comparable valueOk)
resultDictMapCombine mapItem dict =
    Dict.foldl
        (\key value result ->
            case result of
                Ok resultDict ->
                    case mapItem key value of
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
    (Int -> item -> Result err itemOk)
    -> List item
    -> Result err (List itemOk)
resultListIndexedMapCombine mapItem list =
    resultListIndexedMapCombineHelper 0 [] mapItem list


resultListIndexedMapCombineHelper :
    Int
    -> List itemOk
    -> (Int -> item -> Result err itemOk)
    -> List item
    -> Result err (List itemOk)
resultListIndexedMapCombineHelper index completeList mapItem sourceList =
    case sourceList of
        [] ->
            Ok (List.reverse completeList)

        item :: tail ->
            case mapItem index item of
                Ok itemOk ->
                    resultListIndexedMapCombineHelper
                        (index + 1)
                        (itemOk :: completeList)
                        mapItem
                        tail

                Err err ->
                    Err err


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
