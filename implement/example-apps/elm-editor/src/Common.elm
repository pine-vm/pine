module Common exposing (..)

import Base64
import Bytes
import Bytes.Decode


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
