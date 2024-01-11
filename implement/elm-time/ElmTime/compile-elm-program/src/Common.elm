module Common exposing (..)


listFindIndexedMap : (Int -> a -> Maybe b) -> List a -> Maybe b
listFindIndexedMap f list =
    listFindIndexedMapHelper 0 f list


listFindIndexedMapHelper : Int -> (Int -> a -> Maybe b) -> List a -> Maybe b
listFindIndexedMapHelper index f list =
    case list of
        [] ->
            Nothing

        a :: tail ->
            case f index a of
                Just b ->
                    Just b

                Nothing ->
                    listFindIndexedMapHelper (index + 1) f tail
