module Common exposing (..)


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
