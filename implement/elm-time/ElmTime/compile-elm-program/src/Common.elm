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
