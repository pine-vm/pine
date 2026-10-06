module MicroListExtra exposing
    ( fastConcat
    , fastConcatMap
    , find
    , getAt
    , setAt
    , splitWhen
    , transpose
    , unique
    )


getAt : Int -> List a -> Maybe a
getAt index list =
    if index < 0 then
        Nothing

    else
        list
            |> List.drop index
            |> List.head


setAt : Int -> a -> Int -> List a -> List a
setAt index value length list =
    if length <= index || index < 0 then
        list

    else
        List.take index list
            ++ value
            :: List.drop (index + 1) list


fastConcat : List (List a) -> List a
fastConcat =
    List.foldr (++) []


fastConcatMap : (a -> List b) -> List a -> List b
fastConcatMap f =
    List.foldr (f >> (++)) []


find : (a -> Bool) -> List a -> Maybe a
find predicate list =
    case list of
        [] ->
            Nothing

        first :: rest ->
            if predicate first then
                Just first

            else
                find predicate rest


splitWhen : (a -> Bool) -> List a -> Maybe ( List a, List a )
splitWhen predicate list =
    findIndex predicate list
        |> Maybe.map (\i -> splitAt i list)


findIndex : (a -> Bool) -> List a -> Maybe Int
findIndex =
    findIndexHelp 0


findIndexHelp : Int -> (a -> Bool) -> List a -> Maybe Int
findIndexHelp index predicate list =
    case list of
        [] ->
            Nothing

        x :: xs ->
            if predicate x then
                Just index

            else
                findIndexHelp (index + 1) predicate xs


splitAt : Int -> List a -> ( List a, List a )
splitAt n xs =
    ( List.take n xs, List.drop n xs )


transpose : List (List a) -> List (List a)
transpose listOfLists =
    List.foldr (List.map2 (::)) (List.repeat (rowsLength listOfLists) []) listOfLists


rowsLength : List (List a) -> Int
rowsLength listOfLists =
    case listOfLists of
        [] ->
            0

        x :: _ ->
            List.length x


unique : List a -> List a
unique list =
    uniqueHelp identity [] list []


uniqueHelp : (a -> b) -> List b -> List a -> List a -> List a
uniqueHelp f existing remaining accumulator =
    case remaining of
        [] ->
            List.reverse accumulator

        first :: rest ->
            let
                computedFirst =
                    f first
            in
            if List.member computedFirst existing then
                uniqueHelp f existing rest accumulator

            else
                uniqueHelp f (computedFirst :: existing) rest (first :: accumulator)
