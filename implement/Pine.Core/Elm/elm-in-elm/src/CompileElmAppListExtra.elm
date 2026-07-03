module CompileElmAppListExtra exposing
    ( isPrefixOf
    , isSuffixOf
    , remove
    , uniqueBy
    , zip
    )

{-| Take two lists and returns a list of corresponding pairs
-}


zip : List a -> List b -> List ( a, b )
zip =
    List.map2 Tuple.pair


{-| Remove the first occurrence of a value from a list.
-}
remove : a -> List a -> List a
remove x xs =
    removeHelp xs x xs []


removeHelp : List a -> a -> List a -> List a -> List a
removeHelp list x xs previousElements =
    case xs of
        [] ->
            list

        y :: ys ->
            if x == y then
                reverseAppend previousElements ys

            else
                removeHelp list x ys (y :: previousElements)


reverseAppend : List a -> List a -> List a
reverseAppend list1 list2 =
    List.foldl (::) list2 list1


{-| Take two lists and return `True`, if the first list is the prefix of the second list.
-}
isPrefixOf : List a -> List a -> Bool
isPrefixOf prefix list =
    let
        sublist =
            List.take (List.length prefix) list
    in
    sublist == prefix


{-| Take two lists and return `True`, if the first list is the suffix of the second list.
-}
isSuffixOf : List a -> List a -> Bool
isSuffixOf suffix xs =
    let
        sublist =
            List.drop (List.length xs - List.length suffix) xs
    in
    sublist == suffix


{-| Drop duplicates where what is considered to be a duplicate is the result of first applying the supplied function to the elements of the list.
-}
uniqueBy : (a -> b) -> List a -> List a
uniqueBy f list =
    uniqueByHelp f [] list []


uniqueByHelp : (a -> b) -> List b -> List a -> List a -> List a
uniqueByHelp f existing remaining accumulator =
    case remaining of
        [] ->
            List.reverse accumulator

        first :: rest ->
            let
                computedFirst =
                    f first
            in
            if List.member computedFirst existing then
                uniqueByHelp f existing rest accumulator

            else
                uniqueByHelp f (computedFirst :: existing) rest (first :: accumulator)
