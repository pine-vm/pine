module MicroDictExtra exposing (any, increment)

import Dict exposing (Dict)


any : (k -> v -> Bool) -> Dict k v -> Bool
any pred dict =
    dict
        |> Dict.toList
        |> List.any (\( k, v ) -> pred k v)


increment : comparable -> Dict comparable Int -> Dict comparable Int
increment key dict =
    dict
        |> Dict.update key
            (\maybeValue ->
                case maybeValue of
                    Nothing ->
                        Just 1

                    Just value ->
                        Just (value + 1)
            )
