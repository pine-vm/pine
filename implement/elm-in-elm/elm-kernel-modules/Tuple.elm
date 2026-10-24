module Tuple exposing
    ( first
    , mapBoth
    , mapFirst
    , mapSecond
    , pair
    , second
    )


pair : a -> b -> ( a, b )
pair a b =
    ( a, b )


first : ( a, b ) -> a
first ( x, _ ) =
    x


second : ( a, b ) -> b
second ( _, y ) =
    y


mapFirst : (a -> x) -> ( a, b ) -> ( x, b )
mapFirst func ( x, y ) =
    ( func x, y )


mapSecond : (b -> y) -> ( a, b ) -> ( a, y )
mapSecond func ( x, y ) =
    ( x, func y )


mapBoth : (a -> x) -> (b -> y) -> ( a, b ) -> ( x, y )
mapBoth funcA funcB ( x, y ) =
    ( funcA x, funcB y )
