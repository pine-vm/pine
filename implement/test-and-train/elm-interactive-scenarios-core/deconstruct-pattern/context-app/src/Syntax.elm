module Syntax exposing (..)

{-| Some types from <https://github.com/Viir/elm-syntax/tree/f7d9be0a1f346b22dfaa7b55679659874c72714b/src/Elm/Syntax>
-}


{-| Base representation for a syntax node in a source file.
-}
type Node a
    = Node Range a


{-| Source location
-}
type alias Location =
    { row : Int
    , column : Int
    }


{-| Range for a piece of code with a start and end
-}
type alias Range =
    { start : Location
    , end : Location
    }
