module Elm.Syntax.Documentation exposing (Documentation)

{-| This syntax represents documentation comments in Elm.


## Types

@docs Documentation


## Serialization

@docs encode, decoder

-}


{-| Type representing the documentation syntax
-}
type alias Documentation =
    String
