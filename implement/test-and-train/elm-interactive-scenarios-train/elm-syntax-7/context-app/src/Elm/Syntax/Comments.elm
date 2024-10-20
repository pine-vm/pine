module Elm.Syntax.Comments exposing (Comment)

{-| This syntax represents both single and multi line comments in Elm. For example:

    -- A comment




    {- Some
       multi
       line
       comment
    -}


## Types

@docs Comment


## Serialization

@docs encode, decoder

-}


{-| Type representing the comment syntax
-}
type alias Comment =
    String
