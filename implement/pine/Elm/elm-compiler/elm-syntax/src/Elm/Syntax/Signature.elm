module Elm.Syntax.Signature exposing (Signature)

{-| This syntax represents type signatures in Elm.

For example :

    add : Int -> Int -> Int


## Types

@docs Signature


## Serialization

@docs encode, decoder

-}

import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (TypeAnnotation)


{-| Type alias representing a signature in Elm.
-}
type alias Signature =
    { name : Node String
    , typeAnnotation : Node TypeAnnotation
    }
