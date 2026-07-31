module ElmSyntax.Concrete.Infix exposing (Infix, InfixDirection(..))

{-|


## Types

@docs Infix, InfixDirection


## Serialization

@docs encode, encodeDirection, decoder, decodeDirection

-}

import ElmSyntax.Concrete.Node exposing (Node)


{-| Type annotation for a infix definition
-}
type alias Infix =
    { direction : Node InfixDirection
    , precedence : Node Int
    , operator : Node String
    , function : Node String
    }


{-| Union type for infix direction
-}
type InfixDirection
    = Left
    | Right
    | Non
