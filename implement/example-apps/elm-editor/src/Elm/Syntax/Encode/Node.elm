module Elm.Syntax.Encode.Node exposing (encode, decoder)

{-| Represents a `Node` of the AST (Abstract Syntax Tree).

The purpose of this type is to add the information of the [`Range`](./Elm-Syntax-Range), i.e. where in the source code the
element of the tree was found.


## Types

@docs Node


## Functions

@docs empty, combine, range, value, map


## Serialization

@docs encode, decoder

-}

import Elm.Syntax.Encode.Range as EncodeRange
import Elm.Syntax.Node exposing (Node(..))
import Json.Decode as JD exposing (Decoder)
import Json.Encode as JE exposing (Value)


{-| Encode a `Node` into JSON
-}
encode : (a -> Value) -> Node a -> Value
encode f (Node r v) =
    JE.object
        [ ( "range", EncodeRange.encode r )
        , ( "value", f v )
        ]


{-| A JSON decoder for `Node`
-}
decoder : Decoder a -> Decoder (Node a)
decoder sub =
    JD.map2 Node
        (JD.field "range" EncodeRange.decoder)
        (JD.field "value" sub)
