module Elm.Syntax.Node exposing
    ( Node(..)
    , empty, combine, range, value, map
    )

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

import Elm.Syntax.Range as Range exposing (Range)


{-| Base representation for a syntax node in a source file.
-}
type Node a
    = Node Range a


{-| Create a Node with an empty range.
-}
empty : a -> Node a
empty a =
    Node Range.empty a


{-| Combine two nodes, constructing a new node which will have the outer most range of the child nodes
-}
combine : (Node a -> Node b -> c) -> Node a -> Node b -> Node c
combine f ((Node { start } _) as a) ((Node { end } _) as b) =
    Node
        { start = start, end = end }
        (f a b)


{-| Map the value within a node leaving the range untouched
-}
map : (a -> b) -> Node a -> Node b
map f (Node r a) =
    Node r (f a)


{-| Extract the range out of a `Node a`
-}
range : Node a -> Range
range (Node r _) =
    r


{-| Extract the value (`a`) out of a `Node a`
-}
value : Node a -> a
value (Node _ v) =
    v
