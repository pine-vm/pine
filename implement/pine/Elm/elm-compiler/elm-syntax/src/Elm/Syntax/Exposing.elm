module Elm.Syntax.Exposing exposing
    ( Exposing(..), TopLevelExpose(..), ExposedType
    , exposesFunction, operators
    )

{-| This syntax represents the exposing declaration for both imports and module headers.
For example:

    exposing (Foo(..))
    exposing (..)


## Types

@docs Exposing, TopLevelExpose, ExposedType


## Functions

@docs exposesFunction, operators


## Serialization

@docs encode, decoder

-}

import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range as Range exposing (Range)


{-| Different kind of exposing declarations
-}
type Exposing
    = All Range
    | Explicit (List (Node TopLevelExpose))


{-| An exposed entity
-}
type TopLevelExpose
    = InfixExpose String
    | FunctionExpose String
    | TypeOrAliasExpose String
    | TypeExpose ExposedType


{-| Exposed Type
-}
type alias ExposedType =
    { name : String
    , open : Maybe Range
    }



-- Functions


{-| Check whether an import/module exposing list exposes a certain function. Will yield `True` if `Exposing` is exposing everything (`All`).

    exposesFunction "something" (All someRange) == True

    exposesFunction "divide" (Explicit [ Node someRange (FunctionExpose "add") ]) == False

    exposesFunction "add" (Explicit [ Node someRange (FunctionExpose "add") ]) == True

-}
exposesFunction : String -> Exposing -> Bool
exposesFunction s exposure =
    case exposure of
        All _ ->
            True

        Explicit l ->
            List.any
                (\(Node _ value) ->
                    case value of
                        FunctionExpose fun ->
                            fun == s

                        _ ->
                            False
                )
                l


{-| Collect all operator names from a list of TopLevelExposes
-}
operators : List TopLevelExpose -> List String
operators l =
    List.filterMap operator l


operator : TopLevelExpose -> Maybe String
operator t =
    case t of
        InfixExpose s ->
            Just s

        _ ->
            Nothing
