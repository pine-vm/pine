module Elm.Syntax.Pattern exposing
    ( Pattern(..), QualifiedNameRef
    , moduleNames
    )

{-| This syntax represents the patterns.
For example:

    Just x as someMaybe
    {name, age}


## Types

@docs Pattern, QualifiedNameRef


## Functions

@docs moduleNames


## Serialization

@docs encode, decoder

-}

import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))


{-| Custom type for all patterns such as:

  - `AllPattern`: `_`
  - `UnitPattern`: `()`
  - `CharPattern`: `'c'`
  - `StringPattern`: `"hello"`
  - `IntPattern`: `42`
  - `HexPattern`: `0x11`
  - `FloatPattern`: `42.0`
  - `TuplePattern`: `(a, b)`
  - `RecordPattern`: `{name, age}`
  - `UnConsPattern`: `x :: xs`
  - `ListPattern`: `[ x, y ]`
  - `VarPattern`: `x`
  - `NamedPattern`: `Just _`
  - `AsPattern`: `_ as x`
  - `ParenthesizedPattern`: `( _ )`

-}
type Pattern
    = AllPattern
    | UnitPattern
    | CharPattern Char
    | StringPattern String
    | IntPattern Int
    | HexPattern Int
    | FloatPattern Float
    | TuplePattern (List (Node Pattern))
    | RecordPattern (List (Node String))
    | UnConsPattern (Node Pattern) (Node Pattern)
    | ListPattern (List (Node Pattern))
    | VarPattern String
    | NamedPattern QualifiedNameRef (List (Node Pattern))
    | AsPattern (Node Pattern) (Node String)
    | ParenthesizedPattern (Node Pattern)


{-| Qualified name reference such as `Maybe.Just`.
-}
type alias QualifiedNameRef =
    { moduleName : List String
    , name : String
    }


{-| Get all the modules names that are used in the pattern (and its nested patterns).
Use this to collect qualified patterns, such as `Maybe.Just x`.
-}
moduleNames : Pattern -> List ModuleName
moduleNames p =
    -- TODO Remove this function or make it take a `Node Pattern`
    -- Should it return a `Set`?
    case p of
        TuplePattern xs ->
            List.concatMap (\(Node _ x) -> moduleNames x) xs

        RecordPattern _ ->
            []

        UnConsPattern (Node _ left) (Node _ right) ->
            moduleNames left ++ moduleNames right

        ListPattern xs ->
            List.concatMap (\(Node _ x) -> moduleNames x) xs

        NamedPattern qualifiedNameRef subPatterns ->
            qualifiedNameRef.moduleName :: List.concatMap (\(Node _ x) -> moduleNames x) subPatterns

        AsPattern (Node _ inner) _ ->
            moduleNames inner

        ParenthesizedPattern (Node _ inner) ->
            moduleNames inner

        _ ->
            []
