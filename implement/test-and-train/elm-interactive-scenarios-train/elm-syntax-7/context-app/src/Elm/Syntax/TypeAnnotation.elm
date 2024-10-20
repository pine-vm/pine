module Elm.Syntax.TypeAnnotation exposing (TypeAnnotation(..), RecordDefinition, RecordField)

{-| This syntax represents the type annotation syntax.
For example:

    Int -> String


## Types

@docs TypeAnnotation, RecordDefinition, RecordField


## Serialization

@docs encode, decoder

-}

import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)


{-| Custom type for different type annotations. For example:

  - `GenericType`: `a`
  - `Typed`: `Maybe (Int -> String)`
  - `Unit`: `()`
  - `Tuples`: `(a, b, c)`
  - `Record`: `{ name : String}`
  - `GenericRecord`: `{ a | name : String}`
  - `FunctionTypeAnnotation`: `Int -> String`

-}
type TypeAnnotation
    = GenericType String
    | Typed (Node ( ModuleName, String )) (List (Node TypeAnnotation))
    | Unit
    | Tupled (List (Node TypeAnnotation))
    | Record RecordDefinition
    | GenericRecord (Node String) (Node RecordDefinition)
    | FunctionTypeAnnotation (Node TypeAnnotation) (Node TypeAnnotation)


{-| A list of fields in-order of a record type annotation.
-}
type alias RecordDefinition =
    List (Node RecordField)


{-| Single field of a record. A name and its type.
-}
type alias RecordField =
    ( Node String, Node TypeAnnotation )
