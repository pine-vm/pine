module ElmSyntax.Concrete.Exposing exposing (..)

import ElmSyntax.Concrete.Node exposing (Node)
import ElmSyntax.Concrete.Range exposing (Location, Range)
import ElmSyntax.Concrete.SeparatedSyntaxList exposing (SeparatedSyntaxList)


type Exposing
    = All Range
    | Explicit Location (SeparatedSyntaxList (Node TopLevelExpose)) Location


type TopLevelExpose
    = InfixExpose String
    | FunctionExpose String
    | TypeOrAliasExpose String
    | TypeExpose ExposedType


type alias ExposedType =
    { name : String
    , open : Maybe Range
    }
