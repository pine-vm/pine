module ElmSyntax.Concrete.TypeAnnotation exposing (..)

import ElmSyntax.Concrete.Node exposing (Node)
import ElmSyntax.Concrete.Range exposing (Location)
import ElmSyntax.Concrete.SeparatedSyntaxList exposing (SeparatedSyntaxList)


type TypeAnnotation
    = GenericType String
    | Typed (Node ( List String, String )) (List (Node TypeAnnotation))
    | Unit
    | Tupled (SeparatedSyntaxList (Node TypeAnnotation))
    | Record RecordDefinition
    | GenericRecord (Node String) Location (Node RecordDefinition)
    | FunctionTypeAnnotation (Node TypeAnnotation) Location (Node TypeAnnotation)


type alias RecordDefinition =
    SeparatedSyntaxList (Node RecordField)


type alias RecordField =
    { fieldName : Node String
    , colonLocation : Location
    , fieldType : Node TypeAnnotation
    }
