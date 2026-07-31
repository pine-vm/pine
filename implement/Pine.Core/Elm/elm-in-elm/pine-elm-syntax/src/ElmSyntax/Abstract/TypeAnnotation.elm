module ElmSyntax.Abstract.TypeAnnotation exposing
    ( RecordDefinition
    , RecordField
    , TypeAnnotation(..)
    )


type TypeAnnotation
    = GenericType String
    | Typed (List String) String (List TypeAnnotation)
    | Unit
    | Tupled (List TypeAnnotation)
    | Record RecordDefinition
    | GenericRecord String RecordDefinition
    | FunctionTypeAnnotation TypeAnnotation TypeAnnotation


type alias RecordDefinition =
    List RecordField


type alias RecordField =
    { fieldName : String
    , fieldType : TypeAnnotation
    }
