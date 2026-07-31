module ElmSyntax.Abstract.Pattern exposing
    ( Pattern(..)
    , QualifiedNameRef
    )


type Pattern
    = AllPattern
    | VarPattern String
    | UnitPattern
    | CharPattern Int
    | StringPattern String
    | IntPattern Int
    | FloatPattern Float
    | TuplePattern (List Pattern)
    | RecordPattern (List String)
    | UnConsPattern Pattern Pattern
    | ListPattern (List Pattern)
    | NamedPattern QualifiedNameRef (List Pattern)
    | AsPattern Pattern String


type alias QualifiedNameRef =
    { moduleName : List String
    , name : String
    }
