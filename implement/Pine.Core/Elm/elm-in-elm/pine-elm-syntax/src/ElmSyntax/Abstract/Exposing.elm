module ElmSyntax.Abstract.Exposing exposing
    ( ExposedType
    , Exposing(..)
    , TopLevelExpose(..)
    )


type Exposing
    = All
    | Explicit (List TopLevelExpose)


type TopLevelExpose
    = InfixExpose String
    | FunctionExpose String
    | TypeOrAliasExpose String
    | TypeExpose ExposedType


type alias ExposedType =
    { name : String
    , exposesConstructors : Bool
    }
