module ElmSyntax.Abstract.Infix exposing (Infix, InfixDirection(..))


type alias Infix =
    { direction : InfixDirection
    , precedence : Int
    , operator : String
    , functionName : String
    }


type InfixDirection
    = Left
    | Right
    | Non
