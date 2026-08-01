module ElmSyntax.Concrete.Parser.DeclarationOrExpression exposing (..)

import ElmSyntax.Concrete.Declaration as Declaration
import ElmSyntax.Concrete.Expression as Expression


type DeclarationOrExpression
    = Declaration Declaration.Declaration
    | Expression Expression.Expression
