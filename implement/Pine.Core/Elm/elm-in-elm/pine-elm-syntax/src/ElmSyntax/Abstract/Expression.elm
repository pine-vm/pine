module ElmSyntax.Abstract.Expression exposing
    ( Case
    , Expression(..)
    , FunctionImplementation
    , FunctionStruct
    , LetDeclaration(..)
    , RecordSetter
    , Signature
    )

import ElmSyntax.Abstract.Infix exposing (InfixDirection)
import ElmSyntax.Abstract.Pattern exposing (Pattern)
import ElmSyntax.Abstract.TypeAnnotation exposing (TypeAnnotation)


type Expression
    = UnitExpr
    | StringLiteral String
    | CharLiteral Int
    | IntegerLiteral Int
    | FloatLiteral Float
    | Negation Expression
    | ListExpr (List Expression)
    | Identifier (List String) String
    | IfBlock Expression Expression Expression
    | PrefixOperator String
    | Application Expression (List Expression)
    | OperatorApplication String InfixDirection Expression Expression
    | TupledExpression (List Expression)
    | LambdaExpression (List Pattern) Expression
    | CaseExpression Expression (List Case)
    | LetExpression (List LetDeclaration) Expression
    | RecordExpr (List RecordSetter)
    | RecordAccess Expression String
    | RecordAccessFunction String
    | RecordUpdateExpression String (List RecordSetter)
    | GLSLExpression String


type alias RecordSetter =
    { fieldName : String
    , value : Expression
    }


type LetDeclaration
    = LetFunction FunctionStruct
    | LetDestructuring Pattern Expression


type alias Case =
    { pattern : Pattern
    , expression : Expression
    }


type alias FunctionStruct =
    { signature : Maybe Signature
    , declaration : FunctionImplementation
    }


type alias FunctionImplementation =
    { name : String
    , arguments : List Pattern
    , expression : Expression
    }


type alias Signature =
    { name : String
    , typeAnnotation : TypeAnnotation
    }
