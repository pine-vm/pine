module ElmSyntax.Abstract.Declaration exposing
    ( ChoiceTypeStruct
    , Declaration(..)
    , TypeAlias
    , ValueConstructor
    )

import ElmSyntax.Abstract.Expression exposing (FunctionStruct, Signature)
import ElmSyntax.Abstract.Infix exposing (Infix)
import ElmSyntax.Abstract.TypeAnnotation exposing (TypeAnnotation)


type Declaration
    = FunctionDeclaration FunctionStruct
    | ChoiceTypeDeclaration ChoiceTypeStruct
    | AliasDeclaration TypeAlias
    | PortDeclaration Signature
    | InfixDeclaration Infix


type alias TypeAlias =
    { name : String
    , generics : List String
    , typeAnnotation : TypeAnnotation
    }


type alias ChoiceTypeStruct =
    { name : String
    , generics : List String
    , constructors : List ValueConstructor
    }


type alias ValueConstructor =
    { name : String
    , arguments : List TypeAnnotation
    }
