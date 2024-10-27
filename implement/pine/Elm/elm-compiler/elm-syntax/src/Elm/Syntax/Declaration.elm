module Elm.Syntax.Declaration exposing (Declaration(..))

{-| Syntax for the different top-level declarations in Elm.
These can be one of the following (all declared in `Declaration`):

  - Functions: `add x y = x + y`
  - Custom types: `type Color = Blue | Red`
  - Type aliases: `type alias Status = Int`
  - Port declaration: `port sendMessage: String -> Cmd msg`
  - Destructuring: `{name, age} = person`
  - Infix declarations. You will probably not need this, while only core packages can define these.


## Types

@docs Declaration


## Serialization

@docs encode, decoder

-}

import Elm.Syntax.Expression as Expression exposing (Expression, Function)
import Elm.Syntax.Infix as Infix exposing (Infix)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Signature as Signature exposing (Signature)
import Elm.Syntax.Type as Type exposing (Type)
import Elm.Syntax.TypeAlias as TypeAlias exposing (TypeAlias)


{-| Custom type that represents all different top-level declarations.
-}
type Declaration
    = FunctionDeclaration Function
    | AliasDeclaration TypeAlias
    | CustomTypeDeclaration Type
    | PortDeclaration Signature
    | InfixDeclaration Infix
    | Destructuring (Node Pattern) (Node Expression)
