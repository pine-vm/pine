module Elm.Syntax.Expression exposing
    ( Expression(..), Lambda, LetBlock, LetDeclaration(..), RecordSetter, CaseBlock, Cases, Case, Function, FunctionImplementation
    , functionRange, isLambda, isLet, isIfElse, isCase, isOperatorApplication
    )

{-| This syntax represents all that you can express in Elm.
Although it is a easy and simple language, you can express a lot! See the `Expression` type for all the things you can express.


## Types

@docs Expression, Lambda, LetBlock, LetDeclaration, RecordSetter, CaseBlock, Cases, Case, Function, FunctionImplementation


## Functions

@docs functionRange, isLambda, isLet, isIfElse, isCase, isOperatorApplication


## Serialization

@docs encode, encodeFunction, decoder, functionDecoder

-}

import Elm.Syntax.Documentation as Documentation exposing (Documentation)
import Elm.Syntax.Infix as Infix exposing (InfixDirection)
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.Signature as Signature exposing (Signature)


{-| Type alias for a full function
-}
type alias Function =
    { documentation : Maybe (Node Documentation)
    , signature : Maybe (Node Signature)
    , declaration : Node FunctionImplementation
    }


{-| Get the full range of a function
-}
functionRange : Function -> Range
functionRange function =
    let
        { name, expression } =
            Node.value function.declaration

        startRange : Range
        startRange =
            case function.documentation of
                Just documentation ->
                    Node.range documentation

                Nothing ->
                    case function.signature of
                        Just (Node _ value) ->
                            Node.range value.name

                        Nothing ->
                            Node.range name
    in
    { start = startRange.start
    , end = (Node.range expression).end
    }


{-| Type alias for a function's implementation
-}
type alias FunctionImplementation =
    { name : Node String
    , arguments : List (Node Pattern)
    , expression : Node Expression
    }


{-| Custom type for all expressions such as:

  - `Unit`: `()`
  - `Application`: `add a b`
  - `OperatorApplication`: `a + b`
  - `FunctionOrValue`: `add` or `True`
  - `IfBlock`: `if a then b else c`
  - `PrefixOperator`: `(+)`
  - `Operator`: `+` (not possible to get in practice)
  - `Integer`: `42`
  - `Hex`: `0x1F`
  - `Floatable`: `42.0`
  - `Negation`: `-a`
  - `Literal`: `"text"`
  - `CharLiteral`: `'a'`
  - `TupledExpression`: `(a, b)` or `(a, b, c)`
  - `ParenthesizedExpression`: `(a)`
  - `LetExpression`: `let a = 4 in a`
  - `CaseExpression`: `case a of` followed by pattern matches
  - `LambdaExpression`: `(\a -> a)`
  - `RecordExpr`: `{ name = "text" }`
  - `ListExpr`: `[ x, y ]`
  - `RecordAccess`: `a.name`
  - `RecordAccessFunction`: `.name`
  - `RecordUpdateExpression`: `{ a | name = "text" }`
  - `GLSLExpression`: `[glsl| ... |]`

-}
type Expression
    = UnitExpr
    | Application (List (Node Expression))
    | OperatorApplication String InfixDirection (Node Expression) (Node Expression)
    | FunctionOrValue ModuleName String
    | IfBlock (Node Expression) (Node Expression) (Node Expression)
    | PrefixOperator String
    | Operator String
    | Integer Int
    | Hex Int
    | Floatable Float
    | Negation (Node Expression)
    | Literal String
    | CharLiteral Char
    | TupledExpression (List (Node Expression))
    | ParenthesizedExpression (Node Expression)
    | LetExpression LetBlock
    | CaseExpression CaseBlock
    | LambdaExpression Lambda
    | RecordExpr (List (Node RecordSetter))
    | ListExpr (List (Node Expression))
    | RecordAccess (Node Expression) (Node String)
    | RecordAccessFunction String
    | RecordUpdateExpression (Node String) (List (Node RecordSetter))
    | GLSLExpression String


{-| Expression for setting a record field
-}
type alias RecordSetter =
    ( Node String, Node Expression )


{-| Expression for a let block
-}
type alias LetBlock =
    { declarations : List (Node LetDeclaration)
    , expression : Node Expression
    }


{-| Union type for all possible declarations in a let block
-}
type LetDeclaration
    = LetFunction Function
    | LetDestructuring (Node Pattern) (Node Expression)


{-| Expression for a lambda
-}
type alias Lambda =
    { args : List (Node Pattern)
    , expression : Node Expression
    }


{-| Expression for a case block
-}
type alias CaseBlock =
    { expression : Node Expression
    , cases : Cases
    }


{-| A case in a case block
-}
type alias Case =
    ( Node Pattern, Node Expression )


{-| Type alias for a list of cases
-}
type alias Cases =
    List Case


{-| Check whether an expression is a lambda-expression
-}
isLambda : Expression -> Bool
isLambda e =
    case e of
        LambdaExpression _ ->
            True

        _ ->
            False


{-| Check whether an expression is a let-expression
-}
isLet : Expression -> Bool
isLet e =
    case e of
        LetExpression _ ->
            True

        _ ->
            False


{-| Check whether an expression is an if-else-expression
-}
isIfElse : Expression -> Bool
isIfElse e =
    case e of
        IfBlock _ _ _ ->
            True

        _ ->
            False


{-| Check whether an expression is a case-expression
-}
isCase : Expression -> Bool
isCase e =
    case e of
        CaseExpression _ ->
            True

        _ ->
            False


{-| Check whether an expression is an operator application expression
-}
isOperatorApplication : Expression -> Bool
isOperatorApplication e =
    case e of
        OperatorApplication _ _ _ _ ->
            True

        _ ->
            False
