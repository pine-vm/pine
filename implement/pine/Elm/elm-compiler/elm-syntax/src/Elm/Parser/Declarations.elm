module Elm.Parser.Declarations exposing (declaration)

import Combine exposing (Parser)
import Elm.Parser.Expression
import Elm.Parser.Layout as Layout
import Elm.Parser.Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens
import Elm.Parser.TypeAnnotation
import Elm.Parser.Typings
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Infix as Infix exposing (Infix)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Signature exposing (Signature)
import Parser as Core


declaration : Parser State (Node Declaration)
declaration =
    Combine.oneOf
        [ infixDeclaration
        , Elm.Parser.Expression.function
        , Elm.Parser.Typings.typeDefinition
        , portDeclaration
        ]


signature : Parser State Signature
signature =
    Combine.succeed Signature
        |> Combine.keep (Elm.Parser.Node.parser Elm.Parser.Tokens.functionName)
        |> Combine.ignore (Layout.maybeAroundBothSides (Combine.string ":"))
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep Elm.Parser.TypeAnnotation.typeAnnotation


infixDeclaration : Parser State (Node Declaration)
infixDeclaration =
    Combine.succeed Infix
        |> Combine.ignore (Combine.fromCore (Core.keyword "infix"))
        |> Combine.ignore Layout.layout
        |> Combine.keep (Elm.Parser.Node.parser infixDirection)
        |> Combine.ignore Layout.layout
        |> Combine.keep (Elm.Parser.Node.parser (Combine.fromCore Core.int))
        |> Combine.ignore Layout.layout
        |> Combine.keep (Elm.Parser.Node.parser <| Combine.parens Elm.Parser.Tokens.prefixOperatorToken)
        |> Combine.ignore Layout.layout
        |> Combine.ignore (Combine.string "=")
        |> Combine.ignore Layout.layout
        |> Combine.keep (Elm.Parser.Node.parser Elm.Parser.Tokens.functionName)
        |> Combine.map Declaration.InfixDeclaration
        |> Elm.Parser.Node.parser


infixDirection : Parser State Infix.InfixDirection
infixDirection =
    Core.oneOf
        [ Core.keyword "right"
            |> Core.map (\_ -> Infix.Right)
        , Core.keyword "left"
            |> Core.map (\_ -> Infix.Left)
        , Core.keyword "non"
            |> Core.map (\_ -> Infix.Non)
        ]
        |> Combine.fromCore


portDeclaration : Parser State (Node Declaration)
portDeclaration =
    Combine.succeed
        (\(Node { start } _) sig ->
            Node
                { start = start, end = (Node.range sig.typeAnnotation).end }
                (Declaration.PortDeclaration sig)
        )
        |> Combine.keep (Elm.Parser.Node.parser Elm.Parser.Tokens.portToken)
        |> Combine.ignore Layout.layout
        |> Combine.keep signature
