module Elm.Parser.Expression exposing (expression, function)

import Combine exposing (Parser)
import Elm.Parser.Layout as Layout
import Elm.Parser.Node
import Elm.Parser.Numbers
import Elm.Parser.Patterns
import Elm.Parser.State as State exposing (State)
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation
import Elm.Parser.Whitespace
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression
    exposing
        ( Case
        , CaseBlock
        , Cases
        , Expression(..)
        , Function
        , FunctionImplementation
        , Lambda
        , LetBlock
        , LetDeclaration(..)
        , RecordSetter
        )
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Signature exposing (Signature)
import Parser as Core exposing (Nestable(..))


expressionNotApplication : Parser State (Node Expression)
expressionNotApplication =
    Combine.lazy
        (\() ->
            Combine.oneOf
                [ numberExpression
                , referenceExpression
                , ifBlockExpression
                , tupledExpression
                , recordAccessFunctionExpression
                , operatorExpression
                , letExpression
                , lambdaExpression
                , literalExpression
                , charLiteralExpression
                , recordExpression
                , glslExpression
                , listExpression
                , caseExpression
                ]
                |> Combine.andThen liftRecordAccess
        )


liftRecordAccess : Node Expression -> Parser State (Node Expression)
liftRecordAccess e =
    Combine.oneOf
        [ Combine.string "."
            |> Combine.continueWith (Elm.Parser.Node.parser Tokens.functionName)
            |> Combine.andThen
                (\f ->
                    liftRecordAccess
                        (Node
                            { start = (Node.range e).start, end = (Node.range f).end }
                            (RecordAccess e f)
                        )
                )
        , Combine.succeed e
        ]


expression : Parser State (Node Expression)
expression =
    expressionNotApplication
        |> Combine.andThen
            (\first ->
                let
                    complete : Range -> List (Node Expression) -> Parser s (Node Expression)
                    complete lastExpressionRange rest =
                        case rest of
                            [] ->
                                Combine.succeed first

                            (Node _ (Operator _)) :: _ ->
                                Combine.fail "Expression should not end with an operator"

                            _ ->
                                Combine.succeed
                                    (Node
                                        { start = (Node.range first).start, end = lastExpressionRange.end }
                                        (Application (first :: List.reverse rest))
                                    )

                    promoter : Range -> List (Node Expression) -> Parser State (Node Expression)
                    promoter lastExpressionRange rest =
                        Layout.optimisticLayoutWith
                            (\() -> complete lastExpressionRange rest)
                            (\() ->
                                Combine.oneOf
                                    [ expressionNotApplication
                                        |> Combine.andThen (\next -> promoter (Node.range next) (next :: rest))
                                    , Combine.succeed ()
                                        |> Combine.andThen (\() -> complete lastExpressionRange rest)
                                    ]
                            )
                in
                case first of
                    Node _ (Operator _) ->
                        Combine.fail "Expression should not start with an operator"

                    _ ->
                        promoter (Node.range first) []
            )


glslExpression : Parser State (Node Expression)
glslExpression =
    let
        start : String
        start =
            "[glsl|"

        end : String
        end =
            "|]"
    in
    Core.getChompedString (Core.multiComment start end NotNestable)
        |> Combine.fromCore
        |> Combine.map (String.dropLeft (String.length start) >> GLSLExpression)
        |> Combine.ignore (Combine.string end)
        |> Elm.Parser.Node.parser


listExpression : Parser State (Node Expression)
listExpression =
    Combine.succeed ListExpr
        |> Combine.ignore (Combine.string "[")
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep
            (Combine.sepBy
                (Combine.string "," |> Combine.ignore (Combine.maybe Layout.layout))
                expression
            )
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.ignore (Combine.string "]")
        |> Elm.Parser.Node.parser



-- recordExpression


recordExpression : Parser State (Node Expression)
recordExpression =
    Combine.string "{"
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.continueWith
            (Combine.oneOf
                [ Combine.string "}" |> Combine.map (always (RecordExpr []))
                , recordContents
                ]
            )
        |> Elm.Parser.Node.parser


recordContents : Parser State Expression
recordContents =
    Elm.Parser.Node.parser Tokens.functionName
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.andThen
            (\fname ->
                Combine.oneOf
                    [ recordUpdateSyntaxParser fname
                    , Combine.string "="
                        |> Combine.ignore (Combine.maybe Layout.layout)
                        |> Combine.continueWith expression
                        |> Combine.ignore (Combine.maybe Layout.layout)
                        |> Combine.andThen
                            (\e ->
                                let
                                    fieldUpdate : Node RecordSetter
                                    fieldUpdate =
                                        Node.combine Tuple.pair fname e
                                in
                                Combine.oneOf
                                    [ Combine.string "}"
                                        |> Combine.map (always (RecordExpr [ fieldUpdate ]))
                                    , Combine.succeed (\fieldUpdates -> RecordExpr (fieldUpdate :: fieldUpdates))
                                        |> Combine.ignore (Combine.string ",")
                                        |> Combine.ignore (Combine.maybe Layout.layout)
                                        |> Combine.keep recordFields
                                        |> Combine.ignore (Combine.string "}")
                                    ]
                            )
                    ]
            )


recordUpdateSyntaxParser : Node String -> Parser State Expression
recordUpdateSyntaxParser fname =
    Combine.succeed (\e -> RecordUpdateExpression fname e)
        |> Combine.ignore (Combine.string "|")
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep recordFields
        |> Combine.ignore (Combine.string "}")


recordFields : Parser State (List (Node RecordSetter))
recordFields =
    Combine.succeed (::)
        |> Combine.keep recordField
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep
            (Combine.many
                (Combine.string ","
                    |> Combine.ignore (Combine.maybe Layout.layout)
                    |> Combine.continueWith recordField
                    |> Combine.ignore (Combine.maybe Layout.layout)
                )
            )


recordField : Parser State (Node RecordSetter)
recordField =
    Elm.Parser.Node.parser
        (Combine.succeed Tuple.pair
            |> Combine.keep (Elm.Parser.Node.parser Tokens.functionName)
            |> Combine.ignore (Combine.maybe Layout.layout)
            |> Combine.ignore (Combine.string "=")
            |> Combine.ignore (Combine.maybe Layout.layout)
            |> Combine.keep expression
        )


literalExpression : Parser State (Node Expression)
literalExpression =
    Combine.oneOf
        [ Tokens.multiLineStringLiteral
        , Tokens.stringLiteral
        ]
        |> Combine.map Literal
        |> Elm.Parser.Node.parser


charLiteralExpression : Parser State (Node Expression)
charLiteralExpression =
    Elm.Parser.Node.parser (Combine.map CharLiteral Tokens.characterLiteral)



-- lambda


lambdaExpression : Parser State (Node Expression)
lambdaExpression =
    Combine.succeed
        (\(Node { start } _) args expr ->
            Lambda args expr
                |> LambdaExpression
                |> Node { start = start, end = (Node.range expr).end }
        )
        |> Combine.keep (Elm.Parser.Node.parser (Combine.string "\\"))
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep (Combine.sepBy1 (Combine.maybe Layout.layout) Elm.Parser.Patterns.pattern)
        |> Combine.ignore (Layout.maybeAroundBothSides (Combine.string "->"))
        |> Combine.keep expression



-- Case Expression


caseStatement : Parser State Case
caseStatement =
    Combine.succeed Tuple.pair
        |> Combine.keep Elm.Parser.Patterns.pattern
        |> Combine.ignore (Combine.maybe (Combine.oneOf [ Layout.layout, Layout.layoutStrict ]))
        |> Combine.ignore (Combine.string "->")
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep expression


caseStatements : Parser State ( Location, Cases )
caseStatements =
    Combine.many1WithEndLocationForLastElement
        (\( _, case_ ) -> Node.range case_)
        caseStatementWithCorrectIndentation


caseStatementWithCorrectIndentation : Parser State Case
caseStatementWithCorrectIndentation =
    Combine.withState
        (\s ->
            Combine.withLocation
                (\l ->
                    if State.expectedColumn s == l.column then
                        caseStatement

                    else
                        Combine.fail "Indentation is incorrect to be a case statement"
                )
        )


caseExpression : Parser State (Node Expression)
caseExpression =
    Combine.succeed
        (\caseKeyword caseBlock_ ( end, cases ) ->
            Node { start = (Node.range caseKeyword).start, end = end }
                (CaseExpression (CaseBlock caseBlock_ cases))
        )
        |> Combine.keep (Elm.Parser.Node.parser Tokens.caseToken)
        |> Combine.ignore Layout.layout
        |> Combine.keep expression
        |> Combine.ignore Tokens.ofToken
        |> Combine.ignore Layout.layout
        |> Combine.keep (withIndentedState caseStatements)



-- Let Expression


letBody : Parser State (List (Node LetDeclaration))
letBody =
    Combine.succeed (::)
        |> Combine.keep blockElement
        |> Combine.keep
            (Combine.many
                (blockElement
                    |> Combine.ignore (Combine.maybe Layout.layout)
                )
            )


blockElement : Parser State (Node LetDeclaration)
blockElement =
    Elm.Parser.Patterns.pattern
        |> Combine.andThen
            (\(Node r p) ->
                case p of
                    Pattern.VarPattern v ->
                        functionWithNameNode (Node r v)
                            |> Combine.map (\fn -> Node (Expression.functionRange fn) (LetFunction fn))

                    _ ->
                        letDestructuringDeclarationWithPattern (Node r p)
            )


letDestructuringDeclarationWithPattern : Node Pattern -> Parser State (Node LetDeclaration)
letDestructuringDeclarationWithPattern pattern =
    Combine.succeed
        (\expr ->
            Node { start = (Node.range pattern).start, end = (Node.range expr).end } (LetDestructuring pattern expr)
        )
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.ignore (Combine.string "=")
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep expression


letExpression : Parser State (Node Expression)
letExpression =
    Combine.succeed
        (\(Node { start } _) decls expr ->
            Node { start = start, end = (Node.range expr).end } (LetBlock decls expr |> LetExpression)
        )
        |> Combine.keep (Elm.Parser.Node.parser (Combine.string "let"))
        |> Combine.ignore Layout.layout
        |> Combine.keep (withIndentedState letBody)
        |> Combine.ignore (Combine.oneOf [ Layout.layout, Elm.Parser.Whitespace.manySpaces ])
        |> Combine.ignore (Combine.string "in")
        |> Combine.ignore Layout.layout
        |> Combine.keep expression


numberExpression : Parser State (Node Expression)
numberExpression =
    Elm.Parser.Node.parser (Elm.Parser.Numbers.forgivingNumber Floatable Integer Hex)


ifBlockExpression : Parser State (Node Expression)
ifBlockExpression =
    Combine.succeed
        (\(Node { start } _) condition ifTrue ifFalse ->
            Node
                { start = start, end = (Node.range ifFalse).end }
                (IfBlock condition ifTrue ifFalse)
        )
        |> Combine.keep (Elm.Parser.Node.parser Tokens.ifToken)
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep expression
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.ignore Tokens.thenToken
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep expression
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.ignore Tokens.elseToken
        |> Combine.ignore Layout.layout
        |> Combine.keep expression


operatorExpression : Parser State (Node Expression)
operatorExpression =
    let
        negationExpression : Parser State Expression
        negationExpression =
            Combine.map Negation
                (Combine.oneOf
                    [ referenceExpression
                    , numberExpression
                    , tupledExpression
                    ]
                    |> Combine.andThen liftRecordAccess
                )
    in
    Combine.oneOf
        [ Combine.string "-"
            |> Combine.continueWith
                (Combine.oneOf
                    [ negationExpression
                    , Combine.succeed (Operator "-") |> Combine.ignore Layout.layout
                    ]
                )
            |> Elm.Parser.Node.parser
        , Combine.map Operator Tokens.infixOperatorToken
            |> Elm.Parser.Node.parser
        ]


referenceExpression : Parser State (Node Expression)
referenceExpression =
    let
        helper : ModuleName -> String -> Parser s Expression
        helper moduleNameSoFar nameOrSegment =
            Combine.oneOf
                [ Combine.string "."
                    |> Combine.continueWith
                        (Combine.oneOf
                            [ Tokens.typeName
                                |> Combine.andThen (\t -> helper (nameOrSegment :: moduleNameSoFar) t)
                            , Tokens.functionName
                                |> Combine.map
                                    (\name ->
                                        FunctionOrValue
                                            (List.reverse (nameOrSegment :: moduleNameSoFar))
                                            name
                                    )
                            ]
                        )
                , Combine.succeed ()
                    |> Combine.map (\() -> FunctionOrValue (List.reverse moduleNameSoFar) nameOrSegment)
                ]
    in
    Combine.oneOf
        [ Tokens.typeName
            |> Combine.andThen (\t -> helper [] t)
        , Tokens.functionName
            |> Combine.map (\v -> FunctionOrValue [] v)
        ]
        |> Elm.Parser.Node.parser


recordAccessFunctionExpression : Parser State (Node Expression)
recordAccessFunctionExpression =
    Combine.succeed (\field -> RecordAccessFunction ("." ++ field))
        |> Combine.ignore (Combine.string ".")
        |> Combine.keep Tokens.functionName
        |> Elm.Parser.Node.parser


tupledExpression : Parser State (Node Expression)
tupledExpression =
    let
        asExpression : Node Expression -> List (Node Expression) -> Expression
        asExpression x xs =
            case xs of
                [] ->
                    ParenthesizedExpression x

                _ ->
                    TupledExpression (x :: xs)

        commaSep : Parser State (List (Node Expression))
        commaSep =
            Combine.many
                (Combine.string ","
                    |> Combine.ignore (Combine.maybe Layout.layout)
                    |> Combine.continueWith expression
                    |> Combine.ignore (Combine.maybe Layout.layout)
                )

        nested : Parser State Expression
        nested =
            Combine.succeed asExpression
                |> Combine.ignore (Combine.maybe Layout.layout)
                |> Combine.keep expression
                |> Combine.ignore (Combine.maybe Layout.layout)
                |> Combine.keep commaSep

        closingParen : Parser state ()
        closingParen =
            Combine.fromCore (Core.symbol ")")
    in
    Combine.fromCore (Core.symbol "(")
        |> Combine.continueWith
            (Combine.oneOf
                [ closingParen |> Combine.map (always UnitExpr)
                , -- Backtracking needed for record access expression
                  Combine.backtrackable
                    (Tokens.prefixOperatorToken
                        |> Combine.ignore closingParen
                        |> Combine.map PrefixOperator
                    )
                , nested |> Combine.ignore closingParen
                ]
            )
        |> Elm.Parser.Node.parser


withIndentedState : Parser State a -> Parser State a
withIndentedState p =
    Combine.withLocation
        (\location ->
            Combine.modifyState (State.pushColumn location.column)
                |> Combine.continueWith p
                |> Combine.ignore (Combine.modifyState State.popIndent)
        )


functionWithNameNode : Node String -> Parser State Function
functionWithNameNode pointer =
    let
        functionImplementationFromVarPointer : Node String -> Parser State (Node FunctionImplementation)
        functionImplementationFromVarPointer varPointer =
            Combine.succeed
                (\args expr ->
                    Node
                        { start = (Node.range varPointer).start
                        , end = (Node.range expr).end
                        }
                        (FunctionImplementation varPointer args expr)
                )
                |> Combine.keep (Combine.many (Elm.Parser.Patterns.pattern |> Combine.ignore (Combine.maybe Layout.layout)))
                |> Combine.ignore (Combine.string "=")
                |> Combine.ignore (Combine.maybe Layout.layout)
                |> Combine.keep expression

        fromParts : Node Signature -> Node FunctionImplementation -> Function
        fromParts sig decl =
            { documentation = Nothing
            , signature = Just sig
            , declaration = decl
            }

        functionWithSignature : Node String -> Parser State Function
        functionWithSignature varPointer =
            functionSignatureFromVarPointer varPointer
                |> Combine.ignore (Combine.maybe Layout.layoutStrict)
                |> Combine.andThen
                    (\sig ->
                        Elm.Parser.Node.parser Tokens.functionName
                            |> Combine.andThen (failIfDifferentFrom varPointer)
                            |> Combine.ignore (Combine.maybe Layout.layout)
                            |> Combine.andThen functionImplementationFromVarPointer
                            |> Combine.map (fromParts sig)
                    )

        functionWithoutSignature : Node String -> Parser State Function
        functionWithoutSignature varPointer =
            functionImplementationFromVarPointer varPointer
                |> Combine.map (Function Nothing Nothing)
    in
    Combine.oneOf
        [ functionWithSignature pointer
        , functionWithoutSignature pointer
        ]


failIfDifferentFrom : Node String -> Node String -> Parser State (Node String)
failIfDifferentFrom (Node _ expectedName) ((Node _ actualName) as actual) =
    if expectedName == actualName then
        Combine.succeed actual

    else
        Combine.fail
            ("Expected to find the declaration for " ++ expectedName ++ " but found " ++ actualName)


function : Parser State (Node Declaration)
function =
    Elm.Parser.Node.parser Tokens.functionName
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.andThen functionWithNameNode
        |> Combine.map (\f -> Node (Expression.functionRange f) (Declaration.FunctionDeclaration f))


functionSignatureFromVarPointer : Node String -> Parser State (Node Signature)
functionSignatureFromVarPointer varPointer =
    Combine.succeed (\ta -> Node.combine Signature varPointer ta)
        |> Combine.ignore (Combine.string ":")
        |> Combine.ignore (Combine.maybe Layout.layout)
        |> Combine.keep Elm.Parser.TypeAnnotation.typeAnnotation
