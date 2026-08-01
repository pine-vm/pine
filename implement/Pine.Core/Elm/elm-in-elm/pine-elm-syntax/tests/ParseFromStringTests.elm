module ParseFromStringTests exposing (..)

import ElmSyntax.Concrete.Declaration as Declaration
import ElmSyntax.Concrete.Expression as Expression
import ElmSyntax.Concrete.Infix as Infix
import ElmSyntax.Concrete.Node as Node exposing (Node(..))
import ElmSyntax.Concrete.Parser.DeclarationOrExpression as DeclarationOrExpression exposing (DeclarationOrExpression)
import ElmSyntax.Concrete.Parser.FromString
import ElmSyntax.Concrete.Parser.Token as Token
import ElmSyntax.Concrete.Parser.TokensFromString as TokensFromString
import ElmSyntax.Concrete.Pattern as Pattern
import ElmSyntax.Concrete.SeparatedSyntaxList as SeparatedSyntaxList
import ElmSyntax.Concrete.TypeAnnotation as TypeAnnotation
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "ParseFromStringTests"
        [ Test.describe "parseExpression_ok"
            expressionOkSuite
        , Test.describe "all expression variants"
            expressionVariantSuite
        , Test.describe "nested patterns and expression boundaries"
            nestedPatternAndBoundarySuite
        , Test.describe "parseExpression_err"
            expressionErrSuite
        , Test.describe "parseDeclarationOrExpression_ok"
            declarationOrExpressionOkSuite
        , Test.describe "parseDeclarationOrExpression_kind"
            declarationOrExpressionKindSuite
        , Test.describe "parseDeclarationOrExpression_err"
            declarationOrExpressionErrSuite
        , Test.describe "mixed line breaks"
            mixedLineBreakSuite
        , Test.describe "literal and comment tokens"
            literalAndCommentTokenSuite
        ]


expressionOkSuite : List Test
expressionOkSuite =
    [ { input = """ 71 """
      , expectedOk = Expression.IntegerLiteral "71"
      }
    , { input = "\n0xFF\n"
      , expectedOk = Expression.IntegerLiteral "0xFF"
      }
    , { input = "1.25"
      , expectedOk = Expression.FloatLiteral "1.25"
      }
    , { input = "6e2"
      , expectedOk = Expression.FloatLiteral "6e2"
      }
    , { input = "\\x y -> 71"
      , expectedOk =
            Expression.LambdaExpression
                { backslashLocation = location 1 1
                , arguments =
                    [ Node (range 1 2 1 3) (Pattern.VarPattern "x")
                    , Node (range 1 4 1 5) (Pattern.VarPattern "y")
                    ]
                , arrowLocation = location 1 6
                , expression = Node (range 1 9 1 11) (Expression.IntegerLiteral "71")
                }
      }
    , { input = "let\n    x = 1\nin\nx"
      , expectedOk =
            Expression.LetExpression
                { letTokenLocation = location 1 1
                , declarations =
                    [ Node (range 2 5 2 10)
                        (Expression.LetFunction
                            { documentation = Nothing
                            , signature = Nothing
                            , declaration =
                                Node (range 2 5 2 10)
                                    { name = Node (range 2 5 2 6) "x"
                                    , arguments = []
                                    , equalsTokenLocation = location 2 7
                                    , expression = Node (range 2 9 2 10) (Expression.IntegerLiteral "1")
                                    }
                            }
                        )
                    ]
                , inTokenLocation = location 3 1
                , expression = Node (range 4 1 4 2) (Expression.Identifier [] "x")
                }
      }
    , { input = "case value of\n    Nothing -> 0\n    result -> 71"
      , expectedOk =
            Expression.CaseExpression
                { caseTokenLocation = location 1 1
                , expression = Node (range 1 6 1 11) (Expression.Identifier [] "value")
                , ofTokenLocation = location 1 12
                , cases =
                    [ { pattern =
                            Node (range 2 5 2 12)
                                (Pattern.NamedPattern { moduleName = [], name = "Nothing" } [])
                      , arrowLocation = location 2 13
                      , expression = Node (range 2 16 2 17) (Expression.IntegerLiteral "0")
                      }
                    , { pattern = Node (range 3 5 3 11) (Pattern.VarPattern "result")
                      , arrowLocation = location 3 12
                      , expression = Node (range 3 15 3 17) (Expression.IntegerLiteral "71")
                      }
                    ]
                }
      }
    ]
        |> List.map
            (\testCase ->
                Test.test ("parseExpression_ok: " ++ testCase.input) <|
                    \_ ->
                        case ElmSyntax.Concrete.Parser.FromString.parseExpression testCase.input of
                            Ok actual ->
                                Expect.equal actual testCase.expectedOk

                            Err err ->
                                Expect.fail ("Expected Ok, but got Err: " ++ err)
            )


expressionErrSuite : List Test
expressionErrSuite =
    [ ""
    , " "
    , "?"
    , "\\x x"
    , "let x = 1"
    , "case x of"
    ]
        |> List.map
            (\input ->
                Test.test ("parseExpression_err: " ++ Debug.toString input) <|
                    \_ ->
                        case ElmSyntax.Concrete.Parser.FromString.parseExpression input of
                            Ok actual ->
                                Expect.fail ("Expected Err, but got Ok: " ++ Debug.toString actual)

                            Err _ ->
                                Expect.pass
            )


declarationOrExpressionOkSuite : List Test
declarationOrExpressionOkSuite =
    [ { input = "x = 71"
      , expectedOk =
            DeclarationOrExpression.Declaration
                (Declaration.FunctionDeclaration
                    (Node (range 1 1 1 7)
                        { documentation = Nothing
                        , signature = Nothing
                        , declaration =
                            Node (range 1 1 1 7)
                                { name = Node (range 1 1 1 2) "x"
                                , arguments = []
                                , equalsTokenLocation = location 1 3
                                , expression = Node (range 1 5 1 7) (Expression.IntegerLiteral "71")
                                }
                        }
                    )
                )
      }
    , { input = "f x = x"
      , expectedOk =
            DeclarationOrExpression.Declaration
                (Declaration.FunctionDeclaration
                    (Node (range 1 1 1 8)
                        { documentation = Nothing
                        , signature = Nothing
                        , declaration =
                            Node (range 1 1 1 8)
                                { name = Node (range 1 1 1 2) "f"
                                , arguments =
                                    [ Node (range 1 3 1 4) (Pattern.VarPattern "x") ]
                                , equalsTokenLocation = location 1 5
                                , expression = Node (range 1 7 1 8) (Expression.Identifier [] "x")
                                }
                        }
                    )
                )
      }
    , { input = "type alias Name = String"
      , expectedOk =
            DeclarationOrExpression.Declaration
                (Declaration.AliasDeclaration
                    (Node (range 1 1 1 25)
                        { documentation = Nothing
                        , typeTokenLocation = location 1 1
                        , aliasTokenLocation = location 1 6
                        , name = Node (range 1 12 1 16) "Name"
                        , generics = []
                        , equalsTokenLocation = location 1 17
                        , typeAnnotation =
                            Node (range 1 19 1 25)
                                (TypeAnnotation.Typed
                                    (Node (range 1 19 1 25) ( [], "String" ))
                                    []
                                )
                        }
                    )
                )
      }
    , { input = "infix left 6 (+) = add"
      , expectedOk =
            DeclarationOrExpression.Declaration
                (Declaration.InfixDeclaration
                    (Node (range 1 1 1 23)
                        { direction = Node (range 1 7 1 11) Infix.Left
                        , precedence = Node (range 1 12 1 13) 6
                        , operator = Node (range 1 14 1 17) "+"
                        , function = Node (range 1 20 1 23) "add"
                        }
                    )
                )
      }
    , { input = "42"
      , expectedOk =
            DeclarationOrExpression.Expression
                (Expression.IntegerLiteral "42")
      }
    , { input = "let\n    x = 1\nin\nx"
      , expectedOk =
            DeclarationOrExpression.Expression
                (Expression.LetExpression
                    { letTokenLocation = location 1 1
                    , declarations =
                        [ Node (range 2 5 2 10)
                            (Expression.LetFunction
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node (range 2 5 2 10)
                                        { name = Node (range 2 5 2 6) "x"
                                        , arguments = []
                                        , equalsTokenLocation = location 2 7
                                        , expression = Node (range 2 9 2 10) (Expression.IntegerLiteral "1")
                                        }
                                }
                            )
                        ]
                    , inTokenLocation = location 3 1
                    , expression = Node (range 4 1 4 2) (Expression.Identifier [] "x")
                    }
                )
      }
    ]
        |> List.map
            (\testCase ->
                Test.test ("parseDeclarationOrExpression_ok: " ++ testCase.input) <|
                    \_ ->
                        case ElmSyntax.Concrete.Parser.FromString.parseDeclarationOrExpression testCase.input of
                            Ok actual ->
                                Expect.equal actual testCase.expectedOk

                            Err err ->
                                Expect.fail ("Expected Ok, but got Err: " ++ err)
            )


declarationOrExpressionKindSuite : List Test
declarationOrExpressionKindSuite =
    [ ( "identity : a -> a\nidentity x = x", "Declaration/FunctionDeclaration" )
    , ( "type Color = Red | Green | Blue", "Declaration/ChoiceTypeDeclaration" )
    , ( "type Tree a = Leaf | Node a (Tree a)", "Declaration/ChoiceTypeDeclaration" )
    , ( "port sendMessage : String -> Cmd msg", "Declaration/PortDeclaration" )
    , ( "1 + 2", "Expression/OperatorApplication" )
    , ( "x", "Expression/Identifier" )
    ]
        |> List.map
            (\( input, expectedKind ) ->
                Test.test ("parseDeclarationOrExpression_kind: " ++ input) <|
                    \_ ->
                        case ElmSyntax.Concrete.Parser.FromString.parseDeclarationOrExpression input of
                            Ok actual ->
                                Expect.equal expectedKind (declarationOrExpressionKind actual)

                            Err err ->
                                Expect.fail ("Expected Ok, but got Err: " ++ err)
            )


declarationOrExpressionErrSuite : List Test
declarationOrExpressionErrSuite =
    [ ""
    , "port"
    , "type"
    , "infix left"
    , "= x"
    ]
        |> List.map
            (\input ->
                Test.test ("parseDeclarationOrExpression_err: " ++ Debug.toString input) <|
                    \_ ->
                        case ElmSyntax.Concrete.Parser.FromString.parseDeclarationOrExpression input of
                            Ok actual ->
                                Expect.fail
                                    ("Expected Err, but got Ok: " ++ Debug.toString actual)

                            Err _ ->
                                Expect.pass
            )


declarationOrExpressionKind : DeclarationOrExpression -> String
declarationOrExpressionKind doe =
    case doe of
        DeclarationOrExpression.Declaration declaration ->
            "Declaration/" ++ declarationKind declaration

        DeclarationOrExpression.Expression expression ->
            "Expression/" ++ expressionKind expression


declarationKind : Declaration.Declaration -> String
declarationKind declaration =
    case declaration of
        Declaration.FunctionDeclaration _ ->
            "FunctionDeclaration"

        Declaration.ChoiceTypeDeclaration _ ->
            "ChoiceTypeDeclaration"

        Declaration.AliasDeclaration _ ->
            "AliasDeclaration"

        Declaration.PortDeclaration _ ->
            "PortDeclaration"

        Declaration.InfixDeclaration _ ->
            "InfixDeclaration"


mixedLineBreakSuite : List Test
mixedLineBreakSuite =
    [ { description = "LF followed by CRLF"
      , input = "alpha\nbeta\u{000D}\ngamma"
      , expectedTokens = threeLineIdentifierTokens
      }
    , { description = "CRLF followed by LF"
      , input = "alpha\u{000D}\nbeta\ngamma"
      , expectedTokens = threeLineIdentifierTokens
      }
    , { description = "lone CR only"
      , input = "alpha\u{000D}beta\u{000D}gamma"
      , expectedTokens = threeLineIdentifierTokens
      }
    , { description = "LF, then CRLF, then lone CR combined"
      , input = "a\nb\u{000D}\nc\u{000D}d"
      , expectedTokens =
            [ token Token.Identifier "a" 1 1 1 2
            , token Token.Newline "\n" 1 2 2 1
            , token Token.Identifier "b" 2 1 2 2
            , token Token.Newline "\n" 2 2 3 1
            , token Token.Identifier "c" 3 1 3 2
            , token Token.Newline "\n" 3 2 4 1
            , token Token.Identifier "d" 4 1 4 2
            ]
      }
    ]
        |> List.map
            (\testCase ->
                Test.test testCase.description <|
                    \_ ->
                        case TokensFromString.parseExpression testCase.input of
                            Ok actual ->
                                Expect.equal testCase.expectedTokens actual

                            Err err ->
                                Expect.fail ("Expected Ok, but got Err: " ++ err)
            )


threeLineIdentifierTokens : List Token.Token
threeLineIdentifierTokens =
    [ token Token.Identifier "alpha" 1 1 1 6
    , token Token.Newline "\n" 1 6 2 1
    , token Token.Identifier "beta" 2 1 2 5
    , token Token.Newline "\n" 2 5 3 1
    , token Token.Identifier "gamma" 3 1 3 6
    ]


{-| Covers literal decoding/raw-text preservation (escapes, unicode escapes, triple-quoted
strings spanning mixed line breaks) and multi-line comments with a line break inside, all as
single-token expectations exercising the offset/String-based scanners directly.
-}
literalAndCommentTokenSuite : List Test
literalAndCommentTokenSuite =
    [ { description = "string literal with backslash escapes"
      , input = "\"a\\nb\\tc\""
      , expectedTokens =
            [ tokenWithRaw Token.StringLiteral "a\nb\tc" "a\\nb\\tc" 1 1 1 10 ]
      }
    , { description = "string literal with unicode escape"
      , input = "\"\\u{1F600}\""
      , expectedTokens =
            [ tokenWithRaw Token.StringLiteral "\u{1F600}" "\\u{1F600}" 1 1 1 12 ]
      }
    , { description = "char literal"
      , input = "'x'"
      , expectedTokens =
            [ tokenWithRaw Token.CharLiteral "x" "x" 1 1 1 4 ]
      }
    , { description = "hexadecimal integer literal"
      , input = "0xFF"
      , expectedTokens =
            [ token Token.NumberLiteral "0xFF" 1 1 1 5 ]
      }
    , { description = "minus immediately after identifier is an operator"
      , input = "x-y"
      , expectedTokens =
            [ token Token.Identifier "x" 1 1 1 2
            , token Token.Operator "-" 1 2 1 3
            , token Token.Identifier "y" 1 3 1 4
            ]
      }
    , { description = "triple-quoted string spanning LF and CRLF line breaks"
      , input = "\"\"\"a\nb\u{000D}\nc\"\"\""
      , expectedTokens =
            [ tokenWithRaw Token.TripleQuotedStringLiteral "a\nb\nc" "a\nb\nc" 1 1 3 5 ]
      }
    , { description = "nested multi-line comment with a CRLF line break inside"
      , input = "{- outer\u{000D}\n {- inner -} end -}"
      , expectedTokens =
            [ token Token.Comment "{- outer\n {- inner -} end -}" 1 1 2 20 ]
      }
    ]
        |> List.map
            (\testCase ->
                Test.test testCase.description <|
                    \_ ->
                        case TokensFromString.parseExpression testCase.input of
                            Ok actual ->
                                Expect.equal testCase.expectedTokens actual

                            Err err ->
                                Expect.fail ("Expected Ok, but got Err: " ++ err)
            )


expressionVariantSuite : List Test
expressionVariantSuite =
    [ ( "unit", "()", "UnitExpr" )
    , ( "string", "\"hello\\nworld\"", "StringLiteral" )
    , ( "multiline string", "\"\"\"hello\nworld\"\"\"", "MultilineStringLiteral" )
    , ( "character", "'x'", "CharLiteral" )
    , ( "integer", "42", "IntegerLiteral" )
    , ( "float", "3.14", "FloatLiteral" )
    , ( "negation", "-value", "Negation" )
    , ( "list", "[ 1, 2 ]", "ListExpr" )
    , ( "identifier", "Module.value", "Identifier" )
    , ( "if", "if condition then 1 else 2", "IfBlock" )
    , ( "prefix operator", "(+)", "PrefixOperator" )
    , ( "parenthesized", "(value)", "Parenthesized" )
    , ( "application", "map identity values", "Application" )
    , ( "operator application", "1 + 2 * 3", "OperatorApplication" )
    , ( "tuple", "( 1, 2, 3 )", "TupledExpression" )
    , ( "lambda", "\\x -> x", "LambdaExpression" )
    , ( "case", "case value of\n    Nothing -> 0\n    Just x -> x", "CaseExpression" )
    , ( "let", "let\n    x = 1\nin\nx", "LetExpression" )
    , ( "record", "{ first = 1, second = 2 }", "RecordExpr" )
    , ( "record access", "model.user.name", "RecordAccess" )
    , ( "record access function", ".name", "RecordAccessFunction" )
    , ( "record update", "{ model | count = model.count + 1 }", "RecordUpdateExpression" )
    ]
        |> List.map
            (\( description, input, expectedKind ) ->
                Test.test description <|
                    \_ ->
                        case ElmSyntax.Concrete.Parser.FromString.parseExpression input of
                            Ok actual ->
                                Expect.equal expectedKind (expressionKind actual)

                            Err err ->
                                Expect.fail ("Expected Ok, but got Err: " ++ err)
            )
        |> (\variantTests ->
                variantTests
                    ++ [ Test.test "nested expressions retain every nested node" <|
                            \_ ->
                                let
                                    input =
                                        String.trim
                                            """
                                            case outer of
                                                Just x ->
                                                    if x then
                                                        { model | value = [ f (x + 1), 2 ] }
                                                    else
                                                        case x of
                                                            Nothing -> ()
                                                            Just y -> y.field
                                            """

                                    expectedKinds =
                                        [ "CaseExpression"
                                        , "Identifier"
                                        , "IfBlock"
                                        , "Identifier"
                                        , "RecordUpdateExpression"
                                        , "ListExpr"
                                        , "Application"
                                        , "Identifier"
                                        , "Parenthesized"
                                        , "OperatorApplication"
                                        , "Identifier"
                                        , "IntegerLiteral"
                                        , "IntegerLiteral"
                                        , "CaseExpression"
                                        , "Identifier"
                                        , "UnitExpr"
                                        , "RecordAccess"
                                        , "Identifier"
                                        ]
                                in
                                case ElmSyntax.Concrete.Parser.FromString.parseExpression input of
                                    Ok actual ->
                                        Expect.equal expectedKinds (expressionKinds actual)

                                    Err err ->
                                        Expect.fail ("Expected nested expression to parse, but got Err: " ++ err)
                       ]
           )


nestedPatternAndBoundarySuite : List Test
nestedPatternAndBoundarySuite =
    [ Test.test "record patterns allow trivia after commas" <|
        \_ ->
            expectParseOk "\\{ first, second } -> first"
    , Test.test "case branches cover every pattern form" <|
        \_ ->
            expectParseOk
                (String.trim
                    """
                    case value of
                        _ -> 0
                        () -> 1
                        'a' -> 2
                        "text" -> 3
                        4 -> 4
                        0xF -> 5
                        1.5 -> 6
                        ( x, y ) -> 7
                        { x, y } -> 8
                        head :: tail -> 9
                        [ x, y ] -> 10
                        Just x -> 11
                        (Just x) as whole -> 12
                    """
                )
    , Test.test "let destructuring contains a nested application body" <|
        \_ ->
            case
                ElmSyntax.Concrete.Parser.FromString.parseExpression
                    "let\n    ( x, y ) = pair\nin\ncombine x y"
            of
                Ok actual ->
                    Expect.equal
                        [ "LetExpression"
                        , "Identifier"
                        , "Application"
                        , "Identifier"
                        , "Identifier"
                        , "Identifier"
                        ]
                        (expressionKinds actual)

                Err err ->
                    Expect.fail ("Expected Ok, but got Err: " ++ err)
    , Test.test "inline let retains indented application in its body" <|
        \_ ->
            case
                ElmSyntax.Concrete.Parser.FromString.parseExpression
                    "let x = 1 in\n    foo\n        bar"
            of
                Ok actual ->
                    Expect.equal
                        [ "LetExpression"
                        , "IntegerLiteral"
                        , "Application"
                        , "Identifier"
                        , "Identifier"
                        ]
                        (expressionKinds actual)

                Err err ->
                    Expect.fail ("Expected Ok, but got Err: " ++ err)
    , Test.test "space before dot parses a record access function argument" <|
        \_ ->
            case ElmSyntax.Concrete.Parser.FromString.parseExpression "record .field" of
                Ok actual ->
                    Expect.equal
                        [ "Application", "Identifier", "RecordAccessFunction" ]
                        (expressionKinds actual)

                Err err ->
                    Expect.fail ("Expected Ok, but got Err: " ++ err)
    ]


expectParseOk : String -> Expect.Expectation
expectParseOk input =
    case ElmSyntax.Concrete.Parser.FromString.parseExpression input of
        Ok _ ->
            Expect.pass

        Err err ->
            Expect.fail ("Expected Ok, but got Err: " ++ err)


expressionKind : Expression.Expression -> String
expressionKind expression =
    case expression of
        Expression.UnitExpr ->
            "UnitExpr"

        Expression.StringLiteral _ _ ->
            "StringLiteral"

        Expression.MultilineStringLiteral _ _ ->
            "MultilineStringLiteral"

        Expression.CharLiteral _ ->
            "CharLiteral"

        Expression.IntegerLiteral _ ->
            "IntegerLiteral"

        Expression.FloatLiteral _ ->
            "FloatLiteral"

        Expression.Negation _ ->
            "Negation"

        Expression.ListExpr _ ->
            "ListExpr"

        Expression.Identifier _ _ ->
            "Identifier"

        Expression.IfBlock _ _ _ _ _ _ ->
            "IfBlock"

        Expression.PrefixOperator _ ->
            "PrefixOperator"

        Expression.Parenthesized _ ->
            "Parenthesized"

        Expression.Application _ _ ->
            "Application"

        Expression.OperatorApplication _ _ _ _ ->
            "OperatorApplication"

        Expression.TupledExpression _ ->
            "TupledExpression"

        Expression.LambdaExpression _ ->
            "LambdaExpression"

        Expression.CaseExpression _ ->
            "CaseExpression"

        Expression.LetExpression _ ->
            "LetExpression"

        Expression.RecordExpr _ ->
            "RecordExpr"

        Expression.RecordAccess _ _ ->
            "RecordAccess"

        Expression.RecordAccessFunction _ ->
            "RecordAccessFunction"

        Expression.RecordUpdateExpression _ _ _ ->
            "RecordUpdateExpression"

        Expression.GLSLExpression _ ->
            "GLSLExpression"


expressionKinds : Expression.Expression -> List String
expressionKinds expression =
    expressionKind expression
        :: (case expression of
                Expression.Negation nested ->
                    nodeExpressionKinds nested

                Expression.ListExpr elements ->
                    separatedExpressionKinds elements

                Expression.IfBlock _ condition _ thenBranch _ elseBranch ->
                    nodeExpressionKinds condition
                        ++ nodeExpressionKinds thenBranch
                        ++ nodeExpressionKinds elseBranch

                Expression.Parenthesized nested ->
                    nodeExpressionKinds nested

                Expression.Application function arguments ->
                    nodeExpressionKinds function
                        ++ List.concatMap nodeExpressionKinds arguments

                Expression.OperatorApplication _ _ left right ->
                    nodeExpressionKinds left ++ nodeExpressionKinds right

                Expression.TupledExpression elements ->
                    separatedExpressionKinds elements

                Expression.LambdaExpression lambda ->
                    nodeExpressionKinds lambda.expression

                Expression.CaseExpression caseBlock ->
                    nodeExpressionKinds caseBlock.expression
                        ++ List.concatMap (.expression >> nodeExpressionKinds) caseBlock.cases

                Expression.LetExpression letBlock ->
                    List.concatMap letDeclarationExpressionKinds letBlock.declarations
                        ++ nodeExpressionKinds letBlock.expression

                Expression.RecordExpr fields ->
                    separatedRecordFieldKinds fields

                Expression.RecordAccess record _ ->
                    nodeExpressionKinds record

                Expression.RecordUpdateExpression _ _ fields ->
                    separatedRecordFieldKinds fields

                _ ->
                    []
           )


nodeExpressionKinds : Node Expression.Expression -> List String
nodeExpressionKinds (Node _ expression) =
    expressionKinds expression


separatedExpressionKinds : SeparatedSyntaxList.SeparatedSyntaxList (Node Expression.Expression) -> List String
separatedExpressionKinds separated =
    case separated of
        SeparatedSyntaxList.Empty ->
            []

        SeparatedSyntaxList.NonEmpty first rest ->
            nodeExpressionKinds first
                ++ List.concatMap (Tuple.second >> nodeExpressionKinds) rest


separatedRecordFieldKinds : SeparatedSyntaxList.SeparatedSyntaxList Expression.RecordExprField -> List String
separatedRecordFieldKinds separated =
    case separated of
        SeparatedSyntaxList.Empty ->
            []

        SeparatedSyntaxList.NonEmpty first rest ->
            nodeExpressionKinds first.valueExpr
                ++ List.concatMap (Tuple.second >> .valueExpr >> nodeExpressionKinds) rest


letDeclarationExpressionKinds : Node Expression.LetDeclaration -> List String
letDeclarationExpressionKinds (Node _ declaration) =
    case declaration of
        Expression.LetFunction function ->
            nodeExpressionKinds (Node.value function.declaration).expression

        Expression.LetDestructuring _ _ expression ->
            nodeExpressionKinds expression


location : Int -> Int -> { row : Int, column : Int }
location row column =
    { row = row, column = column }


range : Int -> Int -> Int -> Int -> { start : { row : Int, column : Int }, end : { row : Int, column : Int } }
range startRow startColumn endRow endColumn =
    { start = location startRow startColumn
    , end = location endRow endColumn
    }


token : Token.TokenType -> String -> Int -> Int -> Int -> Int -> Token.Token
token tokenType lexeme startRow startColumn endRow endColumn =
    { tokenType = tokenType
    , lexeme = lexeme
    , start = location startRow startColumn
    , end = location endRow endColumn
    , rawText = Nothing
    }


tokenWithRaw : Token.TokenType -> String -> String -> Int -> Int -> Int -> Int -> Token.Token
tokenWithRaw tokenType lexeme rawText startRow startColumn endRow endColumn =
    { tokenType = tokenType
    , lexeme = lexeme
    , start = location startRow startColumn
    , end = location endRow endColumn
    , rawText = Just rawText
    }
