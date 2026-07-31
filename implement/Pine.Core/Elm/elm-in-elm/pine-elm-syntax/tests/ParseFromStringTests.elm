module ParseFromStringTests exposing (..)

import ElmSyntax.Concrete.Expression as Expression
import ElmSyntax.Concrete.Node as Node exposing (Node(..))
import ElmSyntax.Concrete.Parser.FromString
import ElmSyntax.Concrete.Pattern as Pattern
import ElmSyntax.Concrete.SeparatedSyntaxList as SeparatedSyntaxList
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
