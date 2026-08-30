module ParseFromStringTests exposing (..)

import ElmSyntax.Abstract.ConvertFromConcrete as Convert
import ElmSyntax.Abstract.Declaration as AbstractDeclaration
import ElmSyntax.Abstract.Exposing as AbstractExposing
import ElmSyntax.Abstract.Expression as AbstractExpression
import ElmSyntax.Abstract.Infix as AbstractInfix
import ElmSyntax.Abstract.Module as AbstractModule
import ElmSyntax.Abstract.TypeAnnotation as AbstractTypeAnnotation
import ElmSyntax.Concrete.Declaration as Declaration
import ElmSyntax.Concrete.Exposing as Exposing
import ElmSyntax.Concrete.Expression as Expression
import ElmSyntax.Concrete.Infix as Infix
import ElmSyntax.Concrete.Module as Module
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
        , Test.describe "parser-specific tokenization"
            parserSpecificTokenizationSuite
        , Test.describe "literal and comment tokens"
            literalAndCommentTokenSuite
        , Test.describe "parseFile concrete model"
            concreteFileSuite
        , Test.describe "documentation comment attachment"
            documentationCommentSuite
        , Test.describe "parseFile abstract model"
            abstractFileSuite
        , Test.describe "direct source parsing"
            directSourceParsingSuite
        ]


{-| Covers behavior that is specific to parsing directly from the source string: trivia
(comments and line breaks) appearing between arbitrary syntax elements, literal decoding
including raw text, and the adjacency rules that distinguish record access from other uses
of a dot.
-}
directSourceParsingSuite : List Test
directSourceParsingSuite =
    List.append
        (List.map
            (\testCase ->
                Test.test testCase.title <|
                    \_ ->
                        case ElmSyntax.Concrete.Parser.FromString.parseExpression testCase.input of
                            Ok actual ->
                                Expect.equal testCase.expected actual

                            Err err ->
                                Expect.fail ("Expected Ok, but got Err: " ++ err)
            )
            directSourceParsingExpressionCases
        )
        ([ Test.test "carriage returns between declarations do not shift ranges" <|
            \_ ->
                case
                    ElmSyntax.Concrete.Parser.FromString.parseFile
                        "module Main exposing (..)\u{000D}\n\u{000D}\nvalue = 1"
                of
                    Ok file ->
                        Expect.equal
                            [ range 3 1 3 10 ]
                            (nodeRanges file.declarations)

                    Err err ->
                        Expect.fail ("Expected Ok, but got Err: " ++ err)
         , Test.test "comments inside a declaration body are collected" <|
            \_ ->
                case
                    ElmSyntax.Concrete.Parser.FromString.parseFile
                        "module Main exposing (..)\n\nvalue =\n    -- inner\n    1"
                of
                    Ok file ->
                        Expect.equal
                            ( [ Node (range 4 5 4 13) "-- inner" ]
                            , [ range 3 1 5 6 ]
                            )
                            ( file.comments, nodeRanges file.declarations )

                    Err err ->
                        Expect.fail ("Expected Ok, but got Err: " ++ err)
         , Test.test "comment inside a string literal is not a comment" <|
            \_ ->
                case
                    ElmSyntax.Concrete.Parser.FromString.parseFile
                        "module Main exposing (..)\n\nvalue = \"-- not a comment\"\n"
                of
                    Ok file ->
                        Expect.equal [] file.comments

                    Err err ->
                        Expect.fail ("Expected Ok, but got Err: " ++ err)
         ]
            ++ List.map
                (\testCase ->
                    Test.test testCase.title <|
                        \_ ->
                            case ElmSyntax.Concrete.Parser.FromString.parseFile testCase.input of
                                Ok actual ->
                                    Expect.fail ("Expected Err, but got Ok: " ++ Debug.toString actual)

                                Err _ ->
                                    Expect.pass
                )
                directSourceParsingInvalidQualifiedNameCases
        )


directSourceParsingExpressionCases :
    List { title : String, input : String, expected : Expression.Expression }
directSourceParsingExpressionCases =
    [ { title = "block comment between function and argument"
      , input = "alfa {- c -} beta"
      , expected =
            Expression.Application
                (Node (range 1 1 1 5) (Expression.Identifier [] "alfa"))
                [ Node (range 1 14 1 18) (Expression.Identifier [] "beta") ]
      }
    , { title = "nested block comment between function and argument"
      , input = "alfa {- outer {- inner -} -} beta"
      , expected =
            Expression.Application
                (Node (range 1 1 1 5) (Expression.Identifier [] "alfa"))
                [ Node (range 1 30 1 34) (Expression.Identifier [] "beta") ]
      }
    , { title = "line comment between list elements"
      , input = "[ 1\n-- note\n, 2 ]"
      , expected =
            Expression.ListExpr
                (SeparatedSyntaxList.NonEmpty
                    (Node (range 1 3 1 4) (Expression.IntegerLiteral "1"))
                    [ ( location 3 1
                      , Node (range 3 3 3 4) (Expression.IntegerLiteral "2")
                      )
                    ]
                )
      }
    , { title = "string literal decodes escapes and retains raw text"
      , input = "\"a\\nb\""
      , expected =
            Expression.StringLiteral "a\nb" (Just "a\\nb")
      }
    , { title = "triple-quoted string literal spanning two lines"
      , input = "\"\"\"first\nsecond\"\"\""
      , expected =
            Expression.MultilineStringLiteral "first\nsecond"
                (Just [ "first", "second" ])
      }
    , { title = "character literal with a unicode escape"
      , input = "'\\u{1F600}'"
      , expected =
            Expression.CharLiteral 0x0001F600
      }
    , { title = "record access directly after a parenthesized expression"
      , input = "(alfa).field"
      , expected =
            Expression.RecordAccess
                (Node (range 1 1 1 7)
                    (Expression.Parenthesized
                        (Node (range 1 2 1 6) (Expression.Identifier [] "alfa"))
                    )
                )
                (Node (range 1 8 1 13) "field")
      }
    , { title = "comment between qualified name parts"
      , input = "Alfa{- c -}.beta"
      , expected =
            Expression.Application
                (Node (range 1 1 1 5) (Expression.Identifier [] "Alfa"))
                [ Node (range 1 12 1 17) (Expression.RecordAccessFunction ".beta") ]
      }
    ]


directSourceParsingInvalidQualifiedNameCases : List { title : String, input : String }
directSourceParsingInvalidQualifiedNameCases =
    [ { title = "comment between module name parts is rejected"
      , input = "module Main{- c -}.Nested exposing (..)"
      }
    , { title = "comment between imported module name parts is rejected"
      , input = "module Main exposing (..)\n\nimport Alfa{- c -}.Beta"
      }
    , { title = "comment between qualified type name parts is rejected"
      , input = "module Main exposing (..)\n\nvalue : Alfa{- c -}.Beta\nvalue = 0"
      }
    , { title = "comment between qualified pattern name parts is rejected"
      , input = "module Main exposing (..)\n\nvalue (Alfa{- c -}.field) = 0"
      }
    ]


nodeRanges : List (Node a) -> List { start : { row : Int, column : Int }, end : { row : Int, column : Int } }
nodeRanges nodes =
    case nodes of
        (Node nodeRange _) :: rest ->
            nodeRange :: nodeRanges rest

        [] ->
            []


documentationCommentSuite : List Test
documentationCommentSuite =
    [ { title = "attaches documentation and excludes it from file comments"
      , source = "module Main exposing (..)\n\n{-| docs -}\nvalue = 1"
      , expectedDocumentation = Just "{-| docs -}"
      , expectedComments = []
      }
    , { title = "ordinary comment after documentation prevents attachment"
      , source = "module Main exposing (..)\n\nfirst = 1\n\n{-| docs -}\n-- note\nsecond = 2"
      , expectedDocumentation = Nothing
      , expectedComments = [ "{-| docs -}", "-- note" ]
      }
    , { title = "attaches latest documentation and retains earlier candidate"
      , source = "module Main exposing (..)\n\nfirst = 1\n\n{-| earlier -}\n{-| latest -}\nsecond = 2"
      , expectedDocumentation = Just "{-| latest -}"
      , expectedComments = [ "{-| earlier -}" ]
      }
    ]
        |> List.map
            (\testCase ->
                Test.test testCase.title <|
                    \_ ->
                        case ElmSyntax.Concrete.Parser.FromString.parseFile testCase.source of
                            Ok file ->
                                Expect.equal
                                    ( testCase.expectedDocumentation, testCase.expectedComments )
                                    ( lastDeclarationDocumentation file.declarations
                                    , nodeStrings file.comments
                                    )

                            Err error ->
                                Expect.fail ("Expected Ok, but got Err: " ++ error)
            )


lastDeclarationDocumentation : List (Node Declaration.Declaration) -> Maybe String
lastDeclarationDocumentation declarations =
    case List.reverse declarations of
        Node _ (Declaration.FunctionDeclaration function) :: _ ->
            case function.documentation of
                Just (Node _ documentation) ->
                    Just documentation

                Nothing ->
                    Nothing

        _ ->
            Nothing


nodeStrings : List (Node String) -> List String
nodeStrings nodes =
    case nodes of
        Node _ value :: rest ->
            value :: nodeStrings rest

        [] ->
            []


concreteFileSuite : List Test
concreteFileSuite =
    [ { title = "parses module, import, comments, and multiple declarations with ranges"
      , source =
            """
module Main exposing (..)

-- file comment
import Html as H exposing (Html)

first = 1

second = first
"""
      , expected =
            { moduleDefinition =
                Node (range 1 1 1 26)
                    (Module.NormalModule
                        { moduleName = Node (range 1 8 1 12) [ "Main" ]
                        , exposingList =
                            Node (range 1 13 1 26)
                                (Exposing.All (range 1 23 1 25))
                        }
                    )
            , imports =
                [ Node (range 4 1 4 33)
                    { importTokenLocation = location 4 1
                    , moduleName = Node (range 4 8 4 12) [ "Html" ]
                    , moduleAlias =
                        Just
                            ( location 4 13
                            , Node (range 4 16 4 17) [ "H" ]
                            )
                    , exposingList =
                        Just
                            ( location 4 18
                            , Node (range 4 18 4 33)
                                (Exposing.Explicit
                                    (location 4 27)
                                    (SeparatedSyntaxList.NonEmpty
                                        (Node (range 4 28 4 32)
                                            (Exposing.TypeOrAliasExpose "Html")
                                        )
                                        []
                                    )
                                    (location 4 32)
                                )
                            )
                    }
                ]
            , comments =
                [ Node (range 3 1 3 16) "-- file comment" ]
            , declarations =
                [ simpleFunctionDeclaration
                    (range 6 1 6 10)
                    (range 6 1 6 6)
                    (location 6 7)
                    (Node (range 6 9 6 10) (Expression.IntegerLiteral "1"))
                    "first"
                , simpleFunctionDeclaration
                    (range 8 1 8 15)
                    (range 8 1 8 7)
                    (location 8 8)
                    (Node (range 8 10 8 15) (Expression.Identifier [] "first"))
                    "second"
                ]
            , incompleteDeclarations = []
            }
      }
    , { title = "various multi-line comments and a multi-line string literal"
      , source =
            """
module Main exposing (..)

{- multi-line - } comment -}

{- multi-line comment
with a line break -}

{- multi-line comment
{- inner -}
with multiple line breaks
-}


decl = \"\"\"multiline
string\"\"\"

        """
      , expected =
            { moduleDefinition =
                Node (range 1 1 1 26)
                    (Module.NormalModule
                        { moduleName = Node (range 1 8 1 12) [ "Main" ]
                        , exposingList =
                            Node (range 1 13 1 26)
                                (Exposing.All (range 1 23 1 25))
                        }
                    )
            , imports = []
            , comments =
                [ Node (range 3 1 3 29) "{- multi-line - } comment -}"
                , Node (range 5 1 6 21) "{- multi-line comment\nwith a line break -}"
                , Node (range 8 1 11 3) "{- multi-line comment\n{- inner -}\nwith multiple line breaks\n-}"
                ]
            , declarations =
                [ simpleFunctionDeclaration
                    (range 14 1 15 10)
                    (range 14 1 14 5)
                    (location 14 6)
                    (Node (range 14 8 15 10)
                        (Expression.MultilineStringLiteral "multiline\nstring" (Just [ "multiline", "string" ]))
                    )
                    "decl"
                ]
            , incompleteDeclarations = []
            }
      }
    ]
        |> List.map
            (\testCase ->
                Test.test testCase.title <|
                    \_ ->
                        case ElmSyntax.Concrete.Parser.FromString.parseFile (String.trim testCase.source) of
                            Ok actual ->
                                Expect.equal testCase.expected actual

                            Err error ->
                                Expect.fail error
            )


abstractFileSuite : List Test
abstractFileSuite =
    [ Test.test "parses a complete module into the abstract model" <|
        \_ ->
            let
                source =
                    """
module Example exposing (..)

import Html exposing (Html)

type alias Model = Int

type Message = Increment | Set Int

infix left 6 (+) = add

port sendMessage : String -> Cmd msg

initial : Int
initial = 0

declA =
    [ 71, 0x4F, 0x051 ]
"""

                expected =
                    { moduleDefinition =
                        AbstractModule.NormalModule
                            { moduleName = [ "Example" ]
                            , exposingList = AbstractExposing.All
                            }
                    , imports =
                        [ { moduleName = [ "Html" ]
                          , moduleAlias = Nothing
                          , exposingList =
                                Just
                                    (AbstractExposing.Explicit
                                        [ AbstractExposing.TypeOrAliasExpose "Html" ]
                                    )
                          }
                        ]
                    , declarations =
                        [ AbstractDeclaration.AliasDeclaration
                            { name = "Model"
                            , generics = []
                            , typeAnnotation =
                                AbstractTypeAnnotation.Typed [] "Int" []
                            }
                        , AbstractDeclaration.ChoiceTypeDeclaration
                            { name = "Message"
                            , generics = []
                            , constructors =
                                [ { name = "Increment", arguments = [] }
                                , { name = "Set"
                                  , arguments =
                                        [ AbstractTypeAnnotation.Typed [] "Int" [] ]
                                  }
                                ]
                            }
                        , AbstractDeclaration.InfixDeclaration
                            { direction = AbstractInfix.Left
                            , precedence = 6
                            , operator = "+"
                            , functionName = "add"
                            }
                        , AbstractDeclaration.PortDeclaration
                            { name = "sendMessage"
                            , typeAnnotation =
                                AbstractTypeAnnotation.FunctionTypeAnnotation
                                    (AbstractTypeAnnotation.Typed [] "String" [])
                                    (AbstractTypeAnnotation.Typed []
                                        "Cmd"
                                        [ AbstractTypeAnnotation.GenericType "msg" ]
                                    )
                            }
                        , AbstractDeclaration.FunctionDeclaration
                            { signature =
                                Just
                                    { name = "initial"
                                    , typeAnnotation =
                                        AbstractTypeAnnotation.Typed [] "Int" []
                                    }
                            , declaration =
                                { name = "initial"
                                , arguments = []
                                , expression = AbstractExpression.IntegerLiteral 0
                                }
                            }
                        , AbstractDeclaration.FunctionDeclaration
                            { signature = Nothing
                            , declaration =
                                { name = "declA"
                                , arguments = []
                                , expression =
                                    AbstractExpression.ListExpr
                                        [ AbstractExpression.IntegerLiteral 71
                                        , AbstractExpression.IntegerLiteral 0x4F
                                        , AbstractExpression.IntegerLiteral 0x051
                                        ]
                                }
                            }
                        ]
                    }
            in
            case ElmSyntax.Concrete.Parser.FromString.parseFile (String.trim source) of
                Ok concrete ->
                    Expect.equal expected (Convert.fromFile concrete)

                Err error ->
                    Expect.fail error
    ]


simpleFunctionDeclaration :
    { start : { row : Int, column : Int }, end : { row : Int, column : Int } }
    -> { start : { row : Int, column : Int }, end : { row : Int, column : Int } }
    -> { row : Int, column : Int }
    -> Node Expression.Expression
    -> String
    -> Node Declaration.Declaration
simpleFunctionDeclaration declarationRange_ nameRange equalsLocation expression name =
    Node declarationRange_
        (Declaration.FunctionDeclaration
            { documentation = Nothing
            , signature = Nothing
            , declaration =
                Node declarationRange_
                    { name = Node nameRange name
                    , arguments = []
                    , equalsTokenLocation = equalsLocation
                    , expression = expression
                    }
            }
        )


expressionOkSuite : List Test
expressionOkSuite =
    [ { input = """ 71 """
      , expectedOk = Expression.IntegerLiteral "71"
      }
    , { input = """just_an_identifier"""
      , expectedOk = Expression.Identifier [] "just_an_identifier"
      }
    , { input = """[]"""
      , expectedOk = Expression.ListExpr SeparatedSyntaxList.Empty
      }
    , { input = """[ ]"""
      , expectedOk = Expression.ListExpr SeparatedSyntaxList.Empty
      }
    , { input = """[ identifier_in_list ]"""
      , expectedOk =
            Expression.ListExpr
                (SeparatedSyntaxList.NonEmpty
                    (Node (range 1 3 1 21) (Expression.Identifier [] "identifier_in_list"))
                    []
                )
      }
    , { input = """[ 71 ]"""
      , expectedOk =
            Expression.ListExpr
                (SeparatedSyntaxList.NonEmpty
                    (Node (range 1 3 1 5) (Expression.IntegerLiteral "71"))
                    []
                )
      }
    , { input = """[ 71, 73 ]"""
      , expectedOk =
            Expression.ListExpr
                (SeparatedSyntaxList.NonEmpty
                    (Node (range 1 3 1 5) (Expression.IntegerLiteral "71"))
                    [ ( location 1 5
                      , Node (range 1 7 1 9) (Expression.IntegerLiteral "73")
                      )
                    ]
                )
      }
    , { input = """[ 71 {- a comment -}, {- another comment -} 73 ]"""
      , expectedOk =
            Expression.ListExpr
                (SeparatedSyntaxList.NonEmpty
                    (Node (range 1 3 1 5) (Expression.IntegerLiteral "71"))
                    [ ( location 1 21
                      , Node (range 1 45 1 47) (Expression.IntegerLiteral "73")
                      )
                    ]
                )
      }
    , { input = "\n0xFF\n"
      , expectedOk = Expression.IntegerLiteral "0xFF"
      }
    , { input = "\"\""
      , expectedOk = Expression.StringLiteral "" (Just "")
      }
    , { input = "\"hello world\""
      , expectedOk = Expression.StringLiteral "hello world" (Just "hello world")
      }
    , { input = """{}"""
      , expectedOk =
            Expression.RecordExpr
                SeparatedSyntaxList.Empty
      }
    , { input = """{ }"""
      , expectedOk =
            Expression.RecordExpr
                SeparatedSyntaxList.Empty
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
    , { input = "let\n    identity : a -> a\n    identity value = value\nin\nidentity 1"
      , expectedOk =
            Expression.LetExpression
                { letTokenLocation = location 1 1
                , declarations =
                    [ Node (range 2 5 3 27)
                        (Expression.LetFunction
                            { documentation = Nothing
                            , signature =
                                Just
                                    (Node (range 2 5 2 22)
                                        { name = Node (range 2 5 2 13) "identity"
                                        , colonLocation = location 2 14
                                        , typeAnnotation =
                                            Node (range 2 16 2 22)
                                                (TypeAnnotation.FunctionTypeAnnotation
                                                    (Node (range 2 16 2 17)
                                                        (TypeAnnotation.GenericType "a")
                                                    )
                                                    (location 2 18)
                                                    (Node (range 2 21 2 22)
                                                        (TypeAnnotation.GenericType "a")
                                                    )
                                                )
                                        }
                                    )
                            , declaration =
                                Node (range 3 5 3 27)
                                    { name = Node (range 3 5 3 13) "identity"
                                    , arguments =
                                        [ Node (range 3 14 3 19) (Pattern.VarPattern "value") ]
                                    , equalsTokenLocation = location 3 20
                                    , expression =
                                        Node (range 3 22 3 27) (Expression.Identifier [] "value")
                                    }
                            }
                        )
                    ]
                , inTokenLocation = location 4 1
                , expression =
                    Node (range 5 1 5 11)
                        (Expression.Application
                            (Node (range 5 1 5 9) (Expression.Identifier [] "identity"))
                            [ Node (range 5 10 5 11) (Expression.IntegerLiteral "1") ]
                        )
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
    , { input = "alfa ++ beta"
      , expectedOk =
            Expression.OperatorApplication
                (Node (range 1 6 1 8) "++")
                Infix.Right
                (Node (range 1 1 1 5) (Expression.Identifier [] "alfa"))
                (Node (range 1 9 1 13) (Expression.Identifier [] "beta"))
      }
    , { input = "alfa |> beta"
      , expectedOk =
            Expression.OperatorApplication
                (Node (range 1 6 1 8) "|>")
                Infix.Left
                (Node (range 1 1 1 5) (Expression.Identifier [] "alfa"))
                (Node (range 1 9 1 13) (Expression.Identifier [] "beta"))
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
    , "Alfa.{- c -}beta"
    , "Alfa{- c -}.Beta"
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
      }
    , { input = "f x = x"
      , expectedOk =
            DeclarationOrExpression.Declaration
                (Declaration.FunctionDeclaration
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
      }
    , { input = "type alias Name = String"
      , expectedOk =
            DeclarationOrExpression.Declaration
                (Declaration.AliasDeclaration
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
      }
    , { input = "infix left 6 (+) = add"
      , expectedOk =
            DeclarationOrExpression.Declaration
                (Declaration.InfixDeclaration
                    { infixTokenLocation = location 1 1
                    , direction = Node (range 1 7 1 11) Infix.Left
                    , precedence = Node (range 1 12 1 13) 6
                    , operator = Node (range 1 14 1 17) "+"
                    , equalsTokenLocation = location 1 18
                    , function = Node (range 1 20 1 23) "add"
                    }
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
    , { input = "func \"\"\"multi\nline\"\"\""
      , expectedOk =
            DeclarationOrExpression.Expression
                (Expression.Application
                    (Node
                        (range 1 1 1 5)
                        (Expression.Identifier [] "func")
                    )
                    [ Node
                        (range 1 6 2 8)
                        (Expression.MultilineStringLiteral "multi\nline" (Just [ "multi", "line" ]))
                    ]
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
    , ( "type alias Name = String", "Declaration/AliasDeclaration" )
    , ( "port sendMessage : String -> Cmd msg", "Declaration/PortDeclaration" )
    , ( "infix left 6 (+) = add", "Declaration/InfixDeclaration" )
    , ( "1 + 2", "Expression/OperatorApplication" )
    , ( "x", "Expression/Identifier" )
    , ( "render { value = 1 }", "Expression/Application" )
    , ( "Alpha.Beta.Gamma.Delta.value", "Expression/Identifier" )
    , ( "value : Alpha.Beta.Gamma.Type\nvalue = Alpha.Beta.Gamma.value", "Declaration/FunctionDeclaration" )
    , ( "case value of\n    Alpha.Beta.Gamma.Just x -> x", "Expression/CaseExpression" )
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

        Declaration.PortDeclaration _ _ ->
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
            , token Token.Identifier "b" 2 1 2 2
            , token Token.Identifier "c" 3 1 3 2
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
        |> (\successfulTests ->
                successfulTests
                    ++ ([ ( "empty Unicode escape", "\"\\u{}\"" )
                        , ( "surrogate Unicode escape", "\"\\u{D800}\"" )
                        , ( "out-of-range Unicode escape", "\"\\u{110000}\"" )
                        ]
                            |> List.map
                                (\( description, input ) ->
                                    Test.test description <|
                                        \_ ->
                                            case TokensFromString.parseExpression input of
                                                Ok actual ->
                                                    Expect.fail ("Expected Err, but got Ok: " ++ Debug.toString actual)

                                                Err _ ->
                                                    Expect.pass
                                )
                       )
           )


threeLineIdentifierTokens : List Token.Token
threeLineIdentifierTokens =
    [ token Token.Identifier "alpha" 1 1 1 6
    , token Token.Identifier "beta" 2 1 2 5
    , token Token.Identifier "gamma" 3 1 3 6
    ]


parserSpecificTokenizationSuite : List Test
parserSpecificTokenizationSuite =
    [ Test.test "tokenization omits whitespace and newlines while retaining positions" <|
        \_ ->
            case TokensFromString.parseExpression "a \nb" of
                Ok actual ->
                    Expect.equal
                        [ token Token.Identifier "a" 1 1 1 2
                        , token Token.Identifier "b" 2 1 2 2
                        ]
                        actual

                Err err ->
                    Expect.fail ("Expected Ok, but got Err: " ++ err)
    , Test.test "omits whitespace and newlines while retaining comments" <|
        \_ ->
            case TokensFromString.parseExpression "alpha \n-- note\nbeta" of
                Ok actual ->
                    Expect.equal
                        [ token Token.Identifier "alpha" 1 1 1 6
                        , token Token.Comment "-- note" 2 1 2 8
                        , token Token.Identifier "beta" 3 1 3 5
                        ]
                        actual

                Err err ->
                    Expect.fail ("Expected Ok, but got Err: " ++ err)
    , Test.test "whitespace before minus preserves negation" <|
        \_ ->
            case ElmSyntax.Concrete.Parser.FromString.parseExpression "x -y" of
                Ok actual ->
                    Expect.equal
                        [ "Application", "Identifier", "Negation", "Identifier" ]
                        (expressionKinds actual)

                Err err ->
                    Expect.fail ("Expected Ok, but got Err: " ++ err)
    , Test.test "adjacent minus remains an operator" <|
        \_ ->
            case ElmSyntax.Concrete.Parser.FromString.parseExpression "x-y" of
                Ok actual ->
                    Expect.equal "OperatorApplication" (expressionKind actual)

                Err err ->
                    Expect.fail ("Expected Ok, but got Err: " ++ err)
    ]


{-| Covers literal decoding/raw-text preservation (escapes, unicode escapes, triple-quoted
strings spanning mixed line breaks) and multi-line comments with a line break inside, all as
single-token expectations exercising the offset/String-based scanners directly.
-}
literalAndCommentTokenSuite : List Test
literalAndCommentTokenSuite =
    let
        repeatedEscapeCount =
            1000

        repeatedEscapeRaw =
            String.repeat repeatedEscapeCount "a\\nb\\t"

        repeatedEscapeInput =
            "\"" ++ repeatedEscapeRaw ++ "\""

        repeatedCommentContent =
            String.repeat 1000 "{-left-}right"

        repeatedCommentInput =
            "{-" ++ repeatedCommentContent ++ "-}"
    in
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
    , { description = "long string literal preserves decoded and raw chunk order"
      , input = repeatedEscapeInput
      , expectedTokens =
            [ tokenWithRaw
                Token.StringLiteral
                (String.repeat repeatedEscapeCount "a\nb\t")
                repeatedEscapeRaw
                1
                1
                1
                (String.length repeatedEscapeInput + 1)
            ]
      }
    , { description = "long nested comment preserves chunk order"
      , input = repeatedCommentInput
      , expectedTokens =
            [ token
                Token.Comment
                repeatedCommentInput
                1
                1
                1
                (String.length repeatedCommentInput + 1)
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
                    ++ [ Test.test "covers the GLSL expression variant" <|
                            \_ ->
                                Expect.equal "GLSLExpression"
                                    (expressionKind (Expression.GLSLExpression "void main() {}"))
                       , Test.test "nested expressions retain every nested node" <|
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
            let
                input =
                    String.trim
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

                expectedKinds =
                    [ "AllPattern"
                    , "UnitPattern"
                    , "CharPattern"
                    , "StringPattern"
                    , "IntPattern"
                    , "HexPattern"
                    , "FloatPattern"
                    , "TuplePattern"
                    , "VarPattern"
                    , "VarPattern"
                    , "RecordPattern"
                    , "UnConsPattern"
                    , "VarPattern"
                    , "VarPattern"
                    , "ListPattern"
                    , "VarPattern"
                    , "VarPattern"
                    , "NamedPattern"
                    , "VarPattern"
                    , "AsPattern"
                    , "ParenthesizedPattern"
                    , "NamedPattern"
                    , "VarPattern"
                    ]
            in
            case ElmSyntax.Concrete.Parser.FromString.parseExpression input of
                Ok (Expression.CaseExpression caseBlock) ->
                    Expect.equal expectedKinds
                        (List.concatMap (.pattern >> Node.value >> patternKinds) caseBlock.cases)

                Ok actual ->
                    Expect.fail ("Expected a case expression, but got: " ++ Debug.toString actual)

                Err err ->
                    Expect.fail ("Expected Ok, but got Err: " ++ err)
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


patternKinds : Pattern.Pattern -> List String
patternKinds pattern =
    patternKind pattern
        :: (case pattern of
                Pattern.TuplePattern elements ->
                    separatedPatternKinds elements

                Pattern.UnConsPattern head _ tail ->
                    nodePatternKinds head ++ nodePatternKinds tail

                Pattern.ListPattern elements ->
                    separatedPatternKinds elements

                Pattern.NamedPattern _ arguments ->
                    List.concatMap nodePatternKinds arguments

                Pattern.AsPattern nested _ _ ->
                    nodePatternKinds nested

                Pattern.ParenthesizedPattern nested ->
                    nodePatternKinds nested

                _ ->
                    []
           )


patternKind : Pattern.Pattern -> String
patternKind pattern =
    case pattern of
        Pattern.AllPattern ->
            "AllPattern"

        Pattern.VarPattern _ ->
            "VarPattern"

        Pattern.UnitPattern ->
            "UnitPattern"

        Pattern.CharPattern _ ->
            "CharPattern"

        Pattern.StringPattern _ ->
            "StringPattern"

        Pattern.IntPattern _ ->
            "IntPattern"

        Pattern.HexPattern _ ->
            "HexPattern"

        Pattern.FloatPattern _ ->
            "FloatPattern"

        Pattern.TuplePattern _ ->
            "TuplePattern"

        Pattern.RecordPattern _ ->
            "RecordPattern"

        Pattern.UnConsPattern _ _ _ ->
            "UnConsPattern"

        Pattern.ListPattern _ ->
            "ListPattern"

        Pattern.NamedPattern _ _ ->
            "NamedPattern"

        Pattern.AsPattern _ _ _ ->
            "AsPattern"

        Pattern.ParenthesizedPattern _ ->
            "ParenthesizedPattern"


nodePatternKinds : Node Pattern.Pattern -> List String
nodePatternKinds (Node _ pattern) =
    patternKinds pattern


separatedPatternKinds : SeparatedSyntaxList.SeparatedSyntaxList (Node Pattern.Pattern) -> List String
separatedPatternKinds separated =
    case separated of
        SeparatedSyntaxList.Empty ->
            []

        SeparatedSyntaxList.NonEmpty first rest ->
            nodePatternKinds first
                ++ List.concatMap (Tuple.second >> nodePatternKinds) rest


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
