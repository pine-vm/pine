module ConvertConcreteToAbstractTests exposing (suite)

import ElmSyntax.Abstract.ConvertFromConcrete as Convert
import ElmSyntax.Abstract.Declaration as AbstractDeclaration
import ElmSyntax.Abstract.Exposing as AbstractExposing
import ElmSyntax.Abstract.Expression as AbstractExpression
import ElmSyntax.Abstract.Infix as AbstractInfix
import ElmSyntax.Abstract.Module as AbstractModule
import ElmSyntax.Abstract.Pattern as AbstractPattern
import ElmSyntax.Abstract.TypeAnnotation as AbstractTypeAnnotation
import ElmSyntax.Concrete.Declaration as ConcreteDeclaration
import ElmSyntax.Concrete.Exposing as ConcreteExposing
import ElmSyntax.Concrete.Expression as ConcreteExpression
import ElmSyntax.Concrete.File as ConcreteFile
import ElmSyntax.Concrete.Import as ConcreteImport
import ElmSyntax.Concrete.Infix as ConcreteInfix
import ElmSyntax.Concrete.Module as ConcreteModule
import ElmSyntax.Concrete.Node as Node exposing (Node)
import ElmSyntax.Concrete.Parser.FromString
import ElmSyntax.Concrete.Pattern as ConcretePattern
import ElmSyntax.Concrete.Range exposing (Location, Range)
import ElmSyntax.Concrete.SeparatedSyntaxList as SeparatedSyntaxList
import ElmSyntax.Concrete.TypeAnnotation as ConcreteTypeAnnotation
import Expect
import Test exposing (Test)


suite : Test
suite =
    Test.describe "convert concrete syntax to abstract syntax"
        [ expressionSuite
        , patternSuite
        , typeAnnotationSuite
        , moduleAndImportSuite
        , declarationSuite
        , fileSuite
        ]


expressionSuite : Test
expressionSuite =
    Test.describe "expressions"
        [ Test.test "normalizes literals and removes parentheses" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        expectParsedExpression "((0xFF))"
                            (AbstractExpression.IntegerLiteral 255)
                    , \_ ->
                        Expect.equal
                            (AbstractExpression.IntegerLiteral -26)
                            (Convert.fromExpression
                                (ConcreteExpression.IntegerLiteral "-0X1a")
                            )
                    ]
                    ()
        , Test.test "normalizes floating-point literals" <|
            \_ ->
                expectParsedExpression "3.5e2"
                    (AbstractExpression.FloatLiteral 350)
        , Test.test "unifies multiline and regular strings" <|
            \_ ->
                expectParsedExpression "\"\"\"hello\nworld\"\"\""
                    (AbstractExpression.StringLiteral "hello\nworld")
        , Test.test "sorts record fields and strips record-access dots" <|
            \_ ->
                Expect.all
                    [ \_ ->
                        expectParsedExpression "{ z = 1, a = 2 }"
                            (AbstractExpression.RecordExpr
                                [ { fieldName = "a", value = AbstractExpression.IntegerLiteral 2 }
                                , { fieldName = "z", value = AbstractExpression.IntegerLiteral 1 }
                                ]
                            )
                    , \_ ->
                        expectParsedExpression ".field"
                            (AbstractExpression.RecordAccessFunction "field")
                    ]
                    ()
        , Test.test "converts every concrete expression variant" <|
            \_ ->
                let
                    parsedCases =
                        [ ( "()", "UnitExpr" )
                        , ( "\"text\"", "StringLiteral" )
                        , ( "'x'", "CharLiteral" )
                        , ( "42", "IntegerLiteral" )
                        , ( "1.5", "FloatLiteral" )
                        , ( "-value", "Negation" )
                        , ( "[ 1, 2 ]", "ListExpr" )
                        , ( "Module.value", "Identifier" )
                        , ( "if condition then 1 else 2", "IfBlock" )
                        , ( "(+)", "PrefixOperator" )
                        , ( "f x y", "Application" )
                        , ( "1 + 2", "OperatorApplication" )
                        , ( "( 1, 2 )", "TupledExpression" )
                        , ( "\\x -> x", "LambdaExpression" )
                        , ( "case value of\n    Just x -> x", "CaseExpression" )
                        , ( "let\n    x = 1\nin\nx", "LetExpression" )
                        , ( "{ x = 1 }", "RecordExpr" )
                        , ( "model.field", "RecordAccess" )
                        , ( ".field", "RecordAccessFunction" )
                        , ( "{ model | x = 1 }", "RecordUpdateExpression" )
                        ]

                    actualKinds =
                        parsedCases
                            |> List.map
                                (\( source, _ ) ->
                                    ElmSyntax.Concrete.Parser.FromString.parseExpression source
                                        |> Result.map (Convert.fromExpression >> expressionKind)
                                )

                    expectedKinds =
                        List.map (Tuple.second >> Ok) parsedCases
                in
                Expect.equal
                    (expectedKinds ++ [ Ok "GLSLExpression" ])
                    (actualKinds
                        ++ [ Ok
                                (Convert.fromExpression (ConcreteExpression.GLSLExpression "void main() {}")
                                    |> expressionKind
                                )
                           ]
                    )
        ]


patternSuite : Test
patternSuite =
    Test.test "patterns normalize hex and parentheses and convert every variant" <|
        \_ ->
            let
                concretePatterns =
                    [ ConcretePattern.AllPattern
                    , ConcretePattern.VarPattern "value"
                    , ConcretePattern.UnitPattern
                    , ConcretePattern.CharPattern 97
                    , ConcretePattern.StringPattern "text"
                    , ConcretePattern.IntPattern 12
                    , ConcretePattern.HexPattern 255
                    , ConcretePattern.FloatPattern 1.5
                    , ConcretePattern.TuplePattern
                        (separated [ node (ConcretePattern.VarPattern "a"), node (ConcretePattern.VarPattern "b") ])
                    , ConcretePattern.RecordPattern (separated [ node "a", node "b" ])
                    , ConcretePattern.UnConsPattern
                        (node (ConcretePattern.VarPattern "head"))
                        location
                        (node (ConcretePattern.VarPattern "tail"))
                    , ConcretePattern.ListPattern (separated [ node ConcretePattern.UnitPattern ])
                    , ConcretePattern.NamedPattern
                        { moduleName = [ "Maybe" ], name = "Just" }
                        [ node (ConcretePattern.VarPattern "value") ]
                    , ConcretePattern.AsPattern
                        (node (ConcretePattern.VarPattern "value"))
                        location
                        (node "whole")
                    , ConcretePattern.ParenthesizedPattern
                        (node (ConcretePattern.HexPattern 16))
                    ]

                expected =
                    [ AbstractPattern.AllPattern
                    , AbstractPattern.VarPattern "value"
                    , AbstractPattern.UnitPattern
                    , AbstractPattern.CharPattern 97
                    , AbstractPattern.StringPattern "text"
                    , AbstractPattern.IntPattern 12
                    , AbstractPattern.IntPattern 255
                    , AbstractPattern.FloatPattern 1.5
                    , AbstractPattern.TuplePattern
                        [ AbstractPattern.VarPattern "a", AbstractPattern.VarPattern "b" ]
                    , AbstractPattern.RecordPattern [ "a", "b" ]
                    , AbstractPattern.UnConsPattern
                        (AbstractPattern.VarPattern "head")
                        (AbstractPattern.VarPattern "tail")
                    , AbstractPattern.ListPattern [ AbstractPattern.UnitPattern ]
                    , AbstractPattern.NamedPattern
                        { moduleName = [ "Maybe" ], name = "Just" }
                        [ AbstractPattern.VarPattern "value" ]
                    , AbstractPattern.AsPattern
                        (AbstractPattern.VarPattern "value")
                        "whole"
                    , AbstractPattern.IntPattern 16
                    ]
            in
            Expect.equal expected (List.map Convert.fromPattern concretePatterns)


typeAnnotationSuite : Test
typeAnnotationSuite =
    Test.test "type annotations convert every variant and discard syntax locations" <|
        \_ ->
            let
                concrete =
                    [ ConcreteTypeAnnotation.GenericType "a"
                    , ConcreteTypeAnnotation.Typed
                        (node ( [ "Maybe" ], "Maybe" ))
                        [ node (ConcreteTypeAnnotation.GenericType "a") ]
                    , ConcreteTypeAnnotation.Unit
                    , ConcreteTypeAnnotation.Tupled
                        (separated [ node ConcreteTypeAnnotation.Unit, node (ConcreteTypeAnnotation.GenericType "a") ])
                    , ConcreteTypeAnnotation.Record
                        (separated
                            [ node
                                { fieldName = node "name"
                                , colonLocation = location
                                , fieldType = node (ConcreteTypeAnnotation.Typed (node ( [], "String" )) [])
                                }
                            ]
                        )
                    , ConcreteTypeAnnotation.GenericRecord
                        (node "record")
                        location
                        (node SeparatedSyntaxList.Empty)
                    , ConcreteTypeAnnotation.FunctionTypeAnnotation
                        (node (ConcreteTypeAnnotation.GenericType "a"))
                        location
                        (node (ConcreteTypeAnnotation.GenericType "b"))
                    ]

                expected =
                    [ AbstractTypeAnnotation.GenericType "a"
                    , AbstractTypeAnnotation.Typed
                        [ "Maybe" ]
                        "Maybe"
                        [ AbstractTypeAnnotation.GenericType "a" ]
                    , AbstractTypeAnnotation.Unit
                    , AbstractTypeAnnotation.Tupled
                        [ AbstractTypeAnnotation.Unit, AbstractTypeAnnotation.GenericType "a" ]
                    , AbstractTypeAnnotation.Record
                        [ { fieldName = "name"
                          , fieldType = AbstractTypeAnnotation.Typed [] "String" []
                          }
                        ]
                    , AbstractTypeAnnotation.GenericRecord "record" []
                    , AbstractTypeAnnotation.FunctionTypeAnnotation
                        (AbstractTypeAnnotation.GenericType "a")
                        (AbstractTypeAnnotation.GenericType "b")
                    ]
            in
            Expect.equal expected (List.map Convert.fromTypeAnnotation concrete)


moduleAndImportSuite : Test
moduleAndImportSuite =
    Test.describe "modules, exposing lists, and imports"
        [ Test.test "converts every module kind" <|
            \_ ->
                let
                    defaultData =
                        { moduleName = node [ "Example" ]
                        , exposingList = node (ConcreteExposing.All range)
                        }

                    effectData =
                        { moduleName = node [ "Effect" ]
                        , exposingList = node (ConcreteExposing.Explicit location SeparatedSyntaxList.Empty location)
                        , command = Just (node "Cmd")
                        , subscription = Just (node "Sub")
                        }
                in
                Expect.equal
                    [ AbstractModule.NormalModule
                        { moduleName = [ "Example" ], exposingList = AbstractExposing.All }
                    , AbstractModule.PortModule
                        { moduleName = [ "Example" ], exposingList = AbstractExposing.All }
                    , AbstractModule.EffectModule
                        { moduleName = [ "Effect" ]
                        , exposingList = AbstractExposing.Explicit []
                        , command = Just "Cmd"
                        , subscription = Just "Sub"
                        }
                    ]
                    (List.map Convert.fromModule
                        [ ConcreteModule.NormalModule defaultData
                        , ConcreteModule.PortModule defaultData
                        , ConcreteModule.EffectModule effectData
                        ]
                    )
        , Test.test "converts explicit exposing entries" <|
            \_ ->
                Expect.equal
                    (AbstractExposing.Explicit
                        [ AbstractExposing.InfixExpose "+"
                        , AbstractExposing.FunctionExpose "map"
                        , AbstractExposing.TypeOrAliasExpose "Model"
                        , AbstractExposing.TypeExpose
                            { name = "Maybe", exposesConstructors = True }
                        ]
                    )
                    (Convert.fromExposing
                        (ConcreteExposing.Explicit location
                            (separated
                                [ node (ConcreteExposing.InfixExpose "+")
                                , node (ConcreteExposing.FunctionExpose "map")
                                , node (ConcreteExposing.TypeOrAliasExpose "Model")
                                , node
                                    (ConcreteExposing.TypeExpose
                                        { name = "Maybe", open = Just range }
                                    )
                                ]
                            )
                            location
                        )
                    )
        , Test.test "converts import aliases and exposing lists" <|
            \_ ->
                let
                    concrete : ConcreteImport.Import
                    concrete =
                        { importTokenLocation = location
                        , moduleName = node [ "Json", "Decode" ]
                        , moduleAlias = Just ( location, node [ "Decode" ] )
                        , exposingList = Just ( location, node (ConcreteExposing.All range) )
                        }
                in
                Expect.equal
                    { moduleName = [ "Json", "Decode" ]
                    , moduleAlias = Just [ "Decode" ]
                    , exposingList = Just AbstractExposing.All
                    }
                    (Convert.fromImport concrete)
        ]


declarationSuite : Test
declarationSuite =
    Test.test "converts every declaration variant and drops documentation" <|
        \_ ->
            let
                signature =
                    { name = node "value"
                    , colonLocation = location
                    , typeAnnotation = node ConcreteTypeAnnotation.Unit
                    }

                function =
                    { documentation = Just (node "docs")
                    , signature = Just (node signature)
                    , declaration =
                        node
                            { name = node "value"
                            , arguments = []
                            , equalsTokenLocation = location
                            , expression = node ConcreteExpression.UnitExpr
                            }
                    }

                declarations =
                    [ ConcreteDeclaration.FunctionDeclaration function
                    , ConcreteDeclaration.ChoiceTypeDeclaration
                        { documentation = Just (node "docs")
                        , typeTokenLocation = location
                        , name = node "Choice"
                        , generics = [ node "a" ]
                        , equalsTokenLocation = location
                        , constructors =
                            separated
                                [ node
                                    { name = node "Choice"
                                    , arguments = [ node (ConcreteTypeAnnotation.GenericType "a") ]
                                    }
                                ]
                        }
                    , ConcreteDeclaration.AliasDeclaration
                        { documentation = Just (node "docs")
                        , typeTokenLocation = location
                        , aliasTokenLocation = location
                        , name = node "Alias"
                        , generics = []
                        , equalsTokenLocation = location
                        , typeAnnotation = node ConcreteTypeAnnotation.Unit
                        }
                    , ConcreteDeclaration.PortDeclaration location signature
                    , ConcreteDeclaration.InfixDeclaration
                        { infixTokenLocation = location
                        , direction = node ConcreteInfix.Left
                        , precedence = node 5
                        , operator = node "++"
                        , equalsTokenLocation = location
                        , function = node "append"
                        }
                    ]
            in
            Expect.equal
                [ AbstractDeclaration.FunctionDeclaration
                    { signature =
                        Just
                            { name = "value"
                            , typeAnnotation = AbstractTypeAnnotation.Unit
                            }
                    , declaration =
                        { name = "value"
                        , arguments = []
                        , expression = AbstractExpression.UnitExpr
                        }
                    }
                , AbstractDeclaration.ChoiceTypeDeclaration
                    { name = "Choice"
                    , generics = [ "a" ]
                    , constructors =
                        [ { name = "Choice"
                          , arguments = [ AbstractTypeAnnotation.GenericType "a" ]
                          }
                        ]
                    }
                , AbstractDeclaration.AliasDeclaration
                    { name = "Alias"
                    , generics = []
                    , typeAnnotation = AbstractTypeAnnotation.Unit
                    }
                , AbstractDeclaration.PortDeclaration
                    { name = "value", typeAnnotation = AbstractTypeAnnotation.Unit }
                , AbstractDeclaration.InfixDeclaration
                    { direction = AbstractInfix.Left
                    , precedence = 5
                    , operator = "++"
                    , functionName = "append"
                    }
                ]
                (List.map Convert.fromDeclaration declarations)


fileSuite : Test
fileSuite =
    Test.test "file conversion composes children and drops comments and incomplete declarations" <|
        \_ ->
            let
                concrete : ConcreteFile.File
                concrete =
                    { moduleDefinition =
                        node
                            (ConcreteModule.NormalModule
                                { moduleName = node [ "Main" ]
                                , exposingList = node (ConcreteExposing.All range)
                                }
                            )
                    , imports = []
                    , declarations = []
                    , comments = [ node "ignored" ]
                    , incompleteDeclarations =
                        [ node
                            { originalText = "?"
                            , parseError = { message = "ignored", location = location }
                            }
                        ]
                    }
            in
            Expect.equal
                { moduleDefinition =
                    AbstractModule.NormalModule
                        { moduleName = [ "Main" ], exposingList = AbstractExposing.All }
                , imports = []
                , declarations = []
                }
                (Convert.fromFile concrete)


expectParsedExpression : String -> AbstractExpression.Expression -> Expect.Expectation
expectParsedExpression source expected =
    case ElmSyntax.Concrete.Parser.FromString.parseExpression source of
        Ok concrete ->
            Expect.equal expected (Convert.fromExpression concrete)

        Err error ->
            Expect.fail error


expressionKind : AbstractExpression.Expression -> String
expressionKind expression =
    case expression of
        AbstractExpression.UnitExpr ->
            "UnitExpr"

        AbstractExpression.StringLiteral _ ->
            "StringLiteral"

        AbstractExpression.CharLiteral _ ->
            "CharLiteral"

        AbstractExpression.IntegerLiteral _ ->
            "IntegerLiteral"

        AbstractExpression.FloatLiteral _ ->
            "FloatLiteral"

        AbstractExpression.Negation _ ->
            "Negation"

        AbstractExpression.ListExpr _ ->
            "ListExpr"

        AbstractExpression.Identifier _ _ ->
            "Identifier"

        AbstractExpression.IfBlock _ _ _ ->
            "IfBlock"

        AbstractExpression.PrefixOperator _ ->
            "PrefixOperator"

        AbstractExpression.Application _ _ ->
            "Application"

        AbstractExpression.OperatorApplication _ _ _ _ ->
            "OperatorApplication"

        AbstractExpression.TupledExpression _ ->
            "TupledExpression"

        AbstractExpression.LambdaExpression _ _ ->
            "LambdaExpression"

        AbstractExpression.CaseExpression _ _ ->
            "CaseExpression"

        AbstractExpression.LetExpression _ _ ->
            "LetExpression"

        AbstractExpression.RecordExpr _ ->
            "RecordExpr"

        AbstractExpression.RecordAccess _ _ ->
            "RecordAccess"

        AbstractExpression.RecordAccessFunction _ ->
            "RecordAccessFunction"

        AbstractExpression.RecordUpdateExpression _ _ ->
            "RecordUpdateExpression"

        AbstractExpression.GLSLExpression _ ->
            "GLSLExpression"


node : a -> Node a
node =
    Node.empty


separated : List a -> SeparatedSyntaxList.SeparatedSyntaxList a
separated items =
    case items of
        [] ->
            SeparatedSyntaxList.Empty

        first :: rest ->
            SeparatedSyntaxList.NonEmpty first (List.map (\item -> ( location, item )) rest)


location : Location
location =
    { row = 1, column = 1 }


range : Range
range =
    { start = location, end = location }
