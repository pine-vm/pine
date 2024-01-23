module EncodeElmSyntaxAsPineValue exposing (..)

import Elm.Syntax.Declaration
import Elm.Syntax.Exposing
import Elm.Syntax.Expression
import Elm.Syntax.File
import Elm.Syntax.Import
import Elm.Syntax.Infix
import Elm.Syntax.Module
import Elm.Syntax.Node
import Elm.Syntax.Pattern
import Elm.Syntax.Range
import Elm.Syntax.Signature
import Elm.Syntax.Type
import Elm.Syntax.TypeAlias
import Elm.Syntax.TypeAnnotation
import ElmCompiler
import Pine
import Result.Extra


encodeElmSyntaxFileAsPineValue : Elm.Syntax.File.File -> Result String Pine.Value
encodeElmSyntaxFileAsPineValue elmSyntaxFile =
    case
        encodeElmSyntaxNodeAsPineValueResult
            encodeElmSyntaxModuleDefinitionAsPineValue
            elmSyntaxFile.moduleDefinition
    of
        Err err ->
            Err ("Failed to encode the module definition as Pine value: " ++ err)

        Ok moduleDefinitionValue ->
            elmSyntaxFile.declarations
                |> List.map (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxDeclarationAsPineValue)
                |> Result.Extra.combine
                |> Result.map
                    (\declarationsValues ->
                        encodeRecordAsPineValue
                            [ ( "moduleDefinition", moduleDefinitionValue )
                            , ( "imports"
                              , Pine.ListValue
                                    (List.map (encodeElmSyntaxNodeAsPineValue encodeElmSyntaxImportAsPineValue)
                                        elmSyntaxFile.imports
                                    )
                              )
                            , ( "declarations"
                              , Pine.ListValue declarationsValues
                              )
                            , ( "comments"
                              , Pine.ListValue []
                              )
                            ]
                    )


encodeElmSyntaxModuleDefinitionAsPineValue : Elm.Syntax.Module.Module -> Result String Pine.Value
encodeElmSyntaxModuleDefinitionAsPineValue elmSyntaxModuleDefinition =
    case elmSyntaxModuleDefinition of
        Elm.Syntax.Module.NormalModule normal ->
            Ok
                (encodeTagAsPineValue
                    "NormalModule"
                    [ encodeElmSyntaxDefaultModuleDataAsPineValue normal
                    ]
                )

        Elm.Syntax.Module.PortModule _ ->
            Err "PortModule is not implemented"

        Elm.Syntax.Module.EffectModule _ ->
            Err "EffectModule is not implemented"


encodeElmSyntaxDeclarationAsPineValue : Elm.Syntax.Declaration.Declaration -> Result String Pine.Value
encodeElmSyntaxDeclarationAsPineValue elmSyntaxDeclaration =
    case elmSyntaxDeclaration of
        Elm.Syntax.Declaration.FunctionDeclaration functionDeclaration ->
            encodeElmSyntaxFunctionExpressionAsPineValue functionDeclaration
                |> Result.map
                    (\functionDeclarationValue ->
                        encodeTagAsPineValue
                            "FunctionDeclaration"
                            [ functionDeclarationValue ]
                    )

        Elm.Syntax.Declaration.AliasDeclaration typeAlias ->
            encodeElmSyntaxTypeAliasAsPineValue typeAlias
                |> Result.map
                    (\typeAliasValue ->
                        encodeTagAsPineValue
                            "AliasDeclaration"
                            [ typeAliasValue ]
                    )

        Elm.Syntax.Declaration.CustomTypeDeclaration choiceType ->
            Ok
                (encodeTagAsPineValue
                    "CustomTypeDeclaration"
                    [ encodeElmSyntaxTypeAsPineValue choiceType ]
                )

        Elm.Syntax.Declaration.PortDeclaration _ ->
            Err "PortDeclaration is not implemented"

        Elm.Syntax.Declaration.InfixDeclaration infixDeclaration ->
            Ok
                (encodeTagAsPineValue
                    "InfixDeclaration"
                    [ encodeElmSyntaxInfixAsPineValue infixDeclaration ]
                )

        Elm.Syntax.Declaration.Destructuring _ _ ->
            Err "Destructuring is not implemented"


encodeElmSyntaxTypeAsPineValue : Elm.Syntax.Type.Type -> Pine.Value
encodeElmSyntaxTypeAsPineValue elmSyntaxType =
    encodeRecordAsPineValue
        [ ( "documentation"
          , encodeMaybeAsPineValue
                (encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue)
                elmSyntaxType.documentation
          )
        , ( "name"
          , encodeElmSyntaxNodeAsPineValue
                encodeElmStringAsPineValue
                elmSyntaxType.name
          )
        , ( "generics"
          , Pine.ListValue
                (List.map
                    (encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue)
                    elmSyntaxType.generics
                )
          )
        , ( "constructors"
          , Pine.ListValue
                (List.map (encodeElmSyntaxNodeAsPineValue encodeElmSyntaxValueConstructorAsPineValue)
                    elmSyntaxType.constructors
                )
          )
        ]


encodeElmSyntaxValueConstructorAsPineValue : Elm.Syntax.Type.ValueConstructor -> Pine.Value
encodeElmSyntaxValueConstructorAsPineValue elmSyntaxValueConstructor =
    encodeRecordAsPineValue
        [ ( "name"
          , encodeElmSyntaxNodeAsPineValue
                encodeElmStringAsPineValue
                elmSyntaxValueConstructor.name
          )
        , ( "arguments"
          , Pine.ListValue
                (List.map (encodeElmSyntaxNodeAsPineValue encodeElmSyntaxTypeAnnotationAsPineValue)
                    elmSyntaxValueConstructor.arguments
                )
          )
        ]


encodeElmSyntaxTypeAliasAsPineValue : Elm.Syntax.TypeAlias.TypeAlias -> Result String Pine.Value
encodeElmSyntaxTypeAliasAsPineValue elmSyntaxTypeAlias =
    Ok
        (encodeRecordAsPineValue
            [ ( "documentation"
              , encodeMaybeAsPineValue
                    (encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue)
                    elmSyntaxTypeAlias.documentation
              )
            , ( "name"
              , encodeElmSyntaxNodeAsPineValue
                    encodeElmStringAsPineValue
                    elmSyntaxTypeAlias.name
              )
            , ( "generics"
              , Pine.ListValue
                    (List.map
                        (encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue)
                        elmSyntaxTypeAlias.generics
                    )
              )
            , ( "typeAnnotation"
              , encodeElmSyntaxNodeAsPineValue
                    encodeElmSyntaxTypeAnnotationAsPineValue
                    elmSyntaxTypeAlias.typeAnnotation
              )
            ]
        )


encodeElmSyntaxFunctionExpressionAsPineValue : Elm.Syntax.Expression.Function -> Result String Pine.Value
encodeElmSyntaxFunctionExpressionAsPineValue elmSyntaxFunctionExpression =
    encodeElmSyntaxNodeAsPineValueResult
        encodeElmSyntaxFunctionImplementationAsPineValue
        elmSyntaxFunctionExpression.declaration
        |> Result.map
            (\functionImplementationValue ->
                encodeRecordAsPineValue
                    [ ( "documentation"
                      , encodeMaybeAsPineValue
                            (encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue)
                            elmSyntaxFunctionExpression.documentation
                      )
                    , ( "signature"
                      , encodeMaybeAsPineValue
                            (encodeElmSyntaxNodeAsPineValue encodeElmSyntaxSignatureAsPineValue)
                            elmSyntaxFunctionExpression.signature
                      )
                    , ( "declaration"
                      , functionImplementationValue
                      )
                    ]
            )


encodeElmSyntaxFunctionImplementationAsPineValue : Elm.Syntax.Expression.FunctionImplementation -> Result String Pine.Value
encodeElmSyntaxFunctionImplementationAsPineValue elmSyntaxFunctionImplementation =
    List.map
        (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxPatternAsPineValue)
        elmSyntaxFunctionImplementation.arguments
        |> Result.Extra.combine
        |> Result.andThen
            (\argumentsValues ->
                encodeElmSyntaxNodeAsPineValueResult
                    encodeElmSyntaxExpressionAsPineValue
                    elmSyntaxFunctionImplementation.expression
                    |> Result.map
                        (\expressionValue ->
                            encodeRecordAsPineValue
                                [ ( "name"
                                  , encodeElmSyntaxNodeAsPineValue
                                        encodeElmStringAsPineValue
                                        elmSyntaxFunctionImplementation.name
                                  )
                                , ( "arguments"
                                  , Pine.ListValue argumentsValues
                                  )
                                , ( "expression"
                                  , expressionValue
                                  )
                                ]
                        )
            )


encodeElmSyntaxExpressionAsPineValue : Elm.Syntax.Expression.Expression -> Result String Pine.Value
encodeElmSyntaxExpressionAsPineValue expression =
    case expression of
        Elm.Syntax.Expression.UnitExpr ->
            Ok (encodeTagAsPineValue "UnitExpr" [])

        Elm.Syntax.Expression.Literal string ->
            Ok (encodeTagAsPineValue "Literal" [ encodeElmStringAsPineValue string ])

        Elm.Syntax.Expression.ListExpr expressions ->
            expressions
                |> List.map (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxExpressionAsPineValue)
                |> Result.Extra.combine
                |> Result.map
                    (\expressionsValues ->
                        encodeTagAsPineValue
                            "ListExpr"
                            [ Pine.ListValue expressionsValues ]
                    )

        Elm.Syntax.Expression.Application expressions ->
            expressions
                |> List.map (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxExpressionAsPineValue)
                |> Result.Extra.combine
                |> Result.map
                    (\expressionsValues ->
                        encodeTagAsPineValue
                            "Application"
                            [ Pine.ListValue expressionsValues ]
                    )

        Elm.Syntax.Expression.OperatorApplication string infixDirection leftExpression rightExpression ->
            encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxExpressionAsPineValue leftExpression
                |> Result.andThen
                    (\leftExpressionValue ->
                        encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxExpressionAsPineValue rightExpression
                            |> Result.map
                                (\rightExpressionValue ->
                                    encodeTagAsPineValue
                                        "OperatorApplication"
                                        [ encodeElmStringAsPineValue string
                                        , encodeElmSyntaxInfixDirectionAsPineValue infixDirection
                                        , leftExpressionValue
                                        , rightExpressionValue
                                        ]
                                )
                    )

        Elm.Syntax.Expression.FunctionOrValue moduleName string ->
            Ok
                (encodeTagAsPineValue
                    "FunctionOrValue"
                    [ (List.map encodeElmStringAsPineValue >> Pine.ListValue)
                        moduleName
                    , encodeElmStringAsPineValue string
                    ]
                )

        Elm.Syntax.Expression.IfBlock conditionExpression thenExpression elseExpression ->
            encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxExpressionAsPineValue conditionExpression
                |> Result.andThen
                    (\conditionExpressionValue ->
                        encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxExpressionAsPineValue thenExpression
                            |> Result.andThen
                                (\thenExpressionValue ->
                                    encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxExpressionAsPineValue elseExpression
                                        |> Result.map
                                            (\elseExpressionValue ->
                                                encodeTagAsPineValue
                                                    "IfBlock"
                                                    [ conditionExpressionValue
                                                    , thenExpressionValue
                                                    , elseExpressionValue
                                                    ]
                                            )
                                )
                    )

        Elm.Syntax.Expression.PrefixOperator string ->
            Ok (encodeTagAsPineValue "PrefixOperator" [ encodeElmStringAsPineValue string ])

        Elm.Syntax.Expression.Operator string ->
            Ok (encodeTagAsPineValue "Operator" [ encodeElmStringAsPineValue string ])

        Elm.Syntax.Expression.Integer int ->
            Ok (encodeTagAsPineValue "Integer" [ Pine.valueFromInt int ])

        Elm.Syntax.Expression.Hex int ->
            Ok (encodeTagAsPineValue "Hex" [ Pine.valueFromInt int ])

        Elm.Syntax.Expression.Negation expressionNode ->
            encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxExpressionAsPineValue expressionNode
                |> Result.map
                    (\expressionValue ->
                        encodeTagAsPineValue
                            "Negation"
                            [ expressionValue ]
                    )

        Elm.Syntax.Expression.CharLiteral char ->
            Ok (encodeTagAsPineValue "CharLiteral" [ Pine.valueFromChar char ])

        Elm.Syntax.Expression.TupledExpression expressions ->
            expressions
                |> List.map (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxExpressionAsPineValue)
                |> Result.Extra.combine
                |> Result.map
                    (\expressionsValues ->
                        encodeTagAsPineValue
                            "TupledExpression"
                            [ Pine.ListValue expressionsValues ]
                    )

        Elm.Syntax.Expression.ParenthesizedExpression expressionNode ->
            encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxExpressionAsPineValue expressionNode
                |> Result.map
                    (\expressionValue ->
                        encodeTagAsPineValue
                            "ParenthesizedExpression"
                            [ expressionValue ]
                    )

        Elm.Syntax.Expression.LetExpression letBlock ->
            encodeElmSyntaxLetBlockAsPineValue letBlock
                |> Result.map
                    (\letBlockValue ->
                        encodeTagAsPineValue
                            "LetExpression"
                            [ letBlockValue ]
                    )

        Elm.Syntax.Expression.CaseExpression caseBlock ->
            encodeElmSyntaxCaseBlockAsPineValue caseBlock
                |> Result.map
                    (\caseBlockValue ->
                        encodeTagAsPineValue
                            "CaseExpression"
                            [ caseBlockValue ]
                    )

        Elm.Syntax.Expression.LambdaExpression lambda ->
            encodeElmSyntaxLambdaAsPineValue lambda
                |> Result.map
                    (\lambdaValue ->
                        encodeTagAsPineValue
                            "LambdaExpression"
                            [ lambdaValue ]
                    )

        Elm.Syntax.Expression.RecordExpr recordSetters ->
            recordSetters
                |> List.map (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxRecordSetterAsPineValue)
                |> Result.Extra.combine
                |> Result.map
                    (\recordSettersValues ->
                        encodeTagAsPineValue
                            "RecordExpr"
                            [ Pine.ListValue recordSettersValues ]
                    )

        Elm.Syntax.Expression.RecordAccess expressionNode stringNode ->
            encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxExpressionAsPineValue expressionNode
                |> Result.map
                    (\expressionValue ->
                        encodeTagAsPineValue
                            "RecordAccess"
                            [ expressionValue
                            , encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue stringNode
                            ]
                    )

        Elm.Syntax.Expression.RecordAccessFunction string ->
            Ok
                (encodeTagAsPineValue
                    "RecordAccessFunction"
                    [ encodeElmStringAsPineValue string ]
                )

        Elm.Syntax.Expression.RecordUpdateExpression stringNode recordSetters ->
            recordSetters
                |> List.map (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxRecordSetterAsPineValue)
                |> Result.Extra.combine
                |> Result.map
                    (\recordSettersValues ->
                        encodeTagAsPineValue
                            "RecordUpdateExpression"
                            [ encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue stringNode
                            , Pine.ListValue recordSettersValues
                            ]
                    )

        Elm.Syntax.Expression.Floatable _ ->
            Err "Unsupported expression: Floatable"

        Elm.Syntax.Expression.GLSLExpression string_ ->
            Ok (encodeTagAsPineValue "GLSLExpression" [ encodeElmStringAsPineValue string_ ])


encodeElmSyntaxInfixAsPineValue : Elm.Syntax.Infix.Infix -> Pine.Value
encodeElmSyntaxInfixAsPineValue infix =
    encodeRecordAsPineValue
        [ ( "direction"
          , encodeElmSyntaxNodeAsPineValue encodeElmSyntaxInfixDirectionAsPineValue infix.direction
          )
        , ( "precedence"
          , encodeElmSyntaxNodeAsPineValue encodeIntAsPineValue infix.precedence
          )
        , ( "operator"
          , encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue infix.operator
          )
        , ( "function"
          , encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue infix.function
          )
        ]


encodeElmSyntaxInfixDirectionAsPineValue : Elm.Syntax.Infix.InfixDirection -> Pine.Value
encodeElmSyntaxInfixDirectionAsPineValue infixDirection =
    case infixDirection of
        Elm.Syntax.Infix.Left ->
            encodeTagAsPineValue "Left" []

        Elm.Syntax.Infix.Right ->
            encodeTagAsPineValue "Right" []

        Elm.Syntax.Infix.Non ->
            encodeTagAsPineValue "Non" []


encodeElmSyntaxRecordSetterAsPineValue : Elm.Syntax.Expression.RecordSetter -> Result String Pine.Value
encodeElmSyntaxRecordSetterAsPineValue ( stringNode, expressionNode ) =
    encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxExpressionAsPineValue expressionNode
        |> Result.map
            (\expressionNodeValue ->
                Pine.ListValue
                    [ encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue stringNode
                    , expressionNodeValue
                    ]
            )


encodeElmSyntaxLambdaAsPineValue : Elm.Syntax.Expression.Lambda -> Result String Pine.Value
encodeElmSyntaxLambdaAsPineValue lambda =
    lambda.args
        |> List.map (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxPatternAsPineValue)
        |> Result.Extra.combine
        |> Result.andThen
            (\argsValues ->
                encodeElmSyntaxNodeAsPineValueResult
                    encodeElmSyntaxExpressionAsPineValue
                    lambda.expression
                    |> Result.map
                        (\expressionValue ->
                            encodeRecordAsPineValue
                                [ ( "args", Pine.ListValue argsValues )
                                , ( "expression"
                                  , expressionValue
                                  )
                                ]
                        )
            )


encodeElmSyntaxCaseBlockAsPineValue : Elm.Syntax.Expression.CaseBlock -> Result String Pine.Value
encodeElmSyntaxCaseBlockAsPineValue caseBlock =
    encodeElmSyntaxNodeAsPineValueResult
        encodeElmSyntaxExpressionAsPineValue
        caseBlock.expression
        |> Result.andThen
            (\expressionValue ->
                caseBlock.cases
                    |> List.map
                        (\( patternNode, expressionNode ) ->
                            encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxPatternAsPineValue patternNode
                                |> Result.andThen
                                    (\patternValue ->
                                        encodeElmSyntaxNodeAsPineValueResult
                                            encodeElmSyntaxExpressionAsPineValue
                                            expressionNode
                                            |> Result.map
                                                (\patternExpressionValue ->
                                                    encodeRecordAsPineValue
                                                        [ ( "pattern", patternValue )
                                                        , ( "expression", patternExpressionValue )
                                                        ]
                                                )
                                    )
                        )
                    |> Result.Extra.combine
                    |> Result.map
                        (\casesValues ->
                            encodeRecordAsPineValue
                                [ ( "expression"
                                  , expressionValue
                                  )
                                , ( "cases"
                                  , Pine.ListValue casesValues
                                  )
                                ]
                        )
            )


encodeElmSyntaxLetBlockAsPineValue : Elm.Syntax.Expression.LetBlock -> Result String Pine.Value
encodeElmSyntaxLetBlockAsPineValue letBlock =
    let
        encodeLetDeclarationAsPineValue : Elm.Syntax.Expression.LetDeclaration -> Result String Pine.Value
        encodeLetDeclarationAsPineValue letDeclaration =
            case letDeclaration of
                Elm.Syntax.Expression.LetFunction function ->
                    encodeElmSyntaxFunctionExpressionAsPineValue function
                        |> Result.map
                            (\functionExpressionValue ->
                                encodeTagAsPineValue
                                    "LetFunction"
                                    [ functionExpressionValue ]
                            )

                Elm.Syntax.Expression.LetDestructuring patternNode expressionNode ->
                    encodeElmSyntaxNodeAsPineValueResult
                        encodeElmSyntaxPatternAsPineValue
                        patternNode
                        |> Result.andThen
                            (\patternValue ->
                                encodeElmSyntaxNodeAsPineValueResult
                                    encodeElmSyntaxExpressionAsPineValue
                                    expressionNode
                                    |> Result.map
                                        (\expressionValue ->
                                            encodeTagAsPineValue
                                                "LetDestructuring"
                                                [ patternValue, expressionValue ]
                                        )
                            )

        encodeLetDeclarationNodeAsPineValue : Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration -> Result String Pine.Value
        encodeLetDeclarationNodeAsPineValue letDeclarationNode =
            encodeElmSyntaxNodeAsPineValueResult
                encodeLetDeclarationAsPineValue
                letDeclarationNode
    in
    letBlock.declarations
        |> List.map encodeLetDeclarationNodeAsPineValue
        |> Result.Extra.combine
        |> Result.andThen
            (\declarationsValues ->
                encodeElmSyntaxNodeAsPineValueResult
                    encodeElmSyntaxExpressionAsPineValue
                    letBlock.expression
                    |> Result.map
                        (\expressionValue ->
                            encodeRecordAsPineValue
                                [ ( "declarations", Pine.ListValue declarationsValues )
                                , ( "expression", expressionValue )
                                ]
                        )
            )


encodeElmSyntaxPatternAsPineValue : Elm.Syntax.Pattern.Pattern -> Result String Pine.Value
encodeElmSyntaxPatternAsPineValue pattern =
    case pattern of
        Elm.Syntax.Pattern.AllPattern ->
            Ok (encodeTagAsPineValue "AllPattern" [])

        Elm.Syntax.Pattern.UnitPattern ->
            Ok (encodeTagAsPineValue "UnitPattern" [])

        Elm.Syntax.Pattern.CharPattern char ->
            Ok (encodeTagAsPineValue "CharPattern" [ Pine.valueFromChar char ])

        Elm.Syntax.Pattern.StringPattern string ->
            Ok (encodeTagAsPineValue "StringPattern" [ encodeElmStringAsPineValue string ])

        Elm.Syntax.Pattern.IntPattern int ->
            Ok (encodeTagAsPineValue "IntPattern" [ Pine.valueFromInt int ])

        Elm.Syntax.Pattern.HexPattern int ->
            Ok (encodeTagAsPineValue "HexPattern" [ Pine.valueFromInt int ])

        Elm.Syntax.Pattern.FloatPattern _ ->
            Err "Unsupported pattern: FloatPattern"

        Elm.Syntax.Pattern.TuplePattern patterns ->
            patterns
                |> List.map (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxPatternAsPineValue)
                |> Result.Extra.combine
                |> Result.map
                    (\patternsValues ->
                        encodeTagAsPineValue
                            "TuplePattern"
                            [ Pine.ListValue patternsValues ]
                    )

        Elm.Syntax.Pattern.RecordPattern fieldsNodes ->
            Ok
                (encodeTagAsPineValue
                    "RecordPattern"
                    [ Pine.ListValue
                        (List.map (encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue) fieldsNodes)
                    ]
                )

        Elm.Syntax.Pattern.UnConsPattern headPattern tailPattern ->
            encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxPatternAsPineValue headPattern
                |> Result.andThen
                    (\headPatternValue ->
                        encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxPatternAsPineValue tailPattern
                            |> Result.map
                                (\tailPatternValue ->
                                    encodeTagAsPineValue
                                        "UnConsPattern"
                                        [ headPatternValue
                                        , tailPatternValue
                                        ]
                                )
                    )

        Elm.Syntax.Pattern.ListPattern patterns ->
            patterns
                |> List.map (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxPatternAsPineValue)
                |> Result.Extra.combine
                |> Result.map
                    (\patternsValues ->
                        encodeTagAsPineValue
                            "ListPattern"
                            [ Pine.ListValue patternsValues ]
                    )

        Elm.Syntax.Pattern.VarPattern string ->
            Ok (encodeTagAsPineValue "VarPattern" [ encodeElmStringAsPineValue string ])

        Elm.Syntax.Pattern.NamedPattern qualifiedNameRef patterns ->
            patterns
                |> List.map (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxPatternAsPineValue)
                |> Result.Extra.combine
                |> Result.map
                    (\patternsValues ->
                        encodeTagAsPineValue
                            "NamedPattern"
                            [ encodeElmSyntaxQualifiedNameRefAsPineValue qualifiedNameRef
                            , Pine.ListValue patternsValues
                            ]
                    )

        Elm.Syntax.Pattern.AsPattern patternNode stringNode ->
            encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxPatternAsPineValue patternNode
                |> Result.map
                    (\patternValue ->
                        encodeTagAsPineValue
                            "AsPattern"
                            [ patternValue
                            , encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue stringNode
                            ]
                    )

        Elm.Syntax.Pattern.ParenthesizedPattern patternNode ->
            encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxPatternAsPineValue patternNode
                |> Result.map
                    (\patternValue ->
                        encodeTagAsPineValue
                            "ParenthesizedPattern"
                            [ patternValue ]
                    )


encodeElmSyntaxQualifiedNameRefAsPineValue : Elm.Syntax.Pattern.QualifiedNameRef -> Pine.Value
encodeElmSyntaxQualifiedNameRefAsPineValue qualifiedNameRef =
    encodeRecordAsPineValue
        [ ( "moduleName"
          , Pine.ListValue
                (List.map encodeElmStringAsPineValue qualifiedNameRef.moduleName)
          )
        , ( "name", encodeElmStringAsPineValue qualifiedNameRef.name )
        ]


encodeElmSyntaxSignatureAsPineValue : Elm.Syntax.Signature.Signature -> Pine.Value
encodeElmSyntaxSignatureAsPineValue elmSyntaxSignature =
    encodeRecordAsPineValue
        [ ( "name"
          , encodeElmSyntaxNodeAsPineValue
                encodeElmStringAsPineValue
                elmSyntaxSignature.name
          )
        , ( "typeAnnotation"
          , encodeElmSyntaxNodeAsPineValue
                encodeElmSyntaxTypeAnnotationAsPineValue
                elmSyntaxSignature.typeAnnotation
          )
        ]


encodeElmSyntaxTypeAnnotationAsPineValue : Elm.Syntax.TypeAnnotation.TypeAnnotation -> Pine.Value
encodeElmSyntaxTypeAnnotationAsPineValue typeAnnotation =
    case typeAnnotation of
        Elm.Syntax.TypeAnnotation.GenericType string ->
            encodeTagAsPineValue "GenericType" [ encodeElmStringAsPineValue string ]

        Elm.Syntax.TypeAnnotation.Typed instantiatedNode argumentsNodes ->
            encodeTagAsPineValue
                "Typed"
                [ encodeElmSyntaxNodeAsPineValue
                    (\( moduleName, string ) ->
                        Pine.ListValue
                            [ (List.map encodeElmStringAsPineValue >> Pine.ListValue)
                                moduleName
                            , encodeElmStringAsPineValue string
                            ]
                    )
                    instantiatedNode
                , Pine.ListValue
                    (List.map (encodeElmSyntaxNodeAsPineValue encodeElmSyntaxTypeAnnotationAsPineValue)
                        argumentsNodes
                    )
                ]

        Elm.Syntax.TypeAnnotation.Unit ->
            encodeTagAsPineValue "Unit" []

        Elm.Syntax.TypeAnnotation.Tupled typeAnnotations ->
            encodeTagAsPineValue
                "Tupled"
                [ Pine.ListValue
                    (List.map (encodeElmSyntaxNodeAsPineValue encodeElmSyntaxTypeAnnotationAsPineValue)
                        typeAnnotations
                    )
                ]

        Elm.Syntax.TypeAnnotation.Record recordDefinition ->
            encodeTagAsPineValue
                "Record"
                [ Pine.ListValue
                    (List.map
                        (encodeElmSyntaxNodeAsPineValue
                            (\( fieldNameNode, fieldTypeNode ) ->
                                Pine.ListValue
                                    [ encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue fieldNameNode
                                    , encodeElmSyntaxNodeAsPineValue encodeElmSyntaxTypeAnnotationAsPineValue fieldTypeNode
                                    ]
                            )
                        )
                        recordDefinition
                    )
                ]

        Elm.Syntax.TypeAnnotation.GenericRecord nameNode recordDefinitionNode ->
            encodeTagAsPineValue
                "GenericRecord"
                [ encodeElmSyntaxNodeAsPineValue
                    encodeElmStringAsPineValue
                    nameNode
                , encodeElmSyntaxNodeAsPineValue
                    (\recordDefinition ->
                        Pine.ListValue
                            (List.map
                                (encodeElmSyntaxNodeAsPineValue
                                    (\( fieldNameNode, fieldTypeNode ) ->
                                        Pine.ListValue
                                            [ encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue fieldNameNode
                                            , encodeElmSyntaxNodeAsPineValue encodeElmSyntaxTypeAnnotationAsPineValue fieldTypeNode
                                            ]
                                    )
                                )
                                recordDefinition
                            )
                    )
                    recordDefinitionNode
                ]

        Elm.Syntax.TypeAnnotation.FunctionTypeAnnotation fromTypeAnnotationNode toTypeAnnotationNode ->
            encodeTagAsPineValue
                "FunctionTypeAnnotation"
                [ encodeElmSyntaxNodeAsPineValue encodeElmSyntaxTypeAnnotationAsPineValue fromTypeAnnotationNode
                , encodeElmSyntaxNodeAsPineValue encodeElmSyntaxTypeAnnotationAsPineValue toTypeAnnotationNode
                ]


encodeElmSyntaxImportAsPineValue : Elm.Syntax.Import.Import -> Pine.Value
encodeElmSyntaxImportAsPineValue elmSyntaxImport =
    encodeRecordAsPineValue
        [ ( "moduleName"
          , encodeElmSyntaxNodeAsPineValue
                (List.map encodeElmStringAsPineValue >> Pine.ListValue)
                elmSyntaxImport.moduleName
          )
        , ( "moduleAlias"
          , encodeMaybeAsPineValue
                (encodeElmSyntaxNodeAsPineValue (List.map encodeElmStringAsPineValue >> Pine.ListValue))
                elmSyntaxImport.moduleAlias
          )
        , ( "exposingList"
          , encodeMaybeAsPineValue
                (encodeElmSyntaxNodeAsPineValue encodeElmSyntaxImportExposingAsPineValue)
                elmSyntaxImport.exposingList
          )
        ]


encodeElmSyntaxImportExposingAsPineValue : Elm.Syntax.Exposing.Exposing -> Pine.Value
encodeElmSyntaxImportExposingAsPineValue elmSyntaxImportExposing =
    case elmSyntaxImportExposing of
        Elm.Syntax.Exposing.All range ->
            encodeTagAsPineValue
                "All"
                [ encodeElmSyntaxRangeAsPineValue range ]

        Elm.Syntax.Exposing.Explicit elmSyntaxTopLevelExposes ->
            encodeTagAsPineValue
                "Explicit"
                [ Pine.ListValue
                    (List.map (encodeElmSyntaxNodeAsPineValue encodeElmSyntaxTopLevelExposeAsPineValue)
                        elmSyntaxTopLevelExposes
                    )
                ]


encodeElmSyntaxTopLevelExposeAsPineValue : Elm.Syntax.Exposing.TopLevelExpose -> Pine.Value
encodeElmSyntaxTopLevelExposeAsPineValue elmSyntaxTopLevelExpose =
    case elmSyntaxTopLevelExpose of
        Elm.Syntax.Exposing.InfixExpose string ->
            encodeTagAsPineValue "InfixExpose" [ encodeElmStringAsPineValue string ]

        Elm.Syntax.Exposing.FunctionExpose string ->
            encodeTagAsPineValue "FunctionExpose" [ encodeElmStringAsPineValue string ]

        Elm.Syntax.Exposing.TypeOrAliasExpose string ->
            encodeTagAsPineValue "TypeOrAliasExpose" [ encodeElmStringAsPineValue string ]

        Elm.Syntax.Exposing.TypeExpose elmSyntaxExposedType ->
            encodeTagAsPineValue
                "TypeExpose"
                [ encodeRecordAsPineValue
                    [ ( "name", encodeElmStringAsPineValue elmSyntaxExposedType.name )
                    , ( "open"
                      , encodeMaybeAsPineValue
                            encodeElmSyntaxRangeAsPineValue
                            elmSyntaxExposedType.open
                      )
                    ]
                ]


encodeElmSyntaxDefaultModuleDataAsPineValue : Elm.Syntax.Module.DefaultModuleData -> Pine.Value
encodeElmSyntaxDefaultModuleDataAsPineValue defaultModuleData =
    encodeRecordAsPineValue
        [ ( "moduleName"
          , encodeElmSyntaxNodeAsPineValue
                (List.map encodeElmStringAsPineValue >> Pine.ListValue)
                defaultModuleData.moduleName
          )
        , ( "exposingList", Pine.ListValue [] )
        ]


encodeElmSyntaxNodeAsPineValueResult : (a -> Result e Pine.Value) -> Elm.Syntax.Node.Node a -> Result e Pine.Value
encodeElmSyntaxNodeAsPineValueResult encodeNodeContent (Elm.Syntax.Node.Node range nodeContent) =
    case encodeNodeContent nodeContent of
        Err error ->
            Err error

        Ok nodeContentValue ->
            Ok
                (encodeTagAsPineValue
                    "Node"
                    [ encodeElmSyntaxRangeAsPineValue range
                    , nodeContentValue
                    ]
                )


encodeElmSyntaxNodeAsPineValue : (a -> Pine.Value) -> Elm.Syntax.Node.Node a -> Pine.Value
encodeElmSyntaxNodeAsPineValue encodeNodeContent (Elm.Syntax.Node.Node range nodeContent) =
    encodeTagAsPineValue
        "Node"
        [ encodeElmSyntaxRangeAsPineValue range
        , encodeNodeContent nodeContent
        ]


encodeElmSyntaxRangeAsPineValue : Elm.Syntax.Range.Range -> Pine.Value
encodeElmSyntaxRangeAsPineValue range =
    encodeRecordAsPineValue
        [ ( "start", encodeElmSyntaxLocationAsPineValue range.start )
        , ( "end", encodeElmSyntaxLocationAsPineValue range.end )
        ]


encodeElmSyntaxLocationAsPineValue : Elm.Syntax.Range.Location -> Pine.Value
encodeElmSyntaxLocationAsPineValue location =
    encodeRecordAsPineValue
        [ ( "row", Pine.valueFromInt location.row )
        , ( "column", Pine.valueFromInt location.column )
        ]


encodeMaybeAsPineValue : (a -> Pine.Value) -> Maybe a -> Pine.Value
encodeMaybeAsPineValue encodeValue maybeValue =
    case maybeValue of
        Nothing ->
            encodeTagAsPineValue "Nothing"
                []

        Just value ->
            encodeTagAsPineValue "Just"
                [ encodeValue value ]


encodeRecordAsPineValue : List ( String, Pine.Value ) -> Pine.Value
encodeRecordAsPineValue recordFields =
    Pine.ListValue
        [ ElmCompiler.elmRecordTypeTagNameAsValue
        , Pine.ListValue
            [ Pine.ListValue
                (List.map
                    (\( fieldName, fieldValue ) ->
                        Pine.ListValue
                            [ Pine.valueFromString fieldName
                            , fieldValue
                            ]
                    )
                    (List.sortBy Tuple.first recordFields)
                )
            ]
        ]


encodeElmStringAsPineValue : String -> Pine.Value
encodeElmStringAsPineValue string =
    ElmCompiler.valueFromString string


encodeIntAsPineValue : Int -> Pine.Value
encodeIntAsPineValue int =
    Pine.valueFromInt int


encodeTagAsPineValue : String -> List Pine.Value -> Pine.Value
encodeTagAsPineValue tagName tagArguments =
    Pine.ListValue
        [ Pine.valueFromString tagName
        , Pine.ListValue tagArguments
        ]
