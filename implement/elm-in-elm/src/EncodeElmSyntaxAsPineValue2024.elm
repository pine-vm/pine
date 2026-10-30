module EncodeElmSyntaxAsPineValue2024 exposing (..)

import Common
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
import ElmCompiler2024
import Pine2024


encodeInteractiveSubmissionAsPineValue : ElmCompiler2024.InteractiveSubmission -> Result String Pine2024.Value
encodeInteractiveSubmissionAsPineValue submission =
    case submission of
        ElmCompiler2024.ExpressionSubmission expr ->
            case encodeElmSyntaxExpressionAsPineValue expr of
                Err err ->
                    Err ("Failed encoding expression: " ++ err)

                Ok exprValue ->
                    Ok
                        (encodeTagAsPineValue
                            "ExpressionSubmission"
                            [ exprValue
                            ]
                        )

        ElmCompiler2024.DeclarationSubmission decl ->
            case encodeElmSyntaxDeclarationAsPineValue decl of
                Err err ->
                    Err ("Failed encoding declaration: " ++ err)

                Ok declValue ->
                    Ok
                        (encodeTagAsPineValue
                            "DeclarationSubmission"
                            [ declValue
                            ]
                        )


encodeElmSyntaxFileAsPineValue : Elm.Syntax.File.File -> Result String Pine2024.Value
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
                |> Common.resultListMapCombine
                    (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxDeclarationAsPineValue)
                |> Result.map
                    (\declarationsValues ->
                        encodeRecordAsPineValue
                            [ ( "moduleDefinition", moduleDefinitionValue )
                            , ( "imports"
                              , Pine2024.ListValue
                                    (List.map (encodeElmSyntaxNodeAsPineValue encodeElmSyntaxImportAsPineValue)
                                        elmSyntaxFile.imports
                                    )
                              )
                            , ( "declarations"
                              , Pine2024.ListValue declarationsValues
                              )
                            , ( "comments"
                              , Pine2024.ListValue
                                    (List.map (encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue)
                                        elmSyntaxFile.comments
                                    )
                              )
                            ]
                    )


encodeElmSyntaxModuleDefinitionAsPineValue : Elm.Syntax.Module.Module -> Result String Pine2024.Value
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


encodeElmSyntaxDeclarationAsPineValue : Elm.Syntax.Declaration.Declaration -> Result String Pine2024.Value
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


encodeElmSyntaxTypeAsPineValue : Elm.Syntax.Type.Type -> Pine2024.Value
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
          , Pine2024.ListValue
                (List.map
                    (encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue)
                    elmSyntaxType.generics
                )
          )
        , ( "constructors"
          , Pine2024.ListValue
                (List.map (encodeElmSyntaxNodeAsPineValue encodeElmSyntaxValueConstructorAsPineValue)
                    elmSyntaxType.constructors
                )
          )
        ]


encodeElmSyntaxValueConstructorAsPineValue : Elm.Syntax.Type.ValueConstructor -> Pine2024.Value
encodeElmSyntaxValueConstructorAsPineValue elmSyntaxValueConstructor =
    encodeRecordAsPineValue
        [ ( "name"
          , encodeElmSyntaxNodeAsPineValue
                encodeElmStringAsPineValue
                elmSyntaxValueConstructor.name
          )
        , ( "arguments"
          , Pine2024.ListValue
                (List.map (encodeElmSyntaxNodeAsPineValue encodeElmSyntaxTypeAnnotationAsPineValue)
                    elmSyntaxValueConstructor.arguments
                )
          )
        ]


encodeElmSyntaxTypeAliasAsPineValue : Elm.Syntax.TypeAlias.TypeAlias -> Result String Pine2024.Value
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
              , Pine2024.ListValue
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


encodeElmSyntaxFunctionExpressionAsPineValue : Elm.Syntax.Expression.Function -> Result String Pine2024.Value
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


encodeElmSyntaxFunctionImplementationAsPineValue : Elm.Syntax.Expression.FunctionImplementation -> Result String Pine2024.Value
encodeElmSyntaxFunctionImplementationAsPineValue elmSyntaxFunctionImplementation =
    elmSyntaxFunctionImplementation.arguments
        |> Common.resultListMapCombine
            (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxPatternAsPineValue)
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
                                  , Pine2024.ListValue argumentsValues
                                  )
                                , ( "expression"
                                  , expressionValue
                                  )
                                ]
                        )
            )


encodeElmSyntaxExpressionAsPineValue : Elm.Syntax.Expression.Expression -> Result String Pine2024.Value
encodeElmSyntaxExpressionAsPineValue expression =
    case expression of
        Elm.Syntax.Expression.UnitExpr ->
            Ok (encodeTagAsPineValue "UnitExpr" [])

        Elm.Syntax.Expression.Literal string ->
            Ok (encodeTagAsPineValue "Literal" [ encodeElmStringAsPineValue string ])

        Elm.Syntax.Expression.ListExpr expressions ->
            expressions
                |> Common.resultListMapCombine
                    (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxExpressionAsPineValue)
                |> Result.map
                    (\expressionsValues ->
                        encodeTagAsPineValue
                            "ListExpr"
                            [ Pine2024.ListValue expressionsValues ]
                    )

        Elm.Syntax.Expression.Application expressions ->
            expressions
                |> Common.resultListMapCombine
                    (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxExpressionAsPineValue)
                |> Result.map
                    (\expressionsValues ->
                        encodeTagAsPineValue
                            "Application"
                            [ Pine2024.ListValue expressionsValues ]
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
                    [ (List.map encodeElmStringAsPineValue >> Pine2024.ListValue)
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
            Ok (encodeTagAsPineValue "Integer" [ Pine2024.valueFromInt int ])

        Elm.Syntax.Expression.Hex int ->
            Ok (encodeTagAsPineValue "Hex" [ Pine2024.valueFromInt int ])

        Elm.Syntax.Expression.Negation expressionNode ->
            encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxExpressionAsPineValue expressionNode
                |> Result.map
                    (\expressionValue ->
                        encodeTagAsPineValue
                            "Negation"
                            [ expressionValue ]
                    )

        Elm.Syntax.Expression.CharLiteral char ->
            Ok (encodeTagAsPineValue "CharLiteral" [ Pine2024.valueFromChar char ])

        Elm.Syntax.Expression.TupledExpression expressions ->
            expressions
                |> Common.resultListMapCombine
                    (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxExpressionAsPineValue)
                |> Result.map
                    (\expressionsValues ->
                        encodeTagAsPineValue
                            "TupledExpression"
                            [ Pine2024.ListValue expressionsValues ]
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
                |> Common.resultListMapCombine
                    (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxRecordSetterAsPineValue)
                |> Result.map
                    (\recordSettersValues ->
                        encodeTagAsPineValue
                            "RecordExpr"
                            [ Pine2024.ListValue recordSettersValues ]
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
                |> Common.resultListMapCombine
                    (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxRecordSetterAsPineValue)
                |> Result.map
                    (\recordSettersValues ->
                        encodeTagAsPineValue
                            "RecordUpdateExpression"
                            [ encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue stringNode
                            , Pine2024.ListValue recordSettersValues
                            ]
                    )

        Elm.Syntax.Expression.Floatable float ->
            Ok
                (encodeTagAsPineValue "Floatable"
                    [ ElmCompiler2024.valueFromFloat float
                    ]
                )

        Elm.Syntax.Expression.GLSLExpression string_ ->
            Ok (encodeTagAsPineValue "GLSLExpression" [ encodeElmStringAsPineValue string_ ])


encodeElmSyntaxInfixAsPineValue : Elm.Syntax.Infix.Infix -> Pine2024.Value
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


encodeElmSyntaxInfixDirectionAsPineValue : Elm.Syntax.Infix.InfixDirection -> Pine2024.Value
encodeElmSyntaxInfixDirectionAsPineValue infixDirection =
    case infixDirection of
        Elm.Syntax.Infix.Left ->
            encodeTagAsPineValue "Left" []

        Elm.Syntax.Infix.Right ->
            encodeTagAsPineValue "Right" []

        Elm.Syntax.Infix.Non ->
            encodeTagAsPineValue "Non" []


encodeElmSyntaxRecordSetterAsPineValue : Elm.Syntax.Expression.RecordSetter -> Result String Pine2024.Value
encodeElmSyntaxRecordSetterAsPineValue ( stringNode, expressionNode ) =
    encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxExpressionAsPineValue expressionNode
        |> Result.map
            (\expressionNodeValue ->
                Pine2024.ListValue
                    [ encodeElmSyntaxNodeAsPineValue encodeElmStringAsPineValue stringNode
                    , expressionNodeValue
                    ]
            )


encodeElmSyntaxLambdaAsPineValue : Elm.Syntax.Expression.Lambda -> Result String Pine2024.Value
encodeElmSyntaxLambdaAsPineValue lambda =
    lambda.args
        |> Common.resultListMapCombine
            (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxPatternAsPineValue)
        |> Result.andThen
            (\argsValues ->
                encodeElmSyntaxNodeAsPineValueResult
                    encodeElmSyntaxExpressionAsPineValue
                    lambda.expression
                    |> Result.map
                        (\expressionValue ->
                            encodeRecordAsPineValue
                                [ ( "args", Pine2024.ListValue argsValues )
                                , ( "expression"
                                  , expressionValue
                                  )
                                ]
                        )
            )


encodeElmSyntaxCaseBlockAsPineValue : Elm.Syntax.Expression.CaseBlock -> Result String Pine2024.Value
encodeElmSyntaxCaseBlockAsPineValue caseBlock =
    encodeElmSyntaxNodeAsPineValueResult
        encodeElmSyntaxExpressionAsPineValue
        caseBlock.expression
        |> Result.andThen
            (\expressionValue ->
                caseBlock.cases
                    |> Common.resultListMapCombine
                        (\( patternNode, expressionNode ) ->
                            encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxPatternAsPineValue patternNode
                                |> Result.andThen
                                    (\patternValue ->
                                        encodeElmSyntaxNodeAsPineValueResult
                                            encodeElmSyntaxExpressionAsPineValue
                                            expressionNode
                                            |> Result.map
                                                (\patternExpressionValue ->
                                                    Pine2024.ListValue
                                                        [ patternValue
                                                        , patternExpressionValue
                                                        ]
                                                )
                                    )
                        )
                    |> Result.map
                        (\casesValues ->
                            encodeRecordAsPineValue
                                [ ( "expression"
                                  , expressionValue
                                  )
                                , ( "cases"
                                  , Pine2024.ListValue casesValues
                                  )
                                ]
                        )
            )


encodeElmSyntaxLetBlockAsPineValue : Elm.Syntax.Expression.LetBlock -> Result String Pine2024.Value
encodeElmSyntaxLetBlockAsPineValue letBlock =
    let
        encodeLetDeclarationAsPineValue : Elm.Syntax.Expression.LetDeclaration -> Result String Pine2024.Value
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

        encodeLetDeclarationNodeAsPineValue : Elm.Syntax.Node.Node Elm.Syntax.Expression.LetDeclaration -> Result String Pine2024.Value
        encodeLetDeclarationNodeAsPineValue letDeclarationNode =
            encodeElmSyntaxNodeAsPineValueResult
                encodeLetDeclarationAsPineValue
                letDeclarationNode
    in
    letBlock.declarations
        |> Common.resultListMapCombine encodeLetDeclarationNodeAsPineValue
        |> Result.andThen
            (\declarationsValues ->
                encodeElmSyntaxNodeAsPineValueResult
                    encodeElmSyntaxExpressionAsPineValue
                    letBlock.expression
                    |> Result.map
                        (\expressionValue ->
                            encodeRecordAsPineValue
                                [ ( "declarations", Pine2024.ListValue declarationsValues )
                                , ( "expression", expressionValue )
                                ]
                        )
            )


encodeElmSyntaxPatternAsPineValue : Elm.Syntax.Pattern.Pattern -> Result String Pine2024.Value
encodeElmSyntaxPatternAsPineValue pattern =
    case pattern of
        Elm.Syntax.Pattern.AllPattern ->
            Ok (encodeTagAsPineValue "AllPattern" [])

        Elm.Syntax.Pattern.UnitPattern ->
            Ok (encodeTagAsPineValue "UnitPattern" [])

        Elm.Syntax.Pattern.CharPattern char ->
            Ok (encodeTagAsPineValue "CharPattern" [ Pine2024.valueFromChar char ])

        Elm.Syntax.Pattern.StringPattern string ->
            Ok (encodeTagAsPineValue "StringPattern" [ encodeElmStringAsPineValue string ])

        Elm.Syntax.Pattern.IntPattern int ->
            Ok (encodeTagAsPineValue "IntPattern" [ Pine2024.valueFromInt int ])

        Elm.Syntax.Pattern.HexPattern int ->
            Ok (encodeTagAsPineValue "HexPattern" [ Pine2024.valueFromInt int ])

        Elm.Syntax.Pattern.FloatPattern _ ->
            Err "Unsupported pattern: FloatPattern"

        Elm.Syntax.Pattern.TuplePattern patterns ->
            patterns
                |> Common.resultListMapCombine (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxPatternAsPineValue)
                |> Result.map
                    (\patternsValues ->
                        encodeTagAsPineValue
                            "TuplePattern"
                            [ Pine2024.ListValue patternsValues ]
                    )

        Elm.Syntax.Pattern.RecordPattern fieldsNodes ->
            Ok
                (encodeTagAsPineValue
                    "RecordPattern"
                    [ Pine2024.ListValue
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
                |> Common.resultListMapCombine
                    (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxPatternAsPineValue)
                |> Result.map
                    (\patternsValues ->
                        encodeTagAsPineValue
                            "ListPattern"
                            [ Pine2024.ListValue patternsValues ]
                    )

        Elm.Syntax.Pattern.VarPattern string ->
            Ok (encodeTagAsPineValue "VarPattern" [ encodeElmStringAsPineValue string ])

        Elm.Syntax.Pattern.NamedPattern qualifiedNameRef patterns ->
            patterns
                |> Common.resultListMapCombine
                    (encodeElmSyntaxNodeAsPineValueResult encodeElmSyntaxPatternAsPineValue)
                |> Result.map
                    (\patternsValues ->
                        encodeTagAsPineValue
                            "NamedPattern"
                            [ encodeElmSyntaxQualifiedNameRefAsPineValue qualifiedNameRef
                            , Pine2024.ListValue patternsValues
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


encodeElmSyntaxQualifiedNameRefAsPineValue : Elm.Syntax.Pattern.QualifiedNameRef -> Pine2024.Value
encodeElmSyntaxQualifiedNameRefAsPineValue qualifiedNameRef =
    encodeRecordAsPineValue
        [ ( "moduleName"
          , Pine2024.ListValue
                (List.map encodeElmStringAsPineValue qualifiedNameRef.moduleName)
          )
        , ( "name", encodeElmStringAsPineValue qualifiedNameRef.name )
        ]


encodeElmSyntaxSignatureAsPineValue : Elm.Syntax.Signature.Signature -> Pine2024.Value
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


encodeElmSyntaxTypeAnnotationAsPineValue : Elm.Syntax.TypeAnnotation.TypeAnnotation -> Pine2024.Value
encodeElmSyntaxTypeAnnotationAsPineValue typeAnnotation =
    case typeAnnotation of
        Elm.Syntax.TypeAnnotation.GenericType string ->
            encodeTagAsPineValue "GenericType" [ encodeElmStringAsPineValue string ]

        Elm.Syntax.TypeAnnotation.Typed instantiatedNode argumentsNodes ->
            encodeTagAsPineValue
                "Typed"
                [ encodeElmSyntaxNodeAsPineValue
                    (\( moduleName, string ) ->
                        Pine2024.ListValue
                            [ (List.map encodeElmStringAsPineValue >> Pine2024.ListValue)
                                moduleName
                            , encodeElmStringAsPineValue string
                            ]
                    )
                    instantiatedNode
                , Pine2024.ListValue
                    (List.map (encodeElmSyntaxNodeAsPineValue encodeElmSyntaxTypeAnnotationAsPineValue)
                        argumentsNodes
                    )
                ]

        Elm.Syntax.TypeAnnotation.Unit ->
            encodeTagAsPineValue "Unit" []

        Elm.Syntax.TypeAnnotation.Tupled typeAnnotations ->
            encodeTagAsPineValue
                "Tupled"
                [ Pine2024.ListValue
                    (List.map (encodeElmSyntaxNodeAsPineValue encodeElmSyntaxTypeAnnotationAsPineValue)
                        typeAnnotations
                    )
                ]

        Elm.Syntax.TypeAnnotation.Record recordDefinition ->
            encodeTagAsPineValue
                "Record"
                [ Pine2024.ListValue
                    (List.map
                        (encodeElmSyntaxNodeAsPineValue
                            (\( fieldNameNode, fieldTypeNode ) ->
                                Pine2024.ListValue
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
                        Pine2024.ListValue
                            (List.map
                                (encodeElmSyntaxNodeAsPineValue
                                    (\( fieldNameNode, fieldTypeNode ) ->
                                        Pine2024.ListValue
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


encodeElmSyntaxImportAsPineValue : Elm.Syntax.Import.Import -> Pine2024.Value
encodeElmSyntaxImportAsPineValue elmSyntaxImport =
    encodeRecordAsPineValue
        [ ( "moduleName"
          , encodeElmSyntaxNodeAsPineValue
                (List.map encodeElmStringAsPineValue >> Pine2024.ListValue)
                elmSyntaxImport.moduleName
          )
        , ( "moduleAlias"
          , encodeMaybeAsPineValue
                (encodeElmSyntaxNodeAsPineValue (List.map encodeElmStringAsPineValue >> Pine2024.ListValue))
                elmSyntaxImport.moduleAlias
          )
        , ( "exposingList"
          , encodeMaybeAsPineValue
                (encodeElmSyntaxNodeAsPineValue encodeElmSyntaxImportExposingAsPineValue)
                elmSyntaxImport.exposingList
          )
        ]


encodeElmSyntaxImportExposingAsPineValue : Elm.Syntax.Exposing.Exposing -> Pine2024.Value
encodeElmSyntaxImportExposingAsPineValue elmSyntaxImportExposing =
    case elmSyntaxImportExposing of
        Elm.Syntax.Exposing.All range ->
            encodeTagAsPineValue
                "All"
                [ encodeElmSyntaxRangeAsPineValue range ]

        Elm.Syntax.Exposing.Explicit elmSyntaxTopLevelExposes ->
            encodeTagAsPineValue
                "Explicit"
                [ Pine2024.ListValue
                    (List.map (encodeElmSyntaxNodeAsPineValue encodeElmSyntaxTopLevelExposeAsPineValue)
                        elmSyntaxTopLevelExposes
                    )
                ]


encodeElmSyntaxTopLevelExposeAsPineValue : Elm.Syntax.Exposing.TopLevelExpose -> Pine2024.Value
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


encodeElmSyntaxDefaultModuleDataAsPineValue : Elm.Syntax.Module.DefaultModuleData -> Pine2024.Value
encodeElmSyntaxDefaultModuleDataAsPineValue defaultModuleData =
    encodeRecordAsPineValue
        [ ( "moduleName"
          , encodeElmSyntaxNodeAsPineValue
                (List.map encodeElmStringAsPineValue >> Pine2024.ListValue)
                defaultModuleData.moduleName
          )
        , ( "exposingList"
          , encodeElmSyntaxNodeAsPineValue
                encodeElmSyntaxImportExposingAsPineValue
                defaultModuleData.exposingList
          )
        ]


encodeElmSyntaxNodeAsPineValueResult : (a -> Result e Pine2024.Value) -> Elm.Syntax.Node.Node a -> Result e Pine2024.Value
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


encodeElmSyntaxNodeAsPineValue : (a -> Pine2024.Value) -> Elm.Syntax.Node.Node a -> Pine2024.Value
encodeElmSyntaxNodeAsPineValue encodeNodeContent (Elm.Syntax.Node.Node range nodeContent) =
    encodeTagAsPineValue
        "Node"
        [ encodeElmSyntaxRangeAsPineValue range
        , encodeNodeContent nodeContent
        ]


encodeElmSyntaxRangeAsPineValue : Elm.Syntax.Range.Range -> Pine2024.Value
encodeElmSyntaxRangeAsPineValue range =
    encodeRecordAsPineValue
        [ ( "start", encodeElmSyntaxLocationAsPineValue range.start )
        , ( "end", encodeElmSyntaxLocationAsPineValue range.end )
        ]


encodeElmSyntaxLocationAsPineValue : Elm.Syntax.Range.Location -> Pine2024.Value
encodeElmSyntaxLocationAsPineValue location =
    encodeRecordAsPineValue
        [ ( "row", Pine2024.valueFromInt location.row )
        , ( "column", Pine2024.valueFromInt location.column )
        ]


encodeMaybeAsPineValue : (a -> Pine2024.Value) -> Maybe a -> Pine2024.Value
encodeMaybeAsPineValue encodeValue maybeValue =
    case maybeValue of
        Nothing ->
            encodeTagAsPineValue "Nothing"
                []

        Just value ->
            encodeTagAsPineValue "Just"
                [ encodeValue value ]


encodeRecordAsPineValue : List ( String, Pine2024.Value ) -> Pine2024.Value
encodeRecordAsPineValue recordFields =
    Pine2024.ListValue
        [ ElmCompiler2024.elmRecordTypeTagNameAsValue
        , Pine2024.ListValue
            [ Pine2024.ListValue
                (List.map
                    (\( fieldName, fieldValue ) ->
                        Pine2024.ListValue
                            [ Pine2024.valueFromString fieldName
                            , fieldValue
                            ]
                    )
                    (List.sortBy Tuple.first recordFields)
                )
            ]
        ]


encodeElmStringAsPineValue : String -> Pine2024.Value
encodeElmStringAsPineValue string =
    ElmCompiler2024.valueFromString string


encodeIntAsPineValue : Int -> Pine2024.Value
encodeIntAsPineValue int =
    Pine2024.valueFromInt int


encodeTagAsPineValue : String -> List Pine2024.Value -> Pine2024.Value
encodeTagAsPineValue tagName tagArguments =
    Pine2024.ListValue
        [ Pine2024.valueFromString tagName
        , Pine2024.ListValue tagArguments
        ]
