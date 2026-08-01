module ElmSyntax.Abstract.ConvertFromConcrete exposing
    ( fromDeclaration
    , fromExposing
    , fromExpression
    , fromFile
    , fromFunctionStruct
    , fromImport
    , fromInfix
    , fromModule
    , fromPattern
    , fromTypeAnnotation
    )

import ElmSyntax.Abstract.Declaration as AbstractDeclaration
import ElmSyntax.Abstract.Exposing as AbstractExposing
import ElmSyntax.Abstract.Expression as AbstractExpression
import ElmSyntax.Abstract.File as AbstractFile
import ElmSyntax.Abstract.Import as AbstractImport
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
import ElmSyntax.Concrete.Node as Node
import ElmSyntax.Concrete.Pattern as ConcretePattern
import ElmSyntax.Concrete.SeparatedSyntaxList as SeparatedSyntaxList
import ElmSyntax.Concrete.TypeAnnotation as ConcreteTypeAnnotation


fromFile : ConcreteFile.File -> AbstractFile.File
fromFile concrete =
    { moduleDefinition = fromModule (Node.value concrete.moduleDefinition)
    , imports = List.map (Node.value >> fromImport) concrete.imports
    , declarations = List.map (Node.value >> fromDeclaration) concrete.declarations
    }


fromImport : ConcreteImport.Import -> AbstractImport.Import
fromImport concrete =
    { moduleName = Node.value concrete.moduleName
    , moduleAlias =
        concrete.moduleAlias
            |> Maybe.map (Tuple.second >> Node.value)
    , exposingList =
        concrete.exposingList
            |> Maybe.map (Tuple.second >> Node.value >> fromExposing)
    }


fromModule : ConcreteModule.Module -> AbstractModule.Module
fromModule concrete =
    case concrete of
        ConcreteModule.NormalModule data ->
            AbstractModule.NormalModule (defaultModuleDataFromConcrete data)

        ConcreteModule.PortModule data ->
            AbstractModule.PortModule (defaultModuleDataFromConcrete data)

        ConcreteModule.EffectModule data ->
            AbstractModule.EffectModule
                { moduleName = Node.value data.moduleName
                , exposingList = fromExposing (Node.value data.exposingList)
                , command = Maybe.map Node.value data.command
                , subscription = Maybe.map Node.value data.subscription
                }


fromExposing : ConcreteExposing.Exposing -> AbstractExposing.Exposing
fromExposing exposing_ =
    case exposing_ of
        ConcreteExposing.All _ ->
            AbstractExposing.All

        ConcreteExposing.Explicit _ nodes _ ->
            AbstractExposing.Explicit
                (nodes
                    |> separatedToList
                    |> List.map (Node.value >> topLevelExposeFromConcrete)
                )


fromDeclaration : ConcreteDeclaration.Declaration -> AbstractDeclaration.Declaration
fromDeclaration concrete =
    case concrete of
        ConcreteDeclaration.FunctionDeclaration function ->
            AbstractDeclaration.FunctionDeclaration
                (fromFunctionStruct function)

        ConcreteDeclaration.ChoiceTypeDeclaration choice ->
            AbstractDeclaration.ChoiceTypeDeclaration
                (choiceTypeFromConcrete choice)

        ConcreteDeclaration.AliasDeclaration typeAlias ->
            AbstractDeclaration.AliasDeclaration
                (typeAliasFromConcrete typeAlias)

        ConcreteDeclaration.PortDeclaration _ signature ->
            AbstractDeclaration.PortDeclaration
                (signatureFromConcrete signature)

        ConcreteDeclaration.InfixDeclaration infix ->
            AbstractDeclaration.InfixDeclaration
                (fromInfix infix)


fromTypeAnnotation : ConcreteTypeAnnotation.TypeAnnotation -> AbstractTypeAnnotation.TypeAnnotation
fromTypeAnnotation concrete =
    case concrete of
        ConcreteTypeAnnotation.GenericType name ->
            AbstractTypeAnnotation.GenericType name

        ConcreteTypeAnnotation.Typed typeName arguments ->
            let
                ( moduleName, name ) =
                    Node.value typeName
            in
            AbstractTypeAnnotation.Typed
                moduleName
                name
                (List.map (Node.value >> fromTypeAnnotation) arguments)

        ConcreteTypeAnnotation.Unit ->
            AbstractTypeAnnotation.Unit

        ConcreteTypeAnnotation.Tupled annotations ->
            AbstractTypeAnnotation.Tupled
                (mapSeparatedNode fromTypeAnnotation annotations)

        ConcreteTypeAnnotation.Record definition ->
            AbstractTypeAnnotation.Record
                (recordDefinitionFromConcrete definition)

        ConcreteTypeAnnotation.GenericRecord genericName _ definition ->
            AbstractTypeAnnotation.GenericRecord
                (Node.value genericName)
                (recordDefinitionFromConcrete (Node.value definition))

        ConcreteTypeAnnotation.FunctionTypeAnnotation argument _ returnType ->
            AbstractTypeAnnotation.FunctionTypeAnnotation
                (fromTypeAnnotation (Node.value argument))
                (fromTypeAnnotation (Node.value returnType))


fromFunctionStruct : ConcreteExpression.FunctionStruct -> AbstractExpression.FunctionStruct
fromFunctionStruct concrete =
    { signature = Maybe.map (Node.value >> signatureFromConcrete) concrete.signature
    , declaration =
        concrete.declaration
            |> Node.value
            |> functionImplementationFromConcrete
    }


fromPattern : ConcretePattern.Pattern -> AbstractPattern.Pattern
fromPattern concrete =
    case concrete of
        ConcretePattern.AllPattern ->
            AbstractPattern.AllPattern

        ConcretePattern.VarPattern name ->
            AbstractPattern.VarPattern name

        ConcretePattern.UnitPattern ->
            AbstractPattern.UnitPattern

        ConcretePattern.CharPattern value ->
            AbstractPattern.CharPattern value

        ConcretePattern.StringPattern value ->
            AbstractPattern.StringPattern value

        ConcretePattern.IntPattern value ->
            AbstractPattern.IntPattern value

        ConcretePattern.HexPattern value ->
            AbstractPattern.IntPattern value

        ConcretePattern.FloatPattern value ->
            AbstractPattern.FloatPattern value

        ConcretePattern.TuplePattern elements ->
            AbstractPattern.TuplePattern
                (mapSeparatedNode fromPattern elements)

        ConcretePattern.RecordPattern fields ->
            AbstractPattern.RecordPattern
                (mapSeparatedNode identity fields)

        ConcretePattern.UnConsPattern head _ tail ->
            AbstractPattern.UnConsPattern
                (fromPattern (Node.value head))
                (fromPattern (Node.value tail))

        ConcretePattern.ListPattern elements ->
            AbstractPattern.ListPattern
                (mapSeparatedNode fromPattern elements)

        ConcretePattern.NamedPattern name arguments ->
            AbstractPattern.NamedPattern
                name
                (List.map (Node.value >> fromPattern) arguments)

        ConcretePattern.AsPattern pattern _ name ->
            AbstractPattern.AsPattern
                (fromPattern (Node.value pattern))
                (Node.value name)

        ConcretePattern.ParenthesizedPattern pattern ->
            fromPattern (Node.value pattern)


fromExpression : ConcreteExpression.Expression -> AbstractExpression.Expression
fromExpression concrete =
    case concrete of
        ConcreteExpression.UnitExpr ->
            AbstractExpression.UnitExpr

        ConcreteExpression.StringLiteral value _ ->
            AbstractExpression.StringLiteral value

        ConcreteExpression.MultilineStringLiteral value _ ->
            AbstractExpression.StringLiteral value

        ConcreteExpression.CharLiteral value ->
            AbstractExpression.CharLiteral value

        ConcreteExpression.IntegerLiteral literalText ->
            AbstractExpression.IntegerLiteral (parseIntegerLiteral literalText)

        ConcreteExpression.FloatLiteral literalText ->
            AbstractExpression.FloatLiteral (parseFloatLiteral literalText)

        ConcreteExpression.Negation expression ->
            AbstractExpression.Negation
                (fromExpression (Node.value expression))

        ConcreteExpression.ListExpr elements ->
            AbstractExpression.ListExpr
                (mapSeparatedNode fromExpression elements)

        ConcreteExpression.Identifier moduleName name ->
            AbstractExpression.Identifier moduleName name

        ConcreteExpression.IfBlock _ condition _ thenBlock _ elseBlock ->
            AbstractExpression.IfBlock
                (fromExpression (Node.value condition))
                (fromExpression (Node.value thenBlock))
                (fromExpression (Node.value elseBlock))

        ConcreteExpression.PrefixOperator operator ->
            AbstractExpression.PrefixOperator operator

        ConcreteExpression.Parenthesized expression ->
            fromExpression (Node.value expression)

        ConcreteExpression.Application function arguments ->
            AbstractExpression.Application
                (fromExpression (Node.value function))
                (List.map (Node.value >> fromExpression) arguments)

        ConcreteExpression.OperatorApplication operator direction left right ->
            AbstractExpression.OperatorApplication
                (Node.value operator)
                (infixDirectionFromConcrete direction)
                (fromExpression (Node.value left))
                (fromExpression (Node.value right))

        ConcreteExpression.TupledExpression elements ->
            AbstractExpression.TupledExpression
                (mapSeparatedNode fromExpression elements)

        ConcreteExpression.LambdaExpression lambda ->
            AbstractExpression.LambdaExpression
                (List.map (Node.value >> fromPattern) lambda.arguments)
                (fromExpression (Node.value lambda.expression))

        ConcreteExpression.CaseExpression caseBlock ->
            AbstractExpression.CaseExpression
                (fromExpression (Node.value caseBlock.expression))
                (List.map caseFromConcrete caseBlock.cases)

        ConcreteExpression.LetExpression letBlock ->
            AbstractExpression.LetExpression
                (List.map
                    (Node.value >> letDeclarationFromConcrete)
                    letBlock.declarations
                )
                (fromExpression (Node.value letBlock.expression))

        ConcreteExpression.RecordExpr fields ->
            AbstractExpression.RecordExpr
                (recordSettersFromConcrete fields)

        ConcreteExpression.RecordAccess record fieldName ->
            AbstractExpression.RecordAccess
                (fromExpression (Node.value record))
                (Node.value fieldName)

        ConcreteExpression.RecordAccessFunction functionName ->
            AbstractExpression.RecordAccessFunction
                (stripLeadingDot functionName)

        ConcreteExpression.RecordUpdateExpression recordName _ fields ->
            AbstractExpression.RecordUpdateExpression
                (Node.value recordName)
                (recordSettersFromConcrete fields)

        ConcreteExpression.GLSLExpression shaderCode ->
            AbstractExpression.GLSLExpression shaderCode


fromInfix : ConcreteInfix.Infix -> AbstractInfix.Infix
fromInfix concrete =
    { direction = infixDirectionFromConcrete (Node.value concrete.direction)
    , precedence = Node.value concrete.precedence
    , operator = Node.value concrete.operator
    , functionName = Node.value concrete.function
    }


defaultModuleDataFromConcrete : ConcreteModule.DefaultModuleData -> AbstractModule.DefaultModuleData
defaultModuleDataFromConcrete data =
    { moduleName = Node.value data.moduleName
    , exposingList = fromExposing (Node.value data.exposingList)
    }


topLevelExposeFromConcrete : ConcreteExposing.TopLevelExpose -> AbstractExposing.TopLevelExpose
topLevelExposeFromConcrete expose =
    case expose of
        ConcreteExposing.InfixExpose name ->
            AbstractExposing.InfixExpose name

        ConcreteExposing.FunctionExpose name ->
            AbstractExposing.FunctionExpose name

        ConcreteExposing.TypeOrAliasExpose name ->
            AbstractExposing.TypeOrAliasExpose name

        ConcreteExposing.TypeExpose exposedType ->
            AbstractExposing.TypeExpose
                { name = exposedType.name
                , exposesConstructors = exposedType.open /= Nothing
                }


typeAliasFromConcrete : ConcreteDeclaration.TypeAlias -> AbstractDeclaration.TypeAlias
typeAliasFromConcrete concrete =
    { name = Node.value concrete.name
    , generics = List.map Node.value concrete.generics
    , typeAnnotation =
        fromTypeAnnotation (Node.value concrete.typeAnnotation)
    }


choiceTypeFromConcrete : ConcreteDeclaration.ChoiceStruct -> AbstractDeclaration.ChoiceTypeStruct
choiceTypeFromConcrete concrete =
    { name = Node.value concrete.name
    , generics = List.map Node.value concrete.generics
    , constructors =
        concrete.constructors
            |> separatedToList
            |> List.map (Node.value >> valueConstructorFromConcrete)
    }


valueConstructorFromConcrete : ConcreteDeclaration.ValueConstructor -> AbstractDeclaration.ValueConstructor
valueConstructorFromConcrete concrete =
    { name = Node.value concrete.name
    , arguments =
        List.map
            (Node.value >> fromTypeAnnotation)
            concrete.arguments
    }


signatureFromConcrete : ConcreteExpression.Signature -> AbstractExpression.Signature
signatureFromConcrete concrete =
    { name = Node.value concrete.name
    , typeAnnotation =
        fromTypeAnnotation (Node.value concrete.typeAnnotation)
    }


recordDefinitionFromConcrete : ConcreteTypeAnnotation.RecordDefinition -> AbstractTypeAnnotation.RecordDefinition
recordDefinitionFromConcrete definition =
    mapSeparatedNode
        (\field ->
            { fieldName = Node.value field.fieldName
            , fieldType = fromTypeAnnotation (Node.value field.fieldType)
            }
        )
        definition


functionImplementationFromConcrete : ConcreteExpression.FunctionImplementation -> AbstractExpression.FunctionImplementation
functionImplementationFromConcrete concrete =
    { name = Node.value concrete.name
    , arguments = List.map (Node.value >> fromPattern) concrete.arguments
    , expression = fromExpression (Node.value concrete.expression)
    }


caseFromConcrete : ConcreteExpression.Case -> AbstractExpression.Case
caseFromConcrete concrete =
    { pattern = fromPattern (Node.value concrete.pattern)
    , expression = fromExpression (Node.value concrete.expression)
    }


letDeclarationFromConcrete : ConcreteExpression.LetDeclaration -> AbstractExpression.LetDeclaration
letDeclarationFromConcrete concrete =
    case concrete of
        ConcreteExpression.LetFunction function ->
            AbstractExpression.LetFunction (fromFunctionStruct function)

        ConcreteExpression.LetDestructuring pattern _ expression ->
            AbstractExpression.LetDestructuring
                (fromPattern (Node.value pattern))
                (fromExpression (Node.value expression))


recordSettersFromConcrete : SeparatedSyntaxList.SeparatedSyntaxList ConcreteExpression.RecordExprField -> List AbstractExpression.RecordSetter
recordSettersFromConcrete fields =
    fields
        |> separatedToList
        |> List.map
            (\field ->
                { fieldName = Node.value field.fieldName
                , value = fromExpression (Node.value field.valueExpr)
                }
            )
        |> List.sortBy .fieldName


infixDirectionFromConcrete : ConcreteInfix.InfixDirection -> AbstractInfix.InfixDirection
infixDirectionFromConcrete direction =
    case direction of
        ConcreteInfix.Left ->
            AbstractInfix.Left

        ConcreteInfix.Right ->
            AbstractInfix.Right

        ConcreteInfix.Non ->
            AbstractInfix.Non


parseIntegerLiteral : String -> Int
parseIntegerLiteral literalText =
    let
        trimmed =
            String.trim literalText

        ( sign, absolute ) =
            if String.startsWith "-" trimmed then
                ( -1, String.dropLeft 1 trimmed )

            else
                ( 1, trimmed )
    in
    (if String.startsWith "0x" absolute || String.startsWith "0X" absolute then
        hexStringToInt (String.dropLeft 2 absolute)

     else
        String.toInt absolute
    )
        |> Maybe.map ((*) sign)
        |> Maybe.withDefault 0


parseFloatLiteral : String -> Float
parseFloatLiteral literalText =
    String.toFloat (String.trim literalText)
        |> Maybe.withDefault 0


stripLeadingDot : String -> String
stripLeadingDot functionName =
    case String.uncons functionName of
        Just ( '.', fieldName ) ->
            fieldName

        _ ->
            functionName


hexStringToInt : String -> Maybe Int
hexStringToInt string =
    List.foldl
        (\char accumulated ->
            Maybe.map2
                (\value digit -> value * 16 + digit)
                accumulated
                (hexDigit char)
        )
        (Just 0)
        (String.toList string)


hexDigit : Char -> Maybe Int
hexDigit char =
    let
        code =
            Char.toCode char
    in
    if code >= Char.toCode '0' && code <= Char.toCode '9' then
        Just (code - Char.toCode '0')

    else if code >= Char.toCode 'a' && code <= Char.toCode 'f' then
        Just (code - Char.toCode 'a' + 10)

    else if code >= Char.toCode 'A' && code <= Char.toCode 'F' then
        Just (code - Char.toCode 'A' + 10)

    else
        Nothing


mapSeparatedNode : (a -> b) -> SeparatedSyntaxList.SeparatedSyntaxList (Node.Node a) -> List b
mapSeparatedNode mapNode separated =
    separated
        |> separatedToList
        |> List.map (Node.value >> mapNode)


separatedToList : SeparatedSyntaxList.SeparatedSyntaxList a -> List a
separatedToList separated =
    case separated of
        SeparatedSyntaxList.Empty ->
            []

        SeparatedSyntaxList.NonEmpty first rest ->
            first :: List.map Tuple.second rest
