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
    , imports = importsFromConcrete concrete.imports
    , declarations = declarationsFromConcrete concrete.declarations
    }


fromImport : ConcreteImport.Import -> AbstractImport.Import
fromImport concrete =
    { moduleName = Node.value concrete.moduleName
    , moduleAlias =
        case concrete.moduleAlias of
            Just ( _, moduleAlias ) ->
                Just (Node.value moduleAlias)

            Nothing ->
                Nothing
    , exposingList =
        case concrete.exposingList of
            Just ( _, exposingList ) ->
                Just (fromExposing (Node.value exposingList))

            Nothing ->
                Nothing
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
                , command = maybeNodeValue data.command
                , subscription = maybeNodeValue data.subscription
                }


fromExposing : ConcreteExposing.Exposing -> AbstractExposing.Exposing
fromExposing exposing_ =
    case exposing_ of
        ConcreteExposing.All _ ->
            AbstractExposing.All

        ConcreteExposing.Explicit _ nodes _ ->
            AbstractExposing.Explicit
                (topLevelExposesFromConcrete nodes)


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
                (typeAnnotationsFromConcrete arguments)

        ConcreteTypeAnnotation.Unit ->
            AbstractTypeAnnotation.Unit

        ConcreteTypeAnnotation.Tupled annotations ->
            AbstractTypeAnnotation.Tupled
                (typeAnnotationsFromSeparated annotations)

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
    { signature =
        case concrete.signature of
            Just signature ->
                Just (signatureFromConcrete (Node.value signature))

            Nothing ->
                Nothing
    , declaration = functionImplementationFromConcrete (Node.value concrete.declaration)
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
                (patternsFromSeparated elements)

        ConcretePattern.RecordPattern fields ->
            AbstractPattern.RecordPattern
                (nodeStringValuesFromSeparated fields)

        ConcretePattern.UnConsPattern head _ tail ->
            AbstractPattern.UnConsPattern
                (fromPattern (Node.value head))
                (fromPattern (Node.value tail))

        ConcretePattern.ListPattern elements ->
            AbstractPattern.ListPattern
                (patternsFromSeparated elements)

        ConcretePattern.NamedPattern name arguments ->
            AbstractPattern.NamedPattern
                name
                (patternsFromConcrete arguments)

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
                (expressionsFromSeparated elements)

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
                (expressionsFromConcrete arguments)

        ConcreteExpression.OperatorApplication operator direction left right ->
            AbstractExpression.OperatorApplication
                (Node.value operator)
                (infixDirectionFromConcrete direction)
                (fromExpression (Node.value left))
                (fromExpression (Node.value right))

        ConcreteExpression.TupledExpression elements ->
            AbstractExpression.TupledExpression
                (expressionsFromSeparated elements)

        ConcreteExpression.LambdaExpression lambda ->
            AbstractExpression.LambdaExpression
                (patternsFromConcrete lambda.arguments)
                (fromExpression (Node.value lambda.expression))

        ConcreteExpression.CaseExpression caseBlock ->
            AbstractExpression.CaseExpression
                (fromExpression (Node.value caseBlock.expression))
                (casesFromConcrete caseBlock.cases)

        ConcreteExpression.LetExpression letBlock ->
            AbstractExpression.LetExpression
                (letDeclarationsFromConcrete letBlock.declarations)
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
    , generics = nodeStringValues concrete.generics
    , typeAnnotation =
        fromTypeAnnotation (Node.value concrete.typeAnnotation)
    }


choiceTypeFromConcrete : ConcreteDeclaration.ChoiceStruct -> AbstractDeclaration.ChoiceTypeStruct
choiceTypeFromConcrete concrete =
    { name = Node.value concrete.name
    , generics = nodeStringValues concrete.generics
    , constructors = valueConstructorsFromConcrete concrete.constructors
    }


valueConstructorFromConcrete : ConcreteDeclaration.ValueConstructor -> AbstractDeclaration.ValueConstructor
valueConstructorFromConcrete concrete =
    { name = Node.value concrete.name
    , arguments = typeAnnotationsFromConcrete concrete.arguments
    }


signatureFromConcrete : ConcreteExpression.Signature -> AbstractExpression.Signature
signatureFromConcrete concrete =
    { name = Node.value concrete.name
    , typeAnnotation =
        fromTypeAnnotation (Node.value concrete.typeAnnotation)
    }


recordDefinitionFromConcrete : ConcreteTypeAnnotation.RecordDefinition -> AbstractTypeAnnotation.RecordDefinition
recordDefinitionFromConcrete definition =
    recordFieldsFromConcrete definition


functionImplementationFromConcrete : ConcreteExpression.FunctionImplementation -> AbstractExpression.FunctionImplementation
functionImplementationFromConcrete concrete =
    { name = Node.value concrete.name
    , arguments = patternsFromConcrete concrete.arguments
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
    sortRecordSetters (unsortedRecordSettersFromConcrete fields)


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

        parsedAbsolute =
            if String.startsWith "0x" absolute || String.startsWith "0X" absolute then
                hexStringToInt (String.dropLeft 2 absolute)

            else
                String.toInt absolute
    in
    case parsedAbsolute of
        Just value ->
            sign * value

        Nothing ->
            0


parseFloatLiteral : String -> Float
parseFloatLiteral literalText =
    case String.toFloat (String.trim literalText) of
        Just value ->
            value

        Nothing ->
            0


stripLeadingDot : String -> String
stripLeadingDot functionName =
    case String.uncons functionName of
        Just ( '.', fieldName ) ->
            fieldName

        _ ->
            functionName


hexStringToInt : String -> Maybe Int
hexStringToInt string =
    hexStringToIntHelp 0 string


hexStringToIntHelp : Int -> String -> Maybe Int
hexStringToIntHelp value remaining =
    case String.uncons remaining of
        Just ( char, rest ) ->
            case hexDigit char of
                Just digit ->
                    hexStringToIntHelp (value * 16 + digit) rest

                Nothing ->
                    Nothing

        Nothing ->
            Just value


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


importsFromConcrete : List (Node.Node ConcreteImport.Import) -> List AbstractImport.Import
importsFromConcrete imports =
    case imports of
        importNode :: rest ->
            fromImport (Node.value importNode) :: importsFromConcrete rest

        [] ->
            []


declarationsFromConcrete : List (Node.Node ConcreteDeclaration.Declaration) -> List AbstractDeclaration.Declaration
declarationsFromConcrete declarations =
    case declarations of
        declaration :: rest ->
            fromDeclaration (Node.value declaration) :: declarationsFromConcrete rest

        [] ->
            []


maybeNodeValue : Maybe (Node.Node a) -> Maybe a
maybeNodeValue maybeNode =
    case maybeNode of
        Just node ->
            Just (Node.value node)

        Nothing ->
            Nothing


topLevelExposesFromConcrete : SeparatedSyntaxList.SeparatedSyntaxList (Node.Node ConcreteExposing.TopLevelExpose) -> List AbstractExposing.TopLevelExpose
topLevelExposesFromConcrete exposes =
    case exposes of
        SeparatedSyntaxList.Empty ->
            []

        SeparatedSyntaxList.NonEmpty first rest ->
            topLevelExposeFromConcrete (Node.value first)
                :: topLevelExposesFromConcreteRest rest


topLevelExposesFromConcreteRest : List ( a, Node.Node ConcreteExposing.TopLevelExpose ) -> List AbstractExposing.TopLevelExpose
topLevelExposesFromConcreteRest exposes =
    case exposes of
        ( _, expose ) :: rest ->
            topLevelExposeFromConcrete (Node.value expose)
                :: topLevelExposesFromConcreteRest rest

        [] ->
            []


typeAnnotationsFromConcrete : List (Node.Node ConcreteTypeAnnotation.TypeAnnotation) -> List AbstractTypeAnnotation.TypeAnnotation
typeAnnotationsFromConcrete annotations =
    case annotations of
        annotation :: rest ->
            fromTypeAnnotation (Node.value annotation) :: typeAnnotationsFromConcrete rest

        [] ->
            []


typeAnnotationsFromSeparated : SeparatedSyntaxList.SeparatedSyntaxList (Node.Node ConcreteTypeAnnotation.TypeAnnotation) -> List AbstractTypeAnnotation.TypeAnnotation
typeAnnotationsFromSeparated annotations =
    case annotations of
        SeparatedSyntaxList.Empty ->
            []

        SeparatedSyntaxList.NonEmpty first rest ->
            fromTypeAnnotation (Node.value first)
                :: typeAnnotationsFromSeparatedRest rest


typeAnnotationsFromSeparatedRest : List ( a, Node.Node ConcreteTypeAnnotation.TypeAnnotation ) -> List AbstractTypeAnnotation.TypeAnnotation
typeAnnotationsFromSeparatedRest annotations =
    case annotations of
        ( _, annotation ) :: rest ->
            fromTypeAnnotation (Node.value annotation)
                :: typeAnnotationsFromSeparatedRest rest

        [] ->
            []


patternsFromConcrete : List (Node.Node ConcretePattern.Pattern) -> List AbstractPattern.Pattern
patternsFromConcrete patterns =
    case patterns of
        pattern :: rest ->
            fromPattern (Node.value pattern) :: patternsFromConcrete rest

        [] ->
            []


patternsFromSeparated : SeparatedSyntaxList.SeparatedSyntaxList (Node.Node ConcretePattern.Pattern) -> List AbstractPattern.Pattern
patternsFromSeparated patterns =
    case patterns of
        SeparatedSyntaxList.Empty ->
            []

        SeparatedSyntaxList.NonEmpty first rest ->
            fromPattern (Node.value first) :: patternsFromSeparatedRest rest


patternsFromSeparatedRest : List ( a, Node.Node ConcretePattern.Pattern ) -> List AbstractPattern.Pattern
patternsFromSeparatedRest patterns =
    case patterns of
        ( _, pattern ) :: rest ->
            fromPattern (Node.value pattern) :: patternsFromSeparatedRest rest

        [] ->
            []


nodeStringValues : List (Node.Node String) -> List String
nodeStringValues nodes =
    case nodes of
        node :: rest ->
            Node.value node :: nodeStringValues rest

        [] ->
            []


nodeStringValuesFromSeparated : SeparatedSyntaxList.SeparatedSyntaxList (Node.Node String) -> List String
nodeStringValuesFromSeparated nodes =
    case nodes of
        SeparatedSyntaxList.Empty ->
            []

        SeparatedSyntaxList.NonEmpty first rest ->
            Node.value first :: nodeStringValuesFromSeparatedRest rest


nodeStringValuesFromSeparatedRest : List ( a, Node.Node String ) -> List String
nodeStringValuesFromSeparatedRest nodes =
    case nodes of
        ( _, node ) :: rest ->
            Node.value node :: nodeStringValuesFromSeparatedRest rest

        [] ->
            []


expressionsFromConcrete : List (Node.Node ConcreteExpression.Expression) -> List AbstractExpression.Expression
expressionsFromConcrete expressions =
    case expressions of
        expression :: rest ->
            fromExpression (Node.value expression) :: expressionsFromConcrete rest

        [] ->
            []


expressionsFromSeparated : SeparatedSyntaxList.SeparatedSyntaxList (Node.Node ConcreteExpression.Expression) -> List AbstractExpression.Expression
expressionsFromSeparated expressions =
    case expressions of
        SeparatedSyntaxList.Empty ->
            []

        SeparatedSyntaxList.NonEmpty first rest ->
            fromExpression (Node.value first) :: expressionsFromSeparatedRest rest


expressionsFromSeparatedRest : List ( a, Node.Node ConcreteExpression.Expression ) -> List AbstractExpression.Expression
expressionsFromSeparatedRest expressions =
    case expressions of
        ( _, expression ) :: rest ->
            fromExpression (Node.value expression) :: expressionsFromSeparatedRest rest

        [] ->
            []


casesFromConcrete : List ConcreteExpression.Case -> List AbstractExpression.Case
casesFromConcrete cases =
    case cases of
        case_ :: rest ->
            caseFromConcrete case_ :: casesFromConcrete rest

        [] ->
            []


letDeclarationsFromConcrete : List (Node.Node ConcreteExpression.LetDeclaration) -> List AbstractExpression.LetDeclaration
letDeclarationsFromConcrete declarations =
    case declarations of
        declaration :: rest ->
            letDeclarationFromConcrete (Node.value declaration)
                :: letDeclarationsFromConcrete rest

        [] ->
            []


valueConstructorsFromConcrete : SeparatedSyntaxList.SeparatedSyntaxList (Node.Node ConcreteDeclaration.ValueConstructor) -> List AbstractDeclaration.ValueConstructor
valueConstructorsFromConcrete constructors =
    case constructors of
        SeparatedSyntaxList.Empty ->
            []

        SeparatedSyntaxList.NonEmpty first rest ->
            valueConstructorFromConcrete (Node.value first)
                :: valueConstructorsFromConcreteRest rest


valueConstructorsFromConcreteRest : List ( a, Node.Node ConcreteDeclaration.ValueConstructor ) -> List AbstractDeclaration.ValueConstructor
valueConstructorsFromConcreteRest constructors =
    case constructors of
        ( _, constructor ) :: rest ->
            valueConstructorFromConcrete (Node.value constructor)
                :: valueConstructorsFromConcreteRest rest

        [] ->
            []


recordFieldsFromConcrete : ConcreteTypeAnnotation.RecordDefinition -> AbstractTypeAnnotation.RecordDefinition
recordFieldsFromConcrete fields =
    case fields of
        SeparatedSyntaxList.Empty ->
            []

        SeparatedSyntaxList.NonEmpty first rest ->
            recordFieldFromConcrete (Node.value first) :: recordFieldsFromConcreteRest rest


recordFieldsFromConcreteRest : List ( a, Node.Node ConcreteTypeAnnotation.RecordField ) -> AbstractTypeAnnotation.RecordDefinition
recordFieldsFromConcreteRest fields =
    case fields of
        ( _, field ) :: rest ->
            recordFieldFromConcrete (Node.value field) :: recordFieldsFromConcreteRest rest

        [] ->
            []


recordFieldFromConcrete : ConcreteTypeAnnotation.RecordField -> AbstractTypeAnnotation.RecordField
recordFieldFromConcrete field =
    { fieldName = Node.value field.fieldName
    , fieldType = fromTypeAnnotation (Node.value field.fieldType)
    }


unsortedRecordSettersFromConcrete : SeparatedSyntaxList.SeparatedSyntaxList ConcreteExpression.RecordExprField -> List AbstractExpression.RecordSetter
unsortedRecordSettersFromConcrete fields =
    case fields of
        SeparatedSyntaxList.Empty ->
            []

        SeparatedSyntaxList.NonEmpty first rest ->
            recordSetterFromConcrete first :: recordSettersFromConcreteRest rest


recordSettersFromConcreteRest : List ( a, ConcreteExpression.RecordExprField ) -> List AbstractExpression.RecordSetter
recordSettersFromConcreteRest fields =
    case fields of
        ( _, field ) :: rest ->
            recordSetterFromConcrete field :: recordSettersFromConcreteRest rest

        [] ->
            []


recordSetterFromConcrete : ConcreteExpression.RecordExprField -> AbstractExpression.RecordSetter
recordSetterFromConcrete field =
    { fieldName = Node.value field.fieldName
    , value = fromExpression (Node.value field.valueExpr)
    }


sortRecordSetters : List AbstractExpression.RecordSetter -> List AbstractExpression.RecordSetter
sortRecordSetters setters =
    case setters of
        [] ->
            []

        [ _ ] ->
            setters

        _ ->
            let
                ( left, right ) =
                    splitRecordSetters (List.length setters // 2) setters []
            in
            mergeRecordSetters (sortRecordSetters left) (sortRecordSetters right)


splitRecordSetters : Int -> List AbstractExpression.RecordSetter -> List AbstractExpression.RecordSetter -> ( List AbstractExpression.RecordSetter, List AbstractExpression.RecordSetter )
splitRecordSetters remainingCount remaining leftRev =
    if remainingCount <= 0 then
        ( List.reverse leftRev, remaining )

    else
        case remaining of
            setter :: rest ->
                splitRecordSetters (remainingCount - 1) rest (setter :: leftRev)

            [] ->
                ( List.reverse leftRev, [] )


mergeRecordSetters : List AbstractExpression.RecordSetter -> List AbstractExpression.RecordSetter -> List AbstractExpression.RecordSetter
mergeRecordSetters left right =
    case ( left, right ) of
        ( [], _ ) ->
            right

        ( _, [] ) ->
            left

        ( leftFirst :: leftRest, rightFirst :: rightRest ) ->
            if leftFirst.fieldName <= rightFirst.fieldName then
                leftFirst :: mergeRecordSetters leftRest right

            else
                rightFirst :: mergeRecordSetters left rightRest
