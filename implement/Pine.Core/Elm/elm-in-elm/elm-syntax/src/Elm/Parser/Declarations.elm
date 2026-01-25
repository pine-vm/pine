module Elm.Parser.Declarations exposing (declaration)

import Elm.Parser.Comments as Comments
import Elm.Parser.Expression
import Elm.Parser.Layout as Layout
import Elm.Parser.Patterns as Patterns
import Elm.Parser.Tokens as Tokens
import Elm.Parser.TypeAnnotation as TypeAnnotation
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Expression)
import Elm.Syntax.Infix as Infix
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Elm.Syntax.Range exposing (Location, Range)
import Elm.Syntax.Signature exposing (Signature)
import Elm.Syntax.Type exposing (ValueConstructor)
import Elm.Syntax.TypeAnnotation exposing (TypeAnnotation)
import ParserFast exposing (Parser)
import ParserWithComments exposing (Comments, WithComments(..))
import Rope


declaration : Parser (WithComments (Node Declaration))
declaration =
    ParserFast.oneOf5
        functionDeclarationWithoutDocumentation
        declarationWithDocumentation
        typeOrTypeAliasDefinitionWithoutDocumentation
        portDeclarationWithoutDocumentation
        infixDeclaration


declarationWithDocumentation : Parser (WithComments (Node Declaration))
declarationWithDocumentation =
    ParserFast.map2
        (\documentation afterDocumentation ->
            let
                (WithComments afterDocumentationComments afterDocumentationSyntax) =
                    afterDocumentation

                start : Location
                start =
                    (Node.range documentation).start
            in
            case afterDocumentationSyntax of
                FunctionDeclarationAfterDocumentation functionDeclarationAfterDocumentation ->
                    case functionDeclarationAfterDocumentation.signature of
                        Just signature ->
                            let
                                (Node implementationNameRange _) =
                                    signature.implementationName

                                (Node expressionRange _) =
                                    functionDeclarationAfterDocumentation.expression
                            in
                            WithComments
                                afterDocumentationComments
                                (Node { start = start, end = expressionRange.end }
                                    (Declaration.FunctionDeclaration
                                        { documentation = Just documentation
                                        , signature =
                                            Just
                                                (Node.combine Signature
                                                    functionDeclarationAfterDocumentation.startName
                                                    signature.typeAnnotation
                                                )
                                        , declaration =
                                            Node { start = implementationNameRange.start, end = expressionRange.end }
                                                { name = signature.implementationName
                                                , arguments = functionDeclarationAfterDocumentation.arguments
                                                , expression = functionDeclarationAfterDocumentation.expression
                                                }
                                        }
                                    )
                                )

                        Nothing ->
                            let
                                (Node startNameRange _) =
                                    functionDeclarationAfterDocumentation.startName

                                (Node expressionRange _) =
                                    functionDeclarationAfterDocumentation.expression
                            in
                            WithComments
                                afterDocumentationComments
                                (Node { start = start, end = expressionRange.end }
                                    (Declaration.FunctionDeclaration
                                        { documentation = Just documentation
                                        , signature = Nothing
                                        , declaration =
                                            Node { start = startNameRange.start, end = expressionRange.end }
                                                { name = functionDeclarationAfterDocumentation.startName
                                                , arguments = functionDeclarationAfterDocumentation.arguments
                                                , expression = functionDeclarationAfterDocumentation.expression
                                                }
                                        }
                                    )
                                )

                TypeDeclarationAfterDocumentation typeDeclarationAfterDocumentation ->
                    let
                        end : Location
                        end =
                            case typeDeclarationAfterDocumentation.tailVariantsReverse of
                                (Node range _) :: _ ->
                                    range.end

                                [] ->
                                    let
                                        (Node headVariantRange _) =
                                            typeDeclarationAfterDocumentation.headVariant
                                    in
                                    headVariantRange.end
                    in
                    WithComments
                        afterDocumentationComments
                        (Node { start = start, end = end }
                            (Declaration.CustomTypeDeclaration
                                { documentation = Just documentation
                                , name = typeDeclarationAfterDocumentation.name
                                , generics = typeDeclarationAfterDocumentation.parameters
                                , constructors =
                                    typeDeclarationAfterDocumentation.headVariant
                                        :: List.reverse typeDeclarationAfterDocumentation.tailVariantsReverse
                                }
                            )
                        )

                TypeAliasDeclarationAfterDocumentation typeAliasDeclarationAfterDocumentation ->
                    let
                        (Node typeAnnotationRange _) =
                            typeAliasDeclarationAfterDocumentation.typeAnnotation
                    in
                    WithComments
                        afterDocumentationComments
                        (Node { start = start, end = typeAnnotationRange.end }
                            (Declaration.AliasDeclaration
                                { documentation = Just documentation
                                , name = typeAliasDeclarationAfterDocumentation.name
                                , generics = typeAliasDeclarationAfterDocumentation.parameters
                                , typeAnnotation = typeAliasDeclarationAfterDocumentation.typeAnnotation
                                }
                            )
                        )

                PortDeclarationAfterDocumentation portDeclarationAfterName ->
                    let
                        (Node typeAnnotationRange _) =
                            portDeclarationAfterName.typeAnnotation
                    in
                    WithComments
                        (Rope.one documentation
                            |> Rope.filledPrependTo afterDocumentationComments
                        )
                        (Node
                            { start = portDeclarationAfterName.startLocation
                            , end = typeAnnotationRange.end
                            }
                            (Declaration.PortDeclaration
                                { name = portDeclarationAfterName.name
                                , typeAnnotation = portDeclarationAfterName.typeAnnotation
                                }
                            )
                        )
        )
        Comments.declarationDocumentation
        (Layout.layoutStrictFollowedByWithComments
            (ParserFast.oneOf3
                functionAfterDocumentation
                typeOrTypeAliasDefinitionAfterDocumentation
                portDeclarationAfterDocumentation
            )
        )
        |> ParserFast.validate
            (\result ->
                let
                    (WithComments _ (Node _ decl)) =
                        result
                in
                case decl of
                    Declaration.FunctionDeclaration letFunctionDeclaration ->
                        case letFunctionDeclaration.signature of
                            Nothing ->
                                True

                            Just (Node _ signature) ->
                                let
                                    (Node _ implementationName) =
                                        implementation.name

                                    (Node _ implementation) =
                                        letFunctionDeclaration.declaration

                                    (Node _ signatureName) =
                                        signature.name
                                in
                                implementationName == signatureName ++ ""

                    _ ->
                        True
            )
            "Expected to find the same name for declaration and signature"


type DeclarationAfterDocumentation
    = FunctionDeclarationAfterDocumentation
        { startName : Node String
        , signature :
            Maybe
                { typeAnnotation : Node TypeAnnotation
                , implementationName : Node String
                }
        , arguments : List (Node Pattern)
        , expression : Node Expression
        }
    | TypeDeclarationAfterDocumentation
        { name : Node String
        , parameters : List (Node String)
        , headVariant : Node ValueConstructor
        , tailVariantsReverse : List (Node ValueConstructor)
        }
    | TypeAliasDeclarationAfterDocumentation
        { name : Node String
        , parameters : List (Node String)
        , typeAnnotation : Node TypeAnnotation
        }
    | PortDeclarationAfterDocumentation
        { startLocation : Location
        , name : Node String
        , typeAnnotation : Node TypeAnnotation
        }


type TypeOrTypeAliasDeclarationWithoutDocumentation
    = TypeDeclarationWithoutDocumentation
        { name : Node String
        , parameters : List (Node String)
        , headVariant : Node ValueConstructor
        , tailVariantsReverse : List (Node ValueConstructor)
        }
    | TypeAliasDeclarationWithoutDocumentation
        { name : Node String
        , parameters : List (Node String)
        , typeAnnotation : Node TypeAnnotation
        }


functionAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
functionAfterDocumentation =
    ParserFast.map6
        (\startName commentsAfterStartName maybeSignature arguments commentsAfterEqual result ->
            let
                (WithComments argumentsComments argumentsSyntax) =
                    arguments

                (WithComments resultComments resultSyntax) =
                    result

                maybeSignatureSyntax =
                    case maybeSignature of
                        Nothing ->
                            Nothing

                        Just (WithComments signatureComments signatureSyntax) ->
                            Just signatureSyntax
            in
            WithComments
                ((case maybeSignature of
                    Nothing ->
                        commentsAfterStartName

                    Just (WithComments signatureComments _) ->
                        commentsAfterStartName |> Rope.prependTo signatureComments
                 )
                    |> Rope.prependTo argumentsComments
                    |> Rope.prependTo commentsAfterEqual
                    |> Rope.prependTo resultComments
                )
                (FunctionDeclarationAfterDocumentation
                    { startName = startName
                    , signature = maybeSignatureSyntax
                    , arguments = argumentsSyntax
                    , expression = resultSyntax
                    }
                )
        )
        -- infix declarations itself don't have documentation
        Tokens.functionNameNode
        Layout.maybeLayout
        (ParserFast.map4OrSucceed
            (\commentsBeforeTypeAnnotation typeAnnotationResult implementationName afterImplementationName ->
                let
                    (WithComments typeAnnotationComments typeAnnotationSyntax) =
                        typeAnnotationResult

                    (WithComments implementationNameComments implementationNameSyntax) =
                        implementationName
                in
                Just
                    (WithComments
                        (commentsBeforeTypeAnnotation
                            |> Rope.prependTo typeAnnotationComments
                            |> Rope.prependTo implementationNameComments
                            |> Rope.prependTo afterImplementationName
                        )
                        { implementationName = implementationNameSyntax
                        , typeAnnotation = typeAnnotationSyntax
                        }
                    )
            )
            (ParserFast.symbolFollowedBy ":" Layout.maybeLayout)
            TypeAnnotation.typeAnnotation
            (Layout.layoutStrictFollowedBy
                Tokens.functionNameNode
            )
            Layout.maybeLayout
            Nothing
        )
        parameterPatternsEqual
        Layout.maybeLayout
        Elm.Parser.Expression.expression


functionDeclarationWithoutDocumentation : Parser (WithComments (Node Declaration))
functionDeclarationWithoutDocumentation =
    ParserFast.map6WithStartLocation
        (\startNameStart startNameNode commentsAfterStartName maybeSignature arguments commentsAfterEqual result ->
            let
                (WithComments argumentsComments argumentsSyntax) =
                    arguments

                (WithComments resultComments resultSyntax) =
                    result

                allComments : Comments
                allComments =
                    (case maybeSignature of
                        Nothing ->
                            commentsAfterStartName

                        Just signature ->
                            commentsAfterStartName |> Rope.prependTo signature.comments
                    )
                        |> Rope.prependTo argumentsComments
                        |> Rope.prependTo commentsAfterEqual
                        |> Rope.prependTo resultComments
            in
            case maybeSignature of
                Nothing ->
                    let
                        (Node expressionRange _) =
                            resultSyntax
                    in
                    WithComments
                        allComments
                        (Node { start = startNameStart, end = expressionRange.end }
                            (Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = startNameStart, end = expressionRange.end }
                                        { name = startNameNode
                                        , arguments = argumentsSyntax
                                        , expression = resultSyntax
                                        }
                                }
                            )
                        )

                Just signature ->
                    let
                        (Node implementationNameRange _) =
                            signature.implementationName

                        (Node expressionRange _) =
                            resultSyntax
                    in
                    WithComments
                        allComments
                        (Node { start = startNameStart, end = expressionRange.end }
                            (Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Just (Node.combine Signature startNameNode signature.typeAnnotation)
                                , declaration =
                                    Node { start = implementationNameRange.start, end = expressionRange.end }
                                        { name = signature.implementationName
                                        , arguments = argumentsSyntax
                                        , expression = resultSyntax
                                        }
                                }
                            )
                        )
        )
        Tokens.functionNameNotInfixNode
        Layout.maybeLayout
        (ParserFast.map4OrSucceed
            (\commentsBeforeTypeAnnotation typeAnnotationResult implementationName afterImplementationName ->
                let
                    (WithComments typeAnnotationComments typeAnnotationSyntax) =
                        typeAnnotationResult

                    (WithComments implementationNameComments implementationNameSyntax) =
                        implementationName
                in
                Just
                    { comments =
                        commentsBeforeTypeAnnotation
                            |> Rope.prependTo typeAnnotationComments
                            |> Rope.prependTo implementationNameComments
                            |> Rope.prependTo afterImplementationName
                    , implementationName = implementationNameSyntax
                    , typeAnnotation = typeAnnotationSyntax
                    }
            )
            (ParserFast.symbolFollowedBy ":" Layout.maybeLayout)
            TypeAnnotation.typeAnnotation
            (Layout.layoutStrictFollowedBy
                Tokens.functionNameNode
            )
            Layout.maybeLayout
            Nothing
        )
        parameterPatternsEqual
        Layout.maybeLayout
        Elm.Parser.Expression.expression
        |> ParserFast.validate
            (\result ->
                let
                    (WithComments _ (Node _ decl)) =
                        result
                in
                case decl of
                    Declaration.FunctionDeclaration letFunctionDeclaration ->
                        case letFunctionDeclaration.signature of
                            Nothing ->
                                True

                            Just (Node _ signature) ->
                                let
                                    (Node _ implementationName) =
                                        implementation.name

                                    (Node _ implementation) =
                                        letFunctionDeclaration.declaration

                                    (Node _ signatureName) =
                                        signature.name
                                in
                                implementationName == signatureName ++ ""

                    _ ->
                        True
            )
            "Expected to find the same name for declaration and signature"


parameterPatternsEqual : Parser (WithComments (List (Node Pattern)))
parameterPatternsEqual =
    ParserWithComments.until Tokens.equal
        (ParserFast.map2
            (\patternResult commentsAfterPattern ->
                let
                    (WithComments patternComments patternSyntax) =
                        patternResult
                in
                WithComments
                    (patternComments |> Rope.prependTo commentsAfterPattern)
                    patternSyntax
            )
            Patterns.patternNotDirectlyComposing
            Layout.maybeLayout
        )


infixDeclaration : Parser (WithComments (Node Declaration))
infixDeclaration =
    ParserFast.map9WithRange
        (\range commentsAfterInfix direction commentsAfterDirection precedence commentsAfterPrecedence operator commentsAfterOperator commentsAfterEqual fn ->
            WithComments
                (commentsAfterInfix
                    |> Rope.prependTo commentsAfterDirection
                    |> Rope.prependTo commentsAfterPrecedence
                    |> Rope.prependTo commentsAfterOperator
                    |> Rope.prependTo commentsAfterEqual
                )
                (Node range
                    (Declaration.InfixDeclaration
                        { direction = direction, precedence = precedence, operator = operator, function = fn }
                    )
                )
        )
        (ParserFast.keywordFollowedBy "infix" Layout.maybeLayout)
        infixDirection
        Layout.maybeLayout
        (ParserFast.integerDecimalMapWithRange Node)
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy "("
            (ParserFast.whileWithoutLinebreakAnd2PartUtf16ValidateMapWithRangeBacktrackableFollowedBySymbol
                (\operatorRange operator ->
                    Node
                        { start = { row = operatorRange.start.row, column = operatorRange.start.column - 1 }
                        , end = { row = operatorRange.end.row, column = operatorRange.end.column + 1 }
                        }
                        operator
                )
                Tokens.isOperatorSymbolChar
                Tokens.isAllowedOperatorToken
                ")"
            )
        )
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy "=" Layout.maybeLayout)
        Tokens.functionNameNode


infixDirection : ParserFast.Parser (Node Infix.InfixDirection)
infixDirection =
    ParserFast.oneOf3
        (ParserFast.mapWithRange Node (ParserFast.keyword "right" Infix.Right))
        (ParserFast.mapWithRange Node (ParserFast.keyword "left" Infix.Left))
        (ParserFast.mapWithRange Node (ParserFast.keyword "non" Infix.Non))


portDeclarationAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
portDeclarationAfterDocumentation =
    ParserFast.map5
        (\commentsAfterPort ((Node nameRange _) as name) commentsAfterName commentsAfterColon typeAnnotationResult ->
            let
                (WithComments typeAnnotationComments typeAnnotationSyntax) =
                    typeAnnotationResult
            in
            WithComments
                (commentsAfterPort
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo typeAnnotationComments
                    |> Rope.prependTo commentsAfterColon
                )
                (PortDeclarationAfterDocumentation
                    { startLocation = { row = nameRange.start.row, column = 1 }
                    , name = name
                    , typeAnnotation = typeAnnotationSyntax
                    }
                )
        )
        (ParserFast.keywordFollowedBy "port" Layout.maybeLayout)
        Tokens.functionNameNode
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy ":" Layout.maybeLayout)
        TypeAnnotation.typeAnnotation


portDeclarationWithoutDocumentation : Parser (WithComments (Node Declaration))
portDeclarationWithoutDocumentation =
    ParserFast.map5
        (\commentsAfterPort ((Node nameRange _) as name) commentsAfterName commentsAfterColon typeAnnotationResult ->
            let
                (WithComments typeAnnotationComments typeAnnotationSyntax) =
                    typeAnnotationResult

                (Node { end } _) =
                    typeAnnotationSyntax
            in
            WithComments
                (commentsAfterPort
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo commentsAfterColon
                    |> Rope.prependTo typeAnnotationComments
                )
                (Node
                    { start = { row = nameRange.start.row, column = 1 }
                    , end = end
                    }
                    (Declaration.PortDeclaration
                        { name = name
                        , typeAnnotation = typeAnnotationSyntax
                        }
                    )
                )
        )
        (ParserFast.keywordFollowedBy "port" Layout.maybeLayout)
        Tokens.functionNameNode
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy ":" Layout.maybeLayout)
        TypeAnnotation.typeAnnotation


typeOrTypeAliasDefinitionAfterDocumentation : Parser (WithComments DeclarationAfterDocumentation)
typeOrTypeAliasDefinitionAfterDocumentation =
    ParserFast.map2
        (\commentsAfterType declarationAfterDocumentation ->
            let
                (WithComments declarationAfterDocumentationComments declarationAfterDocumentationSyntax) =
                    declarationAfterDocumentation
            in
            WithComments
                (commentsAfterType |> Rope.prependTo declarationAfterDocumentationComments)
                declarationAfterDocumentationSyntax
        )
        (ParserFast.keywordFollowedBy "type" Layout.maybeLayout)
        (ParserFast.oneOf2
            typeAliasDefinitionAfterDocumentationAfterTypePrefix
            customTypeDefinitionAfterDocumentationAfterTypePrefix
        )


typeAliasDefinitionAfterDocumentationAfterTypePrefix : Parser (WithComments DeclarationAfterDocumentation)
typeAliasDefinitionAfterDocumentationAfterTypePrefix =
    ParserFast.map6
        (\commentsAfterAlias name commentsAfterName parameters commentsAfterEquals typeAnnotationResult ->
            let
                (WithComments parametersComments parametersSyntax) =
                    parameters

                (WithComments typeAnnotationComments typeAnnotationSyntax) =
                    typeAnnotationResult
            in
            WithComments
                (commentsAfterAlias
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo parametersComments
                    |> Rope.prependTo commentsAfterEquals
                    |> Rope.prependTo typeAnnotationComments
                )
                (TypeAliasDeclarationAfterDocumentation
                    { name = name
                    , parameters = parametersSyntax
                    , typeAnnotation = typeAnnotationSyntax
                    }
                )
        )
        (ParserFast.keywordFollowedBy "alias" Layout.maybeLayout)
        Tokens.typeNameNode
        Layout.maybeLayout
        typeGenericListEquals
        Layout.maybeLayout
        TypeAnnotation.typeAnnotation


customTypeDefinitionAfterDocumentationAfterTypePrefix : Parser (WithComments DeclarationAfterDocumentation)
customTypeDefinitionAfterDocumentationAfterTypePrefix =
    ParserFast.map6
        (\name commentsAfterName parameters commentsAfterEqual headVariant tailVariantsReverse ->
            let
                (WithComments parametersComments parametersSyntax) =
                    parameters

                (WithComments headVariantComments headVariantSyntax) =
                    headVariant

                (WithComments tailVariantsReverseComments tailVariantsReverseSyntax) =
                    tailVariantsReverse
            in
            WithComments
                (commentsAfterName
                    |> Rope.prependTo parametersComments
                    |> Rope.prependTo commentsAfterEqual
                    |> Rope.prependTo headVariantComments
                    |> Rope.prependTo tailVariantsReverseComments
                )
                (TypeDeclarationAfterDocumentation
                    { name = name
                    , parameters = parametersSyntax
                    , headVariant = headVariantSyntax
                    , tailVariantsReverse = tailVariantsReverseSyntax
                    }
                )
        )
        Tokens.typeNameNode
        Layout.maybeLayout
        typeGenericListEquals
        Layout.maybeLayout
        valueConstructorOptimisticLayout
        (ParserWithComments.manyWithoutReverse
            (ParserFast.symbolFollowedBy "|"
                (Layout.positivelyIndentedPlusFollowedBy 1
                    (ParserFast.map2
                        (\commentsBeforePipe variantResult ->
                            let
                                (WithComments variantComments variantSyntax) =
                                    variantResult
                            in
                            WithComments
                                (commentsBeforePipe
                                    |> Rope.prependTo variantComments
                                )
                                variantSyntax
                        )
                        Layout.maybeLayout
                        valueConstructorOptimisticLayout
                    )
                )
            )
        )


typeOrTypeAliasDefinitionWithoutDocumentation : Parser (WithComments (Node Declaration.Declaration))
typeOrTypeAliasDefinitionWithoutDocumentation =
    ParserFast.map2WithStartLocation
        (\start commentsAfterType afterStart ->
            let
                (WithComments afterStartComments afterStartSyntax) =
                    afterStart

                allComments : Comments
                allComments =
                    commentsAfterType |> Rope.prependTo afterStartComments
            in
            case afterStartSyntax of
                TypeDeclarationWithoutDocumentation typeDeclarationAfterDocumentation ->
                    let
                        end : Location
                        end =
                            case typeDeclarationAfterDocumentation.tailVariantsReverse of
                                (Node range _) :: _ ->
                                    range.end

                                [] ->
                                    let
                                        (Node headVariantRange _) =
                                            typeDeclarationAfterDocumentation.headVariant
                                    in
                                    headVariantRange.end
                    in
                    WithComments
                        allComments
                        (Node { start = start, end = end }
                            (Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = typeDeclarationAfterDocumentation.name
                                , generics = typeDeclarationAfterDocumentation.parameters
                                , constructors =
                                    typeDeclarationAfterDocumentation.headVariant
                                        :: List.reverse typeDeclarationAfterDocumentation.tailVariantsReverse
                                }
                            )
                        )

                TypeAliasDeclarationWithoutDocumentation typeAliasDeclarationAfterDocumentation ->
                    let
                        (Node typeAnnotationRange _) =
                            typeAliasDeclarationAfterDocumentation.typeAnnotation
                    in
                    WithComments
                        allComments
                        (Node { start = start, end = typeAnnotationRange.end }
                            (Declaration.AliasDeclaration
                                { documentation = Nothing
                                , name = typeAliasDeclarationAfterDocumentation.name
                                , generics = typeAliasDeclarationAfterDocumentation.parameters
                                , typeAnnotation = typeAliasDeclarationAfterDocumentation.typeAnnotation
                                }
                            )
                        )
        )
        (ParserFast.keywordFollowedBy "type"
            Layout.maybeLayout
        )
        (ParserFast.oneOf2
            typeAliasDefinitionWithoutDocumentationAfterTypePrefix
            customTypeDefinitionWithoutDocumentationAfterTypePrefix
        )


typeAliasDefinitionWithoutDocumentationAfterTypePrefix : Parser (WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
typeAliasDefinitionWithoutDocumentationAfterTypePrefix =
    ParserFast.map6
        (\commentsAfterAlias name commentsAfterName parameters commentsAfterEqual typeAnnotationResult ->
            let
                (WithComments parametersComments parametersSyntax) =
                    parameters

                (WithComments typeAnnotationComments typeAnnotationSyntax) =
                    typeAnnotationResult
            in
            WithComments
                (commentsAfterAlias
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo parametersComments
                    |> Rope.prependTo commentsAfterEqual
                    |> Rope.prependTo typeAnnotationComments
                )
                (TypeAliasDeclarationWithoutDocumentation
                    { name = name
                    , parameters = parametersSyntax
                    , typeAnnotation = typeAnnotationSyntax
                    }
                )
        )
        (ParserFast.keywordFollowedBy "alias" Layout.maybeLayout)
        Tokens.typeNameNode
        Layout.maybeLayout
        typeGenericListEquals
        Layout.maybeLayout
        TypeAnnotation.typeAnnotation


customTypeDefinitionWithoutDocumentationAfterTypePrefix : Parser (WithComments TypeOrTypeAliasDeclarationWithoutDocumentation)
customTypeDefinitionWithoutDocumentationAfterTypePrefix =
    ParserFast.map6
        (\name commentsAfterName parameters commentsAfterEqual headVariant tailVariantsReverse ->
            let
                (WithComments parametersComments parametersSyntax) =
                    parameters

                (WithComments headVariantComments headVariantSyntax) =
                    headVariant

                (WithComments tailVariantsReverseComments tailVariantsReverseSyntax) =
                    tailVariantsReverse
            in
            WithComments
                (commentsAfterName
                    |> Rope.prependTo parametersComments
                    |> Rope.prependTo commentsAfterEqual
                    |> Rope.prependTo headVariantComments
                    |> Rope.prependTo tailVariantsReverseComments
                )
                (TypeDeclarationWithoutDocumentation
                    { name = name
                    , parameters = parametersSyntax
                    , headVariant = headVariantSyntax
                    , tailVariantsReverse = tailVariantsReverseSyntax
                    }
                )
        )
        Tokens.typeNameNode
        Layout.maybeLayout
        typeGenericListEquals
        Layout.maybeLayout
        valueConstructorOptimisticLayout
        (ParserWithComments.manyWithoutReverse
            (ParserFast.symbolFollowedBy "|"
                (Layout.positivelyIndentedPlusFollowedBy 1
                    (ParserFast.map2
                        (\commentsBeforePipe variantResult ->
                            let
                                (WithComments variantComments variantSyntax) =
                                    variantResult
                            in
                            WithComments
                                (commentsBeforePipe
                                    |> Rope.prependTo variantComments
                                )
                                variantSyntax
                        )
                        Layout.maybeLayout
                        valueConstructorOptimisticLayout
                    )
                )
            )
        )


valueConstructorOptimisticLayout : Parser (WithComments (Node ValueConstructor))
valueConstructorOptimisticLayout =
    ParserFast.map3
        (\((Node nameRange _) as name) commentsAfterName argumentsReverse ->
            let
                (WithComments argumentsReverseComments argumentsReverseSyntax) =
                    argumentsReverse

                fullRange : Range
                fullRange =
                    case argumentsReverseSyntax of
                        (Node lastArgRange _) :: _ ->
                            { start = nameRange.start, end = lastArgRange.end }

                        [] ->
                            nameRange
            in
            WithComments
                (commentsAfterName
                    |> Rope.prependTo argumentsReverseComments
                )
                (Node fullRange
                    { name = name
                    , arguments = List.reverse argumentsReverseSyntax
                    }
                )
        )
        Tokens.typeNameNode
        Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            (Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\typeAnnotationResult commentsAfter ->
                        let
                            (WithComments typeAnnotationComments typeAnnotationSyntax) =
                                typeAnnotationResult
                        in
                        WithComments
                            (typeAnnotationComments |> Rope.prependTo commentsAfter)
                            typeAnnotationSyntax
                    )
                    TypeAnnotation.typeAnnotationNoFnExcludingTypedWithArguments
                    Layout.optimisticLayout
                )
            )
        )


typeGenericListEquals : Parser (WithComments (List (Node String)))
typeGenericListEquals =
    ParserWithComments.until Tokens.equal
        (ParserFast.map2
            (\name commentsAfterName ->
                WithComments
                    commentsAfterName
                    name
            )
            Tokens.functionNameNode
            Layout.maybeLayout
        )
