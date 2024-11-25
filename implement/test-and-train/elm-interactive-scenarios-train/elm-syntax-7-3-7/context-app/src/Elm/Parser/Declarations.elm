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
import ParserWithComments exposing (Comments, WithComments)
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
                start : Location
                start =
                    (Node.range documentation).start
            in
            case afterDocumentation.syntax of
                FunctionDeclarationAfterDocumentation functionDeclarationAfterDocumentation ->
                    case functionDeclarationAfterDocumentation.signature of
                        Just signature ->
                            let
                                (Node implementationNameRange _) =
                                    signature.implementationName

                                (Node expressionRange _) =
                                    functionDeclarationAfterDocumentation.expression
                            in
                            { comments = afterDocumentation.comments
                            , syntax =
                                Node { start = start, end = expressionRange.end }
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
                            }

                        Nothing ->
                            let
                                (Node startNameRange _) =
                                    functionDeclarationAfterDocumentation.startName

                                (Node expressionRange _) =
                                    functionDeclarationAfterDocumentation.expression
                            in
                            { comments = afterDocumentation.comments
                            , syntax =
                                Node { start = start, end = expressionRange.end }
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
                            }

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
                    { comments = afterDocumentation.comments
                    , syntax =
                        Node { start = start, end = end }
                            (Declaration.CustomTypeDeclaration
                                { documentation = Just documentation
                                , name = typeDeclarationAfterDocumentation.name
                                , generics = typeDeclarationAfterDocumentation.parameters
                                , constructors =
                                    typeDeclarationAfterDocumentation.headVariant
                                        :: List.reverse typeDeclarationAfterDocumentation.tailVariantsReverse
                                }
                            )
                    }

                TypeAliasDeclarationAfterDocumentation typeAliasDeclarationAfterDocumentation ->
                    let
                        (Node typeAnnotationRange _) =
                            typeAliasDeclarationAfterDocumentation.typeAnnotation
                    in
                    { comments = afterDocumentation.comments
                    , syntax =
                        Node { start = start, end = typeAnnotationRange.end }
                            (Declaration.AliasDeclaration
                                { documentation = Just documentation
                                , name = typeAliasDeclarationAfterDocumentation.name
                                , generics = typeAliasDeclarationAfterDocumentation.parameters
                                , typeAnnotation = typeAliasDeclarationAfterDocumentation.typeAnnotation
                                }
                            )
                    }

                PortDeclarationAfterDocumentation portDeclarationAfterName ->
                    let
                        (Node typeAnnotationRange _) =
                            portDeclarationAfterName.typeAnnotation
                    in
                    { comments =
                        Rope.one documentation
                            |> Rope.filledPrependTo afterDocumentation.comments
                    , syntax =
                        Node
                            { start = portDeclarationAfterName.startLocation
                            , end = typeAnnotationRange.end
                            }
                            (Declaration.PortDeclaration
                                { name = portDeclarationAfterName.name
                                , typeAnnotation = portDeclarationAfterName.typeAnnotation
                                }
                            )
                    }
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
                    (Node _ decl) =
                        result.syntax
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
            { comments =
                (case maybeSignature of
                    Nothing ->
                        commentsAfterStartName

                    Just signature ->
                        commentsAfterStartName |> Rope.prependTo signature.comments
                )
                    |> Rope.prependTo arguments.comments
                    |> Rope.prependTo commentsAfterEqual
                    |> Rope.prependTo result.comments
            , syntax =
                FunctionDeclarationAfterDocumentation
                    { startName = startName
                    , signature = maybeSignature |> Maybe.map .syntax
                    , arguments = arguments.syntax
                    , expression = result.syntax
                    }
            }
        )
        -- infix declarations itself don't have documentation
        Tokens.functionNameNode
        Layout.maybeLayout
        (ParserFast.map4OrSucceed
            (\commentsBeforeTypeAnnotation typeAnnotationResult implementationName afterImplementationName ->
                Just
                    { comments =
                        commentsBeforeTypeAnnotation
                            |> Rope.prependTo typeAnnotationResult.comments
                            |> Rope.prependTo implementationName.comments
                            |> Rope.prependTo afterImplementationName
                    , syntax =
                        { implementationName = implementationName.syntax
                        , typeAnnotation = typeAnnotationResult.syntax
                        }
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


functionDeclarationWithoutDocumentation : Parser (WithComments (Node Declaration))
functionDeclarationWithoutDocumentation =
    ParserFast.map6WithStartLocation
        (\startNameStart startNameNode commentsAfterStartName maybeSignature arguments commentsAfterEqual result ->
            let
                allComments : Comments
                allComments =
                    (case maybeSignature of
                        Nothing ->
                            commentsAfterStartName

                        Just signature ->
                            commentsAfterStartName |> Rope.prependTo signature.comments
                    )
                        |> Rope.prependTo arguments.comments
                        |> Rope.prependTo commentsAfterEqual
                        |> Rope.prependTo result.comments
            in
            case maybeSignature of
                Nothing ->
                    let
                        (Node expressionRange _) =
                            result.syntax
                    in
                    { comments = allComments
                    , syntax =
                        Node { start = startNameStart, end = expressionRange.end }
                            (Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Nothing
                                , declaration =
                                    Node { start = startNameStart, end = expressionRange.end }
                                        { name = startNameNode
                                        , arguments = arguments.syntax
                                        , expression = result.syntax
                                        }
                                }
                            )
                    }

                Just signature ->
                    let
                        (Node implementationNameRange _) =
                            signature.implementationName

                        (Node expressionRange _) =
                            result.syntax
                    in
                    { comments = allComments
                    , syntax =
                        Node { start = startNameStart, end = expressionRange.end }
                            (Declaration.FunctionDeclaration
                                { documentation = Nothing
                                , signature = Just (Node.combine Signature startNameNode signature.typeAnnotation)
                                , declaration =
                                    Node { start = implementationNameRange.start, end = expressionRange.end }
                                        { name = signature.implementationName
                                        , arguments = arguments.syntax
                                        , expression = result.syntax
                                        }
                                }
                            )
                    }
        )
        Tokens.functionNameNotInfixNode
        Layout.maybeLayout
        (ParserFast.map4OrSucceed
            (\commentsBeforeTypeAnnotation typeAnnotationResult implementationName afterImplementationName ->
                Just
                    { comments =
                        commentsBeforeTypeAnnotation
                            |> Rope.prependTo typeAnnotationResult.comments
                            |> Rope.prependTo implementationName.comments
                            |> Rope.prependTo afterImplementationName
                    , implementationName = implementationName.syntax
                    , typeAnnotation = typeAnnotationResult.syntax
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
                    (Node _ decl) =
                        result.syntax
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
                { comments = patternResult.comments |> Rope.prependTo commentsAfterPattern
                , syntax = patternResult.syntax
                }
            )
            Patterns.patternNotDirectlyComposing
            Layout.maybeLayout
        )


infixDeclaration : Parser (WithComments (Node Declaration))
infixDeclaration =
    ParserFast.map9WithRange
        (\range commentsAfterInfix direction commentsAfterDirection precedence commentsAfterPrecedence operator commentsAfterOperator commentsAfterEqual fn ->
            { comments =
                commentsAfterInfix
                    |> Rope.prependTo commentsAfterDirection
                    |> Rope.prependTo commentsAfterPrecedence
                    |> Rope.prependTo commentsAfterOperator
                    |> Rope.prependTo commentsAfterEqual
            , syntax =
                Node range
                    (Declaration.InfixDeclaration
                        { direction = direction, precedence = precedence, operator = operator, function = fn }
                    )
            }
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
            { comments =
                commentsAfterPort
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo typeAnnotationResult.comments
                    |> Rope.prependTo commentsAfterColon
            , syntax =
                PortDeclarationAfterDocumentation
                    { startLocation = { row = nameRange.start.row, column = 1 }
                    , name = name
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            }
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
                (Node { end } _) =
                    typeAnnotationResult.syntax
            in
            { comments =
                commentsAfterPort
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo commentsAfterColon
                    |> Rope.prependTo typeAnnotationResult.comments
            , syntax =
                Node
                    { start = { row = nameRange.start.row, column = 1 }
                    , end = end
                    }
                    (Declaration.PortDeclaration
                        { name = name
                        , typeAnnotation = typeAnnotationResult.syntax
                        }
                    )
            }
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
            { comments = commentsAfterType |> Rope.prependTo declarationAfterDocumentation.comments
            , syntax = declarationAfterDocumentation.syntax
            }
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
            { comments =
                commentsAfterAlias
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo parameters.comments
                    |> Rope.prependTo commentsAfterEquals
                    |> Rope.prependTo typeAnnotationResult.comments
            , syntax =
                TypeAliasDeclarationAfterDocumentation
                    { name = name
                    , parameters = parameters.syntax
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            }
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
            { comments =
                commentsAfterName
                    |> Rope.prependTo parameters.comments
                    |> Rope.prependTo commentsAfterEqual
                    |> Rope.prependTo headVariant.comments
                    |> Rope.prependTo tailVariantsReverse.comments
            , syntax =
                TypeDeclarationAfterDocumentation
                    { name = name
                    , parameters = parameters.syntax
                    , headVariant = headVariant.syntax
                    , tailVariantsReverse = tailVariantsReverse.syntax
                    }
            }
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
                            { comments =
                                commentsBeforePipe
                                    |> Rope.prependTo variantResult.comments
                            , syntax = variantResult.syntax
                            }
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
                allComments : Comments
                allComments =
                    commentsAfterType |> Rope.prependTo afterStart.comments
            in
            case afterStart.syntax of
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
                    { comments = allComments
                    , syntax =
                        Node { start = start, end = end }
                            (Declaration.CustomTypeDeclaration
                                { documentation = Nothing
                                , name = typeDeclarationAfterDocumentation.name
                                , generics = typeDeclarationAfterDocumentation.parameters
                                , constructors =
                                    typeDeclarationAfterDocumentation.headVariant
                                        :: List.reverse typeDeclarationAfterDocumentation.tailVariantsReverse
                                }
                            )
                    }

                TypeAliasDeclarationWithoutDocumentation typeAliasDeclarationAfterDocumentation ->
                    let
                        (Node typeAnnotationRange _) =
                            typeAliasDeclarationAfterDocumentation.typeAnnotation
                    in
                    { comments = allComments
                    , syntax =
                        Node { start = start, end = typeAnnotationRange.end }
                            (Declaration.AliasDeclaration
                                { documentation = Nothing
                                , name = typeAliasDeclarationAfterDocumentation.name
                                , generics = typeAliasDeclarationAfterDocumentation.parameters
                                , typeAnnotation = typeAliasDeclarationAfterDocumentation.typeAnnotation
                                }
                            )
                    }
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
            { comments =
                commentsAfterAlias
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo parameters.comments
                    |> Rope.prependTo commentsAfterEqual
                    |> Rope.prependTo typeAnnotationResult.comments
            , syntax =
                TypeAliasDeclarationWithoutDocumentation
                    { name = name
                    , parameters = parameters.syntax
                    , typeAnnotation = typeAnnotationResult.syntax
                    }
            }
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
            { comments =
                commentsAfterName
                    |> Rope.prependTo parameters.comments
                    |> Rope.prependTo commentsAfterEqual
                    |> Rope.prependTo headVariant.comments
                    |> Rope.prependTo tailVariantsReverse.comments
            , syntax =
                TypeDeclarationWithoutDocumentation
                    { name = name
                    , parameters = parameters.syntax
                    , headVariant = headVariant.syntax
                    , tailVariantsReverse = tailVariantsReverse.syntax
                    }
            }
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
                            { comments =
                                commentsBeforePipe
                                    |> Rope.prependTo variantResult.comments
                            , syntax = variantResult.syntax
                            }
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
                fullRange : Range
                fullRange =
                    case argumentsReverse.syntax of
                        (Node lastArgRange _) :: _ ->
                            { start = nameRange.start, end = lastArgRange.end }

                        [] ->
                            nameRange
            in
            { comments =
                commentsAfterName
                    |> Rope.prependTo argumentsReverse.comments
            , syntax =
                Node fullRange
                    { name = name
                    , arguments = List.reverse argumentsReverse.syntax
                    }
            }
        )
        Tokens.typeNameNode
        Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            (Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\typeAnnotationResult commentsAfter ->
                        { comments = typeAnnotationResult.comments |> Rope.prependTo commentsAfter
                        , syntax = typeAnnotationResult.syntax
                        }
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
                { comments = commentsAfterName
                , syntax = name
                }
            )
            Tokens.functionNameNode
            Layout.maybeLayout
        )
