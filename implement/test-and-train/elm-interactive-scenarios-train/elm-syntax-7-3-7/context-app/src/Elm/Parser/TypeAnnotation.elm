module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)

import Elm.Parser.Layout as Layout
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation)
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)
import Rope


typeAnnotation : Parser (WithComments (Node TypeAnnotation))
typeAnnotation =
    ParserFast.map3
        (\inType commentsAfterIn maybeOut ->
            { comments =
                inType.comments
                    |> Rope.prependTo commentsAfterIn
                    |> Rope.prependTo maybeOut.comments
            , syntax =
                case maybeOut.syntax of
                    Nothing ->
                        inType.syntax

                    Just out ->
                        Node.combine TypeAnnotation.FunctionTypeAnnotation inType.syntax out
            }
        )
        (ParserFast.lazy (\() -> typeAnnotationNoFnIncludingTypedWithArguments))
        Layout.optimisticLayout
        (ParserFast.map2OrSucceed
            (\commentsAfterArrow typeAnnotationResult ->
                { comments =
                    commentsAfterArrow
                        |> Rope.prependTo typeAnnotationResult.comments
                , syntax = Just typeAnnotationResult.syntax
                }
            )
            (ParserFast.symbolFollowedBy "->"
                (Layout.positivelyIndentedPlusFollowedBy 2
                    Layout.maybeLayout
                )
            )
            (ParserFast.lazy (\() -> typeAnnotation))
            { comments = Rope.empty, syntax = Nothing }
        )


typeAnnotationNoFnExcludingTypedWithArguments : Parser (WithComments (Node TypeAnnotation))
typeAnnotationNoFnExcludingTypedWithArguments =
    ParserFast.oneOf4
        parensTypeAnnotation
        typedTypeAnnotationWithoutArguments
        genericTypeAnnotation
        recordTypeAnnotation


typeAnnotationNoFnIncludingTypedWithArguments : Parser (WithComments (Node TypeAnnotation))
typeAnnotationNoFnIncludingTypedWithArguments =
    ParserFast.oneOf4
        parensTypeAnnotation
        typedTypeAnnotationWithArgumentsOptimisticLayout
        genericTypeAnnotation
        recordTypeAnnotation


parensTypeAnnotation : Parser (WithComments (Node TypeAnnotation))
parensTypeAnnotation =
    ParserFast.symbolFollowedBy "("
        (ParserFast.oneOf2
            (ParserFast.symbolWithEndLocation ")"
                (\end ->
                    { comments = Rope.empty
                    , syntax =
                        Node
                            { start = { row = end.row, column = end.column - 2 }
                            , end = end
                            }
                            TypeAnnotation.Unit
                    }
                )
            )
            (ParserFast.map4WithRange
                (\rangeAfterOpeningParens commentsBeforeFirstPart firstPart commentsAfterFirstPart lastToSecondPart ->
                    { comments =
                        commentsBeforeFirstPart
                            |> Rope.prependTo firstPart.comments
                            |> Rope.prependTo commentsAfterFirstPart
                            |> Rope.prependTo lastToSecondPart.comments
                    , syntax =
                        Node
                            { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                            , end = rangeAfterOpeningParens.end
                            }
                            (case lastToSecondPart.syntax of
                                Nothing ->
                                    -- parenthesized types are not a `Tupled [ firstPart.syntax ]`
                                    -- but their Range still extends to both parens.
                                    -- This is done to not break behavior of v7.
                                    -- This will likely change in v8 after discussion in issues like https://github.com/stil4m/elm-syntax/issues/204
                                    let
                                        (Node _ firstPartType) =
                                            firstPart.syntax
                                    in
                                    firstPartType

                                Just firstAndMaybeThirdPart ->
                                    case firstAndMaybeThirdPart.maybeThirdPart of
                                        Nothing ->
                                            TypeAnnotation.Tupled [ firstPart.syntax, firstAndMaybeThirdPart.secondPart ]

                                        Just thirdPart ->
                                            TypeAnnotation.Tupled [ firstPart.syntax, firstAndMaybeThirdPart.secondPart, thirdPart ]
                            )
                    }
                )
                Layout.maybeLayout
                typeAnnotation
                Layout.maybeLayout
                (ParserFast.oneOf2
                    (ParserFast.symbol ")"
                        { comments = Rope.empty, syntax = Nothing }
                    )
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map4
                            (\commentsBefore secondPartResult commentsAfter maybeThirdPartResult ->
                                { comments =
                                    commentsBefore
                                        |> Rope.prependTo secondPartResult.comments
                                        |> Rope.prependTo commentsAfter
                                , syntax = Just { maybeThirdPart = maybeThirdPartResult.syntax, secondPart = secondPartResult.syntax }
                                }
                            )
                            Layout.maybeLayout
                            typeAnnotation
                            Layout.maybeLayout
                            (ParserFast.oneOf2
                                (ParserFast.symbol ")"
                                    { comments = Rope.empty, syntax = Nothing }
                                )
                                (ParserFast.symbolFollowedBy ","
                                    (ParserFast.map3
                                        (\commentsBefore thirdPartResult commentsAfter ->
                                            { comments =
                                                commentsBefore
                                                    |> Rope.prependTo thirdPartResult.comments
                                                    |> Rope.prependTo commentsAfter
                                            , syntax = Just thirdPartResult.syntax
                                            }
                                        )
                                        Layout.maybeLayout
                                        typeAnnotation
                                        Layout.maybeLayout
                                        |> ParserFast.followedBySymbol ")"
                                    )
                                )
                            )
                        )
                    )
                )
            )
        )


genericTypeAnnotation : Parser (WithComments (Node TypeAnnotation))
genericTypeAnnotation =
    Tokens.functionNameMapWithRange
        (\range var ->
            { comments = Rope.empty
            , syntax =
                Node range (TypeAnnotation.GenericType var)
            }
        )


recordTypeAnnotation : Parser (WithComments (Node TypeAnnotation))
recordTypeAnnotation =
    ParserFast.map2WithRange
        (\range commentsBefore afterCurly ->
            { comments =
                commentsBefore
                    |> Rope.prependTo afterCurly.comments
            , syntax =
                case afterCurly.syntax of
                    Nothing ->
                        Node range typeAnnotationRecordEmpty

                    Just afterCurlyResult ->
                        Node range afterCurlyResult
            }
        )
        (ParserFast.symbolFollowedBy "{" Layout.maybeLayout)
        (ParserFast.oneOf2
            (ParserFast.map3
                (\firstNameNode commentsAfterFirstName afterFirstName ->
                    { comments =
                        commentsAfterFirstName
                            |> Rope.prependTo afterFirstName.comments
                    , syntax =
                        Just
                            (case afterFirstName.syntax of
                                RecordExtensionExpressionAfterName fields ->
                                    TypeAnnotation.GenericRecord firstNameNode fields

                                FieldsAfterName fieldsAfterName ->
                                    TypeAnnotation.Record (Node.combine Tuple.pair firstNameNode fieldsAfterName.firstFieldValue :: fieldsAfterName.tailFields)
                            )
                    }
                )
                Tokens.functionNameNode
                Layout.maybeLayout
                (ParserFast.oneOf2
                    (ParserFast.symbolFollowedBy "|"
                        (ParserFast.map3WithRange
                            (\range commentsBefore head tail ->
                                { comments =
                                    commentsBefore
                                        |> Rope.prependTo head.comments
                                        |> Rope.prependTo tail.comments
                                , syntax =
                                    RecordExtensionExpressionAfterName
                                        (Node range (head.syntax :: tail.syntax))
                                }
                            )
                            Layout.maybeLayout
                            recordFieldDefinition
                            (ParserWithComments.many
                                (ParserFast.symbolFollowedBy ","
                                    (ParserFast.map2
                                        (\commentsBefore field ->
                                            { comments = commentsBefore |> Rope.prependTo field.comments
                                            , syntax = field.syntax
                                            }
                                        )
                                        Layout.maybeLayout
                                        recordFieldDefinition
                                    )
                                )
                            )
                        )
                    )
                    (ParserFast.symbolFollowedBy ":"
                        (ParserFast.map4
                            (\commentsBeforeFirstFieldValue firstFieldValue commentsAfterFirstFieldValue tailFields ->
                                { comments =
                                    commentsBeforeFirstFieldValue
                                        |> Rope.prependTo firstFieldValue.comments
                                        |> Rope.prependTo commentsAfterFirstFieldValue
                                        |> Rope.prependTo tailFields.comments
                                , syntax =
                                    FieldsAfterName
                                        { firstFieldValue = firstFieldValue.syntax
                                        , tailFields = tailFields.syntax
                                        }
                                }
                            )
                            Layout.maybeLayout
                            typeAnnotation
                            Layout.maybeLayout
                            (ParserFast.orSucceed
                                (ParserFast.symbolFollowedBy "," recordFieldsTypeAnnotation)
                                { comments = Rope.empty, syntax = [] }
                            )
                        )
                    )
                )
                |> ParserFast.followedBySymbol "}"
            )
            (ParserFast.symbol "}" { comments = Rope.empty, syntax = Nothing })
        )


typeAnnotationRecordEmpty : TypeAnnotation
typeAnnotationRecordEmpty =
    TypeAnnotation.Record []


type RecordFieldsOrExtensionAfterName
    = RecordExtensionExpressionAfterName (Node RecordDefinition)
    | FieldsAfterName { firstFieldValue : Node TypeAnnotation, tailFields : List (Node RecordField) }


recordFieldsTypeAnnotation : Parser (WithComments TypeAnnotation.RecordDefinition)
recordFieldsTypeAnnotation =
    ParserFast.map3
        (\commentsBefore head tail ->
            { comments =
                commentsBefore
                    |> Rope.prependTo head.comments
                    |> Rope.prependTo tail.comments
            , syntax = head.syntax :: tail.syntax
            }
        )
        Layout.maybeLayout
        recordFieldDefinition
        (ParserWithComments.many
            (ParserFast.symbolFollowedBy ","
                (ParserFast.map2
                    (\commentsBefore field ->
                        { comments = commentsBefore |> Rope.prependTo field.comments
                        , syntax = field.syntax
                        }
                    )
                    Layout.maybeLayout
                    recordFieldDefinition
                )
            )
        )


recordFieldDefinition : Parser (WithComments (Node TypeAnnotation.RecordField))
recordFieldDefinition =
    ParserFast.map6WithRange
        (\range commentsBeforeFunctionName name commentsAfterFunctionName commentsAfterColon value commentsAfterValue ->
            { comments =
                commentsBeforeFunctionName
                    |> Rope.prependTo commentsAfterFunctionName
                    |> Rope.prependTo commentsAfterColon
                    |> Rope.prependTo value.comments
                    |> Rope.prependTo commentsAfterValue
            , syntax = Node range ( name, value.syntax )
            }
        )
        Layout.maybeLayout
        Tokens.functionNameNode
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy ":" Layout.maybeLayout)
        typeAnnotation
        -- This extra whitespace is just included for compatibility with earlier version
        -- TODO for v8: move to recordFieldsTypeAnnotation
        Layout.maybeLayout


typedTypeAnnotationWithoutArguments : Parser (WithComments (Node TypeAnnotation))
typedTypeAnnotationWithoutArguments =
    ParserFast.map2WithRange
        (\range startName afterStartName ->
            let
                name : ( ModuleName, String )
                name =
                    case afterStartName of
                        Nothing ->
                            ( [], startName )

                        Just ( qualificationAfterStartName, unqualified ) ->
                            ( startName :: qualificationAfterStartName, unqualified )
            in
            { comments = Rope.empty
            , syntax =
                Node range
                    (TypeAnnotation.Typed (Node range name) [])
            }
        )
        Tokens.typeName
        maybeDotTypeNamesTuple


maybeDotTypeNamesTuple : ParserFast.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    ParserFast.map2OrSucceed
        (\firstName afterFirstName ->
            case afterFirstName of
                Nothing ->
                    Just ( [], firstName )

                Just ( qualificationAfter, unqualified ) ->
                    Just ( firstName :: qualificationAfter, unqualified )
        )
        (ParserFast.symbolFollowedBy "." Tokens.typeName)
        (ParserFast.lazy (\() -> maybeDotTypeNamesTuple))
        Nothing


typedTypeAnnotationWithArgumentsOptimisticLayout : Parser (WithComments (Node TypeAnnotation))
typedTypeAnnotationWithArgumentsOptimisticLayout =
    ParserFast.map3
        (\((Node nameRange _) as nameNode) commentsAfterName argsReverse ->
            let
                range : Range
                range =
                    case argsReverse.syntax of
                        [] ->
                            nameRange

                        (Node lastArgRange _) :: _ ->
                            { start = nameRange.start, end = lastArgRange.end }
            in
            { comments =
                commentsAfterName
                    |> Rope.prependTo argsReverse.comments
            , syntax =
                Node range (TypeAnnotation.Typed nameNode (List.reverse argsReverse.syntax))
            }
        )
        (ParserFast.map2WithRange
            (\range startName afterStartName ->
                let
                    name : ( ModuleName, String )
                    name =
                        case afterStartName of
                            Nothing ->
                                ( [], startName )

                            Just ( qualificationAfterStartName, unqualified ) ->
                                ( startName :: qualificationAfterStartName, unqualified )
                in
                Node range name
            )
            Tokens.typeName
            maybeDotTypeNamesTuple
        )
        Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            (Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\typeAnnotationResult commentsAfter ->
                        { comments =
                            typeAnnotationResult.comments
                                |> Rope.prependTo commentsAfter
                        , syntax = typeAnnotationResult.syntax
                        }
                    )
                    typeAnnotationNoFnExcludingTypedWithArguments
                    Layout.optimisticLayout
                )
            )
        )
