module Elm.Parser.TypeAnnotation exposing (typeAnnotation, typeAnnotationNoFnExcludingTypedWithArguments)

import Elm.Parser.Layout as Layout
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Range)
import Elm.Syntax.TypeAnnotation as TypeAnnotation exposing (RecordDefinition, RecordField, TypeAnnotation)
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments(..))
import Rope


typeAnnotation : Parser (WithComments (Node TypeAnnotation))
typeAnnotation =
    ParserFast.map3
        (\inType commentsAfterIn maybeOut ->
            let
                (WithComments inTypeComments inTypeSyntax) =
                    inType

                (WithComments maybeOutComments maybeOutSyntax) =
                    maybeOut
            in
            WithComments
                (inTypeComments
                    |> Rope.prependTo commentsAfterIn
                    |> Rope.prependTo maybeOutComments
                )
                (case maybeOutSyntax of
                    Nothing ->
                        inTypeSyntax

                    Just out ->
                        Node.combine TypeAnnotation.FunctionTypeAnnotation inTypeSyntax out
                )
        )
        (ParserFast.lazy (\() -> typeAnnotationNoFnIncludingTypedWithArguments))
        Layout.optimisticLayout
        (ParserFast.map2OrSucceed
            (\commentsAfterArrow typeAnnotationResult ->
                let
                    (WithComments typeAnnotationComments typeAnnotationSyntax) =
                        typeAnnotationResult
                in
                WithComments
                    (commentsAfterArrow
                        |> Rope.prependTo typeAnnotationComments
                    )
                    (Just typeAnnotationSyntax)
            )
            (ParserFast.symbolFollowedBy "->"
                (Layout.positivelyIndentedPlusFollowedBy 2
                    Layout.maybeLayout
                )
            )
            (ParserFast.lazy (\() -> typeAnnotation))
            (WithComments Rope.empty Nothing)
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
                    WithComments
                        Rope.empty
                        (Node
                            { start = { row = end.row, column = end.column - 2 }
                            , end = end
                            }
                            TypeAnnotation.Unit
                        )
                )
            )
            (ParserFast.map4WithRange
                (\rangeAfterOpeningParens commentsBeforeFirstPart firstPart commentsAfterFirstPart lastToSecondPart ->
                    let
                        (WithComments firstPartComments firstPartSyntax) =
                            firstPart

                        (WithComments lastToSecondPartComments lastToSecondPartSyntax) =
                            lastToSecondPart
                    in
                    WithComments
                        (commentsBeforeFirstPart
                            |> Rope.prependTo firstPartComments
                            |> Rope.prependTo commentsAfterFirstPart
                            |> Rope.prependTo lastToSecondPartComments
                        )
                        (Node
                            { start = { row = rangeAfterOpeningParens.start.row, column = rangeAfterOpeningParens.start.column - 1 }
                            , end = rangeAfterOpeningParens.end
                            }
                            (case lastToSecondPartSyntax of
                                Nothing ->
                                    -- parenthesized types are not a `Tupled [ firstPart.syntax ]`
                                    -- but their Range still extends to both parens.
                                    -- This is done to not break behavior of v7.
                                    -- This will likely change in v8 after discussion in issues like https://github.com/stil4m/elm-syntax/issues/204
                                    let
                                        (Node _ firstPartType) =
                                            firstPartSyntax
                                    in
                                    firstPartType

                                Just firstAndMaybeThirdPart ->
                                    case firstAndMaybeThirdPart.maybeThirdPart of
                                        Nothing ->
                                            TypeAnnotation.Tupled [ firstPartSyntax, firstAndMaybeThirdPart.secondPart ]

                                        Just thirdPart ->
                                            TypeAnnotation.Tupled [ firstPartSyntax, firstAndMaybeThirdPart.secondPart, thirdPart ]
                            )
                        )
                )
                Layout.maybeLayout
                typeAnnotation
                Layout.maybeLayout
                (ParserFast.oneOf2
                    (ParserFast.symbol ")"
                        (WithComments Rope.empty Nothing)
                    )
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map4
                            (\commentsBefore secondPartResult commentsAfter maybeThirdPartResult ->
                                let
                                    (WithComments secondPartComments secondPartSyntax) =
                                        secondPartResult

                                    (WithComments maybeThirdPartComments maybeThirdPartSyntax) =
                                        maybeThirdPartResult
                                in
                                WithComments
                                    (commentsBefore
                                        |> Rope.prependTo secondPartComments
                                        |> Rope.prependTo commentsAfter
                                    )
                                    (Just { maybeThirdPart = maybeThirdPartSyntax, secondPart = secondPartSyntax })
                            )
                            Layout.maybeLayout
                            typeAnnotation
                            Layout.maybeLayout
                            (ParserFast.oneOf2
                                (ParserFast.symbol ")"
                                    (WithComments Rope.empty Nothing)
                                )
                                (ParserFast.symbolFollowedBy ","
                                    (ParserFast.map3
                                        (\commentsBefore thirdPartResult commentsAfter ->
                                            let
                                                (WithComments thirdPartComments thirdPartSyntax) =
                                                    thirdPartResult
                                            in
                                            WithComments
                                                (commentsBefore
                                                    |> Rope.prependTo thirdPartComments
                                                    |> Rope.prependTo commentsAfter
                                                )
                                                (Just thirdPartSyntax)
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
            WithComments
                Rope.empty
                (Node range (TypeAnnotation.GenericType var))
        )


recordTypeAnnotation : Parser (WithComments (Node TypeAnnotation))
recordTypeAnnotation =
    ParserFast.map2WithRange
        (\range commentsBefore afterCurly ->
            let
                (WithComments afterCurlyComments afterCurlySyntax) =
                    afterCurly
            in
            WithComments
                (commentsBefore
                    |> Rope.prependTo afterCurlyComments
                )
                (case afterCurlySyntax of
                    Nothing ->
                        Node range typeAnnotationRecordEmpty

                    Just afterCurlyResult ->
                        Node range afterCurlyResult
                )
        )
        (ParserFast.symbolFollowedBy "{" Layout.maybeLayout)
        (ParserFast.oneOf2
            (ParserFast.map3
                (\firstNameNode commentsAfterFirstName afterFirstName ->
                    let
                        (WithComments afterFirstNameComments afterFirstNameSyntax) =
                            afterFirstName
                    in
                    WithComments
                        (commentsAfterFirstName
                            |> Rope.prependTo afterFirstNameComments
                        )
                        (Just
                            (case afterFirstNameSyntax of
                                RecordExtensionExpressionAfterName fields ->
                                    TypeAnnotation.GenericRecord firstNameNode fields

                                FieldsAfterName fieldsAfterName ->
                                    TypeAnnotation.Record (Node.combine Tuple.pair firstNameNode fieldsAfterName.firstFieldValue :: fieldsAfterName.tailFields)
                            )
                        )
                )
                Tokens.functionNameNode
                Layout.maybeLayout
                (ParserFast.oneOf2
                    (ParserFast.symbolFollowedBy "|"
                        (ParserFast.map3WithRange
                            (\range commentsBefore head tail ->
                                let
                                    (WithComments headComments headSyntax) =
                                        head

                                    (WithComments tailComments tailSyntax) =
                                        tail
                                in
                                WithComments
                                    (commentsBefore
                                        |> Rope.prependTo headComments
                                        |> Rope.prependTo tailComments
                                    )
                                    (RecordExtensionExpressionAfterName
                                        (Node range (headSyntax :: tailSyntax))
                                    )
                            )
                            Layout.maybeLayout
                            recordFieldDefinition
                            (ParserWithComments.many
                                (ParserFast.symbolFollowedBy ","
                                    (ParserFast.map2
                                        (\commentsBefore field ->
                                            let
                                                (WithComments fieldComments fieldSyntax) =
                                                    field
                                            in
                                            WithComments
                                                (commentsBefore |> Rope.prependTo fieldComments)
                                                fieldSyntax
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
                                let
                                    (WithComments firstFieldValueComments firstFieldValueSyntax) =
                                        firstFieldValue

                                    (WithComments tailFieldsComments tailFieldsSyntax) =
                                        tailFields
                                in
                                WithComments
                                    (commentsBeforeFirstFieldValue
                                        |> Rope.prependTo firstFieldValueComments
                                        |> Rope.prependTo commentsAfterFirstFieldValue
                                        |> Rope.prependTo tailFieldsComments
                                    )
                                    (FieldsAfterName
                                        { firstFieldValue = firstFieldValueSyntax
                                        , tailFields = tailFieldsSyntax
                                        }
                                    )
                            )
                            Layout.maybeLayout
                            typeAnnotation
                            Layout.maybeLayout
                            (ParserFast.orSucceed
                                (ParserFast.symbolFollowedBy "," recordFieldsTypeAnnotation)
                                (WithComments Rope.empty [])
                            )
                        )
                    )
                )
                |> ParserFast.followedBySymbol "}"
            )
            (ParserFast.symbol "}"
                (WithComments Rope.empty Nothing)
            )
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
            let
                (WithComments headComments headSyntax) =
                    head

                (WithComments tailComments tailSyntax) =
                    tail
            in
            WithComments
                (commentsBefore
                    |> Rope.prependTo headComments
                    |> Rope.prependTo tailComments
                )
                (headSyntax :: tailSyntax)
        )
        Layout.maybeLayout
        recordFieldDefinition
        (ParserWithComments.many
            (ParserFast.symbolFollowedBy ","
                (ParserFast.map2
                    (\commentsBefore field ->
                        let
                            (WithComments fieldComments fieldSyntax) =
                                field
                        in
                        WithComments
                            (commentsBefore |> Rope.prependTo fieldComments)
                            fieldSyntax
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
            let
                (WithComments valueComments valueSyntax) =
                    value
            in
            WithComments
                (commentsBeforeFunctionName
                    |> Rope.prependTo commentsAfterFunctionName
                    |> Rope.prependTo commentsAfterColon
                    |> Rope.prependTo valueComments
                    |> Rope.prependTo commentsAfterValue
                )
                (Node range ( name, valueSyntax ))
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
            WithComments
                Rope.empty
                (Node range
                    (TypeAnnotation.Typed (Node range name) [])
                )
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
                (WithComments argsReverseComments argsReverseSyntax) =
                    argsReverse

                range : Range
                range =
                    case argsReverseSyntax of
                        [] ->
                            nameRange

                        (Node lastArgRange _) :: _ ->
                            { start = nameRange.start, end = lastArgRange.end }
            in
            WithComments
                (commentsAfterName
                    |> Rope.prependTo argsReverseComments
                )
                (Node range (TypeAnnotation.Typed nameNode (List.reverse argsReverseSyntax)))
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
                        let
                            (WithComments typeAnnotationResultComments typeAnnotationResultSyntax) =
                                typeAnnotationResult
                        in
                        WithComments
                            (typeAnnotationResultComments
                                |> Rope.prependTo commentsAfter
                            )
                            typeAnnotationResultSyntax
                    )
                    typeAnnotationNoFnExcludingTypedWithArguments
                    Layout.optimisticLayout
                )
            )
        )
