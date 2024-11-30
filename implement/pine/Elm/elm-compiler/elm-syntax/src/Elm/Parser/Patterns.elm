module Elm.Parser.Patterns exposing (pattern, patternNotDirectlyComposing)

import Elm.Parser.Layout as Layout
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern(..), QualifiedNameRef)
import Elm.Syntax.Range exposing (Range)
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments(..))
import Rope


type PatternComposedWith
    = PatternComposedWithNothing ()
    | PatternComposedWithAs (Node String)
    | PatternComposedWithCons (Node Pattern)


pattern : Parser (WithComments (Node Pattern))
pattern =
    ParserFast.lazy (\() -> composablePatternTryToCompose)


composablePatternTryToCompose : Parser (WithComments (Node Pattern))
composablePatternTryToCompose =
    ParserFast.map3
        (\x commentsAfterLeft maybeComposedWithResult ->
            let
                (WithComments xComments xSyntax) =
                    x

                (WithComments maybeComposedWithResultComments maybeComposedWithResultSyntax) =
                    maybeComposedWithResult
            in
            WithComments
                (xComments
                    |> Rope.prependTo commentsAfterLeft
                    |> Rope.prependTo maybeComposedWithResultComments
                )
                (case maybeComposedWithResultSyntax of
                    PatternComposedWithNothing () ->
                        xSyntax

                    PatternComposedWithAs anotherName ->
                        Node.combine Pattern.AsPattern xSyntax anotherName

                    PatternComposedWithCons y ->
                        Node.combine Pattern.UnConsPattern xSyntax y
                )
        )
        composablePattern
        Layout.maybeLayout
        maybeComposedWith


maybeComposedWith : Parser (WithComments PatternComposedWith)
maybeComposedWith =
    ParserFast.oneOf2OrSucceed
        (ParserFast.keywordFollowedBy "as"
            (ParserFast.map2
                (\commentsAfterAs name ->
                    WithComments
                        commentsAfterAs
                        (PatternComposedWithAs name)
                )
                Layout.maybeLayout
                Tokens.functionNameNode
            )
        )
        (ParserFast.symbolFollowedBy "::"
            (ParserFast.map2
                (\commentsAfterCons patternResult ->
                    let
                        (WithComments patternResultComments patternResultSyntax) =
                            patternResult
                    in
                    WithComments
                        (commentsAfterCons |> Rope.prependTo patternResultComments)
                        (PatternComposedWithCons patternResultSyntax)
                )
                Layout.maybeLayout
                pattern
            )
        )
        (WithComments
            Rope.empty
            (PatternComposedWithNothing ())
        )


parensPattern : Parser (WithComments (Node Pattern))
parensPattern =
    ParserFast.symbolFollowedBy "("
        (ParserFast.map2WithRange
            (\range commentsBeforeHead contentResult ->
                let
                    (WithComments contentResultComments contentResultSyntax) =
                        contentResult
                in
                WithComments
                    (commentsBeforeHead
                        |> Rope.prependTo contentResultComments
                    )
                    (Node { start = { row = range.start.row, column = range.start.column - 1 }, end = range.end }
                        contentResultSyntax
                    )
            )
            Layout.maybeLayout
            -- yes, (  ) is a valid pattern but not a valid type or expression
            (ParserFast.oneOf2
                (ParserFast.symbol ")"
                    (WithComments Rope.empty UnitPattern)
                )
                (ParserFast.map3
                    (\headResult commentsAfterHead tailResult ->
                        let
                            (WithComments headComments headSyntax) =
                                headResult

                            (WithComments tailComments tailSyntax) =
                                tailResult
                        in
                        WithComments
                            (headComments
                                |> Rope.prependTo commentsAfterHead
                                |> Rope.prependTo tailComments
                            )
                            (case tailSyntax of
                                Nothing ->
                                    ParenthesizedPattern headSyntax

                                Just secondAndMaybeThirdPart ->
                                    case secondAndMaybeThirdPart.maybeThirdPart of
                                        Nothing ->
                                            TuplePattern [ headSyntax, secondAndMaybeThirdPart.secondPart ]

                                        Just thirdPart ->
                                            TuplePattern [ headSyntax, secondAndMaybeThirdPart.secondPart, thirdPart ]
                            )
                    )
                    pattern
                    Layout.maybeLayout
                    (ParserFast.oneOf2
                        (ParserFast.symbol ")"
                            (WithComments Rope.empty Nothing)
                        )
                        (ParserFast.symbolFollowedBy ","
                            (ParserFast.map4
                                (\commentsBefore secondPart commentsAfter maybeThirdPart ->
                                    let
                                        (WithComments secondPartComments secondPartSyntax) =
                                            secondPart

                                        (WithComments maybeThirdComments maybeThirdSyntax) =
                                            maybeThirdPart
                                    in
                                    WithComments
                                        (commentsBefore
                                            |> Rope.prependTo secondPartComments
                                            |> Rope.prependTo commentsAfter
                                            |> Rope.prependTo maybeThirdComments
                                        )
                                        (Just { maybeThirdPart = maybeThirdSyntax, secondPart = secondPartSyntax })
                                )
                                Layout.maybeLayout
                                pattern
                                Layout.maybeLayout
                                (ParserFast.oneOf2
                                    (ParserFast.symbol ")"
                                        (WithComments Rope.empty Nothing)
                                    )
                                    (ParserFast.symbolFollowedBy ","
                                        (ParserFast.map3
                                            (\commentsBefore thirdPart commentsAfter ->
                                                let
                                                    (WithComments thirdPartComments thirdPartSyntax) =
                                                        thirdPart
                                                in
                                                WithComments
                                                    (commentsBefore
                                                        |> Rope.prependTo thirdPartComments
                                                        |> Rope.prependTo commentsAfter
                                                    )
                                                    (Just thirdPartSyntax)
                                            )
                                            Layout.maybeLayout
                                            pattern
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
        )


varPattern : Parser (WithComments (Node Pattern))
varPattern =
    Tokens.functionNameMapWithRange
        (\range var ->
            WithComments
                Rope.empty
                (Node range (VarPattern var))
        )


numberPart : Parser (WithComments (Node Pattern))
numberPart =
    ParserFast.integerDecimalOrHexadecimalMapWithRange
        (\range n ->
            WithComments
                Rope.empty
                (Node range (IntPattern n))
        )
        (\range n ->
            WithComments
                Rope.empty
                (Node range (HexPattern n))
        )


charPattern : Parser (WithComments (Node Pattern))
charPattern =
    Tokens.characterLiteralMapWithRange
        (\range char ->
            WithComments
                Rope.empty
                (Node range (CharPattern char))
        )


listPattern : Parser (WithComments (Node Pattern))
listPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeElements maybeElements ->
            case maybeElements of
                Nothing ->
                    WithComments
                        commentsBeforeElements
                        (Node range patternListEmpty)

                Just elements ->
                    let
                        (WithComments elementsComments elementsSyntax) =
                            elements
                    in
                    WithComments
                        (commentsBeforeElements |> Rope.prependTo elementsComments)
                        (Node range (ListPattern elementsSyntax))
        )
        (ParserFast.symbolFollowedBy "[" Layout.maybeLayout)
        (ParserFast.oneOf2
            (ParserFast.symbol "]" Nothing)
            (ParserFast.map3
                (\head commentsAfterHead tail ->
                    let
                        (WithComments headComments headSyntax) =
                            head

                        (WithComments tailComments tailSyntax) =
                            tail
                    in
                    Just
                        (WithComments
                            (headComments
                                |> Rope.prependTo tailComments
                                |> Rope.prependTo commentsAfterHead
                            )
                            (headSyntax :: tailSyntax)
                        )
                )
                pattern
                Layout.maybeLayout
                (ParserWithComments.many
                    (ParserFast.symbolFollowedBy ","
                        (Layout.maybeAroundBothSides pattern)
                    )
                )
                |> ParserFast.followedBySymbol "]"
            )
        )


patternListEmpty : Pattern
patternListEmpty =
    ListPattern []


composablePattern : Parser (WithComments (Node Pattern))
composablePattern =
    ParserFast.oneOf9
        varPattern
        qualifiedPatternWithConsumeArgs
        allPattern
        parensPattern
        recordPattern
        stringPattern
        listPattern
        numberPart
        charPattern


patternNotDirectlyComposing : Parser (WithComments (Node Pattern))
patternNotDirectlyComposing =
    ParserFast.oneOf9
        varPattern
        qualifiedPatternWithoutConsumeArgs
        allPattern
        parensPattern
        recordPattern
        stringPattern
        listPattern
        numberPart
        charPattern


allPattern : Parser (WithComments (Node Pattern))
allPattern =
    ParserFast.symbolWithRange "_"
        (\range ->
            WithComments
                Rope.empty
                (Node range AllPattern)
        )


stringPattern : Parser (WithComments (Node Pattern))
stringPattern =
    Tokens.singleOrTripleQuotedStringLiteralMapWithRange
        (\range string ->
            WithComments
                Rope.empty
                (Node range (StringPattern string))
        )


maybeDotTypeNamesTuple : ParserFast.Parser (Maybe ( List String, String ))
maybeDotTypeNamesTuple =
    ParserFast.map2OrSucceed
        (\startName afterStartName ->
            case afterStartName of
                Nothing ->
                    Just ( [], startName )

                Just ( qualificationAfter, unqualified ) ->
                    Just ( startName :: qualificationAfter, unqualified )
        )
        (ParserFast.symbolFollowedBy "." Tokens.typeName)
        (ParserFast.lazy (\() -> maybeDotTypeNamesTuple))
        Nothing


qualifiedPatternWithConsumeArgs : Parser (WithComments (Node Pattern))
qualifiedPatternWithConsumeArgs =
    ParserFast.map3
        (\(Node nameRange name) afterStartName argsReverse ->
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
                (afterStartName |> Rope.prependTo argsReverseComments)
                (Node range
                    (NamedPattern
                        name
                        (List.reverse argsReverseSyntax)
                    )
                )
        )
        qualifiedNameRefNode
        Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            (Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\(WithComments comments syntax) commentsAfterArg ->
                        WithComments
                            (comments |> Rope.prependTo commentsAfterArg)
                            syntax
                    )
                    patternNotDirectlyComposing
                    Layout.optimisticLayout
                )
            )
        )


qualifiedNameRefNode : Parser (Node QualifiedNameRef)
qualifiedNameRefNode =
    ParserFast.map2WithRange
        (\range firstName after ->
            Node range
                (case after of
                    Nothing ->
                        { moduleName = [], name = firstName }

                    Just ( qualificationAfter, unqualified ) ->
                        { moduleName = firstName :: qualificationAfter, name = unqualified }
                )
        )
        Tokens.typeName
        maybeDotTypeNamesTuple


qualifiedPatternWithoutConsumeArgs : Parser (WithComments (Node Pattern))
qualifiedPatternWithoutConsumeArgs =
    ParserFast.map2WithRange
        (\range firstName after ->
            WithComments
                Rope.empty
                (Node range
                    (NamedPattern
                        (case after of
                            Nothing ->
                                { moduleName = [], name = firstName }

                            Just ( qualificationAfter, unqualified ) ->
                                { moduleName = firstName :: qualificationAfter, name = unqualified }
                        )
                        []
                    )
                )
        )
        Tokens.typeName
        maybeDotTypeNamesTuple


recordPattern : Parser (WithComments (Node Pattern))
recordPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeElements elements ->
            let
                (WithComments elementsComments elementsSyntax) =
                    elements
            in
            WithComments
                (commentsBeforeElements |> Rope.prependTo elementsComments)
                (Node range (RecordPattern elementsSyntax))
        )
        (ParserFast.symbolFollowedBy "{" Layout.maybeLayout)
        (ParserFast.oneOf2
            (ParserFast.map3
                (\head commentsAfterHead tail ->
                    let
                        (WithComments tailComments tailSyntax) =
                            tail
                    in
                    WithComments
                        (commentsAfterHead
                            |> Rope.prependTo tailComments
                        )
                        (head :: tailSyntax)
                )
                Tokens.functionNameNode
                Layout.maybeLayout
                (ParserWithComments.many
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map3
                            (\beforeName name afterName ->
                                WithComments
                                    (beforeName |> Rope.prependTo afterName)
                                    name
                            )
                            Layout.maybeLayout
                            Tokens.functionNameNode
                            Layout.maybeLayout
                        )
                    )
                )
                |> ParserFast.followedBySymbol "}"
            )
            (ParserFast.symbol "}"
                (WithComments Rope.empty [])
            )
        )
