module Elm.Parser.Patterns exposing (pattern, patternNotDirectlyComposing)

import Elm.Parser.Layout as Layout
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern(..), QualifiedNameRef)
import Elm.Syntax.Range exposing (Range)
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)
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
            { comments =
                x.comments
                    |> Rope.prependTo commentsAfterLeft
                    |> Rope.prependTo maybeComposedWithResult.comments
            , syntax =
                case maybeComposedWithResult.syntax of
                    PatternComposedWithNothing () ->
                        x.syntax

                    PatternComposedWithAs anotherName ->
                        Node.combine Pattern.AsPattern x.syntax anotherName

                    PatternComposedWithCons y ->
                        Node.combine Pattern.UnConsPattern x.syntax y
            }
        )
        composablePattern
        Layout.maybeLayout
        maybeComposedWith


maybeComposedWith : Parser { comments : ParserWithComments.Comments, syntax : PatternComposedWith }
maybeComposedWith =
    ParserFast.oneOf2OrSucceed
        (ParserFast.keywordFollowedBy "as"
            (ParserFast.map2
                (\commentsAfterAs name ->
                    { comments = commentsAfterAs
                    , syntax = PatternComposedWithAs name
                    }
                )
                Layout.maybeLayout
                Tokens.functionNameNode
            )
        )
        (ParserFast.symbolFollowedBy "::"
            (ParserFast.map2
                (\commentsAfterCons patternResult ->
                    { comments = commentsAfterCons |> Rope.prependTo patternResult.comments
                    , syntax = PatternComposedWithCons patternResult.syntax
                    }
                )
                Layout.maybeLayout
                pattern
            )
        )
        { comments = Rope.empty, syntax = PatternComposedWithNothing () }


parensPattern : Parser (WithComments (Node Pattern))
parensPattern =
    ParserFast.symbolFollowedBy "("
        (ParserFast.map2WithRange
            (\range commentsBeforeHead contentResult ->
                { comments =
                    commentsBeforeHead
                        |> Rope.prependTo contentResult.comments
                , syntax =
                    Node { start = { row = range.start.row, column = range.start.column - 1 }, end = range.end }
                        contentResult.syntax
                }
            )
            Layout.maybeLayout
            -- yes, (  ) is a valid pattern but not a valid type or expression
            (ParserFast.oneOf2
                (ParserFast.symbol ")" { comments = Rope.empty, syntax = UnitPattern })
                (ParserFast.map3
                    (\headResult commentsAfterHead tailResult ->
                        { comments =
                            headResult.comments
                                |> Rope.prependTo commentsAfterHead
                                |> Rope.prependTo tailResult.comments
                        , syntax =
                            case tailResult.syntax of
                                Nothing ->
                                    ParenthesizedPattern headResult.syntax

                                Just secondAndMaybeThirdPart ->
                                    case secondAndMaybeThirdPart.maybeThirdPart of
                                        Nothing ->
                                            TuplePattern [ headResult.syntax, secondAndMaybeThirdPart.secondPart ]

                                        Just thirdPart ->
                                            TuplePattern [ headResult.syntax, secondAndMaybeThirdPart.secondPart, thirdPart ]
                        }
                    )
                    pattern
                    Layout.maybeLayout
                    (ParserFast.oneOf2
                        (ParserFast.symbol ")" { comments = Rope.empty, syntax = Nothing })
                        (ParserFast.symbolFollowedBy ","
                            (ParserFast.map4
                                (\commentsBefore secondPart commentsAfter maybeThirdPart ->
                                    { comments =
                                        commentsBefore
                                            |> Rope.prependTo secondPart.comments
                                            |> Rope.prependTo commentsAfter
                                            |> Rope.prependTo maybeThirdPart.comments
                                    , syntax = Just { maybeThirdPart = maybeThirdPart.syntax, secondPart = secondPart.syntax }
                                    }
                                )
                                Layout.maybeLayout
                                pattern
                                Layout.maybeLayout
                                (ParserFast.oneOf2
                                    (ParserFast.symbol ")" { comments = Rope.empty, syntax = Nothing })
                                    (ParserFast.symbolFollowedBy ","
                                        (ParserFast.map3
                                            (\commentsBefore thirdPart commentsAfter ->
                                                { comments =
                                                    commentsBefore
                                                        |> Rope.prependTo thirdPart.comments
                                                        |> Rope.prependTo commentsAfter
                                                , syntax = Just thirdPart.syntax
                                                }
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
            { comments = Rope.empty
            , syntax = Node range (VarPattern var)
            }
        )


numberPart : Parser (WithComments (Node Pattern))
numberPart =
    ParserFast.integerDecimalOrHexadecimalMapWithRange
        (\range n -> { comments = Rope.empty, syntax = Node range (IntPattern n) })
        (\range n -> { comments = Rope.empty, syntax = Node range (HexPattern n) })


charPattern : Parser (WithComments (Node Pattern))
charPattern =
    Tokens.characterLiteralMapWithRange
        (\range char ->
            { comments = Rope.empty, syntax = Node range (CharPattern char) }
        )


listPattern : Parser (WithComments (Node Pattern))
listPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeElements maybeElements ->
            case maybeElements of
                Nothing ->
                    { comments = commentsBeforeElements
                    , syntax = Node range patternListEmpty
                    }

                Just elements ->
                    { comments = commentsBeforeElements |> Rope.prependTo elements.comments
                    , syntax = Node range (ListPattern elements.syntax)
                    }
        )
        (ParserFast.symbolFollowedBy "[" Layout.maybeLayout)
        (ParserFast.oneOf2
            (ParserFast.symbol "]" Nothing)
            (ParserFast.map3
                (\head commentsAfterHead tail ->
                    Just
                        { comments =
                            head.comments
                                |> Rope.prependTo tail.comments
                                |> Rope.prependTo commentsAfterHead
                        , syntax = head.syntax :: tail.syntax
                        }
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
            { comments = Rope.empty
            , syntax = Node range AllPattern
            }
        )


stringPattern : Parser (WithComments (Node Pattern))
stringPattern =
    Tokens.singleOrTripleQuotedStringLiteralMapWithRange
        (\range string ->
            { comments = Rope.empty
            , syntax = Node range (StringPattern string)
            }
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
                range : Range
                range =
                    case argsReverse.syntax of
                        [] ->
                            nameRange

                        (Node lastArgRange _) :: _ ->
                            { start = nameRange.start, end = lastArgRange.end }
            in
            { comments = afterStartName |> Rope.prependTo argsReverse.comments
            , syntax =
                Node range
                    (NamedPattern
                        name
                        (List.reverse argsReverse.syntax)
                    )
            }
        )
        qualifiedNameRefNode
        Layout.optimisticLayout
        (ParserWithComments.manyWithoutReverse
            (Layout.positivelyIndentedFollowedBy
                (ParserFast.map2
                    (\arg commentsAfterArg ->
                        { comments = arg.comments |> Rope.prependTo commentsAfterArg
                        , syntax = arg.syntax
                        }
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
            { comments = Rope.empty
            , syntax =
                Node range
                    (NamedPattern
                        (case after of
                            Nothing ->
                                { moduleName = [], name = firstName }

                            Just ( qualificationAfter, unqualified ) ->
                                { moduleName = firstName :: qualificationAfter, name = unqualified }
                        )
                        []
                    )
            }
        )
        Tokens.typeName
        maybeDotTypeNamesTuple


recordPattern : Parser (WithComments (Node Pattern))
recordPattern =
    ParserFast.map2WithRange
        (\range commentsBeforeElements elements ->
            { comments = commentsBeforeElements |> Rope.prependTo elements.comments
            , syntax =
                Node range (RecordPattern elements.syntax)
            }
        )
        (ParserFast.symbolFollowedBy "{" Layout.maybeLayout)
        (ParserFast.oneOf2
            (ParserFast.map3
                (\head commentsAfterHead tail ->
                    { comments =
                        commentsAfterHead
                            |> Rope.prependTo tail.comments
                    , syntax = head :: tail.syntax
                    }
                )
                Tokens.functionNameNode
                Layout.maybeLayout
                (ParserWithComments.many
                    (ParserFast.symbolFollowedBy ","
                        (ParserFast.map3
                            (\beforeName name afterName ->
                                { comments = beforeName |> Rope.prependTo afterName
                                , syntax = name
                                }
                            )
                            Layout.maybeLayout
                            Tokens.functionNameNode
                            Layout.maybeLayout
                        )
                    )
                )
                |> ParserFast.followedBySymbol "}"
            )
            (ParserFast.symbol "}" { comments = Rope.empty, syntax = [] })
        )
