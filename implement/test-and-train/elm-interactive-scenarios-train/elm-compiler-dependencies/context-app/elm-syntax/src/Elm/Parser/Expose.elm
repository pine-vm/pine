module Elm.Parser.Expose exposing (exposeDefinition)

import Elm.Parser.Layout as Layout
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Node exposing (Node(..))
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments(..))
import Rope


exposeDefinition : Parser (WithComments (Node Exposing))
exposeDefinition =
    ParserFast.map3WithRange
        (\range commentsAfterExposing commentsBefore exposingListInnerResult ->
            let
                (WithComments comments syntax) =
                    exposingListInnerResult
            in
            WithComments
                (commentsAfterExposing
                    |> Rope.prependTo commentsBefore
                    |> Rope.prependTo comments
                )
                (Node range syntax)
        )
        (ParserFast.symbolFollowedBy "exposing" Layout.maybeLayout)
        (ParserFast.symbolFollowedBy "(" Layout.optimisticLayout)
        (exposingListInner
            |> ParserFast.followedBySymbol ")"
        )


exposingListInner : Parser (WithComments Exposing)
exposingListInner =
    ParserFast.oneOf2
        (ParserFast.map3
            (\headElement commentsAfterHeadElement tailElements ->
                let
                    (WithComments headElementComments headElementSyntax) =
                        headElement

                    (WithComments tailElementsComments tailElementsSyntax) =
                        tailElements
                in
                WithComments
                    (headElementComments
                        |> Rope.prependTo commentsAfterHeadElement
                        |> Rope.prependTo tailElementsComments
                    )
                    (Explicit
                        (headElementSyntax
                            :: tailElementsSyntax
                        )
                    )
            )
            exposable
            Layout.maybeLayout
            (ParserWithComments.many
                (ParserFast.symbolFollowedBy ","
                    (Layout.maybeAroundBothSides exposable)
                )
            )
        )
        (ParserFast.mapWithRange
            (\range commentsAfterDotDot ->
                WithComments
                    commentsAfterDotDot
                    (All range)
            )
            (ParserFast.symbolFollowedBy ".." Layout.maybeLayout)
        )


exposable : Parser (WithComments (Node TopLevelExpose))
exposable =
    ParserFast.oneOf3
        functionExpose
        typeExpose
        infixExpose


infixExpose : ParserFast.Parser (WithComments (Node TopLevelExpose))
infixExpose =
    ParserFast.map2WithRange
        (\range infixName () ->
            WithComments
                Rope.empty
                (Node range (InfixExpose infixName))
        )
        (ParserFast.symbolFollowedBy "("
            (ParserFast.ifFollowedByWhileWithoutLinebreak
                (\c -> c /= ')' && c /= '\n' && c /= ' ')
                (\c -> c /= ')' && c /= '\n' && c /= ' ')
            )
        )
        Tokens.parensEnd


typeExpose : Parser (WithComments (Node TopLevelExpose))
typeExpose =
    ParserFast.map3
        (\(Node typeNameRange typeName) commentsBeforeMaybeOpen maybeOpen ->
            let
                (WithComments maybeOpenComments maybeOpenSyntax) =
                    maybeOpen
            in
            WithComments
                (commentsBeforeMaybeOpen |> Rope.prependTo maybeOpenComments)
                (case maybeOpenSyntax of
                    Nothing ->
                        Node typeNameRange (TypeOrAliasExpose typeName)

                    Just openRange ->
                        Node
                            { start = typeNameRange.start
                            , end = openRange.end
                            }
                            (TypeExpose { name = typeName, open = maybeOpenSyntax })
                )
        )
        Tokens.typeNameNode
        Layout.optimisticLayout
        (ParserFast.map2WithRangeOrSucceed
            (\range left right ->
                WithComments
                    (left |> Rope.prependTo right)
                    (Just range)
            )
            (ParserFast.symbolFollowedBy "(" Layout.maybeLayout)
            (ParserFast.symbolFollowedBy ".." Layout.maybeLayout
                |> ParserFast.followedBySymbol ")"
            )
            (WithComments
                Rope.empty
                Nothing
            )
        )


functionExpose : Parser (WithComments (Node TopLevelExpose))
functionExpose =
    Tokens.functionNameMapWithRange
        (\range name ->
            WithComments
                Rope.empty
                (Node range (FunctionExpose name))
        )
