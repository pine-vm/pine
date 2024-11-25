module Elm.Parser.Imports exposing (importDefinition)

import Elm.Parser.Base
import Elm.Parser.Expose
import Elm.Parser.Layout as Layout
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node exposing (Node(..))
import ParserFast exposing (Parser)
import ParserWithComments exposing (Comments, WithComments(..))
import Rope


importDefinition : Parser (WithComments (Node Import))
importDefinition =
    ParserFast.map5WithStartLocation
        (\start commentsAfterImport mod commentsAfterModuleName maybeModuleAlias maybeExposingList ->
            let
                commentsBeforeAlias : Comments
                commentsBeforeAlias =
                    commentsAfterImport
                        |> Rope.prependTo commentsAfterModuleName
            in
            case maybeModuleAlias of
                Nothing ->
                    case maybeExposingList of
                        Nothing ->
                            let
                                (Node modRange _) =
                                    mod
                            in
                            WithComments
                                commentsBeforeAlias
                                (Node { start = start, end = modRange.end }
                                    { moduleName = mod
                                    , moduleAlias = Nothing
                                    , exposingList = Nothing
                                    }
                                )

                        Just exposingListValue ->
                            let
                                (WithComments exposingListValueComments exposingListValueSyntax) =
                                    exposingListValue

                                (Node exposingRange _) =
                                    exposingListValueSyntax
                            in
                            WithComments
                                (commentsBeforeAlias |> Rope.prependTo exposingListValueComments)
                                (Node { start = start, end = exposingRange.end }
                                    { moduleName = mod
                                    , moduleAlias = Nothing
                                    , exposingList = Just exposingListValueSyntax
                                    }
                                )

                Just moduleAliasResult ->
                    let
                        (WithComments moduleAliasResultComments moduleAliasResultSyntax) =
                            moduleAliasResult

                        (Node aliasRange _) =
                            moduleAliasResultSyntax
                    in
                    case maybeExposingList of
                        Nothing ->
                            WithComments
                                (commentsBeforeAlias |> Rope.prependTo moduleAliasResultComments)
                                (Node { start = start, end = aliasRange.end }
                                    { moduleName = mod
                                    , moduleAlias = Just moduleAliasResultSyntax
                                    , exposingList = Nothing
                                    }
                                )

                        Just exposingListValue ->
                            let
                                (WithComments exposingListValueComments exposingListValueSyntax) =
                                    exposingListValue

                                (Node exposingRange _) =
                                    exposingListValueSyntax
                            in
                            WithComments
                                (commentsBeforeAlias
                                    |> Rope.prependTo moduleAliasResultComments
                                    |> Rope.prependTo exposingListValueComments
                                )
                                (Node { start = start, end = exposingRange.end }
                                    { moduleName = mod
                                    , moduleAlias = Just moduleAliasResultSyntax
                                    , exposingList = Just exposingListValueSyntax
                                    }
                                )
        )
        (ParserFast.keywordFollowedBy "import" Layout.maybeLayout)
        Elm.Parser.Base.moduleName
        Layout.optimisticLayout
        (ParserFast.map3OrSucceed
            (\commentsBefore moduleAliasNode commentsAfter ->
                Just
                    (WithComments
                        (commentsBefore |> Rope.prependTo commentsAfter)
                        moduleAliasNode
                    )
            )
            (ParserFast.keywordFollowedBy "as" Layout.maybeLayout)
            (Tokens.typeNameMapWithRange
                (\range moduleAlias ->
                    Node range [ moduleAlias ]
                )
            )
            Layout.optimisticLayout
            Nothing
        )
        (ParserFast.map2OrSucceed
            (\(WithComments comments syntax) commentsAfter ->
                Just
                    (WithComments
                        (comments |> Rope.prependTo commentsAfter)
                        syntax
                    )
            )
            Elm.Parser.Expose.exposeDefinition
            Layout.optimisticLayout
            Nothing
        )
