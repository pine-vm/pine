module Elm.Parser.Imports exposing (importDefinition)

import Elm.Parser.Base
import Elm.Parser.Expose
import Elm.Parser.Layout as Layout
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node exposing (Node(..))
import ParserFast exposing (Parser)
import ParserWithComments exposing (Comments, WithComments)
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
                            { comments = commentsBeforeAlias
                            , syntax =
                                Node { start = start, end = modRange.end }
                                    { moduleName = mod
                                    , moduleAlias = Nothing
                                    , exposingList = Nothing
                                    }
                            }

                        Just exposingListValue ->
                            let
                                (Node exposingRange _) =
                                    exposingListValue.syntax
                            in
                            { comments =
                                commentsBeforeAlias |> Rope.prependTo exposingListValue.comments
                            , syntax =
                                Node { start = start, end = exposingRange.end }
                                    { moduleName = mod
                                    , moduleAlias = Nothing
                                    , exposingList = Just exposingListValue.syntax
                                    }
                            }

                Just moduleAliasResult ->
                    case maybeExposingList of
                        Nothing ->
                            let
                                (Node aliasRange _) =
                                    moduleAliasResult.syntax
                            in
                            { comments =
                                commentsBeforeAlias |> Rope.prependTo moduleAliasResult.comments
                            , syntax =
                                Node { start = start, end = aliasRange.end }
                                    { moduleName = mod
                                    , moduleAlias = Just moduleAliasResult.syntax
                                    , exposingList = Nothing
                                    }
                            }

                        Just exposingListValue ->
                            let
                                (Node exposingRange _) =
                                    exposingListValue.syntax
                            in
                            { comments =
                                commentsBeforeAlias
                                    |> Rope.prependTo moduleAliasResult.comments
                                    |> Rope.prependTo exposingListValue.comments
                            , syntax =
                                Node { start = start, end = exposingRange.end }
                                    { moduleName = mod
                                    , moduleAlias = Just moduleAliasResult.syntax
                                    , exposingList = Just exposingListValue.syntax
                                    }
                            }
        )
        (ParserFast.keywordFollowedBy "import" Layout.maybeLayout)
        Elm.Parser.Base.moduleName
        Layout.optimisticLayout
        (ParserFast.map3OrSucceed
            (\commentsBefore moduleAliasNode commentsAfter ->
                Just
                    { comments = commentsBefore |> Rope.prependTo commentsAfter
                    , syntax = moduleAliasNode
                    }
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
            (\exposingResult commentsAfter ->
                Just
                    { comments = exposingResult.comments |> Rope.prependTo commentsAfter
                    , syntax = exposingResult.syntax
                    }
            )
            Elm.Parser.Expose.exposeDefinition
            Layout.optimisticLayout
            Nothing
        )
