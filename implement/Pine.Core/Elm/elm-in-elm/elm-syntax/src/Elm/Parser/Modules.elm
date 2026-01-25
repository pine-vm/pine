module Elm.Parser.Modules exposing (moduleDefinition)

import Elm.Parser.Base
import Elm.Parser.Expose
import Elm.Parser.Layout as Layout
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node exposing (Node(..))
import List.Extra
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments(..))
import Rope


moduleDefinition : Parser (WithComments (Node Module))
moduleDefinition =
    ParserFast.oneOf3
        normalModuleDefinition
        portModuleDefinition
        effectModuleDefinition


effectWhereClause : Parser (WithComments ( String, Node String ))
effectWhereClause =
    ParserFast.map4
        (\fnName commentsAfterFnName commentsAfterEqual typeName_ ->
            WithComments
                (commentsAfterFnName |> Rope.prependTo commentsAfterEqual)
                ( fnName, typeName_ )
        )
        Tokens.functionName
        Layout.maybeLayout
        (ParserFast.symbolFollowedBy "=" Layout.maybeLayout)
        Tokens.typeNameNode


whereBlock : Parser (WithComments { command : Maybe (Node String), subscription : Maybe (Node String) })
whereBlock =
    ParserFast.symbolFollowedBy "{"
        (ParserFast.map4
            (\commentsBeforeHead head commentsAfterHead tail ->
                let
                    (WithComments headComments headSyntax) =
                        head

                    (WithComments tailComments tailSyntax) =
                        tail

                    pairs : List ( String, Node String )
                    pairs =
                        headSyntax :: tailSyntax
                in
                WithComments
                    (commentsBeforeHead
                        |> Rope.prependTo headComments
                        |> Rope.prependTo commentsAfterHead
                        |> Rope.prependTo tailComments
                    )
                    { command =
                        pairs
                            |> List.Extra.find (\( fnName, _ ) -> fnName == "command")
                            |> Maybe.map Tuple.second
                    , subscription =
                        pairs
                            |> List.Extra.find (\( fnName, _ ) -> fnName == "subscription")
                            |> Maybe.map Tuple.second
                    }
            )
            Layout.maybeLayout
            effectWhereClause
            Layout.maybeLayout
            (ParserWithComments.many
                (ParserFast.symbolFollowedBy "," (Layout.maybeAroundBothSides effectWhereClause))
            )
        )
        |> ParserFast.followedBySymbol "}"


effectWhereClauses : Parser (WithComments { command : Maybe (Node String), subscription : Maybe (Node String) })
effectWhereClauses =
    ParserFast.map2
        (\commentsBefore whereResult ->
            let
                (WithComments whereResultComments whereResultSyntax) =
                    whereResult
            in
            WithComments
                (commentsBefore |> Rope.prependTo whereResultComments)
                whereResultSyntax
        )
        (ParserFast.keywordFollowedBy "where" Layout.maybeLayout)
        whereBlock


effectModuleDefinition : Parser (WithComments (Node Module))
effectModuleDefinition =
    ParserFast.map7WithRange
        (\range commentsAfterEffect commentsAfterModule name commentsAfterName whereClauses commentsAfterWhereClauses exp ->
            let
                (WithComments whereClausesComments whereClausesSyntax) =
                    whereClauses

                (WithComments expComments expSyntax) =
                    exp
            in
            WithComments
                (commentsAfterEffect
                    |> Rope.prependTo commentsAfterModule
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo whereClausesComments
                    |> Rope.prependTo commentsAfterWhereClauses
                    |> Rope.prependTo expComments
                )
                (Node range
                    (EffectModule
                        { moduleName = name
                        , exposingList = expSyntax
                        , command = whereClausesSyntax.command
                        , subscription = whereClausesSyntax.subscription
                        }
                    )
                )
        )
        (ParserFast.keywordFollowedBy "effect" Layout.maybeLayout)
        (ParserFast.keywordFollowedBy "module" Layout.maybeLayout)
        Elm.Parser.Base.moduleName
        Layout.maybeLayout
        effectWhereClauses
        Layout.maybeLayout
        Elm.Parser.Expose.exposeDefinition


normalModuleDefinition : Parser (WithComments (Node Module))
normalModuleDefinition =
    ParserFast.map4WithRange
        (\range commentsAfterModule moduleName commentsAfterModuleName exposingList ->
            let
                (WithComments exposingListComments exposingListSyntax) =
                    exposingList
            in
            WithComments
                (commentsAfterModule
                    |> Rope.prependTo commentsAfterModuleName
                    |> Rope.prependTo exposingListComments
                )
                (Node range
                    (NormalModule
                        { moduleName = moduleName
                        , exposingList = exposingListSyntax
                        }
                    )
                )
        )
        (ParserFast.keywordFollowedBy "module" Layout.maybeLayout)
        Elm.Parser.Base.moduleName
        Layout.maybeLayout
        Elm.Parser.Expose.exposeDefinition


portModuleDefinition : Parser (WithComments (Node Module))
portModuleDefinition =
    ParserFast.map5WithRange
        (\range commentsAfterPort commentsAfterModule moduleName commentsAfterModuleName exposingList ->
            let
                (WithComments exposingListComments exposingListSyntax) =
                    exposingList
            in
            WithComments
                (commentsAfterPort
                    |> Rope.prependTo commentsAfterModule
                    |> Rope.prependTo commentsAfterModuleName
                    |> Rope.prependTo exposingListComments
                )
                (Node range
                    (PortModule { moduleName = moduleName, exposingList = exposingListSyntax })
                )
        )
        (ParserFast.keywordFollowedBy "port" Layout.maybeLayout)
        (ParserFast.keywordFollowedBy "module" Layout.maybeLayout)
        Elm.Parser.Base.moduleName
        Layout.maybeLayout
        Elm.Parser.Expose.exposeDefinition
