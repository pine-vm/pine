module Elm.Parser.Modules exposing (..)

import Elm.Parser.Base
import Elm.Parser.Expose
import Elm.Parser.Layout as Layout
import Elm.Parser.Tokens as Tokens
import Elm.Syntax.Module exposing (Module(..))
import Elm.Syntax.Node exposing (Node(..))
import List.Extra
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments)
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
            { comments = commentsAfterFnName |> Rope.prependTo commentsAfterEqual
            , syntax = ( fnName, typeName_ )
            }
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
                    pairs : List ( String, Node String )
                    pairs =
                        head.syntax :: tail.syntax
                in
                { comments =
                    commentsBeforeHead
                        |> Rope.prependTo head.comments
                        |> Rope.prependTo commentsAfterHead
                        |> Rope.prependTo tail.comments
                , syntax =
                    { command =
                        pairs
                            |> List.Extra.find (\( fnName, _ ) -> fnName == "command")
                            |> Maybe.map Tuple.second
                    , subscription =
                        pairs
                            |> List.Extra.find (\( fnName, _ ) -> fnName == "subscription")
                            |> Maybe.map Tuple.second
                    }
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
            { comments = commentsBefore |> Rope.prependTo whereResult.comments
            , syntax = whereResult.syntax
            }
        )
        (ParserFast.keywordFollowedBy "where" Layout.maybeLayout)
        whereBlock


effectModuleDefinition : Parser (WithComments (Node Module))
effectModuleDefinition =
    ParserFast.map7WithRange
        (\range commentsAfterEffect commentsAfterModule name commentsAfterName whereClauses commentsAfterWhereClauses exp ->
            { comments =
                commentsAfterEffect
                    |> Rope.prependTo commentsAfterModule
                    |> Rope.prependTo commentsAfterName
                    |> Rope.prependTo whereClauses.comments
                    |> Rope.prependTo commentsAfterWhereClauses
                    |> Rope.prependTo exp.comments
            , syntax =
                Node range
                    (EffectModule
                        { moduleName = name
                        , exposingList = exp.syntax
                        , command = whereClauses.syntax.command
                        , subscription = whereClauses.syntax.subscription
                        }
                    )
            }
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
            { comments =
                commentsAfterModule
                    |> Rope.prependTo commentsAfterModuleName
                    |> Rope.prependTo exposingList.comments
            , syntax =
                Node range
                    (NormalModule
                        { moduleName = moduleName
                        , exposingList = exposingList.syntax
                        }
                    )
            }
        )
        (ParserFast.keywordFollowedBy "module" Layout.maybeLayout)
        Elm.Parser.Base.moduleName
        Layout.maybeLayout
        Elm.Parser.Expose.exposeDefinition


portModuleDefinition : Parser (WithComments (Node Module))
portModuleDefinition =
    ParserFast.map5WithRange
        (\range commentsAfterPort commentsAfterModule moduleName commentsAfterModuleName exposingList ->
            { comments =
                commentsAfterPort
                    |> Rope.prependTo commentsAfterModule
                    |> Rope.prependTo commentsAfterModuleName
                    |> Rope.prependTo exposingList.comments
            , syntax =
                Node range
                    (PortModule { moduleName = moduleName, exposingList = exposingList.syntax })
            }
        )
        (ParserFast.keywordFollowedBy "port" Layout.maybeLayout)
        (ParserFast.keywordFollowedBy "module" Layout.maybeLayout)
        Elm.Parser.Base.moduleName
        Layout.maybeLayout
        Elm.Parser.Expose.exposeDefinition
