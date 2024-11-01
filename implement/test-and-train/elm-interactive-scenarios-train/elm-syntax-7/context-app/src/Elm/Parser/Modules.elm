module Elm.Parser.Modules exposing (moduleDefinition)

import Combine exposing (Parser)
import Elm.Parser.Base
import Elm.Parser.Expose
import Elm.Parser.Layout as Layout
import Elm.Parser.Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens
import Elm.Syntax.Exposing exposing (Exposing)
import Elm.Syntax.Module exposing (DefaultModuleData, Module(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)


moduleDefinition : Parser State Module
moduleDefinition =
    Combine.oneOf
        [ normalModuleDefinition
        , portModuleDefinition
        , effectModuleDefinition
        ]


effectWhereClause : Parser State ( String, Node String )
effectWhereClause =
    Combine.succeed Tuple.pair
        |> Combine.keep Elm.Parser.Tokens.functionName
        |> Combine.ignore (Layout.maybeAroundBothSides (Combine.string "="))
        |> Combine.keep (Elm.Parser.Node.parser Elm.Parser.Tokens.typeName)


whereBlock : Parser State { command : Maybe (Node String), subscription : Maybe (Node String) }
whereBlock =
    Combine.between
        (Combine.string "{")
        (Combine.string "}")
        (Combine.sepBy1 (Combine.string ",")
            (Layout.maybeAroundBothSides effectWhereClause)
        )
        |> Combine.map
            (\pairs ->
                { command = pairs |> List.filter (Tuple.first >> (==) "command") |> List.head |> Maybe.map Tuple.second
                , subscription = pairs |> List.filter (Tuple.first >> (==) "subscription") |> List.head |> Maybe.map Tuple.second
                }
            )


effectWhereClauses : Parser State { command : Maybe (Node String), subscription : Maybe (Node String) }
effectWhereClauses =
    Combine.string "where"
        |> Combine.continueWith Layout.layout
        |> Combine.continueWith whereBlock


effectModuleDefinition : Parser State Module
effectModuleDefinition =
    let
        createEffectModule : Node ModuleName -> { command : Maybe (Node String), subscription : Maybe (Node String) } -> Node Exposing -> Module
        createEffectModule name whereClauses exp =
            EffectModule
                { moduleName = name
                , exposingList = exp
                , command = whereClauses.command
                , subscription = whereClauses.subscription
                }
    in
    Combine.succeed createEffectModule
        |> Combine.ignore (Combine.string "effect")
        |> Combine.ignore Layout.layout
        |> Combine.ignore Elm.Parser.Tokens.moduleToken
        |> Combine.ignore Layout.layout
        |> Combine.keep (Elm.Parser.Node.parser Elm.Parser.Base.moduleName)
        |> Combine.ignore Layout.layout
        |> Combine.keep effectWhereClauses
        |> Combine.ignore Layout.layout
        |> Combine.keep (Elm.Parser.Node.parser Elm.Parser.Expose.exposeDefinition)


normalModuleDefinition : Parser State Module
normalModuleDefinition =
    Combine.map NormalModule
        (Combine.succeed DefaultModuleData
            |> Combine.ignore Elm.Parser.Tokens.moduleToken
            |> Combine.ignore Layout.layout
            |> Combine.keep (Elm.Parser.Node.parser Elm.Parser.Base.moduleName)
            |> Combine.ignore Layout.layout
            |> Combine.keep (Elm.Parser.Node.parser Elm.Parser.Expose.exposeDefinition)
        )


portModuleDefinition : Parser State Module
portModuleDefinition =
    Combine.map PortModule
        (Combine.succeed DefaultModuleData
            |> Combine.ignore Elm.Parser.Tokens.portToken
            |> Combine.ignore Layout.layout
            |> Combine.ignore Elm.Parser.Tokens.moduleToken
            |> Combine.ignore Layout.layout
            |> Combine.keep (Elm.Parser.Node.parser Elm.Parser.Base.moduleName)
            |> Combine.ignore Layout.layout
            |> Combine.keep (Elm.Parser.Node.parser Elm.Parser.Expose.exposeDefinition)
        )
