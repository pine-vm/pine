module Elm.Parser.Imports exposing (importDefinition)

import Combine exposing (Parser)
import Elm.Parser.Base
import Elm.Parser.Expose
import Elm.Parser.Layout as Layout
import Elm.Parser.Node
import Elm.Parser.State exposing (State)
import Elm.Parser.Tokens
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Range exposing (Location, Range)


importDefinition : Parser State (Node Import)
importDefinition =
    let
        asDefinition : Parser State (Node ModuleName)
        asDefinition =
            Elm.Parser.Tokens.asToken
                |> Combine.continueWith Layout.layout
                |> Combine.continueWith (Elm.Parser.Node.parser Elm.Parser.Base.moduleName)

        parseExposingDefinition : Node ModuleName -> Maybe (Node ModuleName) -> Parser State Import
        parseExposingDefinition mod asDef =
            Combine.oneOf
                [ Elm.Parser.Node.parser Elm.Parser.Expose.exposeDefinition
                    |> Combine.map Just
                , Combine.succeed Nothing
                ]
                |> Combine.map (\exposing_ -> Import mod asDef exposing_)

        parseAsDefinition : Node () -> Node ModuleName -> Parser State (Node Import)
        parseAsDefinition (Node importKeywordRange ()) mod =
            Combine.oneOf
                [ asDefinition
                    |> Combine.ignore Layout.optimisticLayout
                    |> Combine.andThen (\alias_ -> parseExposingDefinition mod (Just alias_))
                , parseExposingDefinition mod Nothing
                ]
                |> Combine.map (setupNode importKeywordRange.start)
    in
    Combine.succeed parseAsDefinition
        |> Combine.keep (Elm.Parser.Node.parser Elm.Parser.Tokens.importToken)
        |> Combine.ignore Layout.layout
        |> Combine.keep (Elm.Parser.Node.parser Elm.Parser.Base.moduleName)
        |> Combine.ignore Layout.optimisticLayout
        |> Combine.andThen identity
        |> Combine.ignore Layout.optimisticLayout


setupNode : Location -> Import -> Node Import
setupNode start imp =
    let
        endRange : Range
        endRange =
            case imp.moduleAlias of
                Just moduleAlias ->
                    Node.range moduleAlias

                Nothing ->
                    case imp.exposingList of
                        Just exposingList ->
                            Node.range exposingList

                        Nothing ->
                            Node.range imp.moduleName
    in
    Node
        { start = start, end = endRange.end }
        imp
