module Elm.Parser.File exposing (file)

import Elm.Parser.Comments as Comments
import Elm.Parser.Declarations
import Elm.Parser.Imports
import Elm.Parser.Layout as Layout
import Elm.Parser.Modules
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node)
import ParserFast exposing (Parser)
import ParserWithComments exposing (WithComments(..))
import Rope


file : ParserFast.Parser File
file =
    ParserFast.map4
        (\moduleDefinition moduleComments imports declarations ->
            let
                (WithComments moduleDefinitionComments moduleDefinitionSyntax) =
                    moduleDefinition

                (WithComments importsComments importsSyntax) =
                    imports

                (WithComments declarationsComments declarationsSyntax) =
                    declarations
            in
            { moduleDefinition = moduleDefinitionSyntax
            , imports = importsSyntax
            , declarations = declarationsSyntax
            , comments =
                moduleDefinitionComments
                    |> Rope.prependTo moduleComments
                    |> Rope.prependTo importsComments
                    |> Rope.prependTo declarationsComments
                    |> Rope.toList
            }
        )
        (Layout.layoutStrictFollowedByWithComments
            Elm.Parser.Modules.moduleDefinition
        )
        (Layout.layoutStrictFollowedByComments
            (ParserFast.map2OrSucceed
                (\moduleDocumentation commentsAfter ->
                    Rope.one moduleDocumentation |> Rope.filledPrependTo commentsAfter
                )
                Comments.moduleDocumentation
                Layout.layoutStrict
                Rope.empty
            )
        )
        (ParserWithComments.many Elm.Parser.Imports.importDefinition)
        fileDeclarations


fileDeclarations : Parser (WithComments (List (Node Declaration)))
fileDeclarations =
    ParserWithComments.many
        (Layout.moduleLevelIndentationFollowedBy
            (ParserFast.map2
                (\declarationParsed commentsAfter ->
                    let
                        (WithComments declarationComments declarationSyntax) =
                            declarationParsed
                    in
                    WithComments
                        (declarationComments |> Rope.prependTo commentsAfter)
                        declarationSyntax
                )
                Elm.Parser.Declarations.declaration
                Layout.optimisticLayout
            )
        )
