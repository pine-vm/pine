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
import ParserWithComments exposing (WithComments)
import Rope


file : ParserFast.Parser File
file =
    ParserFast.map4
        (\moduleDefinition moduleComments imports declarations ->
            { moduleDefinition = moduleDefinition.syntax
            , imports = imports.syntax
            , declarations = declarations.syntax
            , comments =
                moduleDefinition.comments
                    |> Rope.prependTo moduleComments
                    |> Rope.prependTo imports.comments
                    |> Rope.prependTo declarations.comments
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
                    { comments = declarationParsed.comments |> Rope.prependTo commentsAfter
                    , syntax = declarationParsed.syntax
                    }
                )
                Elm.Parser.Declarations.declaration
                Layout.optimisticLayout
            )
        )
