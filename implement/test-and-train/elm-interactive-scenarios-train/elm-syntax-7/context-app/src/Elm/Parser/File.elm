module Elm.Parser.File exposing (file)

import Combine exposing (Parser)
import Elm.Parser.Declarations
import Elm.Parser.Imports
import Elm.Parser.Layout as Layout
import Elm.Parser.Modules
import Elm.Parser.Node
import Elm.Parser.State as State exposing (State)
import Elm.Syntax.Declaration exposing (Declaration)
import Elm.Syntax.File exposing (File)
import Elm.Syntax.Node exposing (Node)


file : Parser State File
file =
    Combine.succeed File
        |> Combine.ignore (Combine.maybe Layout.layoutStrict)
        |> Combine.keep (Elm.Parser.Node.parser Elm.Parser.Modules.moduleDefinition)
        |> Combine.ignore (Combine.maybe Layout.layoutStrict)
        |> Combine.keep (Combine.many Elm.Parser.Imports.importDefinition)
        |> Combine.keep fileDeclarations
        |> Combine.keep collectComments


collectComments : Parser State (List (Node String))
collectComments =
    Combine.withState (State.getComments >> Combine.succeed)


fileDeclarations : Parser State (List (Node Declaration))
fileDeclarations =
    Combine.many
        (Elm.Parser.Declarations.declaration
            |> Combine.ignore (Combine.maybe Layout.layoutStrict)
        )
