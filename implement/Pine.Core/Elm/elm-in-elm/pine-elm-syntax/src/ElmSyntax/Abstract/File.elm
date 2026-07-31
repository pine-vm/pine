module ElmSyntax.Abstract.File exposing (File)

import ElmSyntax.Abstract.Declaration exposing (Declaration)
import ElmSyntax.Abstract.Import exposing (Import)
import ElmSyntax.Abstract.Module exposing (Module)


type alias File =
    { moduleDefinition : Module
    , imports : List Import
    , declarations : List Declaration
    }
