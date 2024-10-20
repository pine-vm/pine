module Elm.Syntax.File exposing (File)

{-| This syntax represents a whole Elm file.


## Types

@docs File


## Serialization

@docs encode, decoder

-}

import Elm.Syntax.Comments as Comments exposing (Comment)
import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Import as Import exposing (Import)
import Elm.Syntax.Module as Module exposing (Module)
import Elm.Syntax.Node as Node exposing (Node)


{-| Type annotation for a file
-}
type alias File =
    { moduleDefinition : Node Module
    , imports : List (Node Import)
    , declarations : List (Node Declaration)
    , comments : List (Node Comment)
    }
