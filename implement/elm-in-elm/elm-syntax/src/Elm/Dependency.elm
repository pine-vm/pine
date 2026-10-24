module Elm.Dependency exposing (Dependency, Version)

{-| This module contains types regarding dependencies of a codebase.
To gain the most information of a codebase, information of the dependencies may be required.
For example, what operators does it define, or what constructors are defined for a custom type.


## Types

@docs Dependency, Version

-}

import Dict exposing (Dict)
import Elm.Interface exposing (Interface)
import Elm.Syntax.ModuleName exposing (ModuleName)


{-| Record that represents a dependency. For example:

    { name = "elm/core"
    , version = "1.0.0"
    , interfaces = Dict.fromList [ ( "Basics", basicsInterface ), ... ]
    }

-}
type alias Dependency =
    { name : String
    , version : Version
    , interfaces : Dict ModuleName Interface
    }


{-| Alias for a version string. For example "1.2.3".
-}
type alias Version =
    String
