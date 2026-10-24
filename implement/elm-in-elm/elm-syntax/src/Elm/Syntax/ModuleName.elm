module Elm.Syntax.ModuleName exposing (ModuleName)

{-| This syntax represents the module names in Elm. These can be used for imports, module names (duh), and for qualified access.
For example:

    module Elm.Syntax.ModuleName ...

    import Foo.Bar ...

    import ... as Something

    My.Module.something

    My.Module.SomeType


## Types

@docs ModuleName


## Serialization

@docs encode, decoder

-}


{-| Base representation for a module name
-}
type alias ModuleName =
    List String
