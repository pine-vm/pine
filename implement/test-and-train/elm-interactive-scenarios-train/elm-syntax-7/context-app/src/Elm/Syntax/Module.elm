module Elm.Syntax.Module exposing
    ( Module(..), DefaultModuleData, EffectModuleData
    , exposingList, moduleName, isPortModule, isEffectModule
    )

{-| This syntax represents module definitions in Elm.
For example:

    module Html.Attributes exposing (style)


## Module

@docs Module, DefaultModuleData, EffectModuleData

@docs exposingList, moduleName, isPortModule, isEffectModule


## Serialization

@docs encode, decoder

-}

import Elm.Syntax.Exposing as Exposing exposing (Exposing)
import Elm.Syntax.ModuleName as ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node)


{-| Union type for different kind of modules
-}
type Module
    = NormalModule DefaultModuleData
    | PortModule DefaultModuleData
    | EffectModule EffectModuleData


{-| Data for a default default
-}
type alias DefaultModuleData =
    { moduleName : Node ModuleName
    , exposingList : Node Exposing
    }


{-| Data for an effect module
-}
type alias EffectModuleData =
    { moduleName : Node ModuleName
    , exposingList : Node Exposing
    , command : Maybe (Node String)
    , subscription : Maybe (Node String)
    }


{-| Get the name for a module. For older modules this may not be present.
-}
moduleName : Module -> ModuleName
moduleName m =
    case m of
        NormalModule x ->
            Node.value x.moduleName

        PortModule x ->
            Node.value x.moduleName

        EffectModule x ->
            Node.value x.moduleName


{-| Get the exposing list for a module.
-}
exposingList : Module -> Exposing
exposingList m =
    case m of
        NormalModule x ->
            Node.value x.exposingList

        PortModule x ->
            Node.value x.exposingList

        EffectModule x ->
            Node.value x.exposingList


{-| Check whether a module is defined as a port-module
-}
isPortModule : Module -> Bool
isPortModule m =
    case m of
        PortModule _ ->
            True

        _ ->
            False


{-| Check whether a module is defined as an effect-module
-}
isEffectModule : Module -> Bool
isEffectModule m =
    case m of
        EffectModule _ ->
            True

        _ ->
            False
