module ElmSyntax.Abstract.Module exposing
    ( DefaultModuleData
    , EffectModuleData
    , Module(..)
    , ModuleName
    , moduleName
    )

import ElmSyntax.Abstract.Exposing exposing (Exposing)


type Module
    = NormalModule DefaultModuleData
    | PortModule DefaultModuleData
    | EffectModule EffectModuleData


type alias DefaultModuleData =
    { moduleName : ModuleName
    , exposingList : Exposing
    }


type alias EffectModuleData =
    { moduleName : ModuleName
    , exposingList : Exposing
    , command : Maybe String
    , subscription : Maybe String
    }


type alias ModuleName =
    List String


moduleName : Module -> ModuleName
moduleName module_ =
    case module_ of
        NormalModule data ->
            data.moduleName

        PortModule data ->
            data.moduleName

        EffectModule data ->
            data.moduleName
