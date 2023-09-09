module ConsumeExposingNamedType exposing (..)

import TypesExposingAll exposing (SimpleChoiceType(..))


construct_variant_Alfa : SimpleChoiceType
construct_variant_Alfa =
    Variant_Alfa


construct_variant_Beta : Int -> SimpleChoiceType
construct_variant_Beta int =
    Variant_Beta int
