module AppModule exposing (..)


type ChoiceType
    = Variant_Alfa
    | Variant_Beta Int
    | Variant_Gamma String Int


type ChoiceTypeWithMoreArgs a b
    = Variant_Epsilon a b String
    | Variant_Zeta a b String Int
    | Variant_Eta a b String Int ChoiceType


construct_variant_Epsilon : a -> b -> String -> ChoiceTypeWithMoreArgs a b
construct_variant_Epsilon a b c =
    Variant_Epsilon a b c
