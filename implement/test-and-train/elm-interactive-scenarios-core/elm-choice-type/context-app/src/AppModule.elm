module AppModule exposing (..)


type ChoiceType
    = Variant_Alfa
    | Variant_Beta Int
    | Variant_Gamma String Int


type ChoiceTypeWithMoreArgs a b
    = Variant_Epsilon a b String
    | Variant_Zeta a b String Int
    | Variant_Eta a b String Int ChoiceType


construct_variant_Gamma : String -> Int -> ChoiceType
construct_variant_Gamma a b =
    Variant_Gamma a b


construct_variant_Epsilon : a -> b -> String -> ChoiceTypeWithMoreArgs a b
construct_variant_Epsilon a b c =
    Variant_Epsilon a b c


construct_variant_Gamma_from_nested_result : Result String ( String, Result String Int ) -> Result String ChoiceType
construct_variant_Gamma_from_nested_result outerResult =
    case outerResult of
        Ok ( aOk, innerResult ) ->
            case innerResult of
                Err err ->
                    Err ("Inner error: " ++ err)

                Ok b ->
                    Ok (Variant_Gamma aOk b)

        Err err ->
            Err ("Outer error: " ++ err)
