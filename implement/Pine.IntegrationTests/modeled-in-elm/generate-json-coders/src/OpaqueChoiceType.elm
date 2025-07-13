module OpaqueChoiceType exposing (OpaqueChoiceType, constructTagA, constructTagB)


type OpaqueChoiceType
    = TagA
    | TagB Int


constructTagA : OpaqueChoiceType
constructTagA =
    TagA


constructTagB : Int -> OpaqueChoiceType
constructTagB int =
    TagB int
