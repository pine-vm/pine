module OpaqueCustomType exposing (OpaqueCustomType, constructTagA, constructTagB)


type OpaqueCustomType
    = TagA
    | TagB Int


constructTagA : OpaqueCustomType
constructTagA =
    TagA


constructTagB : Int -> OpaqueCustomType
constructTagB int =
    TagB int
