module CoverageTypes exposing (Choice(..), Envelope(..), Person)


type Choice
    = Empty
    | Single Int
    | Pair Int String
    | Nested (Maybe ( Int, List Int ))
    | Triple Int Bool String


type Envelope
    = Envelope ( Int, { label : String, values : List Int } )


type alias Person =
    { name : String
    , age : Int
    , active : Bool
    }
