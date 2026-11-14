module AppTypes exposing (App, Payload)


type alias App msg =
    Program () () msg


type alias Payload a =
    { value : a, entries : List a, pair : ( a, a ) }
