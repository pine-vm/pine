module DecodeExtra exposing (..)

import Bytes.Decode


list : Int -> Bytes.Decode.Decoder a -> Bytes.Decode.Decoder (List a)
list length aDecoder =
    Bytes.Decode.loop
        ( length, [] )
        (listStep aDecoder)


listStep :
    Bytes.Decode.Decoder a
    -> ( Int, List a )
    -> Bytes.Decode.Decoder (Bytes.Decode.Step ( Int, List a ) (List a))
listStep elementDecoder ( n, elements ) =
    if n <= 0 then
        Bytes.Decode.succeed
            (Bytes.Decode.Done (List.reverse elements))

    else
        Bytes.Decode.map
            (\element -> Bytes.Decode.Loop ( n - 1, element :: elements ))
            elementDecoder
