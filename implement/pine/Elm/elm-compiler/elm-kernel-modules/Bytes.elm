module Bytes exposing (..)


type Bytes
    = Elm_Bytes Int


width : Bytes -> Int
width bytes =
    case bytes of
        Elm_Bytes list ->
            Pine_kernel.length list


type Endianness
    = LE
    | BE
