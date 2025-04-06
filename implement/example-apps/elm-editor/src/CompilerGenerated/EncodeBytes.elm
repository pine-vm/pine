module CompilerGenerated.EncodeBytes exposing (..)

import Bytes
import Bytes.Encode


bytes_encoder_from_uint32_uint8 : { uint32 : List Int, uint8 : List Int } -> Bytes.Encode.Encoder
bytes_encoder_from_uint32_uint8 { uint32, uint8 } =
    [ uint32 |> List.map (Bytes.Encode.unsignedInt32 Bytes.BE)
    , uint8 |> List.map Bytes.Encode.unsignedInt8
    ]
        |> List.concat
        |> Bytes.Encode.sequence

