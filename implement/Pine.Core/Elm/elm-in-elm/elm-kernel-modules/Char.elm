module Char exposing (..)


type alias Char =
    Int


toCode : Char -> Int
toCode char =
    Pine_builtin.int_add
        [ -- Add the sign prefix byte
          Pine_builtin.concat [ Pine_builtin.take [ 1, 0 ], char ]

        -- Use kernel function 'add' to ensure canonical form
        , 0
        ]


fromCode : Int -> Char
fromCode code =
    -- Remove the sign prefix byte
    Pine_builtin.reverse
        (Pine_builtin.take
            [ 4
            , Pine_builtin.concat
                [ Pine_builtin.reverse (Pine_builtin.skip [ 1, code ])
                , Pine_builtin.skip [ 2, 0x0000000100000000 ]
                ]
            ]
        )


{-| Detect digits `0123456789`

    isDigit '0' == True

    isDigit '1'
        == True
        ... isDigit '9'
        == True

    isDigit 'a' == False

    isDigit 'b' == False

    isDigit 'A' == False

-}
isDigit : Char -> Bool
isDigit char =
    let
        code : Int
        code =
            Pine_builtin.concat [ Pine_builtin.take [ 1, 0 ], char ]
    in
    Pine_builtin.int_is_sorted_asc [ 0x30, code, 0x39 ]


{-| Detect octal digits `01234567`

    isOctDigit '0' == True

    isOctDigit '1'
        == True
        ... isOctDigit '7'
        == True

    isOctDigit '8' == False

    isOctDigit 'a' == False

    isOctDigit 'A' == False

-}
isOctDigit : Char -> Bool
isOctDigit char =
    let
        code : Int
        code =
            Pine_builtin.concat [ Pine_builtin.take [ 1, 0 ], char ]
    in
    Pine_builtin.int_is_sorted_asc [ 0x30, code, 0x37 ]


{-| Detect hexadecimal digits `0123456789abcdefABCDEF`
-}
isHexDigit : Char -> Bool
isHexDigit char =
    let
        code : Int
        code =
            Pine_builtin.concat [ Pine_builtin.take [ 1, 0 ], char ]
    in
    Pine_builtin.int_is_sorted_asc [ 0x30, code, 0x39 ]
        || Pine_builtin.int_is_sorted_asc [ 0x41, code, 0x46 ]
        || Pine_builtin.int_is_sorted_asc [ 0x61, code, 0x66 ]


isUpper : Char -> Bool
isUpper char =
    let
        code : Int
        code =
            Pine_builtin.concat [ Pine_builtin.take [ 1, 0 ], char ]
    in
    Pine_builtin.int_is_sorted_asc [ 0x41, code, 0x5A ]


isLower : Char -> Bool
isLower char =
    let
        code : Int
        code =
            Pine_builtin.concat [ Pine_builtin.take [ 1, 0 ], char ]
    in
    Pine_builtin.int_is_sorted_asc [ 0x61, code, 0x7A ]


isAlpha : Char -> Bool
isAlpha char =
    let
        code : Int
        code =
            Pine_builtin.concat [ Pine_builtin.take [ 1, 0 ], char ]
    in
    if Pine_builtin.int_is_sorted_asc [ 0x41, code, 0x5A ] then
        True

    else
        Pine_builtin.int_is_sorted_asc [ 0x61, code, 0x7A ]


isAlphaNum : Char -> Bool
isAlphaNum char =
    let
        code =
            Pine_builtin.concat [ Pine_builtin.take [ 1, 0 ], char ]
    in
    if Pine_builtin.int_is_sorted_asc [ 0x41, code, 0x5A ] then
        True

    else if Pine_builtin.int_is_sorted_asc [ 0x61, code, 0x7A ] then
        True

    else
        Pine_builtin.int_is_sorted_asc [ 0x30, code, 0x39 ]


toUpper : Char -> Char
toUpper char =
    let
        code =
            Pine_builtin.concat [ Pine_builtin.take [ 1, 0 ], char ]
    in
    if Pine_builtin.int_is_sorted_asc [ 0x61, code, 0x7A ] then
        Pine_builtin.concat
            [ Pine_builtin.take [ 3, Pine_builtin.skip [ 2, 0x0000000100000000 ] ]
            , Pine_builtin.skip [ 1, Pine_builtin.int_add [ code, -0x20 ] ]
            ]

    else
        char


toLower : Char -> Char
toLower char =
    let
        code =
            Pine_builtin.concat [ Pine_builtin.take [ 1, 0 ], char ]
    in
    if Pine_builtin.int_is_sorted_asc [ 0x41, code, 0x5A ] then
        Pine_builtin.concat
            [ Pine_builtin.take [ 3, Pine_builtin.skip [ 2, 0x0000000100000000 ] ]
            , Pine_builtin.skip [ 1, Pine_builtin.int_add [ code, 0x20 ] ]
            ]

    else
        char
