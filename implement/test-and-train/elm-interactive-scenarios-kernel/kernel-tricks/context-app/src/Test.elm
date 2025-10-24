module Test exposing (..)


isBlobValue : a -> Bool
isBlobValue value =
    Pine_kernel.equal
        [ Pine_kernel.take [ 0, value ]
        , Pine_kernel.take [ 0, 123 ]
        ]


isListValue : a -> Bool
isListValue value =
    Pine_kernel.equal
        [ Pine_kernel.take [ 0, value ]
        , []
        ]


isBlobValueOfLength_7 : a -> Bool
isBlobValueOfLength_7 value =
    if
        Pine_kernel.equal
            [ Pine_kernel.take [ 0, value ]
            , Pine_kernel.take [ 0, 123 ]
            ]
    then
        Pine_kernel.equal
            [ Pine_kernel.length value
            , 7
            ]

    else
        False


isListValueOfLength_13 : a -> Bool
isListValueOfLength_13 value =
    if
        Pine_kernel.equal
            [ Pine_kernel.take [ 0, value ]
            , []
            ]
    then
        Pine_kernel.equal
            [ Pine_kernel.length value
            , 13
            ]

    else
        False
