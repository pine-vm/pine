using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Xunit;

namespace Pine.IntegrationTests.CodeAnalysis;

public class JsonDecodeParseIntTests
{
    /*
     * This module focuses on a set of functions to parse integers as they could appear in a JSON parser.
     * 
     * See https://github.com/pine-vm/pine/blob/c0f22e4b67533e47d5e8c14a6c415bae1eeeefdf/implement/pine/Elm/elm-compiler/elm-kernel-modules/Json/Decode.elm#L1260-L1391
     * 
     * After compiling from Elm to Pine, we parse the Pine code into a static program.
     * 
     * */

    [Fact]
    public void Parse_static_Json_Decode_parseInt()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            parseInt : Int -> Int -> ( Result String Int, Int )
            parseInt srcBytes offset0 =
                let
                    nextChar =
                        Pine_kernel.take [ 4, Pine_kernel.skip [ offset0, srcBytes ] ]
                in
                if Pine_kernel.equal [ nextChar, '-' ] then
                    -- If we see a minus sign, parse the rest as an unsigned integer
                    let
                        ( unsignedResult, offset1 ) =
                            parseUnsignedInt
                                srcBytes
                                (Pine_kernel.int_add [ offset0, 4 ])
                    in
                    case unsignedResult of
                        Ok unsignedVal ->
                            ( Ok (Pine_kernel.int_mul [ -1, unsignedVal ])
                            , offset1
                            )

                        Err err ->
                            ( Err err
                            , offset1
                            )

                else
                    -- If no minus sign, parse the rest as an unsigned integer
                    parseUnsignedInt srcBytes offset0


            parseUnsignedInt : Int -> Int -> ( Result String Int, Int )
            parseUnsignedInt srcBytes offset0 =
                case Pine_kernel.take [ 4, Pine_kernel.skip [ offset0, srcBytes ] ] of
                    '0' ->
                        ( Ok 0, Pine_kernel.int_add [ offset0, 4 ] )

                    '1' ->
                        parseUnsignedIntRec 1 srcBytes (Pine_kernel.int_add [ offset0, 4 ])

                    '2' ->
                        parseUnsignedIntRec 2 srcBytes (Pine_kernel.int_add [ offset0, 4 ])

                    '3' ->
                        parseUnsignedIntRec 3 srcBytes (Pine_kernel.int_add [ offset0, 4 ])

                    '4' ->
                        parseUnsignedIntRec 4 srcBytes (Pine_kernel.int_add [ offset0, 4 ])

                    '5' ->
                        parseUnsignedIntRec 5 srcBytes (Pine_kernel.int_add [ offset0, 4 ])

                    '6' ->
                        parseUnsignedIntRec 6 srcBytes (Pine_kernel.int_add [ offset0, 4 ])

                    '7' ->
                        parseUnsignedIntRec 7 srcBytes (Pine_kernel.int_add [ offset0, 4 ])

                    '8' ->
                        parseUnsignedIntRec 8 srcBytes (Pine_kernel.int_add [ offset0, 4 ])

                    '9' ->
                        parseUnsignedIntRec 9 srcBytes (Pine_kernel.int_add [ offset0, 4 ])

                    _ ->
                        ( Err "Expecting a digit", offset0 )


            parseUnsignedIntRec : Int -> Int -> Int -> ( Result String Int, Int )
            parseUnsignedIntRec upper srcBytes offset0 =
                case Pine_kernel.take [ 4, Pine_kernel.skip [ offset0, srcBytes ] ] of
                    '0' ->
                        parseUnsignedIntRec
                            (Pine_kernel.int_mul [ upper, 10 ])
                            srcBytes
                            (Pine_kernel.int_add [ offset0, 4 ])

                    '1' ->
                        parseUnsignedIntRec
                            (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 1 ])
                            srcBytes
                            (Pine_kernel.int_add [ offset0, 4 ])

                    '2' ->
                        parseUnsignedIntRec
                            (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 2 ])
                            srcBytes
                            (Pine_kernel.int_add [ offset0, 4 ])

                    '3' ->
                        parseUnsignedIntRec
                            (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 3 ])
                            srcBytes
                            (Pine_kernel.int_add [ offset0, 4 ])

                    '4' ->
                        parseUnsignedIntRec
                            (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 4 ])
                            srcBytes
                            (Pine_kernel.int_add [ offset0, 4 ])

                    '5' ->
                        parseUnsignedIntRec
                            (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 5 ])
                            srcBytes
                            (Pine_kernel.int_add [ offset0, 4 ])

                    '6' ->
                        parseUnsignedIntRec
                            (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 6 ])
                            srcBytes
                            (Pine_kernel.int_add [ offset0, 4 ])

                    '7' ->
                        parseUnsignedIntRec
                            (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 7 ])
                            srcBytes
                            (Pine_kernel.int_add [ offset0, 4 ])

                    '8' ->
                        parseUnsignedIntRec
                            (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 8 ])
                            srcBytes
                            (Pine_kernel.int_add [ offset0, 4 ])

                    '9' ->
                        parseUnsignedIntRec
                            (Pine_kernel.int_add [ Pine_kernel.int_mul [ upper, 10 ], 9 ])
                            srcBytes
                            (Pine_kernel.int_add [ offset0, 4 ])

                    _ ->
                        ( Ok upper, offset0 )
            
            """";

        var parseCache = new PineVMParseCache();

        var (_, staticProgram, functionMetadata) =
            CodeAnalysisTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                includeDeclaration:
                declName =>
                {
                    return declName == DeclQualifiedName.Create(["Test"], "parseInt");
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram, functionMetadata);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.parseInt param_1 param_2 =
                if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_2
                                , param_1
                                ]
                            ]
                        , '-'
                        ]
                then
                    if
                        Pine_kernel.equal
                            [ Pine_kernel.head
                                (Pine_kernel.skip
                                    [ 1
                                    , Pine_kernel.head
                                        (Test.parseUnsignedInt
                                            param_1
                                            (Pine_kernel.int_add
                                                [ param_2
                                                , 4
                                                ]
                                            ))
                                    ]
                                )
                            , Ok
                            ]
                    then
                        [ [ Blob 0x0000003c00000043000000680000006f0000006900000063000000650000003e
                          , Ok
                          , Pine_kernel.int_mul
                              [ -1
                              , Pine_kernel.head
                                  (Pine_kernel.skip
                                      [ 2
                                      , Pine_kernel.head
                                          (Test.parseUnsignedInt
                                              param_1
                                              (Pine_kernel.int_add
                                                  [ param_2
                                                  , 4
                                                  ]
                                              ))
                                      ]
                                  )
                              ]
                          ]
                        , Pine_kernel.head
                            (Pine_kernel.skip
                                [ 1
                                , Test.parseUnsignedInt
                                    param_1
                                    (Pine_kernel.int_add
                                        [ param_2
                                        , 4
                                        ]
                                    )
                                ]
                            )
                        ]

                    else if
                        Pine_kernel.equal
                            [ Pine_kernel.head
                                (Pine_kernel.skip
                                    [ 1
                                    , Pine_kernel.head
                                        (Test.parseUnsignedInt
                                            param_1
                                            (Pine_kernel.int_add
                                                [ param_2
                                                , 4
                                                ]
                                            ))
                                    ]
                                )
                            , Err
                            ]
                    then
                        [ [ Blob 0x0000003c00000043000000680000006f0000006900000063000000650000003e
                          , Err
                          , Pine_kernel.head
                              (Pine_kernel.skip
                                  [ 2
                                  , Pine_kernel.head
                                      (Test.parseUnsignedInt
                                          param_1
                                          (Pine_kernel.int_add
                                              [ param_2
                                              , 4
                                              ]
                                          ))
                                  ]
                              )
                          ]
                        , Pine_kernel.head
                            (Pine_kernel.skip
                                [ 1
                                , Test.parseUnsignedInt
                                    param_1
                                    (Pine_kernel.int_add
                                        [ param_2
                                        , 4
                                        ]
                                    )
                                ]
                            )
                        ]

                    else
                        <always_crash>

                else
                    Test.parseUnsignedInt
                        param_1
                        param_2


            Test.parseUnsignedInt param_1 param_2 =
                if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_2
                                , param_1
                                ]
                            ]
                        , '0'
                        ]
                then
                    [ Ok 0
                    , Pine_kernel.int_add
                        [ param_2
                        , 4
                        ]
                    ]

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_2
                                , param_1
                                ]
                            ]
                        , '1'
                        ]
                then
                    Test.parseUnsignedIntRec
                        1
                        param_1
                        (Pine_kernel.int_add
                            [ param_2
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_2
                                , param_1
                                ]
                            ]
                        , '2'
                        ]
                then
                    Test.parseUnsignedIntRec
                        2
                        param_1
                        (Pine_kernel.int_add
                            [ param_2
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_2
                                , param_1
                                ]
                            ]
                        , '3'
                        ]
                then
                    Test.parseUnsignedIntRec
                        3
                        param_1
                        (Pine_kernel.int_add
                            [ param_2
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_2
                                , param_1
                                ]
                            ]
                        , '4'
                        ]
                then
                    Test.parseUnsignedIntRec
                        4
                        param_1
                        (Pine_kernel.int_add
                            [ param_2
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_2
                                , param_1
                                ]
                            ]
                        , '5'
                        ]
                then
                    Test.parseUnsignedIntRec
                        5
                        param_1
                        (Pine_kernel.int_add
                            [ param_2
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_2
                                , param_1
                                ]
                            ]
                        , '6'
                        ]
                then
                    Test.parseUnsignedIntRec
                        6
                        param_1
                        (Pine_kernel.int_add
                            [ param_2
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_2
                                , param_1
                                ]
                            ]
                        , '7'
                        ]
                then
                    Test.parseUnsignedIntRec
                        7
                        param_1
                        (Pine_kernel.int_add
                            [ param_2
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_2
                                , param_1
                                ]
                            ]
                        , '8'
                        ]
                then
                    Test.parseUnsignedIntRec
                        8
                        param_1
                        (Pine_kernel.int_add
                            [ param_2
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_2
                                , param_1
                                ]
                            ]
                        , '9'
                        ]
                then
                    Test.parseUnsignedIntRec
                        9
                        param_1
                        (Pine_kernel.int_add
                            [ param_2
                            , 4
                            ]
                        )

                else
                    [ Err "Expecting a digit"
                    , param_2
                    ]


            Test.parseUnsignedIntRec param_1 param_2 param_3 =
                if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_3
                                , param_2
                                ]
                            ]
                        , '0'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_mul
                            [ param_1
                            , 10
                            ]
                        )
                        param_2
                        (Pine_kernel.int_add
                            [ param_3
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_3
                                , param_2
                                ]
                            ]
                        , '1'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1
                                , 10
                                ]
                            , 1
                            ]
                        )
                        param_2
                        (Pine_kernel.int_add
                            [ param_3
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_3
                                , param_2
                                ]
                            ]
                        , '2'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1
                                , 10
                                ]
                            , 2
                            ]
                        )
                        param_2
                        (Pine_kernel.int_add
                            [ param_3
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_3
                                , param_2
                                ]
                            ]
                        , '3'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1
                                , 10
                                ]
                            , 3
                            ]
                        )
                        param_2
                        (Pine_kernel.int_add
                            [ param_3
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_3
                                , param_2
                                ]
                            ]
                        , '4'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1
                                , 10
                                ]
                            , 4
                            ]
                        )
                        param_2
                        (Pine_kernel.int_add
                            [ param_3
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_3
                                , param_2
                                ]
                            ]
                        , '5'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1
                                , 10
                                ]
                            , 5
                            ]
                        )
                        param_2
                        (Pine_kernel.int_add
                            [ param_3
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_3
                                , param_2
                                ]
                            ]
                        , '6'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1
                                , 10
                                ]
                            , 6
                            ]
                        )
                        param_2
                        (Pine_kernel.int_add
                            [ param_3
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_3
                                , param_2
                                ]
                            ]
                        , '7'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1
                                , 10
                                ]
                            , 7
                            ]
                        )
                        param_2
                        (Pine_kernel.int_add
                            [ param_3
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_3
                                , param_2
                                ]
                            ]
                        , '8'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1
                                , 10
                                ]
                            , 8
                            ]
                        )
                        param_2
                        (Pine_kernel.int_add
                            [ param_3
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_3
                                , param_2
                                ]
                            ]
                        , '9'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1
                                , 10
                                ]
                            , 9
                            ]
                        )
                        param_2
                        (Pine_kernel.int_add
                            [ param_3
                            , 4
                            ]
                        )

                else
                    [ [ Blob 0x0000003c00000043000000680000006f0000006900000063000000650000003e
                      , Ok
                      , param_1
                      ]
                    , param_3
                    ]
            """".Trim());

    }
}
