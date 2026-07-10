using AwesomeAssertions;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Internal;
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
     * Then we use the static program IR to build an optimized encoding in a lower level language.
     * 
     * Optimizations we want to see here:
     * 
     * + Inlined function to enable downstream optimizations.
     * + Consolidation of branching to distinguish values between List and Blob.
     * + Recursive tail calls coverted to loops.
     * + Specialized representation of the accumulated integer value in 'parseUnsignedInt' to avoid internal conversion to/from Pine values.
     * + Specialized representation of the accumulated integer value returned from 'parseUnsignedInt' so that the negation can be done without conversion from Pine value.
     * + Specialized representation of the offset value limited to Int64, because we can proof termination if that value exceeds the blob length.
     * */

    [Fact]
    public void Parse_and_emit_optimized_Json_Decode_parseInt()
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

        var (parsedEnv, staticProgram, functionMetadata) =
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
                        if
                            Pine_kernel.equal
                                [ Pine_kernel.length
                                    (Pine_kernel.head
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
                                    )
                                , 1
                                ]
                        then
                            Pine_kernel.equal
                                [ Pine_kernel.head
                                    (Pine_kernel.head
                                        (Test.parseUnsignedInt
                                            param_1
                                            (Pine_kernel.int_add
                                                [ param_2
                                                , 4
                                                ]
                                            ))
                                    )
                                , Ok
                                ]

                        else
                            False
                    then
                        [ [ Ok
                          , [ Pine_kernel.int_mul
                                [ -1
                                , Pine_kernel.head
                                    (Pine_kernel.head
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
                                    )
                                ]
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

                    else
                        [ [ Err
                          , [ Pine_kernel.head
                                (Pine_kernel.head
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
                    [ [ Ok
                      , [ param_1
                        ]
                      ]
                    , param_3
                    ]
            
            """".Trim());

        var asCSharp =
            Core.DotNet.StaticProgramCSharp.FromStaticProgram(
                staticProgram,
                functionMetadata,
                CodeAnalysisTestHelper.DeclarationSyntaxContext);

        var testClass = asCSharp.ModulesClasses[DeclQualifiedName.Create([], "Test")];

        var moduleTestCSharpText =
            testClass.RenderToString();

        var commonValuesClassText =
            Core.DotNet.StaticProgramCSharpClass.RenderToString(asCSharp.CommonValueClass);

        moduleTestCSharpText.Trim().Should().Be(
            """"
            public static class Test
            {
                public static PineValue parseInt(PineValue param_1, PineValue param_2)
                {
                    if (KernelFunctionFused.SkipAndTake(takeCount: 4, skipCountValue: param_2, argument: param_1) ==
                        CommonReusedValues.Blob_Char_hyphen)
                    {
                        PineValue local_001 = KernelFunctionSpecialized.int_add(4, param_2);
                        PineValue local_003 = Test.parseUnsignedInt(param_1, local_001);
                        PineValue local_004 = PineValueExtension.ValueFromPathOrEmptyList(local_003, [0]);
                        PineValue local_008 = PineValueExtension.ValueFromPathOrEmptyList(local_003, [1]);
                        PineValue local_010 = PineValueExtension.ValueFromPathOrEmptyList(local_004, [1]);
                        PineValue local_011 = PineValueExtension.ValueFromPathOrEmptyList(local_010, [0]);

                        if ((KernelFunctionSpecialized.length_as_int(local_010) == 1) &&
                            (PineValueExtension.ValueFromPathOrEmptyList(local_004, [0]) == CommonReusedValues.Blob_Str_Ok))
                        {
                            return
                                PineValue.List(
                                    [
                                    PineValue.List(
                                        [
                                        CommonReusedValues.Blob_Str_Ok,
                                        PineValue.List([KernelFunctionSpecialized.int_mul(-1, local_011)])
                                        ]),
                                    local_008
                                    ]);
                        }

                        return
                            PineValue.List(
                                [
                                PineValue.List([CommonReusedValues.Blob_Str_Err, PineValue.List([local_011])]),
                                local_008
                                ]);
                    }

                    return Test.parseUnsignedInt(param_1, param_2);
                }

                public static PineValue parseUnsignedInt(PineValue param_1, PineValue param_2)
                {
                    PineValue local_000 = KernelFunctionFused.SkipAndTake(takeCount: 4, skipCountValue: param_2, argument: param_1);

                    if (local_000 == CommonReusedValues.Blob_Char_digit_0)
                    {
                        return PineValue.List([CommonReusedValues.List_c3304aab, KernelFunctionSpecialized.int_add(4, param_2)]);
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_1)
                    {
                        return
                            Test.parseUnsignedIntRec(
                                CommonReusedValues.Blob_Int_1,
                                param_1,
                                KernelFunctionSpecialized.int_add(4, param_2));
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_2)
                    {
                        return
                            Test.parseUnsignedIntRec(
                                CommonReusedValues.Blob_Int_2,
                                param_1,
                                KernelFunctionSpecialized.int_add(4, param_2));
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_3)
                    {
                        return
                            Test.parseUnsignedIntRec(
                                CommonReusedValues.Blob_Int_3,
                                param_1,
                                KernelFunctionSpecialized.int_add(4, param_2));
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_4)
                    {
                        return
                            Test.parseUnsignedIntRec(
                                CommonReusedValues.Blob_Int_4,
                                param_1,
                                KernelFunctionSpecialized.int_add(4, param_2));
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_5)
                    {
                        return
                            Test.parseUnsignedIntRec(
                                CommonReusedValues.Blob_Int_5,
                                param_1,
                                KernelFunctionSpecialized.int_add(4, param_2));
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_6)
                    {
                        return
                            Test.parseUnsignedIntRec(
                                CommonReusedValues.Blob_Int_6,
                                param_1,
                                KernelFunctionSpecialized.int_add(4, param_2));
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_7)
                    {
                        return
                            Test.parseUnsignedIntRec(
                                CommonReusedValues.Blob_Int_7,
                                param_1,
                                KernelFunctionSpecialized.int_add(4, param_2));
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_8)
                    {
                        return
                            Test.parseUnsignedIntRec(
                                CommonReusedValues.Blob_Int_8,
                                param_1,
                                KernelFunctionSpecialized.int_add(4, param_2));
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_9)
                    {
                        return
                            Test.parseUnsignedIntRec(
                                CommonReusedValues.Blob_Int_9,
                                param_1,
                                KernelFunctionSpecialized.int_add(4, param_2));
                    }

                    return PineValue.List([CommonReusedValues.List_ae45bd54, param_2]);
                }

                public static PineValue parseUnsignedIntRec(PineValue param_1, PineValue param_2, PineValue param_3)
                {
                    PineValue local_param_1 = param_1;
                    PineValue local_param_2 = param_2;
                    PineValue local_param_3 = param_3;

                    while (true)
                    {
                        PineValue local_000 =
                            KernelFunctionFused.SkipAndTake(takeCount: 4, skipCountValue: local_param_3, argument: local_param_2);

                        if (local_000 == CommonReusedValues.Blob_Char_digit_0)
                        {
                            {
                                PineValue local_param_1_temp = KernelFunctionSpecialized.int_mul(10, local_param_1);
                                PineValue local_param_3_temp = KernelFunctionSpecialized.int_add(4, local_param_3);
                                local_param_1 = local_param_1_temp;
                                local_param_3 = local_param_3_temp;
                            }

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_1)
                        {
                            {
                                PineValue local_param_1_temp =
                                    KernelFunctionSpecialized.int_add(1, KernelFunctionSpecialized.int_mul(10, local_param_1));

                                PineValue local_param_3_temp = KernelFunctionSpecialized.int_add(4, local_param_3);
                                local_param_1 = local_param_1_temp;
                                local_param_3 = local_param_3_temp;
                            }

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_2)
                        {
                            {
                                PineValue local_param_1_temp =
                                    KernelFunctionSpecialized.int_add(2, KernelFunctionSpecialized.int_mul(10, local_param_1));

                                PineValue local_param_3_temp = KernelFunctionSpecialized.int_add(4, local_param_3);
                                local_param_1 = local_param_1_temp;
                                local_param_3 = local_param_3_temp;
                            }

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_3)
                        {
                            {
                                PineValue local_param_1_temp =
                                    KernelFunctionSpecialized.int_add(3, KernelFunctionSpecialized.int_mul(10, local_param_1));

                                PineValue local_param_3_temp = KernelFunctionSpecialized.int_add(4, local_param_3);
                                local_param_1 = local_param_1_temp;
                                local_param_3 = local_param_3_temp;
                            }

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_4)
                        {
                            {
                                PineValue local_param_1_temp =
                                    KernelFunctionSpecialized.int_add(4, KernelFunctionSpecialized.int_mul(10, local_param_1));

                                PineValue local_param_3_temp = KernelFunctionSpecialized.int_add(4, local_param_3);
                                local_param_1 = local_param_1_temp;
                                local_param_3 = local_param_3_temp;
                            }

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_5)
                        {
                            {
                                PineValue local_param_1_temp =
                                    KernelFunctionSpecialized.int_add(5, KernelFunctionSpecialized.int_mul(10, local_param_1));

                                PineValue local_param_3_temp = KernelFunctionSpecialized.int_add(4, local_param_3);
                                local_param_1 = local_param_1_temp;
                                local_param_3 = local_param_3_temp;
                            }

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_6)
                        {
                            {
                                PineValue local_param_1_temp =
                                    KernelFunctionSpecialized.int_add(6, KernelFunctionSpecialized.int_mul(10, local_param_1));

                                PineValue local_param_3_temp = KernelFunctionSpecialized.int_add(4, local_param_3);
                                local_param_1 = local_param_1_temp;
                                local_param_3 = local_param_3_temp;
                            }

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_7)
                        {
                            {
                                PineValue local_param_1_temp =
                                    KernelFunctionSpecialized.int_add(7, KernelFunctionSpecialized.int_mul(10, local_param_1));

                                PineValue local_param_3_temp = KernelFunctionSpecialized.int_add(4, local_param_3);
                                local_param_1 = local_param_1_temp;
                                local_param_3 = local_param_3_temp;
                            }

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_8)
                        {
                            {
                                PineValue local_param_1_temp =
                                    KernelFunctionSpecialized.int_add(8, KernelFunctionSpecialized.int_mul(10, local_param_1));

                                PineValue local_param_3_temp = KernelFunctionSpecialized.int_add(4, local_param_3);
                                local_param_1 = local_param_1_temp;
                                local_param_3 = local_param_3_temp;
                            }

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_9)
                        {
                            {
                                PineValue local_param_1_temp =
                                    KernelFunctionSpecialized.int_add(9, KernelFunctionSpecialized.int_mul(10, local_param_1));

                                PineValue local_param_3_temp = KernelFunctionSpecialized.int_add(4, local_param_3);
                                local_param_1 = local_param_1_temp;
                                local_param_3 = local_param_3_temp;
                            }

                            continue;
                        }

                        return
                            PineValue.List(
                                [
                                PineValue.List([CommonReusedValues.Blob_Str_Ok, PineValue.List([local_param_1])]),
                                local_param_3
                                ]);
                    }
                }
            }
            
            """".Trim());

        commonValuesClassText.Trim().Should().Be(
            """"
            public static class CommonReusedValues
            {
                public static readonly PineValue Blob_Int_neg_1 = IntegerEncoding.EncodeSignedInteger(-1);

                public static readonly PineValue Blob_Int_0 = IntegerEncoding.EncodeSignedInteger(0);

                public static readonly PineValue Blob_Int_1 = IntegerEncoding.EncodeSignedInteger(1);

                public static readonly PineValue Blob_Int_2 = IntegerEncoding.EncodeSignedInteger(2);

                public static readonly PineValue Blob_Int_3 = IntegerEncoding.EncodeSignedInteger(3);

                public static readonly PineValue Blob_Int_4 = IntegerEncoding.EncodeSignedInteger(4);

                public static readonly PineValue Blob_Int_5 = IntegerEncoding.EncodeSignedInteger(5);

                public static readonly PineValue Blob_Int_6 = IntegerEncoding.EncodeSignedInteger(6);

                public static readonly PineValue Blob_Int_7 = IntegerEncoding.EncodeSignedInteger(7);

                public static readonly PineValue Blob_Int_8 = IntegerEncoding.EncodeSignedInteger(8);

                public static readonly PineValue Blob_Int_9 = IntegerEncoding.EncodeSignedInteger(9);

                public static readonly PineValue Blob_Int_10 = IntegerEncoding.EncodeSignedInteger(10);

                public static readonly PineValue Blob_Char_hyphen = StringEncoding.ValueFromString("-");

                public static readonly PineValue Blob_Char_digit_0 = StringEncoding.ValueFromString("0");

                public static readonly PineValue Blob_Char_digit_1 = StringEncoding.ValueFromString("1");

                public static readonly PineValue Blob_Char_digit_2 = StringEncoding.ValueFromString("2");

                public static readonly PineValue Blob_Char_digit_3 = StringEncoding.ValueFromString("3");

                public static readonly PineValue Blob_Char_digit_4 = StringEncoding.ValueFromString("4");

                public static readonly PineValue Blob_Char_digit_5 = StringEncoding.ValueFromString("5");

                public static readonly PineValue Blob_Char_digit_6 = StringEncoding.ValueFromString("6");

                public static readonly PineValue Blob_Char_digit_7 = StringEncoding.ValueFromString("7");

                public static readonly PineValue Blob_Char_digit_8 = StringEncoding.ValueFromString("8");

                public static readonly PineValue Blob_Char_digit_9 = StringEncoding.ValueFromString("9");

                public static readonly PineValue Blob_Str_Ok = StringEncoding.ValueFromString("Ok");

                public static readonly PineValue Blob_Str_Err = StringEncoding.ValueFromString("Err");

                public static readonly PineValue Blob_Str_List = StringEncoding.ValueFromString("List");

                public static readonly PineValue Blob_Str_head = StringEncoding.ValueFromString("head");

                public static readonly PineValue Blob_Str_skip = StringEncoding.ValueFromString("skip");

                public static readonly PineValue Blob_Str_take = StringEncoding.ValueFromString("take");

                public static readonly PineValue Blob_Str_equal = StringEncoding.ValueFromString("equal");

                public static readonly PineValue Blob_Str_String = StringEncoding.ValueFromString("String");

                public static readonly PineValue Blob_Str_length = StringEncoding.ValueFromString("length");

                public static readonly PineValue Blob_Str_Literal = StringEncoding.ValueFromString("Literal");

                public static readonly PineValue Blob_Str_int_add = StringEncoding.ValueFromString("int_add");

                public static readonly PineValue Blob_Str_int_mul = StringEncoding.ValueFromString("int_mul");

                public static readonly PineValue Blob_Str_Conditional = StringEncoding.ValueFromString("Conditional");

                public static readonly PineValue Blob_Str_Environment = StringEncoding.ValueFromString("Environment");

                public static readonly PineValue Blob_Str_ParseAndEval = StringEncoding.ValueFromString("ParseAndEval");

                public static readonly PineValue Blob_cfca8ee1 = StringEncoding.ValueFromString("Expecting a digit");

                public static readonly PineValue Blob_Str_KernelApplication = StringEncoding.ValueFromString("KernelApplication");

                public static readonly PineValue List_Single_List_Empty = PineValue.List([PineValue.EmptyList]);

                public static readonly PineValue List_Single_Bool_False = PineValue.List([PineKernelValues.FalseValue]);

                public static readonly PineValue List_Single_Blob_Int_neg_1 =
                    PineValue.List([IntegerEncoding.EncodeSignedInteger(-1)]);

                public static readonly PineValue List_Single_Blob_Int_0 = PineValue.List([IntegerEncoding.EncodeSignedInteger(0)]);

                public static readonly PineValue List_Single_Blob_Int_1 = PineValue.List([IntegerEncoding.EncodeSignedInteger(1)]);

                public static readonly PineValue List_Single_Blob_Int_2 = PineValue.List([IntegerEncoding.EncodeSignedInteger(2)]);

                public static readonly PineValue List_Single_Blob_Int_3 = PineValue.List([IntegerEncoding.EncodeSignedInteger(3)]);

                public static readonly PineValue List_Single_Blob_Int_4 = PineValue.List([IntegerEncoding.EncodeSignedInteger(4)]);

                public static readonly PineValue List_Single_Blob_Int_5 = PineValue.List([IntegerEncoding.EncodeSignedInteger(5)]);

                public static readonly PineValue List_Single_Blob_Int_6 = PineValue.List([IntegerEncoding.EncodeSignedInteger(6)]);

                public static readonly PineValue List_Single_Blob_Int_7 = PineValue.List([IntegerEncoding.EncodeSignedInteger(7)]);

                public static readonly PineValue List_Single_Blob_Int_8 = PineValue.List([IntegerEncoding.EncodeSignedInteger(8)]);

                public static readonly PineValue List_Single_Blob_Int_9 = PineValue.List([IntegerEncoding.EncodeSignedInteger(9)]);

                public static readonly PineValue List_Single_Blob_Int_10 =
                    PineValue.List([IntegerEncoding.EncodeSignedInteger(10)]);

                public static readonly PineValue List_Single_Blob_Char_hyphen = PineValue.List([Blob_Char_hyphen]);

                public static readonly PineValue List_Single_Blob_Char_digit_0 = PineValue.List([Blob_Char_digit_0]);

                public static readonly PineValue List_Single_Blob_Char_digit_1 = PineValue.List([Blob_Char_digit_1]);

                public static readonly PineValue List_Single_Blob_Char_digit_2 = PineValue.List([Blob_Char_digit_2]);

                public static readonly PineValue List_Single_Blob_Char_digit_3 = PineValue.List([Blob_Char_digit_3]);

                public static readonly PineValue List_Single_Blob_Char_digit_4 = PineValue.List([Blob_Char_digit_4]);

                public static readonly PineValue List_Single_Blob_Char_digit_5 = PineValue.List([Blob_Char_digit_5]);

                public static readonly PineValue List_Single_Blob_Char_digit_6 = PineValue.List([Blob_Char_digit_6]);

                public static readonly PineValue List_Single_Blob_Char_digit_7 = PineValue.List([Blob_Char_digit_7]);

                public static readonly PineValue List_Single_Blob_Char_digit_8 = PineValue.List([Blob_Char_digit_8]);

                public static readonly PineValue List_Single_Blob_Char_digit_9 = PineValue.List([Blob_Char_digit_9]);

                public static readonly PineValue List_Single_Blob_Str_Ok = PineValue.List([Blob_Str_Ok]);

                public static readonly PineValue List_Single_Blob_Str_Err = PineValue.List([Blob_Str_Err]);

                public static readonly PineValue List_c3304aab = PineValue.List([Blob_Str_Ok, List_Single_Blob_Int_0]);

                public static readonly PineValue List_Single_List_c3304aab = PineValue.List([List_c3304aab]);

                public static readonly PineValue List_e190d1f5 = PineValue.List([Blob_Str_Literal, List_Single_List_Empty]);

                public static readonly PineValue List_bb14d771 = PineValue.List([Blob_Str_Literal, List_Single_Bool_False]);

                public static readonly PineValue List_56404869 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_neg_1]);

                public static readonly PineValue List_0dcd86c0 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_1]);

                public static readonly PineValue List_43b95777 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_2]);

                public static readonly PineValue List_450c12a0 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_3]);

                public static readonly PineValue List_0c82888c = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_4]);

                public static readonly PineValue List_c1b27e6e = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_5]);

                public static readonly PineValue List_282dee3a = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_6]);

                public static readonly PineValue List_9f1e38f9 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_7]);

                public static readonly PineValue List_14a0ba72 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_8]);

                public static readonly PineValue List_a710c27f = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_9]);

                public static readonly PineValue List_c41d9105 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_10]);

                public static readonly PineValue List_87a2a774 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Char_hyphen]);

                public static readonly PineValue List_4c2609c3 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Char_digit_0]);

                public static readonly PineValue List_c3b08663 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Char_digit_1]);

                public static readonly PineValue List_e844984b = PineValue.List([Blob_Str_Literal, List_Single_Blob_Char_digit_2]);

                public static readonly PineValue List_a9196577 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Char_digit_3]);

                public static readonly PineValue List_9728f698 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Char_digit_4]);

                public static readonly PineValue List_86fad7dd = PineValue.List([Blob_Str_Literal, List_Single_Blob_Char_digit_5]);

                public static readonly PineValue List_c005c994 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Char_digit_6]);

                public static readonly PineValue List_33c6f4ad = PineValue.List([Blob_Str_Literal, List_Single_Blob_Char_digit_7]);

                public static readonly PineValue List_1a0b8610 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Char_digit_8]);

                public static readonly PineValue List_ecf5f39d = PineValue.List([Blob_Str_Literal, List_Single_Blob_Char_digit_9]);

                public static readonly PineValue List_0fcdb59d = PineValue.List([Blob_Str_Literal, List_Single_Blob_Str_Ok]);

                public static readonly PineValue List_60c29edb = PineValue.List([Blob_Str_Literal, List_Single_Blob_Str_Err]);

                public static readonly PineValue List_283f6f4a = PineValue.List([Blob_Str_Literal, List_Single_List_c3304aab]);

                public static readonly PineValue List_bd06385a = PineValue.List([Blob_Str_Environment, PineValue.EmptyList]);

                public static readonly PineValue List_4aad0d20 = PineValue.List([Blob_Str_head, List_bd06385a]);

                public static readonly PineValue List_Single_Blob_cfca8ee1 = PineValue.List([Blob_cfca8ee1]);

                public static readonly PineValue List_c1cc00b1 = PineValue.List([List_0dcd86c0, List_bd06385a]);

                public static readonly PineValue List_71a831bc = PineValue.List([List_43b95777, List_bd06385a]);

                public static readonly PineValue List_5f90b1c8 = PineValue.List([List_450c12a0, List_bd06385a]);

                public static readonly PineValue List_Single_List_c1cc00b1 = PineValue.List([List_c1cc00b1]);

                public static readonly PineValue List_Single_List_71a831bc = PineValue.List([List_71a831bc]);

                public static readonly PineValue List_Single_List_5f90b1c8 = PineValue.List([List_5f90b1c8]);

                public static readonly PineValue List_708c3c98 = PineValue.List([Blob_Str_String, List_Single_Blob_cfca8ee1]);

                public static readonly PineValue List_Single_List_708c3c98 = PineValue.List([List_708c3c98]);

                public static readonly PineValue List_4c301747 = PineValue.List([Blob_Str_List, List_Single_List_c1cc00b1]);

                public static readonly PineValue List_7ed5803b = PineValue.List([Blob_Str_List, List_Single_List_71a831bc]);

                public static readonly PineValue List_7b3639f9 = PineValue.List([Blob_Str_List, List_Single_List_5f90b1c8]);

                public static readonly PineValue List_ae45bd54 = PineValue.List([Blob_Str_Err, List_Single_List_708c3c98]);

                public static readonly PineValue List_Single_List_ae45bd54 = PineValue.List([List_ae45bd54]);

                public static readonly PineValue List_b49facc1 = PineValue.List([Blob_Str_skip, List_4c301747]);

                public static readonly PineValue List_0c396738 = PineValue.List([Blob_Str_skip, List_7ed5803b]);

                public static readonly PineValue List_fc75d0e2 = PineValue.List([Blob_Str_skip, List_7b3639f9]);

                public static readonly PineValue List_a5cbaf18 = PineValue.List([Blob_Str_KernelApplication, List_4aad0d20]);

                public static readonly PineValue List_abd0252a = PineValue.List([Blob_Str_Literal, List_Single_List_ae45bd54]);

                public static readonly PineValue List_61fa9b59 = PineValue.List([Blob_Str_head, List_a5cbaf18]);

                public static readonly PineValue List_65405503 = PineValue.List([Blob_Str_KernelApplication, List_b49facc1]);

                public static readonly PineValue List_97963d43 = PineValue.List([Blob_Str_KernelApplication, List_0c396738]);

                public static readonly PineValue List_6a3ea921 = PineValue.List([Blob_Str_KernelApplication, List_fc75d0e2]);

                public static readonly PineValue List_7552bdea = PineValue.List([Blob_Str_head, List_65405503]);

                public static readonly PineValue List_391923b1 = PineValue.List([Blob_Str_head, List_97963d43]);

                public static readonly PineValue List_d0f1d606 = PineValue.List([Blob_Str_head, List_6a3ea921]);

                public static readonly PineValue List_976730e9 = PineValue.List([Blob_Str_KernelApplication, List_61fa9b59]);

                public static readonly PineValue List_f1126ab7 = PineValue.List([Blob_Str_KernelApplication, List_7552bdea]);

                public static readonly PineValue List_3c738443 = PineValue.List([Blob_Str_KernelApplication, List_391923b1]);

                public static readonly PineValue List_3f9010d5 = PineValue.List([Blob_Str_KernelApplication, List_d0f1d606]);

                public static readonly PineValue List_Single_List_f1126ab7 = PineValue.List([List_f1126ab7]);

                public static readonly PineValue List_Single_List_Single_List_f1126ab7 =
                    PineValue.List([List_Single_List_f1126ab7]);

                public static readonly PineValue List_25270f46 =
                    PineValue.List([Blob_Str_List, List_Single_List_Single_List_f1126ab7]);

                public static readonly PineValue List_ed9f3fb4 = PineValue.List([List_f1126ab7, List_c41d9105]);

                public static readonly PineValue List_a9b571b8 = PineValue.List([List_3c738443, List_0c82888c]);

                public static readonly PineValue List_df65fbb0 = PineValue.List([List_3f9010d5, List_0c82888c]);

                public static readonly PineValue List_Single_List_ed9f3fb4 = PineValue.List([List_ed9f3fb4]);

                public static readonly PineValue List_Single_List_a9b571b8 = PineValue.List([List_a9b571b8]);

                public static readonly PineValue List_Single_List_df65fbb0 = PineValue.List([List_df65fbb0]);

                public static readonly PineValue List_6f242950 = PineValue.List([Blob_Str_List, List_Single_List_ed9f3fb4]);

                public static readonly PineValue List_931e9a62 = PineValue.List([Blob_Str_List, List_Single_List_a9b571b8]);

                public static readonly PineValue List_1d76831a = PineValue.List([Blob_Str_List, List_Single_List_df65fbb0]);

                public static readonly PineValue List_aaa9f69e = PineValue.List([List_0fcdb59d, List_25270f46]);

                public static readonly PineValue List_Single_List_aaa9f69e = PineValue.List([List_aaa9f69e]);

                public static readonly PineValue List_fe5d1fd2 = PineValue.List([Blob_Str_List, List_Single_List_aaa9f69e]);

                public static readonly PineValue List_ed34a8b3 = PineValue.List([Blob_Str_int_add, List_931e9a62]);

                public static readonly PineValue List_f5695632 = PineValue.List([Blob_Str_int_add, List_1d76831a]);

                public static readonly PineValue List_bf4cad47 = PineValue.List([Blob_Str_int_mul, List_6f242950]);

                public static readonly PineValue List_3486f049 = PineValue.List([List_abd0252a, List_3c738443]);

                public static readonly PineValue List_Single_List_3486f049 = PineValue.List([List_3486f049]);

                public static readonly PineValue List_7749046a = PineValue.List([Blob_Str_KernelApplication, List_ed34a8b3]);

                public static readonly PineValue List_0fa795f4 = PineValue.List([Blob_Str_KernelApplication, List_f5695632]);

                public static readonly PineValue List_8958d78c = PineValue.List([Blob_Str_KernelApplication, List_bf4cad47]);

                public static readonly PineValue List_84ea9fe4 = PineValue.List([Blob_Str_List, List_Single_List_3486f049]);

                public static readonly PineValue List_1e77ca98 = PineValue.List([List_8958d78c, List_0dcd86c0]);

                public static readonly PineValue List_f88358f2 = PineValue.List([List_8958d78c, List_43b95777]);

                public static readonly PineValue List_fe595fac = PineValue.List([List_8958d78c, List_450c12a0]);

                public static readonly PineValue List_d40b63e8 = PineValue.List([List_8958d78c, List_0c82888c]);

                public static readonly PineValue List_2a83f0ef = PineValue.List([List_8958d78c, List_c1b27e6e]);

                public static readonly PineValue List_5f471647 = PineValue.List([List_8958d78c, List_282dee3a]);

                public static readonly PineValue List_fbd792d3 = PineValue.List([List_8958d78c, List_9f1e38f9]);

                public static readonly PineValue List_695b8820 = PineValue.List([List_8958d78c, List_14a0ba72]);

                public static readonly PineValue List_f4a86d66 = PineValue.List([List_8958d78c, List_a710c27f]);

                public static readonly PineValue List_Single_List_1e77ca98 = PineValue.List([List_1e77ca98]);

                public static readonly PineValue List_Single_List_f88358f2 = PineValue.List([List_f88358f2]);

                public static readonly PineValue List_Single_List_fe595fac = PineValue.List([List_fe595fac]);

                public static readonly PineValue List_Single_List_d40b63e8 = PineValue.List([List_d40b63e8]);

                public static readonly PineValue List_Single_List_2a83f0ef = PineValue.List([List_2a83f0ef]);

                public static readonly PineValue List_Single_List_5f471647 = PineValue.List([List_5f471647]);

                public static readonly PineValue List_Single_List_fbd792d3 = PineValue.List([List_fbd792d3]);

                public static readonly PineValue List_Single_List_695b8820 = PineValue.List([List_695b8820]);

                public static readonly PineValue List_Single_List_f4a86d66 = PineValue.List([List_f4a86d66]);

                public static readonly PineValue List_cf02ff4b = PineValue.List([List_283f6f4a, List_7749046a]);

                public static readonly PineValue List_Single_List_cf02ff4b = PineValue.List([List_cf02ff4b]);

                public static readonly PineValue List_45f120c8 = PineValue.List([Blob_Str_List, List_Single_List_1e77ca98]);

                public static readonly PineValue List_91d973df = PineValue.List([Blob_Str_List, List_Single_List_f88358f2]);

                public static readonly PineValue List_0ecbf87e = PineValue.List([Blob_Str_List, List_Single_List_fe595fac]);

                public static readonly PineValue List_e8637ebc = PineValue.List([Blob_Str_List, List_Single_List_d40b63e8]);

                public static readonly PineValue List_acd4c10c = PineValue.List([Blob_Str_List, List_Single_List_2a83f0ef]);

                public static readonly PineValue List_97e2e750 = PineValue.List([Blob_Str_List, List_Single_List_5f471647]);

                public static readonly PineValue List_028efd37 = PineValue.List([Blob_Str_List, List_Single_List_fbd792d3]);

                public static readonly PineValue List_af348c5d = PineValue.List([Blob_Str_List, List_Single_List_695b8820]);

                public static readonly PineValue List_99e8f3dd = PineValue.List([Blob_Str_List, List_Single_List_f4a86d66]);

                public static readonly PineValue List_86917490 = PineValue.List([Blob_Str_List, List_Single_List_cf02ff4b]);

                public static readonly PineValue List_52675188 = PineValue.List([Blob_Str_int_add, List_45f120c8]);

                public static readonly PineValue List_853a9535 = PineValue.List([Blob_Str_int_add, List_91d973df]);

                public static readonly PineValue List_31774a0a = PineValue.List([Blob_Str_int_add, List_0ecbf87e]);

                public static readonly PineValue List_f11e25ab = PineValue.List([Blob_Str_int_add, List_e8637ebc]);

                public static readonly PineValue List_c33d220a = PineValue.List([Blob_Str_int_add, List_acd4c10c]);

                public static readonly PineValue List_ce1d9f3a = PineValue.List([Blob_Str_int_add, List_97e2e750]);

                public static readonly PineValue List_b4230411 = PineValue.List([Blob_Str_int_add, List_028efd37]);

                public static readonly PineValue List_e5b191b7 = PineValue.List([Blob_Str_int_add, List_af348c5d]);

                public static readonly PineValue List_219202ec = PineValue.List([Blob_Str_int_add, List_99e8f3dd]);

                public static readonly PineValue List_2886d4ff = PineValue.List([List_3c738443, List_f1126ab7]);

                public static readonly PineValue List_bb45a699 = PineValue.List([List_3f9010d5, List_3c738443]);

                public static readonly PineValue List_Single_List_2886d4ff = PineValue.List([List_2886d4ff]);

                public static readonly PineValue List_Single_List_bb45a699 = PineValue.List([List_bb45a699]);

                public static readonly PineValue List_8315cc98 = PineValue.List([Blob_Str_List, List_Single_List_2886d4ff]);

                public static readonly PineValue List_8c38828c = PineValue.List([Blob_Str_List, List_Single_List_bb45a699]);

                public static readonly PineValue List_fee07eeb = PineValue.List([Blob_Str_KernelApplication, List_52675188]);

                public static readonly PineValue List_08be1380 = PineValue.List([Blob_Str_KernelApplication, List_853a9535]);

                public static readonly PineValue List_fc37234d = PineValue.List([Blob_Str_KernelApplication, List_31774a0a]);

                public static readonly PineValue List_c51d0fb5 = PineValue.List([Blob_Str_KernelApplication, List_f11e25ab]);

                public static readonly PineValue List_c205849e = PineValue.List([Blob_Str_KernelApplication, List_c33d220a]);

                public static readonly PineValue List_8b566b65 = PineValue.List([Blob_Str_KernelApplication, List_ce1d9f3a]);

                public static readonly PineValue List_b53936c0 = PineValue.List([Blob_Str_KernelApplication, List_b4230411]);

                public static readonly PineValue List_1ffb1d35 = PineValue.List([Blob_Str_KernelApplication, List_e5b191b7]);

                public static readonly PineValue List_069c236e = PineValue.List([Blob_Str_KernelApplication, List_219202ec]);

                public static readonly PineValue List_74eb383b = PineValue.List([List_e190d1f5, List_f1126ab7, List_3c738443]);

                public static readonly PineValue List_Single_List_74eb383b = PineValue.List([List_74eb383b]);

                public static readonly PineValue List_dd0e3578 = PineValue.List([Blob_Str_skip, List_8315cc98]);

                public static readonly PineValue List_966321a0 = PineValue.List([Blob_Str_skip, List_8c38828c]);

                public static readonly PineValue List_f359b046 = PineValue.List([Blob_Str_List, List_Single_List_74eb383b]);

                public static readonly PineValue List_1eed7965 = PineValue.List([List_fe5d1fd2, List_3f9010d5]);

                public static readonly PineValue List_Single_List_1eed7965 = PineValue.List([List_1eed7965]);

                public static readonly PineValue List_76515acb = PineValue.List([Blob_Str_List, List_Single_List_1eed7965]);

                public static readonly PineValue List_0396ffc2 = PineValue.List([Blob_Str_KernelApplication, List_dd0e3578]);

                public static readonly PineValue List_cc61da74 = PineValue.List([Blob_Str_KernelApplication, List_966321a0]);

                public static readonly PineValue List_920e5328 = PineValue.List([List_0c82888c, List_0396ffc2]);

                public static readonly PineValue List_16063b04 = PineValue.List([List_0c82888c, List_cc61da74]);

                public static readonly PineValue List_Single_List_920e5328 = PineValue.List([List_920e5328]);

                public static readonly PineValue List_Single_List_16063b04 = PineValue.List([List_16063b04]);

                public static readonly PineValue List_be347120 = PineValue.List([Blob_Str_List, List_Single_List_920e5328]);

                public static readonly PineValue List_3b8be6eb = PineValue.List([Blob_Str_List, List_Single_List_16063b04]);

                public static readonly PineValue List_e0a5d877 = PineValue.List([Blob_Str_take, List_be347120]);

                public static readonly PineValue List_0886f217 = PineValue.List([Blob_Str_take, List_3b8be6eb]);

                public static readonly PineValue List_0c55e760 = PineValue.List([List_e190d1f5, List_f1126ab7, List_7749046a]);

                public static readonly PineValue List_Single_List_0c55e760 = PineValue.List([List_0c55e760]);

                public static readonly PineValue List_6eaa25e4 = PineValue.List([Blob_Str_List, List_Single_List_0c55e760]);

                public static readonly PineValue List_e92f07d9 = PineValue.List([Blob_Str_KernelApplication, List_e0a5d877]);

                public static readonly PineValue List_8aac5910 = PineValue.List([Blob_Str_KernelApplication, List_0886f217]);

                public static readonly PineValue List_e511282c = PineValue.List([List_e92f07d9, List_87a2a774]);

                public static readonly PineValue List_6814399a = PineValue.List([List_e92f07d9, List_4c2609c3]);

                public static readonly PineValue List_25567dd1 = PineValue.List([List_e92f07d9, List_c3b08663]);

                public static readonly PineValue List_f60fb02c = PineValue.List([List_e92f07d9, List_e844984b]);

                public static readonly PineValue List_15cd1d36 = PineValue.List([List_e92f07d9, List_a9196577]);

                public static readonly PineValue List_dd6ae59f = PineValue.List([List_e92f07d9, List_9728f698]);

                public static readonly PineValue List_d443571b = PineValue.List([List_e92f07d9, List_86fad7dd]);

                public static readonly PineValue List_20782d6b = PineValue.List([List_e92f07d9, List_c005c994]);

                public static readonly PineValue List_a762d148 = PineValue.List([List_e92f07d9, List_33c6f4ad]);

                public static readonly PineValue List_31c7134c = PineValue.List([List_e92f07d9, List_1a0b8610]);

                public static readonly PineValue List_2a635c1a = PineValue.List([List_e92f07d9, List_ecf5f39d]);

                public static readonly PineValue List_409af409 = PineValue.List([List_8aac5910, List_4c2609c3]);

                public static readonly PineValue List_615104eb = PineValue.List([List_8aac5910, List_c3b08663]);

                public static readonly PineValue List_82fb4579 = PineValue.List([List_8aac5910, List_e844984b]);

                public static readonly PineValue List_48f24c4f = PineValue.List([List_8aac5910, List_a9196577]);

                public static readonly PineValue List_3bf83004 = PineValue.List([List_8aac5910, List_9728f698]);

                public static readonly PineValue List_c135e062 = PineValue.List([List_8aac5910, List_86fad7dd]);

                public static readonly PineValue List_73fe9a0e = PineValue.List([List_8aac5910, List_c005c994]);

                public static readonly PineValue List_b21276b4 = PineValue.List([List_8aac5910, List_33c6f4ad]);

                public static readonly PineValue List_9154ea2d = PineValue.List([List_8aac5910, List_1a0b8610]);

                public static readonly PineValue List_628057c5 = PineValue.List([List_8aac5910, List_ecf5f39d]);

                public static readonly PineValue List_Single_List_e511282c = PineValue.List([List_e511282c]);

                public static readonly PineValue List_Single_List_6814399a = PineValue.List([List_6814399a]);

                public static readonly PineValue List_Single_List_25567dd1 = PineValue.List([List_25567dd1]);

                public static readonly PineValue List_Single_List_f60fb02c = PineValue.List([List_f60fb02c]);

                public static readonly PineValue List_Single_List_15cd1d36 = PineValue.List([List_15cd1d36]);

                public static readonly PineValue List_Single_List_dd6ae59f = PineValue.List([List_dd6ae59f]);

                public static readonly PineValue List_Single_List_d443571b = PineValue.List([List_d443571b]);

                public static readonly PineValue List_Single_List_20782d6b = PineValue.List([List_20782d6b]);

                public static readonly PineValue List_Single_List_a762d148 = PineValue.List([List_a762d148]);

                public static readonly PineValue List_Single_List_31c7134c = PineValue.List([List_31c7134c]);

                public static readonly PineValue List_Single_List_2a635c1a = PineValue.List([List_2a635c1a]);

                public static readonly PineValue List_Single_List_409af409 = PineValue.List([List_409af409]);

                public static readonly PineValue List_Single_List_615104eb = PineValue.List([List_615104eb]);

                public static readonly PineValue List_Single_List_82fb4579 = PineValue.List([List_82fb4579]);

                public static readonly PineValue List_Single_List_48f24c4f = PineValue.List([List_48f24c4f]);

                public static readonly PineValue List_Single_List_3bf83004 = PineValue.List([List_3bf83004]);

                public static readonly PineValue List_Single_List_c135e062 = PineValue.List([List_c135e062]);

                public static readonly PineValue List_Single_List_73fe9a0e = PineValue.List([List_73fe9a0e]);

                public static readonly PineValue List_Single_List_b21276b4 = PineValue.List([List_b21276b4]);

                public static readonly PineValue List_Single_List_9154ea2d = PineValue.List([List_9154ea2d]);

                public static readonly PineValue List_Single_List_628057c5 = PineValue.List([List_628057c5]);

                public static readonly PineValue List_b62c2cdf = PineValue.List([Blob_Str_List, List_Single_List_e511282c]);

                public static readonly PineValue List_b47d6acf = PineValue.List([Blob_Str_List, List_Single_List_6814399a]);

                public static readonly PineValue List_e7226e3c = PineValue.List([Blob_Str_List, List_Single_List_25567dd1]);

                public static readonly PineValue List_ff79cb2b = PineValue.List([Blob_Str_List, List_Single_List_f60fb02c]);

                public static readonly PineValue List_aba226f4 = PineValue.List([Blob_Str_List, List_Single_List_15cd1d36]);

                public static readonly PineValue List_44815811 = PineValue.List([Blob_Str_List, List_Single_List_dd6ae59f]);

                public static readonly PineValue List_094f4a3c = PineValue.List([Blob_Str_List, List_Single_List_d443571b]);

                public static readonly PineValue List_116e6b97 = PineValue.List([Blob_Str_List, List_Single_List_20782d6b]);

                public static readonly PineValue List_c36611cf = PineValue.List([Blob_Str_List, List_Single_List_a762d148]);

                public static readonly PineValue List_caedd9a5 = PineValue.List([Blob_Str_List, List_Single_List_31c7134c]);

                public static readonly PineValue List_541a6b2e = PineValue.List([Blob_Str_List, List_Single_List_2a635c1a]);

                public static readonly PineValue List_bdf23604 = PineValue.List([Blob_Str_List, List_Single_List_409af409]);

                public static readonly PineValue List_be84ba66 = PineValue.List([Blob_Str_List, List_Single_List_615104eb]);

                public static readonly PineValue List_69ba12f0 = PineValue.List([Blob_Str_List, List_Single_List_82fb4579]);

                public static readonly PineValue List_e52ee74a = PineValue.List([Blob_Str_List, List_Single_List_48f24c4f]);

                public static readonly PineValue List_403d8fb5 = PineValue.List([Blob_Str_List, List_Single_List_3bf83004]);

                public static readonly PineValue List_3e6819a3 = PineValue.List([Blob_Str_List, List_Single_List_c135e062]);

                public static readonly PineValue List_1aeb1da2 = PineValue.List([Blob_Str_List, List_Single_List_73fe9a0e]);

                public static readonly PineValue List_76a46b5b = PineValue.List([Blob_Str_List, List_Single_List_b21276b4]);

                public static readonly PineValue List_d7084e38 = PineValue.List([Blob_Str_List, List_Single_List_9154ea2d]);

                public static readonly PineValue List_ef150a39 = PineValue.List([Blob_Str_List, List_Single_List_628057c5]);

                public static readonly PineValue List_e8e068b3 = PineValue.List([Blob_Str_equal, List_b62c2cdf]);

                public static readonly PineValue List_ae8075fb = PineValue.List([Blob_Str_equal, List_b47d6acf]);

                public static readonly PineValue List_a4e6d363 = PineValue.List([Blob_Str_equal, List_e7226e3c]);

                public static readonly PineValue List_ada60273 = PineValue.List([Blob_Str_equal, List_ff79cb2b]);

                public static readonly PineValue List_44323a9d = PineValue.List([Blob_Str_equal, List_aba226f4]);

                public static readonly PineValue List_c7457794 = PineValue.List([Blob_Str_equal, List_44815811]);

                public static readonly PineValue List_eb923b38 = PineValue.List([Blob_Str_equal, List_094f4a3c]);

                public static readonly PineValue List_54262f58 = PineValue.List([Blob_Str_equal, List_116e6b97]);

                public static readonly PineValue List_7413b0f4 = PineValue.List([Blob_Str_equal, List_c36611cf]);

                public static readonly PineValue List_e6507c1a = PineValue.List([Blob_Str_equal, List_caedd9a5]);

                public static readonly PineValue List_271ce0cf = PineValue.List([Blob_Str_equal, List_541a6b2e]);

                public static readonly PineValue List_94e18716 = PineValue.List([Blob_Str_equal, List_bdf23604]);

                public static readonly PineValue List_eef14e25 = PineValue.List([Blob_Str_equal, List_be84ba66]);

                public static readonly PineValue List_ead9b813 = PineValue.List([Blob_Str_equal, List_69ba12f0]);

                public static readonly PineValue List_ce72bb5d = PineValue.List([Blob_Str_equal, List_e52ee74a]);

                public static readonly PineValue List_f7a23651 = PineValue.List([Blob_Str_equal, List_403d8fb5]);

                public static readonly PineValue List_caaf8f80 = PineValue.List([Blob_Str_equal, List_3e6819a3]);

                public static readonly PineValue List_2a0ff981 = PineValue.List([Blob_Str_equal, List_1aeb1da2]);

                public static readonly PineValue List_710c4b62 = PineValue.List([Blob_Str_equal, List_76a46b5b]);

                public static readonly PineValue List_5def5c7b = PineValue.List([Blob_Str_equal, List_d7084e38]);

                public static readonly PineValue List_2b95efb9 = PineValue.List([Blob_Str_equal, List_ef150a39]);

                public static readonly PineValue List_a96fdd25 = PineValue.List([Blob_Str_KernelApplication, List_e8e068b3]);

                public static readonly PineValue List_d77a99bc = PineValue.List([Blob_Str_KernelApplication, List_ae8075fb]);

                public static readonly PineValue List_9bd39670 = PineValue.List([Blob_Str_KernelApplication, List_a4e6d363]);

                public static readonly PineValue List_5fd2af1e = PineValue.List([Blob_Str_KernelApplication, List_ada60273]);

                public static readonly PineValue List_ee5057ef = PineValue.List([Blob_Str_KernelApplication, List_44323a9d]);

                public static readonly PineValue List_f14a9fc1 = PineValue.List([Blob_Str_KernelApplication, List_c7457794]);

                public static readonly PineValue List_4ea4085c = PineValue.List([Blob_Str_KernelApplication, List_eb923b38]);

                public static readonly PineValue List_3364c17c = PineValue.List([Blob_Str_KernelApplication, List_54262f58]);

                public static readonly PineValue List_166f484c = PineValue.List([Blob_Str_KernelApplication, List_7413b0f4]);

                public static readonly PineValue List_a67bdb04 = PineValue.List([Blob_Str_KernelApplication, List_e6507c1a]);

                public static readonly PineValue List_46757927 = PineValue.List([Blob_Str_KernelApplication, List_271ce0cf]);

                public static readonly PineValue List_e6206346 = PineValue.List([Blob_Str_KernelApplication, List_94e18716]);

                public static readonly PineValue List_da320320 = PineValue.List([Blob_Str_KernelApplication, List_eef14e25]);

                public static readonly PineValue List_303d5456 = PineValue.List([Blob_Str_KernelApplication, List_ead9b813]);

                public static readonly PineValue List_f19981ca = PineValue.List([Blob_Str_KernelApplication, List_ce72bb5d]);

                public static readonly PineValue List_7ecebbc2 = PineValue.List([Blob_Str_KernelApplication, List_f7a23651]);

                public static readonly PineValue List_df756adf = PineValue.List([Blob_Str_KernelApplication, List_caaf8f80]);

                public static readonly PineValue List_c4f71420 = PineValue.List([Blob_Str_KernelApplication, List_2a0ff981]);

                public static readonly PineValue List_eda26b6b = PineValue.List([Blob_Str_KernelApplication, List_710c4b62]);

                public static readonly PineValue List_6680b0ad = PineValue.List([Blob_Str_KernelApplication, List_5def5c7b]);

                public static readonly PineValue List_67632ff4 = PineValue.List([Blob_Str_KernelApplication, List_2b95efb9]);

                public static readonly PineValue List_ed425ea0 =
                    PineValue.List([List_a5cbaf18, List_8958d78c, List_3c738443, List_0fa795f4]);

                public static readonly PineValue List_Single_List_ed425ea0 = PineValue.List([List_ed425ea0]);

                public static readonly PineValue List_143487b1 = PineValue.List([Blob_Str_List, List_Single_List_ed425ea0]);

                public static readonly PineValue List_6929834e =
                    PineValue.List([List_a5cbaf18, List_fee07eeb, List_3c738443, List_0fa795f4]);

                public static readonly PineValue List_7100392a =
                    PineValue.List([List_a5cbaf18, List_08be1380, List_3c738443, List_0fa795f4]);

                public static readonly PineValue List_25c5878b =
                    PineValue.List([List_a5cbaf18, List_fc37234d, List_3c738443, List_0fa795f4]);

                public static readonly PineValue List_11ace401 =
                    PineValue.List([List_a5cbaf18, List_c51d0fb5, List_3c738443, List_0fa795f4]);

                public static readonly PineValue List_26713f5e =
                    PineValue.List([List_a5cbaf18, List_c205849e, List_3c738443, List_0fa795f4]);

                public static readonly PineValue List_343c31bd =
                    PineValue.List([List_a5cbaf18, List_8b566b65, List_3c738443, List_0fa795f4]);

                public static readonly PineValue List_9c54a6fa =
                    PineValue.List([List_a5cbaf18, List_b53936c0, List_3c738443, List_0fa795f4]);

                public static readonly PineValue List_70787f90 =
                    PineValue.List([List_a5cbaf18, List_1ffb1d35, List_3c738443, List_0fa795f4]);

                public static readonly PineValue List_78554bbc =
                    PineValue.List([List_a5cbaf18, List_069c236e, List_3c738443, List_0fa795f4]);

                public static readonly PineValue List_Single_List_6929834e = PineValue.List([List_6929834e]);

                public static readonly PineValue List_Single_List_7100392a = PineValue.List([List_7100392a]);

                public static readonly PineValue List_Single_List_25c5878b = PineValue.List([List_25c5878b]);

                public static readonly PineValue List_Single_List_11ace401 = PineValue.List([List_11ace401]);

                public static readonly PineValue List_Single_List_26713f5e = PineValue.List([List_26713f5e]);

                public static readonly PineValue List_Single_List_343c31bd = PineValue.List([List_343c31bd]);

                public static readonly PineValue List_Single_List_9c54a6fa = PineValue.List([List_9c54a6fa]);

                public static readonly PineValue List_Single_List_70787f90 = PineValue.List([List_70787f90]);

                public static readonly PineValue List_Single_List_78554bbc = PineValue.List([List_78554bbc]);

                public static readonly PineValue List_1be8a251 = PineValue.List([Blob_Str_List, List_Single_List_6929834e]);

                public static readonly PineValue List_3bceda82 = PineValue.List([Blob_Str_List, List_Single_List_7100392a]);

                public static readonly PineValue List_021a476c = PineValue.List([Blob_Str_List, List_Single_List_25c5878b]);

                public static readonly PineValue List_f843eb63 = PineValue.List([Blob_Str_List, List_Single_List_11ace401]);

                public static readonly PineValue List_4147fd11 = PineValue.List([Blob_Str_List, List_Single_List_26713f5e]);

                public static readonly PineValue List_a5a99e88 = PineValue.List([Blob_Str_List, List_Single_List_343c31bd]);

                public static readonly PineValue List_167bffb9 = PineValue.List([Blob_Str_List, List_Single_List_9c54a6fa]);

                public static readonly PineValue List_f8cc95c0 = PineValue.List([Blob_Str_List, List_Single_List_70787f90]);

                public static readonly PineValue List_0ead61c8 = PineValue.List([Blob_Str_List, List_Single_List_78554bbc]);

                public static readonly PineValue List_f40c6908 = PineValue.List([List_976730e9, List_143487b1]);

                public static readonly PineValue List_3587bd65 = PineValue.List([Blob_Str_ParseAndEval, List_f40c6908]);

                public static readonly PineValue List_2a141992 = PineValue.List([List_976730e9, List_1be8a251]);

                public static readonly PineValue List_c7ffb877 = PineValue.List([List_976730e9, List_3bceda82]);

                public static readonly PineValue List_8331719d = PineValue.List([List_976730e9, List_021a476c]);

                public static readonly PineValue List_3d487a93 = PineValue.List([List_976730e9, List_f843eb63]);

                public static readonly PineValue List_a1c1399d = PineValue.List([List_976730e9, List_4147fd11]);

                public static readonly PineValue List_2dec7c56 = PineValue.List([List_976730e9, List_a5a99e88]);

                public static readonly PineValue List_a29f57bb = PineValue.List([List_976730e9, List_167bffb9]);

                public static readonly PineValue List_8cb66c92 = PineValue.List([List_976730e9, List_f8cc95c0]);

                public static readonly PineValue List_1be165a6 = PineValue.List([List_976730e9, List_0ead61c8]);

                public static readonly PineValue List_fd3edde6 = PineValue.List([Blob_Str_ParseAndEval, List_2a141992]);

                public static readonly PineValue List_8aa71e4b = PineValue.List([Blob_Str_ParseAndEval, List_c7ffb877]);

                public static readonly PineValue List_a96a93bb = PineValue.List([Blob_Str_ParseAndEval, List_8331719d]);

                public static readonly PineValue List_e39487f6 = PineValue.List([Blob_Str_ParseAndEval, List_3d487a93]);

                public static readonly PineValue List_487763b8 = PineValue.List([Blob_Str_ParseAndEval, List_a1c1399d]);

                public static readonly PineValue List_46edd205 = PineValue.List([Blob_Str_ParseAndEval, List_2dec7c56]);

                public static readonly PineValue List_53a0b5da = PineValue.List([Blob_Str_ParseAndEval, List_a29f57bb]);

                public static readonly PineValue List_f2b1f829 = PineValue.List([Blob_Str_ParseAndEval, List_8cb66c92]);

                public static readonly PineValue List_82d11184 = PineValue.List([Blob_Str_ParseAndEval, List_1be165a6]);

                public static readonly PineValue List_4d475a70 = PineValue.List([List_67632ff4, List_76515acb, List_82d11184]);

                public static readonly PineValue List_8d4ccb0e = PineValue.List([Blob_Str_Conditional, List_4d475a70]);

                public static readonly PineValue List_232beeef = PineValue.List([List_6680b0ad, List_8d4ccb0e, List_f2b1f829]);

                public static readonly PineValue List_1cc2a79f = PineValue.List([Blob_Str_Conditional, List_232beeef]);

                public static readonly PineValue List_187a6c4b = PineValue.List([List_eda26b6b, List_1cc2a79f, List_53a0b5da]);

                public static readonly PineValue List_162d120f = PineValue.List([Blob_Str_Conditional, List_187a6c4b]);

                public static readonly PineValue List_958d9dbe = PineValue.List([List_c4f71420, List_162d120f, List_46edd205]);

                public static readonly PineValue List_11eb0322 = PineValue.List([Blob_Str_Conditional, List_958d9dbe]);

                public static readonly PineValue List_6ed7d793 = PineValue.List([List_df756adf, List_11eb0322, List_487763b8]);

                public static readonly PineValue List_1086fa7f = PineValue.List([Blob_Str_Conditional, List_6ed7d793]);

                public static readonly PineValue List_c867f777 = PineValue.List([List_7ecebbc2, List_1086fa7f, List_e39487f6]);

                public static readonly PineValue List_febac2a8 = PineValue.List([Blob_Str_Conditional, List_c867f777]);

                public static readonly PineValue List_6150a7a2 = PineValue.List([List_f19981ca, List_febac2a8, List_a96a93bb]);

                public static readonly PineValue List_d9b5ae07 = PineValue.List([Blob_Str_Conditional, List_6150a7a2]);

                public static readonly PineValue List_0795045b = PineValue.List([List_303d5456, List_d9b5ae07, List_8aa71e4b]);

                public static readonly PineValue List_09ca44d3 = PineValue.List([Blob_Str_Conditional, List_0795045b]);

                public static readonly PineValue List_57d3b6be = PineValue.List([List_da320320, List_09ca44d3, List_fd3edde6]);

                public static readonly PineValue List_09dccd4d = PineValue.List([Blob_Str_Conditional, List_57d3b6be]);

                public static readonly PineValue List_b2c7f4a2 = PineValue.List([List_e6206346, List_09dccd4d, List_3587bd65]);

                public static readonly PineValue List_6ff31d68 = PineValue.List([Blob_Str_Conditional, List_b2c7f4a2]);

                public static readonly PineValue List_Single_List_6ff31d68 = PineValue.List([List_6ff31d68]);

                public static readonly PineValue List_Single_List_Single_List_6ff31d68 =
                    PineValue.List([List_Single_List_6ff31d68]);

                public static readonly PineValue List_bdac1446 = PineValue.List([Blob_Str_Literal, List_Single_List_6ff31d68]);

                public static readonly PineValue List_66efe92b =
                    PineValue.List([Blob_Str_Literal, List_Single_List_Single_List_6ff31d68]);

                public static readonly PineValue List_89cfca1b =
                    PineValue.List([List_66efe92b, List_0dcd86c0, List_f1126ab7, List_7749046a]);

                public static readonly PineValue List_89946b4f =
                    PineValue.List([List_66efe92b, List_43b95777, List_f1126ab7, List_7749046a]);

                public static readonly PineValue List_ab50c157 =
                    PineValue.List([List_66efe92b, List_450c12a0, List_f1126ab7, List_7749046a]);

                public static readonly PineValue List_d428d13e =
                    PineValue.List([List_66efe92b, List_0c82888c, List_f1126ab7, List_7749046a]);

                public static readonly PineValue List_808ecfbc =
                    PineValue.List([List_66efe92b, List_c1b27e6e, List_f1126ab7, List_7749046a]);

                public static readonly PineValue List_13b113b0 =
                    PineValue.List([List_66efe92b, List_282dee3a, List_f1126ab7, List_7749046a]);

                public static readonly PineValue List_0ddaec51 =
                    PineValue.List([List_66efe92b, List_9f1e38f9, List_f1126ab7, List_7749046a]);

                public static readonly PineValue List_0bdacc79 =
                    PineValue.List([List_66efe92b, List_14a0ba72, List_f1126ab7, List_7749046a]);

                public static readonly PineValue List_8ece916f =
                    PineValue.List([List_66efe92b, List_a710c27f, List_f1126ab7, List_7749046a]);

                public static readonly PineValue List_Single_List_89cfca1b = PineValue.List([List_89cfca1b]);

                public static readonly PineValue List_Single_List_89946b4f = PineValue.List([List_89946b4f]);

                public static readonly PineValue List_Single_List_ab50c157 = PineValue.List([List_ab50c157]);

                public static readonly PineValue List_Single_List_d428d13e = PineValue.List([List_d428d13e]);

                public static readonly PineValue List_Single_List_808ecfbc = PineValue.List([List_808ecfbc]);

                public static readonly PineValue List_Single_List_13b113b0 = PineValue.List([List_13b113b0]);

                public static readonly PineValue List_Single_List_0ddaec51 = PineValue.List([List_0ddaec51]);

                public static readonly PineValue List_Single_List_0bdacc79 = PineValue.List([List_0bdacc79]);

                public static readonly PineValue List_Single_List_8ece916f = PineValue.List([List_8ece916f]);

                public static readonly PineValue List_ae3292de = PineValue.List([Blob_Str_List, List_Single_List_89cfca1b]);

                public static readonly PineValue List_e952984b = PineValue.List([Blob_Str_List, List_Single_List_89946b4f]);

                public static readonly PineValue List_b0851e49 = PineValue.List([Blob_Str_List, List_Single_List_ab50c157]);

                public static readonly PineValue List_2f548397 = PineValue.List([Blob_Str_List, List_Single_List_d428d13e]);

                public static readonly PineValue List_5b729ec4 = PineValue.List([Blob_Str_List, List_Single_List_808ecfbc]);

                public static readonly PineValue List_c37f2a30 = PineValue.List([Blob_Str_List, List_Single_List_13b113b0]);

                public static readonly PineValue List_6782fa4e = PineValue.List([Blob_Str_List, List_Single_List_0ddaec51]);

                public static readonly PineValue List_88d2ba85 = PineValue.List([Blob_Str_List, List_Single_List_0bdacc79]);

                public static readonly PineValue List_9eb44e75 = PineValue.List([Blob_Str_List, List_Single_List_8ece916f]);

                public static readonly PineValue List_9de99d7b = PineValue.List([List_bdac1446, List_ae3292de]);

                public static readonly PineValue List_518a6f44 = PineValue.List([List_bdac1446, List_e952984b]);

                public static readonly PineValue List_8fd692e6 = PineValue.List([List_bdac1446, List_b0851e49]);

                public static readonly PineValue List_63343da7 = PineValue.List([List_bdac1446, List_2f548397]);

                public static readonly PineValue List_b4f3e6c8 = PineValue.List([List_bdac1446, List_5b729ec4]);

                public static readonly PineValue List_fc67f194 = PineValue.List([List_bdac1446, List_c37f2a30]);

                public static readonly PineValue List_6ef2784f = PineValue.List([List_bdac1446, List_6782fa4e]);

                public static readonly PineValue List_a45413ac = PineValue.List([List_bdac1446, List_88d2ba85]);

                public static readonly PineValue List_3361ff54 = PineValue.List([List_bdac1446, List_9eb44e75]);

                public static readonly PineValue List_286744e6 = PineValue.List([Blob_Str_ParseAndEval, List_9de99d7b]);

                public static readonly PineValue List_dc2b7889 = PineValue.List([Blob_Str_ParseAndEval, List_518a6f44]);

                public static readonly PineValue List_3416adcb = PineValue.List([Blob_Str_ParseAndEval, List_8fd692e6]);

                public static readonly PineValue List_a80e94c1 = PineValue.List([Blob_Str_ParseAndEval, List_63343da7]);

                public static readonly PineValue List_6c5a512c = PineValue.List([Blob_Str_ParseAndEval, List_b4f3e6c8]);

                public static readonly PineValue List_b7762237 = PineValue.List([Blob_Str_ParseAndEval, List_fc67f194]);

                public static readonly PineValue List_63b2cead = PineValue.List([Blob_Str_ParseAndEval, List_6ef2784f]);

                public static readonly PineValue List_7bec14ce = PineValue.List([Blob_Str_ParseAndEval, List_a45413ac]);

                public static readonly PineValue List_ced83e2f = PineValue.List([Blob_Str_ParseAndEval, List_3361ff54]);

                public static readonly PineValue List_bf450ebd = PineValue.List([List_46757927, List_84ea9fe4, List_ced83e2f]);

                public static readonly PineValue List_400585ec = PineValue.List([Blob_Str_Conditional, List_bf450ebd]);

                public static readonly PineValue List_75d81e18 = PineValue.List([List_a67bdb04, List_400585ec, List_7bec14ce]);

                public static readonly PineValue List_07d12d97 = PineValue.List([Blob_Str_Conditional, List_75d81e18]);

                public static readonly PineValue List_0ff2659d = PineValue.List([List_166f484c, List_07d12d97, List_63b2cead]);

                public static readonly PineValue List_1716c564 = PineValue.List([Blob_Str_Conditional, List_0ff2659d]);

                public static readonly PineValue List_db4439d4 = PineValue.List([List_3364c17c, List_1716c564, List_b7762237]);

                public static readonly PineValue List_f3aca1e4 = PineValue.List([Blob_Str_Conditional, List_db4439d4]);

                public static readonly PineValue List_6a1187c0 = PineValue.List([List_4ea4085c, List_f3aca1e4, List_6c5a512c]);

                public static readonly PineValue List_04d3dd35 = PineValue.List([Blob_Str_Conditional, List_6a1187c0]);

                public static readonly PineValue List_0c9da4b1 = PineValue.List([List_f14a9fc1, List_04d3dd35, List_a80e94c1]);

                public static readonly PineValue List_4c63b38d = PineValue.List([Blob_Str_Conditional, List_0c9da4b1]);

                public static readonly PineValue List_564157c0 = PineValue.List([List_ee5057ef, List_4c63b38d, List_3416adcb]);

                public static readonly PineValue List_7770882b = PineValue.List([Blob_Str_Conditional, List_564157c0]);

                public static readonly PineValue List_0340495c = PineValue.List([List_5fd2af1e, List_7770882b, List_dc2b7889]);

                public static readonly PineValue List_fe63183e = PineValue.List([Blob_Str_Conditional, List_0340495c]);

                public static readonly PineValue List_edaef4f3 = PineValue.List([List_9bd39670, List_fe63183e, List_286744e6]);

                public static readonly PineValue List_fbc3c724 = PineValue.List([Blob_Str_Conditional, List_edaef4f3]);

                public static readonly PineValue List_4cb29c54 = PineValue.List([List_d77a99bc, List_fbc3c724, List_86917490]);

                public static readonly PineValue List_92e98db7 = PineValue.List([Blob_Str_Conditional, List_4cb29c54]);

                public static readonly PineValue List_Single_List_92e98db7 = PineValue.List([List_92e98db7]);

                public static readonly PineValue List_d8c64152 = PineValue.List([Blob_Str_Literal, List_Single_List_92e98db7]);

                public static readonly PineValue List_14c300c2 = PineValue.List([List_d8c64152, List_f359b046]);

                public static readonly PineValue List_7c594931 = PineValue.List([Blob_Str_ParseAndEval, List_14c300c2]);

                public static readonly PineValue List_933cb317 = PineValue.List([List_d8c64152, List_6eaa25e4]);

                public static readonly PineValue List_8491441d = PineValue.List([Blob_Str_ParseAndEval, List_933cb317]);

                public static readonly PineValue List_3536ff19 = PineValue.List([Blob_Str_head, List_8491441d]);

                public static readonly PineValue List_7d37fd38 = PineValue.List([List_0dcd86c0, List_8491441d]);

                public static readonly PineValue List_Single_List_7d37fd38 = PineValue.List([List_7d37fd38]);

                public static readonly PineValue List_aa89aa7d = PineValue.List([Blob_Str_List, List_Single_List_7d37fd38]);

                public static readonly PineValue List_5ca14b6f = PineValue.List([Blob_Str_skip, List_aa89aa7d]);

                public static readonly PineValue List_d61ab592 = PineValue.List([Blob_Str_KernelApplication, List_3536ff19]);

                public static readonly PineValue List_3d45c81f = PineValue.List([Blob_Str_head, List_d61ab592]);

                public static readonly PineValue List_df54fc7c = PineValue.List([List_0dcd86c0, List_d61ab592]);

                public static readonly PineValue List_Single_List_df54fc7c = PineValue.List([List_df54fc7c]);

                public static readonly PineValue List_1ad25605 = PineValue.List([Blob_Str_List, List_Single_List_df54fc7c]);

                public static readonly PineValue List_baa65f35 = PineValue.List([Blob_Str_KernelApplication, List_5ca14b6f]);

                public static readonly PineValue List_0b98fd18 = PineValue.List([Blob_Str_head, List_baa65f35]);

                public static readonly PineValue List_12a43a99 = PineValue.List([Blob_Str_skip, List_1ad25605]);

                public static readonly PineValue List_e78c3561 = PineValue.List([Blob_Str_KernelApplication, List_3d45c81f]);

                public static readonly PineValue List_267921f2 = PineValue.List([List_e78c3561, List_0fcdb59d]);

                public static readonly PineValue List_Single_List_267921f2 = PineValue.List([List_267921f2]);

                public static readonly PineValue List_6aa28a01 = PineValue.List([Blob_Str_KernelApplication, List_0b98fd18]);

                public static readonly PineValue List_3eba11ea = PineValue.List([Blob_Str_KernelApplication, List_12a43a99]);

                public static readonly PineValue List_f2339352 = PineValue.List([Blob_Str_List, List_Single_List_267921f2]);

                public static readonly PineValue List_61e80345 = PineValue.List([Blob_Str_head, List_3eba11ea]);

                public static readonly PineValue List_6c106a54 = PineValue.List([Blob_Str_equal, List_f2339352]);

                public static readonly PineValue List_1efd62cc = PineValue.List([Blob_Str_KernelApplication, List_61e80345]);

                public static readonly PineValue List_49d6f26a = PineValue.List([Blob_Str_KernelApplication, List_6c106a54]);

                public static readonly PineValue List_b65b0587 = PineValue.List([Blob_Str_head, List_1efd62cc]);

                public static readonly PineValue List_f11efd55 = PineValue.List([Blob_Str_length, List_1efd62cc]);

                public static readonly PineValue List_135b6e7a = PineValue.List([Blob_Str_KernelApplication, List_b65b0587]);

                public static readonly PineValue List_Single_List_135b6e7a = PineValue.List([List_135b6e7a]);

                public static readonly PineValue List_Single_List_Single_List_135b6e7a =
                    PineValue.List([List_Single_List_135b6e7a]);

                public static readonly PineValue List_69478665 = PineValue.List([Blob_Str_KernelApplication, List_f11efd55]);

                public static readonly PineValue List_cb8cac92 =
                    PineValue.List([Blob_Str_List, List_Single_List_Single_List_135b6e7a]);

                public static readonly PineValue List_872debfe = PineValue.List([List_56404869, List_135b6e7a]);

                public static readonly PineValue List_Single_List_872debfe = PineValue.List([List_872debfe]);

                public static readonly PineValue List_1539324f = PineValue.List([List_69478665, List_0dcd86c0]);

                public static readonly PineValue List_Single_List_1539324f = PineValue.List([List_1539324f]);

                public static readonly PineValue List_a14d1649 = PineValue.List([Blob_Str_List, List_Single_List_872debfe]);

                public static readonly PineValue List_d36db637 = PineValue.List([Blob_Str_List, List_Single_List_1539324f]);

                public static readonly PineValue List_44926873 = PineValue.List([List_60c29edb, List_cb8cac92]);

                public static readonly PineValue List_Single_List_44926873 = PineValue.List([List_44926873]);

                public static readonly PineValue List_fc8e4107 = PineValue.List([Blob_Str_List, List_Single_List_44926873]);

                public static readonly PineValue List_e7e8c01f = PineValue.List([Blob_Str_equal, List_d36db637]);

                public static readonly PineValue List_6f2c2a97 = PineValue.List([Blob_Str_int_mul, List_a14d1649]);

                public static readonly PineValue List_0ee2b9b7 = PineValue.List([Blob_Str_KernelApplication, List_e7e8c01f]);

                public static readonly PineValue List_60789f7d = PineValue.List([Blob_Str_KernelApplication, List_6f2c2a97]);

                public static readonly PineValue List_Single_List_60789f7d = PineValue.List([List_60789f7d]);

                public static readonly PineValue List_Single_List_Single_List_60789f7d =
                    PineValue.List([List_Single_List_60789f7d]);

                public static readonly PineValue List_9956afdf =
                    PineValue.List([Blob_Str_List, List_Single_List_Single_List_60789f7d]);

                public static readonly PineValue List_a6a8cc4a = PineValue.List([List_0fcdb59d, List_9956afdf]);

                public static readonly PineValue List_Single_List_a6a8cc4a = PineValue.List([List_a6a8cc4a]);

                public static readonly PineValue List_94d93771 = PineValue.List([Blob_Str_List, List_Single_List_a6a8cc4a]);

                public static readonly PineValue List_a560e7dc = PineValue.List([List_fc8e4107, List_6aa28a01]);

                public static readonly PineValue List_Single_List_a560e7dc = PineValue.List([List_a560e7dc]);

                public static readonly PineValue List_c043b0f9 = PineValue.List([Blob_Str_List, List_Single_List_a560e7dc]);

                public static readonly PineValue List_994ba95d = PineValue.List([List_94d93771, List_6aa28a01]);

                public static readonly PineValue List_Single_List_994ba95d = PineValue.List([List_994ba95d]);

                public static readonly PineValue List_974dab64 = PineValue.List([Blob_Str_List, List_Single_List_994ba95d]);

                public static readonly PineValue List_925a0b0f = PineValue.List([List_0ee2b9b7, List_bb14d771, List_49d6f26a]);

                public static readonly PineValue List_b575cd71 = PineValue.List([Blob_Str_Conditional, List_925a0b0f]);

                public static readonly PineValue List_9e18d460 = PineValue.List([List_b575cd71, List_c043b0f9, List_974dab64]);

                public static readonly PineValue List_19efc503 = PineValue.List([Blob_Str_Conditional, List_9e18d460]);

                public static readonly PineValue List_15d4bf0f = PineValue.List([List_a96fdd25, List_7c594931, List_19efc503]);

                public static readonly PineValue List_f091141a = PineValue.List([Blob_Str_Conditional, List_15d4bf0f]);
            }
            """".Trim());
    }
}

public class SketchContainer
{
    // Sketching out the C# class we expect to generate:

    public static class Test
    {
        /*
         * 
         * The rendering below shows a (close to) ideal version we plan to arrive at: Using various proofs, we consolidated the code similar to how (experienced) humans would do to arrive at a nearly idiomatic (minus the variable labels) representation.
         * 
         * + We proved that if param_1_0 is not a blob, there is only one possible return value, and moved that check to the top accordingly.
         * + We proved that if param_1_1 does not encode a valid integer, there is only one possible return value, and moved that check to the top accordingly.
         * + Similarly, for the (remaining) length of the blob bytes: Since all the char literals we compare to in the inner loop are encoded as sequences of four bytes, we can prove that if the current slice is less than four bytes long, we will end up in the terminating case.
         * + The use of 'BinaryPrimitives.ReadInt*' seen below is an optimization for cases where we read a slice thats always two bytes or four bytes or eight bytes long. For other fixed slice lengths, we would use other approaches.
         * (This could be further optimized to avoid endianness conversions, by instead converting the other side of the equality check)
         * */

        /*
        public static PineValue parseInt_generic(PineValue env)
        {
            var param_1_0 =
                Common.ValueFromPathInValueOrEmptyList(env, [1, 0]);

            var param_1_1 =
                Common.ValueFromPathInValueOrEmptyList(env, [1, 1]);

            return parseInt(param_1_0, param_1_1);
        }
        */

        /*
        public static PineValue parseInt_specialized(
            PineValue param_1_0,
            PineValue param_1_1)
        {
            if (param_1_0 is not PineValue.BlobValue param_1_0_blob)
            {
                return
                    PineValue.List
                    ([
                        parseUnsignedInt_val_00,
                        IntegerEncoding.EncodeSignedInteger(0)
                    ]);
            }

            if (KernelFunction.SignedIntegerFromValueRelaxed(param_1_1) is not { } param_1_1_int)
            {
                return
                    PineValue.List
                    ([
                        parseUnsignedInt_val_00,
                        IntegerEncoding.EncodeSignedInteger(0)
                    ]);
            }

            if (param_1_0_blob.Bytes.Length < 4)
            {
                return
                    PineValue.List
                    ([
                        parseUnsignedInt_val_00,
                        IntegerEncoding.EncodeSignedInteger(0)
                    ]);
            }

            var param_1_0_blob_bytes =
                param_1_0_blob.Bytes.Span;

            var local_001_int32 =
                BinaryPrimitives.ReadInt32BigEndian(
                    param_1_0_blob_bytes[0..]);

            var local_001_int64 =
                (long)param_1_1_int;

            var local_002 =
                1;

            if (local_001_int32 is '-')
            {
                local_002 =
                    -1;

                local_001_int64 +=
                    4;

                if (param_1_0_blob_bytes.Length < local_001_int64 + 4)
                {
                    return
                        PineValue.List
                        ([
                            parseUnsignedInt_val_00,
                            IntegerEncoding.EncodeSignedInteger(local_001_int64)
                        ]);
                }

                local_001_int32 =
                    BinaryPrimitives.ReadInt32BigEndian(
                        param_1_0_blob_bytes[(int)local_001_int64..]);
            }

            BigInteger local_003_int =
                0;

            switch (local_001_int32)
            {
                case '0':
                    return
                        PineValue.List
                        ([
                            PineValue.List
                            ([
                                CommonValues.Tag_name_value_Ok,
                                IntegerEncoding.EncodeSignedInteger(0),
                            ]),
                            IntegerEncoding.EncodeSignedInteger(local_001_int64 + 4)
                        ]);

                case '1':
                    local_003_int =
                        1;

                    local_001_int64 +=
                        4;

                    break;

                case '2':
                    local_003_int =
                        2;

                    local_001_int64 +=
                        4;

                    break;

                case '3':
                    local_003_int =
                        3;

                    local_001_int64 +=
                        4;

                    break;

                case '4':
                    local_003_int =
                        4;

                    local_001_int64 +=
                        4;

                    break;

                case '5':
                    local_003_int =
                        5;

                    local_001_int64 +=
                        4;

                    break;

                case '6':
                    local_003_int =
                        6;

                    local_001_int64 +=
                        4;

                    break;

                case '7':
                    local_003_int =
                        7;

                    local_001_int64 +=
                        4;

                    break;

                case '8':
                    local_003_int =
                        8;

                    local_001_int64 +=
                        4;

                    break;

                case '9':
                    local_003_int =
                        9;

                    local_001_int64 +=
                        4;

                    break;

                default:
                    return
                        PineValue.List
                        ([
                            parseUnsignedInt_val_00,
                            IntegerEncoding.EncodeSignedInteger(local_001_int64)
                        ]);
            }

            while (true)
            {
                if (param_1_0_blob_bytes.Length < local_001_int64 + 4)
                {
                    break;
                }

                local_001_int32 =
                    BinaryPrimitives.ReadInt32BigEndian(
                        param_1_0_blob_bytes[(int)local_001_int64..]);

                if (local_001_int32 is '0')
                {
                    local_003_int =
                        local_003_int * 10;

                    local_001_int64 +=
                        4;

                    continue;
                }

                if (local_001_int32 is '1')
                {
                    local_003_int =
                        local_003_int * 10 + 1;

                    local_001_int64 +=
                        4;

                    continue;
                }

                if (local_001_int32 is '2')
                {
                    local_003_int =
                        local_003_int * 10 + 2;

                    local_001_int64 +=
                        4;

                    continue;
                }

                if (local_001_int32 is '3')
                {
                    local_003_int =
                        local_003_int * 10 + 3;

                    local_001_int64 +=
                        4;

                    continue;
                }

                if (local_001_int32 is '4')
                {
                    local_003_int =
                        local_003_int * 10 + 4;

                    local_001_int64 +=
                        4;

                    continue;
                }

                if (local_001_int32 is '5')
                {
                    local_003_int =
                        local_003_int * 10 + 5;

                    local_001_int64 +=
                        4;

                    continue;
                }

                if (local_001_int32 is '6')
                {
                    local_003_int =
                        local_003_int * 10 + 6;

                    local_001_int64 +=
                        4;

                    continue;
                }

                if (local_001_int32 is '7')
                {
                    local_003_int =
                        local_003_int * 10 + 7;

                    local_001_int64 +=
                        4;

                    continue;
                }

                if (local_001_int32 is '8')
                {
                    local_003_int =
                        local_003_int * 10 + 8;

                    local_001_int64 +=
                        4;

                    continue;
                }

                if (local_001_int32 is '9')
                {
                    local_003_int =
                        local_003_int * 10 + 9;

                    local_001_int64 +=
                        4;

                    continue;
                }

                break;
            }

            local_003_int *=
                local_002;

            return
                PineValue.List
                ([
                    PineValue.List
                    ([
                        CommonValues.Tag_name_value_Ok,
                        IntegerEncoding.EncodeSignedInteger(local_003_int),
                    ]),
                    IntegerEncoding.EncodeSignedInteger(local_001_int64)
                ]);
        }
        */

        /*
         * A first, primitive version, without any of the above proofs applied.
         * The only optimizations here are the spreading of function arguments and common sub-expression elimination.
         * */

        /*
                Test.parseInt param_1_0 param_1_1 =
                    if
                        Pine_kernel.equal
                            [ Pine_kernel.take
                                [ 4
                                , Pine_kernel.skip
                                    [ param_1_1
                                    , param_1_0
                                    ]
                                ]
                            , '-'
                            ]
                    then
                        if
                            Pine_kernel.equal
                                [ Ok
                                , Pine_kernel.head
                                    (Pine_kernel.head
                                        (Test.parseUnsignedInt
                                            param_1_0
                                            (Pine_kernel.int_add
                                                [ param_1_1
                                                , 4
                                                ]
                                            ))
                                    )
                                ]
                        then
                            [ [ Ok
                              , [ Pine_kernel.int_mul
                                    [ -1
                                    , Pine_kernel.head
                                        (Pine_kernel.head
                                            (Pine_kernel.skip
                                                [ 1
                                                , Pine_kernel.head
                                                    (Test.parseUnsignedInt
                                                        param_1_0
                                                        (Pine_kernel.int_add
                                                            [ param_1_1
                                                            , 4
                                                            ]
                                                        ))
                                                ]
                                            )
                                        )
                                    ]
                                ]
                              ]
                            , Pine_kernel.head
                                (Pine_kernel.skip
                                    [ 1
                                    , Test.parseUnsignedInt
                                        param_1_0
                                        (Pine_kernel.int_add
                                            [ param_1_1
                                            , 4
                                            ]
                                        )
                                    ]
                                )
                            ]

                        else if
                            Pine_kernel.equal
                                [ Err
                                , Pine_kernel.head
                                    (Pine_kernel.head
                                        (Test.parseUnsignedInt
                                            param_1_0
                                            (Pine_kernel.int_add
                                                [ param_1_1
                                                , 4
                                                ]
                                            ))
                                    )
                                ]
                        then
                            [ [ Err
                              , [ Pine_kernel.head
                                    (Pine_kernel.head
                                        (Pine_kernel.skip
                                            [ 1
                                            , Pine_kernel.head
                                                (Test.parseUnsignedInt
                                                    param_1_0
                                                    (Pine_kernel.int_add
                                                        [ param_1_1
                                                        , 4
                                                        ]
                                                    ))
                                            ]
                                        )
                                    )
                                ]
                              ]
                            , Pine_kernel.head
                                (Pine_kernel.skip
                                    [ 1
                                    , Test.parseUnsignedInt
                                        param_1_0
                                        (Pine_kernel.int_add
                                            [ param_1_1
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
                            param_1_0
                            param_1_1
         * */

        public static PineValue parseInt(
            PineValue param_1_0,
            PineValue param_1_1)
        {
            if (KernelFunctionFused.SkipAndTake(takeCount: 4, skipCountValue: param_1_1, argument: param_1_0) == CommonReusedValues.Blob_37f21fcb)
            {
                PineValue local_000 =
                    Test.parseUnsignedInt(
                        param_1_0,
                        KernelFunctionSpecialized.int_add(4, param_1_1));

                PineValue local_001 =
                    KernelFunction.head(local_000);

                PineValue local_002 =
                    KernelFunction.head(local_001);

                if (CommonReusedValues.Blob_Str_Ok == local_002)
                {
                    return
                        PineValue.List(
                            [
                                PineValue.List(
                                [
                                    CommonReusedValues.Blob_Str_Ok,
                                    PineValue.List(
                                        [
                                            KernelFunctionSpecialized.int_mul(
                                                -1,
                                                KernelFunction.head(
                                                    KernelFunction.head(
                                                        KernelFunctionSpecialized.skip(1, local_001))))
                                        ])
                                ]),
                            KernelFunction.head(
                                KernelFunctionSpecialized.skip(1, local_000))
                            ]);
                }

                if (CommonReusedValues.Blob_Str_Err == local_002)
                {
                    return
                        PineValue.List(
                            [
                                PineValue.List(
                                [
                                    CommonReusedValues.Blob_Str_Err,
                                    PineValue.List(
                                        [
                                            KernelFunction.head(
                                                KernelFunction.head(
                                                    KernelFunctionSpecialized.skip(1, local_001)))
                                        ])
                                ]),
                            KernelFunction.head(
                                KernelFunctionSpecialized.skip(1, local_000))
                            ]);
                }

                throw new ParseExpressionException("TODO: Include details from encoded and env subexpressions");
            }

            return Test.parseUnsignedInt(param_1_0, param_1_1);
        }


        public static PineValue parseUnsignedInt(
            PineValue param_1_0,
            PineValue param_1_1)
        {
            PineValue local_000 =
                KernelFunctionFused.SkipAndTake(takeCount: 4, skipCountValue: param_1_1, argument: param_1_0);

            if (local_000 == CommonReusedValues.Blob_Str_0)
            {
                return
                    PineValue.List(
                        [
                            CommonReusedValues.List_c3304aab,
                        KernelFunctionSpecialized.int_add(4, param_1_1)
                        ]);
            }

            if (local_000 == CommonReusedValues.Blob_Str_1)
            {
                return
                    Test.parseUnsignedIntRec(
                        CommonReusedValues.Blob_Int_1,
                        param_1_0,
                        KernelFunctionSpecialized.int_add(4, param_1_1));
            }

            if (local_000 == CommonReusedValues.Blob_Str_2)
            {
                return
                    Test.parseUnsignedIntRec(
                        CommonReusedValues.Blob_Int_2,
                        param_1_0,
                        KernelFunctionSpecialized.int_add(4, param_1_1));
            }

            if (local_000 == CommonReusedValues.Blob_Str_3)
            {
                return
                    Test.parseUnsignedIntRec(
                        CommonReusedValues.Blob_Int_3,
                        param_1_0,
                        KernelFunctionSpecialized.int_add(4, param_1_1));
            }

            if (local_000 == CommonReusedValues.Blob_Str_4)
            {
                return
                    Test.parseUnsignedIntRec(
                        CommonReusedValues.Blob_Int_4,
                        param_1_0,
                        KernelFunctionSpecialized.int_add(4, param_1_1));
            }

            if (local_000 == CommonReusedValues.Blob_Str_5)
            {
                return
                    Test.parseUnsignedIntRec(
                        CommonReusedValues.Blob_Int_5,
                        param_1_0,
                        KernelFunctionSpecialized.int_add(4, param_1_1));
            }

            if (local_000 == CommonReusedValues.Blob_Str_6)
            {
                return
                    Test.parseUnsignedIntRec(
                        CommonReusedValues.Blob_Int_6,
                        param_1_0,
                        KernelFunctionSpecialized.int_add(4, param_1_1));
            }

            if (local_000 == CommonReusedValues.Blob_Str_7)
            {
                return
                    Test.parseUnsignedIntRec(
                        CommonReusedValues.Blob_Int_7,
                        param_1_0,
                        KernelFunctionSpecialized.int_add(4, param_1_1));
            }

            if (local_000 == CommonReusedValues.Blob_Str_8)
            {
                return
                    Test.parseUnsignedIntRec(
                        CommonReusedValues.Blob_Int_8,
                        param_1_0,
                        KernelFunctionSpecialized.int_add(4, param_1_1));
            }

            if (local_000 == CommonReusedValues.Blob_Str_9)
            {
                return
                    Test.parseUnsignedIntRec(
                        CommonReusedValues.Blob_Int_9,
                        param_1_0,
                        KernelFunctionSpecialized.int_add(4, param_1_1));
            }

            return
                PineValue.List(
                    [CommonReusedValues.List_ae45bd54, param_1_1]);
        }


        public static PineValue parseUnsignedIntRec(
            PineValue param_1_0,
            PineValue param_1_1,
            PineValue param_1_2)
        {
            PineValue local_param_1_0 =
                param_1_0;

            PineValue local_param_1_1 =
                param_1_1;

            PineValue local_param_1_2 =
                param_1_2;

            while (true)
            {
                PineValue local_000 =
                    KernelFunctionFused.SkipAndTake(takeCount: 4, skipCountValue: local_param_1_2, argument: local_param_1_1);

                if (local_000 == CommonReusedValues.Blob_Str_0)
                {
                    local_param_1_0 =
                        KernelFunctionSpecialized.int_mul(10, local_param_1_0);

                    local_param_1_2 =
                        KernelFunctionSpecialized.int_add(4, local_param_1_2);

                    continue;
                }

                if (local_000 == CommonReusedValues.Blob_Str_1)
                {
                    local_param_1_0 =
                        KernelFunctionSpecialized.int_add(
                            1,
                            KernelFunctionSpecialized.int_mul(10, local_param_1_0));

                    local_param_1_2 =
                        KernelFunctionSpecialized.int_add(4, local_param_1_2);

                    continue;
                }

                if (local_000 == CommonReusedValues.Blob_Str_2)
                {
                    local_param_1_0 =
                        KernelFunctionSpecialized.int_add(
                            2,
                            KernelFunctionSpecialized.int_mul(10, local_param_1_0));

                    local_param_1_2 =
                        KernelFunctionSpecialized.int_add(4, local_param_1_2);

                    continue;
                }

                if (local_000 == CommonReusedValues.Blob_Str_3)
                {
                    local_param_1_0 =
                        KernelFunctionSpecialized.int_add(
                            3,
                            KernelFunctionSpecialized.int_mul(10, local_param_1_0));

                    local_param_1_2 =
                        KernelFunctionSpecialized.int_add(4, local_param_1_2);

                    continue;
                }

                if (local_000 == CommonReusedValues.Blob_Str_4)
                {
                    local_param_1_0 =
                        KernelFunctionSpecialized.int_add(
                            4,
                            KernelFunctionSpecialized.int_mul(10, local_param_1_0));

                    local_param_1_2 =
                        KernelFunctionSpecialized.int_add(4, local_param_1_2);

                    continue;
                }

                if (local_000 == CommonReusedValues.Blob_Str_5)
                {
                    local_param_1_0 =
                        KernelFunctionSpecialized.int_add(
                            5,
                            KernelFunctionSpecialized.int_mul(10, local_param_1_0));

                    local_param_1_2 =
                        KernelFunctionSpecialized.int_add(4, local_param_1_2);

                    continue;
                }

                if (local_000 == CommonReusedValues.Blob_Str_6)
                {
                    local_param_1_0 =
                        KernelFunctionSpecialized.int_add(
                            6,
                            KernelFunctionSpecialized.int_mul(10, local_param_1_0));

                    local_param_1_2 =
                        KernelFunctionSpecialized.int_add(4, local_param_1_2);

                    continue;
                }

                if (local_000 == CommonReusedValues.Blob_Str_7)
                {
                    local_param_1_0 =
                        KernelFunctionSpecialized.int_add(
                            7,
                            KernelFunctionSpecialized.int_mul(10, local_param_1_0));

                    local_param_1_2 =
                        KernelFunctionSpecialized.int_add(4, local_param_1_2);

                    continue;
                }

                if (local_000 == CommonReusedValues.Blob_Str_8)
                {
                    local_param_1_0 =
                        KernelFunctionSpecialized.int_add(
                            8,
                            KernelFunctionSpecialized.int_mul(10, local_param_1_0));

                    local_param_1_2 =
                        KernelFunctionSpecialized.int_add(4, local_param_1_2);

                    continue;
                }

                if (local_000 == CommonReusedValues.Blob_Str_9)
                {
                    local_param_1_0 =
                        KernelFunctionSpecialized.int_add(
                            9,
                            KernelFunctionSpecialized.int_mul(10, local_param_1_0));

                    local_param_1_2 =
                        KernelFunctionSpecialized.int_add(4, local_param_1_2);

                    continue;
                }

                return
                    PineValue.List(
                        [
                            PineValue.List(
                            [
                                CommonReusedValues.Blob_Str_Ok,
                                PineValue.List(
                                    [local_param_1_0])
                            ]),
                        local_param_1_2
                        ]);
            }
        }
    }


    public static class CommonReusedValues
    {
        public static readonly PineValue Blob_Int_4 =
            IntegerEncoding.EncodeSignedInteger(4);

        public static readonly PineValue Blob_Int_1 =
            IntegerEncoding.EncodeSignedInteger(1);

        public static readonly PineValue Blob_Int_neg_1 =
            IntegerEncoding.EncodeSignedInteger(-1);

        public static readonly PineValue Blob_Str_Ok =
            StringEncoding.ValueFromString("Ok");

        public static readonly PineValue Blob_Str_Err =
            StringEncoding.ValueFromString("Err");

        public static readonly PineValue Blob_37f21fcb =
            StringEncoding.ValueFromString("-");

        public static readonly PineValue Blob_Int_2 =
            IntegerEncoding.EncodeSignedInteger(2);

        public static readonly PineValue Blob_Int_3 =
            IntegerEncoding.EncodeSignedInteger(3);

        public static readonly PineValue Blob_Int_5 =
            IntegerEncoding.EncodeSignedInteger(5);

        public static readonly PineValue Blob_Int_6 =
            IntegerEncoding.EncodeSignedInteger(6);

        public static readonly PineValue Blob_Int_7 =
            IntegerEncoding.EncodeSignedInteger(7);

        public static readonly PineValue Blob_Int_8 =
            IntegerEncoding.EncodeSignedInteger(8);

        public static readonly PineValue Blob_Int_9 =
            IntegerEncoding.EncodeSignedInteger(9);

        public static readonly PineValue Blob_Str_9 =
            StringEncoding.ValueFromString("9");

        public static readonly PineValue Blob_Str_8 =
            StringEncoding.ValueFromString("8");

        public static readonly PineValue Blob_Str_7 =
            StringEncoding.ValueFromString("7");

        public static readonly PineValue Blob_Str_6 =
            StringEncoding.ValueFromString("6");

        public static readonly PineValue Blob_Str_5 =
            StringEncoding.ValueFromString("5");

        public static readonly PineValue Blob_Str_4 =
            StringEncoding.ValueFromString("4");

        public static readonly PineValue Blob_Str_3 =
            StringEncoding.ValueFromString("3");

        public static readonly PineValue Blob_Str_2 =
            StringEncoding.ValueFromString("2");

        public static readonly PineValue Blob_Str_1 =
            StringEncoding.ValueFromString("1");

        public static readonly PineValue Blob_Str_0 =
            StringEncoding.ValueFromString("0");

        public static readonly PineValue Blob_Int_10 =
            IntegerEncoding.EncodeSignedInteger(10);

        public static readonly PineValue Blob_Str_String =
            StringEncoding.ValueFromString("String");

        public static readonly PineValue Blob_cfca8ee1 =
            StringEncoding.ValueFromString("Expecting a digit");

        public static readonly PineValue Blob_Int_0 =
            IntegerEncoding.EncodeSignedInteger(0);

        public static readonly PineValue Blob_Str_Conditional =
            StringEncoding.ValueFromString("Conditional");

        public static readonly PineValue Blob_Str_ParseAndEval =
            StringEncoding.ValueFromString("ParseAndEval");

        public static readonly PineValue Blob_Str_List =
            StringEncoding.ValueFromString("List");

        public static readonly PineValue Blob_Str_KernelApplication =
            StringEncoding.ValueFromString("KernelApplication");

        public static readonly PineValue Blob_Str_int_add =
            StringEncoding.ValueFromString("int_add");

        public static readonly PineValue Blob_Str_Literal =
            StringEncoding.ValueFromString("Literal");

        public static readonly PineValue Blob_Str_head =
            StringEncoding.ValueFromString("head");

        public static readonly PineValue Blob_Str_skip =
            StringEncoding.ValueFromString("skip");

        public static readonly PineValue Blob_Str_Environment =
            StringEncoding.ValueFromString("Environment");

        public static readonly PineValue Blob_Str_int_mul =
            StringEncoding.ValueFromString("int_mul");

        public static readonly PineValue Blob_Str_equal =
            StringEncoding.ValueFromString("equal");

        public static readonly PineValue Blob_Str_take =
            StringEncoding.ValueFromString("take");

        public static readonly PineValue List_dda26649 =
            PineValue.EmptyList;

        public static readonly PineValue List_37ca38cd =
            PineValue.List(
                [Blob_cfca8ee1]);

        public static readonly PineValue List_0a7103c5 =
            PineValue.List(
                [
                    IntegerEncoding.EncodeSignedInteger(0)
                ]);

        public static readonly PineValue List_fd811a2e =
            PineValue.List(
                [
                    IntegerEncoding.EncodeSignedInteger(4)
                ]);

        public static readonly PineValue List_7cb7ad19 =
            PineValue.List(
                [
                    IntegerEncoding.EncodeSignedInteger(1)
                ]);

        public static readonly PineValue List_c286665f =
            PineValue.List(
                [
                    IntegerEncoding.EncodeSignedInteger(2)
                ]);

        public static readonly PineValue List_1cb34950 =
            PineValue.List(
                [
                    IntegerEncoding.EncodeSignedInteger(10)
                ]);

        public static readonly PineValue List_9b844394 =
            PineValue.List(
                [
                    IntegerEncoding.EncodeSignedInteger(3)
                ]);

        public static readonly PineValue List_844b2842 =
            PineValue.List(
                [
                    IntegerEncoding.EncodeSignedInteger(5)
                ]);

        public static readonly PineValue List_2c4fdf60 =
            PineValue.List(
                [
                    IntegerEncoding.EncodeSignedInteger(6)
                ]);

        public static readonly PineValue List_6ded852c =
            PineValue.List(
                [
                    IntegerEncoding.EncodeSignedInteger(7)
                ]);

        public static readonly PineValue List_259edf24 =
            PineValue.List(
                [
                    IntegerEncoding.EncodeSignedInteger(8)
                ]);

        public static readonly PineValue List_71131828 =
            PineValue.List(
                [
                    IntegerEncoding.EncodeSignedInteger(9)
                ]);

        public static readonly PineValue List_fa93aa90 =
            PineValue.List(
                [Blob_Str_Ok]);

        public static readonly PineValue List_87b3a5b0 =
            PineValue.List(
                [Blob_Str_9]);

        public static readonly PineValue List_5f13cdae =
            PineValue.List(
                [Blob_Str_8]);

        public static readonly PineValue List_f767fe42 =
            PineValue.List(
                [Blob_Str_7]);

        public static readonly PineValue List_4d45d38a =
            PineValue.List(
                [Blob_Str_6]);

        public static readonly PineValue List_d8088a1d =
            PineValue.List(
                [Blob_Str_5]);

        public static readonly PineValue List_53ffebf2 =
            PineValue.List(
                [Blob_Str_4]);

        public static readonly PineValue List_b93b4b39 =
            PineValue.List(
                [Blob_Str_3]);

        public static readonly PineValue List_d6a1a968 =
            PineValue.List(
                [Blob_Str_2]);

        public static readonly PineValue List_e3413a44 =
            PineValue.List(
                [Blob_Str_1]);

        public static readonly PineValue List_c7702eda =
            PineValue.List(
                [Blob_Str_0]);

        public static readonly PineValue List_bd06385a =
            PineValue.List(
                [Blob_Str_Environment, List_dda26649]);

        public static readonly PineValue List_708c3c98 =
            PineValue.List(
                [Blob_Str_String, List_37ca38cd]);

        public static readonly PineValue List_c3304aab =
            PineValue.List(
                [Blob_Str_Ok, List_0a7103c5]);

        public static readonly PineValue List_0c82888c =
            PineValue.List(
                [Blob_Str_Literal, List_fd811a2e]);

        public static readonly PineValue List_0dcd86c0 =
            PineValue.List(
                [Blob_Str_Literal, List_7cb7ad19]);

        public static readonly PineValue List_43b95777 =
            PineValue.List(
                [Blob_Str_Literal, List_c286665f]);

        public static readonly PineValue List_c41d9105 =
            PineValue.List(
                [Blob_Str_Literal, List_1cb34950]);

        public static readonly PineValue List_450c12a0 =
            PineValue.List(
                [Blob_Str_Literal, List_9b844394]);

        public static readonly PineValue List_c1b27e6e =
            PineValue.List(
                [Blob_Str_Literal, List_844b2842]);

        public static readonly PineValue List_282dee3a =
            PineValue.List(
                [Blob_Str_Literal, List_2c4fdf60]);

        public static readonly PineValue List_9f1e38f9 =
            PineValue.List(
                [Blob_Str_Literal, List_6ded852c]);

        public static readonly PineValue List_14a0ba72 =
            PineValue.List(
                [Blob_Str_Literal, List_259edf24]);

        public static readonly PineValue List_a710c27f =
            PineValue.List(
                [Blob_Str_Literal, List_71131828]);

        public static readonly PineValue List_0fcdb59d =
            PineValue.List(
                [Blob_Str_Literal, List_fa93aa90]);

        public static readonly PineValue List_ecf5f39d =
            PineValue.List(
                [Blob_Str_Literal, List_87b3a5b0]);

        public static readonly PineValue List_1a0b8610 =
            PineValue.List(
                [Blob_Str_Literal, List_5f13cdae]);

        public static readonly PineValue List_33c6f4ad =
            PineValue.List(
                [Blob_Str_Literal, List_f767fe42]);

        public static readonly PineValue List_c005c994 =
            PineValue.List(
                [Blob_Str_Literal, List_4d45d38a]);

        public static readonly PineValue List_86fad7dd =
            PineValue.List(
                [Blob_Str_Literal, List_d8088a1d]);

        public static readonly PineValue List_9728f698 =
            PineValue.List(
                [Blob_Str_Literal, List_53ffebf2]);

        public static readonly PineValue List_a9196577 =
            PineValue.List(
                [Blob_Str_Literal, List_b93b4b39]);

        public static readonly PineValue List_e844984b =
            PineValue.List(
                [Blob_Str_Literal, List_d6a1a968]);

        public static readonly PineValue List_c3b08663 =
            PineValue.List(
                [Blob_Str_Literal, List_e3413a44]);

        public static readonly PineValue List_4c2609c3 =
            PineValue.List(
                [Blob_Str_Literal, List_c7702eda]);

        public static readonly PineValue List_81718afa =
            PineValue.List(
                [List_708c3c98]);

        public static readonly PineValue List_4aad0d20 =
            PineValue.List(
                [Blob_Str_head, List_bd06385a]);

        public static readonly PineValue List_ae45bd54 =
            PineValue.List(
                [Blob_Str_Err, List_81718afa]);

        public static readonly PineValue List_a5cbaf18 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_4aad0d20]);

        public static readonly PineValue List_c1cc00b1 =
            PineValue.List(
                [List_0dcd86c0, List_bd06385a]);

        public static readonly PineValue List_dcbfdaf0 =
            PineValue.List(
                [List_c1cc00b1]);

        public static readonly PineValue List_61fa9b59 =
            PineValue.List(
                [Blob_Str_head, List_a5cbaf18]);

        public static readonly PineValue List_4c301747 =
            PineValue.List(
                [Blob_Str_List, List_dcbfdaf0]);

        public static readonly PineValue List_976730e9 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_61fa9b59]);

        public static readonly PineValue List_b49facc1 =
            PineValue.List(
                [Blob_Str_skip, List_4c301747]);

        public static readonly PineValue List_65405503 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_b49facc1]);

        public static readonly PineValue List_7552bdea =
            PineValue.List(
                [Blob_Str_head, List_65405503]);

        public static readonly PineValue List_f1126ab7 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_7552bdea]);

        public static readonly PineValue List_16c4bb4a =
            PineValue.List(
                [Blob_Str_head, List_f1126ab7]);

        public static readonly PineValue List_b43468c9 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_16c4bb4a]);

        public static readonly PineValue List_fd602b70 =
            PineValue.List(
                [List_43b95777, List_f1126ab7]);

        public static readonly PineValue List_13c67965 =
            PineValue.List(
                [List_0dcd86c0, List_f1126ab7]);

        public static readonly PineValue List_ecedc467 =
            PineValue.List(
                [List_b43468c9]);

        public static readonly PineValue List_22254e85 =
            PineValue.List(
                [List_fd602b70]);

        public static readonly PineValue List_498d834e =
            PineValue.List(
                [List_13c67965]);

        public static readonly PineValue List_1a17feda =
            PineValue.List(
                [List_ecedc467]);

        public static readonly PineValue List_4668ada6 =
            PineValue.List(
                [Blob_Str_List, List_22254e85]);

        public static readonly PineValue List_85fcd3b2 =
            PineValue.List(
                [Blob_Str_List, List_498d834e]);

        public static readonly PineValue List_c0abcf7c =
            PineValue.List(
                [Blob_Str_List, List_1a17feda]);

        public static readonly PineValue List_fcd219b8 =
            PineValue.List(
                [List_b43468c9, List_c41d9105]);

        public static readonly PineValue List_3218a5a2 =
            PineValue.List(
                [Blob_Str_skip, List_4668ada6]);

        public static readonly PineValue List_6b896037 =
            PineValue.List(
                [Blob_Str_skip, List_85fcd3b2]);

        public static readonly PineValue List_3762bb34 =
            PineValue.List(
                [List_fcd219b8]);

        public static readonly PineValue List_ee3ba15f =
            PineValue.List(
                [Blob_Str_KernelApplication, List_3218a5a2]);

        public static readonly PineValue List_c37ef632 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_6b896037]);

        public static readonly PineValue List_00efda36 =
            PineValue.List(
                [Blob_Str_List, List_3762bb34]);

        public static readonly PineValue List_fd51f5ba =
            PineValue.List(
                [List_0fcdb59d, List_c0abcf7c]);

        public static readonly PineValue List_290d4c0d =
            PineValue.List(
                [Blob_Str_head, List_ee3ba15f]);

        public static readonly PineValue List_a53f1292 =
            PineValue.List(
                [Blob_Str_head, List_c37ef632]);

        public static readonly PineValue List_58360a74 =
            PineValue.List(
                [Blob_Str_int_mul, List_00efda36]);

        public static readonly PineValue List_2af1c018 =
            PineValue.List(
                [List_fd51f5ba]);

        public static readonly PineValue List_768cc61e =
            PineValue.List(
                [Blob_Str_KernelApplication, List_290d4c0d]);

        public static readonly PineValue List_4af1f0ec =
            PineValue.List(
                [Blob_Str_KernelApplication, List_a53f1292]);

        public static readonly PineValue List_46a1ad58 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_58360a74]);

        public static readonly PineValue List_a2e49ce2 =
            PineValue.List(
                [Blob_Str_List, List_2af1c018]);

        public static readonly PineValue List_b0e5fccb =
            PineValue.List(
                [List_768cc61e, List_0c82888c]);

        public static readonly PineValue List_5eb53fae =
            PineValue.List(
                [List_46a1ad58, List_0dcd86c0]);

        public static readonly PineValue List_b6c4e1b8 =
            PineValue.List(
                [List_46a1ad58, List_43b95777]);

        public static readonly PineValue List_5f70f875 =
            PineValue.List(
                [List_46a1ad58, List_450c12a0]);

        public static readonly PineValue List_df3bbeff =
            PineValue.List(
                [List_46a1ad58, List_0c82888c]);

        public static readonly PineValue List_ce74c88a =
            PineValue.List(
                [List_46a1ad58, List_c1b27e6e]);

        public static readonly PineValue List_27223788 =
            PineValue.List(
                [List_46a1ad58, List_282dee3a]);

        public static readonly PineValue List_3b7fa2f9 =
            PineValue.List(
                [List_46a1ad58, List_9f1e38f9]);

        public static readonly PineValue List_8b277fbf =
            PineValue.List(
                [List_46a1ad58, List_14a0ba72]);

        public static readonly PineValue List_dc67832f =
            PineValue.List(
                [List_46a1ad58, List_a710c27f]);

        public static readonly PineValue List_65e9b2dc =
            PineValue.List(
                [List_b0e5fccb]);

        public static readonly PineValue List_ddb03eb1 =
            PineValue.List(
                [List_5eb53fae]);

        public static readonly PineValue List_aa48859b =
            PineValue.List(
                [List_b6c4e1b8]);

        public static readonly PineValue List_616b737a =
            PineValue.List(
                [List_5f70f875]);

        public static readonly PineValue List_d15b35e2 =
            PineValue.List(
                [List_df3bbeff]);

        public static readonly PineValue List_4982a6d1 =
            PineValue.List(
                [List_ce74c88a]);

        public static readonly PineValue List_e19d671e =
            PineValue.List(
                [List_27223788]);

        public static readonly PineValue List_6ee6ab58 =
            PineValue.List(
                [List_3b7fa2f9]);

        public static readonly PineValue List_7ef2e53c =
            PineValue.List(
                [List_8b277fbf]);

        public static readonly PineValue List_c6c68a29 =
            PineValue.List(
                [List_dc67832f]);

        public static readonly PineValue List_6781a24c =
            PineValue.List(
                [Blob_Str_List, List_65e9b2dc]);

        public static readonly PineValue List_5c3bbdb0 =
            PineValue.List(
                [Blob_Str_List, List_ddb03eb1]);

        public static readonly PineValue List_e7d01de3 =
            PineValue.List(
                [Blob_Str_List, List_aa48859b]);

        public static readonly PineValue List_6906c1af =
            PineValue.List(
                [Blob_Str_List, List_616b737a]);

        public static readonly PineValue List_cb8d914f =
            PineValue.List(
                [Blob_Str_List, List_d15b35e2]);

        public static readonly PineValue List_43198349 =
            PineValue.List(
                [Blob_Str_List, List_4982a6d1]);

        public static readonly PineValue List_b77909c1 =
            PineValue.List(
                [Blob_Str_List, List_e19d671e]);

        public static readonly PineValue List_efe72e41 =
            PineValue.List(
                [Blob_Str_List, List_6ee6ab58]);

        public static readonly PineValue List_f86ea0de =
            PineValue.List(
                [Blob_Str_List, List_7ef2e53c]);

        public static readonly PineValue List_2b81f547 =
            PineValue.List(
                [Blob_Str_List, List_c6c68a29]);

        public static readonly PineValue List_0d8426a4 =
            PineValue.List(
                [Blob_Str_int_add, List_6781a24c]);

        public static readonly PineValue List_66726dc9 =
            PineValue.List(
                [Blob_Str_int_add, List_5c3bbdb0]);

        public static readonly PineValue List_e4e2cda5 =
            PineValue.List(
                [Blob_Str_int_add, List_e7d01de3]);

        public static readonly PineValue List_75a24f9c =
            PineValue.List(
                [Blob_Str_int_add, List_6906c1af]);

        public static readonly PineValue List_5892ef1e =
            PineValue.List(
                [Blob_Str_int_add, List_cb8d914f]);

        public static readonly PineValue List_86a202c9 =
            PineValue.List(
                [Blob_Str_int_add, List_43198349]);

        public static readonly PineValue List_f22e19d7 =
            PineValue.List(
                [Blob_Str_int_add, List_b77909c1]);

        public static readonly PineValue List_72982702 =
            PineValue.List(
                [Blob_Str_int_add, List_efe72e41]);

        public static readonly PineValue List_6826ad0e =
            PineValue.List(
                [Blob_Str_int_add, List_f86ea0de]);

        public static readonly PineValue List_0dfb84af =
            PineValue.List(
                [Blob_Str_int_add, List_2b81f547]);

        public static readonly PineValue List_d64a63d0 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_0d8426a4]);

        public static readonly PineValue List_c02d27c5 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_66726dc9]);

        public static readonly PineValue List_529fb4ab =
            PineValue.List(
                [Blob_Str_KernelApplication, List_e4e2cda5]);

        public static readonly PineValue List_87ef138d =
            PineValue.List(
                [Blob_Str_KernelApplication, List_75a24f9c]);

        public static readonly PineValue List_b2dcfd38 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_5892ef1e]);

        public static readonly PineValue List_8a3b6b92 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_86a202c9]);

        public static readonly PineValue List_5810dc4f =
            PineValue.List(
                [Blob_Str_KernelApplication, List_f22e19d7]);

        public static readonly PineValue List_6de702a0 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_72982702]);

        public static readonly PineValue List_7c0e13ba =
            PineValue.List(
                [Blob_Str_KernelApplication, List_6826ad0e]);

        public static readonly PineValue List_e5771b61 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_0dfb84af]);

        public static readonly PineValue List_21c04354 =
            PineValue.List(
                [List_a2e49ce2, List_768cc61e]);

        public static readonly PineValue List_2449c52e =
            PineValue.List(
                [List_768cc61e, List_4af1f0ec]);

        public static readonly PineValue List_ab939a5d =
            PineValue.List(
                [List_21c04354]);

        public static readonly PineValue List_2563cb2b =
            PineValue.List(
                [List_2449c52e]);

        public static readonly PineValue List_2966387f =
            PineValue.List(
                [Blob_Str_List, List_ab939a5d]);

        public static readonly PineValue List_d8c1129d =
            PineValue.List(
                [Blob_Str_List, List_2563cb2b]);

        public static readonly PineValue List_ed23348b =
            PineValue.List(
                [Blob_Str_skip, List_d8c1129d]);

        public static readonly PineValue List_84e5f176 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_ed23348b]);

        public static readonly PineValue List_5d745015 =
            PineValue.List(
                [List_0c82888c, List_84e5f176]);

        public static readonly PineValue List_fda03c60 =
            PineValue.List(
                [List_5d745015]);

        public static readonly PineValue List_da5eb341 =
            PineValue.List(
                [Blob_Str_List, List_fda03c60]);

        public static readonly PineValue List_d7ca573f =
            PineValue.List(
                [Blob_Str_take, List_da5eb341]);

        public static readonly PineValue List_147fffd6 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_d7ca573f]);

        public static readonly PineValue List_02261755 =
            PineValue.List(
                [List_147fffd6, List_ecf5f39d]);

        public static readonly PineValue List_46d7bb4a =
            PineValue.List(
                [List_147fffd6, List_1a0b8610]);

        public static readonly PineValue List_bf8e6307 =
            PineValue.List(
                [List_147fffd6, List_33c6f4ad]);

        public static readonly PineValue List_03208aa8 =
            PineValue.List(
                [List_147fffd6, List_c005c994]);

        public static readonly PineValue List_cd3c531a =
            PineValue.List(
                [List_147fffd6, List_86fad7dd]);

        public static readonly PineValue List_ebfbbb42 =
            PineValue.List(
                [List_147fffd6, List_9728f698]);

        public static readonly PineValue List_283df93a =
            PineValue.List(
                [List_147fffd6, List_a9196577]);

        public static readonly PineValue List_7cd434e2 =
            PineValue.List(
                [List_147fffd6, List_e844984b]);

        public static readonly PineValue List_8dc97585 =
            PineValue.List(
                [List_147fffd6, List_c3b08663]);

        public static readonly PineValue List_972ec495 =
            PineValue.List(
                [List_147fffd6, List_4c2609c3]);

        public static readonly PineValue List_d53f72de =
            PineValue.List(
                [List_02261755]);

        public static readonly PineValue List_bf3d44d0 =
            PineValue.List(
                [List_46d7bb4a]);

        public static readonly PineValue List_041622e9 =
            PineValue.List(
                [List_bf8e6307]);

        public static readonly PineValue List_f74fcd51 =
            PineValue.List(
                [List_03208aa8]);

        public static readonly PineValue List_1ad14b7b =
            PineValue.List(
                [List_cd3c531a]);

        public static readonly PineValue List_9c9c8886 =
            PineValue.List(
                [List_ebfbbb42]);

        public static readonly PineValue List_d8c2b6dc =
            PineValue.List(
                [List_283df93a]);

        public static readonly PineValue List_2fc31437 =
            PineValue.List(
                [List_7cd434e2]);

        public static readonly PineValue List_db749ca1 =
            PineValue.List(
                [List_8dc97585]);

        public static readonly PineValue List_1cab1ec5 =
            PineValue.List(
                [List_972ec495]);

        public static readonly PineValue List_75bd24ea =
            PineValue.List(
                [Blob_Str_List, List_d53f72de]);

        public static readonly PineValue List_9a4b564f =
            PineValue.List(
                [Blob_Str_List, List_bf3d44d0]);

        public static readonly PineValue List_f9fc4a76 =
            PineValue.List(
                [Blob_Str_List, List_041622e9]);

        public static readonly PineValue List_43b511e6 =
            PineValue.List(
                [Blob_Str_List, List_f74fcd51]);

        public static readonly PineValue List_7b97cebf =
            PineValue.List(
                [Blob_Str_List, List_1ad14b7b]);

        public static readonly PineValue List_76afddf9 =
            PineValue.List(
                [Blob_Str_List, List_9c9c8886]);

        public static readonly PineValue List_3dbe51bb =
            PineValue.List(
                [Blob_Str_List, List_d8c2b6dc]);

        public static readonly PineValue List_9d3dd2b9 =
            PineValue.List(
                [Blob_Str_List, List_2fc31437]);

        public static readonly PineValue List_a228d941 =
            PineValue.List(
                [Blob_Str_List, List_db749ca1]);

        public static readonly PineValue List_dbd07d5a =
            PineValue.List(
                [Blob_Str_List, List_1cab1ec5]);

        public static readonly PineValue List_c3c9441c =
            PineValue.List(
                [Blob_Str_equal, List_75bd24ea]);

        public static readonly PineValue List_118b39c9 =
            PineValue.List(
                [Blob_Str_equal, List_9a4b564f]);

        public static readonly PineValue List_b8872664 =
            PineValue.List(
                [Blob_Str_equal, List_f9fc4a76]);

        public static readonly PineValue List_b21cc8a5 =
            PineValue.List(
                [Blob_Str_equal, List_43b511e6]);

        public static readonly PineValue List_1822bf9f =
            PineValue.List(
                [Blob_Str_equal, List_7b97cebf]);

        public static readonly PineValue List_0dd4cf27 =
            PineValue.List(
                [Blob_Str_equal, List_76afddf9]);

        public static readonly PineValue List_3531bee3 =
            PineValue.List(
                [Blob_Str_equal, List_3dbe51bb]);

        public static readonly PineValue List_9b0e5640 =
            PineValue.List(
                [Blob_Str_equal, List_9d3dd2b9]);

        public static readonly PineValue List_e25acbcc =
            PineValue.List(
                [Blob_Str_equal, List_a228d941]);

        public static readonly PineValue List_da91fc98 =
            PineValue.List(
                [Blob_Str_equal, List_dbd07d5a]);

        public static readonly PineValue List_b910054d =
            PineValue.List(
                [Blob_Str_KernelApplication, List_c3c9441c]);

        public static readonly PineValue List_89dda7ec =
            PineValue.List(
                [Blob_Str_KernelApplication, List_118b39c9]);

        public static readonly PineValue List_bb3968df =
            PineValue.List(
                [Blob_Str_KernelApplication, List_b8872664]);

        public static readonly PineValue List_1df81331 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_b21cc8a5]);

        public static readonly PineValue List_524a6624 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_1822bf9f]);

        public static readonly PineValue List_41b9d240 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_0dd4cf27]);

        public static readonly PineValue List_421486a4 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_3531bee3]);

        public static readonly PineValue List_2302c126 =
            PineValue.List(
                [Blob_Str_KernelApplication, List_9b0e5640]);

        public static readonly PineValue List_71cb9f8d =
            PineValue.List(
                [Blob_Str_KernelApplication, List_e25acbcc]);

        public static readonly PineValue List_890cb2fa =
            PineValue.List(
                [Blob_Str_KernelApplication, List_da91fc98]);

        public static readonly PineValue List_f9e1aa3d =
            PineValue.List(
                [List_46a1ad58, List_4af1f0ec, List_d64a63d0]);

        public static readonly PineValue List_0bf4cc22 =
            PineValue.List(
                [List_f9e1aa3d]);

        public static readonly PineValue List_699a7e32 =
            PineValue.List(
                [Blob_Str_List, List_0bf4cc22]);

        public static readonly PineValue List_f4f41c08 =
            PineValue.List(
                [List_a5cbaf18, List_699a7e32]);

        public static readonly PineValue List_90715299 =
            PineValue.List(
                [List_f4f41c08]);

        public static readonly PineValue List_736d22f6 =
            PineValue.List(
                [List_c02d27c5, List_4af1f0ec, List_d64a63d0]);

        public static readonly PineValue List_0fb0703b =
            PineValue.List(
                [List_529fb4ab, List_4af1f0ec, List_d64a63d0]);

        public static readonly PineValue List_48179cb9 =
            PineValue.List(
                [List_87ef138d, List_4af1f0ec, List_d64a63d0]);

        public static readonly PineValue List_9f9f2240 =
            PineValue.List(
                [List_b2dcfd38, List_4af1f0ec, List_d64a63d0]);

        public static readonly PineValue List_6b1768ca =
            PineValue.List(
                [List_8a3b6b92, List_4af1f0ec, List_d64a63d0]);

        public static readonly PineValue List_7e668ec6 =
            PineValue.List(
                [List_5810dc4f, List_4af1f0ec, List_d64a63d0]);

        public static readonly PineValue List_467b9aaf =
            PineValue.List(
                [List_6de702a0, List_4af1f0ec, List_d64a63d0]);

        public static readonly PineValue List_f68b7a8e =
            PineValue.List(
                [List_7c0e13ba, List_4af1f0ec, List_d64a63d0]);

        public static readonly PineValue List_879db3dc =
            PineValue.List(
                [List_e5771b61, List_4af1f0ec, List_d64a63d0]);

        public static readonly PineValue List_f6c49b44 =
            PineValue.List(
                [List_736d22f6]);

        public static readonly PineValue List_ceccab00 =
            PineValue.List(
                [List_0fb0703b]);

        public static readonly PineValue List_82b59ab4 =
            PineValue.List(
                [List_48179cb9]);

        public static readonly PineValue List_c2fd9eef =
            PineValue.List(
                [List_9f9f2240]);

        public static readonly PineValue List_a8fc1e46 =
            PineValue.List(
                [List_6b1768ca]);

        public static readonly PineValue List_f9eca326 =
            PineValue.List(
                [List_7e668ec6]);

        public static readonly PineValue List_c3977bc0 =
            PineValue.List(
                [List_467b9aaf]);

        public static readonly PineValue List_ab7df382 =
            PineValue.List(
                [List_f68b7a8e]);

        public static readonly PineValue List_a1400d1d =
            PineValue.List(
                [List_879db3dc]);

        public static readonly PineValue List_e160db8a =
            PineValue.List(
                [Blob_Str_List, List_90715299]);

        public static readonly PineValue List_ea53280f =
            PineValue.List(
                [Blob_Str_List, List_f6c49b44]);

        public static readonly PineValue List_df7253cc =
            PineValue.List(
                [Blob_Str_List, List_ceccab00]);

        public static readonly PineValue List_d974e64d =
            PineValue.List(
                [Blob_Str_List, List_82b59ab4]);

        public static readonly PineValue List_4b6778fd =
            PineValue.List(
                [Blob_Str_List, List_c2fd9eef]);

        public static readonly PineValue List_08ee34ed =
            PineValue.List(
                [Blob_Str_List, List_a8fc1e46]);

        public static readonly PineValue List_4deb8d08 =
            PineValue.List(
                [Blob_Str_List, List_f9eca326]);

        public static readonly PineValue List_d91765b4 =
            PineValue.List(
                [Blob_Str_List, List_c3977bc0]);

        public static readonly PineValue List_bab50718 =
            PineValue.List(
                [Blob_Str_List, List_ab7df382]);

        public static readonly PineValue List_6c2dcb86 =
            PineValue.List(
                [Blob_Str_List, List_a1400d1d]);

        public static readonly PineValue List_dc640ce9 =
            PineValue.List(
                [List_a5cbaf18, List_ea53280f]);

        public static readonly PineValue List_6652fd23 =
            PineValue.List(
                [List_a5cbaf18, List_df7253cc]);

        public static readonly PineValue List_63e64de1 =
            PineValue.List(
                [List_a5cbaf18, List_d974e64d]);

        public static readonly PineValue List_7972a2c3 =
            PineValue.List(
                [List_a5cbaf18, List_4b6778fd]);

        public static readonly PineValue List_bb868b30 =
            PineValue.List(
                [List_a5cbaf18, List_08ee34ed]);

        public static readonly PineValue List_1c81b9ab =
            PineValue.List(
                [List_a5cbaf18, List_4deb8d08]);

        public static readonly PineValue List_ca16f9cc =
            PineValue.List(
                [List_a5cbaf18, List_d91765b4]);

        public static readonly PineValue List_b8985418 =
            PineValue.List(
                [List_a5cbaf18, List_bab50718]);

        public static readonly PineValue List_f0c52223 =
            PineValue.List(
                [List_a5cbaf18, List_6c2dcb86]);

        public static readonly PineValue List_a964e162 =
            PineValue.List(
                [List_dc640ce9]);

        public static readonly PineValue List_fb2404d9 =
            PineValue.List(
                [List_6652fd23]);

        public static readonly PineValue List_659f2349 =
            PineValue.List(
                [List_63e64de1]);

        public static readonly PineValue List_e45eb830 =
            PineValue.List(
                [List_7972a2c3]);

        public static readonly PineValue List_c786afd9 =
            PineValue.List(
                [List_bb868b30]);

        public static readonly PineValue List_dd5e6978 =
            PineValue.List(
                [List_1c81b9ab]);

        public static readonly PineValue List_e2e18afe =
            PineValue.List(
                [List_ca16f9cc]);

        public static readonly PineValue List_08e0a6f3 =
            PineValue.List(
                [List_b8985418]);

        public static readonly PineValue List_bea9813b =
            PineValue.List(
                [List_f0c52223]);

        public static readonly PineValue List_b74643b3 =
            PineValue.List(
                [List_976730e9, List_e160db8a]);

        public static readonly PineValue List_56557b82 =
            PineValue.List(
                [Blob_Str_List, List_a964e162]);

        public static readonly PineValue List_82be7865 =
            PineValue.List(
                [Blob_Str_List, List_fb2404d9]);

        public static readonly PineValue List_755b55d5 =
            PineValue.List(
                [Blob_Str_List, List_659f2349]);

        public static readonly PineValue List_05bb3fb5 =
            PineValue.List(
                [Blob_Str_List, List_e45eb830]);

        public static readonly PineValue List_5f917418 =
            PineValue.List(
                [Blob_Str_List, List_c786afd9]);

        public static readonly PineValue List_b4bccc04 =
            PineValue.List(
                [Blob_Str_List, List_dd5e6978]);

        public static readonly PineValue List_b31ae109 =
            PineValue.List(
                [Blob_Str_List, List_e2e18afe]);

        public static readonly PineValue List_d214f9d1 =
            PineValue.List(
                [Blob_Str_List, List_08e0a6f3]);

        public static readonly PineValue List_fb4f036b =
            PineValue.List(
                [Blob_Str_List, List_bea9813b]);

        public static readonly PineValue List_9abec2dd =
            PineValue.List(
                [Blob_Str_ParseAndEval, List_b74643b3]);

        public static readonly PineValue List_d6a98ab4 =
            PineValue.List(
                [List_976730e9, List_56557b82]);

        public static readonly PineValue List_784d1ff6 =
            PineValue.List(
                [List_976730e9, List_82be7865]);

        public static readonly PineValue List_24779daa =
            PineValue.List(
                [List_976730e9, List_755b55d5]);

        public static readonly PineValue List_5ac6fcbb =
            PineValue.List(
                [List_976730e9, List_05bb3fb5]);

        public static readonly PineValue List_9e9d518b =
            PineValue.List(
                [List_976730e9, List_5f917418]);

        public static readonly PineValue List_870f1278 =
            PineValue.List(
                [List_976730e9, List_b4bccc04]);

        public static readonly PineValue List_49306f10 =
            PineValue.List(
                [List_976730e9, List_b31ae109]);

        public static readonly PineValue List_1aa18ee7 =
            PineValue.List(
                [List_976730e9, List_d214f9d1]);

        public static readonly PineValue List_a905eec7 =
            PineValue.List(
                [List_976730e9, List_fb4f036b]);

        public static readonly PineValue List_dfc8da5f =
            PineValue.List(
                [Blob_Str_ParseAndEval, List_d6a98ab4]);

        public static readonly PineValue List_543856fc =
            PineValue.List(
                [Blob_Str_ParseAndEval, List_784d1ff6]);

        public static readonly PineValue List_71ac3637 =
            PineValue.List(
                [Blob_Str_ParseAndEval, List_24779daa]);

        public static readonly PineValue List_2bdc172b =
            PineValue.List(
                [Blob_Str_ParseAndEval, List_5ac6fcbb]);

        public static readonly PineValue List_1fca4f4c =
            PineValue.List(
                [Blob_Str_ParseAndEval, List_9e9d518b]);

        public static readonly PineValue List_7ed08cae =
            PineValue.List(
                [Blob_Str_ParseAndEval, List_870f1278]);

        public static readonly PineValue List_25e77689 =
            PineValue.List(
                [Blob_Str_ParseAndEval, List_49306f10]);

        public static readonly PineValue List_29bfa820 =
            PineValue.List(
                [Blob_Str_ParseAndEval, List_1aa18ee7]);

        public static readonly PineValue List_735859ac =
            PineValue.List(
                [Blob_Str_ParseAndEval, List_a905eec7]);

        public static readonly PineValue List_8e774fb4 =
            PineValue.List(
                [List_b910054d, List_2966387f, List_735859ac]);

        public static readonly PineValue List_896090dc =
            PineValue.List(
                [Blob_Str_Conditional, List_8e774fb4]);

        public static readonly PineValue List_e96b749e =
            PineValue.List(
                [List_89dda7ec, List_896090dc, List_29bfa820]);

        public static readonly PineValue List_bb9d158b =
            PineValue.List(
                [Blob_Str_Conditional, List_e96b749e]);

        public static readonly PineValue List_1df2444b =
            PineValue.List(
                [List_bb3968df, List_bb9d158b, List_25e77689]);

        public static readonly PineValue List_30941767 =
            PineValue.List(
                [Blob_Str_Conditional, List_1df2444b]);

        public static readonly PineValue List_a02735d9 =
            PineValue.List(
                [List_1df81331, List_30941767, List_7ed08cae]);

        public static readonly PineValue List_c5a32dac =
            PineValue.List(
                [Blob_Str_Conditional, List_a02735d9]);

        public static readonly PineValue List_4fe84dd0 =
            PineValue.List(
                [List_524a6624, List_c5a32dac, List_1fca4f4c]);

        public static readonly PineValue List_2e47160a =
            PineValue.List(
                [Blob_Str_Conditional, List_4fe84dd0]);

        public static readonly PineValue List_1d7fb539 =
            PineValue.List(
                [List_41b9d240, List_2e47160a, List_2bdc172b]);

        public static readonly PineValue List_40810f3e =
            PineValue.List(
                [Blob_Str_Conditional, List_1d7fb539]);

        public static readonly PineValue List_94e5fb7d =
            PineValue.List(
                [List_421486a4, List_40810f3e, List_71ac3637]);

        public static readonly PineValue List_131a122b =
            PineValue.List(
                [Blob_Str_Conditional, List_94e5fb7d]);

        public static readonly PineValue List_2f80fb96 =
            PineValue.List(
                [List_2302c126, List_131a122b, List_543856fc]);

        public static readonly PineValue List_27d57f0b =
            PineValue.List(
                [Blob_Str_Conditional, List_2f80fb96]);

        public static readonly PineValue List_5b8a853f =
            PineValue.List(
                [List_71cb9f8d, List_27d57f0b, List_dfc8da5f]);

        public static readonly PineValue List_59347aa5 =
            PineValue.List(
                [Blob_Str_Conditional, List_5b8a853f]);

        public static readonly PineValue List_fd4beee6 =
            PineValue.List(
                [List_890cb2fa, List_59347aa5, List_9abec2dd]);

        public static readonly PineValue List_f1cd4f95 =
            PineValue.List(
                [Blob_Str_Conditional, List_fd4beee6]);
    }


    class Common
    {
        public static PineValue ValueFromPathInValueOrEmptyList(
            PineValue environment,
            System.ReadOnlySpan<int> path)
        {
            var currentNode = environment;

            for (var i = 0; i < path.Length; i++)
            {
                if (currentNode is not PineValue.ListValue listValue)
                    return PineValue.EmptyList;

                var skipCount = path[i];

                if (skipCount >= listValue.Items.Length)
                    return PineValue.EmptyList;

                currentNode = listValue.Items.Span[skipCount < 0 ? 0 : skipCount];
            }

            return currentNode;
        }
    }

}
