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

                public static readonly PineValue Blob_Str_Eval = StringEncoding.ValueFromString("Eval");

                public static readonly PineValue Blob_Str_List = StringEncoding.ValueFromString("List");

                public static readonly PineValue Blob_Str_head = StringEncoding.ValueFromString("head");

                public static readonly PineValue Blob_Str_skip = StringEncoding.ValueFromString("skip");

                public static readonly PineValue Blob_Str_take = StringEncoding.ValueFromString("take");

                public static readonly PineValue Blob_Str_equal = StringEncoding.ValueFromString("equal");

                public static readonly PineValue Blob_Str_Litral = StringEncoding.ValueFromString("Litral");

                public static readonly PineValue Blob_Str_String = StringEncoding.ValueFromString("String");

                public static readonly PineValue Blob_Str_length = StringEncoding.ValueFromString("length");

                public static readonly PineValue Blob_Str_Builtin = StringEncoding.ValueFromString("Builtin");

                public static readonly PineValue Blob_Str_int_add = StringEncoding.ValueFromString("int_add");

                public static readonly PineValue Blob_Str_int_mul = StringEncoding.ValueFromString("int_mul");

                public static readonly PineValue Blob_Str_Condition = StringEncoding.ValueFromString("Condition");

                public static readonly PineValue Blob_Str_Environment = StringEncoding.ValueFromString("Environment");

                public static readonly PineValue Blob_cfca8ee1 = StringEncoding.ValueFromString("Expecting a digit");

                public static readonly PineValue List_Single_Blob_Int_0 = PineValue.List([IntegerEncoding.EncodeSignedInteger(0)]);

                public static readonly PineValue List_c3304aab = PineValue.List([Blob_Str_Ok, List_Single_Blob_Int_0]);

                public static readonly PineValue List_476058cb = PineValue.List([Blob_Str_Litral, PineValue.EmptyList]);

                public static readonly PineValue List_42fd7a4b = PineValue.List([Blob_Str_Litral, PineKernelValues.FalseValue]);

                public static readonly PineValue List_d289000a = PineValue.List([Blob_Str_Litral, Blob_Int_neg_1]);

                public static readonly PineValue List_c2d2b628 = PineValue.List([Blob_Str_Litral, Blob_Int_1]);

                public static readonly PineValue List_9a4bf642 = PineValue.List([Blob_Str_Litral, Blob_Int_2]);

                public static readonly PineValue List_245be8ca = PineValue.List([Blob_Str_Litral, Blob_Int_3]);

                public static readonly PineValue List_07612f6b = PineValue.List([Blob_Str_Litral, Blob_Int_4]);

                public static readonly PineValue List_e76ffc54 = PineValue.List([Blob_Str_Litral, Blob_Int_5]);

                public static readonly PineValue List_015a080e = PineValue.List([Blob_Str_Litral, Blob_Int_6]);

                public static readonly PineValue List_cff5f9a2 = PineValue.List([Blob_Str_Litral, Blob_Int_7]);

                public static readonly PineValue List_7dce08cb = PineValue.List([Blob_Str_Litral, Blob_Int_8]);

                public static readonly PineValue List_6dafb618 = PineValue.List([Blob_Str_Litral, Blob_Int_9]);

                public static readonly PineValue List_939614df = PineValue.List([Blob_Str_Litral, Blob_Int_10]);

                public static readonly PineValue List_d6b2081d = PineValue.List([Blob_Str_Litral, Blob_Char_hyphen]);

                public static readonly PineValue List_93ecb8c9 = PineValue.List([Blob_Str_Litral, Blob_Char_digit_0]);

                public static readonly PineValue List_4504664a = PineValue.List([Blob_Str_Litral, Blob_Char_digit_1]);

                public static readonly PineValue List_1774d30c = PineValue.List([Blob_Str_Litral, Blob_Char_digit_2]);

                public static readonly PineValue List_397ea59d = PineValue.List([Blob_Str_Litral, Blob_Char_digit_3]);

                public static readonly PineValue List_562fdb27 = PineValue.List([Blob_Str_Litral, Blob_Char_digit_4]);

                public static readonly PineValue List_1f4525bf = PineValue.List([Blob_Str_Litral, Blob_Char_digit_5]);

                public static readonly PineValue List_72cee054 = PineValue.List([Blob_Str_Litral, Blob_Char_digit_6]);

                public static readonly PineValue List_671a63f9 = PineValue.List([Blob_Str_Litral, Blob_Char_digit_7]);

                public static readonly PineValue List_c2482749 = PineValue.List([Blob_Str_Litral, Blob_Char_digit_8]);

                public static readonly PineValue List_ead096c4 = PineValue.List([Blob_Str_Litral, Blob_Char_digit_9]);

                public static readonly PineValue List_921b24bb = PineValue.List([Blob_Str_Litral, Blob_Str_Ok]);

                public static readonly PineValue List_fc06676b = PineValue.List([Blob_Str_Litral, Blob_Str_Err]);

                public static readonly PineValue List_f9b6ed40 = PineValue.List([Blob_Str_Litral, List_c3304aab]);

                public static readonly PineValue List_Single_Blob_Str_Environment = PineValue.List([Blob_Str_Environment]);

                public static readonly PineValue List_Single_Blob_cfca8ee1 = PineValue.List([Blob_cfca8ee1]);

                public static readonly PineValue List_626e9e10 =
                    PineValue.List([Blob_Str_List, List_c2d2b628, List_Single_Blob_Str_Environment]);

                public static readonly PineValue List_e6d95047 =
                    PineValue.List([Blob_Str_List, List_9a4bf642, List_Single_Blob_Str_Environment]);

                public static readonly PineValue List_4fe88b09 =
                    PineValue.List([Blob_Str_List, List_245be8ca, List_Single_Blob_Str_Environment]);

                public static readonly PineValue List_65eb76d7 =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_head, List_Single_Blob_Str_Environment]);

                public static readonly PineValue List_708c3c98 = PineValue.List([Blob_Str_String, List_Single_Blob_cfca8ee1]);

                public static readonly PineValue List_Single_List_708c3c98 = PineValue.List([List_708c3c98]);

                public static readonly PineValue List_ae45bd54 = PineValue.List([Blob_Str_Err, List_Single_List_708c3c98]);

                public static readonly PineValue List_a3e45e10 = PineValue.List([Blob_Str_Litral, List_ae45bd54]);

                public static readonly PineValue List_854f7611 = PineValue.List([Blob_Str_Builtin, Blob_Str_head, List_65eb76d7]);

                public static readonly PineValue List_a6554f03 = PineValue.List([Blob_Str_Builtin, Blob_Str_skip, List_626e9e10]);

                public static readonly PineValue List_7a36ad92 = PineValue.List([Blob_Str_Builtin, Blob_Str_skip, List_e6d95047]);

                public static readonly PineValue List_f0f99078 = PineValue.List([Blob_Str_Builtin, Blob_Str_skip, List_4fe88b09]);

                public static readonly PineValue List_e4f998f4 = PineValue.List([Blob_Str_Builtin, Blob_Str_head, List_a6554f03]);

                public static readonly PineValue List_076751a5 = PineValue.List([Blob_Str_Builtin, Blob_Str_head, List_7a36ad92]);

                public static readonly PineValue List_5f6bd89b = PineValue.List([Blob_Str_Builtin, Blob_Str_head, List_f0f99078]);

                public static readonly PineValue List_c17c4e24 = PineValue.List([Blob_Str_List, List_e4f998f4]);

                public static readonly PineValue List_a4a8c1fa = PineValue.List([Blob_Str_List, List_e4f998f4, List_939614df]);

                public static readonly PineValue List_8cfe2574 = PineValue.List([Blob_Str_List, List_076751a5, List_07612f6b]);

                public static readonly PineValue List_a9d2bae4 = PineValue.List([Blob_Str_List, List_5f6bd89b, List_07612f6b]);

                public static readonly PineValue List_66c4afc3 = PineValue.List([Blob_Str_List, List_921b24bb, List_c17c4e24]);

                public static readonly PineValue List_c105b7df =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_add, List_8cfe2574]);

                public static readonly PineValue List_31c4175c =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_add, List_a9d2bae4]);

                public static readonly PineValue List_c35c17da =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_mul, List_a4a8c1fa]);

                public static readonly PineValue List_5ac0b32e = PineValue.List([Blob_Str_List, List_c35c17da, List_c2d2b628]);

                public static readonly PineValue List_ebbf5345 = PineValue.List([Blob_Str_List, List_c35c17da, List_9a4bf642]);

                public static readonly PineValue List_a9076b27 = PineValue.List([Blob_Str_List, List_c35c17da, List_245be8ca]);

                public static readonly PineValue List_7df6b06a = PineValue.List([Blob_Str_List, List_c35c17da, List_07612f6b]);

                public static readonly PineValue List_70eafbae = PineValue.List([Blob_Str_List, List_c35c17da, List_e76ffc54]);

                public static readonly PineValue List_4e5a9c51 = PineValue.List([Blob_Str_List, List_c35c17da, List_015a080e]);

                public static readonly PineValue List_1f546f66 = PineValue.List([Blob_Str_List, List_c35c17da, List_cff5f9a2]);

                public static readonly PineValue List_01ebf17b = PineValue.List([Blob_Str_List, List_c35c17da, List_7dce08cb]);

                public static readonly PineValue List_a8f5e1c1 = PineValue.List([Blob_Str_List, List_c35c17da, List_6dafb618]);

                public static readonly PineValue List_9117e787 = PineValue.List([Blob_Str_List, List_a3e45e10, List_076751a5]);

                public static readonly PineValue List_372a5e01 = PineValue.List([Blob_Str_List, List_f9b6ed40, List_c105b7df]);

                public static readonly PineValue List_499507bf = PineValue.List([Blob_Str_List, List_076751a5, List_e4f998f4]);

                public static readonly PineValue List_87999c0e = PineValue.List([Blob_Str_List, List_5f6bd89b, List_076751a5]);

                public static readonly PineValue List_23ff9579 =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_add, List_5ac0b32e]);

                public static readonly PineValue List_72127b5c =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_add, List_ebbf5345]);

                public static readonly PineValue List_b08052da =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_add, List_a9076b27]);

                public static readonly PineValue List_5f644871 =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_add, List_7df6b06a]);

                public static readonly PineValue List_9bb5bff4 =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_add, List_70eafbae]);

                public static readonly PineValue List_52188b4c =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_add, List_4e5a9c51]);

                public static readonly PineValue List_b16492a8 =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_add, List_1f546f66]);

                public static readonly PineValue List_e509a76a =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_add, List_01ebf17b]);

                public static readonly PineValue List_3c9216fe =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_add, List_a8f5e1c1]);

                public static readonly PineValue List_2fa7933b =
                    PineValue.List([Blob_Str_List, List_476058cb, List_e4f998f4, List_076751a5]);

                public static readonly PineValue List_4fc2bfdf = PineValue.List([Blob_Str_Builtin, Blob_Str_skip, List_499507bf]);

                public static readonly PineValue List_f7af2fe8 = PineValue.List([Blob_Str_Builtin, Blob_Str_skip, List_87999c0e]);

                public static readonly PineValue List_f32481e1 = PineValue.List([Blob_Str_List, List_66c4afc3, List_5f6bd89b]);

                public static readonly PineValue List_aae98c3b = PineValue.List([Blob_Str_List, List_07612f6b, List_4fc2bfdf]);

                public static readonly PineValue List_072231f7 = PineValue.List([Blob_Str_List, List_07612f6b, List_f7af2fe8]);

                public static readonly PineValue List_fecc37ce =
                    PineValue.List([Blob_Str_List, List_476058cb, List_e4f998f4, List_c105b7df]);

                public static readonly PineValue List_2fcbc670 = PineValue.List([Blob_Str_Builtin, Blob_Str_take, List_aae98c3b]);

                public static readonly PineValue List_f6d94ac6 = PineValue.List([Blob_Str_Builtin, Blob_Str_take, List_072231f7]);

                public static readonly PineValue List_797c587b = PineValue.List([Blob_Str_List, List_2fcbc670, List_d6b2081d]);

                public static readonly PineValue List_f968752b = PineValue.List([Blob_Str_List, List_2fcbc670, List_93ecb8c9]);

                public static readonly PineValue List_d4f5536a = PineValue.List([Blob_Str_List, List_2fcbc670, List_4504664a]);

                public static readonly PineValue List_72caf17e = PineValue.List([Blob_Str_List, List_2fcbc670, List_1774d30c]);

                public static readonly PineValue List_c1c15e26 = PineValue.List([Blob_Str_List, List_2fcbc670, List_397ea59d]);

                public static readonly PineValue List_a1ed0f2b = PineValue.List([Blob_Str_List, List_2fcbc670, List_562fdb27]);

                public static readonly PineValue List_1e4367bd = PineValue.List([Blob_Str_List, List_2fcbc670, List_1f4525bf]);

                public static readonly PineValue List_5487d924 = PineValue.List([Blob_Str_List, List_2fcbc670, List_72cee054]);

                public static readonly PineValue List_421c2c6f = PineValue.List([Blob_Str_List, List_2fcbc670, List_671a63f9]);

                public static readonly PineValue List_917a5827 = PineValue.List([Blob_Str_List, List_2fcbc670, List_c2482749]);

                public static readonly PineValue List_48bd0096 = PineValue.List([Blob_Str_List, List_2fcbc670, List_ead096c4]);

                public static readonly PineValue List_901e5c93 = PineValue.List([Blob_Str_List, List_f6d94ac6, List_93ecb8c9]);

                public static readonly PineValue List_132197a1 = PineValue.List([Blob_Str_List, List_f6d94ac6, List_4504664a]);

                public static readonly PineValue List_1cba1c51 = PineValue.List([Blob_Str_List, List_f6d94ac6, List_1774d30c]);

                public static readonly PineValue List_f81d14ef = PineValue.List([Blob_Str_List, List_f6d94ac6, List_397ea59d]);

                public static readonly PineValue List_ac8c2c61 = PineValue.List([Blob_Str_List, List_f6d94ac6, List_562fdb27]);

                public static readonly PineValue List_e350d5ef = PineValue.List([Blob_Str_List, List_f6d94ac6, List_1f4525bf]);

                public static readonly PineValue List_31f173da = PineValue.List([Blob_Str_List, List_f6d94ac6, List_72cee054]);

                public static readonly PineValue List_444169b5 = PineValue.List([Blob_Str_List, List_f6d94ac6, List_671a63f9]);

                public static readonly PineValue List_7e754eb5 = PineValue.List([Blob_Str_List, List_f6d94ac6, List_c2482749]);

                public static readonly PineValue List_a51b6851 = PineValue.List([Blob_Str_List, List_f6d94ac6, List_ead096c4]);

                public static readonly PineValue List_af975ca0 = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_797c587b]);

                public static readonly PineValue List_cbd1266a = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_f968752b]);

                public static readonly PineValue List_c9300bee = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_d4f5536a]);

                public static readonly PineValue List_c13febb0 = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_72caf17e]);

                public static readonly PineValue List_851a342d = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_c1c15e26]);

                public static readonly PineValue List_3312740a = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_a1ed0f2b]);

                public static readonly PineValue List_214b8649 = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_1e4367bd]);

                public static readonly PineValue List_2cf23da2 = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_5487d924]);

                public static readonly PineValue List_9733a7b0 = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_421c2c6f]);

                public static readonly PineValue List_e6ee7110 = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_917a5827]);

                public static readonly PineValue List_3285fa15 = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_48bd0096]);

                public static readonly PineValue List_7ef07c84 = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_901e5c93]);

                public static readonly PineValue List_e71f9935 = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_132197a1]);

                public static readonly PineValue List_e9042276 = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_1cba1c51]);

                public static readonly PineValue List_5d629e06 = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_f81d14ef]);

                public static readonly PineValue List_6524ad03 = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_ac8c2c61]);

                public static readonly PineValue List_4ccc3fdd = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_e350d5ef]);

                public static readonly PineValue List_3afceef3 = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_31f173da]);

                public static readonly PineValue List_6ca1069c = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_444169b5]);

                public static readonly PineValue List_1613f4cb = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_7e754eb5]);

                public static readonly PineValue List_adf15c2f = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_a51b6851]);

                public static readonly PineValue List_53555e2b =
                    PineValue.List([Blob_Str_List, List_65eb76d7, List_c35c17da, List_076751a5, List_31c4175c]);

                public static readonly PineValue List_a2b6043c =
                    PineValue.List([Blob_Str_List, List_65eb76d7, List_23ff9579, List_076751a5, List_31c4175c]);

                public static readonly PineValue List_99bfb80e =
                    PineValue.List([Blob_Str_List, List_65eb76d7, List_72127b5c, List_076751a5, List_31c4175c]);

                public static readonly PineValue List_c4d0d190 =
                    PineValue.List([Blob_Str_List, List_65eb76d7, List_b08052da, List_076751a5, List_31c4175c]);

                public static readonly PineValue List_847e06ee =
                    PineValue.List([Blob_Str_List, List_65eb76d7, List_5f644871, List_076751a5, List_31c4175c]);

                public static readonly PineValue List_d62cce01 =
                    PineValue.List([Blob_Str_List, List_65eb76d7, List_9bb5bff4, List_076751a5, List_31c4175c]);

                public static readonly PineValue List_ce883b5e =
                    PineValue.List([Blob_Str_List, List_65eb76d7, List_52188b4c, List_076751a5, List_31c4175c]);

                public static readonly PineValue List_afd14bc1 =
                    PineValue.List([Blob_Str_List, List_65eb76d7, List_b16492a8, List_076751a5, List_31c4175c]);

                public static readonly PineValue List_f95cccdb =
                    PineValue.List([Blob_Str_List, List_65eb76d7, List_e509a76a, List_076751a5, List_31c4175c]);

                public static readonly PineValue List_d0df29ee =
                    PineValue.List([Blob_Str_List, List_65eb76d7, List_3c9216fe, List_076751a5, List_31c4175c]);

                public static readonly PineValue List_92043aa2 = PineValue.List([Blob_Str_Eval, List_854f7611, List_53555e2b]);

                public static readonly PineValue List_077e2911 = PineValue.List([Blob_Str_Eval, List_854f7611, List_a2b6043c]);

                public static readonly PineValue List_e195a162 = PineValue.List([Blob_Str_Eval, List_854f7611, List_99bfb80e]);

                public static readonly PineValue List_fa11e587 = PineValue.List([Blob_Str_Eval, List_854f7611, List_c4d0d190]);

                public static readonly PineValue List_dd8a0d1b = PineValue.List([Blob_Str_Eval, List_854f7611, List_847e06ee]);

                public static readonly PineValue List_db586353 = PineValue.List([Blob_Str_Eval, List_854f7611, List_d62cce01]);

                public static readonly PineValue List_11ee37d8 = PineValue.List([Blob_Str_Eval, List_854f7611, List_ce883b5e]);

                public static readonly PineValue List_5305922b = PineValue.List([Blob_Str_Eval, List_854f7611, List_afd14bc1]);

                public static readonly PineValue List_a5b897a8 = PineValue.List([Blob_Str_Eval, List_854f7611, List_f95cccdb]);

                public static readonly PineValue List_8064dbcc = PineValue.List([Blob_Str_Eval, List_854f7611, List_d0df29ee]);

                public static readonly PineValue List_fb3fcbd1 =
                    PineValue.List([Blob_Str_Condition, List_adf15c2f, List_f32481e1, List_8064dbcc]);

                public static readonly PineValue List_8c4cab36 =
                    PineValue.List([Blob_Str_Condition, List_1613f4cb, List_fb3fcbd1, List_a5b897a8]);

                public static readonly PineValue List_d995e7b6 =
                    PineValue.List([Blob_Str_Condition, List_6ca1069c, List_8c4cab36, List_5305922b]);

                public static readonly PineValue List_265ea824 =
                    PineValue.List([Blob_Str_Condition, List_3afceef3, List_d995e7b6, List_11ee37d8]);

                public static readonly PineValue List_29150fca =
                    PineValue.List([Blob_Str_Condition, List_4ccc3fdd, List_265ea824, List_db586353]);

                public static readonly PineValue List_a1d2a94a =
                    PineValue.List([Blob_Str_Condition, List_6524ad03, List_29150fca, List_dd8a0d1b]);

                public static readonly PineValue List_7c393051 =
                    PineValue.List([Blob_Str_Condition, List_5d629e06, List_a1d2a94a, List_fa11e587]);

                public static readonly PineValue List_1099e5aa =
                    PineValue.List([Blob_Str_Condition, List_e9042276, List_7c393051, List_e195a162]);

                public static readonly PineValue List_4be56938 =
                    PineValue.List([Blob_Str_Condition, List_e71f9935, List_1099e5aa, List_077e2911]);

                public static readonly PineValue List_a0d6a020 =
                    PineValue.List([Blob_Str_Condition, List_7ef07c84, List_4be56938, List_92043aa2]);

                public static readonly PineValue List_Single_List_a0d6a020 = PineValue.List([List_a0d6a020]);

                public static readonly PineValue List_37952f4c = PineValue.List([Blob_Str_Litral, List_a0d6a020]);

                public static readonly PineValue List_9d845d0f = PineValue.List([Blob_Str_Litral, List_Single_List_a0d6a020]);

                public static readonly PineValue List_6269b9a5 =
                    PineValue.List([Blob_Str_List, List_9d845d0f, List_c2d2b628, List_e4f998f4, List_c105b7df]);

                public static readonly PineValue List_95ddf38a =
                    PineValue.List([Blob_Str_List, List_9d845d0f, List_9a4bf642, List_e4f998f4, List_c105b7df]);

                public static readonly PineValue List_da41d58a =
                    PineValue.List([Blob_Str_List, List_9d845d0f, List_245be8ca, List_e4f998f4, List_c105b7df]);

                public static readonly PineValue List_a94592c1 =
                    PineValue.List([Blob_Str_List, List_9d845d0f, List_07612f6b, List_e4f998f4, List_c105b7df]);

                public static readonly PineValue List_407ea823 =
                    PineValue.List([Blob_Str_List, List_9d845d0f, List_e76ffc54, List_e4f998f4, List_c105b7df]);

                public static readonly PineValue List_179e3f40 =
                    PineValue.List([Blob_Str_List, List_9d845d0f, List_015a080e, List_e4f998f4, List_c105b7df]);

                public static readonly PineValue List_f8e575a8 =
                    PineValue.List([Blob_Str_List, List_9d845d0f, List_cff5f9a2, List_e4f998f4, List_c105b7df]);

                public static readonly PineValue List_f645b61c =
                    PineValue.List([Blob_Str_List, List_9d845d0f, List_7dce08cb, List_e4f998f4, List_c105b7df]);

                public static readonly PineValue List_3a3bbc68 =
                    PineValue.List([Blob_Str_List, List_9d845d0f, List_6dafb618, List_e4f998f4, List_c105b7df]);

                public static readonly PineValue List_cb70e0e3 = PineValue.List([Blob_Str_Eval, List_37952f4c, List_6269b9a5]);

                public static readonly PineValue List_c0bc3577 = PineValue.List([Blob_Str_Eval, List_37952f4c, List_95ddf38a]);

                public static readonly PineValue List_7ff2057d = PineValue.List([Blob_Str_Eval, List_37952f4c, List_da41d58a]);

                public static readonly PineValue List_de40f90f = PineValue.List([Blob_Str_Eval, List_37952f4c, List_a94592c1]);

                public static readonly PineValue List_3a8f13e6 = PineValue.List([Blob_Str_Eval, List_37952f4c, List_407ea823]);

                public static readonly PineValue List_cd24c538 = PineValue.List([Blob_Str_Eval, List_37952f4c, List_179e3f40]);

                public static readonly PineValue List_cdc3da33 = PineValue.List([Blob_Str_Eval, List_37952f4c, List_f8e575a8]);

                public static readonly PineValue List_5e82b48f = PineValue.List([Blob_Str_Eval, List_37952f4c, List_f645b61c]);

                public static readonly PineValue List_8effd8c8 = PineValue.List([Blob_Str_Eval, List_37952f4c, List_3a3bbc68]);

                public static readonly PineValue List_8af396f4 =
                    PineValue.List([Blob_Str_Condition, List_3285fa15, List_9117e787, List_8effd8c8]);

                public static readonly PineValue List_29bbcf04 =
                    PineValue.List([Blob_Str_Condition, List_e6ee7110, List_8af396f4, List_5e82b48f]);

                public static readonly PineValue List_06b7d2c8 =
                    PineValue.List([Blob_Str_Condition, List_9733a7b0, List_29bbcf04, List_cdc3da33]);

                public static readonly PineValue List_bbe36d08 =
                    PineValue.List([Blob_Str_Condition, List_2cf23da2, List_06b7d2c8, List_cd24c538]);

                public static readonly PineValue List_832a54c9 =
                    PineValue.List([Blob_Str_Condition, List_214b8649, List_bbe36d08, List_3a8f13e6]);

                public static readonly PineValue List_2bb5191a =
                    PineValue.List([Blob_Str_Condition, List_3312740a, List_832a54c9, List_de40f90f]);

                public static readonly PineValue List_591e533d =
                    PineValue.List([Blob_Str_Condition, List_851a342d, List_2bb5191a, List_7ff2057d]);

                public static readonly PineValue List_797f07c7 =
                    PineValue.List([Blob_Str_Condition, List_c13febb0, List_591e533d, List_c0bc3577]);

                public static readonly PineValue List_99d66b2a =
                    PineValue.List([Blob_Str_Condition, List_c9300bee, List_797f07c7, List_cb70e0e3]);

                public static readonly PineValue List_a2fcf86e =
                    PineValue.List([Blob_Str_Condition, List_cbd1266a, List_99d66b2a, List_372a5e01]);

                public static readonly PineValue List_3446fb0e = PineValue.List([Blob_Str_Litral, List_a2fcf86e]);

                public static readonly PineValue List_8c6c7863 = PineValue.List([Blob_Str_Eval, List_3446fb0e, List_2fa7933b]);

                public static readonly PineValue List_dc604cda = PineValue.List([Blob_Str_Eval, List_3446fb0e, List_fecc37ce]);

                public static readonly PineValue List_b37d2b2a = PineValue.List([Blob_Str_List, List_c2d2b628, List_dc604cda]);

                public static readonly PineValue List_e3abe56e = PineValue.List([Blob_Str_Builtin, Blob_Str_head, List_dc604cda]);

                public static readonly PineValue List_747ff2a0 = PineValue.List([Blob_Str_List, List_c2d2b628, List_e3abe56e]);

                public static readonly PineValue List_e03fa6e9 = PineValue.List([Blob_Str_Builtin, Blob_Str_head, List_e3abe56e]);

                public static readonly PineValue List_a1d5fd48 = PineValue.List([Blob_Str_Builtin, Blob_Str_skip, List_b37d2b2a]);

                public static readonly PineValue List_c9bab74a = PineValue.List([Blob_Str_Builtin, Blob_Str_head, List_a1d5fd48]);

                public static readonly PineValue List_951f3e5a = PineValue.List([Blob_Str_Builtin, Blob_Str_skip, List_747ff2a0]);

                public static readonly PineValue List_6879a5b8 = PineValue.List([Blob_Str_List, List_e03fa6e9, List_921b24bb]);

                public static readonly PineValue List_d74788d8 = PineValue.List([Blob_Str_Builtin, Blob_Str_head, List_951f3e5a]);

                public static readonly PineValue List_cc1d0213 = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_6879a5b8]);

                public static readonly PineValue List_b034d3a7 = PineValue.List([Blob_Str_Builtin, Blob_Str_head, List_d74788d8]);

                public static readonly PineValue List_bdfbd983 = PineValue.List([Blob_Str_Builtin, Blob_Str_length, List_d74788d8]);

                public static readonly PineValue List_26ae2a55 = PineValue.List([Blob_Str_List, List_b034d3a7]);

                public static readonly PineValue List_355baa01 = PineValue.List([Blob_Str_List, List_d289000a, List_b034d3a7]);

                public static readonly PineValue List_0a25880d = PineValue.List([Blob_Str_List, List_bdfbd983, List_c2d2b628]);

                public static readonly PineValue List_d335c588 = PineValue.List([Blob_Str_List, List_fc06676b, List_26ae2a55]);

                public static readonly PineValue List_716722a2 = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_0a25880d]);

                public static readonly PineValue List_3dd6e019 =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_mul, List_355baa01]);

                public static readonly PineValue List_828f23cd = PineValue.List([Blob_Str_List, List_3dd6e019]);

                public static readonly PineValue List_3c200dcd = PineValue.List([Blob_Str_List, List_921b24bb, List_828f23cd]);

                public static readonly PineValue List_dd81866b = PineValue.List([Blob_Str_List, List_d335c588, List_c9bab74a]);

                public static readonly PineValue List_d74bf74b = PineValue.List([Blob_Str_List, List_3c200dcd, List_c9bab74a]);

                public static readonly PineValue List_6c55adb5 =
                    PineValue.List([Blob_Str_Condition, List_716722a2, List_42fd7a4b, List_cc1d0213]);

                public static readonly PineValue List_937261a3 =
                    PineValue.List([Blob_Str_Condition, List_6c55adb5, List_dd81866b, List_d74bf74b]);

                public static readonly PineValue List_88aa7c7c =
                    PineValue.List([Blob_Str_Condition, List_af975ca0, List_8c6c7863, List_937261a3]);
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
