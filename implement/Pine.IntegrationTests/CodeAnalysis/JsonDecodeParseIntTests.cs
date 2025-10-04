using AwesomeAssertions;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.Internal;
using Pine.Core.PopularEncodings;
using Pine.Elm;
using System.Text;
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
        var elmJsonFile =
            """
            {
                "type": "application",
                "source-directories": [
                    "src"
                ],
                "elm-version": "0.19.1",
                "dependencies": {
                    "direct": {
                        "elm/core": "1.0.5"
                    },
                    "indirect": {
                    }
                },
                "test-dependencies": {
                    "direct": {
                    },
                    "indirect": {
                    }
                }
            }
            """;

        var elmModuleText =
            """
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
            
            """;

        var appCodeTree =
            BlobTreeWithStringPath.EmptyTree
            .SetNodeAtPathSorted(
                ["elm.json"],
                BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmJsonFile)))
            .SetNodeAtPathSorted(
                ["src", "Test.elm"],
                BlobTreeWithStringPath.Blob(Encoding.UTF8.GetBytes(elmModuleText)));

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths: [["src", "Test.elm"]],
                skipLowering: true,
                skipFilteringForSourceDirs: false)
            .Extract(err => throw new System.Exception(err));

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(err => throw new System.Exception("Failed parsing interactive environment: " + err));

        var staticProgram =
            CodeAnalysisTestHelper.ParseAsStaticMonomorphicProgramAndCrashOnAnyFailure(
                parsedEnv,
                includeDeclaration:
                declName =>
                {
                    return declName == new DeclQualifiedName(["Test"], "parseInt");
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"
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


            Test.parseUnsignedInt param_1_0 param_1_1 =
                if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_1
                                , param_1_0
                                ]
                            ]
                        , '0'
                        ]
                then
                    [ Ok 0
                    , Pine_kernel.int_add
                        [ param_1_1
                        , 4
                        ]
                    ]

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_1
                                , param_1_0
                                ]
                            ]
                        , '1'
                        ]
                then
                    Test.parseUnsignedIntRec
                        1
                        param_1_0
                        (Pine_kernel.int_add
                            [ param_1_1
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_1
                                , param_1_0
                                ]
                            ]
                        , '2'
                        ]
                then
                    Test.parseUnsignedIntRec
                        2
                        param_1_0
                        (Pine_kernel.int_add
                            [ param_1_1
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_1
                                , param_1_0
                                ]
                            ]
                        , '3'
                        ]
                then
                    Test.parseUnsignedIntRec
                        3
                        param_1_0
                        (Pine_kernel.int_add
                            [ param_1_1
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_1
                                , param_1_0
                                ]
                            ]
                        , '4'
                        ]
                then
                    Test.parseUnsignedIntRec
                        4
                        param_1_0
                        (Pine_kernel.int_add
                            [ param_1_1
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_1
                                , param_1_0
                                ]
                            ]
                        , '5'
                        ]
                then
                    Test.parseUnsignedIntRec
                        5
                        param_1_0
                        (Pine_kernel.int_add
                            [ param_1_1
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_1
                                , param_1_0
                                ]
                            ]
                        , '6'
                        ]
                then
                    Test.parseUnsignedIntRec
                        6
                        param_1_0
                        (Pine_kernel.int_add
                            [ param_1_1
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_1
                                , param_1_0
                                ]
                            ]
                        , '7'
                        ]
                then
                    Test.parseUnsignedIntRec
                        7
                        param_1_0
                        (Pine_kernel.int_add
                            [ param_1_1
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_1
                                , param_1_0
                                ]
                            ]
                        , '8'
                        ]
                then
                    Test.parseUnsignedIntRec
                        8
                        param_1_0
                        (Pine_kernel.int_add
                            [ param_1_1
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_1
                                , param_1_0
                                ]
                            ]
                        , '9'
                        ]
                then
                    Test.parseUnsignedIntRec
                        9
                        param_1_0
                        (Pine_kernel.int_add
                            [ param_1_1
                            , 4
                            ]
                        )

                else
                    [ Err "Expecting a digit"
                    , param_1_1
                    ]


            Test.parseUnsignedIntRec param_1_0 param_1_1 param_1_2 =
                if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_2
                                , param_1_1
                                ]
                            ]
                        , '0'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_mul
                            [ param_1_0
                            , 10
                            ]
                        )
                        param_1_1
                        (Pine_kernel.int_add
                            [ param_1_2
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_2
                                , param_1_1
                                ]
                            ]
                        , '1'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1_0
                                , 10
                                ]
                            , 1
                            ]
                        )
                        param_1_1
                        (Pine_kernel.int_add
                            [ param_1_2
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_2
                                , param_1_1
                                ]
                            ]
                        , '2'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1_0
                                , 10
                                ]
                            , 2
                            ]
                        )
                        param_1_1
                        (Pine_kernel.int_add
                            [ param_1_2
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_2
                                , param_1_1
                                ]
                            ]
                        , '3'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1_0
                                , 10
                                ]
                            , 3
                            ]
                        )
                        param_1_1
                        (Pine_kernel.int_add
                            [ param_1_2
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_2
                                , param_1_1
                                ]
                            ]
                        , '4'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1_0
                                , 10
                                ]
                            , 4
                            ]
                        )
                        param_1_1
                        (Pine_kernel.int_add
                            [ param_1_2
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_2
                                , param_1_1
                                ]
                            ]
                        , '5'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1_0
                                , 10
                                ]
                            , 5
                            ]
                        )
                        param_1_1
                        (Pine_kernel.int_add
                            [ param_1_2
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_2
                                , param_1_1
                                ]
                            ]
                        , '6'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1_0
                                , 10
                                ]
                            , 6
                            ]
                        )
                        param_1_1
                        (Pine_kernel.int_add
                            [ param_1_2
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_2
                                , param_1_1
                                ]
                            ]
                        , '7'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1_0
                                , 10
                                ]
                            , 7
                            ]
                        )
                        param_1_1
                        (Pine_kernel.int_add
                            [ param_1_2
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_2
                                , param_1_1
                                ]
                            ]
                        , '8'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1_0
                                , 10
                                ]
                            , 8
                            ]
                        )
                        param_1_1
                        (Pine_kernel.int_add
                            [ param_1_2
                            , 4
                            ]
                        )

                else if
                    Pine_kernel.equal
                        [ Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip
                                [ param_1_2
                                , param_1_1
                                ]
                            ]
                        , '9'
                        ]
                then
                    Test.parseUnsignedIntRec
                        (Pine_kernel.int_add
                            [ Pine_kernel.int_mul
                                [ param_1_0
                                , 10
                                ]
                            , 9
                            ]
                        )
                        param_1_1
                        (Pine_kernel.int_add
                            [ param_1_2
                            , 4
                            ]
                        )

                else
                    [ [ Ok
                      , [ param_1_0
                        ]
                      ]
                    , param_1_2
                    ]
            
            """"
            .Trim());

        var asCSharp =
            Core.DotNet.StaticProgramCSharp.FromStaticProgram(
                staticProgram,
                CodeAnalysisTestHelper.DeclarationSyntaxContext);

        var testClass = asCSharp.ModulesClasses[new DeclQualifiedName([], "Test")];

        var moduleTestCSharpText =
            testClass.RenderToString();

        var commonValuesClassText =
            Pine.PineVM.StaticProgramCSharpClass.RenderToString(asCSharp.CommonValueClass);

        moduleTestCSharpText.Trim().Should().Be(
            """"
            public static class Test
            {
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

                    if (local_000 == CommonReusedValues.Blob_Char_digit_0)
                    {
                        return
                            PineValue.List(
                                [
                                    CommonReusedValues.List_c3304aab,
                                    KernelFunctionSpecialized.int_add(4, param_1_1)
                                ]);
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_1)
                    {
                        return
                            Test.parseUnsignedIntRec(
                                CommonReusedValues.Blob_Int_1,
                                param_1_0,
                                KernelFunctionSpecialized.int_add(4, param_1_1));
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_2)
                    {
                        return
                            Test.parseUnsignedIntRec(
                                CommonReusedValues.Blob_Int_2,
                                param_1_0,
                                KernelFunctionSpecialized.int_add(4, param_1_1));
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_3)
                    {
                        return
                            Test.parseUnsignedIntRec(
                                CommonReusedValues.Blob_Int_3,
                                param_1_0,
                                KernelFunctionSpecialized.int_add(4, param_1_1));
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_4)
                    {
                        return
                            Test.parseUnsignedIntRec(
                                CommonReusedValues.Blob_Int_4,
                                param_1_0,
                                KernelFunctionSpecialized.int_add(4, param_1_1));
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_5)
                    {
                        return
                            Test.parseUnsignedIntRec(
                                CommonReusedValues.Blob_Int_5,
                                param_1_0,
                                KernelFunctionSpecialized.int_add(4, param_1_1));
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_6)
                    {
                        return
                            Test.parseUnsignedIntRec(
                                CommonReusedValues.Blob_Int_6,
                                param_1_0,
                                KernelFunctionSpecialized.int_add(4, param_1_1));
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_7)
                    {
                        return
                            Test.parseUnsignedIntRec(
                                CommonReusedValues.Blob_Int_7,
                                param_1_0,
                                KernelFunctionSpecialized.int_add(4, param_1_1));
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_8)
                    {
                        return
                            Test.parseUnsignedIntRec(
                                CommonReusedValues.Blob_Int_8,
                                param_1_0,
                                KernelFunctionSpecialized.int_add(4, param_1_1));
                    }

                    if (local_000 == CommonReusedValues.Blob_Char_digit_9)
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

                        if (local_000 == CommonReusedValues.Blob_Char_digit_0)
                        {
                            local_param_1_0 =
                                KernelFunctionSpecialized.int_mul(10, local_param_1_0);

                            local_param_1_2 =
                                KernelFunctionSpecialized.int_add(4, local_param_1_2);

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_1)
                        {
                            local_param_1_0 =
                                KernelFunctionSpecialized.int_add(
                                    1,
                                    KernelFunctionSpecialized.int_mul(10, local_param_1_0));

                            local_param_1_2 =
                                KernelFunctionSpecialized.int_add(4, local_param_1_2);

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_2)
                        {
                            local_param_1_0 =
                                KernelFunctionSpecialized.int_add(
                                    2,
                                    KernelFunctionSpecialized.int_mul(10, local_param_1_0));

                            local_param_1_2 =
                                KernelFunctionSpecialized.int_add(4, local_param_1_2);

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_3)
                        {
                            local_param_1_0 =
                                KernelFunctionSpecialized.int_add(
                                    3,
                                    KernelFunctionSpecialized.int_mul(10, local_param_1_0));

                            local_param_1_2 =
                                KernelFunctionSpecialized.int_add(4, local_param_1_2);

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_4)
                        {
                            local_param_1_0 =
                                KernelFunctionSpecialized.int_add(
                                    4,
                                    KernelFunctionSpecialized.int_mul(10, local_param_1_0));

                            local_param_1_2 =
                                KernelFunctionSpecialized.int_add(4, local_param_1_2);

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_5)
                        {
                            local_param_1_0 =
                                KernelFunctionSpecialized.int_add(
                                    5,
                                    KernelFunctionSpecialized.int_mul(10, local_param_1_0));

                            local_param_1_2 =
                                KernelFunctionSpecialized.int_add(4, local_param_1_2);

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_6)
                        {
                            local_param_1_0 =
                                KernelFunctionSpecialized.int_add(
                                    6,
                                    KernelFunctionSpecialized.int_mul(10, local_param_1_0));

                            local_param_1_2 =
                                KernelFunctionSpecialized.int_add(4, local_param_1_2);

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_7)
                        {
                            local_param_1_0 =
                                KernelFunctionSpecialized.int_add(
                                    7,
                                    KernelFunctionSpecialized.int_mul(10, local_param_1_0));

                            local_param_1_2 =
                                KernelFunctionSpecialized.int_add(4, local_param_1_2);

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_8)
                        {
                            local_param_1_0 =
                                KernelFunctionSpecialized.int_add(
                                    8,
                                    KernelFunctionSpecialized.int_mul(10, local_param_1_0));

                            local_param_1_2 =
                                KernelFunctionSpecialized.int_add(4, local_param_1_2);

                            continue;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_digit_9)
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

            """".Trim());

        commonValuesClassText.Trim().Should().Be(
            """"
            public static class CommonReusedValues
            {
                public static readonly PineValue Blob_Int_neg_1 =
                    IntegerEncoding.EncodeSignedInteger(-1);

                public static readonly PineValue Blob_Int_0 =
                    IntegerEncoding.EncodeSignedInteger(0);

                public static readonly PineValue Blob_Int_1 =
                    IntegerEncoding.EncodeSignedInteger(1);

                public static readonly PineValue Blob_Int_2 =
                    IntegerEncoding.EncodeSignedInteger(2);

                public static readonly PineValue Blob_Int_3 =
                    IntegerEncoding.EncodeSignedInteger(3);

                public static readonly PineValue Blob_Int_4 =
                    IntegerEncoding.EncodeSignedInteger(4);

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

                public static readonly PineValue Blob_Int_10 =
                    IntegerEncoding.EncodeSignedInteger(10);

                public static readonly PineValue Blob_37f21fcb =
                    StringEncoding.ValueFromString("-");

                public static readonly PineValue Blob_Char_digit_0 =
                    StringEncoding.ValueFromString("0");

                public static readonly PineValue Blob_Char_digit_1 =
                    StringEncoding.ValueFromString("1");

                public static readonly PineValue Blob_Char_digit_2 =
                    StringEncoding.ValueFromString("2");

                public static readonly PineValue Blob_Char_digit_3 =
                    StringEncoding.ValueFromString("3");

                public static readonly PineValue Blob_Char_digit_4 =
                    StringEncoding.ValueFromString("4");

                public static readonly PineValue Blob_Char_digit_5 =
                    StringEncoding.ValueFromString("5");

                public static readonly PineValue Blob_Char_digit_6 =
                    StringEncoding.ValueFromString("6");

                public static readonly PineValue Blob_Char_digit_7 =
                    StringEncoding.ValueFromString("7");

                public static readonly PineValue Blob_Char_digit_8 =
                    StringEncoding.ValueFromString("8");

                public static readonly PineValue Blob_Char_digit_9 =
                    StringEncoding.ValueFromString("9");

                public static readonly PineValue Blob_Str_Ok =
                    StringEncoding.ValueFromString("Ok");

                public static readonly PineValue Blob_Str_Err =
                    StringEncoding.ValueFromString("Err");

                public static readonly PineValue Blob_Str_List =
                    StringEncoding.ValueFromString("List");

                public static readonly PineValue Blob_Str_head =
                    StringEncoding.ValueFromString("head");

                public static readonly PineValue Blob_Str_skip =
                    StringEncoding.ValueFromString("skip");

                public static readonly PineValue Blob_Str_take =
                    StringEncoding.ValueFromString("take");

                public static readonly PineValue Blob_Str_equal =
                    StringEncoding.ValueFromString("equal");

                public static readonly PineValue Blob_Str_String =
                    StringEncoding.ValueFromString("String");

                public static readonly PineValue Blob_Str_concat =
                    StringEncoding.ValueFromString("concat");

                public static readonly PineValue Blob_Str_length =
                    StringEncoding.ValueFromString("length");

                public static readonly PineValue Blob_Str_Literal =
                    StringEncoding.ValueFromString("Literal");

                public static readonly PineValue Blob_Str_int_add =
                    StringEncoding.ValueFromString("int_add");

                public static readonly PineValue Blob_Str_int_mul =
                    StringEncoding.ValueFromString("int_mul");

                public static readonly PineValue Blob_Str_Function =
                    StringEncoding.ValueFromString("Function");

                public static readonly PineValue Blob_Str_Conditional =
                    StringEncoding.ValueFromString("Conditional");

                public static readonly PineValue Blob_Str_Environment =
                    StringEncoding.ValueFromString("Environment");

                public static readonly PineValue Blob_Str_ParseAndEval =
                    StringEncoding.ValueFromString("ParseAndEval");

                public static readonly PineValue Blob_cfca8ee1 =
                    StringEncoding.ValueFromString("Expecting a digit");

                public static readonly PineValue Blob_Str_KernelApplication =
                    StringEncoding.ValueFromString("KernelApplication");

                public static readonly PineValue Blob_d348781a =
                    StringEncoding.ValueFromString("Error in case-of block: No matching branch.");

                public static readonly PineValue List_dda26649 =
                    PineValue.EmptyList;

                public static readonly PineValue List_d64a6579 =
                    PineValue.List(
                        [List_dda26649]);

                public static readonly PineValue List_49dc21d7 =
                    PineValue.List(
                        [
                            IntegerEncoding.EncodeSignedInteger(-1)
                        ]);

                public static readonly PineValue List_0a7103c5 =
                    PineValue.List(
                        [
                            IntegerEncoding.EncodeSignedInteger(0)
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

                public static readonly PineValue List_9b844394 =
                    PineValue.List(
                        [
                            IntegerEncoding.EncodeSignedInteger(3)
                        ]);

                public static readonly PineValue List_fd811a2e =
                    PineValue.List(
                        [
                            IntegerEncoding.EncodeSignedInteger(4)
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

                public static readonly PineValue List_1cb34950 =
                    PineValue.List(
                        [
                            IntegerEncoding.EncodeSignedInteger(10)
                        ]);

                public static readonly PineValue List_06c9f446 =
                    PineValue.List(
                        [Blob_37f21fcb]);

                public static readonly PineValue List_c7702eda =
                    PineValue.List(
                        [Blob_Char_digit_0]);

                public static readonly PineValue List_e3413a44 =
                    PineValue.List(
                        [Blob_Char_digit_1]);

                public static readonly PineValue List_d6a1a968 =
                    PineValue.List(
                        [Blob_Char_digit_2]);

                public static readonly PineValue List_b93b4b39 =
                    PineValue.List(
                        [Blob_Char_digit_3]);

                public static readonly PineValue List_53ffebf2 =
                    PineValue.List(
                        [Blob_Char_digit_4]);

                public static readonly PineValue List_d8088a1d =
                    PineValue.List(
                        [Blob_Char_digit_5]);

                public static readonly PineValue List_4d45d38a =
                    PineValue.List(
                        [Blob_Char_digit_6]);

                public static readonly PineValue List_f767fe42 =
                    PineValue.List(
                        [Blob_Char_digit_7]);

                public static readonly PineValue List_5f13cdae =
                    PineValue.List(
                        [Blob_Char_digit_8]);

                public static readonly PineValue List_87b3a5b0 =
                    PineValue.List(
                        [Blob_Char_digit_9]);

                public static readonly PineValue List_fa93aa90 =
                    PineValue.List(
                        [Blob_Str_Ok]);

                public static readonly PineValue List_2245d502 =
                    PineValue.List(
                        [Blob_Str_Err]);

                public static readonly PineValue List_c3304aab =
                    PineValue.List(
                        [Blob_Str_Ok, List_0a7103c5]);

                public static readonly PineValue List_68944031 =
                    PineValue.List(
                        [List_c3304aab]);

                public static readonly PineValue List_3bb3accf =
                    PineValue.List(
                        [Blob_Str_List, List_d64a6579]);

                public static readonly PineValue List_75564e20 =
                    PineValue.List(
                        [Blob_Str_Function]);

                public static readonly PineValue List_56404869 =
                    PineValue.List(
                        [Blob_Str_Literal, List_49dc21d7]);

                public static readonly PineValue List_0dcd86c0 =
                    PineValue.List(
                        [Blob_Str_Literal, List_7cb7ad19]);

                public static readonly PineValue List_43b95777 =
                    PineValue.List(
                        [Blob_Str_Literal, List_c286665f]);

                public static readonly PineValue List_450c12a0 =
                    PineValue.List(
                        [Blob_Str_Literal, List_9b844394]);

                public static readonly PineValue List_0c82888c =
                    PineValue.List(
                        [Blob_Str_Literal, List_fd811a2e]);

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

                public static readonly PineValue List_c41d9105 =
                    PineValue.List(
                        [Blob_Str_Literal, List_1cb34950]);

                public static readonly PineValue List_87a2a774 =
                    PineValue.List(
                        [Blob_Str_Literal, List_06c9f446]);

                public static readonly PineValue List_4c2609c3 =
                    PineValue.List(
                        [Blob_Str_Literal, List_c7702eda]);

                public static readonly PineValue List_c3b08663 =
                    PineValue.List(
                        [Blob_Str_Literal, List_e3413a44]);

                public static readonly PineValue List_e844984b =
                    PineValue.List(
                        [Blob_Str_Literal, List_d6a1a968]);

                public static readonly PineValue List_a9196577 =
                    PineValue.List(
                        [Blob_Str_Literal, List_b93b4b39]);

                public static readonly PineValue List_9728f698 =
                    PineValue.List(
                        [Blob_Str_Literal, List_53ffebf2]);

                public static readonly PineValue List_86fad7dd =
                    PineValue.List(
                        [Blob_Str_Literal, List_d8088a1d]);

                public static readonly PineValue List_c005c994 =
                    PineValue.List(
                        [Blob_Str_Literal, List_4d45d38a]);

                public static readonly PineValue List_33c6f4ad =
                    PineValue.List(
                        [Blob_Str_Literal, List_f767fe42]);

                public static readonly PineValue List_1a0b8610 =
                    PineValue.List(
                        [Blob_Str_Literal, List_5f13cdae]);

                public static readonly PineValue List_ecf5f39d =
                    PineValue.List(
                        [Blob_Str_Literal, List_87b3a5b0]);

                public static readonly PineValue List_0fcdb59d =
                    PineValue.List(
                        [Blob_Str_Literal, List_fa93aa90]);

                public static readonly PineValue List_60c29edb =
                    PineValue.List(
                        [Blob_Str_Literal, List_2245d502]);

                public static readonly PineValue List_283f6f4a =
                    PineValue.List(
                        [Blob_Str_Literal, List_68944031]);

                public static readonly PineValue List_bd06385a =
                    PineValue.List(
                        [Blob_Str_Environment, List_dda26649]);

                public static readonly PineValue List_2f1c6993 =
                    PineValue.List(
                        [Blob_Str_Literal, List_75564e20]);

                public static readonly PineValue List_4aad0d20 =
                    PineValue.List(
                        [Blob_Str_head, List_bd06385a]);

                public static readonly PineValue List_37ca38cd =
                    PineValue.List(
                        [Blob_cfca8ee1]);

                public static readonly PineValue List_c1cc00b1 =
                    PineValue.List(
                        [List_0dcd86c0, List_bd06385a]);

                public static readonly PineValue List_71a831bc =
                    PineValue.List(
                        [List_43b95777, List_bd06385a]);

                public static readonly PineValue List_dcbfdaf0 =
                    PineValue.List(
                        [List_c1cc00b1]);

                public static readonly PineValue List_aa0d6f6c =
                    PineValue.List(
                        [List_71a831bc]);

                public static readonly PineValue List_708c3c98 =
                    PineValue.List(
                        [Blob_Str_String, List_37ca38cd]);

                public static readonly PineValue List_81718afa =
                    PineValue.List(
                        [List_708c3c98]);

                public static readonly PineValue List_4c301747 =
                    PineValue.List(
                        [Blob_Str_List, List_dcbfdaf0]);

                public static readonly PineValue List_7ed5803b =
                    PineValue.List(
                        [Blob_Str_List, List_aa0d6f6c]);

                public static readonly PineValue List_ae45bd54 =
                    PineValue.List(
                        [Blob_Str_Err, List_81718afa]);

                public static readonly PineValue List_e9cee56f =
                    PineValue.List(
                        [List_ae45bd54]);

                public static readonly PineValue List_b49facc1 =
                    PineValue.List(
                        [Blob_Str_skip, List_4c301747]);

                public static readonly PineValue List_0c396738 =
                    PineValue.List(
                        [Blob_Str_skip, List_7ed5803b]);

                public static readonly PineValue List_a5cbaf18 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_4aad0d20]);

                public static readonly PineValue List_abd0252a =
                    PineValue.List(
                        [Blob_Str_Literal, List_e9cee56f]);

                public static readonly PineValue List_61fa9b59 =
                    PineValue.List(
                        [Blob_Str_head, List_a5cbaf18]);

                public static readonly PineValue List_9517421c =
                    PineValue.List(
                        [List_0dcd86c0, List_a5cbaf18]);

                public static readonly PineValue List_f5d22246 =
                    PineValue.List(
                        [List_43b95777, List_a5cbaf18]);

                public static readonly PineValue List_12d91a58 =
                    PineValue.List(
                        [List_9517421c]);

                public static readonly PineValue List_a671a9e1 =
                    PineValue.List(
                        [List_f5d22246]);

                public static readonly PineValue List_c2aae15a =
                    PineValue.List(
                        [Blob_d348781a]);

                public static readonly PineValue List_94d58849 =
                    PineValue.List(
                        [Blob_Str_List, List_12d91a58]);

                public static readonly PineValue List_6a975ff5 =
                    PineValue.List(
                        [Blob_Str_List, List_a671a9e1]);

                public static readonly PineValue List_65405503 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_b49facc1]);

                public static readonly PineValue List_97963d43 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_0c396738]);

                public static readonly PineValue List_db582731 =
                    PineValue.List(
                        [Blob_Str_Literal, List_c2aae15a]);

                public static readonly PineValue List_7552bdea =
                    PineValue.List(
                        [Blob_Str_head, List_65405503]);

                public static readonly PineValue List_391923b1 =
                    PineValue.List(
                        [Blob_Str_head, List_97963d43]);

                public static readonly PineValue List_f942a019 =
                    PineValue.List(
                        [Blob_Str_skip, List_94d58849]);

                public static readonly PineValue List_0175b775 =
                    PineValue.List(
                        [Blob_Str_skip, List_6a975ff5]);

                public static readonly PineValue List_976730e9 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_61fa9b59]);

                public static readonly PineValue List_f1126ab7 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_7552bdea]);

                public static readonly PineValue List_3c738443 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_391923b1]);

                public static readonly PineValue List_908d1ad7 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_f942a019]);

                public static readonly PineValue List_e38aa202 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_0175b775]);

                public static readonly PineValue List_16c4bb4a =
                    PineValue.List(
                        [Blob_Str_head, List_f1126ab7]);

                public static readonly PineValue List_57070c8f =
                    PineValue.List(
                        [Blob_Str_head, List_3c738443]);

                public static readonly PineValue List_2456c204 =
                    PineValue.List(
                        [Blob_Str_head, List_908d1ad7]);

                public static readonly PineValue List_4045f225 =
                    PineValue.List(
                        [Blob_Str_head, List_e38aa202]);

                public static readonly PineValue List_584a0752 =
                    PineValue.List(
                        [List_3bb3accf, List_3c738443]);

                public static readonly PineValue List_bc8c8111 =
                    PineValue.List(
                        [List_584a0752]);

                public static readonly PineValue List_13c67965 =
                    PineValue.List(
                        [List_0dcd86c0, List_f1126ab7]);

                public static readonly PineValue List_5d630f29 =
                    PineValue.List(
                        [List_0dcd86c0, List_3c738443]);

                public static readonly PineValue List_fd602b70 =
                    PineValue.List(
                        [List_43b95777, List_f1126ab7]);

                public static readonly PineValue List_498d834e =
                    PineValue.List(
                        [List_13c67965]);

                public static readonly PineValue List_44044e9b =
                    PineValue.List(
                        [List_5d630f29]);

                public static readonly PineValue List_22254e85 =
                    PineValue.List(
                        [List_fd602b70]);

                public static readonly PineValue List_1d0e40fb =
                    PineValue.List(
                        [Blob_Str_List, List_bc8c8111]);

                public static readonly PineValue List_85fcd3b2 =
                    PineValue.List(
                        [Blob_Str_List, List_498d834e]);

                public static readonly PineValue List_b011cd28 =
                    PineValue.List(
                        [Blob_Str_List, List_44044e9b]);

                public static readonly PineValue List_4668ada6 =
                    PineValue.List(
                        [Blob_Str_List, List_22254e85]);

                public static readonly PineValue List_2b305696 =
                    PineValue.List(
                        [Blob_Str_equal, List_1d0e40fb]);

                public static readonly PineValue List_6b896037 =
                    PineValue.List(
                        [Blob_Str_skip, List_85fcd3b2]);

                public static readonly PineValue List_8562198d =
                    PineValue.List(
                        [Blob_Str_skip, List_b011cd28]);

                public static readonly PineValue List_3218a5a2 =
                    PineValue.List(
                        [Blob_Str_skip, List_4668ada6]);

                public static readonly PineValue List_b43468c9 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_16c4bb4a]);

                public static readonly PineValue List_b1530ee5 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_57070c8f]);

                public static readonly PineValue List_6dbb5f29 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_2456c204]);

                public static readonly PineValue List_d08202cd =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_4045f225]);

                public static readonly PineValue List_ecedc467 =
                    PineValue.List(
                        [List_b43468c9]);

                public static readonly PineValue List_4d54c0e3 =
                    PineValue.List(
                        [List_b1530ee5]);

                public static readonly PineValue List_fb3114c2 =
                    PineValue.List(
                        [List_d08202cd]);

                public static readonly PineValue List_1a17feda =
                    PineValue.List(
                        [List_ecedc467]);

                public static readonly PineValue List_62636573 =
                    PineValue.List(
                        [List_4d54c0e3]);

                public static readonly PineValue List_c3229c9e =
                    PineValue.List(
                        [List_fb3114c2]);

                public static readonly PineValue List_c0abcf7c =
                    PineValue.List(
                        [Blob_Str_List, List_1a17feda]);

                public static readonly PineValue List_009614b4 =
                    PineValue.List(
                        [Blob_Str_List, List_62636573]);

                public static readonly PineValue List_bc0ffa85 =
                    PineValue.List(
                        [Blob_Str_List, List_c3229c9e]);

                public static readonly PineValue List_fcd219b8 =
                    PineValue.List(
                        [List_b43468c9, List_c41d9105]);

                public static readonly PineValue List_3762bb34 =
                    PineValue.List(
                        [List_fcd219b8]);

                public static readonly PineValue List_ad2fad57 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_2b305696]);

                public static readonly PineValue List_00efda36 =
                    PineValue.List(
                        [Blob_Str_List, List_3762bb34]);

                public static readonly PineValue List_c37ef632 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_6b896037]);

                public static readonly PineValue List_4de8c353 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_8562198d]);

                public static readonly PineValue List_ee3ba15f =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_3218a5a2]);

                public static readonly PineValue List_fd51f5ba =
                    PineValue.List(
                        [List_0fcdb59d, List_c0abcf7c]);

                public static readonly PineValue List_2af1c018 =
                    PineValue.List(
                        [List_fd51f5ba]);

                public static readonly PineValue List_cbdf5919 =
                    PineValue.List(
                        [List_b43468c9, List_2f1c6993]);

                public static readonly PineValue List_1225d477 =
                    PineValue.List(
                        [List_cbdf5919]);

                public static readonly PineValue List_a53f1292 =
                    PineValue.List(
                        [Blob_Str_head, List_c37ef632]);

                public static readonly PineValue List_290d4c0d =
                    PineValue.List(
                        [Blob_Str_head, List_ee3ba15f]);

                public static readonly PineValue List_a2e49ce2 =
                    PineValue.List(
                        [Blob_Str_List, List_2af1c018]);

                public static readonly PineValue List_87642022 =
                    PineValue.List(
                        [Blob_Str_List, List_1225d477]);

                public static readonly PineValue List_58360a74 =
                    PineValue.List(
                        [Blob_Str_int_mul, List_00efda36]);

                public static readonly PineValue List_44a94f99 =
                    PineValue.List(
                        [Blob_Str_equal, List_87642022]);

                public static readonly PineValue List_4af1f0ec =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_a53f1292]);

                public static readonly PineValue List_768cc61e =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_290d4c0d]);

                public static readonly PineValue List_46a1ad58 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_58360a74]);

                public static readonly PineValue List_3152db3d =
                    PineValue.List(
                        [Blob_Str_head, List_4af1f0ec]);

                public static readonly PineValue List_583bd6fe =
                    PineValue.List(
                        [Blob_Str_head, List_768cc61e]);

                public static readonly PineValue List_04fe9790 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_44a94f99]);

                public static readonly PineValue List_43c3a629 =
                    PineValue.List(
                        [List_0dcd86c0, List_4af1f0ec]);

                public static readonly PineValue List_2acabf78 =
                    PineValue.List(
                        [List_0dcd86c0, List_768cc61e]);

                public static readonly PineValue List_fe24865c =
                    PineValue.List(
                        [List_43b95777, List_4af1f0ec]);

                public static readonly PineValue List_89e2dbe2 =
                    PineValue.List(
                        [List_450c12a0, List_4af1f0ec]);

                public static readonly PineValue List_18b551f9 =
                    PineValue.List(
                        [List_4af1f0ec, List_0c82888c]);

                public static readonly PineValue List_b0e5fccb =
                    PineValue.List(
                        [List_768cc61e, List_0c82888c]);

                public static readonly PineValue List_8b2061b8 =
                    PineValue.List(
                        [List_43c3a629]);

                public static readonly PineValue List_79f2a1d2 =
                    PineValue.List(
                        [List_2acabf78]);

                public static readonly PineValue List_099e08b0 =
                    PineValue.List(
                        [List_fe24865c]);

                public static readonly PineValue List_99155730 =
                    PineValue.List(
                        [List_89e2dbe2]);

                public static readonly PineValue List_645f0282 =
                    PineValue.List(
                        [List_18b551f9]);

                public static readonly PineValue List_65e9b2dc =
                    PineValue.List(
                        [List_b0e5fccb]);

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

                public static readonly PineValue List_c12fd35a =
                    PineValue.List(
                        [Blob_Str_List, List_8b2061b8]);

                public static readonly PineValue List_b92e3174 =
                    PineValue.List(
                        [Blob_Str_List, List_79f2a1d2]);

                public static readonly PineValue List_6d80c629 =
                    PineValue.List(
                        [Blob_Str_List, List_099e08b0]);

                public static readonly PineValue List_5e3370cb =
                    PineValue.List(
                        [Blob_Str_List, List_99155730]);

                public static readonly PineValue List_174dc04f =
                    PineValue.List(
                        [Blob_Str_List, List_645f0282]);

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

                public static readonly PineValue List_2102b77e =
                    PineValue.List(
                        [Blob_Str_skip, List_c12fd35a]);

                public static readonly PineValue List_0bc76b59 =
                    PineValue.List(
                        [Blob_Str_skip, List_b92e3174]);

                public static readonly PineValue List_09da05b0 =
                    PineValue.List(
                        [Blob_Str_skip, List_6d80c629]);

                public static readonly PineValue List_bae6cac0 =
                    PineValue.List(
                        [Blob_Str_skip, List_5e3370cb]);

                public static readonly PineValue List_770c8d31 =
                    PineValue.List(
                        [Blob_Str_int_add, List_174dc04f]);

                public static readonly PineValue List_0d8426a4 =
                    PineValue.List(
                        [Blob_Str_int_add, List_6781a24c]);

                public static readonly PineValue List_ffb3657e =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_3152db3d]);

                public static readonly PineValue List_de173be4 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_583bd6fe]);

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

                public static readonly PineValue List_8c6d652b =
                    PineValue.List(
                        [List_0fcdb59d, List_de173be4]);

                public static readonly PineValue List_a0e5d60d =
                    PineValue.List(
                        [List_8c6d652b]);

                public static readonly PineValue List_1152af79 =
                    PineValue.List(
                        [List_60c29edb, List_de173be4]);

                public static readonly PineValue List_7a741ccd =
                    PineValue.List(
                        [List_1152af79]);

                public static readonly PineValue List_1a2b5777 =
                    PineValue.List(
                        [List_f1126ab7, List_b1530ee5]);

                public static readonly PineValue List_c88b942a =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_2102b77e]);

                public static readonly PineValue List_d0698c30 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_0bc76b59]);

                public static readonly PineValue List_ddf7fc1c =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_09da05b0]);

                public static readonly PineValue List_a22f03df =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_bae6cac0]);

                public static readonly PineValue List_65a3abe8 =
                    PineValue.List(
                        [List_abd0252a, List_4af1f0ec]);

                public static readonly PineValue List_8783616b =
                    PineValue.List(
                        [List_65a3abe8]);

                public static readonly PineValue List_d9c9e345 =
                    PineValue.List(
                        [Blob_Str_List, List_a0e5d60d]);

                public static readonly PineValue List_692da849 =
                    PineValue.List(
                        [Blob_Str_List, List_7a741ccd]);

                public static readonly PineValue List_40007240 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_770c8d31]);

                public static readonly PineValue List_d64a63d0 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_0d8426a4]);

                public static readonly PineValue List_d61ec4ce =
                    PineValue.List(
                        [Blob_Str_head, List_c88b942a]);

                public static readonly PineValue List_ab8a69c6 =
                    PineValue.List(
                        [Blob_Str_head, List_d0698c30]);

                public static readonly PineValue List_6ac25a38 =
                    PineValue.List(
                        [Blob_Str_head, List_ddf7fc1c]);

                public static readonly PineValue List_b655c855 =
                    PineValue.List(
                        [Blob_Str_head, List_a22f03df]);

                public static readonly PineValue List_def272c2 =
                    PineValue.List(
                        [Blob_Str_List, List_8783616b]);

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

                public static readonly PineValue List_fb11d7b4 =
                    PineValue.List(
                        [Blob_Str_equal, List_d9c9e345]);

                public static readonly PineValue List_9e5ef04d =
                    PineValue.List(
                        [Blob_Str_equal, List_692da849]);

                public static readonly PineValue List_7e881476 =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_1a2b5777]);

                public static readonly PineValue List_28783f7c =
                    PineValue.List(
                        [List_283f6f4a, List_40007240]);

                public static readonly PineValue List_d3bfa601 =
                    PineValue.List(
                        [List_28783f7c]);

                public static readonly PineValue List_55a6d783 =
                    PineValue.List(
                        [List_db582731, List_768cc61e]);

                public static readonly PineValue List_8c9ba17c =
                    PineValue.List(
                        [Blob_Str_List, List_d3bfa601]);

                public static readonly PineValue List_1da3871f =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_d61ec4ce]);

                public static readonly PineValue List_1fd0a7ec =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_ab8a69c6]);

                public static readonly PineValue List_6cab91e9 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_6ac25a38]);

                public static readonly PineValue List_1c80da68 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_b655c855]);

                public static readonly PineValue List_02aa03c8 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_fb11d7b4]);

                public static readonly PineValue List_d17c51de =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_9e5ef04d]);

                public static readonly PineValue List_b978ab71 =
                    PineValue.List(
                        [Blob_Str_head, List_1fd0a7ec]);

                public static readonly PineValue List_3f936680 =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_55a6d783]);

                public static readonly PineValue List_1e3edf76 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_b978ab71]);

                public static readonly PineValue List_444422d4 =
                    PineValue.List(
                        [List_1e3edf76]);

                public static readonly PineValue List_f1bae3ad =
                    PineValue.List(
                        [List_444422d4]);

                public static readonly PineValue List_ffe47aa6 =
                    PineValue.List(
                        [Blob_Str_List, List_f1bae3ad]);

                public static readonly PineValue List_9abebbb1 =
                    PineValue.List(
                        [List_56404869, List_1e3edf76]);

                public static readonly PineValue List_7dce9bb4 =
                    PineValue.List(
                        [List_9abebbb1]);

                public static readonly PineValue List_81929877 =
                    PineValue.List(
                        [List_b43468c9, List_4af1f0ec]);

                public static readonly PineValue List_989a3a14 =
                    PineValue.List(
                        [List_4af1f0ec, List_b43468c9]);

                public static readonly PineValue List_d3af6d10 =
                    PineValue.List(
                        [List_81929877]);

                public static readonly PineValue List_ad513199 =
                    PineValue.List(
                        [List_989a3a14]);

                public static readonly PineValue List_0d60107e =
                    PineValue.List(
                        [Blob_Str_List, List_7dce9bb4]);

                public static readonly PineValue List_6e40e668 =
                    PineValue.List(
                        [List_60c29edb, List_ffe47aa6]);

                public static readonly PineValue List_17da7205 =
                    PineValue.List(
                        [List_6e40e668]);

                public static readonly PineValue List_d9dcda92 =
                    PineValue.List(
                        [Blob_Str_List, List_d3af6d10]);

                public static readonly PineValue List_a511c140 =
                    PineValue.List(
                        [Blob_Str_List, List_ad513199]);

                public static readonly PineValue List_a3ae724a =
                    PineValue.List(
                        [Blob_Str_List, List_17da7205]);

                public static readonly PineValue List_38f4f14c =
                    PineValue.List(
                        [Blob_Str_int_mul, List_0d60107e]);

                public static readonly PineValue List_90a1f73c =
                    PineValue.List(
                        [Blob_Str_skip, List_a511c140]);

                public static readonly PineValue List_21c04354 =
                    PineValue.List(
                        [List_a2e49ce2, List_768cc61e]);

                public static readonly PineValue List_ab939a5d =
                    PineValue.List(
                        [List_21c04354]);

                public static readonly PineValue List_2966387f =
                    PineValue.List(
                        [Blob_Str_List, List_ab939a5d]);

                public static readonly PineValue List_c6f65d71 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_38f4f14c]);

                public static readonly PineValue List_1050dca6 =
                    PineValue.List(
                        [List_c6f65d71]);

                public static readonly PineValue List_2c5eb8d4 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_90a1f73c]);

                public static readonly PineValue List_6be47f1f =
                    PineValue.List(
                        [List_1050dca6]);

                public static readonly PineValue List_13572e94 =
                    PineValue.List(
                        [Blob_Str_List, List_6be47f1f]);

                public static readonly PineValue List_a4ea2f3b =
                    PineValue.List(
                        [List_0c82888c, List_2c5eb8d4]);

                public static readonly PineValue List_2449c52e =
                    PineValue.List(
                        [List_768cc61e, List_4af1f0ec]);

                public static readonly PineValue List_3da572ce =
                    PineValue.List(
                        [List_a4ea2f3b]);

                public static readonly PineValue List_2563cb2b =
                    PineValue.List(
                        [List_2449c52e]);

                public static readonly PineValue List_6133ffd6 =
                    PineValue.List(
                        [List_b43468c9, List_40007240]);

                public static readonly PineValue List_dacd6550 =
                    PineValue.List(
                        [List_6133ffd6]);

                public static readonly PineValue List_b1e0ac34 =
                    PineValue.List(
                        [Blob_Str_List, List_3da572ce]);

                public static readonly PineValue List_d8c1129d =
                    PineValue.List(
                        [Blob_Str_List, List_2563cb2b]);

                public static readonly PineValue List_5709306d =
                    PineValue.List(
                        [List_0fcdb59d, List_13572e94]);

                public static readonly PineValue List_68b25f09 =
                    PineValue.List(
                        [List_5709306d]);

                public static readonly PineValue List_98a55f77 =
                    PineValue.List(
                        [Blob_Str_List, List_dacd6550]);

                public static readonly PineValue List_ed23348b =
                    PineValue.List(
                        [Blob_Str_skip, List_d8c1129d]);

                public static readonly PineValue List_11be84f7 =
                    PineValue.List(
                        [Blob_Str_take, List_b1e0ac34]);

                public static readonly PineValue List_fde32251 =
                    PineValue.List(
                        [Blob_Str_List, List_68b25f09]);

                public static readonly PineValue List_49fd4802 =
                    PineValue.List(
                        [List_0dcd86c0, List_b43468c9, List_40007240]);

                public static readonly PineValue List_bec006ed =
                    PineValue.List(
                        [List_43b95777, List_b43468c9, List_40007240]);

                public static readonly PineValue List_c21e4f61 =
                    PineValue.List(
                        [List_450c12a0, List_b43468c9, List_40007240]);

                public static readonly PineValue List_9d34c3d5 =
                    PineValue.List(
                        [List_0c82888c, List_b43468c9, List_40007240]);

                public static readonly PineValue List_3ccca30d =
                    PineValue.List(
                        [List_c1b27e6e, List_b43468c9, List_40007240]);

                public static readonly PineValue List_3f11762c =
                    PineValue.List(
                        [List_282dee3a, List_b43468c9, List_40007240]);

                public static readonly PineValue List_b748b0dc =
                    PineValue.List(
                        [List_9f1e38f9, List_b43468c9, List_40007240]);

                public static readonly PineValue List_e8c90f25 =
                    PineValue.List(
                        [List_14a0ba72, List_b43468c9, List_40007240]);

                public static readonly PineValue List_321a5571 =
                    PineValue.List(
                        [List_a710c27f, List_b43468c9, List_40007240]);

                public static readonly PineValue List_758221cc =
                    PineValue.List(
                        [List_49fd4802]);

                public static readonly PineValue List_0de6b3f4 =
                    PineValue.List(
                        [List_bec006ed]);

                public static readonly PineValue List_3bf74958 =
                    PineValue.List(
                        [List_c21e4f61]);

                public static readonly PineValue List_f904748f =
                    PineValue.List(
                        [List_9d34c3d5]);

                public static readonly PineValue List_1c6319e9 =
                    PineValue.List(
                        [List_3ccca30d]);

                public static readonly PineValue List_8c7e4cbe =
                    PineValue.List(
                        [List_3f11762c]);

                public static readonly PineValue List_52bbccb3 =
                    PineValue.List(
                        [List_b748b0dc]);

                public static readonly PineValue List_2e6f3e2b =
                    PineValue.List(
                        [List_e8c90f25]);

                public static readonly PineValue List_e60859c8 =
                    PineValue.List(
                        [List_321a5571]);

                public static readonly PineValue List_c32f8d23 =
                    PineValue.List(
                        [Blob_Str_List, List_758221cc]);

                public static readonly PineValue List_a2c8f94f =
                    PineValue.List(
                        [Blob_Str_List, List_0de6b3f4]);

                public static readonly PineValue List_6227bc04 =
                    PineValue.List(
                        [Blob_Str_List, List_3bf74958]);

                public static readonly PineValue List_8594c0bd =
                    PineValue.List(
                        [Blob_Str_List, List_f904748f]);

                public static readonly PineValue List_11925635 =
                    PineValue.List(
                        [Blob_Str_List, List_1c6319e9]);

                public static readonly PineValue List_bdf4568f =
                    PineValue.List(
                        [Blob_Str_List, List_8c7e4cbe]);

                public static readonly PineValue List_afc020d7 =
                    PineValue.List(
                        [Blob_Str_List, List_52bbccb3]);

                public static readonly PineValue List_1a65ed37 =
                    PineValue.List(
                        [Blob_Str_List, List_2e6f3e2b]);

                public static readonly PineValue List_358ebecf =
                    PineValue.List(
                        [Blob_Str_List, List_e60859c8]);

                public static readonly PineValue List_84e5f176 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_ed23348b]);

                public static readonly PineValue List_60939d9c =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_11be84f7]);

                public static readonly PineValue List_75de6aa1 =
                    PineValue.List(
                        [List_1c80da68, List_009614b4]);

                public static readonly PineValue List_55e2516e =
                    PineValue.List(
                        [List_75de6aa1]);

                public static readonly PineValue List_d4a89214 =
                    PineValue.List(
                        [Blob_Str_List, List_55e2516e]);

                public static readonly PineValue List_5d745015 =
                    PineValue.List(
                        [List_0c82888c, List_84e5f176]);

                public static readonly PineValue List_fda03c60 =
                    PineValue.List(
                        [List_5d745015]);

                public static readonly PineValue List_001d1eca =
                    PineValue.List(
                        [List_60939d9c, List_87a2a774]);

                public static readonly PineValue List_e0a86b82 =
                    PineValue.List(
                        [List_60939d9c, List_4c2609c3]);

                public static readonly PineValue List_8d351a9e =
                    PineValue.List(
                        [List_60939d9c, List_c3b08663]);

                public static readonly PineValue List_7521852b =
                    PineValue.List(
                        [List_60939d9c, List_e844984b]);

                public static readonly PineValue List_831ec1fd =
                    PineValue.List(
                        [List_60939d9c, List_a9196577]);

                public static readonly PineValue List_4278c9fa =
                    PineValue.List(
                        [List_60939d9c, List_9728f698]);

                public static readonly PineValue List_a1606e60 =
                    PineValue.List(
                        [List_60939d9c, List_86fad7dd]);

                public static readonly PineValue List_a2644dd5 =
                    PineValue.List(
                        [List_60939d9c, List_c005c994]);

                public static readonly PineValue List_90335742 =
                    PineValue.List(
                        [List_60939d9c, List_33c6f4ad]);

                public static readonly PineValue List_fd9460de =
                    PineValue.List(
                        [List_60939d9c, List_1a0b8610]);

                public static readonly PineValue List_8ed941ee =
                    PineValue.List(
                        [List_60939d9c, List_ecf5f39d]);

                public static readonly PineValue List_6f94aa62 =
                    PineValue.List(
                        [List_001d1eca]);

                public static readonly PineValue List_399dab64 =
                    PineValue.List(
                        [List_e0a86b82]);

                public static readonly PineValue List_37740652 =
                    PineValue.List(
                        [List_8d351a9e]);

                public static readonly PineValue List_5363db38 =
                    PineValue.List(
                        [List_7521852b]);

                public static readonly PineValue List_7bb74928 =
                    PineValue.List(
                        [List_831ec1fd]);

                public static readonly PineValue List_b0170612 =
                    PineValue.List(
                        [List_4278c9fa]);

                public static readonly PineValue List_2e59bc5f =
                    PineValue.List(
                        [List_a1606e60]);

                public static readonly PineValue List_3adb4b83 =
                    PineValue.List(
                        [List_a2644dd5]);

                public static readonly PineValue List_118061b3 =
                    PineValue.List(
                        [List_90335742]);

                public static readonly PineValue List_0b491071 =
                    PineValue.List(
                        [List_fd9460de]);

                public static readonly PineValue List_a198e751 =
                    PineValue.List(
                        [List_8ed941ee]);

                public static readonly PineValue List_14382e40 =
                    PineValue.List(
                        [Blob_Str_concat, List_d4a89214]);

                public static readonly PineValue List_da5eb341 =
                    PineValue.List(
                        [Blob_Str_List, List_fda03c60]);

                public static readonly PineValue List_9b2e4f9b =
                    PineValue.List(
                        [Blob_Str_List, List_6f94aa62]);

                public static readonly PineValue List_dd1a37bb =
                    PineValue.List(
                        [Blob_Str_List, List_399dab64]);

                public static readonly PineValue List_2b6482ae =
                    PineValue.List(
                        [Blob_Str_List, List_37740652]);

                public static readonly PineValue List_8b2f7377 =
                    PineValue.List(
                        [Blob_Str_List, List_5363db38]);

                public static readonly PineValue List_e6036e4f =
                    PineValue.List(
                        [Blob_Str_List, List_7bb74928]);

                public static readonly PineValue List_d0ea62b2 =
                    PineValue.List(
                        [Blob_Str_List, List_b0170612]);

                public static readonly PineValue List_65c1509a =
                    PineValue.List(
                        [Blob_Str_List, List_2e59bc5f]);

                public static readonly PineValue List_91dafb6f =
                    PineValue.List(
                        [Blob_Str_List, List_3adb4b83]);

                public static readonly PineValue List_a8b39634 =
                    PineValue.List(
                        [Blob_Str_List, List_118061b3]);

                public static readonly PineValue List_14fcccba =
                    PineValue.List(
                        [Blob_Str_List, List_0b491071]);

                public static readonly PineValue List_57766905 =
                    PineValue.List(
                        [Blob_Str_List, List_a198e751]);

                public static readonly PineValue List_d7ca573f =
                    PineValue.List(
                        [Blob_Str_take, List_da5eb341]);

                public static readonly PineValue List_24164017 =
                    PineValue.List(
                        [Blob_Str_equal, List_9b2e4f9b]);

                public static readonly PineValue List_af10111d =
                    PineValue.List(
                        [Blob_Str_equal, List_dd1a37bb]);

                public static readonly PineValue List_75b309a4 =
                    PineValue.List(
                        [Blob_Str_equal, List_2b6482ae]);

                public static readonly PineValue List_ddbb8f6b =
                    PineValue.List(
                        [Blob_Str_equal, List_8b2f7377]);

                public static readonly PineValue List_a1ae6ce3 =
                    PineValue.List(
                        [Blob_Str_equal, List_e6036e4f]);

                public static readonly PineValue List_36481480 =
                    PineValue.List(
                        [Blob_Str_equal, List_d0ea62b2]);

                public static readonly PineValue List_95079eff =
                    PineValue.List(
                        [Blob_Str_equal, List_65c1509a]);

                public static readonly PineValue List_1c67486d =
                    PineValue.List(
                        [Blob_Str_equal, List_91dafb6f]);

                public static readonly PineValue List_4696a1ae =
                    PineValue.List(
                        [Blob_Str_equal, List_a8b39634]);

                public static readonly PineValue List_637dd30c =
                    PineValue.List(
                        [Blob_Str_equal, List_14fcccba]);

                public static readonly PineValue List_d4e3e57f =
                    PineValue.List(
                        [Blob_Str_equal, List_57766905]);

                public static readonly PineValue List_4070bf0f =
                    PineValue.List(
                        [List_a5cbaf18, List_c32f8d23]);

                public static readonly PineValue List_eda76c75 =
                    PineValue.List(
                        [List_a5cbaf18, List_a2c8f94f]);

                public static readonly PineValue List_bbd0c3ab =
                    PineValue.List(
                        [List_a5cbaf18, List_6227bc04]);

                public static readonly PineValue List_e7f5e57b =
                    PineValue.List(
                        [List_a5cbaf18, List_8594c0bd]);

                public static readonly PineValue List_23ddbfa3 =
                    PineValue.List(
                        [List_a5cbaf18, List_11925635]);

                public static readonly PineValue List_06869e90 =
                    PineValue.List(
                        [List_a5cbaf18, List_bdf4568f]);

                public static readonly PineValue List_f039433a =
                    PineValue.List(
                        [List_a5cbaf18, List_afc020d7]);

                public static readonly PineValue List_83ba3f98 =
                    PineValue.List(
                        [List_a5cbaf18, List_1a65ed37]);

                public static readonly PineValue List_f3fe2344 =
                    PineValue.List(
                        [List_a5cbaf18, List_358ebecf]);

                public static readonly PineValue List_a5f9512a =
                    PineValue.List(
                        [List_4070bf0f]);

                public static readonly PineValue List_16b55e8b =
                    PineValue.List(
                        [List_eda76c75]);

                public static readonly PineValue List_08249d6f =
                    PineValue.List(
                        [List_bbd0c3ab]);

                public static readonly PineValue List_6ab6184e =
                    PineValue.List(
                        [List_e7f5e57b]);

                public static readonly PineValue List_e5d6867a =
                    PineValue.List(
                        [List_23ddbfa3]);

                public static readonly PineValue List_950cd0d4 =
                    PineValue.List(
                        [List_06869e90]);

                public static readonly PineValue List_d896c5cd =
                    PineValue.List(
                        [List_f039433a]);

                public static readonly PineValue List_7ec9d568 =
                    PineValue.List(
                        [List_83ba3f98]);

                public static readonly PineValue List_a694321e =
                    PineValue.List(
                        [List_f3fe2344]);

                public static readonly PineValue List_54c4b604 =
                    PineValue.List(
                        [Blob_Str_List, List_a5f9512a]);

                public static readonly PineValue List_3ccd01f9 =
                    PineValue.List(
                        [Blob_Str_List, List_16b55e8b]);

                public static readonly PineValue List_65f31d12 =
                    PineValue.List(
                        [Blob_Str_List, List_08249d6f]);

                public static readonly PineValue List_dfbab881 =
                    PineValue.List(
                        [Blob_Str_List, List_6ab6184e]);

                public static readonly PineValue List_b7ef3b06 =
                    PineValue.List(
                        [Blob_Str_List, List_e5d6867a]);

                public static readonly PineValue List_d21cda04 =
                    PineValue.List(
                        [Blob_Str_List, List_950cd0d4]);

                public static readonly PineValue List_4a790866 =
                    PineValue.List(
                        [Blob_Str_List, List_d896c5cd]);

                public static readonly PineValue List_ccad033e =
                    PineValue.List(
                        [Blob_Str_List, List_7ec9d568]);

                public static readonly PineValue List_7fc982d7 =
                    PineValue.List(
                        [Blob_Str_List, List_a694321e]);

                public static readonly PineValue List_d1125cdd =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_14382e40]);

                public static readonly PineValue List_9e9a0cdc =
                    PineValue.List(
                        [Blob_Str_length, List_d1125cdd]);

                public static readonly PineValue List_147fffd6 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_d7ca573f]);

                public static readonly PineValue List_f2ed7a61 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_24164017]);

                public static readonly PineValue List_df6df33c =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_af10111d]);

                public static readonly PineValue List_3cab333c =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_75b309a4]);

                public static readonly PineValue List_653f3279 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_ddbb8f6b]);

                public static readonly PineValue List_5f85ed23 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_a1ae6ce3]);

                public static readonly PineValue List_1b2d1a32 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_36481480]);

                public static readonly PineValue List_93b72dec =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_95079eff]);

                public static readonly PineValue List_da97fbb4 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_1c67486d]);

                public static readonly PineValue List_c2c0d1a9 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_4696a1ae]);

                public static readonly PineValue List_8ebf4631 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_637dd30c]);

                public static readonly PineValue List_58c0de4c =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_d4e3e57f]);

                public static readonly PineValue List_44990da3 =
                    PineValue.List(
                        [List_bc0ffa85, List_d9dcda92]);

                public static readonly PineValue List_75e1b2fd =
                    PineValue.List(
                        [List_44990da3]);

                public static readonly PineValue List_4e666950 =
                    PineValue.List(
                        [Blob_Str_List, List_75e1b2fd]);

                public static readonly PineValue List_972ec495 =
                    PineValue.List(
                        [List_147fffd6, List_4c2609c3]);

                public static readonly PineValue List_8dc97585 =
                    PineValue.List(
                        [List_147fffd6, List_c3b08663]);

                public static readonly PineValue List_7cd434e2 =
                    PineValue.List(
                        [List_147fffd6, List_e844984b]);

                public static readonly PineValue List_283df93a =
                    PineValue.List(
                        [List_147fffd6, List_a9196577]);

                public static readonly PineValue List_ebfbbb42 =
                    PineValue.List(
                        [List_147fffd6, List_9728f698]);

                public static readonly PineValue List_cd3c531a =
                    PineValue.List(
                        [List_147fffd6, List_86fad7dd]);

                public static readonly PineValue List_03208aa8 =
                    PineValue.List(
                        [List_147fffd6, List_c005c994]);

                public static readonly PineValue List_bf8e6307 =
                    PineValue.List(
                        [List_147fffd6, List_33c6f4ad]);

                public static readonly PineValue List_46d7bb4a =
                    PineValue.List(
                        [List_147fffd6, List_1a0b8610]);

                public static readonly PineValue List_02261755 =
                    PineValue.List(
                        [List_147fffd6, List_ecf5f39d]);

                public static readonly PineValue List_1cab1ec5 =
                    PineValue.List(
                        [List_972ec495]);

                public static readonly PineValue List_db749ca1 =
                    PineValue.List(
                        [List_8dc97585]);

                public static readonly PineValue List_2fc31437 =
                    PineValue.List(
                        [List_7cd434e2]);

                public static readonly PineValue List_d8c2b6dc =
                    PineValue.List(
                        [List_283df93a]);

                public static readonly PineValue List_9c9c8886 =
                    PineValue.List(
                        [List_ebfbbb42]);

                public static readonly PineValue List_1ad14b7b =
                    PineValue.List(
                        [List_cd3c531a]);

                public static readonly PineValue List_f74fcd51 =
                    PineValue.List(
                        [List_03208aa8]);

                public static readonly PineValue List_041622e9 =
                    PineValue.List(
                        [List_bf8e6307]);

                public static readonly PineValue List_bf3d44d0 =
                    PineValue.List(
                        [List_46d7bb4a]);

                public static readonly PineValue List_d53f72de =
                    PineValue.List(
                        [List_02261755]);

                public static readonly PineValue List_dbd07d5a =
                    PineValue.List(
                        [Blob_Str_List, List_1cab1ec5]);

                public static readonly PineValue List_a228d941 =
                    PineValue.List(
                        [Blob_Str_List, List_db749ca1]);

                public static readonly PineValue List_9d3dd2b9 =
                    PineValue.List(
                        [Blob_Str_List, List_2fc31437]);

                public static readonly PineValue List_3dbe51bb =
                    PineValue.List(
                        [Blob_Str_List, List_d8c2b6dc]);

                public static readonly PineValue List_76afddf9 =
                    PineValue.List(
                        [Blob_Str_List, List_9c9c8886]);

                public static readonly PineValue List_7b97cebf =
                    PineValue.List(
                        [Blob_Str_List, List_1ad14b7b]);

                public static readonly PineValue List_43b511e6 =
                    PineValue.List(
                        [Blob_Str_List, List_f74fcd51]);

                public static readonly PineValue List_f9fc4a76 =
                    PineValue.List(
                        [Blob_Str_List, List_041622e9]);

                public static readonly PineValue List_9a4b564f =
                    PineValue.List(
                        [Blob_Str_List, List_bf3d44d0]);

                public static readonly PineValue List_75bd24ea =
                    PineValue.List(
                        [Blob_Str_List, List_d53f72de]);

                public static readonly PineValue List_2c6083ca =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_9e9a0cdc]);

                public static readonly PineValue List_da91fc98 =
                    PineValue.List(
                        [Blob_Str_equal, List_dbd07d5a]);

                public static readonly PineValue List_e25acbcc =
                    PineValue.List(
                        [Blob_Str_equal, List_a228d941]);

                public static readonly PineValue List_9b0e5640 =
                    PineValue.List(
                        [Blob_Str_equal, List_9d3dd2b9]);

                public static readonly PineValue List_3531bee3 =
                    PineValue.List(
                        [Blob_Str_equal, List_3dbe51bb]);

                public static readonly PineValue List_0dd4cf27 =
                    PineValue.List(
                        [Blob_Str_equal, List_76afddf9]);

                public static readonly PineValue List_1822bf9f =
                    PineValue.List(
                        [Blob_Str_equal, List_7b97cebf]);

                public static readonly PineValue List_b21cc8a5 =
                    PineValue.List(
                        [Blob_Str_equal, List_43b511e6]);

                public static readonly PineValue List_b8872664 =
                    PineValue.List(
                        [Blob_Str_equal, List_f9fc4a76]);

                public static readonly PineValue List_118b39c9 =
                    PineValue.List(
                        [Blob_Str_equal, List_9a4b564f]);

                public static readonly PineValue List_c3c9441c =
                    PineValue.List(
                        [Blob_Str_equal, List_75bd24ea]);

                public static readonly PineValue List_890cb2fa =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_da91fc98]);

                public static readonly PineValue List_71cb9f8d =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_e25acbcc]);

                public static readonly PineValue List_2302c126 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_9b0e5640]);

                public static readonly PineValue List_421486a4 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_3531bee3]);

                public static readonly PineValue List_41b9d240 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_0dd4cf27]);

                public static readonly PineValue List_524a6624 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_1822bf9f]);

                public static readonly PineValue List_1df81331 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_b21cc8a5]);

                public static readonly PineValue List_bb3968df =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_b8872664]);

                public static readonly PineValue List_89dda7ec =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_118b39c9]);

                public static readonly PineValue List_b910054d =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_c3c9441c]);

                public static readonly PineValue List_8ac7a72b =
                    PineValue.List(
                        [List_bc0ffa85, List_98a55f77]);

                public static readonly PineValue List_d8192640 =
                    PineValue.List(
                        [List_8ac7a72b]);

                public static readonly PineValue List_3b1a6e23 =
                    PineValue.List(
                        [Blob_Str_List, List_d8192640]);

                public static readonly PineValue List_481ba5c9 =
                    PineValue.List(
                        [List_976730e9, List_54c4b604]);

                public static readonly PineValue List_ea4abdf1 =
                    PineValue.List(
                        [List_976730e9, List_3ccd01f9]);

                public static readonly PineValue List_c24147bf =
                    PineValue.List(
                        [List_976730e9, List_65f31d12]);

                public static readonly PineValue List_8252c7c7 =
                    PineValue.List(
                        [List_976730e9, List_dfbab881]);

                public static readonly PineValue List_75e495bf =
                    PineValue.List(
                        [List_976730e9, List_b7ef3b06]);

                public static readonly PineValue List_46711533 =
                    PineValue.List(
                        [List_976730e9, List_d21cda04]);

                public static readonly PineValue List_c496aa10 =
                    PineValue.List(
                        [List_976730e9, List_4a790866]);

                public static readonly PineValue List_d5dab593 =
                    PineValue.List(
                        [List_976730e9, List_ccad033e]);

                public static readonly PineValue List_fe4df18e =
                    PineValue.List(
                        [List_976730e9, List_7fc982d7]);

                public static readonly PineValue List_b5221b1b =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_481ba5c9]);

                public static readonly PineValue List_10ee6dd0 =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_ea4abdf1]);

                public static readonly PineValue List_a691016a =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_c24147bf]);

                public static readonly PineValue List_b8b71b30 =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_8252c7c7]);

                public static readonly PineValue List_c86be949 =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_75e495bf]);

                public static readonly PineValue List_6906b28f =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_46711533]);

                public static readonly PineValue List_4c8b632e =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_c496aa10]);

                public static readonly PineValue List_c7ba562f =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_d5dab593]);

                public static readonly PineValue List_644d6208 =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_fe4df18e]);

                public static readonly PineValue List_f3b69b33 =
                    PineValue.List(
                        [List_6dbb5f29, List_4e666950]);

                public static readonly PineValue List_f9e1aa3d =
                    PineValue.List(
                        [List_46a1ad58, List_4af1f0ec, List_d64a63d0]);

                public static readonly PineValue List_0bf4cc22 =
                    PineValue.List(
                        [List_f9e1aa3d]);

                public static readonly PineValue List_699a7e32 =
                    PineValue.List(
                        [Blob_Str_List, List_0bf4cc22]);

                public static readonly PineValue List_b88b0efa =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_f3b69b33]);

                public static readonly PineValue List_3d63742a =
                    PineValue.List(
                        [List_6dbb5f29, List_3b1a6e23]);

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

                public static readonly PineValue List_f4f41c08 =
                    PineValue.List(
                        [List_a5cbaf18, List_699a7e32]);

                public static readonly PineValue List_90715299 =
                    PineValue.List(
                        [List_f4f41c08]);

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

                public static readonly PineValue List_e160db8a =
                    PineValue.List(
                        [Blob_Str_List, List_90715299]);

                public static readonly PineValue List_86163fd8 =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_3d63742a]);

                public static readonly PineValue List_9f610e07 =
                    PineValue.List(
                        [Blob_Str_head, List_86163fd8]);

                public static readonly PineValue List_943fedfe =
                    PineValue.List(
                        [List_0dcd86c0, List_86163fd8]);

                public static readonly PineValue List_db6e2c43 =
                    PineValue.List(
                        [List_943fedfe]);

                public static readonly PineValue List_17bf73cf =
                    PineValue.List(
                        [Blob_Str_List, List_db6e2c43]);

                public static readonly PineValue List_ce9cb925 =
                    PineValue.List(
                        [Blob_Str_skip, List_17bf73cf]);

                public static readonly PineValue List_4977ea40 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_9f610e07]);

                public static readonly PineValue List_21f29a75 =
                    PineValue.List(
                        [List_6cab91e9, List_d1125cdd]);

                public static readonly PineValue List_6e6af778 =
                    PineValue.List(
                        [List_21f29a75]);

                public static readonly PineValue List_8f24da42 =
                    PineValue.List(
                        [Blob_Str_List, List_6e6af778]);

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

                public static readonly PineValue List_a6b2b774 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_ce9cb925]);

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

                public static readonly PineValue List_c5a13bb8 =
                    PineValue.List(
                        [Blob_Str_head, List_a6b2b774]);

                public static readonly PineValue List_107d83e1 =
                    PineValue.List(
                        [List_2c6083ca, List_1da3871f]);

                public static readonly PineValue List_6a789d80 =
                    PineValue.List(
                        [List_107d83e1]);

                public static readonly PineValue List_b74643b3 =
                    PineValue.List(
                        [List_976730e9, List_e160db8a]);

                public static readonly PineValue List_00e3a1ad =
                    PineValue.List(
                        [Blob_Str_List, List_6a789d80]);

                public static readonly PineValue List_97ee12a9 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_c5a13bb8]);

                public static readonly PineValue List_6f3e5f31 =
                    PineValue.List(
                        [Blob_Str_equal, List_00e3a1ad]);

                public static readonly PineValue List_9abec2dd =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_b74643b3]);

                public static readonly PineValue List_ef2f17f4 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_6f3e5f31]);

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

                public static readonly PineValue List_93d11250 =
                    PineValue.List(
                        [List_ffb3657e, List_8f24da42]);

                public static readonly PineValue List_220685f6 =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_93d11250]);

                public static readonly PineValue List_1071faa7 =
                    PineValue.List(
                        [List_b43468c9, List_4af1f0ec, List_4977ea40]);

                public static readonly PineValue List_3d8d507c =
                    PineValue.List(
                        [List_1071faa7]);

                public static readonly PineValue List_5891f6c8 =
                    PineValue.List(
                        [Blob_Str_List, List_3d8d507c]);

                public static readonly PineValue List_7f6f1489 =
                    PineValue.List(
                        [List_a5cbaf18, List_5891f6c8]);

                public static readonly PineValue List_3f26b2a6 =
                    PineValue.List(
                        [List_7f6f1489]);

                public static readonly PineValue List_b3898a24 =
                    PineValue.List(
                        [Blob_Str_List, List_3f26b2a6]);

                public static readonly PineValue List_a487734f =
                    PineValue.List(
                        [List_a3ae724a, List_97ee12a9]);

                public static readonly PineValue List_679fccff =
                    PineValue.List(
                        [List_a487734f]);

                public static readonly PineValue List_8cf77e15 =
                    PineValue.List(
                        [Blob_Str_List, List_679fccff]);

                public static readonly PineValue List_b176d99f =
                    PineValue.List(
                        [List_fde32251, List_97ee12a9]);

                public static readonly PineValue List_969224ba =
                    PineValue.List(
                        [List_b176d99f]);

                public static readonly PineValue List_cc7bd471 =
                    PineValue.List(
                        [Blob_Str_List, List_969224ba]);

                public static readonly PineValue List_ca0978d3 =
                    PineValue.List(
                        [List_ffb3657e, List_1da3871f, List_6cab91e9, List_d1125cdd]);

                public static readonly PineValue List_f14ae252 =
                    PineValue.List(
                        [List_ca0978d3]);

                public static readonly PineValue List_4303578c =
                    PineValue.List(
                        [Blob_Str_List, List_f14ae252]);

                public static readonly PineValue List_a87f525f =
                    PineValue.List(
                        [List_2f1c6993, List_4303578c]);

                public static readonly PineValue List_78238f9c =
                    PineValue.List(
                        [List_a87f525f]);

                public static readonly PineValue List_d5786383 =
                    PineValue.List(
                        [Blob_Str_List, List_78238f9c]);

                public static readonly PineValue List_811016b0 =
                    PineValue.List(
                        [List_58c0de4c, List_def272c2, List_644d6208]);

                public static readonly PineValue List_af7680af =
                    PineValue.List(
                        [Blob_Str_Conditional, List_811016b0]);

                public static readonly PineValue List_706aba10 =
                    PineValue.List(
                        [List_d17c51de, List_3f936680, List_8cf77e15]);

                public static readonly PineValue List_afdc880a =
                    PineValue.List(
                        [Blob_Str_Conditional, List_706aba10]);

                public static readonly PineValue List_8e774fb4 =
                    PineValue.List(
                        [List_b910054d, List_2966387f, List_735859ac]);

                public static readonly PineValue List_896090dc =
                    PineValue.List(
                        [Blob_Str_Conditional, List_8e774fb4]);

                public static readonly PineValue List_8285ba2b =
                    PineValue.List(
                        [List_8ebf4631, List_af7680af, List_c7ba562f]);

                public static readonly PineValue List_bc8fb405 =
                    PineValue.List(
                        [Blob_Str_Conditional, List_8285ba2b]);

                public static readonly PineValue List_1c6b5224 =
                    PineValue.List(
                        [List_ef2f17f4, List_d5786383, List_220685f6]);

                public static readonly PineValue List_d6680f3e =
                    PineValue.List(
                        [Blob_Str_Conditional, List_1c6b5224]);

                public static readonly PineValue List_e96b749e =
                    PineValue.List(
                        [List_89dda7ec, List_896090dc, List_29bfa820]);

                public static readonly PineValue List_bb9d158b =
                    PineValue.List(
                        [Blob_Str_Conditional, List_e96b749e]);

                public static readonly PineValue List_47d20079 =
                    PineValue.List(
                        [List_02aa03c8, List_afdc880a, List_cc7bd471]);

                public static readonly PineValue List_26572a9d =
                    PineValue.List(
                        [Blob_Str_Conditional, List_47d20079]);

                public static readonly PineValue List_633ab05a =
                    PineValue.List(
                        [List_26572a9d]);

                public static readonly PineValue List_3e76f1b2 =
                    PineValue.List(
                        [Blob_Str_Literal, List_633ab05a]);

                public static readonly PineValue List_8ec62786 =
                    PineValue.List(
                        [List_c2c0d1a9, List_bc8fb405, List_4c8b632e]);

                public static readonly PineValue List_d078be00 =
                    PineValue.List(
                        [Blob_Str_Conditional, List_8ec62786]);

                public static readonly PineValue List_d21ba4f0 =
                    PineValue.List(
                        [List_04fe9790, List_7e881476, List_d6680f3e]);

                public static readonly PineValue List_a410766d =
                    PineValue.List(
                        [Blob_Str_Conditional, List_d21ba4f0]);

                public static readonly PineValue List_0a786f4b =
                    PineValue.List(
                        [List_a5cbaf18, List_a410766d, List_4de8c353]);

                public static readonly PineValue List_a1070209 =
                    PineValue.List(
                        [List_0a786f4b]);

                public static readonly PineValue List_faa15f75 =
                    PineValue.List(
                        [Blob_Str_List, List_a1070209]);

                public static readonly PineValue List_48de095f =
                    PineValue.List(
                        [List_a5cbaf18, List_faa15f75]);

                public static readonly PineValue List_7190788e =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_48de095f]);

                public static readonly PineValue List_c24cf806 =
                    PineValue.List(
                        [List_ad2fad57, List_7190788e, List_f1126ab7]);

                public static readonly PineValue List_dc8c7872 =
                    PineValue.List(
                        [Blob_Str_Conditional, List_c24cf806]);

                public static readonly PineValue List_83576aa3 =
                    PineValue.List(
                        [List_3e76f1b2, List_b3898a24]);

                public static readonly PineValue List_a2f38178 =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_83576aa3]);

                public static readonly PineValue List_1c2102bd =
                    PineValue.List(
                        [List_da97fbb4, List_d078be00, List_6906b28f]);

                public static readonly PineValue List_99b49a8c =
                    PineValue.List(
                        [Blob_Str_Conditional, List_1c2102bd]);

                public static readonly PineValue List_1df2444b =
                    PineValue.List(
                        [List_bb3968df, List_bb9d158b, List_25e77689]);

                public static readonly PineValue List_30941767 =
                    PineValue.List(
                        [Blob_Str_Conditional, List_1df2444b]);

                public static readonly PineValue List_3a0d12fc =
                    PineValue.List(
                        [List_f2ed7a61, List_b88b0efa, List_a2f38178]);

                public static readonly PineValue List_4cf95911 =
                    PineValue.List(
                        [Blob_Str_Conditional, List_3a0d12fc]);

                public static readonly PineValue List_683cfcad =
                    PineValue.List(
                        [List_93b72dec, List_99b49a8c, List_c86be949]);

                public static readonly PineValue List_0080fc89 =
                    PineValue.List(
                        [Blob_Str_Conditional, List_683cfcad]);

                public static readonly PineValue List_a02735d9 =
                    PineValue.List(
                        [List_1df81331, List_30941767, List_7ed08cae]);

                public static readonly PineValue List_c5a32dac =
                    PineValue.List(
                        [Blob_Str_Conditional, List_a02735d9]);

                public static readonly PineValue List_90897146 =
                    PineValue.List(
                        [List_1b2d1a32, List_0080fc89, List_b8b71b30]);

                public static readonly PineValue List_35a37145 =
                    PineValue.List(
                        [Blob_Str_Conditional, List_90897146]);

                public static readonly PineValue List_4fe84dd0 =
                    PineValue.List(
                        [List_524a6624, List_c5a32dac, List_1fca4f4c]);

                public static readonly PineValue List_2e47160a =
                    PineValue.List(
                        [Blob_Str_Conditional, List_4fe84dd0]);

                public static readonly PineValue List_a6bde584 =
                    PineValue.List(
                        [List_5f85ed23, List_35a37145, List_a691016a]);

                public static readonly PineValue List_9411898d =
                    PineValue.List(
                        [Blob_Str_Conditional, List_a6bde584]);

                public static readonly PineValue List_120288be =
                    PineValue.List(
                        [List_653f3279, List_9411898d, List_10ee6dd0]);

                public static readonly PineValue List_231d85a0 =
                    PineValue.List(
                        [Blob_Str_Conditional, List_120288be]);

                public static readonly PineValue List_1d7fb539 =
                    PineValue.List(
                        [List_41b9d240, List_2e47160a, List_2bdc172b]);

                public static readonly PineValue List_40810f3e =
                    PineValue.List(
                        [Blob_Str_Conditional, List_1d7fb539]);

                public static readonly PineValue List_b31aaea8 =
                    PineValue.List(
                        [List_3cab333c, List_231d85a0, List_b5221b1b]);

                public static readonly PineValue List_47615e59 =
                    PineValue.List(
                        [Blob_Str_Conditional, List_b31aaea8]);

                public static readonly PineValue List_94e5fb7d =
                    PineValue.List(
                        [List_421486a4, List_40810f3e, List_71ac3637]);

                public static readonly PineValue List_131a122b =
                    PineValue.List(
                        [Blob_Str_Conditional, List_94e5fb7d]);

                public static readonly PineValue List_72481f4c =
                    PineValue.List(
                        [List_df6df33c, List_47615e59, List_8c9ba17c]);

                public static readonly PineValue List_46782f74 =
                    PineValue.List(
                        [Blob_Str_Conditional, List_72481f4c]);

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

            """"
            .Trim());
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
