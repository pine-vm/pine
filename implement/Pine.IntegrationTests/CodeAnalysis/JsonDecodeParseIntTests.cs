using AwesomeAssertions;
using Pine.Core;
using Pine.Core.CodeAnalysis;
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

        var asCsharp = Pine.PineVM.StaticProgramCSharp.FromStaticProgram(staticProgram);

        var testClass = asCsharp.Classes[new DeclQualifiedName([], "Test")];

        var testClassCsharpText = testClass.RenderToString();

        // Console.WriteLine("C# code:\n" + csharpText);

        testClassCsharpText.Trim().Should().Be(
            """
            public static class Test
            {
                public static PineValue parseInt(
                    PineValue param_1_0,
                    PineValue param_1_1)
                {
                    if (KernelFunction.ApplyKernelFunctionGeneric(
                        "take",
                        PineValue.List(
                            [
                                IntegerEncoding.EncodeSignedInteger(4),
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "skip",
                                    PineValue.List(
                                        [param_1_1, param_1_0]))
                            ])) == StringEncoding.ValueFromString("-"))
                    {
                        PineValue local_000 =
                            Test.parseUnsignedInt(
                                param_1_0,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_1,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));

                        PineValue local_001 =
                            KernelFunction.ApplyKernelFunctionGeneric("head", local_000);

                        PineValue local_002 =
                            KernelFunction.ApplyKernelFunctionGeneric("head", local_001);

                        if (StringEncoding.ValueFromString("Ok") == local_002)
                        {
                            return
                                PineValue.List(
                                    [
                                        PineValue.List(
                                            [
                                                StringEncoding.ValueFromString("Ok"),
                                                PineValue.List(
                                                    [
                                                        KernelFunction.ApplyKernelFunctionGeneric(
                                                            "int_mul",
                                                            PineValue.List(
                                                                [
                                                                    IntegerEncoding.EncodeSignedInteger(-1),
                                                                    KernelFunction.ApplyKernelFunctionGeneric(
                                                                        "head",
                                                                        KernelFunction.ApplyKernelFunctionGeneric(
                                                                            "head",
                                                                            KernelFunction.ApplyKernelFunctionGeneric(
                                                                                "skip",
                                                                                PineValue.List(
                                                                                    [
                                                                                        IntegerEncoding.EncodeSignedInteger(1),
                                                                                        local_001
                                                                                    ]))))
                                                                ]))
                                                    ])
                                            ]),
                                        KernelFunction.ApplyKernelFunctionGeneric(
                                            "head",
                                            KernelFunction.ApplyKernelFunctionGeneric(
                                                "skip",
                                                PineValue.List(
                                                    [
                                                        IntegerEncoding.EncodeSignedInteger(1),
                                                        local_000
                                                    ])))
                                    ]);
                        }

                        if (StringEncoding.ValueFromString("Err") == local_002)
                        {
                            return
                                PineValue.List(
                                    [
                                        PineValue.List(
                                            [
                                                StringEncoding.ValueFromString("Err"),
                                                PineValue.List(
                                                    [
                                                        KernelFunction.ApplyKernelFunctionGeneric(
                                                            "head",
                                                            KernelFunction.ApplyKernelFunctionGeneric(
                                                                "head",
                                                                KernelFunction.ApplyKernelFunctionGeneric(
                                                                    "skip",
                                                                    PineValue.List(
                                                                        [
                                                                            IntegerEncoding.EncodeSignedInteger(1),
                                                                            local_001
                                                                        ]))))
                                                    ])
                                            ]),
                                        KernelFunction.ApplyKernelFunctionGeneric(
                                            "head",
                                            KernelFunction.ApplyKernelFunctionGeneric(
                                                "skip",
                                                PineValue.List(
                                                    [
                                                        IntegerEncoding.EncodeSignedInteger(1),
                                                        local_000
                                                    ])))
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
                        KernelFunction.ApplyKernelFunctionGeneric(
                            "take",
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(4),
                                    KernelFunction.ApplyKernelFunctionGeneric(
                                        "skip",
                                        PineValue.List(
                                            [param_1_1, param_1_0]))
                                ]));

                    if (local_000 == StringEncoding.ValueFromString("0"))
                    {
                        return
                            PineValue.List(
                                [
                                    PineValue.List(
                                        [
                                            StringEncoding.ValueFromString("Ok"),
                                            PineValue.List(
                                                [
                                                    IntegerEncoding.EncodeSignedInteger(0)
                                                ])
                                        ]),
                                    KernelFunction.ApplyKernelFunctionGeneric(
                                        "int_add",
                                        PineValue.List(
                                            [
                                                param_1_1,
                                                IntegerEncoding.EncodeSignedInteger(4)
                                            ]))
                                ]);
                    }

                    if (local_000 == StringEncoding.ValueFromString("1"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                IntegerEncoding.EncodeSignedInteger(1),
                                param_1_0,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_1,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    if (local_000 == StringEncoding.ValueFromString("2"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                IntegerEncoding.EncodeSignedInteger(2),
                                param_1_0,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_1,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    if (local_000 == StringEncoding.ValueFromString("3"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                IntegerEncoding.EncodeSignedInteger(3),
                                param_1_0,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_1,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    if (local_000 == StringEncoding.ValueFromString("4"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                IntegerEncoding.EncodeSignedInteger(4),
                                param_1_0,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_1,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    if (local_000 == StringEncoding.ValueFromString("5"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                IntegerEncoding.EncodeSignedInteger(5),
                                param_1_0,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_1,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    if (local_000 == StringEncoding.ValueFromString("6"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                IntegerEncoding.EncodeSignedInteger(6),
                                param_1_0,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_1,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    if (local_000 == StringEncoding.ValueFromString("7"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                IntegerEncoding.EncodeSignedInteger(7),
                                param_1_0,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_1,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    if (local_000 == StringEncoding.ValueFromString("8"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                IntegerEncoding.EncodeSignedInteger(8),
                                param_1_0,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_1,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    if (local_000 == StringEncoding.ValueFromString("9"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                IntegerEncoding.EncodeSignedInteger(9),
                                param_1_0,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_1,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    return
                        PineValue.List(
                            [
                                PineValue.List(
                                    [
                                        StringEncoding.ValueFromString("Err"),
                                        PineValue.List(
                                            [
                                                PineValue.List(
                                                    [
                                                        StringEncoding.ValueFromString("String"),
                                                        PineValue.List(
                                                            [
                                                                StringEncoding.ValueFromString("Expecting a digit")
                                                            ])
                                                    ])
                                            ])
                                    ]),
                                param_1_1
                            ]);
                }


                public static PineValue parseUnsignedIntRec(
                    PineValue param_1_0,
                    PineValue param_1_1,
                    PineValue param_1_2)
                {
                    PineValue local_000 =
                        KernelFunction.ApplyKernelFunctionGeneric(
                            "take",
                            PineValue.List(
                                [
                                    IntegerEncoding.EncodeSignedInteger(4),
                                    KernelFunction.ApplyKernelFunctionGeneric(
                                        "skip",
                                        PineValue.List(
                                            [param_1_2, param_1_1]))
                                ]));

                    if (local_000 == StringEncoding.ValueFromString("0"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_mul",
                                    PineValue.List(
                                        [
                                            param_1_0,
                                            IntegerEncoding.EncodeSignedInteger(10)
                                        ])),
                                param_1_1,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_2,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    if (local_000 == StringEncoding.ValueFromString("1"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            KernelFunction.ApplyKernelFunctionGeneric(
                                                "int_mul",
                                                PineValue.List(
                                                    [
                                                        param_1_0,
                                                        IntegerEncoding.EncodeSignedInteger(10)
                                                    ])),
                                            IntegerEncoding.EncodeSignedInteger(1)
                                        ])),
                                param_1_1,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_2,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    if (local_000 == StringEncoding.ValueFromString("2"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            KernelFunction.ApplyKernelFunctionGeneric(
                                                "int_mul",
                                                PineValue.List(
                                                    [
                                                        param_1_0,
                                                        IntegerEncoding.EncodeSignedInteger(10)
                                                    ])),
                                            IntegerEncoding.EncodeSignedInteger(2)
                                        ])),
                                param_1_1,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_2,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    if (local_000 == StringEncoding.ValueFromString("3"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            KernelFunction.ApplyKernelFunctionGeneric(
                                                "int_mul",
                                                PineValue.List(
                                                    [
                                                        param_1_0,
                                                        IntegerEncoding.EncodeSignedInteger(10)
                                                    ])),
                                            IntegerEncoding.EncodeSignedInteger(3)
                                        ])),
                                param_1_1,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_2,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    if (local_000 == StringEncoding.ValueFromString("4"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            KernelFunction.ApplyKernelFunctionGeneric(
                                                "int_mul",
                                                PineValue.List(
                                                    [
                                                        param_1_0,
                                                        IntegerEncoding.EncodeSignedInteger(10)
                                                    ])),
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])),
                                param_1_1,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_2,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    if (local_000 == StringEncoding.ValueFromString("5"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            KernelFunction.ApplyKernelFunctionGeneric(
                                                "int_mul",
                                                PineValue.List(
                                                    [
                                                        param_1_0,
                                                        IntegerEncoding.EncodeSignedInteger(10)
                                                    ])),
                                            IntegerEncoding.EncodeSignedInteger(5)
                                        ])),
                                param_1_1,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_2,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    if (local_000 == StringEncoding.ValueFromString("6"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            KernelFunction.ApplyKernelFunctionGeneric(
                                                "int_mul",
                                                PineValue.List(
                                                    [
                                                        param_1_0,
                                                        IntegerEncoding.EncodeSignedInteger(10)
                                                    ])),
                                            IntegerEncoding.EncodeSignedInteger(6)
                                        ])),
                                param_1_1,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_2,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    if (local_000 == StringEncoding.ValueFromString("7"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            KernelFunction.ApplyKernelFunctionGeneric(
                                                "int_mul",
                                                PineValue.List(
                                                    [
                                                        param_1_0,
                                                        IntegerEncoding.EncodeSignedInteger(10)
                                                    ])),
                                            IntegerEncoding.EncodeSignedInteger(7)
                                        ])),
                                param_1_1,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_2,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    if (local_000 == StringEncoding.ValueFromString("8"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            KernelFunction.ApplyKernelFunctionGeneric(
                                                "int_mul",
                                                PineValue.List(
                                                    [
                                                        param_1_0,
                                                        IntegerEncoding.EncodeSignedInteger(10)
                                                    ])),
                                            IntegerEncoding.EncodeSignedInteger(8)
                                        ])),
                                param_1_1,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_2,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    if (local_000 == StringEncoding.ValueFromString("9"))
                    {
                        return
                            Test.parseUnsignedIntRec(
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            KernelFunction.ApplyKernelFunctionGeneric(
                                                "int_mul",
                                                PineValue.List(
                                                    [
                                                        param_1_0,
                                                        IntegerEncoding.EncodeSignedInteger(10)
                                                    ])),
                                            IntegerEncoding.EncodeSignedInteger(9)
                                        ])),
                                param_1_1,
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_add",
                                    PineValue.List(
                                        [
                                            param_1_2,
                                            IntegerEncoding.EncodeSignedInteger(4)
                                        ])));
                    }

                    return
                        PineValue.List(
                            [
                                PineValue.List(
                                    [
                                        StringEncoding.ValueFromString("Ok"),
                                        PineValue.List(
                                            [param_1_0])
                                    ]),
                                param_1_2
                            ]);
                }
            }
            

            """.Trim());
    }
}

// Sketching out the C# class we expect to generate:

public static class Test
{
    public static PineValue parseUnsignedInt_val_00 =
        PineValue.List
        ([
            CommonValues.Tag_name_value_Err,
            PineValue.List([StringEncoding.ValueFromString("Expecting a digit")]),
        ]);

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

    public static PineValue parseInt_generic(PineValue env)
    {
        var param_1_0 =
            Common.ValueFromPathInValueOrEmptyList(env, [1, 0]);

        var param_1_1 =
            Common.ValueFromPathInValueOrEmptyList(env, [1, 1]);

        return parseInt(param_1_0, param_1_1);
    }

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
        if (KernelFunction.ApplyKernelFunctionGeneric(
            "take",
            PineValue.List(
                [
                    IntegerEncoding.EncodeSignedInteger(4),
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "skip",
                        PineValue.List(
                            [param_1_1, param_1_0]))
                ])) == StringEncoding.ValueFromString("-"))
        {
            PineValue local_000 =
                Test.parseUnsignedInt(
                    param_1_0,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_1,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));

            PineValue local_001 =
                KernelFunction.ApplyKernelFunctionGeneric("head", local_000);

            PineValue local_002 =
                KernelFunction.ApplyKernelFunctionGeneric("head", local_001);

            if (StringEncoding.ValueFromString("Ok") == local_002)
            {
                return
                    PineValue.List(
                        [
                            PineValue.List(
                                [
                                    StringEncoding.ValueFromString("Ok"),
                                    PineValue.List(
                                        [
                                            KernelFunction.ApplyKernelFunctionGeneric(
                                                "int_mul",
                                                PineValue.List(
                                                    [
                                                        IntegerEncoding.EncodeSignedInteger(-1),
                                                        KernelFunction.ApplyKernelFunctionGeneric(
                                                            "head",
                                                            KernelFunction.ApplyKernelFunctionGeneric(
                                                                "head",
                                                                KernelFunction.ApplyKernelFunctionGeneric(
                                                                    "skip",
                                                                    PineValue.List(
                                                                        [
                                                                            IntegerEncoding.EncodeSignedInteger(1),
                                                                            local_001
                                                                        ]))))
                                                    ]))
                                        ])
                                ]),
                            KernelFunction.ApplyKernelFunctionGeneric(
                                "head",
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "skip",
                                    PineValue.List(
                                        [
                                            IntegerEncoding.EncodeSignedInteger(1),
                                            local_000
                                        ])))
                        ]);
            }
            if (StringEncoding.ValueFromString("Err") == local_002)
            {
                return
                    PineValue.List(
                        [
                            PineValue.List(
                                [
                                    StringEncoding.ValueFromString("Err"),
                                    PineValue.List(
                                        [
                                            KernelFunction.ApplyKernelFunctionGeneric(
                                                "head",
                                                KernelFunction.ApplyKernelFunctionGeneric(
                                                    "head",
                                                    KernelFunction.ApplyKernelFunctionGeneric(
                                                        "skip",
                                                        PineValue.List(
                                                            [
                                                                IntegerEncoding.EncodeSignedInteger(1),
                                                                local_001
                                                            ]))))
                                        ])
                                ]),
                            KernelFunction.ApplyKernelFunctionGeneric(
                                "head",
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "skip",
                                    PineValue.List(
                                        [
                                            IntegerEncoding.EncodeSignedInteger(1),
                                            local_000
                                        ])))
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
            KernelFunction.ApplyKernelFunctionGeneric(
                "take",
                PineValue.List(
                    [
                        IntegerEncoding.EncodeSignedInteger(4),
                        KernelFunction.ApplyKernelFunctionGeneric(
                            "skip",
                            PineValue.List(
                                [param_1_1, param_1_0]))
                    ]));

        if (local_000 == StringEncoding.ValueFromString("0"))
        {
            return
                PineValue.List(
                    [
                        PineValue.List(
                            [
                                StringEncoding.ValueFromString("Ok"),
                                PineValue.List(
                                    [
                                        IntegerEncoding.EncodeSignedInteger(0)
                                    ])
                            ]),
                        KernelFunction.ApplyKernelFunctionGeneric(
                            "int_add",
                            PineValue.List(
                                [
                                    param_1_1,
                                    IntegerEncoding.EncodeSignedInteger(4)
                                ]))
                    ]);
        }
        if (local_000 == StringEncoding.ValueFromString("1"))
        {
            return
                Test.parseUnsignedIntRec(
                    IntegerEncoding.EncodeSignedInteger(1),
                    param_1_0,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_1,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        if (local_000 == StringEncoding.ValueFromString("2"))
        {
            return
                Test.parseUnsignedIntRec(
                    IntegerEncoding.EncodeSignedInteger(2),
                    param_1_0,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_1,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        if (local_000 == StringEncoding.ValueFromString("3"))
        {
            return
                Test.parseUnsignedIntRec(
                    IntegerEncoding.EncodeSignedInteger(3),
                    param_1_0,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_1,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        if (local_000 == StringEncoding.ValueFromString("4"))
        {
            return
                Test.parseUnsignedIntRec(
                    IntegerEncoding.EncodeSignedInteger(4),
                    param_1_0,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_1,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        if (local_000 == StringEncoding.ValueFromString("5"))
        {
            return
                Test.parseUnsignedIntRec(
                    IntegerEncoding.EncodeSignedInteger(5),
                    param_1_0,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_1,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        if (local_000 == StringEncoding.ValueFromString("6"))
        {
            return
                Test.parseUnsignedIntRec(
                    IntegerEncoding.EncodeSignedInteger(6),
                    param_1_0,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_1,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        if (local_000 == StringEncoding.ValueFromString("7"))
        {
            return
                Test.parseUnsignedIntRec(
                    IntegerEncoding.EncodeSignedInteger(7),
                    param_1_0,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_1,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        if (local_000 == StringEncoding.ValueFromString("8"))
        {
            return
                Test.parseUnsignedIntRec(
                    IntegerEncoding.EncodeSignedInteger(8),
                    param_1_0,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_1,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        if (local_000 == StringEncoding.ValueFromString("9"))
        {
            return
                Test.parseUnsignedIntRec(
                    IntegerEncoding.EncodeSignedInteger(9),
                    param_1_0,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_1,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        return
            PineValue.List(
                [
                    PineValue.List(
                        [
                            StringEncoding.ValueFromString("Err"),
                            PineValue.List(
                                [
                                    PineValue.List(
                                        [
                                            StringEncoding.ValueFromString("String"),
                                            PineValue.List(
                                                [
                                                    StringEncoding.ValueFromString("Expecting a digit")
                                                ])
                                        ])
                                ])
                        ]),
                    param_1_1
                ]);
    }


    public static PineValue parseUnsignedIntRec(
        PineValue param_1_0,
        PineValue param_1_1,
        PineValue param_1_2)
    {
        PineValue local_000 =
            KernelFunction.ApplyKernelFunctionGeneric(
                "take",
                PineValue.List(
                    [
                        IntegerEncoding.EncodeSignedInteger(4),
                        KernelFunction.ApplyKernelFunctionGeneric(
                            "skip",
                            PineValue.List(
                                [param_1_2, param_1_1]))
                    ]));

        if (local_000 == StringEncoding.ValueFromString("0"))
        {
            return
                Test.parseUnsignedIntRec(
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_mul",
                        PineValue.List(
                            [
                                param_1_0,
                                IntegerEncoding.EncodeSignedInteger(10)
                            ])),
                    param_1_1,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_2,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        if (local_000 == StringEncoding.ValueFromString("1"))
        {
            return
                Test.parseUnsignedIntRec(
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_mul",
                                    PineValue.List(
                                        [
                                            param_1_0,
                                            IntegerEncoding.EncodeSignedInteger(10)
                                        ])),
                                IntegerEncoding.EncodeSignedInteger(1)
                            ])),
                    param_1_1,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_2,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        if (local_000 == StringEncoding.ValueFromString("2"))
        {
            return
                Test.parseUnsignedIntRec(
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_mul",
                                    PineValue.List(
                                        [
                                            param_1_0,
                                            IntegerEncoding.EncodeSignedInteger(10)
                                        ])),
                                IntegerEncoding.EncodeSignedInteger(2)
                            ])),
                    param_1_1,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_2,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        if (local_000 == StringEncoding.ValueFromString("3"))
        {
            return
                Test.parseUnsignedIntRec(
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_mul",
                                    PineValue.List(
                                        [
                                            param_1_0,
                                            IntegerEncoding.EncodeSignedInteger(10)
                                        ])),
                                IntegerEncoding.EncodeSignedInteger(3)
                            ])),
                    param_1_1,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_2,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        if (local_000 == StringEncoding.ValueFromString("4"))
        {
            return
                Test.parseUnsignedIntRec(
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_mul",
                                    PineValue.List(
                                        [
                                            param_1_0,
                                            IntegerEncoding.EncodeSignedInteger(10)
                                        ])),
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])),
                    param_1_1,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_2,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        if (local_000 == StringEncoding.ValueFromString("5"))
        {
            return
                Test.parseUnsignedIntRec(
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_mul",
                                    PineValue.List(
                                        [
                                            param_1_0,
                                            IntegerEncoding.EncodeSignedInteger(10)
                                        ])),
                                IntegerEncoding.EncodeSignedInteger(5)
                            ])),
                    param_1_1,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_2,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        if (local_000 == StringEncoding.ValueFromString("6"))
        {
            return
                Test.parseUnsignedIntRec(
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_mul",
                                    PineValue.List(
                                        [
                                            param_1_0,
                                            IntegerEncoding.EncodeSignedInteger(10)
                                        ])),
                                IntegerEncoding.EncodeSignedInteger(6)
                            ])),
                    param_1_1,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_2,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        if (local_000 == StringEncoding.ValueFromString("7"))
        {
            return
                Test.parseUnsignedIntRec(
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_mul",
                                    PineValue.List(
                                        [
                                            param_1_0,
                                            IntegerEncoding.EncodeSignedInteger(10)
                                        ])),
                                IntegerEncoding.EncodeSignedInteger(7)
                            ])),
                    param_1_1,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_2,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        if (local_000 == StringEncoding.ValueFromString("8"))
        {
            return
                Test.parseUnsignedIntRec(
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_mul",
                                    PineValue.List(
                                        [
                                            param_1_0,
                                            IntegerEncoding.EncodeSignedInteger(10)
                                        ])),
                                IntegerEncoding.EncodeSignedInteger(8)
                            ])),
                    param_1_1,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_2,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        if (local_000 == StringEncoding.ValueFromString("9"))
        {
            return
                Test.parseUnsignedIntRec(
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                KernelFunction.ApplyKernelFunctionGeneric(
                                    "int_mul",
                                    PineValue.List(
                                        [
                                            param_1_0,
                                            IntegerEncoding.EncodeSignedInteger(10)
                                        ])),
                                IntegerEncoding.EncodeSignedInteger(9)
                            ])),
                    param_1_1,
                    KernelFunction.ApplyKernelFunctionGeneric(
                        "int_add",
                        PineValue.List(
                            [
                                param_1_2,
                                IntegerEncoding.EncodeSignedInteger(4)
                            ])));
        }
        return
            PineValue.List(
                [
                    PineValue.List(
                        [
                            StringEncoding.ValueFromString("Ok"),
                            PineValue.List(
                                [param_1_0])
                        ]),
                    param_1_2
                ]);
    }
}

class CommonValues
{
    public static PineValue Tag_name_value_Ok =
        StringEncoding.ValueFromString("Ok");

    public static PineValue Tag_name_value_Err =
        StringEncoding.ValueFromString("Err");
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

