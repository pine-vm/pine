using AwesomeAssertions;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.PopularEncodings;
using Pine.Elm;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.IntegrationTests.CodeAnalysis;

public class OptimizeAndEmitIdivTests
{
    [Fact]
    public void Parse_and_emit_optimized_idiv()
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


            idiv : Int -> Int -> Int
            idiv dividend divisor =
                if Pine_kernel.equal [ divisor, 0 ] then
                    0
            
                else
                    let
                        ( dividendNegative, absDividend ) =
                            if Pine_kernel.int_is_sorted_asc [ 0, dividend ] then
                                ( False
                                , dividend
                                )
            
                            else
                                ( True
                                , Pine_kernel.int_mul [ dividend, -1 ]
                                )
            
                        ( divisorNegative, absDivisor ) =
                            if Pine_kernel.int_is_sorted_asc [ 0, divisor ] then
                                ( False
                                , divisor
                                )
            
                            else
                                ( True
                                , Pine_kernel.int_mul [ divisor, -1 ]
                                )
            
                        absQuotient : Int
                        absQuotient =
                            idivHelper absDividend absDivisor 0
                    in
                    if Pine_kernel.equal [ dividendNegative, divisorNegative ] then
                        absQuotient
            
                    else
                        Pine_kernel.int_mul [ absQuotient, -1 ]
            
            
            idivHelper : Int -> Int -> Int -> Int
            idivHelper dividend divisor quotient =
                let
                    scaledDivisor =
                        Pine_kernel.int_mul [ divisor, 17 ]
                in
                if Pine_kernel.int_is_sorted_asc [ scaledDivisor, dividend ] then
                    let
                        scaledQuotient =
                            idivHelper
                                dividend
                                scaledDivisor
                                0
            
                        scaledQuotientSum =
                            Pine_kernel.int_mul [ scaledQuotient, 17 ]
            
                        remainder =
                            Pine_kernel.int_add
                                [ dividend
                                , Pine_kernel.int_mul [ scaledQuotient, scaledDivisor, -1 ]
                                ]
            
                        remainderQuotient =
                            idivHelper remainder divisor 0
                    in
                    Pine_kernel.int_add [ scaledQuotientSum, remainderQuotient ]
            
                else if Pine_kernel.int_is_sorted_asc [ divisor, dividend ] then
                    idivHelper
                        (Pine_kernel.int_add
                            [ dividend
                            , Pine_kernel.int_mul [ divisor, -1 ]
                            ]
                        )
                        divisor
                        (Pine_kernel.int_add [ quotient, 1 ])
            
                else
                    quotient
            
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
                    return declName == new DeclQualifiedName(["Test"], "idiv");
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.idiv param_1_0 param_1_1 =
                if
                    Pine_kernel.equal
                        [ param_1_1
                        , 0
                        ]
                then
                    0

                else if
                    Pine_kernel.equal
                        [ if
                            Pine_kernel.int_is_sorted_asc
                                [ 0
                                , param_1_0
                                ]
                          then
                            False

                          else
                            True
                        , if
                            Pine_kernel.int_is_sorted_asc
                                [ 0
                                , param_1_1
                                ]
                          then
                            False

                          else
                            True
                        ]
                then
                    Test.idivHelper
                        if
                            Pine_kernel.int_is_sorted_asc
                                [ 0
                                , param_1_0
                                ]
                        then
                            param_1_0

                        else
                            Pine_kernel.int_mul
                                [ param_1_0
                                , -1
                                ]
                        if
                            Pine_kernel.int_is_sorted_asc
                                [ 0
                                , param_1_1
                                ]
                        then
                            param_1_1

                        else
                            Pine_kernel.int_mul
                                [ param_1_1
                                , -1
                                ]
                        0

                else
                    Pine_kernel.int_mul
                        [ Test.idivHelper
                            if
                                Pine_kernel.int_is_sorted_asc
                                    [ 0
                                    , param_1_0
                                    ]
                            then
                                param_1_0

                            else
                                Pine_kernel.int_mul
                                    [ param_1_0
                                    , -1
                                    ]
                            if
                                Pine_kernel.int_is_sorted_asc
                                    [ 0
                                    , param_1_1
                                    ]
                            then
                                param_1_1

                            else
                                Pine_kernel.int_mul
                                    [ param_1_1
                                    , -1
                                    ]
                            0
                        , -1
                        ]


            Test.idivHelper param_1_0 param_1_1 param_1_2 =
                if
                    Pine_kernel.int_is_sorted_asc
                        [ Pine_kernel.int_mul
                            [ param_1_1
                            , 17
                            ]
                        , param_1_0
                        ]
                then
                    Pine_kernel.int_add
                        [ Pine_kernel.int_mul
                            [ Test.idivHelper
                                param_1_0
                                (Pine_kernel.int_mul
                                    [ param_1_1
                                    , 17
                                    ]
                                )
                                0
                            , 17
                            ]
                        , Test.idivHelper
                            (Pine_kernel.int_add
                                [ param_1_0
                                , Pine_kernel.int_mul
                                    [ Test.idivHelper
                                        param_1_0
                                        (Pine_kernel.int_mul
                                            [ param_1_1
                                            , 17
                                            ]
                                        )
                                        0
                                    , Pine_kernel.int_mul
                                        [ param_1_1
                                        , 17
                                        ]
                                    , -1
                                    ]
                                ]
                            )
                            param_1_1
                            0
                        ]

                else if
                    Pine_kernel.int_is_sorted_asc
                        [ param_1_1
                        , param_1_0
                        ]
                then
                    Test.idivHelper
                        (Pine_kernel.int_add
                            [ param_1_0
                            , Pine_kernel.int_mul
                                [ param_1_1
                                , -1
                                ]
                            ]
                        )
                        param_1_1
                        (Pine_kernel.int_add
                            [ param_1_2
                            , 1
                            ]
                        )

                else
                    param_1_2
            
            
            """"
            .Trim());

        var asCSharp =
            StaticProgramCSharp.FromStaticProgram(
                staticProgram,
                CodeAnalysisTestHelper.DeclarationSyntaxContext);

        var testClass = asCSharp.ModulesClasses[new DeclQualifiedName([], "Test")];

        var moduleTestCSharpText =
            testClass.RenderToString();

        var commonValuesClassText =
            Pine.PineVM.StaticProgramCSharpClass.RenderToString(asCSharp.CommonValueClass);

        var dispatcherClassText =
            Pine.PineVM.StaticProgramCSharpClass.RenderToString(asCSharp.DispatcherClass);

        moduleTestCSharpText.Trim().Should().Be(
            """"
            public static class Test
            {
                public static PineValue idiv(
                    PineValue param_1_0,
                    PineValue param_1_1)
                {
                    if (param_1_1 == CommonReusedValues.Blob_Int_0)
                    {
                        return CommonReusedValues.Blob_Int_0;
                    }

                    PineValue local_000 =
                        KernelFunction.ValueFromBool(
                            KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(0, param_1_0));

                    PineValue local_001 =
                        KernelFunction.ValueFromBool(
                            KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(0, param_1_1));

                    if ((local_000 == PineKernelValues.TrueValue
                    ?
                    CommonReusedValues.Bool_False
                    :
                    CommonReusedValues.Bool_True) == (local_001 == PineKernelValues.TrueValue
                    ?
                    CommonReusedValues.Bool_False
                    :
                    CommonReusedValues.Bool_True))
                    {
                        return
                            Test.idivHelper(
                                local_000 == PineKernelValues.TrueValue
                                ?
                                param_1_0
                                :
                                KernelFunctionSpecialized.int_mul(-1, param_1_0),
                                local_001 == PineKernelValues.TrueValue
                                ?
                                param_1_1
                                :
                                KernelFunctionSpecialized.int_mul(-1, param_1_1),
                                CommonReusedValues.Blob_Int_0);
                    }

                    return
                        KernelFunctionSpecialized.int_mul(
                            -1,
                            Test.idivHelper(
                                local_000 == PineKernelValues.TrueValue
                                ?
                                param_1_0
                                :
                                KernelFunctionSpecialized.int_mul(-1, param_1_0),
                                local_001 == PineKernelValues.TrueValue
                                ?
                                param_1_1
                                :
                                KernelFunctionSpecialized.int_mul(-1, param_1_1),
                                CommonReusedValues.Blob_Int_0));
                }


                public static PineValue idivHelper(
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
                            KernelFunctionSpecialized.int_mul(17, local_param_1_1);

                        if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(local_000, local_param_1_0))
                        {
                            PineValue local_001 =
                                Test.idivHelper(local_param_1_0, local_000, CommonReusedValues.Blob_Int_0);

                            return
                                KernelFunctionSpecialized.int_add(
                                    KernelFunctionSpecialized.int_mul(17, local_001),
                                    Test.idivHelper(
                                        KernelFunctionSpecialized.int_add(
                                            local_param_1_0,
                                            KernelFunctionSpecialized.int_mul(local_001, local_000, -1)),
                                        local_param_1_1,
                                        CommonReusedValues.Blob_Int_0));
                        }

                        if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(local_param_1_1, local_param_1_0))
                        {
                            local_param_1_0 =
                                KernelFunctionSpecialized.int_add(
                                    local_param_1_0,
                                    KernelFunctionSpecialized.int_mul(-1, local_param_1_1));

                            local_param_1_2 =
                                KernelFunctionSpecialized.int_add(1, local_param_1_2);

                            continue;
                        }

                        return local_param_1_2;
                    }
                }
            }

            """".Trim());

        dispatcherClassText.Trim().Should().Be(
            """"
            public static class Dispatcher
            {
                public static IReadOnlyDictionary<PineValue, System.Func<PineValue, PineValue>> dispatcherDictionary =
                    BuildDispatcherDictionary();

                public static IReadOnlyDictionary<PineValue, System.Func<PineValue, PineValue>> BuildDispatcherDictionary()
                {
                    var dict =
                        new Dictionary<PineValue, System.Func<PineValue, PineValue>>();

                    dict[CommonReusedValues.List_9a1e26cb] =
                        Dispatch_9a1e26cb;

                    dict[CommonReusedValues.List_6a38b355] =
                        Dispatch_6a38b355;

                    return dict;
                }


                public static PineValue? Dispatch_9a1e26cb(PineValue environment)
                {
                    if (PineValueExtension.ValueFromPathOrEmptyList(
                        environment,
                        [0, 0]) == CommonReusedValues.List_6a38b355)
                    {
                        var arg_1_0 =
                            PineValueExtension.ValueFromPathOrEmptyList(
                                environment,
                                [1, 0]);

                        var arg_1_1 =
                            PineValueExtension.ValueFromPathOrEmptyList(
                                environment,
                                [1, 1]);

                        return Test.idiv(arg_1_0, arg_1_1);
                    }

                    return null;
                }


                public static PineValue? Dispatch_6a38b355(PineValue environment)
                {
                    if (PineValueExtension.ValueFromPathOrEmptyList(
                        environment,
                        [0, 0]) == CommonReusedValues.List_6a38b355)
                    {
                        var arg_1_0 =
                            PineValueExtension.ValueFromPathOrEmptyList(
                                environment,
                                [1, 0]);

                        var arg_1_1 =
                            PineValueExtension.ValueFromPathOrEmptyList(
                                environment,
                                [1, 1]);

                        var arg_1_2 =
                            PineValueExtension.ValueFromPathOrEmptyList(
                                environment,
                                [1, 2]);

                        return Test.idivHelper(arg_1_0, arg_1_1, arg_1_2);
                    }

                    return null;
                }
            }

            """"
            .Trim());

        commonValuesClassText.Trim().Should().Be(
            """"
            public static class CommonReusedValues
            {
                public static readonly PineValue Bool_False =
                    PineValue.Blob(
                        [2]);

                public static readonly PineValue Bool_True =
                    PineValue.Blob(
                        [4]);

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

                public static readonly PineValue Blob_Int_17 =
                    IntegerEncoding.EncodeSignedInteger(17);

                public static readonly PineValue Blob_Str_List =
                    StringEncoding.ValueFromString("List");

                public static readonly PineValue Blob_Str_head =
                    StringEncoding.ValueFromString("head");

                public static readonly PineValue Blob_Str_skip =
                    StringEncoding.ValueFromString("skip");

                public static readonly PineValue Blob_Str_equal =
                    StringEncoding.ValueFromString("equal");

                public static readonly PineValue Blob_Str_Literal =
                    StringEncoding.ValueFromString("Literal");

                public static readonly PineValue Blob_Str_int_add =
                    StringEncoding.ValueFromString("int_add");

                public static readonly PineValue Blob_Str_int_mul =
                    StringEncoding.ValueFromString("int_mul");

                public static readonly PineValue Blob_Str_Conditional =
                    StringEncoding.ValueFromString("Conditional");

                public static readonly PineValue Blob_Str_Environment =
                    StringEncoding.ValueFromString("Environment");

                public static readonly PineValue Blob_Str_ParseAndEval =
                    StringEncoding.ValueFromString("ParseAndEval");

                public static readonly PineValue Blob_Str_KernelApplication =
                    StringEncoding.ValueFromString("KernelApplication");

                public static readonly PineValue Blob_Str_int_is_sorted_asc =
                    StringEncoding.ValueFromString("int_is_sorted_asc");

                public static readonly PineValue List_dda26649 =
                    PineValue.EmptyList;

                public static readonly PineValue List_d64a6579 =
                    PineValue.List(
                        [List_dda26649]);

                public static readonly PineValue List_3cddd7c0 =
                    PineValue.List(
                        [Bool_False]);

                public static readonly PineValue List_fc40d297 =
                    PineValue.List(
                        [Bool_True]);

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

                public static readonly PineValue List_917d790b =
                    PineValue.List(
                        [
                            IntegerEncoding.EncodeSignedInteger(17)
                        ]);

                public static readonly PineValue List_e190d1f5 =
                    PineValue.List(
                        [Blob_Str_Literal, List_d64a6579]);

                public static readonly PineValue List_bb14d771 =
                    PineValue.List(
                        [Blob_Str_Literal, List_3cddd7c0]);

                public static readonly PineValue List_d0b6bef5 =
                    PineValue.List(
                        [Blob_Str_Literal, List_fc40d297]);

                public static readonly PineValue List_56404869 =
                    PineValue.List(
                        [Blob_Str_Literal, List_49dc21d7]);

                public static readonly PineValue List_b5d02807 =
                    PineValue.List(
                        [Blob_Str_Literal, List_0a7103c5]);

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

                public static readonly PineValue List_e5ac7a1c =
                    PineValue.List(
                        [Blob_Str_Literal, List_917d790b]);

                public static readonly PineValue List_bd06385a =
                    PineValue.List(
                        [Blob_Str_Environment, List_dda26649]);

                public static readonly PineValue List_4aad0d20 =
                    PineValue.List(
                        [Blob_Str_head, List_bd06385a]);

                public static readonly PineValue List_c1cc00b1 =
                    PineValue.List(
                        [List_0dcd86c0, List_bd06385a]);

                public static readonly PineValue List_dcbfdaf0 =
                    PineValue.List(
                        [List_c1cc00b1]);

                public static readonly PineValue List_4c301747 =
                    PineValue.List(
                        [Blob_Str_List, List_dcbfdaf0]);

                public static readonly PineValue List_b49facc1 =
                    PineValue.List(
                        [Blob_Str_skip, List_4c301747]);

                public static readonly PineValue List_a5cbaf18 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_4aad0d20]);

                public static readonly PineValue List_61fa9b59 =
                    PineValue.List(
                        [Blob_Str_head, List_a5cbaf18]);

                public static readonly PineValue List_ad7e5b86 =
                    PineValue.List(
                        [List_a5cbaf18, List_e190d1f5]);

                public static readonly PineValue List_1b59d0c9 =
                    PineValue.List(
                        [List_ad7e5b86]);

                public static readonly PineValue List_9517421c =
                    PineValue.List(
                        [List_0dcd86c0, List_a5cbaf18]);

                public static readonly PineValue List_f5d22246 =
                    PineValue.List(
                        [List_43b95777, List_a5cbaf18]);

                public static readonly PineValue List_e350b941 =
                    PineValue.List(
                        [List_450c12a0, List_a5cbaf18]);

                public static readonly PineValue List_a28f3c19 =
                    PineValue.List(
                        [List_0c82888c, List_a5cbaf18]);

                public static readonly PineValue List_ce6f52a7 =
                    PineValue.List(
                        [List_c1b27e6e, List_a5cbaf18]);

                public static readonly PineValue List_75dd3db2 =
                    PineValue.List(
                        [List_282dee3a, List_a5cbaf18]);

                public static readonly PineValue List_f7dfc6e5 =
                    PineValue.List(
                        [List_9f1e38f9, List_a5cbaf18]);

                public static readonly PineValue List_12d91a58 =
                    PineValue.List(
                        [List_9517421c]);

                public static readonly PineValue List_a671a9e1 =
                    PineValue.List(
                        [List_f5d22246]);

                public static readonly PineValue List_7793aceb =
                    PineValue.List(
                        [List_e350b941]);

                public static readonly PineValue List_ca40958b =
                    PineValue.List(
                        [List_a28f3c19]);

                public static readonly PineValue List_57491543 =
                    PineValue.List(
                        [List_ce6f52a7]);

                public static readonly PineValue List_e3d9524f =
                    PineValue.List(
                        [List_75dd3db2]);

                public static readonly PineValue List_09116b9d =
                    PineValue.List(
                        [List_f7dfc6e5]);

                public static readonly PineValue List_28767619 =
                    PineValue.List(
                        [Blob_Str_List, List_1b59d0c9]);

                public static readonly PineValue List_94d58849 =
                    PineValue.List(
                        [Blob_Str_List, List_12d91a58]);

                public static readonly PineValue List_6a975ff5 =
                    PineValue.List(
                        [Blob_Str_List, List_a671a9e1]);

                public static readonly PineValue List_7b8c1bfc =
                    PineValue.List(
                        [Blob_Str_List, List_7793aceb]);

                public static readonly PineValue List_96e328f4 =
                    PineValue.List(
                        [Blob_Str_List, List_ca40958b]);

                public static readonly PineValue List_a4c2d7ab =
                    PineValue.List(
                        [Blob_Str_List, List_57491543]);

                public static readonly PineValue List_5a3bfc4d =
                    PineValue.List(
                        [Blob_Str_List, List_e3d9524f]);

                public static readonly PineValue List_56408778 =
                    PineValue.List(
                        [Blob_Str_List, List_09116b9d]);

                public static readonly PineValue List_65405503 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_b49facc1]);

                public static readonly PineValue List_7552bdea =
                    PineValue.List(
                        [Blob_Str_head, List_65405503]);

                public static readonly PineValue List_f942a019 =
                    PineValue.List(
                        [Blob_Str_skip, List_94d58849]);

                public static readonly PineValue List_0175b775 =
                    PineValue.List(
                        [Blob_Str_skip, List_6a975ff5]);

                public static readonly PineValue List_57e7e72f =
                    PineValue.List(
                        [Blob_Str_skip, List_7b8c1bfc]);

                public static readonly PineValue List_e175dee1 =
                    PineValue.List(
                        [Blob_Str_skip, List_96e328f4]);

                public static readonly PineValue List_b2076a7a =
                    PineValue.List(
                        [Blob_Str_skip, List_a4c2d7ab]);

                public static readonly PineValue List_e159da15 =
                    PineValue.List(
                        [Blob_Str_skip, List_5a3bfc4d]);

                public static readonly PineValue List_f70cb462 =
                    PineValue.List(
                        [Blob_Str_skip, List_56408778]);

                public static readonly PineValue List_976730e9 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_61fa9b59]);

                public static readonly PineValue List_f1126ab7 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_7552bdea]);

                public static readonly PineValue List_908d1ad7 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_f942a019]);

                public static readonly PineValue List_e38aa202 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_0175b775]);

                public static readonly PineValue List_6f6d2891 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_57e7e72f]);

                public static readonly PineValue List_39f2ae3c =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_e175dee1]);

                public static readonly PineValue List_2f4a0deb =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_b2076a7a]);

                public static readonly PineValue List_95e38d7c =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_e159da15]);

                public static readonly PineValue List_34360713 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_f70cb462]);

                public static readonly PineValue List_16c4bb4a =
                    PineValue.List(
                        [Blob_Str_head, List_f1126ab7]);

                public static readonly PineValue List_2456c204 =
                    PineValue.List(
                        [Blob_Str_head, List_908d1ad7]);

                public static readonly PineValue List_4045f225 =
                    PineValue.List(
                        [Blob_Str_head, List_e38aa202]);

                public static readonly PineValue List_48ca5353 =
                    PineValue.List(
                        [Blob_Str_head, List_6f6d2891]);

                public static readonly PineValue List_57c1abc7 =
                    PineValue.List(
                        [Blob_Str_head, List_39f2ae3c]);

                public static readonly PineValue List_cfeaf156 =
                    PineValue.List(
                        [Blob_Str_head, List_2f4a0deb]);

                public static readonly PineValue List_7ed4e0f7 =
                    PineValue.List(
                        [Blob_Str_head, List_95e38d7c]);

                public static readonly PineValue List_3c772bcb =
                    PineValue.List(
                        [Blob_Str_head, List_34360713]);

                public static readonly PineValue List_13c67965 =
                    PineValue.List(
                        [List_0dcd86c0, List_f1126ab7]);

                public static readonly PineValue List_fd602b70 =
                    PineValue.List(
                        [List_43b95777, List_f1126ab7]);

                public static readonly PineValue List_498d834e =
                    PineValue.List(
                        [List_13c67965]);

                public static readonly PineValue List_22254e85 =
                    PineValue.List(
                        [List_fd602b70]);

                public static readonly PineValue List_85fcd3b2 =
                    PineValue.List(
                        [Blob_Str_List, List_498d834e]);

                public static readonly PineValue List_4668ada6 =
                    PineValue.List(
                        [Blob_Str_List, List_22254e85]);

                public static readonly PineValue List_6b896037 =
                    PineValue.List(
                        [Blob_Str_skip, List_85fcd3b2]);

                public static readonly PineValue List_3218a5a2 =
                    PineValue.List(
                        [Blob_Str_skip, List_4668ada6]);

                public static readonly PineValue List_b43468c9 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_16c4bb4a]);

                public static readonly PineValue List_6dbb5f29 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_2456c204]);

                public static readonly PineValue List_d08202cd =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_4045f225]);

                public static readonly PineValue List_35cf017c =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_48ca5353]);

                public static readonly PineValue List_86d38b8d =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_57c1abc7]);

                public static readonly PineValue List_34849c45 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_cfeaf156]);

                public static readonly PineValue List_d181119a =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_7ed4e0f7]);

                public static readonly PineValue List_96cfcb00 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_3c772bcb]);

                public static readonly PineValue List_5043654b =
                    PineValue.List(
                        [List_bb14d771, List_b43468c9]);

                public static readonly PineValue List_ebc294be =
                    PineValue.List(
                        [List_bb14d771, List_6dbb5f29]);

                public static readonly PineValue List_62b47060 =
                    PineValue.List(
                        [List_bb14d771, List_d08202cd]);

                public static readonly PineValue List_4d585e4f =
                    PineValue.List(
                        [List_5043654b]);

                public static readonly PineValue List_3f58bd96 =
                    PineValue.List(
                        [List_ebc294be]);

                public static readonly PineValue List_e702c077 =
                    PineValue.List(
                        [List_62b47060]);

                public static readonly PineValue List_1a6050bf =
                    PineValue.List(
                        [List_b5d02807, List_b43468c9]);

                public static readonly PineValue List_44d35e66 =
                    PineValue.List(
                        [List_b5d02807, List_6dbb5f29]);

                public static readonly PineValue List_8f2aee6f =
                    PineValue.List(
                        [List_b5d02807, List_d08202cd]);

                public static readonly PineValue List_8ea850ab =
                    PineValue.List(
                        [List_b43468c9, List_56404869]);

                public static readonly PineValue List_3c72a55c =
                    PineValue.List(
                        [List_6dbb5f29, List_56404869]);

                public static readonly PineValue List_d9c17fa9 =
                    PineValue.List(
                        [List_d08202cd, List_56404869]);

                public static readonly PineValue List_f4cfb374 =
                    PineValue.List(
                        [List_35cf017c, List_56404869]);

                public static readonly PineValue List_7fe04a88 =
                    PineValue.List(
                        [List_35cf017c, List_0dcd86c0]);

                public static readonly PineValue List_5defd11d =
                    PineValue.List(
                        [List_96cfcb00, List_e5ac7a1c]);

                public static readonly PineValue List_8223b8c9 =
                    PineValue.List(
                        [List_1a6050bf]);

                public static readonly PineValue List_fcdfa534 =
                    PineValue.List(
                        [List_44d35e66]);

                public static readonly PineValue List_e710004a =
                    PineValue.List(
                        [List_8f2aee6f]);

                public static readonly PineValue List_9998f8b7 =
                    PineValue.List(
                        [List_8ea850ab]);

                public static readonly PineValue List_5d895b1a =
                    PineValue.List(
                        [List_3c72a55c]);

                public static readonly PineValue List_7c507f6a =
                    PineValue.List(
                        [List_d9c17fa9]);

                public static readonly PineValue List_b7d638b4 =
                    PineValue.List(
                        [List_f4cfb374]);

                public static readonly PineValue List_a47b40be =
                    PineValue.List(
                        [List_7fe04a88]);

                public static readonly PineValue List_2b5a45a1 =
                    PineValue.List(
                        [List_5defd11d]);

                public static readonly PineValue List_1f27fb25 =
                    PineValue.List(
                        [Blob_Str_List, List_4d585e4f]);

                public static readonly PineValue List_23bb8f95 =
                    PineValue.List(
                        [Blob_Str_List, List_3f58bd96]);

                public static readonly PineValue List_69f0dbcb =
                    PineValue.List(
                        [Blob_Str_List, List_e702c077]);

                public static readonly PineValue List_9cd0e5ac =
                    PineValue.List(
                        [Blob_Str_List, List_8223b8c9]);

                public static readonly PineValue List_3b615ac8 =
                    PineValue.List(
                        [Blob_Str_List, List_fcdfa534]);

                public static readonly PineValue List_5149771a =
                    PineValue.List(
                        [Blob_Str_List, List_e710004a]);

                public static readonly PineValue List_8cfa47af =
                    PineValue.List(
                        [Blob_Str_List, List_9998f8b7]);

                public static readonly PineValue List_97e04f57 =
                    PineValue.List(
                        [Blob_Str_List, List_5d895b1a]);

                public static readonly PineValue List_a12542a5 =
                    PineValue.List(
                        [Blob_Str_List, List_7c507f6a]);

                public static readonly PineValue List_a83d5f28 =
                    PineValue.List(
                        [Blob_Str_List, List_b7d638b4]);

                public static readonly PineValue List_90d23f25 =
                    PineValue.List(
                        [Blob_Str_List, List_a47b40be]);

                public static readonly PineValue List_8b43304e =
                    PineValue.List(
                        [Blob_Str_List, List_2b5a45a1]);

                public static readonly PineValue List_c37ef632 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_6b896037]);

                public static readonly PineValue List_ee3ba15f =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_3218a5a2]);

                public static readonly PineValue List_a53f1292 =
                    PineValue.List(
                        [Blob_Str_head, List_c37ef632]);

                public static readonly PineValue List_290d4c0d =
                    PineValue.List(
                        [Blob_Str_head, List_ee3ba15f]);

                public static readonly PineValue List_0934a05a =
                    PineValue.List(
                        [Blob_Str_int_add, List_90d23f25]);

                public static readonly PineValue List_c000b476 =
                    PineValue.List(
                        [Blob_Str_int_mul, List_8cfa47af]);

                public static readonly PineValue List_b98b999e =
                    PineValue.List(
                        [Blob_Str_int_mul, List_97e04f57]);

                public static readonly PineValue List_7310c8bc =
                    PineValue.List(
                        [Blob_Str_int_mul, List_a12542a5]);

                public static readonly PineValue List_ee9b9f21 =
                    PineValue.List(
                        [Blob_Str_int_mul, List_a83d5f28]);

                public static readonly PineValue List_205116de =
                    PineValue.List(
                        [Blob_Str_int_mul, List_8b43304e]);

                public static readonly PineValue List_e2703db8 =
                    PineValue.List(
                        [Blob_Str_int_is_sorted_asc, List_9cd0e5ac]);

                public static readonly PineValue List_2586c3c6 =
                    PineValue.List(
                        [Blob_Str_int_is_sorted_asc, List_3b615ac8]);

                public static readonly PineValue List_a89dfaa5 =
                    PineValue.List(
                        [Blob_Str_int_is_sorted_asc, List_5149771a]);

                public static readonly PineValue List_4af1f0ec =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_a53f1292]);

                public static readonly PineValue List_768cc61e =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_290d4c0d]);

                public static readonly PineValue List_13c1b98b =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_0934a05a]);

                public static readonly PineValue List_9496b055 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_c000b476]);

                public static readonly PineValue List_47457b14 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_b98b999e]);

                public static readonly PineValue List_7a9bb0f9 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_7310c8bc]);

                public static readonly PineValue List_aa0efd15 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_ee9b9f21]);

                public static readonly PineValue List_6f1c2e11 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_205116de]);

                public static readonly PineValue List_c8025fde =
                    PineValue.List(
                        [List_6f1c2e11]);

                public static readonly PineValue List_8f8475f4 =
                    PineValue.List(
                        [List_bb14d771, List_4af1f0ec]);

                public static readonly PineValue List_b77971dc =
                    PineValue.List(
                        [List_8f8475f4]);

                public static readonly PineValue List_cb2eac87 =
                    PineValue.List(
                        [List_b5d02807, List_4af1f0ec]);

                public static readonly PineValue List_a4ac239c =
                    PineValue.List(
                        [List_4af1f0ec, List_56404869]);

                public static readonly PineValue List_84523d46 =
                    PineValue.List(
                        [List_4af1f0ec, List_b5d02807]);

                public static readonly PineValue List_1ccfd4d0 =
                    PineValue.List(
                        [List_4af1f0ec, List_e5ac7a1c]);

                public static readonly PineValue List_a474ca67 =
                    PineValue.List(
                        [List_cb2eac87]);

                public static readonly PineValue List_3aa5a807 =
                    PineValue.List(
                        [List_a4ac239c]);

                public static readonly PineValue List_fc4da83d =
                    PineValue.List(
                        [List_84523d46]);

                public static readonly PineValue List_29d89cd2 =
                    PineValue.List(
                        [List_1ccfd4d0]);

                public static readonly PineValue List_1efd6566 =
                    PineValue.List(
                        [Blob_Str_Literal, List_c8025fde]);

                public static readonly PineValue List_b172fbd6 =
                    PineValue.List(
                        [List_d0b6bef5, List_9496b055]);

                public static readonly PineValue List_b81534b6 =
                    PineValue.List(
                        [List_d0b6bef5, List_47457b14]);

                public static readonly PineValue List_6d34492d =
                    PineValue.List(
                        [List_d0b6bef5, List_7a9bb0f9]);

                public static readonly PineValue List_f9f39fec =
                    PineValue.List(
                        [List_86d38b8d, List_28767619]);

                public static readonly PineValue List_d56d288f =
                    PineValue.List(
                        [List_34849c45, List_28767619]);

                public static readonly PineValue List_5ed70eaf =
                    PineValue.List(
                        [List_d181119a, List_28767619]);

                public static readonly PineValue List_1543ec55 =
                    PineValue.List(
                        [List_b172fbd6]);

                public static readonly PineValue List_5806162e =
                    PineValue.List(
                        [List_b81534b6]);

                public static readonly PineValue List_719629bc =
                    PineValue.List(
                        [List_6d34492d]);

                public static readonly PineValue List_23afeea9 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_e2703db8]);

                public static readonly PineValue List_7e774b12 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_2586c3c6]);

                public static readonly PineValue List_a39c07a7 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_a89dfaa5]);

                public static readonly PineValue List_6386e244 =
                    PineValue.List(
                        [Blob_Str_List, List_b77971dc]);

                public static readonly PineValue List_4d3b6f7f =
                    PineValue.List(
                        [Blob_Str_List, List_a474ca67]);

                public static readonly PineValue List_a40c9373 =
                    PineValue.List(
                        [Blob_Str_List, List_3aa5a807]);

                public static readonly PineValue List_affeafd7 =
                    PineValue.List(
                        [Blob_Str_List, List_fc4da83d]);

                public static readonly PineValue List_c34b9c14 =
                    PineValue.List(
                        [Blob_Str_List, List_29d89cd2]);

                public static readonly PineValue List_45aab610 =
                    PineValue.List(
                        [Blob_Str_List, List_1543ec55]);

                public static readonly PineValue List_0da2d82f =
                    PineValue.List(
                        [Blob_Str_List, List_5806162e]);

                public static readonly PineValue List_8a25cb53 =
                    PineValue.List(
                        [Blob_Str_List, List_719629bc]);

                public static readonly PineValue List_8baea653 =
                    PineValue.List(
                        [Blob_Str_equal, List_affeafd7]);

                public static readonly PineValue List_d9a8bf69 =
                    PineValue.List(
                        [Blob_Str_int_mul, List_a40c9373]);

                public static readonly PineValue List_1449c130 =
                    PineValue.List(
                        [Blob_Str_int_mul, List_c34b9c14]);

                public static readonly PineValue List_af278ce0 =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_f9f39fec]);

                public static readonly PineValue List_24a01a94 =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_d56d288f]);

                public static readonly PineValue List_fe2319f6 =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_5ed70eaf]);

                public static readonly PineValue List_d46b5c67 =
                    PineValue.List(
                        [Blob_Str_int_is_sorted_asc, List_4d3b6f7f]);

                public static readonly PineValue List_f01ccf80 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_8baea653]);

                public static readonly PineValue List_66f15e7d =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_d9a8bf69]);

                public static readonly PineValue List_3aad3f38 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_1449c130]);

                public static readonly PineValue List_b1ba27e4 =
                    PineValue.List(
                        [List_d0b6bef5, List_66f15e7d]);

                public static readonly PineValue List_d2642efc =
                    PineValue.List(
                        [List_b1ba27e4]);

                public static readonly PineValue List_c42737be =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_d46b5c67]);

                public static readonly PineValue List_d003d670 =
                    PineValue.List(
                        [Blob_Str_List, List_d2642efc]);

                public static readonly PineValue List_162ee665 =
                    PineValue.List(
                        [List_d08202cd, List_6dbb5f29]);

                public static readonly PineValue List_039fa762 =
                    PineValue.List(
                        [List_86d38b8d, List_6dbb5f29]);

                public static readonly PineValue List_5cae01b8 =
                    PineValue.List(
                        [List_162ee665]);

                public static readonly PineValue List_c3543586 =
                    PineValue.List(
                        [List_039fa762]);

                public static readonly PineValue List_5f35ef89 =
                    PineValue.List(
                        [Blob_Str_List, List_5cae01b8]);

                public static readonly PineValue List_97774c0f =
                    PineValue.List(
                        [Blob_Str_List, List_c3543586]);

                public static readonly PineValue List_1aabcf9b =
                    PineValue.List(
                        [List_6dbb5f29, List_86d38b8d, List_b5d02807]);

                public static readonly PineValue List_cd1eb242 =
                    PineValue.List(
                        [List_96cfcb00, List_35cf017c, List_56404869]);

                public static readonly PineValue List_242e1c50 =
                    PineValue.List(
                        [List_1aabcf9b]);

                public static readonly PineValue List_8ab8835b =
                    PineValue.List(
                        [List_cd1eb242]);

                public static readonly PineValue List_8c46c627 =
                    PineValue.List(
                        [Blob_Str_List, List_242e1c50]);

                public static readonly PineValue List_ddc25906 =
                    PineValue.List(
                        [Blob_Str_List, List_8ab8835b]);

                public static readonly PineValue List_dc74ad16 =
                    PineValue.List(
                        [Blob_Str_int_mul, List_ddc25906]);

                public static readonly PineValue List_e4fdbb4a =
                    PineValue.List(
                        [Blob_Str_int_is_sorted_asc, List_5f35ef89]);

                public static readonly PineValue List_0347b797 =
                    PineValue.List(
                        [Blob_Str_int_is_sorted_asc, List_97774c0f]);

                public static readonly PineValue List_7b45cce4 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_dc74ad16]);

                public static readonly PineValue List_400282b1 =
                    PineValue.List(
                        [List_6dbb5f29, List_7a9bb0f9]);

                public static readonly PineValue List_9d531ce0 =
                    PineValue.List(
                        [List_400282b1]);

                public static readonly PineValue List_feef8adf =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_e4fdbb4a]);

                public static readonly PineValue List_f8bfc2d4 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_0347b797]);

                public static readonly PineValue List_27ca9399 =
                    PineValue.List(
                        [Blob_Str_List, List_9d531ce0]);

                public static readonly PineValue List_97cfc8ff =
                    PineValue.List(
                        [List_a5cbaf18, List_8c46c627]);

                public static readonly PineValue List_d8b71c8d =
                    PineValue.List(
                        [List_97cfc8ff]);

                public static readonly PineValue List_8c532958 =
                    PineValue.List(
                        [Blob_Str_int_add, List_27ca9399]);

                public static readonly PineValue List_62698512 =
                    PineValue.List(
                        [Blob_Str_List, List_d8b71c8d]);

                public static readonly PineValue List_7bc32dc1 =
                    PineValue.List(
                        [List_24a01a94, List_d08202cd, List_b5d02807]);

                public static readonly PineValue List_2c5c67a8 =
                    PineValue.List(
                        [List_7bc32dc1]);

                public static readonly PineValue List_6d171847 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_8c532958]);

                public static readonly PineValue List_4d2d313a =
                    PineValue.List(
                        [Blob_Str_List, List_2c5c67a8]);

                public static readonly PineValue List_60ada136 =
                    PineValue.List(
                        [List_a5cbaf18, List_4d2d313a]);

                public static readonly PineValue List_4de6eb72 =
                    PineValue.List(
                        [List_60ada136]);

                public static readonly PineValue List_18b61b69 =
                    PineValue.List(
                        [List_976730e9, List_62698512]);

                public static readonly PineValue List_738a33a5 =
                    PineValue.List(
                        [Blob_Str_List, List_4de6eb72]);

                public static readonly PineValue List_92e1bf14 =
                    PineValue.List(
                        [List_af278ce0, List_fe2319f6]);

                public static readonly PineValue List_cfb34803 =
                    PineValue.List(
                        [List_92e1bf14]);

                public static readonly PineValue List_bb1301d2 =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_18b61b69]);

                public static readonly PineValue List_8d64b0d3 =
                    PineValue.List(
                        [Blob_Str_List, List_cfb34803]);

                public static readonly PineValue List_4357613e =
                    PineValue.List(
                        [List_6dbb5f29, List_7b45cce4]);

                public static readonly PineValue List_f47ded23 =
                    PineValue.List(
                        [List_4357613e]);

                public static readonly PineValue List_aea8af18 =
                    PineValue.List(
                        [Blob_Str_int_add, List_8d64b0d3]);

                public static readonly PineValue List_5b2fd16d =
                    PineValue.List(
                        [Blob_Str_List, List_f47ded23]);

                public static readonly PineValue List_78ca0de2 =
                    PineValue.List(
                        [Blob_Str_int_add, List_5b2fd16d]);

                public static readonly PineValue List_c4d2d1eb =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_aea8af18]);

                public static readonly PineValue List_9d15fc29 =
                    PineValue.List(
                        [List_c4d2d1eb]);

                public static readonly PineValue List_5c94f165 =
                    PineValue.List(
                        [Blob_Str_Literal, List_9d15fc29]);

                public static readonly PineValue List_8b10bec8 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_78ca0de2]);

                public static readonly PineValue List_acc38675 =
                    PineValue.List(
                        [List_8b10bec8]);

                public static readonly PineValue List_14e626b6 =
                    PineValue.List(
                        [Blob_Str_Literal, List_acc38675]);

                public static readonly PineValue List_256d7d08 =
                    PineValue.List(
                        [List_976730e9, List_738a33a5]);

                public static readonly PineValue List_ec48c01b =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_256d7d08]);

                public static readonly PineValue List_7a9095e0 =
                    PineValue.List(
                        [List_ec48c01b]);

                public static readonly PineValue List_9de4765d =
                    PineValue.List(
                        [Blob_Str_Literal, List_7a9095e0]);

                public static readonly PineValue List_8e7447f8 =
                    PineValue.List(
                        [List_23afeea9, List_45aab610, List_1f27fb25]);

                public static readonly PineValue List_17675c6d =
                    PineValue.List(
                        [List_7e774b12, List_0da2d82f, List_23bb8f95]);

                public static readonly PineValue List_0fcda3c8 =
                    PineValue.List(
                        [List_a39c07a7, List_8a25cb53, List_69f0dbcb]);

                public static readonly PineValue List_de67f37d =
                    PineValue.List(
                        [Blob_Str_Conditional, List_8e7447f8]);

                public static readonly PineValue List_b2424253 =
                    PineValue.List(
                        [Blob_Str_Conditional, List_17675c6d]);

                public static readonly PineValue List_2fc55208 =
                    PineValue.List(
                        [Blob_Str_Conditional, List_0fcda3c8]);

                public static readonly PineValue List_191d6742 =
                    PineValue.List(
                        [Blob_Str_head, List_b2424253]);

                public static readonly PineValue List_355577ca =
                    PineValue.List(
                        [Blob_Str_head, List_2fc55208]);

                public static readonly PineValue List_0245b2cf =
                    PineValue.List(
                        [List_0dcd86c0, List_de67f37d]);

                public static readonly PineValue List_b25a9bee =
                    PineValue.List(
                        [List_0245b2cf]);

                public static readonly PineValue List_454e3b62 =
                    PineValue.List(
                        [Blob_Str_List, List_b25a9bee]);

                public static readonly PineValue List_1d181cbb =
                    PineValue.List(
                        [Blob_Str_skip, List_454e3b62]);

                public static readonly PineValue List_0e5d5f28 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_191d6742]);

                public static readonly PineValue List_4338879c =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_355577ca]);

                public static readonly PineValue List_c6fbbbde =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_1d181cbb]);

                public static readonly PineValue List_09a8370d =
                    PineValue.List(
                        [Blob_Str_head, List_c6fbbbde]);

                public static readonly PineValue List_ce5b6f29 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_09a8370d]);

                public static readonly PineValue List_57e6d4a8 =
                    PineValue.List(
                        [List_6d171847, List_d08202cd, List_13c1b98b]);

                public static readonly PineValue List_580eeb25 =
                    PineValue.List(
                        [List_57e6d4a8]);

                public static readonly PineValue List_ea35af13 =
                    PineValue.List(
                        [Blob_Str_List, List_580eeb25]);

                public static readonly PineValue List_dd76f543 =
                    PineValue.List(
                        [List_c42737be, List_d003d670, List_6386e244]);

                public static readonly PineValue List_07385e0e =
                    PineValue.List(
                        [Blob_Str_Conditional, List_dd76f543]);

                public static readonly PineValue List_721028b9 =
                    PineValue.List(
                        [List_a5cbaf18, List_ea35af13]);

                public static readonly PineValue List_a139528b =
                    PineValue.List(
                        [List_721028b9]);

                public static readonly PineValue List_a060ab93 =
                    PineValue.List(
                        [List_0dcd86c0, List_07385e0e]);

                public static readonly PineValue List_9e01c172 =
                    PineValue.List(
                        [List_a060ab93]);

                public static readonly PineValue List_7f207ea8 =
                    PineValue.List(
                        [Blob_Str_List, List_a139528b]);

                public static readonly PineValue List_a6fd9c1f =
                    PineValue.List(
                        [Blob_Str_List, List_9e01c172]);

                public static readonly PineValue List_b1cc18a9 =
                    PineValue.List(
                        [Blob_Str_skip, List_a6fd9c1f]);

                public static readonly PineValue List_6129cd4b =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_b1cc18a9]);

                public static readonly PineValue List_3540f806 =
                    PineValue.List(
                        [Blob_Str_head, List_6129cd4b]);

                public static readonly PineValue List_c27949d0 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_3540f806]);

                public static readonly PineValue List_076c0c9e =
                    PineValue.List(
                        [List_976730e9, List_b43468c9, List_4af1f0ec, List_768cc61e, List_3aad3f38]);

                public static readonly PineValue List_3386b235 =
                    PineValue.List(
                        [List_076c0c9e]);

                public static readonly PineValue List_fc51776a =
                    PineValue.List(
                        [Blob_Str_List, List_3386b235]);

                public static readonly PineValue List_22024894 =
                    PineValue.List(
                        [List_976730e9, List_7f207ea8]);

                public static readonly PineValue List_f7365a2e =
                    PineValue.List(
                        [List_fc51776a, List_e190d1f5]);

                public static readonly PineValue List_54746879 =
                    PineValue.List(
                        [List_f7365a2e]);

                public static readonly PineValue List_04c29a7a =
                    PineValue.List(
                        [Blob_Str_List, List_54746879]);

                public static readonly PineValue List_5506574a =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_22024894]);

                public static readonly PineValue List_a1c1583d =
                    PineValue.List(
                        [List_0e5d5f28, List_4338879c]);

                public static readonly PineValue List_67f7f068 =
                    PineValue.List(
                        [List_a1c1583d]);

                public static readonly PineValue List_5d80bbdc =
                    PineValue.List(
                        [Blob_Str_List, List_67f7f068]);

                public static readonly PineValue List_1badf006 =
                    PineValue.List(
                        [Blob_Str_equal, List_5d80bbdc]);

                public static readonly PineValue List_3bcf95d7 =
                    PineValue.List(
                        [Blob_Str_KernelApplication, List_1badf006]);

                public static readonly PineValue List_46753c9d =
                    PineValue.List(
                        [List_feef8adf, List_35cf017c, List_5506574a]);

                public static readonly PineValue List_eaa5a196 =
                    PineValue.List(
                        [Blob_Str_Conditional, List_46753c9d]);

                public static readonly PineValue List_55091f63 =
                    PineValue.List(
                        [List_ce5b6f29, List_c27949d0, List_b5d02807]);

                public static readonly PineValue List_1e99d050 =
                    PineValue.List(
                        [List_55091f63]);

                public static readonly PineValue List_95935e34 =
                    PineValue.List(
                        [Blob_Str_List, List_1e99d050]);

                public static readonly PineValue List_4e4cd185 =
                    PineValue.List(
                        [List_a5cbaf18, List_95935e34]);

                public static readonly PineValue List_97125b6f =
                    PineValue.List(
                        [List_4e4cd185]);

                public static readonly PineValue List_3f9515cb =
                    PineValue.List(
                        [Blob_Str_List, List_97125b6f]);

                public static readonly PineValue List_bf404c82 =
                    PineValue.List(
                        [List_3bcf95d7, List_aa0efd15, List_35cf017c]);

                public static readonly PineValue List_71c0f142 =
                    PineValue.List(
                        [Blob_Str_Conditional, List_bf404c82]);

                public static readonly PineValue List_91a601c4 =
                    PineValue.List(
                        [List_71c0f142]);

                public static readonly PineValue List_602d25fc =
                    PineValue.List(
                        [Blob_Str_Literal, List_91a601c4]);

                public static readonly PineValue List_dc598f43 =
                    PineValue.List(
                        [List_976730e9, List_3f9515cb]);

                public static readonly PineValue List_2a07a877 =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_dc598f43]);

                public static readonly PineValue List_f3328233 =
                    PineValue.List(
                        [List_976730e9, List_b43468c9, List_4af1f0ec, List_2a07a877]);

                public static readonly PineValue List_99515be2 =
                    PineValue.List(
                        [List_f3328233]);

                public static readonly PineValue List_bdc906ba =
                    PineValue.List(
                        [Blob_Str_List, List_99515be2]);

                public static readonly PineValue List_5e888933 =
                    PineValue.List(
                        [List_bdc906ba, List_e190d1f5]);

                public static readonly PineValue List_8927f83d =
                    PineValue.List(
                        [List_5e888933]);

                public static readonly PineValue List_b89681ec =
                    PineValue.List(
                        [Blob_Str_List, List_8927f83d]);

                public static readonly PineValue List_c4758898 =
                    PineValue.List(
                        [List_976730e9, List_6dbb5f29, List_d08202cd, List_86d38b8d, List_1efd6566, List_14e626b6, List_9de4765d, List_bb1301d2]);

                public static readonly PineValue List_ee039cc8 =
                    PineValue.List(
                        [List_c4758898]);

                public static readonly PineValue List_d8407e6c =
                    PineValue.List(
                        [Blob_Str_List, List_ee039cc8]);

                public static readonly PineValue List_ff09aaa3 =
                    PineValue.List(
                        [List_d8407e6c, List_e190d1f5]);

                public static readonly PineValue List_8eae7f73 =
                    PineValue.List(
                        [List_ff09aaa3]);

                public static readonly PineValue List_d0049474 =
                    PineValue.List(
                        [Blob_Str_List, List_8eae7f73]);

                public static readonly PineValue List_a7d7c8cf =
                    PineValue.List(
                        [List_5c94f165, List_d0049474]);

                public static readonly PineValue List_100b3650 =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_a7d7c8cf]);

                public static readonly PineValue List_cc74513b =
                    PineValue.List(
                        [List_602d25fc, List_b89681ec]);

                public static readonly PineValue List_612de97a =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_cc74513b]);

                public static readonly PineValue List_8f389de6 =
                    PineValue.List(
                        [List_f01ccf80, List_612de97a, List_b5d02807]);

                public static readonly PineValue List_9a1e26cb =
                    PineValue.List(
                        [Blob_Str_Conditional, List_8f389de6]);

                public static readonly PineValue List_c0b9b19d =
                    PineValue.List(
                        [List_f8bfc2d4, List_eaa5a196, List_100b3650]);

                public static readonly PineValue List_0b12e039 =
                    PineValue.List(
                        [Blob_Str_Conditional, List_c0b9b19d]);

                public static readonly PineValue List_38fbd3e3 =
                    PineValue.List(
                        [List_0b12e039]);

                public static readonly PineValue List_54e1a6e2 =
                    PineValue.List(
                        [Blob_Str_Literal, List_38fbd3e3]);

                public static readonly PineValue List_052f6296 =
                    PineValue.List(
                        [List_54e1a6e2, List_04c29a7a]);

                public static readonly PineValue List_6a38b355 =
                    PineValue.List(
                        [Blob_Str_ParseAndEval, List_052f6296]);
            }

            """"
            .Trim());

        // Now compile this to a .NET assembly.

        IReadOnlyList<string> compilationNamespacePrefix =
            ["TestAlfa", "TestBeta"];

        var compileToAssemblyResult =
            CompileToAssembly.Compile(
                asCSharp,
                namespacePrefix: compilationNamespacePrefix,
                optimizationLevel: Microsoft.CodeAnalysis.OptimizationLevel.Release)
            .Extract(err =>
            throw new System.Exception("Compilation to assembly failed: " + err.ToString()));

        var compiledDictionary =
            compileToAssemblyResult.BuildCompiledExpressionsDictionary();

        var testModule =
            parsedEnv.Modules.Single(m => m.moduleName is "Test");

        var idivDeclValue =
            testModule.moduleContent.FunctionDeclarations["idiv"];

        var idivFunctionRecord =
            ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(idivDeclValue, parseCache)
            .Extract(err => throw new System.Exception(
                "Parsing function record for 'idiv' failed: " + err.ToString()));

        // Implementation detail: There should be only one entry in environment functions, for idivHelper.

        idivFunctionRecord.EnvFunctions.Length.Should().Be(1);

        var callEnvValue =
            PineValue.List(
                [
                PineValue.List(idivFunctionRecord.EnvFunctions.ToArray()),
                PineValue.List(
                    [
                    IntegerEncoding.EncodeSignedInteger(100),
                    IntegerEncoding.EncodeSignedInteger(3)
                    ]),
                ]);

        var dictEntry =
            compiledDictionary[idivFunctionRecord.EnvFunctions.Span[0]];

        dictEntry.Should().NotBeNull();

        var resultValue = dictEntry(callEnvValue);

        resultValue.Should().NotBeNull();

        resultValue.Should().Be(IntegerEncoding.EncodeSignedInteger(33));
    }
}
