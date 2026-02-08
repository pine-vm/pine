using AwesomeAssertions;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.DotNet;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.IntegrationTests.CodeAnalysis;

public class OptimizeAndEmitIdivTests
{
    [Fact]
    public void Parse_and_emit_optimized_idiv()
    {
        var elmModuleText =
            """"
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
            
            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            CodeAnalysisTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
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
                                    [ -17
                                    , Test.idivHelper
                                        param_1_0
                                        (Pine_kernel.int_mul
                                            [ param_1_1
                                            , 17
                                            ]
                                        )
                                        0
                                    , param_1_1
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
            """".Trim());

        var asCSharp =
            StaticProgramCSharp.FromStaticProgram(
                staticProgram,
                CodeAnalysisTestHelper.DeclarationSyntaxContext);

        var testClass = asCSharp.ModulesClasses[new DeclQualifiedName([], "Test")];

        var moduleTestCSharpText =
            testClass.RenderToString();

        var commonValuesClassText =
            StaticProgramCSharpClass.RenderToString(asCSharp.CommonValueClass);

        var dispatcherClassText =
            StaticProgramCSharpClass.RenderToString(asCSharp.DispatcherClass);

        moduleTestCSharpText.Trim().Should().Be(
            """"
            public static class Test
            {
                public static PineValue idiv(PineValue param_1_0, PineValue param_1_1)
                {
                    if (param_1_1 == CommonReusedValues.Blob_Int_0)
                    {
                        return CommonReusedValues.Blob_Int_0;
                    }

                    PineValue local_001 =
                        KernelFunction.ValueFromBool(KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(0, param_1_0));

                    PineValue local_003 =
                        KernelFunction.ValueFromBool(KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(0, param_1_1));

                    PineValue local_004 =
                        local_001 == PineKernelValues.TrueValue ? param_1_0 : KernelFunctionSpecialized.int_mul(-1, param_1_0);

                    PineValue local_005 =
                        local_003 == PineKernelValues.TrueValue ? param_1_1 : KernelFunctionSpecialized.int_mul(-1, param_1_1);

                    PineValue local_008 = Test.idivHelper(local_004, local_005, CommonReusedValues.Blob_Int_0);

                    if ((local_001 == PineKernelValues.TrueValue ? false : true) == (local_003 == PineKernelValues.TrueValue ? false : true))
                    {
                        return local_008;
                    }

                    return KernelFunctionSpecialized.int_mul(-1, local_008);
                }



                public static PineValue idivHelper(PineValue param_1_0, PineValue param_1_1, PineValue param_1_2)
                {
                    PineValue local_param_1_0 = param_1_0;
                    PineValue local_param_1_1 = param_1_1;
                    PineValue local_param_1_2 = param_1_2;

                    while (true)
                    {
                        PineValue local_000 = KernelFunctionSpecialized.int_mul(17, local_param_1_1);

                        if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(local_000, local_param_1_0))
                        {
                            PineValue local_001 = Test.idivHelper(local_param_1_0, local_000, CommonReusedValues.Blob_Int_0);

                            return
                                KernelFunctionSpecialized.int_add(
                                    KernelFunctionSpecialized.int_mul(17, local_001),
                                    Test.idivHelper(
                                        KernelFunctionSpecialized.int_add(
                                            local_param_1_0,
                                            KernelFunctionSpecialized.int_mul(local_001, local_param_1_1, -17)),
                                        local_param_1_1,
                                        CommonReusedValues.Blob_Int_0));
                        }

                        if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(local_param_1_1, local_param_1_0))
                        {
                            {
                                PineValue local_param_1_0_temp =
                                    KernelFunctionSpecialized.int_add(
                                        local_param_1_0,
                                        KernelFunctionSpecialized.int_mul(-1, local_param_1_1));

                                PineValue local_param_1_2_temp = KernelFunctionSpecialized.int_add(1, local_param_1_2);
                                local_param_1_0 = local_param_1_0_temp;
                                local_param_1_2 = local_param_1_2_temp;
                            }

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
                    var dict = new Dictionary<PineValue, System.Func<PineValue, PineValue>>();
                    dict[CommonReusedValues.List_9a1e26cb] = Dispatch_9a1e26cb;
                    dict[CommonReusedValues.List_1c954123] = Dispatch_1c954123;
                    return dict;
                }



                public static PineValue? Dispatch_9a1e26cb(PineValue environment)
                {
                    if (PineValueExtension.ValueFromPathOrEmptyList(environment, [0, 0]) == CommonReusedValues.List_1c954123)
                    {
                        var arg_1_0 = PineValueExtension.ValueFromPathOrEmptyList(environment, [1, 0]);
                        var arg_1_1 = PineValueExtension.ValueFromPathOrEmptyList(environment, [1, 1]);
                        return Test.idiv(arg_1_0, arg_1_1);
                    }

                    return null;
                }



                public static PineValue? Dispatch_1c954123(PineValue environment)
                {
                    if (PineValueExtension.ValueFromPathOrEmptyList(environment, [0, 0]) == CommonReusedValues.List_1c954123)
                    {
                        var arg_1_0 = PineValueExtension.ValueFromPathOrEmptyList(environment, [1, 0]);
                        var arg_1_1 = PineValueExtension.ValueFromPathOrEmptyList(environment, [1, 1]);
                        var arg_1_2 = PineValueExtension.ValueFromPathOrEmptyList(environment, [1, 2]);
                        return Test.idivHelper(arg_1_0, arg_1_1, arg_1_2);
                    }

                    return null;
                }
            }
            """".Trim());

        commonValuesClassText.Trim().Should().Be(
            """"
            public static class CommonReusedValues
            {
                public static readonly PineValue Blob_Int_neg_1 = IntegerEncoding.EncodeSignedInteger(-1);

                public static readonly PineValue Blob_Int_neg_17 = IntegerEncoding.EncodeSignedInteger(-17);

                public static readonly PineValue Blob_Int_0 = IntegerEncoding.EncodeSignedInteger(0);

                public static readonly PineValue Blob_Int_1 = IntegerEncoding.EncodeSignedInteger(1);

                public static readonly PineValue Blob_Int_2 = IntegerEncoding.EncodeSignedInteger(2);

                public static readonly PineValue Blob_Int_3 = IntegerEncoding.EncodeSignedInteger(3);

                public static readonly PineValue Blob_Int_4 = IntegerEncoding.EncodeSignedInteger(4);

                public static readonly PineValue Blob_Int_5 = IntegerEncoding.EncodeSignedInteger(5);

                public static readonly PineValue Blob_Int_6 = IntegerEncoding.EncodeSignedInteger(6);

                public static readonly PineValue Blob_Int_17 = IntegerEncoding.EncodeSignedInteger(17);

                public static readonly PineValue Blob_Str_List = StringEncoding.ValueFromString("List");

                public static readonly PineValue Blob_Str_head = StringEncoding.ValueFromString("head");

                public static readonly PineValue Blob_Str_skip = StringEncoding.ValueFromString("skip");

                public static readonly PineValue Blob_Str_equal = StringEncoding.ValueFromString("equal");

                public static readonly PineValue Blob_Str_Literal = StringEncoding.ValueFromString("Literal");

                public static readonly PineValue Blob_Str_int_add = StringEncoding.ValueFromString("int_add");

                public static readonly PineValue Blob_Str_int_mul = StringEncoding.ValueFromString("int_mul");

                public static readonly PineValue Blob_Str_Conditional = StringEncoding.ValueFromString("Conditional");

                public static readonly PineValue Blob_Str_Environment = StringEncoding.ValueFromString("Environment");

                public static readonly PineValue Blob_Str_ParseAndEval = StringEncoding.ValueFromString("ParseAndEval");

                public static readonly PineValue Blob_Str_KernelApplication = StringEncoding.ValueFromString("KernelApplication");

                public static readonly PineValue Blob_Str_int_is_sorted_asc = StringEncoding.ValueFromString("int_is_sorted_asc");

                public static readonly PineValue List_Single_List_Empty = PineValue.List([PineValue.EmptyList]);

                public static readonly PineValue List_Single_Bool_False = PineValue.List([PineKernelValues.FalseValue]);

                public static readonly PineValue List_Single_Bool_True = PineValue.List([PineKernelValues.TrueValue]);

                public static readonly PineValue List_Single_Blob_Int_neg_1 =
                    PineValue.List([IntegerEncoding.EncodeSignedInteger(-1)]);

                public static readonly PineValue List_Single_Blob_Int_0 = PineValue.List([IntegerEncoding.EncodeSignedInteger(0)]);

                public static readonly PineValue List_Single_Blob_Int_1 = PineValue.List([IntegerEncoding.EncodeSignedInteger(1)]);

                public static readonly PineValue List_Single_Blob_Int_2 = PineValue.List([IntegerEncoding.EncodeSignedInteger(2)]);

                public static readonly PineValue List_Single_Blob_Int_3 = PineValue.List([IntegerEncoding.EncodeSignedInteger(3)]);

                public static readonly PineValue List_Single_Blob_Int_4 = PineValue.List([IntegerEncoding.EncodeSignedInteger(4)]);

                public static readonly PineValue List_Single_Blob_Int_5 = PineValue.List([IntegerEncoding.EncodeSignedInteger(5)]);

                public static readonly PineValue List_Single_Blob_Int_6 = PineValue.List([IntegerEncoding.EncodeSignedInteger(6)]);

                public static readonly PineValue List_Single_Blob_Int_17 =
                    PineValue.List([IntegerEncoding.EncodeSignedInteger(17)]);

                public static readonly PineValue List_e190d1f5 = PineValue.List([Blob_Str_Literal, List_Single_List_Empty]);

                public static readonly PineValue List_bb14d771 = PineValue.List([Blob_Str_Literal, List_Single_Bool_False]);

                public static readonly PineValue List_d0b6bef5 = PineValue.List([Blob_Str_Literal, List_Single_Bool_True]);

                public static readonly PineValue List_56404869 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_neg_1]);

                public static readonly PineValue List_b5d02807 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_0]);

                public static readonly PineValue List_0dcd86c0 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_1]);

                public static readonly PineValue List_43b95777 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_2]);

                public static readonly PineValue List_450c12a0 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_3]);

                public static readonly PineValue List_0c82888c = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_4]);

                public static readonly PineValue List_c1b27e6e = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_5]);

                public static readonly PineValue List_282dee3a = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_6]);

                public static readonly PineValue List_e5ac7a1c = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_17]);

                public static readonly PineValue List_bd06385a = PineValue.List([Blob_Str_Environment, PineValue.EmptyList]);

                public static readonly PineValue List_4aad0d20 = PineValue.List([Blob_Str_head, List_bd06385a]);

                public static readonly PineValue List_c1cc00b1 = PineValue.List([List_0dcd86c0, List_bd06385a]);

                public static readonly PineValue List_Single_List_c1cc00b1 = PineValue.List([List_c1cc00b1]);

                public static readonly PineValue List_4c301747 = PineValue.List([Blob_Str_List, List_Single_List_c1cc00b1]);

                public static readonly PineValue List_b49facc1 = PineValue.List([Blob_Str_skip, List_4c301747]);

                public static readonly PineValue List_a5cbaf18 = PineValue.List([Blob_Str_KernelApplication, List_4aad0d20]);

                public static readonly PineValue List_61fa9b59 = PineValue.List([Blob_Str_head, List_a5cbaf18]);

                public static readonly PineValue List_ad7e5b86 = PineValue.List([List_a5cbaf18, List_e190d1f5]);

                public static readonly PineValue List_Single_List_ad7e5b86 = PineValue.List([List_ad7e5b86]);

                public static readonly PineValue List_9517421c = PineValue.List([List_0dcd86c0, List_a5cbaf18]);

                public static readonly PineValue List_f5d22246 = PineValue.List([List_43b95777, List_a5cbaf18]);

                public static readonly PineValue List_e350b941 = PineValue.List([List_450c12a0, List_a5cbaf18]);

                public static readonly PineValue List_a28f3c19 = PineValue.List([List_0c82888c, List_a5cbaf18]);

                public static readonly PineValue List_ce6f52a7 = PineValue.List([List_c1b27e6e, List_a5cbaf18]);

                public static readonly PineValue List_75dd3db2 = PineValue.List([List_282dee3a, List_a5cbaf18]);

                public static readonly PineValue List_Single_List_9517421c = PineValue.List([List_9517421c]);

                public static readonly PineValue List_Single_List_f5d22246 = PineValue.List([List_f5d22246]);

                public static readonly PineValue List_Single_List_e350b941 = PineValue.List([List_e350b941]);

                public static readonly PineValue List_Single_List_a28f3c19 = PineValue.List([List_a28f3c19]);

                public static readonly PineValue List_Single_List_ce6f52a7 = PineValue.List([List_ce6f52a7]);

                public static readonly PineValue List_Single_List_75dd3db2 = PineValue.List([List_75dd3db2]);

                public static readonly PineValue List_28767619 = PineValue.List([Blob_Str_List, List_Single_List_ad7e5b86]);

                public static readonly PineValue List_94d58849 = PineValue.List([Blob_Str_List, List_Single_List_9517421c]);

                public static readonly PineValue List_6a975ff5 = PineValue.List([Blob_Str_List, List_Single_List_f5d22246]);

                public static readonly PineValue List_7b8c1bfc = PineValue.List([Blob_Str_List, List_Single_List_e350b941]);

                public static readonly PineValue List_96e328f4 = PineValue.List([Blob_Str_List, List_Single_List_a28f3c19]);

                public static readonly PineValue List_a4c2d7ab = PineValue.List([Blob_Str_List, List_Single_List_ce6f52a7]);

                public static readonly PineValue List_5a3bfc4d = PineValue.List([Blob_Str_List, List_Single_List_75dd3db2]);

                public static readonly PineValue List_65405503 = PineValue.List([Blob_Str_KernelApplication, List_b49facc1]);

                public static readonly PineValue List_7552bdea = PineValue.List([Blob_Str_head, List_65405503]);

                public static readonly PineValue List_f942a019 = PineValue.List([Blob_Str_skip, List_94d58849]);

                public static readonly PineValue List_0175b775 = PineValue.List([Blob_Str_skip, List_6a975ff5]);

                public static readonly PineValue List_57e7e72f = PineValue.List([Blob_Str_skip, List_7b8c1bfc]);

                public static readonly PineValue List_e175dee1 = PineValue.List([Blob_Str_skip, List_96e328f4]);

                public static readonly PineValue List_b2076a7a = PineValue.List([Blob_Str_skip, List_a4c2d7ab]);

                public static readonly PineValue List_e159da15 = PineValue.List([Blob_Str_skip, List_5a3bfc4d]);

                public static readonly PineValue List_976730e9 = PineValue.List([Blob_Str_KernelApplication, List_61fa9b59]);

                public static readonly PineValue List_f1126ab7 = PineValue.List([Blob_Str_KernelApplication, List_7552bdea]);

                public static readonly PineValue List_908d1ad7 = PineValue.List([Blob_Str_KernelApplication, List_f942a019]);

                public static readonly PineValue List_e38aa202 = PineValue.List([Blob_Str_KernelApplication, List_0175b775]);

                public static readonly PineValue List_6f6d2891 = PineValue.List([Blob_Str_KernelApplication, List_57e7e72f]);

                public static readonly PineValue List_39f2ae3c = PineValue.List([Blob_Str_KernelApplication, List_e175dee1]);

                public static readonly PineValue List_2f4a0deb = PineValue.List([Blob_Str_KernelApplication, List_b2076a7a]);

                public static readonly PineValue List_95e38d7c = PineValue.List([Blob_Str_KernelApplication, List_e159da15]);

                public static readonly PineValue List_16c4bb4a = PineValue.List([Blob_Str_head, List_f1126ab7]);

                public static readonly PineValue List_2456c204 = PineValue.List([Blob_Str_head, List_908d1ad7]);

                public static readonly PineValue List_4045f225 = PineValue.List([Blob_Str_head, List_e38aa202]);

                public static readonly PineValue List_48ca5353 = PineValue.List([Blob_Str_head, List_6f6d2891]);

                public static readonly PineValue List_57c1abc7 = PineValue.List([Blob_Str_head, List_39f2ae3c]);

                public static readonly PineValue List_cfeaf156 = PineValue.List([Blob_Str_head, List_2f4a0deb]);

                public static readonly PineValue List_7ed4e0f7 = PineValue.List([Blob_Str_head, List_95e38d7c]);

                public static readonly PineValue List_13c67965 = PineValue.List([List_0dcd86c0, List_f1126ab7]);

                public static readonly PineValue List_fd602b70 = PineValue.List([List_43b95777, List_f1126ab7]);

                public static readonly PineValue List_Single_List_13c67965 = PineValue.List([List_13c67965]);

                public static readonly PineValue List_Single_List_fd602b70 = PineValue.List([List_fd602b70]);

                public static readonly PineValue List_85fcd3b2 = PineValue.List([Blob_Str_List, List_Single_List_13c67965]);

                public static readonly PineValue List_4668ada6 = PineValue.List([Blob_Str_List, List_Single_List_fd602b70]);

                public static readonly PineValue List_6b896037 = PineValue.List([Blob_Str_skip, List_85fcd3b2]);

                public static readonly PineValue List_3218a5a2 = PineValue.List([Blob_Str_skip, List_4668ada6]);

                public static readonly PineValue List_b43468c9 = PineValue.List([Blob_Str_KernelApplication, List_16c4bb4a]);

                public static readonly PineValue List_6dbb5f29 = PineValue.List([Blob_Str_KernelApplication, List_2456c204]);

                public static readonly PineValue List_d08202cd = PineValue.List([Blob_Str_KernelApplication, List_4045f225]);

                public static readonly PineValue List_35cf017c = PineValue.List([Blob_Str_KernelApplication, List_48ca5353]);

                public static readonly PineValue List_86d38b8d = PineValue.List([Blob_Str_KernelApplication, List_57c1abc7]);

                public static readonly PineValue List_34849c45 = PineValue.List([Blob_Str_KernelApplication, List_cfeaf156]);

                public static readonly PineValue List_d181119a = PineValue.List([Blob_Str_KernelApplication, List_7ed4e0f7]);

                public static readonly PineValue List_5043654b = PineValue.List([List_bb14d771, List_b43468c9]);

                public static readonly PineValue List_ebc294be = PineValue.List([List_bb14d771, List_6dbb5f29]);

                public static readonly PineValue List_62b47060 = PineValue.List([List_bb14d771, List_d08202cd]);

                public static readonly PineValue List_Single_List_5043654b = PineValue.List([List_5043654b]);

                public static readonly PineValue List_Single_List_ebc294be = PineValue.List([List_ebc294be]);

                public static readonly PineValue List_Single_List_62b47060 = PineValue.List([List_62b47060]);

                public static readonly PineValue List_1a6050bf = PineValue.List([List_b5d02807, List_b43468c9]);

                public static readonly PineValue List_44d35e66 = PineValue.List([List_b5d02807, List_6dbb5f29]);

                public static readonly PineValue List_8f2aee6f = PineValue.List([List_b5d02807, List_d08202cd]);

                public static readonly PineValue List_8ea850ab = PineValue.List([List_b43468c9, List_56404869]);

                public static readonly PineValue List_3c72a55c = PineValue.List([List_6dbb5f29, List_56404869]);

                public static readonly PineValue List_d9c17fa9 = PineValue.List([List_d08202cd, List_56404869]);

                public static readonly PineValue List_f4cfb374 = PineValue.List([List_35cf017c, List_56404869]);

                public static readonly PineValue List_73f97999 = PineValue.List([List_d181119a, List_e5ac7a1c]);

                public static readonly PineValue List_Single_List_1a6050bf = PineValue.List([List_1a6050bf]);

                public static readonly PineValue List_Single_List_44d35e66 = PineValue.List([List_44d35e66]);

                public static readonly PineValue List_Single_List_8f2aee6f = PineValue.List([List_8f2aee6f]);

                public static readonly PineValue List_Single_List_8ea850ab = PineValue.List([List_8ea850ab]);

                public static readonly PineValue List_Single_List_3c72a55c = PineValue.List([List_3c72a55c]);

                public static readonly PineValue List_Single_List_d9c17fa9 = PineValue.List([List_d9c17fa9]);

                public static readonly PineValue List_Single_List_f4cfb374 = PineValue.List([List_f4cfb374]);

                public static readonly PineValue List_Single_List_73f97999 = PineValue.List([List_73f97999]);

                public static readonly PineValue List_1f27fb25 = PineValue.List([Blob_Str_List, List_Single_List_5043654b]);

                public static readonly PineValue List_23bb8f95 = PineValue.List([Blob_Str_List, List_Single_List_ebc294be]);

                public static readonly PineValue List_69f0dbcb = PineValue.List([Blob_Str_List, List_Single_List_62b47060]);

                public static readonly PineValue List_9cd0e5ac = PineValue.List([Blob_Str_List, List_Single_List_1a6050bf]);

                public static readonly PineValue List_3b615ac8 = PineValue.List([Blob_Str_List, List_Single_List_44d35e66]);

                public static readonly PineValue List_5149771a = PineValue.List([Blob_Str_List, List_Single_List_8f2aee6f]);

                public static readonly PineValue List_8cfa47af = PineValue.List([Blob_Str_List, List_Single_List_8ea850ab]);

                public static readonly PineValue List_97e04f57 = PineValue.List([Blob_Str_List, List_Single_List_3c72a55c]);

                public static readonly PineValue List_a12542a5 = PineValue.List([Blob_Str_List, List_Single_List_d9c17fa9]);

                public static readonly PineValue List_a83d5f28 = PineValue.List([Blob_Str_List, List_Single_List_f4cfb374]);

                public static readonly PineValue List_a6a4e317 = PineValue.List([Blob_Str_List, List_Single_List_73f97999]);

                public static readonly PineValue List_c37ef632 = PineValue.List([Blob_Str_KernelApplication, List_6b896037]);

                public static readonly PineValue List_ee3ba15f = PineValue.List([Blob_Str_KernelApplication, List_3218a5a2]);

                public static readonly PineValue List_a53f1292 = PineValue.List([Blob_Str_head, List_c37ef632]);

                public static readonly PineValue List_290d4c0d = PineValue.List([Blob_Str_head, List_ee3ba15f]);

                public static readonly PineValue List_c000b476 = PineValue.List([Blob_Str_int_mul, List_8cfa47af]);

                public static readonly PineValue List_b98b999e = PineValue.List([Blob_Str_int_mul, List_97e04f57]);

                public static readonly PineValue List_7310c8bc = PineValue.List([Blob_Str_int_mul, List_a12542a5]);

                public static readonly PineValue List_ee9b9f21 = PineValue.List([Blob_Str_int_mul, List_a83d5f28]);

                public static readonly PineValue List_893a0c49 = PineValue.List([Blob_Str_int_mul, List_a6a4e317]);

                public static readonly PineValue List_e2703db8 = PineValue.List([Blob_Str_int_is_sorted_asc, List_9cd0e5ac]);

                public static readonly PineValue List_2586c3c6 = PineValue.List([Blob_Str_int_is_sorted_asc, List_3b615ac8]);

                public static readonly PineValue List_a89dfaa5 = PineValue.List([Blob_Str_int_is_sorted_asc, List_5149771a]);

                public static readonly PineValue List_4af1f0ec = PineValue.List([Blob_Str_KernelApplication, List_a53f1292]);

                public static readonly PineValue List_768cc61e = PineValue.List([Blob_Str_KernelApplication, List_290d4c0d]);

                public static readonly PineValue List_9496b055 = PineValue.List([Blob_Str_KernelApplication, List_c000b476]);

                public static readonly PineValue List_47457b14 = PineValue.List([Blob_Str_KernelApplication, List_b98b999e]);

                public static readonly PineValue List_7a9bb0f9 = PineValue.List([Blob_Str_KernelApplication, List_7310c8bc]);

                public static readonly PineValue List_aa0efd15 = PineValue.List([Blob_Str_KernelApplication, List_ee9b9f21]);

                public static readonly PineValue List_f0f69c95 = PineValue.List([Blob_Str_KernelApplication, List_893a0c49]);

                public static readonly PineValue List_Single_List_f0f69c95 = PineValue.List([List_f0f69c95]);

                public static readonly PineValue List_8f8475f4 = PineValue.List([List_bb14d771, List_4af1f0ec]);

                public static readonly PineValue List_Single_List_8f8475f4 = PineValue.List([List_8f8475f4]);

                public static readonly PineValue List_cb2eac87 = PineValue.List([List_b5d02807, List_4af1f0ec]);

                public static readonly PineValue List_a4ac239c = PineValue.List([List_4af1f0ec, List_56404869]);

                public static readonly PineValue List_84523d46 = PineValue.List([List_4af1f0ec, List_b5d02807]);

                public static readonly PineValue List_1ccfd4d0 = PineValue.List([List_4af1f0ec, List_e5ac7a1c]);

                public static readonly PineValue List_3e1ee66a = PineValue.List([List_768cc61e, List_0dcd86c0]);

                public static readonly PineValue List_Single_List_cb2eac87 = PineValue.List([List_cb2eac87]);

                public static readonly PineValue List_Single_List_a4ac239c = PineValue.List([List_a4ac239c]);

                public static readonly PineValue List_Single_List_84523d46 = PineValue.List([List_84523d46]);

                public static readonly PineValue List_Single_List_1ccfd4d0 = PineValue.List([List_1ccfd4d0]);

                public static readonly PineValue List_Single_List_3e1ee66a = PineValue.List([List_3e1ee66a]);

                public static readonly PineValue List_cda574e6 = PineValue.List([Blob_Str_Literal, List_Single_List_f0f69c95]);

                public static readonly PineValue List_b172fbd6 = PineValue.List([List_d0b6bef5, List_9496b055]);

                public static readonly PineValue List_b81534b6 = PineValue.List([List_d0b6bef5, List_47457b14]);

                public static readonly PineValue List_6d34492d = PineValue.List([List_d0b6bef5, List_7a9bb0f9]);

                public static readonly PineValue List_1d374f35 = PineValue.List([List_6dbb5f29, List_28767619]);

                public static readonly PineValue List_70c22cad = PineValue.List([List_d08202cd, List_28767619]);

                public static readonly PineValue List_2d6356a2 = PineValue.List([List_35cf017c, List_28767619]);

                public static readonly PineValue List_Single_List_b172fbd6 = PineValue.List([List_b172fbd6]);

                public static readonly PineValue List_Single_List_b81534b6 = PineValue.List([List_b81534b6]);

                public static readonly PineValue List_Single_List_6d34492d = PineValue.List([List_6d34492d]);

                public static readonly PineValue List_23afeea9 = PineValue.List([Blob_Str_KernelApplication, List_e2703db8]);

                public static readonly PineValue List_7e774b12 = PineValue.List([Blob_Str_KernelApplication, List_2586c3c6]);

                public static readonly PineValue List_a39c07a7 = PineValue.List([Blob_Str_KernelApplication, List_a89dfaa5]);

                public static readonly PineValue List_6386e244 = PineValue.List([Blob_Str_List, List_Single_List_8f8475f4]);

                public static readonly PineValue List_4d3b6f7f = PineValue.List([Blob_Str_List, List_Single_List_cb2eac87]);

                public static readonly PineValue List_a40c9373 = PineValue.List([Blob_Str_List, List_Single_List_a4ac239c]);

                public static readonly PineValue List_affeafd7 = PineValue.List([Blob_Str_List, List_Single_List_84523d46]);

                public static readonly PineValue List_c34b9c14 = PineValue.List([Blob_Str_List, List_Single_List_1ccfd4d0]);

                public static readonly PineValue List_a377003f = PineValue.List([Blob_Str_List, List_Single_List_3e1ee66a]);

                public static readonly PineValue List_45aab610 = PineValue.List([Blob_Str_List, List_Single_List_b172fbd6]);

                public static readonly PineValue List_0da2d82f = PineValue.List([Blob_Str_List, List_Single_List_b81534b6]);

                public static readonly PineValue List_8a25cb53 = PineValue.List([Blob_Str_List, List_Single_List_6d34492d]);

                public static readonly PineValue List_8baea653 = PineValue.List([Blob_Str_equal, List_affeafd7]);

                public static readonly PineValue List_40c877bc = PineValue.List([Blob_Str_int_add, List_a377003f]);

                public static readonly PineValue List_d9a8bf69 = PineValue.List([Blob_Str_int_mul, List_a40c9373]);

                public static readonly PineValue List_1449c130 = PineValue.List([Blob_Str_int_mul, List_c34b9c14]);

                public static readonly PineValue List_e609c3f7 = PineValue.List([Blob_Str_ParseAndEval, List_1d374f35]);

                public static readonly PineValue List_64c03889 = PineValue.List([Blob_Str_ParseAndEval, List_70c22cad]);

                public static readonly PineValue List_fe772476 = PineValue.List([Blob_Str_ParseAndEval, List_2d6356a2]);

                public static readonly PineValue List_d46b5c67 = PineValue.List([Blob_Str_int_is_sorted_asc, List_4d3b6f7f]);

                public static readonly PineValue List_f01ccf80 = PineValue.List([Blob_Str_KernelApplication, List_8baea653]);

                public static readonly PineValue List_3e268af1 = PineValue.List([Blob_Str_KernelApplication, List_40c877bc]);

                public static readonly PineValue List_66f15e7d = PineValue.List([Blob_Str_KernelApplication, List_d9a8bf69]);

                public static readonly PineValue List_3aad3f38 = PineValue.List([Blob_Str_KernelApplication, List_1449c130]);

                public static readonly PineValue List_b1ba27e4 = PineValue.List([List_d0b6bef5, List_66f15e7d]);

                public static readonly PineValue List_Single_List_b1ba27e4 = PineValue.List([List_b1ba27e4]);

                public static readonly PineValue List_c42737be = PineValue.List([Blob_Str_KernelApplication, List_d46b5c67]);

                public static readonly PineValue List_d003d670 = PineValue.List([Blob_Str_List, List_Single_List_b1ba27e4]);

                public static readonly PineValue List_3868557a =
                    PineValue.List([List_d181119a, List_34849c45, List_e5ac7a1c, List_56404869]);

                public static readonly PineValue List_Single_List_3868557a = PineValue.List([List_3868557a]);

                public static readonly PineValue List_00bc5d71 = PineValue.List([Blob_Str_List, List_Single_List_3868557a]);

                public static readonly PineValue List_bb7397a3 = PineValue.List([Blob_Str_int_mul, List_00bc5d71]);

                public static readonly PineValue List_989a3a14 = PineValue.List([List_4af1f0ec, List_b43468c9]);

                public static readonly PineValue List_Single_List_989a3a14 = PineValue.List([List_989a3a14]);

                public static readonly PineValue List_a511c140 = PineValue.List([Blob_Str_List, List_Single_List_989a3a14]);

                public static readonly PineValue List_ff2295b7 = PineValue.List([Blob_Str_KernelApplication, List_bb7397a3]);

                public static readonly PineValue List_e39e00a7 = PineValue.List([Blob_Str_int_is_sorted_asc, List_a511c140]);

                public static readonly PineValue List_8cdc50cc = PineValue.List([List_64c03889, List_34849c45, List_b5d02807]);

                public static readonly PineValue List_Single_List_8cdc50cc = PineValue.List([List_8cdc50cc]);

                public static readonly PineValue List_bc4e1594 = PineValue.List([Blob_Str_List, List_Single_List_8cdc50cc]);

                public static readonly PineValue List_d6e6a357 = PineValue.List([List_b43468c9, List_66f15e7d]);

                public static readonly PineValue List_0ec3ea68 = PineValue.List([List_3aad3f38, List_b43468c9]);

                public static readonly PineValue List_Single_List_d6e6a357 = PineValue.List([List_d6e6a357]);

                public static readonly PineValue List_Single_List_0ec3ea68 = PineValue.List([List_0ec3ea68]);

                public static readonly PineValue List_957e898f = PineValue.List([Blob_Str_KernelApplication, List_e39e00a7]);

                public static readonly PineValue List_4382973a = PineValue.List([Blob_Str_List, List_Single_List_d6e6a357]);

                public static readonly PineValue List_5ff18f69 = PineValue.List([Blob_Str_List, List_Single_List_0ec3ea68]);

                public static readonly PineValue List_c259ca59 = PineValue.List([List_b43468c9, List_3aad3f38, List_b5d02807]);

                public static readonly PineValue List_Single_List_c259ca59 = PineValue.List([List_c259ca59]);

                public static readonly PineValue List_809325df = PineValue.List([Blob_Str_int_add, List_4382973a]);

                public static readonly PineValue List_db7ebd2d = PineValue.List([Blob_Str_List, List_Single_List_c259ca59]);

                public static readonly PineValue List_e5dfda56 = PineValue.List([Blob_Str_int_is_sorted_asc, List_5ff18f69]);

                public static readonly PineValue List_22593985 = PineValue.List([Blob_Str_KernelApplication, List_809325df]);

                public static readonly PineValue List_c47e7f10 = PineValue.List([List_a5cbaf18, List_bc4e1594]);

                public static readonly PineValue List_Single_List_c47e7f10 = PineValue.List([List_c47e7f10]);

                public static readonly PineValue List_bdd1ba77 = PineValue.List([Blob_Str_List, List_Single_List_c47e7f10]);

                public static readonly PineValue List_95b42d32 = PineValue.List([Blob_Str_KernelApplication, List_e5dfda56]);

                public static readonly PineValue List_cfc86da8 = PineValue.List([List_e609c3f7, List_fe772476]);

                public static readonly PineValue List_Single_List_cfc86da8 = PineValue.List([List_cfc86da8]);

                public static readonly PineValue List_dd4d8ba5 = PineValue.List([List_a5cbaf18, List_db7ebd2d]);

                public static readonly PineValue List_Single_List_dd4d8ba5 = PineValue.List([List_dd4d8ba5]);

                public static readonly PineValue List_d84388e0 = PineValue.List([Blob_Str_List, List_Single_List_cfc86da8]);

                public static readonly PineValue List_dc15d19e = PineValue.List([Blob_Str_List, List_Single_List_dd4d8ba5]);

                public static readonly PineValue List_004c1aaf = PineValue.List([Blob_Str_int_add, List_d84388e0]);

                public static readonly PineValue List_2b9efd45 = PineValue.List([List_86d38b8d, List_ff2295b7]);

                public static readonly PineValue List_Single_List_2b9efd45 = PineValue.List([List_2b9efd45]);

                public static readonly PineValue List_86a6fff8 = PineValue.List([Blob_Str_List, List_Single_List_2b9efd45]);

                public static readonly PineValue List_6aa88603 = PineValue.List([Blob_Str_KernelApplication, List_004c1aaf]);

                public static readonly PineValue List_Single_List_6aa88603 = PineValue.List([List_6aa88603]);

                public static readonly PineValue List_a66110fb = PineValue.List([Blob_Str_int_add, List_86a6fff8]);

                public static readonly PineValue List_8931d3ee = PineValue.List([Blob_Str_Literal, List_Single_List_6aa88603]);

                public static readonly PineValue List_504c32b9 = PineValue.List([List_976730e9, List_bdd1ba77]);

                public static readonly PineValue List_24f0b814 = PineValue.List([Blob_Str_KernelApplication, List_a66110fb]);

                public static readonly PineValue List_Single_List_24f0b814 = PineValue.List([List_24f0b814]);

                public static readonly PineValue List_83b118fe = PineValue.List([Blob_Str_Literal, List_Single_List_24f0b814]);

                public static readonly PineValue List_d56d5d5b = PineValue.List([Blob_Str_ParseAndEval, List_504c32b9]);

                public static readonly PineValue List_Single_List_d56d5d5b = PineValue.List([List_d56d5d5b]);

                public static readonly PineValue List_bfc6ced2 = PineValue.List([List_976730e9, List_dc15d19e]);

                public static readonly PineValue List_10b825a0 = PineValue.List([Blob_Str_Literal, List_Single_List_d56d5d5b]);

                public static readonly PineValue List_43554e47 = PineValue.List([Blob_Str_ParseAndEval, List_bfc6ced2]);

                public static readonly PineValue List_8e7447f8 = PineValue.List([List_23afeea9, List_45aab610, List_1f27fb25]);

                public static readonly PineValue List_17675c6d = PineValue.List([List_7e774b12, List_0da2d82f, List_23bb8f95]);

                public static readonly PineValue List_0fcda3c8 = PineValue.List([List_a39c07a7, List_8a25cb53, List_69f0dbcb]);

                public static readonly PineValue List_de67f37d = PineValue.List([Blob_Str_Conditional, List_8e7447f8]);

                public static readonly PineValue List_b2424253 = PineValue.List([Blob_Str_Conditional, List_17675c6d]);

                public static readonly PineValue List_2fc55208 = PineValue.List([Blob_Str_Conditional, List_0fcda3c8]);

                public static readonly PineValue List_191d6742 = PineValue.List([Blob_Str_head, List_b2424253]);

                public static readonly PineValue List_355577ca = PineValue.List([Blob_Str_head, List_2fc55208]);

                public static readonly PineValue List_0245b2cf = PineValue.List([List_0dcd86c0, List_de67f37d]);

                public static readonly PineValue List_Single_List_0245b2cf = PineValue.List([List_0245b2cf]);

                public static readonly PineValue List_454e3b62 = PineValue.List([Blob_Str_List, List_Single_List_0245b2cf]);

                public static readonly PineValue List_1d181cbb = PineValue.List([Blob_Str_skip, List_454e3b62]);

                public static readonly PineValue List_0e5d5f28 = PineValue.List([Blob_Str_KernelApplication, List_191d6742]);

                public static readonly PineValue List_4338879c = PineValue.List([Blob_Str_KernelApplication, List_355577ca]);

                public static readonly PineValue List_c6fbbbde = PineValue.List([Blob_Str_KernelApplication, List_1d181cbb]);

                public static readonly PineValue List_09a8370d = PineValue.List([Blob_Str_head, List_c6fbbbde]);

                public static readonly PineValue List_ce5b6f29 = PineValue.List([Blob_Str_KernelApplication, List_09a8370d]);

                public static readonly PineValue List_dd76f543 = PineValue.List([List_c42737be, List_d003d670, List_6386e244]);

                public static readonly PineValue List_07385e0e = PineValue.List([Blob_Str_Conditional, List_dd76f543]);

                public static readonly PineValue List_a060ab93 = PineValue.List([List_0dcd86c0, List_07385e0e]);

                public static readonly PineValue List_Single_List_a060ab93 = PineValue.List([List_a060ab93]);

                public static readonly PineValue List_a6fd9c1f = PineValue.List([Blob_Str_List, List_Single_List_a060ab93]);

                public static readonly PineValue List_b1cc18a9 = PineValue.List([Blob_Str_skip, List_a6fd9c1f]);

                public static readonly PineValue List_6129cd4b = PineValue.List([Blob_Str_KernelApplication, List_b1cc18a9]);

                public static readonly PineValue List_3540f806 = PineValue.List([Blob_Str_head, List_6129cd4b]);

                public static readonly PineValue List_c27949d0 = PineValue.List([Blob_Str_KernelApplication, List_3540f806]);

                public static readonly PineValue List_6023ebe2 = PineValue.List([List_22593985, List_4af1f0ec, List_3e268af1]);

                public static readonly PineValue List_Single_List_6023ebe2 = PineValue.List([List_6023ebe2]);

                public static readonly PineValue List_059c3d0e = PineValue.List([Blob_Str_List, List_Single_List_6023ebe2]);

                public static readonly PineValue List_04c1ebd4 = PineValue.List([List_a5cbaf18, List_059c3d0e]);

                public static readonly PineValue List_Single_List_04c1ebd4 = PineValue.List([List_04c1ebd4]);

                public static readonly PineValue List_fa3a72bb = PineValue.List([Blob_Str_List, List_Single_List_04c1ebd4]);

                public static readonly PineValue List_bb804557 = PineValue.List([List_976730e9, List_fa3a72bb]);

                public static readonly PineValue List_ca198a11 = PineValue.List([Blob_Str_ParseAndEval, List_bb804557]);

                public static readonly PineValue List_a1c1583d = PineValue.List([List_0e5d5f28, List_4338879c]);

                public static readonly PineValue List_Single_List_a1c1583d = PineValue.List([List_a1c1583d]);

                public static readonly PineValue List_5d80bbdc = PineValue.List([Blob_Str_List, List_Single_List_a1c1583d]);

                public static readonly PineValue List_1badf006 = PineValue.List([Blob_Str_equal, List_5d80bbdc]);

                public static readonly PineValue List_3bcf95d7 = PineValue.List([Blob_Str_KernelApplication, List_1badf006]);

                public static readonly PineValue List_55091f63 = PineValue.List([List_ce5b6f29, List_c27949d0, List_b5d02807]);

                public static readonly PineValue List_Single_List_55091f63 = PineValue.List([List_55091f63]);

                public static readonly PineValue List_95935e34 = PineValue.List([Blob_Str_List, List_Single_List_55091f63]);

                public static readonly PineValue List_4e4cd185 = PineValue.List([List_a5cbaf18, List_95935e34]);

                public static readonly PineValue List_Single_List_4e4cd185 = PineValue.List([List_4e4cd185]);

                public static readonly PineValue List_3f9515cb = PineValue.List([Blob_Str_List, List_Single_List_4e4cd185]);

                public static readonly PineValue List_661408a9 = PineValue.List([List_957e898f, List_768cc61e, List_ca198a11]);

                public static readonly PineValue List_f9eea0f0 = PineValue.List([Blob_Str_Conditional, List_661408a9]);

                public static readonly PineValue List_bf404c82 = PineValue.List([List_3bcf95d7, List_aa0efd15, List_35cf017c]);

                public static readonly PineValue List_71c0f142 = PineValue.List([Blob_Str_Conditional, List_bf404c82]);

                public static readonly PineValue List_Single_List_71c0f142 = PineValue.List([List_71c0f142]);

                public static readonly PineValue List_602d25fc = PineValue.List([Blob_Str_Literal, List_Single_List_71c0f142]);

                public static readonly PineValue List_dc598f43 = PineValue.List([List_976730e9, List_3f9515cb]);

                public static readonly PineValue List_2a07a877 = PineValue.List([Blob_Str_ParseAndEval, List_dc598f43]);

                public static readonly PineValue List_f3328233 =
                    PineValue.List([List_976730e9, List_b43468c9, List_4af1f0ec, List_2a07a877]);

                public static readonly PineValue List_Single_List_f3328233 = PineValue.List([List_f3328233]);

                public static readonly PineValue List_bdc906ba = PineValue.List([Blob_Str_List, List_Single_List_f3328233]);

                public static readonly PineValue List_5e888933 = PineValue.List([List_bdc906ba, List_e190d1f5]);

                public static readonly PineValue List_Single_List_5e888933 = PineValue.List([List_5e888933]);

                public static readonly PineValue List_b89681ec = PineValue.List([Blob_Str_List, List_Single_List_5e888933]);

                public static readonly PineValue List_69645e6c =
                    PineValue.List(
                        [List_976730e9, List_cda574e6, List_83b118fe, List_10b825a0, List_b43468c9, List_4af1f0ec, List_43554e47]);

                public static readonly PineValue List_Single_List_69645e6c = PineValue.List([List_69645e6c]);

                public static readonly PineValue List_e425f5e7 = PineValue.List([Blob_Str_List, List_Single_List_69645e6c]);

                public static readonly PineValue List_7d7f2b10 = PineValue.List([List_e425f5e7, List_e190d1f5]);

                public static readonly PineValue List_Single_List_7d7f2b10 = PineValue.List([List_7d7f2b10]);

                public static readonly PineValue List_3368e129 = PineValue.List([Blob_Str_List, List_Single_List_7d7f2b10]);

                public static readonly PineValue List_219a6270 = PineValue.List([List_8931d3ee, List_3368e129]);

                public static readonly PineValue List_4c4cced5 = PineValue.List([Blob_Str_ParseAndEval, List_219a6270]);

                public static readonly PineValue List_cc74513b = PineValue.List([List_602d25fc, List_b89681ec]);

                public static readonly PineValue List_612de97a = PineValue.List([Blob_Str_ParseAndEval, List_cc74513b]);

                public static readonly PineValue List_8f389de6 = PineValue.List([List_f01ccf80, List_612de97a, List_b5d02807]);

                public static readonly PineValue List_9a1e26cb = PineValue.List([Blob_Str_Conditional, List_8f389de6]);

                public static readonly PineValue List_ab0140cc = PineValue.List([List_95b42d32, List_f9eea0f0, List_4c4cced5]);

                public static readonly PineValue List_1c954123 = PineValue.List([Blob_Str_Conditional, List_ab0140cc]);
            }
            """".Trim());

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
            FunctionRecord.ParseFunctionRecordTagged(idivDeclValue, parseCache)
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
