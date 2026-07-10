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

        var (parsedEnv, staticProgram, functionMetadata) =
            CodeAnalysisTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                includeDeclaration:
                declName =>
                {
                    return declName.FullName == "Test.idiv";
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram, functionMetadata);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.idiv param_1 param_2 =
                if
                    Pine_kernel.equal
                        [ param_2
                        , 0
                        ]
                then
                    0

                else if
                    Pine_kernel.equal
                        [ if
                            Pine_kernel.int_is_sorted_asc
                                [ 0
                                , param_1
                                ]
                          then
                            False

                          else
                            True
                        , if
                            Pine_kernel.int_is_sorted_asc
                                [ 0
                                , param_2
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
                                , param_1
                                ]
                        then
                            param_1

                        else
                            Pine_kernel.int_mul
                                [ param_1
                                , -1
                                ]
                        if
                            Pine_kernel.int_is_sorted_asc
                                [ 0
                                , param_2
                                ]
                        then
                            param_2

                        else
                            Pine_kernel.int_mul
                                [ param_2
                                , -1
                                ]
                        0

                else
                    Pine_kernel.int_mul
                        [ Test.idivHelper
                            if
                                Pine_kernel.int_is_sorted_asc
                                    [ 0
                                    , param_1
                                    ]
                            then
                                param_1

                            else
                                Pine_kernel.int_mul
                                    [ param_1
                                    , -1
                                    ]
                            if
                                Pine_kernel.int_is_sorted_asc
                                    [ 0
                                    , param_2
                                    ]
                            then
                                param_2

                            else
                                Pine_kernel.int_mul
                                    [ param_2
                                    , -1
                                    ]
                            0
                        , -1
                        ]


            Test.idivHelper param_1 param_2 param_3 =
                if
                    Pine_kernel.int_is_sorted_asc
                        [ Pine_kernel.int_mul
                            [ param_2
                            , 17
                            ]
                        , param_1
                        ]
                then
                    Pine_kernel.int_add
                        [ Pine_kernel.int_mul
                            [ Test.idivHelper
                                param_1
                                (Pine_kernel.int_mul
                                    [ param_2
                                    , 17
                                    ]
                                )
                                0
                            , 17
                            ]
                        , Test.idivHelper
                            (Pine_kernel.int_add
                                [ param_1
                                , Pine_kernel.int_mul
                                    [ -17
                                    , Test.idivHelper
                                        param_1
                                        (Pine_kernel.int_mul
                                            [ param_2
                                            , 17
                                            ]
                                        )
                                        0
                                    , param_2
                                    ]
                                ]
                            )
                            param_2
                            0
                        ]

                else if
                    Pine_kernel.int_is_sorted_asc
                        [ param_2
                        , param_1
                        ]
                then
                    Test.idivHelper
                        (Pine_kernel.int_add
                            [ param_1
                            , Pine_kernel.int_mul
                                [ param_2
                                , -1
                                ]
                            ]
                        )
                        param_2
                        (Pine_kernel.int_add
                            [ param_3
                            , 1
                            ]
                        )

                else
                    param_3
            """".Trim());

        var asCSharp =
            StaticProgramCSharp.FromStaticProgram(
                staticProgram,
                functionMetadata,
                CodeAnalysisTestHelper.DeclarationSyntaxContext);

        var testClass = asCSharp.ModulesClasses[DeclQualifiedName.Create([], "Test")];

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
                public static PineValue idiv(PineValue param_1, PineValue param_2)
                {
                    if (param_2 == CommonReusedValues.Blob_Int_0)
                    {
                        return CommonReusedValues.Blob_Int_0;
                    }

                    PineValue local_002 =
                        KernelFunction.ValueFromBool(KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(0, param_1));

                    PineValue local_003 =
                        KernelFunction.ValueFromBool(KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(0, param_2));

                    PineValue local_004 =
                        local_002 == PineKernelValues.TrueValue ? param_1 : KernelFunctionSpecialized.int_mul(-1, param_1);

                    PineValue local_005 =
                        local_003 == PineKernelValues.TrueValue ? param_2 : KernelFunctionSpecialized.int_mul(-1, param_2);

                    PineValue local_007 = Test.idivHelper(local_004, local_005, CommonReusedValues.Blob_Int_0);

                    if ((local_002 == PineKernelValues.TrueValue ? false : true) ==
                        (local_003 == PineKernelValues.TrueValue ? false : true))
                    {
                        return local_007;
                    }

                    return KernelFunctionSpecialized.int_mul(-1, local_007);
                }

                public static PineValue idivHelper(PineValue param_1, PineValue param_2, PineValue param_3)
                {
                    PineValue local_param_1 = param_1;
                    PineValue local_param_2 = param_2;
                    PineValue local_param_3 = param_3;

                    while (true)
                    {
                        PineValue local_000 = KernelFunctionSpecialized.int_mul(17, local_param_2);

                        if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(local_000, local_param_1))
                        {
                            PineValue local_001 = Test.idivHelper(local_param_1, local_000, CommonReusedValues.Blob_Int_0);

                            return
                                KernelFunctionSpecialized.int_add(
                                    KernelFunctionSpecialized.int_mul(17, local_001),
                                    Test.idivHelper(
                                        KernelFunctionSpecialized.int_add(
                                            local_param_1,
                                            KernelFunctionSpecialized.int_mul(local_001, local_param_2, -17)),
                                        local_param_2,
                                        CommonReusedValues.Blob_Int_0));
                        }

                        if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(local_param_2, local_param_1))
                        {
                            {
                                PineValue local_param_1_temp =
                                    KernelFunctionSpecialized.int_add(
                                        local_param_1,
                                        KernelFunctionSpecialized.int_mul(-1, local_param_2));

                                PineValue local_param_3_temp = KernelFunctionSpecialized.int_add(1, local_param_3);
                                local_param_1 = local_param_1_temp;
                                local_param_3 = local_param_3_temp;
                            }

                            continue;
                        }

                        return local_param_3;
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
                    dict[CommonReusedValues.List_1c49c845] = Dispatch_1c49c845;
                    dict[CommonReusedValues.List_6b1fa514] = Dispatch_6b1fa514;
                    return dict;
                }

                public static PineValue? Dispatch_1c49c845(PineValue environment)
                {
                    if (true)
                    {
                        var arg_1 = PineValueExtension.ValueFromPathOrEmptyList(environment, [1]);
                        var arg_2 = PineValueExtension.ValueFromPathOrEmptyList(environment, [2]);
                        return Test.idiv(arg_1, arg_2);
                    }

                    return null;
                }

                public static PineValue? Dispatch_6b1fa514(PineValue environment)
                {
                    if (PineValueExtension.ValueFromPathOrEmptyList(environment, [0, 0]) == CommonReusedValues.List_6b1fa514)
                    {
                        var arg_1 = PineValueExtension.ValueFromPathOrEmptyList(environment, [1]);
                        var arg_2 = PineValueExtension.ValueFromPathOrEmptyList(environment, [2]);
                        var arg_3 = PineValueExtension.ValueFromPathOrEmptyList(environment, [3]);
                        return Test.idivHelper(arg_1, arg_2, arg_3);
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

                public static readonly PineValue List_Single_Bool_False = PineValue.List([PineKernelValues.FalseValue]);

                public static readonly PineValue List_Single_Bool_True = PineValue.List([PineKernelValues.TrueValue]);

                public static readonly PineValue List_Single_Blob_Int_neg_1 =
                    PineValue.List([IntegerEncoding.EncodeSignedInteger(-1)]);

                public static readonly PineValue List_Single_Blob_Int_neg_17 =
                    PineValue.List([IntegerEncoding.EncodeSignedInteger(-17)]);

                public static readonly PineValue List_Single_Blob_Int_0 = PineValue.List([IntegerEncoding.EncodeSignedInteger(0)]);

                public static readonly PineValue List_Single_Blob_Int_1 = PineValue.List([IntegerEncoding.EncodeSignedInteger(1)]);

                public static readonly PineValue List_Single_Blob_Int_2 = PineValue.List([IntegerEncoding.EncodeSignedInteger(2)]);

                public static readonly PineValue List_Single_Blob_Int_3 = PineValue.List([IntegerEncoding.EncodeSignedInteger(3)]);

                public static readonly PineValue List_Single_Blob_Int_17 =
                    PineValue.List([IntegerEncoding.EncodeSignedInteger(17)]);

                public static readonly PineValue List_bb14d771 = PineValue.List([Blob_Str_Literal, List_Single_Bool_False]);

                public static readonly PineValue List_d0b6bef5 = PineValue.List([Blob_Str_Literal, List_Single_Bool_True]);

                public static readonly PineValue List_56404869 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_neg_1]);

                public static readonly PineValue List_451f602b = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_neg_17]);

                public static readonly PineValue List_b5d02807 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_0]);

                public static readonly PineValue List_0dcd86c0 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_1]);

                public static readonly PineValue List_43b95777 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_2]);

                public static readonly PineValue List_450c12a0 = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_3]);

                public static readonly PineValue List_e5ac7a1c = PineValue.List([Blob_Str_Literal, List_Single_Blob_Int_17]);

                public static readonly PineValue List_bd06385a = PineValue.List([Blob_Str_Environment, PineValue.EmptyList]);

                public static readonly PineValue List_4aad0d20 = PineValue.List([Blob_Str_head, List_bd06385a]);

                public static readonly PineValue List_c1cc00b1 = PineValue.List([List_0dcd86c0, List_bd06385a]);

                public static readonly PineValue List_71a831bc = PineValue.List([List_43b95777, List_bd06385a]);

                public static readonly PineValue List_5f90b1c8 = PineValue.List([List_450c12a0, List_bd06385a]);

                public static readonly PineValue List_Single_List_c1cc00b1 = PineValue.List([List_c1cc00b1]);

                public static readonly PineValue List_Single_List_71a831bc = PineValue.List([List_71a831bc]);

                public static readonly PineValue List_Single_List_5f90b1c8 = PineValue.List([List_5f90b1c8]);

                public static readonly PineValue List_4c301747 = PineValue.List([Blob_Str_List, List_Single_List_c1cc00b1]);

                public static readonly PineValue List_7ed5803b = PineValue.List([Blob_Str_List, List_Single_List_71a831bc]);

                public static readonly PineValue List_7b3639f9 = PineValue.List([Blob_Str_List, List_Single_List_5f90b1c8]);

                public static readonly PineValue List_b49facc1 = PineValue.List([Blob_Str_skip, List_4c301747]);

                public static readonly PineValue List_0c396738 = PineValue.List([Blob_Str_skip, List_7ed5803b]);

                public static readonly PineValue List_fc75d0e2 = PineValue.List([Blob_Str_skip, List_7b3639f9]);

                public static readonly PineValue List_a5cbaf18 = PineValue.List([Blob_Str_KernelApplication, List_4aad0d20]);

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

                public static readonly PineValue List_2608698a = PineValue.List([List_b5d02807, List_f1126ab7]);

                public static readonly PineValue List_42c7fba5 = PineValue.List([List_b5d02807, List_3c738443]);

                public static readonly PineValue List_bdcd5406 = PineValue.List([List_f1126ab7, List_56404869]);

                public static readonly PineValue List_ab14fbc0 = PineValue.List([List_3c738443, List_56404869]);

                public static readonly PineValue List_0fdbcb26 = PineValue.List([List_3c738443, List_b5d02807]);

                public static readonly PineValue List_163ac448 = PineValue.List([List_3c738443, List_e5ac7a1c]);

                public static readonly PineValue List_b31ea4ee = PineValue.List([List_3f9010d5, List_0dcd86c0]);

                public static readonly PineValue List_Single_List_2608698a = PineValue.List([List_2608698a]);

                public static readonly PineValue List_Single_List_42c7fba5 = PineValue.List([List_42c7fba5]);

                public static readonly PineValue List_Single_List_bdcd5406 = PineValue.List([List_bdcd5406]);

                public static readonly PineValue List_Single_List_ab14fbc0 = PineValue.List([List_ab14fbc0]);

                public static readonly PineValue List_Single_List_0fdbcb26 = PineValue.List([List_0fdbcb26]);

                public static readonly PineValue List_Single_List_163ac448 = PineValue.List([List_163ac448]);

                public static readonly PineValue List_Single_List_b31ea4ee = PineValue.List([List_b31ea4ee]);

                public static readonly PineValue List_3605773f = PineValue.List([Blob_Str_List, List_Single_List_2608698a]);

                public static readonly PineValue List_80799df0 = PineValue.List([Blob_Str_List, List_Single_List_42c7fba5]);

                public static readonly PineValue List_99d55fc2 = PineValue.List([Blob_Str_List, List_Single_List_bdcd5406]);

                public static readonly PineValue List_eb413989 = PineValue.List([Blob_Str_List, List_Single_List_ab14fbc0]);

                public static readonly PineValue List_7d8f600a = PineValue.List([Blob_Str_List, List_Single_List_0fdbcb26]);

                public static readonly PineValue List_6908924e = PineValue.List([Blob_Str_List, List_Single_List_163ac448]);

                public static readonly PineValue List_8973c9c4 = PineValue.List([Blob_Str_List, List_Single_List_b31ea4ee]);

                public static readonly PineValue List_642b6731 = PineValue.List([Blob_Str_equal, List_7d8f600a]);

                public static readonly PineValue List_587a1866 = PineValue.List([Blob_Str_int_add, List_8973c9c4]);

                public static readonly PineValue List_60cf9719 = PineValue.List([Blob_Str_int_mul, List_99d55fc2]);

                public static readonly PineValue List_5e9539db = PineValue.List([Blob_Str_int_mul, List_eb413989]);

                public static readonly PineValue List_f07a65cb = PineValue.List([Blob_Str_int_mul, List_6908924e]);

                public static readonly PineValue List_9f3c535f = PineValue.List([Blob_Str_int_is_sorted_asc, List_3605773f]);

                public static readonly PineValue List_0e49e1b6 = PineValue.List([Blob_Str_int_is_sorted_asc, List_80799df0]);

                public static readonly PineValue List_fd952de8 = PineValue.List([Blob_Str_KernelApplication, List_642b6731]);

                public static readonly PineValue List_fc095f03 = PineValue.List([Blob_Str_KernelApplication, List_587a1866]);

                public static readonly PineValue List_62b04467 = PineValue.List([Blob_Str_KernelApplication, List_60cf9719]);

                public static readonly PineValue List_9a543148 = PineValue.List([Blob_Str_KernelApplication, List_5e9539db]);

                public static readonly PineValue List_574fb09f = PineValue.List([Blob_Str_KernelApplication, List_f07a65cb]);

                public static readonly PineValue List_ab68cdc0 = PineValue.List([Blob_Str_KernelApplication, List_9f3c535f]);

                public static readonly PineValue List_a91b07e1 = PineValue.List([Blob_Str_KernelApplication, List_0e49e1b6]);

                public static readonly PineValue List_afebc09e = PineValue.List([List_ab68cdc0, List_d0b6bef5, List_bb14d771]);

                public static readonly PineValue List_78351f38 = PineValue.List([List_a91b07e1, List_d0b6bef5, List_bb14d771]);

                public static readonly PineValue List_2886d4ff = PineValue.List([List_3c738443, List_f1126ab7]);

                public static readonly PineValue List_Single_List_2886d4ff = PineValue.List([List_2886d4ff]);

                public static readonly PineValue List_8315cc98 = PineValue.List([Blob_Str_List, List_Single_List_2886d4ff]);

                public static readonly PineValue List_a3f503c6 = PineValue.List([Blob_Str_Conditional, List_afebc09e]);

                public static readonly PineValue List_3de8d9f3 = PineValue.List([Blob_Str_Conditional, List_78351f38]);

                public static readonly PineValue List_b9d198ee = PineValue.List([Blob_Str_int_is_sorted_asc, List_8315cc98]);

                public static readonly PineValue List_3816dd85 = PineValue.List([List_f1126ab7, List_9a543148]);

                public static readonly PineValue List_897a310c = PineValue.List([List_574fb09f, List_f1126ab7]);

                public static readonly PineValue List_Single_List_3816dd85 = PineValue.List([List_3816dd85]);

                public static readonly PineValue List_Single_List_897a310c = PineValue.List([List_897a310c]);

                public static readonly PineValue List_931637c8 = PineValue.List([Blob_Str_KernelApplication, List_b9d198ee]);

                public static readonly PineValue List_b8ccaffd = PineValue.List([Blob_Str_List, List_Single_List_3816dd85]);

                public static readonly PineValue List_ee5c9448 = PineValue.List([Blob_Str_List, List_Single_List_897a310c]);

                public static readonly PineValue List_88ce7b21 = PineValue.List([Blob_Str_int_add, List_b8ccaffd]);

                public static readonly PineValue List_7a9fd3f3 = PineValue.List([Blob_Str_int_is_sorted_asc, List_ee5c9448]);

                public static readonly PineValue List_3811a644 = PineValue.List([Blob_Str_KernelApplication, List_88ce7b21]);

                public static readonly PineValue List_0f550e7d = PineValue.List([Blob_Str_KernelApplication, List_7a9fd3f3]);

                public static readonly PineValue List_47c183bf =
                    PineValue.List([List_a5cbaf18, List_f1126ab7, List_574fb09f, List_b5d02807]);

                public static readonly PineValue List_Single_List_47c183bf = PineValue.List([List_47c183bf]);

                public static readonly PineValue List_a386c83d = PineValue.List([Blob_Str_List, List_Single_List_47c183bf]);

                public static readonly PineValue List_4c37bab0 = PineValue.List([List_976730e9, List_a386c83d]);

                public static readonly PineValue List_dbeb286b = PineValue.List([List_a3f503c6, List_3de8d9f3]);

                public static readonly PineValue List_Single_List_dbeb286b = PineValue.List([List_dbeb286b]);

                public static readonly PineValue List_28505e20 = PineValue.List([Blob_Str_ParseAndEval, List_4c37bab0]);

                public static readonly PineValue List_51377543 = PineValue.List([List_ab68cdc0, List_62b04467, List_f1126ab7]);

                public static readonly PineValue List_3e66c22f = PineValue.List([List_a91b07e1, List_9a543148, List_3c738443]);

                public static readonly PineValue List_a226535c = PineValue.List([Blob_Str_List, List_Single_List_dbeb286b]);

                public static readonly PineValue List_ec1e5fbf = PineValue.List([List_28505e20, List_e5ac7a1c]);

                public static readonly PineValue List_Single_List_ec1e5fbf = PineValue.List([List_ec1e5fbf]);

                public static readonly PineValue List_fbffa927 = PineValue.List([Blob_Str_equal, List_a226535c]);

                public static readonly PineValue List_2bddf3b0 = PineValue.List([Blob_Str_List, List_Single_List_ec1e5fbf]);

                public static readonly PineValue List_b601e81d = PineValue.List([Blob_Str_Conditional, List_51377543]);

                public static readonly PineValue List_82788b53 = PineValue.List([Blob_Str_Conditional, List_3e66c22f]);

                public static readonly PineValue List_b5d8a2f4 = PineValue.List([Blob_Str_int_mul, List_2bddf3b0]);

                public static readonly PineValue List_a70b5523 = PineValue.List([Blob_Str_KernelApplication, List_fbffa927]);

                public static readonly PineValue List_8e9840ce = PineValue.List([Blob_Str_KernelApplication, List_b5d8a2f4]);

                public static readonly PineValue List_ea65f470 = PineValue.List([List_451f602b, List_28505e20, List_3c738443]);

                public static readonly PineValue List_Single_List_ea65f470 = PineValue.List([List_ea65f470]);

                public static readonly PineValue List_ea679047 = PineValue.List([Blob_Str_List, List_Single_List_ea65f470]);

                public static readonly PineValue List_7cefe4b7 = PineValue.List([Blob_Str_int_mul, List_ea679047]);

                public static readonly PineValue List_cfa32b66 = PineValue.List([Blob_Str_KernelApplication, List_7cefe4b7]);

                public static readonly PineValue List_b42c33d1 =
                    PineValue.List([List_a5cbaf18, List_3811a644, List_3c738443, List_fc095f03]);

                public static readonly PineValue List_Single_List_b42c33d1 = PineValue.List([List_b42c33d1]);

                public static readonly PineValue List_cfa97b6a = PineValue.List([Blob_Str_List, List_Single_List_b42c33d1]);

                public static readonly PineValue List_2e2182e1 = PineValue.List([List_f1126ab7, List_cfa32b66]);

                public static readonly PineValue List_Single_List_2e2182e1 = PineValue.List([List_2e2182e1]);

                public static readonly PineValue List_f207fd7c = PineValue.List([Blob_Str_List, List_Single_List_2e2182e1]);

                public static readonly PineValue List_ccbd0905 = PineValue.List([List_976730e9, List_cfa97b6a]);

                public static readonly PineValue List_5ce7ff79 = PineValue.List([Blob_Str_int_add, List_f207fd7c]);

                public static readonly PineValue List_b6bc700a = PineValue.List([Blob_Str_ParseAndEval, List_ccbd0905]);

                public static readonly PineValue List_cb0af8dc = PineValue.List([Blob_Str_KernelApplication, List_5ce7ff79]);

                public static readonly PineValue List_4350956d =
                    PineValue.List([List_a5cbaf18, List_cb0af8dc, List_3c738443, List_b5d02807]);

                public static readonly PineValue List_Single_List_4350956d = PineValue.List([List_4350956d]);

                public static readonly PineValue List_df5f9531 = PineValue.List([Blob_Str_List, List_Single_List_4350956d]);

                public static readonly PineValue List_443b9d15 = PineValue.List([List_976730e9, List_df5f9531]);

                public static readonly PineValue List_f84f623c = PineValue.List([Blob_Str_ParseAndEval, List_443b9d15]);

                public static readonly PineValue List_b9701336 = PineValue.List([List_931637c8, List_3f9010d5, List_b6bc700a]);

                public static readonly PineValue List_7c73d4fa = PineValue.List([Blob_Str_Conditional, List_b9701336]);

                public static readonly PineValue List_c56d9334 = PineValue.List([List_8e9840ce, List_f84f623c]);

                public static readonly PineValue List_Single_List_c56d9334 = PineValue.List([List_c56d9334]);

                public static readonly PineValue List_687a4db7 = PineValue.List([Blob_Str_List, List_Single_List_c56d9334]);

                public static readonly PineValue List_80fe2c01 = PineValue.List([Blob_Str_int_add, List_687a4db7]);

                public static readonly PineValue List_e9346b6f = PineValue.List([Blob_Str_KernelApplication, List_80fe2c01]);

                public static readonly PineValue List_ed4d2d7b = PineValue.List([List_0f550e7d, List_7c73d4fa, List_e9346b6f]);

                public static readonly PineValue List_6b1fa514 = PineValue.List([Blob_Str_Conditional, List_ed4d2d7b]);

                public static readonly PineValue List_Single_List_6b1fa514 = PineValue.List([List_6b1fa514]);

                public static readonly PineValue List_Single_List_Single_List_6b1fa514 =
                    PineValue.List([List_Single_List_6b1fa514]);

                public static readonly PineValue List_0d41d4f1 = PineValue.List([Blob_Str_Literal, List_Single_List_6b1fa514]);

                public static readonly PineValue List_e7540ae2 =
                    PineValue.List([Blob_Str_Literal, List_Single_List_Single_List_6b1fa514]);

                public static readonly PineValue List_f4c4ba98 =
                    PineValue.List([List_e7540ae2, List_b601e81d, List_82788b53, List_b5d02807]);

                public static readonly PineValue List_Single_List_f4c4ba98 = PineValue.List([List_f4c4ba98]);

                public static readonly PineValue List_4173242b = PineValue.List([Blob_Str_List, List_Single_List_f4c4ba98]);

                public static readonly PineValue List_6245fbe0 = PineValue.List([List_0d41d4f1, List_4173242b]);

                public static readonly PineValue List_ebfb6f08 = PineValue.List([Blob_Str_ParseAndEval, List_6245fbe0]);

                public static readonly PineValue List_4e287315 = PineValue.List([List_ebfb6f08, List_56404869]);

                public static readonly PineValue List_Single_List_4e287315 = PineValue.List([List_4e287315]);

                public static readonly PineValue List_9549c265 = PineValue.List([Blob_Str_List, List_Single_List_4e287315]);

                public static readonly PineValue List_f1e34858 = PineValue.List([Blob_Str_int_mul, List_9549c265]);

                public static readonly PineValue List_45691cbe = PineValue.List([Blob_Str_KernelApplication, List_f1e34858]);

                public static readonly PineValue List_634d4f35 = PineValue.List([List_a70b5523, List_45691cbe, List_ebfb6f08]);

                public static readonly PineValue List_02c54abc = PineValue.List([Blob_Str_Conditional, List_634d4f35]);

                public static readonly PineValue List_35d12d6f = PineValue.List([List_fd952de8, List_02c54abc, List_b5d02807]);

                public static readonly PineValue List_1c49c845 = PineValue.List([Blob_Str_Conditional, List_35d12d6f]);
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

        // Implementation detail: With the flat calling convention of the current
        // compiler, 'idiv' does not capture any environment functions.

        idivFunctionRecord.EnvFunctions.Length.Should().Be(0);

        var buildApp =
            NamesFromCompiledEnv.BuildApplicationFromFunctionRecord(
                idivFunctionRecord,
                arguments: [],
                parseCache: parseCache);

        // The runtime environment is uniformly [envFunctions, arg0, arg1, ...].

        var callEnvValue =
            PineValue.List(
                [
                PineValue.List(idivFunctionRecord.EnvFunctions.ToArray()),
                IntegerEncoding.EncodeSignedInteger(100),
                IntegerEncoding.EncodeSignedInteger(3),
                ]);

        var dictEntry =
            compiledDictionary[buildApp.encodedExpr];

        dictEntry.Should().NotBeNull();

        var resultValue = dictEntry(callEnvValue);

        resultValue.Should().NotBeNull();

        resultValue.Should().Be(IntegerEncoding.EncodeSignedInteger(33));
    }
}
