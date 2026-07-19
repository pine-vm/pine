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
                        BuiltinFunction.ValueFromBool(BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(0, param_1));

                    PineValue local_003 =
                        BuiltinFunction.ValueFromBool(BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(0, param_2));

                    PineValue local_004 =
                        local_002 == PineKernelValues.TrueValue ? param_1 : BuiltinFunctionSpecialized.int_mul(-1, param_1);

                    PineValue local_005 =
                        local_003 == PineKernelValues.TrueValue ? param_2 : BuiltinFunctionSpecialized.int_mul(-1, param_2);

                    PineValue local_007 = Test.idivHelper(local_004, local_005, CommonReusedValues.Blob_Int_0);

                    if ((local_002 == PineKernelValues.TrueValue ? false : true) ==
                        (local_003 == PineKernelValues.TrueValue ? false : true))
                    {
                        return local_007;
                    }

                    return BuiltinFunctionSpecialized.int_mul(-1, local_007);
                }

                public static PineValue idivHelper(PineValue param_1, PineValue param_2, PineValue param_3)
                {
                    PineValue local_param_1 = param_1;
                    PineValue local_param_2 = param_2;
                    PineValue local_param_3 = param_3;

                    while (true)
                    {
                        PineValue local_000 = BuiltinFunctionSpecialized.int_mul(17, local_param_2);

                        if (BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(local_000, local_param_1))
                        {
                            PineValue local_001 = Test.idivHelper(local_param_1, local_000, CommonReusedValues.Blob_Int_0);

                            return
                                BuiltinFunctionSpecialized.int_add(
                                    BuiltinFunctionSpecialized.int_mul(17, local_001),
                                    Test.idivHelper(
                                        BuiltinFunctionSpecialized.int_add(
                                            local_param_1,
                                            BuiltinFunctionSpecialized.int_mul(local_001, local_param_2, -17)),
                                        local_param_2,
                                        CommonReusedValues.Blob_Int_0));
                        }

                        if (BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(local_param_2, local_param_1))
                        {
                            {
                                PineValue local_param_1_temp =
                                    BuiltinFunctionSpecialized.int_add(
                                        local_param_1,
                                        BuiltinFunctionSpecialized.int_mul(-1, local_param_2));

                                PineValue local_param_3_temp = BuiltinFunctionSpecialized.int_add(1, local_param_3);
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
                    dict[CommonReusedValues.List_5b8b136b] = Dispatch_5b8b136b;
                    dict[CommonReusedValues.List_9e788d05] = Dispatch_9e788d05;
                    return dict;
                }

                public static PineValue? Dispatch_5b8b136b(PineValue environment)
                {
                    if (true)
                    {
                        var arg_1 = PineValueExtension.ValueFromPathOrEmptyList(environment, [1]);
                        var arg_2 = PineValueExtension.ValueFromPathOrEmptyList(environment, [2]);
                        return Test.idiv(arg_1, arg_2);
                    }

                    return null;
                }

                public static PineValue? Dispatch_9e788d05(PineValue environment)
                {
                    if (PineValueExtension.ValueFromPathOrEmptyList(environment, [0, 0]) == CommonReusedValues.List_9e788d05)
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

                public static readonly PineValue Blob_Str_Eval = StringEncoding.ValueFromString("Eval");

                public static readonly PineValue Blob_Str_List = StringEncoding.ValueFromString("List");

                public static readonly PineValue Blob_Str_head = StringEncoding.ValueFromString("head");

                public static readonly PineValue Blob_Str_skip = StringEncoding.ValueFromString("skip");

                public static readonly PineValue Blob_Str_equal = StringEncoding.ValueFromString("equal");

                public static readonly PineValue Blob_Str_Litral = StringEncoding.ValueFromString("Litral");

                public static readonly PineValue Blob_Str_Builtin = StringEncoding.ValueFromString("Builtin");

                public static readonly PineValue Blob_Str_int_add = StringEncoding.ValueFromString("int_add");

                public static readonly PineValue Blob_Str_int_mul = StringEncoding.ValueFromString("int_mul");

                public static readonly PineValue Blob_Str_Conditional = StringEncoding.ValueFromString("Conditional");

                public static readonly PineValue Blob_Str_Environment = StringEncoding.ValueFromString("Environment");

                public static readonly PineValue Blob_Str_int_is_sorted_asc = StringEncoding.ValueFromString("int_is_sorted_asc");

                public static readonly PineValue List_42fd7a4b = PineValue.List([Blob_Str_Litral, PineKernelValues.FalseValue]);

                public static readonly PineValue List_d3f120a8 = PineValue.List([Blob_Str_Litral, PineKernelValues.TrueValue]);

                public static readonly PineValue List_d289000a = PineValue.List([Blob_Str_Litral, Blob_Int_neg_1]);

                public static readonly PineValue List_1e62b855 = PineValue.List([Blob_Str_Litral, Blob_Int_neg_17]);

                public static readonly PineValue List_497388a2 = PineValue.List([Blob_Str_Litral, Blob_Int_0]);

                public static readonly PineValue List_c2d2b628 = PineValue.List([Blob_Str_Litral, Blob_Int_1]);

                public static readonly PineValue List_9a4bf642 = PineValue.List([Blob_Str_Litral, Blob_Int_2]);

                public static readonly PineValue List_245be8ca = PineValue.List([Blob_Str_Litral, Blob_Int_3]);

                public static readonly PineValue List_a5991fb6 = PineValue.List([Blob_Str_Litral, Blob_Int_17]);

                public static readonly PineValue List_Single_Blob_Str_Environment = PineValue.List([Blob_Str_Environment]);

                public static readonly PineValue List_626e9e10 =
                    PineValue.List([Blob_Str_List, List_c2d2b628, List_Single_Blob_Str_Environment]);

                public static readonly PineValue List_e6d95047 =
                    PineValue.List([Blob_Str_List, List_9a4bf642, List_Single_Blob_Str_Environment]);

                public static readonly PineValue List_4fe88b09 =
                    PineValue.List([Blob_Str_List, List_245be8ca, List_Single_Blob_Str_Environment]);

                public static readonly PineValue List_65eb76d7 =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_head, List_Single_Blob_Str_Environment]);

                public static readonly PineValue List_854f7611 = PineValue.List([Blob_Str_Builtin, Blob_Str_head, List_65eb76d7]);

                public static readonly PineValue List_a6554f03 = PineValue.List([Blob_Str_Builtin, Blob_Str_skip, List_626e9e10]);

                public static readonly PineValue List_7a36ad92 = PineValue.List([Blob_Str_Builtin, Blob_Str_skip, List_e6d95047]);

                public static readonly PineValue List_f0f99078 = PineValue.List([Blob_Str_Builtin, Blob_Str_skip, List_4fe88b09]);

                public static readonly PineValue List_e4f998f4 = PineValue.List([Blob_Str_Builtin, Blob_Str_head, List_a6554f03]);

                public static readonly PineValue List_076751a5 = PineValue.List([Blob_Str_Builtin, Blob_Str_head, List_7a36ad92]);

                public static readonly PineValue List_5f6bd89b = PineValue.List([Blob_Str_Builtin, Blob_Str_head, List_f0f99078]);

                public static readonly PineValue List_0fc897d8 = PineValue.List([Blob_Str_List, List_497388a2, List_e4f998f4]);

                public static readonly PineValue List_d2bcb85a = PineValue.List([Blob_Str_List, List_497388a2, List_076751a5]);

                public static readonly PineValue List_13b76347 = PineValue.List([Blob_Str_List, List_e4f998f4, List_d289000a]);

                public static readonly PineValue List_050193ff = PineValue.List([Blob_Str_List, List_076751a5, List_d289000a]);

                public static readonly PineValue List_5c70ae8e = PineValue.List([Blob_Str_List, List_076751a5, List_497388a2]);

                public static readonly PineValue List_615f1213 = PineValue.List([Blob_Str_List, List_076751a5, List_a5991fb6]);

                public static readonly PineValue List_6df7af63 = PineValue.List([Blob_Str_List, List_5f6bd89b, List_c2d2b628]);

                public static readonly PineValue List_bf3678f5 = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_5c70ae8e]);

                public static readonly PineValue List_431c89b1 =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_add, List_6df7af63]);

                public static readonly PineValue List_061ff3bb =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_mul, List_13b76347]);

                public static readonly PineValue List_22df8735 =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_mul, List_050193ff]);

                public static readonly PineValue List_65b736fb =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_mul, List_615f1213]);

                public static readonly PineValue List_2173f079 =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_is_sorted_asc, List_0fc897d8]);

                public static readonly PineValue List_dc9f8853 =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_is_sorted_asc, List_d2bcb85a]);

                public static readonly PineValue List_499507bf = PineValue.List([Blob_Str_List, List_076751a5, List_e4f998f4]);

                public static readonly PineValue List_a65a239f =
                    PineValue.List([Blob_Str_Conditional, List_2173f079, List_d3f120a8, List_42fd7a4b]);

                public static readonly PineValue List_54ce9293 =
                    PineValue.List([Blob_Str_Conditional, List_dc9f8853, List_d3f120a8, List_42fd7a4b]);

                public static readonly PineValue List_7b45e4f2 =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_is_sorted_asc, List_499507bf]);

                public static readonly PineValue List_1f2bd39a = PineValue.List([Blob_Str_List, List_e4f998f4, List_22df8735]);

                public static readonly PineValue List_7b640d31 = PineValue.List([Blob_Str_List, List_65b736fb, List_e4f998f4]);

                public static readonly PineValue List_096cef68 =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_add, List_1f2bd39a]);

                public static readonly PineValue List_fecd03fe =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_is_sorted_asc, List_7b640d31]);

                public static readonly PineValue List_b32f0d51 =
                    PineValue.List([Blob_Str_List, List_65eb76d7, List_e4f998f4, List_65b736fb, List_497388a2]);

                public static readonly PineValue List_453d568a = PineValue.List([Blob_Str_Eval, List_854f7611, List_b32f0d51]);

                public static readonly PineValue List_ce138350 = PineValue.List([Blob_Str_List, List_453d568a, List_a5991fb6]);

                public static readonly PineValue List_14fddd2e =
                    PineValue.List([Blob_Str_Conditional, List_2173f079, List_061ff3bb, List_e4f998f4]);

                public static readonly PineValue List_d7389812 =
                    PineValue.List([Blob_Str_Conditional, List_dc9f8853, List_22df8735, List_076751a5]);

                public static readonly PineValue List_d264dc24 =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_mul, List_ce138350]);

                public static readonly PineValue List_3fb4f21a = PineValue.List([Blob_Str_List, List_a65a239f, List_54ce9293]);

                public static readonly PineValue List_66f0632b = PineValue.List([Blob_Str_Builtin, Blob_Str_equal, List_3fb4f21a]);

                public static readonly PineValue List_8a0a4ab2 =
                    PineValue.List([Blob_Str_List, List_1e62b855, List_453d568a, List_076751a5]);

                public static readonly PineValue List_f746936e =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_mul, List_8a0a4ab2]);

                public static readonly PineValue List_8dc06c21 =
                    PineValue.List([Blob_Str_List, List_65eb76d7, List_096cef68, List_076751a5, List_431c89b1]);

                public static readonly PineValue List_cce74c20 = PineValue.List([Blob_Str_List, List_e4f998f4, List_f746936e]);

                public static readonly PineValue List_10d32677 = PineValue.List([Blob_Str_Eval, List_854f7611, List_8dc06c21]);

                public static readonly PineValue List_22d9039e =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_add, List_cce74c20]);

                public static readonly PineValue List_f44758e9 =
                    PineValue.List([Blob_Str_List, List_65eb76d7, List_22d9039e, List_076751a5, List_497388a2]);

                public static readonly PineValue List_1fb24e21 = PineValue.List([Blob_Str_Eval, List_854f7611, List_f44758e9]);

                public static readonly PineValue List_4fa1664f =
                    PineValue.List([Blob_Str_Conditional, List_7b45e4f2, List_5f6bd89b, List_10d32677]);

                public static readonly PineValue List_73b5fa3c = PineValue.List([Blob_Str_List, List_d264dc24, List_1fb24e21]);

                public static readonly PineValue List_0bacc3b9 =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_add, List_73b5fa3c]);

                public static readonly PineValue List_9e788d05 =
                    PineValue.List([Blob_Str_Conditional, List_fecd03fe, List_4fa1664f, List_0bacc3b9]);

                public static readonly PineValue List_Single_List_9e788d05 = PineValue.List([List_9e788d05]);

                public static readonly PineValue List_39d3da85 = PineValue.List([Blob_Str_Litral, List_9e788d05]);

                public static readonly PineValue List_cdb77299 = PineValue.List([Blob_Str_Litral, List_Single_List_9e788d05]);

                public static readonly PineValue List_ce2e2e53 =
                    PineValue.List([Blob_Str_List, List_cdb77299, List_14fddd2e, List_d7389812, List_497388a2]);

                public static readonly PineValue List_d1ee8bb7 = PineValue.List([Blob_Str_Eval, List_39d3da85, List_ce2e2e53]);

                public static readonly PineValue List_3fb1b91a = PineValue.List([Blob_Str_List, List_d1ee8bb7, List_d289000a]);

                public static readonly PineValue List_ffeca394 =
                    PineValue.List([Blob_Str_Builtin, Blob_Str_int_mul, List_3fb1b91a]);

                public static readonly PineValue List_0dc00e3b =
                    PineValue.List([Blob_Str_Conditional, List_66f0632b, List_ffeca394, List_d1ee8bb7]);

                public static readonly PineValue List_5b8b136b =
                    PineValue.List([Blob_Str_Conditional, List_bf3678f5, List_0dc00e3b, List_497388a2]);
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
