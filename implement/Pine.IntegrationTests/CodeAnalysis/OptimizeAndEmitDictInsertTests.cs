using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Xunit;

namespace Pine.IntegrationTests.CodeAnalysis;

public class OptimizeAndEmitDictInsertTests
{
    [Fact]
    public void Parse_and_emit_optimized_Dict_insert()
    {
        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            CodeAnalysisTestHelper.StaticProgramFromElmModules(
                [],
                includeDeclaration:
                declName =>
                {
                    return declName == new DeclQualifiedName(["Dict"], "insert");
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        var asCSharp =
            StaticProgramCSharp.FromStaticProgram(
                staticProgram,
                CodeAnalysisTestHelper.DeclarationSyntaxContext);

        var moduleBasics = asCSharp.ModulesClasses[new DeclQualifiedName([], "Basics")];

        var moduleDict = asCSharp.ModulesClasses[new DeclQualifiedName([], "Dict")];

        var moduleBasicsCSharpText =
            moduleBasics.RenderToString();

        var moduleDictCSharpText =
            moduleDict.RenderToString();

        var moduleGlobalAnonymousText =
            asCSharp.GlobalAnonymousClass.RenderToString();

        moduleDictCSharpText.Trim().Should().Be(
            """"
            public static class Dict
            {
                public static PineValue insert(
                    PineValue param_1_0,
                    PineValue param_1_1,
                    PineValue param_1_2)
                {
                    PineValue local_000 =
                        Global_Anonymous.zzz_anon_ea679199_b65beb0f(param_1_0, param_1_1, param_1_2);

                    PineValue local_001 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_000,
                            [1]);

                    if ((CommonReusedValues.Blob_Str_Red == PineValueExtension.ValueFromPathOrEmptyList(
                        local_001,
                        [0, 0])) && (CommonReusedValues.Blob_Str_RBNode_elm_builtin == PineValueExtension.ValueFromPathOrEmptyList(
                        local_000,
                        [0])))
                    {
                        return
                            PineValue.List(
                                [
                                    CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                    PineValue.List(
                                        [
                                            CommonReusedValues.List_7222f8d4,
                                            PineValueExtension.ValueFromPathOrEmptyList(
                                                local_001,
                                                [1]),
                                            PineValueExtension.ValueFromPathOrEmptyList(
                                                local_001,
                                                [2]),
                                            PineValueExtension.ValueFromPathOrEmptyList(
                                                local_001,
                                                [3]),
                                            PineValueExtension.ValueFromPathOrEmptyList(
                                                local_001,
                                                [4])
                                        ])
                                ]);
                    }

                    return local_000;
                }
            }

            """".Trim());

        moduleBasicsCSharpText.Trim().Should().Be(
            """"
            public static class Basics
            {
                public static PineValue compare(
                    PineValue param_1_0,
                    PineValue param_1_1)
                {
                    PineValue local_000 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            param_1_0,
                            [1]);
            
                    PineValue local_001 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_000,
                            [0]);
            
                    PineValue local_002 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            param_1_1,
                            [1]);
            
                    PineValue local_003 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_002,
                            [0]);
            
                    PineValue local_004 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_000,
                            [1]);
            
                    PineValue local_006 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_002,
                            [1]);
            
                    if (param_1_0 == param_1_1)
                    {
                        return CommonReusedValues.List_ac855cb8;
                    }
            
                    PineValue local_007 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            param_1_1,
                            [0]);
            
                    if ((CommonReusedValues.Blob_Str_String == local_007) && (CommonReusedValues.Blob_Str_String == PineValueExtension.ValueFromPathOrEmptyList(
                        param_1_0,
                        [0])))
                    {
                        return Basics.compareStrings(CommonReusedValues.Blob_Int_0, local_001, local_003);
                    }
            
                    PineValue local_008 =
                        KernelFunction.ValueFromBool(
                            CommonReusedValues.Blob_Str_Elm_Float == local_007);
            
                    if ((local_008 == PineKernelValues.TrueValue) && (CommonReusedValues.Blob_Str_Elm_Float == PineValueExtension.ValueFromPathOrEmptyList(
                        param_1_0,
                        [0])))
                    {
                        if (KernelFunctionSpecialized.int_mul(local_001, local_006) == KernelFunctionSpecialized.int_mul(local_003, local_004))
                        {
                            return CommonReusedValues.List_ac855cb8;
                        }
            
                        if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(
                            KernelFunctionSpecialized.int_mul(local_001, local_006),
                            KernelFunctionSpecialized.int_mul(local_003, local_004)))
                        {
                            return CommonReusedValues.List_af0e3cad;
                        }
            
                        return CommonReusedValues.List_50724673;
                    }
            
                    if (CommonReusedValues.Blob_Str_Elm_Float == PineValueExtension.ValueFromPathOrEmptyList(
                        param_1_0,
                        [0]))
                    {
                        if (local_001 == KernelFunctionSpecialized.int_mul(local_004, param_1_1))
                        {
                            return CommonReusedValues.List_ac855cb8;
                        }
            
                        if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(
                            local_001,
                            KernelFunctionSpecialized.int_mul(local_004, param_1_1)))
                        {
                            return CommonReusedValues.List_af0e3cad;
                        }
            
                        return CommonReusedValues.List_50724673;
                    }
            
                    if (local_008 == PineKernelValues.TrueValue)
                    {
                        if (KernelFunctionSpecialized.int_mul(param_1_0, local_006) == local_003)
                        {
                            return CommonReusedValues.List_ac855cb8;
                        }
            
                        if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(
                            KernelFunctionSpecialized.int_mul(param_1_0, local_006),
                            local_003))
                        {
                            return CommonReusedValues.List_af0e3cad;
                        }
            
                        return CommonReusedValues.List_50724673;
                    }
            
                    if (Global_Anonymous.zzz_anon_c78b4c00_dda26649(param_1_0) == PineKernelValues.TrueValue)
                    {
                        return Basics.compareList(param_1_0, param_1_1);
                    }
            
                    if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(param_1_0, param_1_1))
                    {
                        return CommonReusedValues.List_af0e3cad;
                    }
            
                    return CommonReusedValues.List_50724673;
                }
            
            
                public static PineValue compareList(
                    PineValue param_1_0,
                    PineValue param_1_1)
                {
                    ImmutableSliceBuilder local_param_1_0 =
                        ImmutableSliceBuilder.Create(param_1_0);
            
                    ImmutableSliceBuilder local_param_1_1 =
                        ImmutableSliceBuilder.Create(param_1_1);
            
                    while (true)
                    {
                        PineValue local_001 =
                            Basics.compare(
                                local_param_1_0.GetHead(),
                                local_param_1_1.GetHead());
            
                        if (local_param_1_0.IsEmptyList())
                        {
                            if (local_param_1_1.IsEmptyList())
                            {
                                return CommonReusedValues.List_ac855cb8;
                            }
            
                            return CommonReusedValues.List_af0e3cad;
                        }
            
                        if (!(local_param_1_0.GetLength() == 0))
                        {
                            if (local_param_1_1.IsEmptyList())
                            {
                                return CommonReusedValues.List_50724673;
                            }
            
                            if (!(local_param_1_1.GetLength() == 0))
                            {
                                if (local_001 == CommonReusedValues.List_ac855cb8)
                                {
                                    {
                                        local_param_1_0 =
                                            local_param_1_0.Skip(1);
            
                                        local_param_1_1 =
                                            local_param_1_1.Skip(1);
                                    }
            
                                    continue;
                                }
            
                                return local_001;
                            }
            
                            throw new ParseExpressionException("TODO: Include details from encoded and env subexpressions");
                        }
            
                        throw new ParseExpressionException("TODO: Include details from encoded and env subexpressions");
                    }
                }
            
            
                public static PineValue compareStrings(
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
                            KernelFunctionFused.SkipAndTake(takeCount: 4, skipCountValue: local_param_1_0, argument: local_param_1_1);
            
                        PineValue local_001 =
                            KernelFunctionFused.SkipAndTake(takeCount: 4, skipCountValue: local_param_1_0, argument: local_param_1_2);
            
                        PineValue local_002 =
                            KernelFunction.ValueFromBool(
                                KernelFunctionSpecialized.length_as_int(local_001) == 0);
            
                        if (KernelFunctionSpecialized.length_as_int(local_000) == 0)
                        {
                            if (local_002 == PineKernelValues.TrueValue)
                            {
                                return CommonReusedValues.List_ac855cb8;
                            }
            
                            return CommonReusedValues.List_af0e3cad;
                        }
            
                        if (local_002 == PineKernelValues.TrueValue)
                        {
                            return CommonReusedValues.List_50724673;
                        }
            
                        if (local_000 == local_001)
                        {
                            {
                                PineValue local_param_1_0_temp =
                                    KernelFunctionSpecialized.int_add(4, local_param_1_0);
            
                                local_param_1_0 =
                                    local_param_1_0_temp;
                            }
            
                            continue;
                        }
            
                        if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(
                            KernelFunctionSpecialized.concat(
                                IntegerEncoding.EncodeSignedInteger(0),
                                local_000),
                            KernelFunctionSpecialized.concat(
                                IntegerEncoding.EncodeSignedInteger(0),
                                local_001)))
                        {
                            return CommonReusedValues.List_af0e3cad;
                        }
            
                        return CommonReusedValues.List_50724673;
                    }
                }
            }
            """".Trim());

        moduleGlobalAnonymousText.Trim().Should().Be(
            """"
            public static class Global_Anonymous
            {
                public static PineValue zzz_anon_c78b4c00_dda26649(PineValue param_1_0)
                {
                    return
                        KernelFunctionSpecialized.equal(
                            KernelFunctionSpecialized.take(0, param_1_0),
                            PineValue.EmptyList);
                }
            
            
                public static PineValue zzz_anon_e6d15ff4_dda26649(
                    PineValue param_1_0,
                    PineValue param_1_1,
                    PineValue param_1_2,
                    PineValue param_1_3,
                    PineValue param_1_4)
                {
                    PineValue local_000 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            param_1_4,
                            [1]);
            
                    PineValue local_001 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            param_1_3,
                            [1]);
            
                    PineValue local_002 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_001,
                            [1]);
            
                    PineValue local_003 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_001,
                            [2]);
            
                    PineValue local_004 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_000,
                            [1]);
            
                    PineValue local_005 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_000,
                            [2]);
            
                    PineValue local_006 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_000,
                            [4]);
            
                    PineValue local_007 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_000,
                            [3]);
            
                    PineValue local_008 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_001,
                            [4]);
            
                    PineValue local_009 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_001,
                            [3]);
            
                    if ((CommonReusedValues.Blob_Str_Red == PineValueExtension.ValueFromPathOrEmptyList(
                        local_000,
                        [0, 0])) && (CommonReusedValues.Blob_Str_RBNode_elm_builtin == PineValueExtension.ValueFromPathOrEmptyList(
                        param_1_4,
                        [0])))
                    {
                        if ((CommonReusedValues.Blob_Str_Red == PineValueExtension.ValueFromPathOrEmptyList(
                            local_001,
                            [0, 0])) && (CommonReusedValues.Blob_Str_RBNode_elm_builtin == PineValueExtension.ValueFromPathOrEmptyList(
                            param_1_3,
                            [0])))
                        {
                            return
                                PineValue.List(
                                    [
                                        CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                        PineValue.List(
                                            [
                                                CommonReusedValues.List_dafb9d35,
                                                param_1_1,
                                                param_1_2,
                                                PineValue.List(
                                                    [
                                                        CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                                        PineValue.List(
                                                            [CommonReusedValues.List_7222f8d4, local_002, local_003, local_009, local_008])
                                                    ]),
                                                PineValue.List(
                                                    [
                                                        CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                                        PineValue.List(
                                                            [CommonReusedValues.List_7222f8d4, local_004, local_005, local_007, local_006])
                                                    ])
                                            ])
                                    ]);
                        }
            
                        return
                            PineValue.List(
                                [
                                    CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                    PineValue.List(
                                        [
                                            param_1_0,
                                            local_004,
                                            local_005,
                                            PineValue.List(
                                                [
                                                    CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                                    PineValue.List(
                                                        [CommonReusedValues.List_dafb9d35, param_1_1, param_1_2, param_1_3, local_007])
                                                ]),
                                            local_006
                                        ])
                                ]);
                    }
            
                    PineValue local_011 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_009,
                            [1]);
            
                    if ((((CommonReusedValues.Blob_Str_Red == PineValueExtension.ValueFromPathOrEmptyList(
                        local_011,
                        [0, 0])) && (CommonReusedValues.Blob_Str_RBNode_elm_builtin == PineValueExtension.ValueFromPathOrEmptyList(
                        local_009,
                        [0]))) && (CommonReusedValues.Blob_Str_Red == PineValueExtension.ValueFromPathOrEmptyList(
                        local_001,
                        [0, 0]))) && (CommonReusedValues.Blob_Str_RBNode_elm_builtin == PineValueExtension.ValueFromPathOrEmptyList(
                        param_1_3,
                        [0])))
                    {
                        return
                            PineValue.List(
                                [
                                    CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                    PineValue.List(
                                        [
                                            CommonReusedValues.List_dafb9d35,
                                            local_002,
                                            local_003,
                                            PineValue.List(
                                                [
                                                    CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                                    PineValue.List(
                                                        [
                                                            CommonReusedValues.List_7222f8d4,
                                                            PineValueExtension.ValueFromPathOrEmptyList(
                                                                local_011,
                                                                [1]),
                                                            PineValueExtension.ValueFromPathOrEmptyList(
                                                                local_011,
                                                                [2]),
                                                            PineValueExtension.ValueFromPathOrEmptyList(
                                                                local_011,
                                                                [3]),
                                                            PineValueExtension.ValueFromPathOrEmptyList(
                                                                local_011,
                                                                [4])
                                                        ])
                                                ]),
                                            PineValue.List(
                                                [
                                                    CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                                    PineValue.List(
                                                        [CommonReusedValues.List_7222f8d4, param_1_1, param_1_2, local_008, param_1_4])
                                                ])
                                        ])
                                ]);
                    }
            
                    return
                        PineValue.List(
                            [
                                CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                PineValue.List(
                                    [param_1_0, param_1_1, param_1_2, param_1_3, param_1_4])
                            ]);
                }
            
            
                public static PineValue zzz_anon_ea679199_b65beb0f(
                    PineValue param_1_0,
                    PineValue param_1_1,
                    PineValue param_1_2)
                {
                    PineValue local_000 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            param_1_2,
                            [0]);
            
                    PineValue local_001 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            param_1_2,
                            [1]);
            
                    PineValue local_002 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_001,
                            [0]);
            
                    PineValue local_003 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_001,
                            [1]);
            
                    PineValue local_004 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_001,
                            [4]);
            
                    PineValue local_005 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_001,
                            [2]);
            
                    PineValue local_006 =
                        PineValueExtension.ValueFromPathOrEmptyList(
                            local_001,
                            [3]);
            
                    if (CommonReusedValues.Blob_Str_RBEmpty_elm_builtin == local_000)
                    {
                        return
                            PineValue.List(
                                [
                                    CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                    PineValue.List(
                                        [CommonReusedValues.List_dafb9d35, param_1_0, param_1_1, CommonReusedValues.List_71a3df23, CommonReusedValues.List_71a3df23])
                                ]);
                    }
            
                    if (CommonReusedValues.Blob_Str_RBNode_elm_builtin == local_000)
                    {
                        PineValue local_007 =
                            Basics.compare(param_1_0, local_003);
            
                        PineValue local_008 =
                            PineValueExtension.ValueFromPathOrEmptyList(
                                local_007,
                                [0]);
            
                        if (CommonReusedValues.Blob_Str_LT == local_008)
                        {
                            return
                                Global_Anonymous.zzz_anon_e6d15ff4_dda26649(
                                    local_002,
                                    local_003,
                                    local_005,
                                    Global_Anonymous.zzz_anon_ea679199_b65beb0f(param_1_0, param_1_1, local_006),
                                    local_004);
                        }
            
                        if (CommonReusedValues.Blob_Str_EQ == local_008)
                        {
                            return
                                PineValue.List(
                                    [
                                        CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                        PineValue.List(
                                            [local_002, local_003, param_1_1, local_006, local_004])
                                    ]);
                        }
            
                        if (CommonReusedValues.Blob_Str_GT == local_008)
                        {
                            return
                                Global_Anonymous.zzz_anon_e6d15ff4_dda26649(
                                    local_002,
                                    local_003,
                                    local_005,
                                    local_006,
                                    Global_Anonymous.zzz_anon_ea679199_b65beb0f(param_1_0, param_1_1, local_004));
                        }
            
                        throw new ParseExpressionException("TODO: Include details from encoded and env subexpressions");
                    }
            
                    throw new ParseExpressionException("TODO: Include details from encoded and env subexpressions");
                }
            }

            """"
            .Trim());

        var compileToAssemblyResult =
            CompileToAssembly.Compile(
                asCSharp,
                namespacePrefix: [],
                optimizationLevel: Microsoft.CodeAnalysis.OptimizationLevel.Release)
            .Extract(err =>
            throw new System.Exception("Compilation to assembly failed: " + err.ToString()));


    }
}
