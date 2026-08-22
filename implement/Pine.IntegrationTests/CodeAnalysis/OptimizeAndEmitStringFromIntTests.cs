using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using System.Linq;
using Xunit;

namespace Pine.IntegrationTests.CodeAnalysis;

public class OptimizeAndEmitStringFromIntTests
{
    [Fact]
    public void Parse_and_emit_optimized_String_fromInt()
    {
        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram, functionMetadata) =
            CodeAnalysisTestHelper.StaticProgramFromElmKernelModules(
                ["String.elm"],
                includeDeclaration:
                declName =>
                {
                    return declName.FullName is "String.fromInt";
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram, functionMetadata);

        var asCSharp =
            StaticProgramCSharp.FromStaticProgram(
                staticProgram,
                functionMetadata,
                CodeAnalysisTestHelper.DeclarationSyntaxContext);

        var moduleString = asCSharp.ModulesClasses[DeclQualifiedName.Create([], "String")];

        var moduleStringCSharpText =
            moduleString.RenderToString();

        var moduleGlobalAnonymousText =
            asCSharp.GlobalAnonymousClass.RenderToString();

        moduleStringCSharpText.Trim().Should().Be(
            """"
            public static class String
            {
                public static PineValue fromInt(PineValue param_1)
                {
                    return
                        PineValue.List(
                            [
                            CommonReusedValues.Blob_91938c50,
                            CommonReusedValues.Blob_Str_String,
                            BuiltinFunction.concat(String.fromIntAsList(param_1))
                            ]);
                }

                public static PineValue fromIntAsList(PineValue param_1)
                {
                    if (BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(0, param_1))
                    {
                        return String.fromUnsignedIntAsList(param_1);
                    }

                    return
                        BuiltinFunctionFused.ListPrependItem(
                            itemToPrepend: CommonReusedValues.Blob_Char_hyphen,
                            suffix: String.fromUnsignedIntAsList(BuiltinFunctionSpecialized.int_mul(-1, param_1)));
                }

                public static PineValue fromUnsignedIntAsList(PineValue param_1)
                {
                    return
                        String.fromUnsignedIntAsListHelper(
                            param_1,
                            PineValueExtension.ValueFromPathOrEmptyList(PineValue.EmptyList, [1]),
                            PineValueExtension.ValueFromPathOrEmptyList(PineValue.EmptyList, [2]),
                            PineValueExtension.ValueFromPathOrEmptyList(PineValue.EmptyList, [3]));
                }

                public static PineValue fromUnsignedIntAsListHelper(
                    PineValue param_1,
                    PineValue param_2_1,
                    PineValue param_2_2,
                    PineValue param_2_3)
                {
                    PineValue local_param_1 = param_1;
                    PineValue local_param_2_1 = param_2_1;
                    PineValue local_param_2_2 = param_2_2;
                    PineValue local_param_2_3 = param_2_3;

                    while (true)
                    {
                        if (BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(local_param_1, 0))
                        {
                            if ((PineValue.EmptyList == PineValue.EmptyList
                            ?
                            PineKernelValues.TrueValue
                            :
                            (local_param_2_1 == CommonReusedValues.Blob_Str_Elm_Float
                            ?
                            (local_param_2_3 == CommonReusedValues.Blob_Int_0
                            ?
                            PineKernelValues.FalseValue
                            :
                            (local_param_2_2 == BuiltinFunctionSpecialized.int_mul(local_param_2_3, PineValue.EmptyList)
                            ?
                            PineKernelValues.TrueValue
                            :
                            PineKernelValues.FalseValue))
                            :
                            (BuiltinFunctionSpecialized.take(0, PineValue.EmptyList) == PineValue.EmptyBlob
                            ?
                            PineKernelValues.FalseValue
                            :
                            (BuiltinFunctionSpecialized.length_as_int(PineValue.EmptyList) == 0
                            ?
                            (local_param_2_1 == CommonReusedValues.Blob_Str_String
                            ?
                            PineKernelValues.FalseValue
                            :
                            (local_param_2_1 == CommonReusedValues.Blob_Str_RBNode_elm_builtin
                            ?
                            (Global_Anonymous.zzz_anon_b23c308e_2ff90e16(
                                PineValueExtension.ValueFromPathOrEmptyList(PineValue.EmptyList, [1]),
                                PineValueExtension.ValueFromPathOrEmptyList(PineValue.EmptyList, [3]),
                                PineValueExtension.ValueFromPathOrEmptyList(PineValue.EmptyList, [4]),
                                PineValueExtension.ValueFromPathOrEmptyList(PineValue.EmptyList, [5]),
                                PineValueExtension.ValueFromPathOrEmptyList(PineValue.EmptyList, [6])) ==
                            PineValue.EmptyList
                            ?
                            PineKernelValues.TrueValue
                            :
                            PineKernelValues.FalseValue)
                            :
                            (local_param_2_1 == CommonReusedValues.Blob_Str_Set_elm_builtin
                            ?
                            (Global_Anonymous.zzz_anon_62d6db05_3716fcbd(
                                PineValueExtension.ValueFromPathOrEmptyList(local_param_2_2, [1]),
                                PineValueExtension.ValueFromPathOrEmptyList(local_param_2_2, [3]),
                                PineValueExtension.ValueFromPathOrEmptyList(local_param_2_2, [5]),
                                PineValueExtension.ValueFromPathOrEmptyList(local_param_2_2, [6])) ==
                            PineValue.EmptyList
                            ?
                            PineKernelValues.TrueValue
                            :
                            PineKernelValues.FalseValue)
                            :
                            Global_Anonymous.zzz_anon_a25a170f_7e9f1185(PineValue.EmptyList, PineValue.EmptyList))))
                            :
                            PineKernelValues.FalseValue)))) ==
                                PineKernelValues.TrueValue)
                            {
                                return CommonReusedValues.List_Single_Blob_Char_digit_0;
                            }

                            return PineValue.EmptyList;
                        }

                        PineValue local_001 =
                            BuiltinFunction.ValueFromBool(BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(0, local_param_1));

                        PineValue local_002 =
                            local_001 == PineKernelValues.TrueValue
                            ?
                            local_param_1
                            :
                            BuiltinFunctionSpecialized.int_mul(-1, local_param_1);

                        PineValue local_005 =
                            Global_Anonymous.zzz_anon_60bf67c9_187f7517(
                                local_002,
                                CommonReusedValues.Blob_Int_10,
                                CommonReusedValues.Blob_Int_0);

                        PineValue local_006 =
                            local_001 == PineKernelValues.TrueValue ? local_005 : BuiltinFunctionSpecialized.int_mul(-1, local_005);

                        {
                            PineValue local_param_1_temp = local_006;

                            PineValue local_param_2_1_temp =
                                PineValueExtension.ValueFromPathOrEmptyList(
                                    BuiltinFunctionFused.ListPrependItem(
                                        itemToPrepend: String.unsafeDigitCharacterFromValue(
                                            BuiltinFunctionSpecialized.int_add(
                                                local_param_1,
                                                BuiltinFunctionSpecialized.int_mul(-10, local_006))),
                                        suffix: PineValue.EmptyList),
                                    [
                                    1
                                    ]);

                            PineValue local_param_2_2_temp =
                                PineValueExtension.ValueFromPathOrEmptyList(
                                    BuiltinFunctionFused.ListPrependItem(
                                        itemToPrepend: String.unsafeDigitCharacterFromValue(
                                            BuiltinFunctionSpecialized.int_add(
                                                local_param_1,
                                                BuiltinFunctionSpecialized.int_mul(-10, local_006))),
                                        suffix: PineValue.EmptyList),
                                    [
                                    2
                                    ]);

                            PineValue local_param_2_3_temp =
                                PineValueExtension.ValueFromPathOrEmptyList(
                                    BuiltinFunctionFused.ListPrependItem(
                                        itemToPrepend: String.unsafeDigitCharacterFromValue(
                                            BuiltinFunctionSpecialized.int_add(
                                                local_param_1,
                                                BuiltinFunctionSpecialized.int_mul(-10, local_006))),
                                        suffix: PineValue.EmptyList),
                                    [
                                    3
                                    ]);

                            local_param_1 = local_param_1_temp;
                            local_param_2_1 = local_param_2_1_temp;
                            local_param_2_2 = local_param_2_2_temp;
                            local_param_2_3 = local_param_2_3_temp;
                        }

                        continue;
                    }
                }

                public static PineValue unsafeDigitCharacterFromValue(PineValue param_1)
                {
                    PineValue local_param_1 = param_1;

                    while (true)
                    {
                        if (local_param_1 == CommonReusedValues.Blob_Int_0)
                        {
                            return CommonReusedValues.Blob_Char_digit_0;
                        }

                        if (local_param_1 == CommonReusedValues.Blob_Int_1)
                        {
                            return CommonReusedValues.Blob_Char_digit_1;
                        }

                        if (local_param_1 == CommonReusedValues.Blob_Int_2)
                        {
                            return CommonReusedValues.Blob_Char_digit_2;
                        }

                        if (local_param_1 == CommonReusedValues.Blob_Int_3)
                        {
                            return CommonReusedValues.Blob_Char_digit_3;
                        }

                        if (local_param_1 == CommonReusedValues.Blob_Int_4)
                        {
                            return CommonReusedValues.Blob_Char_digit_4;
                        }

                        if (local_param_1 == CommonReusedValues.Blob_Int_5)
                        {
                            return CommonReusedValues.Blob_Char_digit_5;
                        }

                        if (local_param_1 == CommonReusedValues.Blob_Int_6)
                        {
                            return CommonReusedValues.Blob_Char_digit_6;
                        }

                        if (local_param_1 == CommonReusedValues.Blob_Int_7)
                        {
                            return CommonReusedValues.Blob_Char_digit_7;
                        }

                        if (local_param_1 == CommonReusedValues.Blob_Int_8)
                        {
                            return CommonReusedValues.Blob_Char_digit_8;
                        }

                        if (local_param_1 == CommonReusedValues.Blob_Int_9)
                        {
                            return CommonReusedValues.Blob_Char_digit_9;
                        }

                        {
                        }

                        continue;
                    }
                }
            }
            """".Trim());

        moduleGlobalAnonymousText.Trim().Should().Be(
            """"
            public static class Global_Anonymous
            {
                public static PineValue zzz_anon_60bf67c9_187f7517(PineValue param_1_0, PineValue param_1_1, PineValue param_1_2)
                {
                    PineValue local_param_1_0 = param_1_0;
                    PineValue local_param_1_1 = param_1_1;
                    PineValue local_param_1_2 = param_1_2;

                    while (true)
                    {
                        PineValue local_000 = BuiltinFunctionSpecialized.int_mul(16, local_param_1_1);

                        if (BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(local_000, local_param_1_0))
                        {
                            PineValue local_001 =
                                Global_Anonymous.zzz_anon_60bf67c9_187f7517(
                                    local_param_1_0,
                                    local_000,
                                    CommonReusedValues.Blob_Int_0);

                            return
                                BuiltinFunctionSpecialized.int_add(
                                    BuiltinFunctionSpecialized.int_mul(16, local_001),
                                    Global_Anonymous.zzz_anon_60bf67c9_187f7517(
                                        BuiltinFunctionSpecialized.int_add(
                                            local_param_1_0,
                                            BuiltinFunctionSpecialized.int_mul(
                                                local_001,
                                                BuiltinFunctionSpecialized.int_mul(-1, local_000))),
                                        local_param_1_1,
                                        CommonReusedValues.Blob_Int_0));
                        }

                        if (BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(local_param_1_1, local_param_1_0))
                        {
                            {
                                PineValue local_param_1_0_temp =
                                    BuiltinFunctionSpecialized.int_add(
                                        local_param_1_0,
                                        BuiltinFunctionSpecialized.int_mul(-1, local_param_1_1));

                                PineValue local_param_1_2_temp = BuiltinFunctionSpecialized.int_add(1, local_param_1_2);
                                local_param_1_0 = local_param_1_0_temp;
                                local_param_1_2 = local_param_1_2_temp;
                            }

                            continue;
                        }

                        return local_param_1_2;
                    }
                }

                public static PineValue zzz_anon_62d6db05_3716fcbd(
                    PineValue param_1_1,
                    PineValue param_1_3,
                    PineValue param_1_5,
                    PineValue param_1_6)
                {
                    if (param_1_1 == CommonReusedValues.Blob_Str_RBNode_elm_builtin)
                    {
                        return
                            BuiltinFunction.concat(
                                PineValue.List(
                                    [
                                    Global_Anonymous.zzz_anon_62d6db05_3716fcbd(
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_5, [1]),
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_5, [3]),
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_5, [5]),
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_5, [6])),
                                    PineValue.List([param_1_3]),
                                    Global_Anonymous.zzz_anon_62d6db05_3716fcbd(
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_6, [1]),
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_6, [3]),
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_6, [5]),
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_6, [6]))
                                    ]));
                    }

                    return PineValue.List([]);
                }

                public static PineValue zzz_anon_9752e34c_014dd9eb(PineValue param_1_0, PineValue param_1_1)
                {
                    if (param_1_0 == param_1_1)
                    {
                        return PineKernelValues.TrueValue;
                    }

                    PineValue local_001 = PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [1]);
                    PineValue local_004 = PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [1]);
                    PineValue local_006 = BuiltinFunction.ValueFromBool(local_004 == CommonReusedValues.Blob_Str_Elm_Float);

                    if (local_001 == CommonReusedValues.Blob_Str_Elm_Float)
                    {
                        PineValue local_007 = PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [3]);

                        if (local_006 == PineKernelValues.TrueValue)
                        {
                            if (BuiltinFunctionSpecialized.int_mul(
                                PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [2]),
                                PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [3])) ==
                                BuiltinFunctionSpecialized.int_mul(
                                    PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [2]),
                                    local_007))
                            {
                                return PineKernelValues.TrueValue;
                            }

                            return PineKernelValues.FalseValue;
                        }

                        if (local_007 == CommonReusedValues.Blob_Int_0)
                        {
                            return PineKernelValues.FalseValue;
                        }

                        if (PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [2]) ==
                            BuiltinFunctionSpecialized.int_mul(local_007, param_1_1))
                        {
                            return PineKernelValues.TrueValue;
                        }

                        return PineKernelValues.FalseValue;
                    }

                    if (local_006 == PineKernelValues.TrueValue)
                    {
                        PineValue local_008 = PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [3]);

                        if (local_008 == CommonReusedValues.Blob_Int_0)
                        {
                            return PineKernelValues.FalseValue;
                        }

                        if (BuiltinFunctionSpecialized.int_mul(param_1_0, local_008) ==
                            PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [2]))
                        {
                            return PineKernelValues.TrueValue;
                        }

                        return PineKernelValues.FalseValue;
                    }

                    if (BuiltinFunctionSpecialized.take(0, param_1_0) ==
                        BuiltinFunctionSpecialized.take(0, IntegerEncoding.EncodeSignedInteger(0)))
                    {
                        return PineKernelValues.FalseValue;
                    }

                    if (BuiltinFunctionSpecialized.length_as_int(param_1_0) == BuiltinFunctionSpecialized.length_as_int(param_1_1))
                    {
                        if (local_001 == CommonReusedValues.Blob_Str_String)
                        {
                            return PineKernelValues.FalseValue;
                        }

                        if (local_001 == CommonReusedValues.Blob_Str_RBNode_elm_builtin)
                        {
                            if (Global_Anonymous.zzz_anon_b23c308e_2ff90e16(
                                PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [1]),
                                PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [3]),
                                PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [4]),
                                PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [5]),
                                PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [6])) ==
                                Global_Anonymous.zzz_anon_b23c308e_2ff90e16(
                                    PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [1]),
                                    PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [3]),
                                    PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [4]),
                                    PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [5]),
                                    PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [6])))
                            {
                                return PineKernelValues.TrueValue;
                            }

                            return PineKernelValues.FalseValue;
                        }

                        if (local_001 == CommonReusedValues.Blob_Str_Set_elm_builtin)
                        {
                            if (Global_Anonymous.zzz_anon_62d6db05_3716fcbd(
                                PineValueExtension.ValueFromPathOrEmptyList(
                                    PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [2]),
                                    [
                                    1
                                    ]),
                                PineValueExtension.ValueFromPathOrEmptyList(
                                    PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [2]),
                                    [
                                    3
                                    ]),
                                PineValueExtension.ValueFromPathOrEmptyList(
                                    PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [2]),
                                    [
                                    5
                                    ]),
                                PineValueExtension.ValueFromPathOrEmptyList(
                                    PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [2]),
                                    [
                                    6
                                    ])) ==
                                Global_Anonymous.zzz_anon_62d6db05_3716fcbd(
                                    PineValueExtension.ValueFromPathOrEmptyList(
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [2]),
                                        [
                                        1
                                        ]),
                                    PineValueExtension.ValueFromPathOrEmptyList(
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [2]),
                                        [
                                        3
                                        ]),
                                    PineValueExtension.ValueFromPathOrEmptyList(
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [2]),
                                        [
                                        5
                                        ]),
                                    PineValueExtension.ValueFromPathOrEmptyList(
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [2]),
                                        [
                                        6
                                        ])))
                            {
                                return PineKernelValues.TrueValue;
                            }

                            return PineKernelValues.FalseValue;
                        }

                        return Global_Anonymous.zzz_anon_a25a170f_7e9f1185(param_1_0, param_1_1);
                    }

                    return PineKernelValues.FalseValue;
                }

                public static PineValue zzz_anon_a25a170f_7e9f1185(PineValue param_1_0, PineValue param_1_1)
                {
                    ImmutableSliceBuilder local_param_1_0 = ImmutableSliceBuilder.Create(param_1_0);
                    ImmutableSliceBuilder local_param_1_1 = ImmutableSliceBuilder.Create(param_1_1);

                    while (true)
                    {
                        if (local_param_1_0.Evaluate() == PineValue.List([]))
                        {
                            return PineKernelValues.TrueValue;
                        }

                        if (Global_Anonymous.zzz_anon_9752e34c_014dd9eb(local_param_1_0.GetHead(), local_param_1_1.GetHead()) ==
                            PineKernelValues.TrueValue)
                        {
                            {
                                local_param_1_0 = local_param_1_0.Skip(1);
                                local_param_1_1 = local_param_1_1.Skip(1);
                            }

                            continue;
                        }

                        return PineKernelValues.FalseValue;
                    }
                }

                public static PineValue zzz_anon_b23c308e_2ff90e16(
                    PineValue param_1_1,
                    PineValue param_1_3,
                    PineValue param_1_4,
                    PineValue param_1_5,
                    PineValue param_1_6)
                {
                    if (param_1_1 == CommonReusedValues.Blob_Str_RBNode_elm_builtin)
                    {
                        return
                            BuiltinFunction.concat(
                                PineValue.List(
                                    [
                                    Global_Anonymous.zzz_anon_b23c308e_2ff90e16(
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_5, [1]),
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_5, [3]),
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_5, [4]),
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_5, [5]),
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_5, [6])),
                                    PineValue.List([PineValue.List([param_1_3, param_1_4])]),
                                    Global_Anonymous.zzz_anon_b23c308e_2ff90e16(
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_6, [1]),
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_6, [3]),
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_6, [4]),
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_6, [5]),
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_6, [6]))
                                    ]));
                    }

                    return PineValue.List([]);
                }
            }
            """".Trim());


        // Now compile this to a .NET assembly.

        var compileToAssemblyResult =
            CompileToAssembly.Compile(
                asCSharp,
                namespacePrefix: [],
                optimizationLevel: Microsoft.CodeAnalysis.OptimizationLevel.Debug)
            .Extract(
                err =>
                throw new System.Exception("Compilation to assembly failed: " + err.ToString()));

        var stringModule =
            parsedEnv.Modules.Single(m => m.moduleName is "String");

        var fromIntDeclValue =
            stringModule.moduleContent.FunctionDeclarations["fromInt"];

        var fromIntFunctionRecord =
            FunctionRecord.ParseFunctionRecordTagged(fromIntDeclValue, parseCache)
            .Extract(
                err => throw new System.Exception(
                    "Parsing function record for 'fromInt' failed: " + err.ToString()));

        fromIntFunctionRecord.EnvFunctions.Length.Should().Be(0);
        fromIntFunctionRecord.ParameterCount.Should().Be(1);
    }
}
