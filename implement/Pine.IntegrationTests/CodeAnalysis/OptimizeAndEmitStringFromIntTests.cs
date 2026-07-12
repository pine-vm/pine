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
                    return declName.FullName == "String.fromInt";
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
                            CommonReusedValues.Blob_Str_String,
                            PineValue.List([BuiltinFunction.concat(String.fromIntAsList(param_1))])
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
                            PineValueExtension.ValueFromPathOrEmptyList(PineValue.EmptyList, [0]),
                            PineValueExtension.ValueFromPathOrEmptyList(PineValue.EmptyList, [1]));
                }

                public static PineValue fromUnsignedIntAsListHelper(PineValue param_1, PineValue param_2_0, PineValue param_2_1)
                {
                    PineValue local_param_1 = param_1;
                    PineValue local_param_2_0 = param_2_0;
                    PineValue local_param_2_1 = param_2_1;

                    while (true)
                    {
                        if (BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(local_param_1, 0))
                        {
                            if ((PineValue.EmptyList == PineValue.EmptyList
                            ?
                            PineKernelValues.TrueValue
                            :
                            (local_param_2_0 == CommonReusedValues.Blob_Str_Elm_Float
                            ?
                            (PineValueExtension.ValueFromPathOrEmptyList(local_param_2_1, [0]) == PineValue.EmptyList
                            ?
                            (PineValueExtension.ValueFromPathOrEmptyList(local_param_2_1, [1]) == CommonReusedValues.Blob_Int_1
                            ?
                            PineKernelValues.TrueValue
                            :
                            PineKernelValues.FalseValue)
                            :
                            PineKernelValues.FalseValue)
                            :
                            (BuiltinFunctionSpecialized.take(0, PineValue.EmptyList) == PineValue.EmptyBlob
                            ?
                            PineKernelValues.FalseValue
                            :
                            (BuiltinFunctionSpecialized.length_as_int(PineValue.EmptyList) == 0
                            ?
                            (local_param_2_0 == CommonReusedValues.Blob_Str_String
                            ?
                            PineKernelValues.FalseValue
                            :
                            (local_param_2_0 == CommonReusedValues.Blob_Str_RBNode_elm_builtin
                            ?
                            (Global_Anonymous.zzz_anon_0460788b_b36022e5(
                                PineValueExtension.ValueFromPathOrEmptyList(PineValue.EmptyList, [0]),
                                PineValueExtension.ValueFromPathOrEmptyList(PineValue.EmptyList, [1])) ==
                            PineValue.EmptyList
                            ?
                            PineKernelValues.TrueValue
                            :
                            PineKernelValues.FalseValue)
                            :
                            (local_param_2_0 == CommonReusedValues.Blob_Str_Set_elm_builtin
                            ?
                            (Global_Anonymous.zzz_anon_74ecdc38_c78bed40(
                                PineValueExtension.ValueFromPathOrEmptyList(
                                    PineValueExtension.ValueFromPathOrEmptyList(local_param_2_1, [0]),
                                    [
                                    0
                                    ]),
                                PineValueExtension.ValueFromPathOrEmptyList(
                                    PineValueExtension.ValueFromPathOrEmptyList(local_param_2_1, [0]),
                                    [
                                    1
                                    ])) ==
                            PineValue.EmptyList
                            ?
                            PineKernelValues.TrueValue
                            :
                            PineKernelValues.FalseValue)
                            :
                            Global_Anonymous.zzz_anon_42a24045_77105bd2(PineValue.EmptyList, PineValue.EmptyList))))
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
                            Global_Anonymous.zzz_anon_38e6937c_a50db278(
                                local_002,
                                CommonReusedValues.Blob_Int_10,
                                CommonReusedValues.Blob_Int_0);

                        PineValue local_006 =
                            local_001 == PineKernelValues.TrueValue ? local_005 : BuiltinFunctionSpecialized.int_mul(-1, local_005);

                        {
                            PineValue local_param_1_temp = local_006;

                            PineValue local_param_2_0_temp =
                                PineValueExtension.ValueFromPathOrEmptyList(
                                    BuiltinFunctionFused.ListPrependItem(
                                        itemToPrepend: String.unsafeDigitCharacterFromValue(
                                            BuiltinFunctionSpecialized.int_add(
                                                local_param_1,
                                                BuiltinFunctionSpecialized.int_mul(-10, local_006))),
                                        suffix: PineValue.EmptyList),
                                    [
                                    0
                                    ]);

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

                            local_param_1 = local_param_1_temp;
                            local_param_2_0 = local_param_2_0_temp;
                            local_param_2_1 = local_param_2_1_temp;
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
                public static PineValue zzz_anon_0460788b_b36022e5(PineValue param_1_0, PineValue param_1_1)
                {
                    if (param_1_0 == CommonReusedValues.Blob_Str_RBNode_elm_builtin)
                    {
                        return
                            BuiltinFunction.concat(
                                PineValue.List(
                                    [
                                    Global_Anonymous.zzz_anon_0460788b_b36022e5(
                                        PineValueExtension.ValueFromPathOrEmptyList(
                                            PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [3]),
                                            [
                                            0
                                            ]),
                                        PineValueExtension.ValueFromPathOrEmptyList(
                                            PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [3]),
                                            [
                                            1
                                            ])),
                                    PineValue.List(
                                        [
                                        PineValue.List(
                                            [
                                            PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [1]),
                                            PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [2])
                                            ])
                                        ]),
                                    Global_Anonymous.zzz_anon_0460788b_b36022e5(
                                        PineValueExtension.ValueFromPathOrEmptyList(
                                            PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [4]),
                                            [
                                            0
                                            ]),
                                        PineValueExtension.ValueFromPathOrEmptyList(
                                            PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [4]),
                                            [
                                            1
                                            ]))
                                    ]));
                    }

                    return PineValue.List([]);
                }

                public static PineValue zzz_anon_38e6937c_a50db278(PineValue param_1_0, PineValue param_1_1, PineValue param_1_2)
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
                                Global_Anonymous.zzz_anon_38e6937c_a50db278(
                                    local_param_1_0,
                                    local_000,
                                    CommonReusedValues.Blob_Int_0);

                            return
                                BuiltinFunctionSpecialized.int_add(
                                    BuiltinFunctionSpecialized.int_mul(16, local_001),
                                    Global_Anonymous.zzz_anon_38e6937c_a50db278(
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

                public static PineValue zzz_anon_42a24045_77105bd2(PineValue param_1_0, PineValue param_1_1)
                {
                    ImmutableSliceBuilder local_param_1_0 = ImmutableSliceBuilder.Create(param_1_0);
                    ImmutableSliceBuilder local_param_1_1 = ImmutableSliceBuilder.Create(param_1_1);

                    while (true)
                    {
                        if (local_param_1_0.Evaluate() == PineValue.List([]))
                        {
                            return PineKernelValues.TrueValue;
                        }

                        if (Global_Anonymous.zzz_anon_821c863b_0f659626(local_param_1_0.GetHead(), local_param_1_1.GetHead()) ==
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

                public static PineValue zzz_anon_74ecdc38_c78bed40(PineValue param_1_0, PineValue param_1_1)
                {
                    if (param_1_0 == CommonReusedValues.Blob_Str_RBNode_elm_builtin)
                    {
                        return
                            BuiltinFunction.concat(
                                PineValue.List(
                                    [
                                    Global_Anonymous.zzz_anon_74ecdc38_c78bed40(
                                        PineValueExtension.ValueFromPathOrEmptyList(
                                            PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [3]),
                                            [
                                            0
                                            ]),
                                        PineValueExtension.ValueFromPathOrEmptyList(
                                            PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [3]),
                                            [
                                            1
                                            ])),
                                    PineValue.List([PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [1])]),
                                    Global_Anonymous.zzz_anon_74ecdc38_c78bed40(
                                        PineValueExtension.ValueFromPathOrEmptyList(
                                            PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [4]),
                                            [
                                            0
                                            ]),
                                        PineValueExtension.ValueFromPathOrEmptyList(
                                            PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [4]),
                                            [
                                            1
                                            ]))
                                    ]));
                    }

                    return PineValue.List([]);
                }

                public static PineValue zzz_anon_821c863b_0f659626(PineValue param_1_0, PineValue param_1_1)
                {
                    if (param_1_0 == param_1_1)
                    {
                        return PineKernelValues.TrueValue;
                    }

                    PineValue local_001 = PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [0]);
                    PineValue local_002 = PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [0]);
                    PineValue local_004 = BuiltinFunction.ValueFromBool(local_002 == CommonReusedValues.Blob_Str_Elm_Float);

                    if (local_001 == CommonReusedValues.Blob_Str_Elm_Float)
                    {
                        PineValue local_005 = PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [1]);
                        PineValue local_006 = PineValueExtension.ValueFromPathOrEmptyList(local_005, [0]);

                        if (local_004 == PineKernelValues.TrueValue)
                        {
                            PineValue local_007 = PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [1]);

                            if (BuiltinFunctionSpecialized.int_mul(
                                local_006,
                                PineValueExtension.ValueFromPathOrEmptyList(local_007, [1])) ==
                                BuiltinFunctionSpecialized.int_mul(
                                    PineValueExtension.ValueFromPathOrEmptyList(local_007, [0]),
                                    PineValueExtension.ValueFromPathOrEmptyList(local_005, [1])))
                            {
                                return PineKernelValues.TrueValue;
                            }

                            return PineKernelValues.FalseValue;
                        }

                        if (local_006 == param_1_1)
                        {
                            if (PineValueExtension.ValueFromPathOrEmptyList(local_005, [1]) == CommonReusedValues.Blob_Int_1)
                            {
                                return PineKernelValues.TrueValue;
                            }

                            return PineKernelValues.FalseValue;
                        }

                        return PineKernelValues.FalseValue;
                    }

                    if (local_004 == PineKernelValues.TrueValue)
                    {
                        PineValue local_008 = PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [1]);

                        if (param_1_0 == PineValueExtension.ValueFromPathOrEmptyList(local_008, [0]))
                        {
                            if (PineValueExtension.ValueFromPathOrEmptyList(local_008, [1]) == CommonReusedValues.Blob_Int_1)
                            {
                                return PineKernelValues.TrueValue;
                            }

                            return PineKernelValues.FalseValue;
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
                            if (Global_Anonymous.zzz_anon_0460788b_b36022e5(
                                PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [0]),
                                PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [1])) ==
                                Global_Anonymous.zzz_anon_0460788b_b36022e5(
                                    PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [0]),
                                    PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [1])))
                            {
                                return PineKernelValues.TrueValue;
                            }

                            return PineKernelValues.FalseValue;
                        }

                        if (local_001 == CommonReusedValues.Blob_Str_Set_elm_builtin)
                        {
                            if (Global_Anonymous.zzz_anon_74ecdc38_c78bed40(
                                PineValueExtension.ValueFromPathOrEmptyList(
                                    PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [1, 0]),
                                    [
                                    0
                                    ]),
                                PineValueExtension.ValueFromPathOrEmptyList(
                                    PineValueExtension.ValueFromPathOrEmptyList(param_1_0, [1, 0]),
                                    [
                                    1
                                    ])) ==
                                Global_Anonymous.zzz_anon_74ecdc38_c78bed40(
                                    PineValueExtension.ValueFromPathOrEmptyList(
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [1, 0]),
                                        [
                                        0
                                        ]),
                                    PineValueExtension.ValueFromPathOrEmptyList(
                                        PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [1, 0]),
                                        [
                                        1
                                        ])))
                            {
                                return PineKernelValues.TrueValue;
                            }

                            return PineKernelValues.FalseValue;
                        }

                        return Global_Anonymous.zzz_anon_42a24045_77105bd2(param_1_0, param_1_1);
                    }

                    return PineKernelValues.FalseValue;
                }
            }
            """".Trim());


        // Now compile this to a .NET assembly.

        var compileToAssemblyResult =
            CompileToAssembly.Compile(
                asCSharp,
                namespacePrefix: [],
                optimizationLevel: Microsoft.CodeAnalysis.OptimizationLevel.Debug)
            .Extract(err =>
            throw new System.Exception("Compilation to assembly failed: " + err.ToString()));

        var stringModule =
            parsedEnv.Modules.Single(m => m.moduleName is "String");

        var fromIntDeclValue =
            stringModule.moduleContent.FunctionDeclarations["fromInt"];

        var fromIntFunctionRecord =
            FunctionRecord.ParseFunctionRecordTagged(fromIntDeclValue, parseCache)
            .Extract(err => throw new System.Exception(
                "Parsing function record for 'fromInt' failed: " + err.ToString()));

        fromIntFunctionRecord.EnvFunctions.Length.Should().Be(0);
        fromIntFunctionRecord.ParameterCount.Should().Be(1);
    }
}
