using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Xunit;

namespace Pine.IntegrationTests.CodeAnalysis;

public class OptimizeAndEmitValueFromStringTests
{
    [Fact]
    public void Parse_and_emit_optimized_Pine_computeValueFromString()
    {
        var elmModuleText =
            """"
            module Test exposing (..)
            

            type Value
                = BlobValue (List Int)
                | ListValue (List Value)


            computeValueFromString : String -> Value
            computeValueFromString string =
                let
                    charsBytes : List (List Int)
                    charsBytes =
                        blobBytesFromChars [] (String.toList string)
                in
                BlobValue
                    (List.concat charsBytes)


            blobBytesFromChars : List (List Int) -> List Char -> List (List Int)
            blobBytesFromChars acc remaining =
                case remaining of
                    [] ->
                        List.reverse acc

                    char :: rest ->
                        blobBytesFromChars
                            (blobBytesFromChar char :: acc)
                            rest


            valueFromChar : Char -> Value
            valueFromChar char =
                BlobValue (blobBytesFromChar char)


            blobBytesFromChar : Char -> List Int
            blobBytesFromChar char =
                let
                    charCode : Int
                    charCode =
                        Char.toCode char
                in
                [ modBy 0x0100 (charCode // 0x01000000)
                , modBy 0x0100 (charCode // 0x00010000)
                , modBy 0x0100 (charCode // 0x0100)
                , modBy 0x0100 charCode
                ]
            
            
            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram, functionMetadata) =
            CodeAnalysisTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                includeDeclaration:
                declName =>
                {
                    return declName.FullName == "Test.computeValueFromString";
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram, functionMetadata);

        var asCSharp =
            StaticProgramCSharp.FromStaticProgram(
                staticProgram,
                functionMetadata,
                CodeAnalysisTestHelper.DeclarationSyntaxContext);

        var moduleTest = asCSharp.ModulesClasses[DeclQualifiedName.Create([], "Test")];

        var moduleTestCSharpText =
            moduleTest.RenderToString();

        var moduleGlobalAnonymousText =
            asCSharp.GlobalAnonymousClass.RenderToString();

        moduleTestCSharpText.Trim().Should().Be(
            """"
            public static class Test
            {
                public static PineValue blobBytesFromChar(PineValue param_1)
                {
                    PineValue local_001 = BuiltinFunctionFused.BlobPrependByte(byteToPrepend: 4, suffix: param_1);

                    PineValue local_003 =
                        BuiltinFunctionFused.CanonicalIntegerFromUnsigned(signIsPositive: true, unsignedValue: param_1);

                    PineValue local_005 =
                        BuiltinFunction.ValueFromBool(BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(0, local_003));

                    PineValue local_006 =
                        local_005 == PineKernelValues.TrueValue ? local_003 : BuiltinFunctionSpecialized.int_mul(-1, local_003);

                    PineValue local_013 =
                        Global_Anonymous.zzz_anon_38e6937c_a50db278(
                            local_006,
                            CommonReusedValues.Blob_Int_256,
                            CommonReusedValues.Blob_Int_0);

                    PineValue local_014 =
                        Global_Anonymous.zzz_anon_38e6937c_a50db278(
                            local_006,
                            CommonReusedValues.Blob_Int_65536,
                            CommonReusedValues.Blob_Int_0);

                    PineValue local_015 =
                        Global_Anonymous.zzz_anon_38e6937c_a50db278(
                            local_006,
                            CommonReusedValues.Blob_Int_16777216,
                            CommonReusedValues.Blob_Int_0);

                    PineValue local_016 =
                        local_005 == PineKernelValues.TrueValue ? local_013 : BuiltinFunctionSpecialized.int_mul(-1, local_013);

                    PineValue local_017 =
                        local_005 == PineKernelValues.TrueValue ? local_014 : BuiltinFunctionSpecialized.int_mul(-1, local_014);

                    PineValue local_018 =
                        local_005 == PineKernelValues.TrueValue ? local_015 : BuiltinFunctionSpecialized.int_mul(-1, local_015);

                    PineValue local_019 = PineValue.List([local_016]);
                    PineValue local_020 = PineValue.List([local_017]);
                    PineValue local_021 = PineValue.List([local_018]);
                    PineValue local_026 = BuiltinFunctionSpecialized.int_mul(-256, local_016);
                    PineValue local_027 = Global_Anonymous.zzz_anon_4c579ba7_dda26649(local_019, CommonReusedValues.Blob_Int_256);
                    PineValue local_028 = Global_Anonymous.zzz_anon_4c579ba7_dda26649(local_020, CommonReusedValues.Blob_Int_256);
                    PineValue local_029 = Global_Anonymous.zzz_anon_4c579ba7_dda26649(local_021, CommonReusedValues.Blob_Int_256);
                    PineValue local_033 = BuiltinFunctionSpecialized.int_mul(-256, local_027);
                    PineValue local_034 = BuiltinFunctionSpecialized.int_mul(-256, local_028);
                    PineValue local_035 = BuiltinFunctionSpecialized.int_mul(-256, local_029);

                    PineValue local_036 =
                        BuiltinFunction.int_add(PineValue.List([local_001, IntegerEncoding.EncodeSignedInteger(0), local_026]));

                    PineValue local_037 = BuiltinFunctionSpecialized.int_add(local_016, local_033);
                    PineValue local_038 = BuiltinFunctionSpecialized.int_add(local_017, local_034);
                    PineValue local_039 = BuiltinFunctionSpecialized.int_add(local_018, local_035);

                    return
                        PineValue.List(
                            [
                            BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(0, local_039)
                            ?
                            local_039
                            :
                            BuiltinFunction.int_add(
                                PineValue.List([local_018, local_035, IntegerEncoding.EncodeSignedInteger(256)])),
                            BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(0, local_038)
                            ?
                            local_038
                            :
                            BuiltinFunction.int_add(
                                PineValue.List([local_017, local_034, IntegerEncoding.EncodeSignedInteger(256)])),
                            BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(0, local_037)
                            ?
                            local_037
                            :
                            BuiltinFunction.int_add(
                                PineValue.List([local_016, local_033, IntegerEncoding.EncodeSignedInteger(256)])),
                            BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(0, local_036)
                            ?
                            local_036
                            :
                            BuiltinFunction.int_add(
                                PineValue.List([IntegerEncoding.EncodeSignedInteger(256), local_001, local_026]))
                            ]);
                }

                public static PineValue blobBytesFromChars(PineValue param_1, PineValue param_2_0)
                {
                    ImmutableConcatBuilder local_param_1 = ImmutableConcatBuilder.Create([param_1]);
                    PineValue local_param_2_0 = param_2_0;

                    while (true)
                    {
                        if (BuiltinFunctionSpecialized.length_as_int(PineValue.EmptyList) == 0)
                        {
                            return local_param_1.EvaluateReverse();
                        }

                        {
                            PineValue local_param_2_0_temp =
                                PineValueExtension.ValueFromPathOrEmptyList(
                                    BuiltinFunctionSpecialized.skip(1, PineValue.EmptyList),
                                    [
                                    0
                                    ]);

                            local_param_1 = local_param_1.PrependItems([PineValue.List([Test.blobBytesFromChar(local_param_2_0)])]);
                            local_param_2_0 = local_param_2_0_temp;
                        }

                        continue;
                    }
                }

                public static PineValue computeValueFromString()
                {
                    return
                        PineValue.List(
                            [
                            CommonReusedValues.Blob_Str_BlobValue,
                            PineValue.List(
                                [
                                BuiltinFunction.concat(
                                    Test.blobBytesFromChars(
                                        PineValue.EmptyList,
                                        PineValueExtension.ValueFromPathOrEmptyList(
                                            String.toList(PineValueExtension.ValueFromPathOrEmptyList(PineValue.EmptyList, [1])),
                                            [
                                            0
                                            ])))
                                ])
                            ]);
                }
            }
            
            """".Trim());

        moduleGlobalAnonymousText.Trim().Should().Be(
            """"
            public static class Global_Anonymous
            {
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

                public static PineValue zzz_anon_4c579ba7_dda26649(PineValue param_0, PineValue param_1)
                {
                    PineValue local_000 =
                        BuiltinFunctionFused.ListPrependItem(
                            itemToPrepend: PineValue.EmptyList,
                            suffix: BuiltinFunctionFused.ListAppendItem(prefix: param_0, itemToAppend: param_1));

                    PineValue local_001 = PineValueExtension.ValueFromPathOrEmptyList(local_000, [2]);

                    if (local_001 == CommonReusedValues.Blob_Int_0)
                    {
                        return CommonReusedValues.Blob_Int_0;
                    }

                    PineValue local_004 = PineValueExtension.ValueFromPathOrEmptyList(local_000, [1]);

                    PineValue local_007 =
                        BuiltinFunction.ValueFromBool(BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(0, local_004));

                    PineValue local_008 =
                        BuiltinFunction.ValueFromBool(BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(0, local_001));

                    PineValue local_009 =
                        local_007 == PineKernelValues.TrueValue ? local_004 : BuiltinFunctionSpecialized.int_mul(-1, local_004);

                    PineValue local_010 =
                        local_008 == PineKernelValues.TrueValue ? local_001 : BuiltinFunctionSpecialized.int_mul(-1, local_001);

                    PineValue local_013 =
                        Global_Anonymous.zzz_anon_38e6937c_a50db278(local_009, local_010, CommonReusedValues.Blob_Int_0);

                    if (local_007 == local_008)
                    {
                        return local_013;
                    }

                    return BuiltinFunctionSpecialized.int_mul(-1, local_013);
                }
            }
            
            """".Trim());

        var compileToAssemblyResult =
            CompileToAssembly.Compile(
                asCSharp,
                namespacePrefix: [],
                optimizationLevel: Microsoft.CodeAnalysis.OptimizationLevel.Debug)
            .Extract(err =>
            throw new System.Exception("Compilation to assembly failed: " + err.ToString()));

        var compiledDictionary =
            compileToAssemblyResult.BuildCompiledExpressionsDictionary();
    }
}
