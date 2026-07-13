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

                    PineValue local_023 = BuiltinFunctionSpecialized.int_mul(-256, local_016);
                    PineValue local_024 = Global_Anonymous.zzz_anon_66da43a6_dda26649(local_016, CommonReusedValues.Blob_Int_256);
                    PineValue local_025 = Global_Anonymous.zzz_anon_66da43a6_dda26649(local_017, CommonReusedValues.Blob_Int_256);
                    PineValue local_026 = Global_Anonymous.zzz_anon_66da43a6_dda26649(local_018, CommonReusedValues.Blob_Int_256);
                    PineValue local_030 = BuiltinFunctionSpecialized.int_mul(-256, local_024);
                    PineValue local_031 = BuiltinFunctionSpecialized.int_mul(-256, local_025);
                    PineValue local_032 = BuiltinFunctionSpecialized.int_mul(-256, local_026);

                    PineValue local_033 =
                        BuiltinFunction.int_add(PineValue.List([local_001, IntegerEncoding.EncodeSignedInteger(0), local_023]));

                    PineValue local_034 = BuiltinFunctionSpecialized.int_add(local_016, local_030);
                    PineValue local_035 = BuiltinFunctionSpecialized.int_add(local_017, local_031);
                    PineValue local_036 = BuiltinFunctionSpecialized.int_add(local_018, local_032);

                    return
                        PineValue.List(
                            [
                            BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(0, local_036)
                            ?
                            local_036
                            :
                            BuiltinFunction.int_add(
                                PineValue.List([local_018, local_032, IntegerEncoding.EncodeSignedInteger(256)])),
                            BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(0, local_035)
                            ?
                            local_035
                            :
                            BuiltinFunction.int_add(
                                PineValue.List([local_017, local_031, IntegerEncoding.EncodeSignedInteger(256)])),
                            BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(0, local_034)
                            ?
                            local_034
                            :
                            BuiltinFunction.int_add(
                                PineValue.List([local_016, local_030, IntegerEncoding.EncodeSignedInteger(256)])),
                            BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(0, local_033)
                            ?
                            local_033
                            :
                            BuiltinFunction.int_add(
                                PineValue.List([IntegerEncoding.EncodeSignedInteger(256), local_001, local_023]))
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

                public static PineValue zzz_anon_66da43a6_dda26649(PineValue param_1, PineValue param_2)
                {
                    if (param_2 == CommonReusedValues.Blob_Int_0)
                    {
                        return CommonReusedValues.Blob_Int_0;
                    }

                    PineValue local_003 =
                        BuiltinFunction.ValueFromBool(BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(0, param_1));

                    PineValue local_004 =
                        BuiltinFunction.ValueFromBool(BuiltinFunctionSpecialized.int_is_sorted_asc_as_boolean(0, param_2));

                    PineValue local_005 =
                        local_003 == PineKernelValues.TrueValue ? param_1 : BuiltinFunctionSpecialized.int_mul(-1, param_1);

                    PineValue local_006 =
                        local_004 == PineKernelValues.TrueValue ? param_2 : BuiltinFunctionSpecialized.int_mul(-1, param_2);

                    PineValue local_009 =
                        Global_Anonymous.zzz_anon_38e6937c_a50db278(local_005, local_006, CommonReusedValues.Blob_Int_0);

                    if (local_003 == local_004)
                    {
                        return local_009;
                    }

                    return BuiltinFunctionSpecialized.int_mul(-1, local_009);
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
