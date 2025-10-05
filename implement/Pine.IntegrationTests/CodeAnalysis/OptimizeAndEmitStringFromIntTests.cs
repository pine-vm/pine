using AwesomeAssertions;
using Pine.Core;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.Elm;
using Pine.Core.PopularEncodings;
using System.Linq;
using Xunit;

namespace Pine.IntegrationTests.CodeAnalysis;

public class OptimizeAndEmitStringFromIntTests
{
    [Fact]
    public void Parse_and_emit_optimized_String_fromInt()
    {
        var compiledEnv =
            BundledElmEnvironments.BundledElmCompilerCompiledEnvValue()
            ??
            throw new System.Exception("Failed to load Elm compiler from bundle.");

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
                    return declName == new DeclQualifiedName(["String"], "fromInt");
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        var asCSharp =
            StaticProgramCSharp.FromStaticProgram(
                staticProgram,
                CodeAnalysisTestHelper.DeclarationSyntaxContext);

        var moduleString = asCSharp.ModulesClasses[new DeclQualifiedName([], "String")];

        var moduleStringCSharpText =
            moduleString.RenderToString();

        var moduleGlobalAnonymousText =
            asCSharp.GlobalAnonymousClass.RenderToString();

        moduleStringCSharpText.Trim().Should().Be(
            """"
            public static class String
            {
                public static PineValue fromInt(PineValue param_1_0)
                {
                    return
                        PineValue.List(
                            [
                                CommonReusedValues.Blob_Str_String,
                                PineValue.List(
                                    [
                                        KernelFunction.concat(
                                            Global_Anonymous.zzz_anon_0f6e756a_a4f70149(param_1_0))
                                    ])
                            ]);
                }
            }

            """".Trim());

        moduleGlobalAnonymousText.Trim().Should().Be(
            """"
            public static class Global_Anonymous
            {
                public static PineValue zzz_anon_0f6e756a_a4f70149(PineValue param_1_0)
                {
                    if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(0, param_1_0))
                    {
                        return Global_Anonymous.zzz_anon_12af8dcc_650df00b(param_1_0);
                    }

                    return
                        KernelFunctionSpecialized.concat(
                            CommonReusedValues.List_Single_Blob_Char_hyphen,
                            Global_Anonymous.zzz_anon_12af8dcc_650df00b(
                                KernelFunction.negate(param_1_0)));
                }


                public static PineValue zzz_anon_12af8dcc_650df00b(PineValue param_1_0)
                {
                    return Global_Anonymous.zzz_anon_632693ae_2f148225(param_1_0, PineValue.EmptyList);
                }


                public static PineValue zzz_anon_39fa68f8_2402eeb0(PineValue param_1_0)
                {
                    PineValue local_param_1_0 =
                        param_1_0;

                    while (true)
                    {
                        if (local_param_1_0 == CommonReusedValues.Blob_Int_0)
                        {
                            return CommonReusedValues.Blob_Char_digit_0;
                        }

                        if (local_param_1_0 == CommonReusedValues.Blob_Int_1)
                        {
                            return CommonReusedValues.Blob_Char_digit_1;
                        }

                        if (local_param_1_0 == CommonReusedValues.Blob_Int_2)
                        {
                            return CommonReusedValues.Blob_Char_digit_2;
                        }

                        if (local_param_1_0 == CommonReusedValues.Blob_Int_3)
                        {
                            return CommonReusedValues.Blob_Char_digit_3;
                        }

                        if (local_param_1_0 == CommonReusedValues.Blob_Int_4)
                        {
                            return CommonReusedValues.Blob_Char_digit_4;
                        }

                        if (local_param_1_0 == CommonReusedValues.Blob_Int_5)
                        {
                            return CommonReusedValues.Blob_Char_digit_5;
                        }

                        if (local_param_1_0 == CommonReusedValues.Blob_Int_6)
                        {
                            return CommonReusedValues.Blob_Char_digit_6;
                        }

                        if (local_param_1_0 == CommonReusedValues.Blob_Int_7)
                        {
                            return CommonReusedValues.Blob_Char_digit_7;
                        }

                        if (local_param_1_0 == CommonReusedValues.Blob_Int_8)
                        {
                            return CommonReusedValues.Blob_Char_digit_8;
                        }

                        if (local_param_1_0 == CommonReusedValues.Blob_Int_9)
                        {
                            return CommonReusedValues.Blob_Char_digit_9;
                        }

                        continue;
                    }
                }


                public static PineValue zzz_anon_632693ae_2f148225(
                    PineValue param_1_0,
                    PineValue param_1_1)
                {
                    PineValue local_param_1_0 =
                        param_1_0;

                    PineValue local_param_1_1 =
                        param_1_1;

                    while (true)
                    {
                        if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(local_param_1_0, 0))
                        {
                            if (local_param_1_1 == PineValue.EmptyList)
                            {
                                return CommonReusedValues.List_Single_Blob_Char_digit_0;
                            }

                            return local_param_1_1;
                        }

                        PineValue local_000 =
                            Basics.idiv(local_param_1_0, CommonReusedValues.Blob_Int_10);

                        PineValue local_param_1_0_temp =
                            local_000;

                        PineValue local_param_1_1_temp =
                            KernelFunctionSpecialized.concat(
                                PineValue.List(
                                    [
                                        Global_Anonymous.zzz_anon_39fa68f8_2402eeb0(
                                            KernelFunctionSpecialized.int_add(
                                                local_param_1_0,
                                                KernelFunctionSpecialized.int_mul(-10, local_000)))
                                    ]),
                                local_param_1_1);

                        local_param_1_0 =
                            local_param_1_0_temp;

                        local_param_1_1 =
                            local_param_1_1_temp;

                        continue;
                    }
                }


                public static PineValue zzz_anon_f8cc3fb0_cbcb2ff6(
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
                            KernelFunctionSpecialized.int_mul(16, local_param_1_1);

                        if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(local_000, local_param_1_0))
                        {
                            PineValue local_001 =
                                Global_Anonymous.zzz_anon_f8cc3fb0_cbcb2ff6(local_param_1_0, local_000, CommonReusedValues.Blob_Int_0);

                            return
                                KernelFunctionSpecialized.int_add(
                                    KernelFunctionSpecialized.int_mul(16, local_001),
                                    Global_Anonymous.zzz_anon_f8cc3fb0_cbcb2ff6(
                                        KernelFunctionSpecialized.int_add(
                                            local_param_1_0,
                                            KernelFunctionSpecialized.int_mul(local_001, local_000, -1)),
                                        local_param_1_1,
                                        CommonReusedValues.Blob_Int_0));
                        }

                        if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(local_param_1_1, local_param_1_0))
                        {
                            PineValue local_param_1_0_temp =
                                KernelFunctionSpecialized.int_add(
                                    local_param_1_0,
                                    KernelFunctionSpecialized.int_mul(-1, local_param_1_1));

                            PineValue local_param_1_2_temp =
                                KernelFunctionSpecialized.int_add(1, local_param_1_2);

                            local_param_1_0 =
                                local_param_1_0_temp;

                            local_param_1_2 =
                                local_param_1_2_temp;

                            continue;
                        }

                        return local_param_1_2;
                    }
                }
            }

            """"
            .Trim());


        // Now compile this to a .NET assembly.

        var compileToAssemblyResult =
            CompileToAssembly.Compile(
                asCSharp,
                namespacePrefix: [],
                optimizationLevel: Microsoft.CodeAnalysis.OptimizationLevel.Debug)
            .Extract(err =>
            throw new System.Exception("Compilation to assembly failed: " + err.ToString()));

        var compiledDictionary =
            compileToAssemblyResult.BuildCompiledExpressionsDictionary();

        var stringModule =
            parsedEnv.Modules.Single(m => m.moduleName is "String");

        var fromIntDeclValue =
            stringModule.moduleContent.FunctionDeclarations["fromInt"];

        var fromIntFunctionRecord =
            ElmInteractiveEnvironment.ParseFunctionRecordFromValueTagged(fromIntDeclValue, parseCache)
            .Extract(err => throw new System.Exception(
                "Parsing function record for 'fromInt' failed: " + err.ToString()));

        fromIntFunctionRecord.EnvFunctions.Length.Should().Be(5);

        var buildApp =
            NamesFromCompiledEnv.BuildApplicationFromFunctionRecord(
                fromIntFunctionRecord,
                arguments: [],
                parseCache: parseCache);

        var envFunctions =
            Enumerable.Range(0, 5)
            .Select(efi => buildApp.envValueClass.ParsedItems[[0, efi]])
            .ToArray();

        var callEnvValue =
            PineValue.List(
                [
                PineValue.List(envFunctions),
                PineValue.List(
                    [
                    IntegerEncoding.EncodeSignedInteger(12_345),
                    ]),
                ]);

        var dictEntry =
            compiledDictionary[buildApp.encodedExpr];

        dictEntry.Should().NotBeNull();

        var resultValue = dictEntry(callEnvValue);

        resultValue.Should().NotBeNull();

        resultValue.Should().Be(ElmValueEncoding.StringAsPineValue("12345"));
    }
}
