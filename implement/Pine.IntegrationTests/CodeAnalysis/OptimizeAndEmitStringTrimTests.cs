using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.Elm;
using Xunit;

namespace Pine.IntegrationTests.CodeAnalysis;

public class OptimizeAndEmitStringTrimTests
{
    [Fact]
    public void Parse_and_emit_optimized_String_trim()
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
                    return declName == new DeclQualifiedName(["String"], "trim");
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
                public static PineValue trim(PineValue param_1_0)
                {
                    return
                        PineValue.List(
                            [
                                CommonReusedValues.Blob_Str_String,
                                PineValue.List(
                                    [
                                        KernelFunctionSpecialized.skip(
                                            Global_Anonymous.zzz_anon_449d95bc_da6f86d5(
                                                CommonReusedValues.Blob_Int_0,
                                                KernelFunction.head(
                                                    KernelFunction.head(
                                                        KernelFunctionSpecialized.skip(1, param_1_0)))),
                                            KernelFunctionSpecialized.take(
                                                Global_Anonymous.zzz_anon_627f403e_dca18c16(
                                                    KernelFunction.length(
                                                        KernelFunction.head(
                                                            KernelFunction.head(
                                                                KernelFunctionSpecialized.skip(1, param_1_0)))),
                                                    KernelFunction.head(
                                                        KernelFunction.head(
                                                            KernelFunctionSpecialized.skip(1, param_1_0)))),
                                                KernelFunction.head(
                                                    KernelFunction.head(
                                                        KernelFunctionSpecialized.skip(1, param_1_0)))))
                                    ])
                            ]);
                }
            }

            """".Trim());

        moduleGlobalAnonymousText.Trim().Should().Be(
            """"
            public static class Global_Anonymous
            {
                public static PineValue zzz_anon_627f403e_dca18c16(
                    PineValue param_1_0,
                    PineValue param_1_1)
                {
                    PineValue local_param_1_0 =
                        param_1_0;

                    PineValue local_param_1_1 =
                        param_1_1;

                    while (true)
                    {
                        if (local_param_1_0 == CommonReusedValues.Blob_Int_0)
                        {
                            return CommonReusedValues.Blob_Int_0;
                        }

                        if (Global_Anonymous.zzz_anon_d97a2014_dda26649(
                            KernelFunctionFused.SkipAndTake(
                                takeCount: 4,
                                skipCountValue: KernelFunctionSpecialized.int_add(-4, local_param_1_0),
                                argument: local_param_1_1)) == PineKernelValues.TrueValue)
                        {
                            local_param_1_0 =
                                KernelFunctionSpecialized.int_add(-4, local_param_1_0);

                            continue;
                        }

                        return local_param_1_0;
                    }
                }


                public static PineValue zzz_anon_449d95bc_da6f86d5(
                    PineValue param_1_0,
                    PineValue param_1_1)
                {
                    PineValue local_param_1_0 =
                        param_1_0;

                    PineValue local_param_1_1 =
                        param_1_1;

                    while (true)
                    {
                        PineValue local_000 =
                            KernelFunctionFused.SkipAndTake(takeCount: 4, skipCountValue: local_param_1_0, argument: local_param_1_1);

                        if (KernelFunction.length(local_000) == CommonReusedValues.Blob_Int_0)
                        {
                            return local_param_1_0;
                        }

                        if (Global_Anonymous.zzz_anon_d97a2014_dda26649(local_000) == PineKernelValues.TrueValue)
                        {
                            local_param_1_0 =
                                KernelFunctionSpecialized.int_add(4, local_param_1_0);

                            continue;
                        }

                        return local_param_1_0;
                    }
                }


                public static PineValue zzz_anon_d97a2014_dda26649(PineValue param_1_0)
                {
                    if (param_1_0 == CommonReusedValues.Blob_4ac777a0)
                    {
                        return CommonReusedValues.Bool_True;
                    }

                    if (param_1_0 == CommonReusedValues.Blob_2a82563d)
                    {
                        return CommonReusedValues.Bool_True;
                    }

                    if (param_1_0 == CommonReusedValues.Blob_19caa572)
                    {
                        return CommonReusedValues.Bool_True;
                    }

                    if (param_1_0 == CommonReusedValues.Blob_3c2131e8)
                    {
                        return CommonReusedValues.Bool_True;
                    }

                    if (param_1_0 == CommonReusedValues.Blob_f17714cc)
                    {
                        return CommonReusedValues.Bool_True;
                    }

                    return CommonReusedValues.Bool_False;
                }
            }

            """"
            .Trim());
    }
}
