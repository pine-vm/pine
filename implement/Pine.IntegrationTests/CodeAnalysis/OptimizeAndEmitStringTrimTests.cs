using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Xunit;

namespace Pine.IntegrationTests.CodeAnalysis;

public class OptimizeAndEmitStringTrimTests
{
    [Fact]
    public void Parse_and_emit_optimized_String_trim()
    {
        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram, functionMetadata) =
            CodeAnalysisTestHelper.StaticProgramFromElmKernelModules(
                ["String.elm"],
                includeDeclaration:
                declName =>
                {
                    return declName.FullName == "String.trim";
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
                public static PineValue trim(PineValue param_1_1)
                {
                    PineValue local_000 = PineValueExtension.ValueFromPathOrEmptyList(param_1_1, [0]);

                    return
                        PineValue.List(
                            [
                            CommonReusedValues.Blob_Str_String,
                            PineValue.List(
                                [
                                KernelFunctionFused.TakeAndSkip(
                                    skipCountValue: String.trimLeftCountBytesTrimmed(CommonReusedValues.Blob_Int_0, local_000),
                                    takeCountValue: String.trimRightCountBytesRemaining(KernelFunction.length(local_000), local_000),
                                    argument: local_000)
                                ])
                            ]);
                }

                public static PineValue trimLeftCountBytesTrimmed(PineValue param_1, PineValue param_2)
                {
                    PineValue local_param_1 = param_1;
                    PineValue local_param_2 = param_2;

                    while (true)
                    {
                        PineValue local_000 =
                            KernelFunctionFused.SkipAndTake(takeCount: 4, skipCountValue: local_param_1, argument: local_param_2);

                        if (KernelFunctionSpecialized.length_as_int(local_000) == 0)
                        {
                            return local_param_1;
                        }

                        if (local_000 == CommonReusedValues.Blob_Char_space
                        ?
                        true
                        :
                        (local_000 == CommonReusedValues.Blob_Char_tab
                        ?
                        true
                        :
                        (local_000 == CommonReusedValues.Blob_Char_newline
                        ?
                        true
                        :
                        (local_000 == CommonReusedValues.Blob_Char_carriagereturn
                        ?
                        true
                        :
                        (local_000 == CommonReusedValues.Blob_Char_nobreakspace ? true : false)))))
                        {
                            {
                                PineValue local_param_1_temp = KernelFunctionSpecialized.int_add(4, local_param_1);
                                local_param_1 = local_param_1_temp;
                            }

                            continue;
                        }

                        return local_param_1;
                    }
                }

                public static PineValue trimRightCountBytesRemaining(PineValue param_1, PineValue param_2)
                {
                    PineValue local_param_1 = param_1;
                    PineValue local_param_2 = param_2;

                    while (true)
                    {
                        if (local_param_1 == CommonReusedValues.Blob_Int_0)
                        {
                            return CommonReusedValues.Blob_Int_0;
                        }

                        PineValue local_000 = KernelFunctionSpecialized.int_add(-4, local_param_1);

                        PineValue local_001 =
                            KernelFunctionFused.SkipAndTake(takeCount: 4, skipCountValue: local_000, argument: local_param_2);

                        if (local_001 == CommonReusedValues.Blob_Char_space
                        ?
                        true
                        :
                        (local_001 == CommonReusedValues.Blob_Char_tab
                        ?
                        true
                        :
                        (local_001 == CommonReusedValues.Blob_Char_newline
                        ?
                        true
                        :
                        (local_001 == CommonReusedValues.Blob_Char_carriagereturn
                        ?
                        true
                        :
                        (local_001 == CommonReusedValues.Blob_Char_nobreakspace ? true : false)))))
                        {
                            {
                                PineValue local_param_1_temp = local_000;
                                local_param_1 = local_param_1_temp;
                            }

                            continue;
                        }

                        return local_param_1;
                    }
                }
            }
            """".Trim());

        moduleGlobalAnonymousText.Trim().Should().Be(
            """"
            public static class Global_Anonymous
            {
            }
            """".Trim());
    }
}
