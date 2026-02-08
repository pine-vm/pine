using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Xunit;

namespace Pine.IntegrationTests.CodeAnalysis;

public class OptimizeAndEmitStringToListTests
{
    [Fact]
    public void Parse_and_emit_optimized_String_toList()
    {
        var elmModuleText =
            """"
            module Test exposing (..)

            
            type String
                = String Int


            toList : String -> List Char
            toList (String charsBlob) =
                toListRecursive
                    0
                    []
                    charsBlob


            toListRecursive : Int -> List Char -> Int -> List Char
            toListRecursive offset list blob =
                let
                    nextChar =
                        Pine_kernel.take
                            [ 4
                            , Pine_kernel.skip [ offset, blob ]
                            ]
                in
                if Pine_kernel.equal [ Pine_kernel.length nextChar, 0 ] then
                    list

                else
                    toListRecursive
                        (Pine_kernel.int_add [ offset, 4 ])
                        (Pine_kernel.concat [ list, [ nextChar ] ])
                        blob
            
            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            CodeAnalysisTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                includeDeclaration:
                declName =>
                {
                    return declName == new DeclQualifiedName(["Test"], "toList");
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        var asCSharp =
            StaticProgramCSharp.FromStaticProgram(
                staticProgram,
                CodeAnalysisTestHelper.DeclarationSyntaxContext);

        var moduleTest = asCSharp.ModulesClasses[new DeclQualifiedName([], "Test")];

        var moduleTestCSharpText =
            moduleTest.RenderToString();

        var moduleGlobalAnonymousText =
            asCSharp.GlobalAnonymousClass.RenderToString();

        moduleTestCSharpText.Trim().Should().Be(
            """"
            public static class Test
            {
                public static PineValue toListRecursive(PineValue param_1_0, PineValue param_1_1, PineValue param_1_2)
                {
                    PineValue local_param_1_0 = param_1_0;
                    ImmutableConcatBuilder local_param_1_1 = ImmutableConcatBuilder.Create([param_1_1]);
                    PineValue local_param_1_2 = param_1_2;

                    while (true)
                    {
                        PineValue local_000 =
                            KernelFunctionFused.SkipAndTake(
                                takeCount: 4,
                                skipCountValue: local_param_1_0,
                                argument: local_param_1_2);

                        if (KernelFunctionSpecialized.length_as_int(local_000) is 0)
                        {
                            return local_param_1_1.Evaluate();
                        }

                        {
                            PineValue local_param_1_0_temp = KernelFunctionSpecialized.int_add(4, local_param_1_0);
                            local_param_1_1 = local_param_1_1.AppendItems([PineValue.List([local_000])]);
                            local_param_1_0 = local_param_1_0_temp;
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
            }

            """"
            .Trim());

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
