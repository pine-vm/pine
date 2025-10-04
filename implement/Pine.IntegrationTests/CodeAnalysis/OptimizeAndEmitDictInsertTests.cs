using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.DotNet;
using Pine.Core.Elm;
using Xunit;

namespace Pine.IntegrationTests.CodeAnalysis;

public class OptimizeAndEmitDictInsertTests
{
    [Fact]
    public void Parse_and_emit_optimized_Dict_insert()
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
                    return declName == new DeclQualifiedName(["Dict"], "insert");
                },
                parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        var asCSharp =
            StaticProgramCSharp.FromStaticProgram(
                staticProgram,
                CodeAnalysisTestHelper.DeclarationSyntaxContext);

        var moduleDict = asCSharp.ModulesClasses[new DeclQualifiedName([], "Dict")];

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
                        KernelFunction.head(
                            KernelFunctionSpecialized.skip(
                                1,
                                Global_Anonymous.zzz_anon_ea679199_b65beb0f(param_1_0, param_1_1, param_1_2)));

                    if ((CommonReusedValues.Blob_Str_Red == KernelFunction.head(
                        KernelFunction.head(local_000))
                    ?
                    KernelFunction.ValueFromBool(
                        CommonReusedValues.Blob_Str_RBNode_elm_builtin == KernelFunction.head(
                            Global_Anonymous.zzz_anon_ea679199_b65beb0f(param_1_0, param_1_1, param_1_2)))
                    :
                    CommonReusedValues.Bool_False) == PineKernelValues.TrueValue)
                    {
                        return
                            PineValue.List(
                                [
                                    CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                    PineValue.List(
                                        [
                                            CommonReusedValues.List_7222f8d4,
                                            KernelFunction.head(
                                                KernelFunctionSpecialized.skip(1, local_000)),
                                            KernelFunction.head(
                                                KernelFunctionSpecialized.skip(2, local_000)),
                                            KernelFunction.head(
                                                KernelFunctionSpecialized.skip(3, local_000)),
                                            KernelFunction.head(
                                                KernelFunctionSpecialized.skip(4, local_000))
                                        ])
                                ]);
                    }

                    return Global_Anonymous.zzz_anon_ea679199_b65beb0f(param_1_0, param_1_1, param_1_2);
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
                        KernelFunction.equal(
                            PineValue.List(
                                [
                                    KernelFunctionSpecialized.take(0, param_1_0),
                                    CommonReusedValues.List_dda26649
                                ]));
                }


                public static PineValue zzz_anon_e6d15ff4_dda26649(
                    PineValue param_1_0,
                    PineValue param_1_1,
                    PineValue param_1_2,
                    PineValue param_1_3,
                    PineValue param_1_4)
                {
                    PineValue local_000 =
                        KernelFunction.head(
                            KernelFunctionSpecialized.skip(1, param_1_4));

                    if ((CommonReusedValues.Blob_Str_Red == KernelFunction.head(
                        KernelFunction.head(local_000))
                    ?
                    KernelFunction.ValueFromBool(
                        CommonReusedValues.Blob_Str_RBNode_elm_builtin == KernelFunction.head(param_1_4))
                    :
                    CommonReusedValues.Bool_False) == PineKernelValues.TrueValue)
                    {
                        PineValue local_001 =
                            KernelFunction.head(
                                KernelFunctionSpecialized.skip(1, param_1_3));

                        if ((CommonReusedValues.Blob_Str_Red == KernelFunction.head(
                            KernelFunction.head(local_001))
                        ?
                        KernelFunction.ValueFromBool(
                            CommonReusedValues.Blob_Str_RBNode_elm_builtin == KernelFunction.head(param_1_3))
                        :
                        CommonReusedValues.Bool_False) == PineKernelValues.TrueValue)
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
                                                            [
                                                                CommonReusedValues.List_7222f8d4,
                                                                KernelFunction.head(
                                                                    KernelFunctionSpecialized.skip(1, local_001)),
                                                                KernelFunction.head(
                                                                    KernelFunctionSpecialized.skip(2, local_001)),
                                                                KernelFunction.head(
                                                                    KernelFunctionSpecialized.skip(3, local_001)),
                                                                KernelFunction.head(
                                                                    KernelFunctionSpecialized.skip(4, local_001))
                                                            ])
                                                    ]),
                                                PineValue.List(
                                                    [
                                                        CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                                        PineValue.List(
                                                            [
                                                                CommonReusedValues.List_7222f8d4,
                                                                KernelFunction.head(
                                                                    KernelFunctionSpecialized.skip(1, local_000)),
                                                                KernelFunction.head(
                                                                    KernelFunctionSpecialized.skip(2, local_000)),
                                                                KernelFunction.head(
                                                                    KernelFunctionSpecialized.skip(3, local_000)),
                                                                KernelFunction.head(
                                                                    KernelFunctionSpecialized.skip(4, local_000))
                                                            ])
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
                                            KernelFunction.head(
                                                KernelFunctionSpecialized.skip(1, local_000)),
                                            KernelFunction.head(
                                                KernelFunctionSpecialized.skip(2, local_000)),
                                            PineValue.List(
                                                [
                                                    CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                                    PineValue.List(
                                                        [
                                                            CommonReusedValues.List_dafb9d35,
                                                            param_1_1,
                                                            param_1_2,
                                                            param_1_3,
                                                            KernelFunction.head(
                                                                KernelFunctionSpecialized.skip(3, local_000))
                                                        ])
                                                ]),
                                            KernelFunction.head(
                                                KernelFunctionSpecialized.skip(4, local_000))
                                        ])
                                ]);
                    }

                    PineValue local_002 =
                        KernelFunction.head(
                            KernelFunctionSpecialized.skip(
                                1,
                                KernelFunction.head(
                                    KernelFunctionSpecialized.skip(
                                        3,
                                        KernelFunction.head(
                                            KernelFunctionSpecialized.skip(1, param_1_3))))));

                    if ((CommonReusedValues.Blob_Str_Red == KernelFunction.head(
                        KernelFunction.head(local_002))
                    ?
                    CommonReusedValues.Blob_Str_RBNode_elm_builtin == KernelFunction.head(
                        KernelFunction.head(
                            KernelFunctionSpecialized.skip(
                                3,
                                KernelFunction.head(
                                    KernelFunctionSpecialized.skip(1, param_1_3)))))
                    ?
                    CommonReusedValues.Blob_Str_Red == KernelFunction.head(
                        KernelFunction.head(
                            KernelFunction.head(
                                KernelFunctionSpecialized.skip(1, param_1_3))))
                    ?
                    KernelFunction.ValueFromBool(
                        CommonReusedValues.Blob_Str_RBNode_elm_builtin == KernelFunction.head(param_1_3))
                    :
                    CommonReusedValues.Bool_False
                    :
                    CommonReusedValues.Bool_False
                    :
                    CommonReusedValues.Bool_False) == PineKernelValues.TrueValue)
                    {
                        PineValue local_003 =
                            KernelFunction.head(
                                KernelFunctionSpecialized.skip(1, param_1_3));

                        return
                            PineValue.List(
                                [
                                    CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                    PineValue.List(
                                        [
                                            CommonReusedValues.List_dafb9d35,
                                            KernelFunction.head(
                                                KernelFunctionSpecialized.skip(1, local_003)),
                                            KernelFunction.head(
                                                KernelFunctionSpecialized.skip(2, local_003)),
                                            PineValue.List(
                                                [
                                                    CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                                    PineValue.List(
                                                        [
                                                            CommonReusedValues.List_7222f8d4,
                                                            KernelFunction.head(
                                                                KernelFunctionSpecialized.skip(1, local_002)),
                                                            KernelFunction.head(
                                                                KernelFunctionSpecialized.skip(2, local_002)),
                                                            KernelFunction.head(
                                                                KernelFunctionSpecialized.skip(3, local_002)),
                                                            KernelFunction.head(
                                                                KernelFunctionSpecialized.skip(4, local_002))
                                                        ])
                                                ]),
                                            PineValue.List(
                                                [
                                                    CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                                    PineValue.List(
                                                        [
                                                            CommonReusedValues.List_7222f8d4,
                                                            param_1_1,
                                                            param_1_2,
                                                            KernelFunction.head(
                                                                KernelFunctionSpecialized.skip(4, local_003)),
                                                            param_1_4
                                                        ])
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
                        KernelFunction.head(param_1_2);

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
                        PineValue local_001 =
                            KernelFunction.head(
                                KernelFunctionSpecialized.skip(1, param_1_2));

                        PineValue local_002 =
                            Basics.compare(
                                param_1_0,
                                KernelFunction.head(
                                    KernelFunctionSpecialized.skip(1, local_001)));

                        PineValue local_003 =
                            KernelFunction.head(local_002);

                        if (CommonReusedValues.Blob_Str_LT == local_003)
                        {
                            return
                                Global_Anonymous.zzz_anon_e6d15ff4_dda26649(
                                    KernelFunction.head(local_001),
                                    KernelFunction.head(
                                        KernelFunctionSpecialized.skip(1, local_001)),
                                    KernelFunction.head(
                                        KernelFunctionSpecialized.skip(2, local_001)),
                                    Global_Anonymous.zzz_anon_ea679199_b65beb0f(
                                        param_1_0,
                                        param_1_1,
                                        KernelFunction.head(
                                            KernelFunctionSpecialized.skip(3, local_001))),
                                    KernelFunction.head(
                                        KernelFunctionSpecialized.skip(4, local_001)));
                        }

                        if (CommonReusedValues.Blob_Str_EQ == local_003)
                        {
                            return
                                PineValue.List(
                                    [
                                        CommonReusedValues.Blob_Str_RBNode_elm_builtin,
                                        PineValue.List(
                                            [
                                                KernelFunction.head(local_001),
                                                KernelFunction.head(
                                                    KernelFunctionSpecialized.skip(1, local_001)),
                                                param_1_1,
                                                KernelFunction.head(
                                                    KernelFunctionSpecialized.skip(3, local_001)),
                                                KernelFunction.head(
                                                    KernelFunctionSpecialized.skip(4, local_001))
                                            ])
                                    ]);
                        }

                        if (CommonReusedValues.Blob_Str_GT == local_003)
                        {
                            return
                                Global_Anonymous.zzz_anon_e6d15ff4_dda26649(
                                    KernelFunction.head(local_001),
                                    KernelFunction.head(
                                        KernelFunctionSpecialized.skip(1, local_001)),
                                    KernelFunction.head(
                                        KernelFunctionSpecialized.skip(2, local_001)),
                                    KernelFunction.head(
                                        KernelFunctionSpecialized.skip(3, local_001)),
                                    Global_Anonymous.zzz_anon_ea679199_b65beb0f(
                                        param_1_0,
                                        param_1_1,
                                        KernelFunction.head(
                                            KernelFunctionSpecialized.skip(4, local_001))));
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
