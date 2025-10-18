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
            """
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
            
            
            """;

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            CodeAnalysisTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                includeDeclaration:
                declName =>
                {
                    return declName == new DeclQualifiedName(["Test"], "computeValueFromString");
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
                public static PineValue blobBytesFromChar(PineValue param_1_0)
                {
                    PineValue local_000 =
                        KernelFunctionFused.CanonicalIntegerFromUnsigned(signIsPositive: true, unsignedValue: param_1_0);

                    PineValue local_001 =
                        IntegerEncoding.EncodeSignedInteger(
                            KernelFunctionSpecialized.length_as_int(local_000));

                    PineValue local_002 =
                        KernelFunction.ValueFromBool(
                            KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(2, local_001));

                    return
                        PineValue.List(
                            [
                                Basics.modBy(
                                    CommonReusedValues.Blob_Int_256,
                                    Basics.idiv(local_000, CommonReusedValues.Blob_Int_16777216)),
                                Basics.modBy(
                                    CommonReusedValues.Blob_Int_256,
                                    local_002 == PineKernelValues.TrueValue
                                    ?
                                    (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(4, local_001)
                                    ?
                                    KernelFunctionFused.SkipLast(skipCount: 2, value: local_000)
                                    :
                                    CommonReusedValues.Blob_Int_0)
                                    :
                                    PineValue.EmptyList),
                                Basics.modBy(
                                    CommonReusedValues.Blob_Int_256,
                                    local_002 == PineKernelValues.TrueValue
                                    ?
                                    (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(3, local_001)
                                    ?
                                    KernelFunctionFused.SkipLast(skipCount: 1, value: local_000)
                                    :
                                    CommonReusedValues.Blob_Int_0)
                                    :
                                    PineValue.EmptyList),
                                Basics.modBy(CommonReusedValues.Blob_Int_256, local_000)
                            ]);
                }


                public static PineValue blobBytesFromChars(
                    PineValue param_1_0,
                    PineValue param_1_1)
                {
                    ImmutableConcatBuilder local_param_1_0 =
                        ImmutableConcatBuilder.Create(
                            [param_1_0]);

                    ImmutableSliceBuilder local_param_1_1 =
                        ImmutableSliceBuilder.Create(param_1_1);

                    while (true)
                    {
                        if (local_param_1_1.IsEmptyList())
                        {
                            return local_param_1_0.EvaluateReverse();
                        }

                        if (!(local_param_1_1.GetLength() == 0))
                        {
                            {
                                local_param_1_0 =
                                    local_param_1_0.PrependItems(
                                        [
                                            PineValue.List(
                                                [
                                                    Test.blobBytesFromChar(
                                                        local_param_1_1.GetHead())
                                                ])
                                        ]);

                                local_param_1_1 =
                                    local_param_1_1.Skip(1);
                            }

                            continue;
                        }

                        throw new ParseExpressionException("TODO: Include details from encoded and env subexpressions");
                    }
                }


                public static PineValue computeValueFromString(PineValue param_1_0)
                {
                    return
                        PineValue.List(
                            [
                                CommonReusedValues.Blob_Str_BlobValue,
                                PineValue.List(
                                    [
                                        KernelFunction.concat(
                                            Test.blobBytesFromChars(
                                                PineValue.EmptyList,
                                                Global_Anonymous.zzz_anon_bef7232d_3c2bb8d5(
                                                    CommonReusedValues.Blob_Int_0,
                                                    PineValue.EmptyList,
                                                    PineValueExtension.ValueFromPathOrEmptyList(
                                                        param_1_0,
                                                        [1, 0]))))
                                    ])
                            ]);
                }
            }

            """".Trim());

        moduleGlobalAnonymousText.Trim().Should().Be(
            """"
            public static class Global_Anonymous
            {
                public static PineValue zzz_anon_bef7232d_3c2bb8d5(
                    PineValue param_1_0,
                    PineValue param_1_1,
                    PineValue param_1_2)
                {
                    PineValue local_param_1_0 =
                        param_1_0;
            
                    ImmutableConcatBuilder local_param_1_1 =
                        ImmutableConcatBuilder.Create(
                            [param_1_1]);
            
                    PineValue local_param_1_2 =
                        param_1_2;
            
                    while (true)
                    {
                        PineValue local_000 =
                            KernelFunctionFused.SkipAndTake(takeCount: 4, skipCountValue: local_param_1_0, argument: local_param_1_2);
            
                        if (KernelFunctionSpecialized.length_as_int(local_000) == 0)
                        {
                            return local_param_1_1.Evaluate();
                        }
            
                        {
                            PineValue local_param_1_0_temp =
                                KernelFunctionSpecialized.int_add(4, local_param_1_0);
            
                            local_param_1_1 =
                                local_param_1_1.AppendItems(
                                    [
                                        PineValue.List(
                                            [local_000])
                                    ]);
            
                            local_param_1_0 =
                                local_param_1_0_temp;
                        }
            
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
                            PineValue local_002 =
                                Global_Anonymous.zzz_anon_f8cc3fb0_cbcb2ff6(local_param_1_0, local_000, CommonReusedValues.Blob_Int_0);
            
                            return
                                KernelFunctionSpecialized.int_add(
                                    KernelFunctionSpecialized.int_mul(16, local_002),
                                    Global_Anonymous.zzz_anon_f8cc3fb0_cbcb2ff6(
                                        KernelFunctionSpecialized.int_add(
                                            local_param_1_0,
                                            KernelFunctionSpecialized.int_mul(local_002, local_000, -1)),
                                        local_param_1_1,
                                        CommonReusedValues.Blob_Int_0));
                        }
            
                        if (KernelFunctionSpecialized.int_is_sorted_asc_as_boolean(local_param_1_1, local_param_1_0))
                        {
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
                            }
            
                            continue;
                        }
            
                        return local_param_1_2;
                    }
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
