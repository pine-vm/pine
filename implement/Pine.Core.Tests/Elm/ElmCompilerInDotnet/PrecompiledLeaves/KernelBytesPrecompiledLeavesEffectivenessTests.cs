using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using Pine.Core.Interpreter.IntermediateVM;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.PrecompiledLeaves;

public class KernelBytesPrecompiledLeavesEffectivenessTests
{
    private const string TestModuleText =
        """"
        module KernelBytesPrecompiledLeavesTestModule exposing (..)

        import Bytes exposing (Bytes)
        import Bytes.Decode
        import Bytes.Encode


        decodeString : Bytes -> Maybe String
        decodeString bytes =
            Bytes.Decode.decode
                (Bytes.Decode.string (Bytes.width bytes))
                bytes


        encodeString : String -> Bytes
        encodeString value =
            Bytes.Encode.encode (Bytes.Encode.string value)
        """"
        ;

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(
            () =>
            {
                var kernelModulesTree = BundledFiles.ElmKernelModulesDefault.Value;

                var treeWithTest =
                    kernelModulesTree.SetNodeAtPathSorted(
                        ["KernelBytesPrecompiledLeavesTestModule.elm"],
                        FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

                var rootFilePaths =
                    treeWithTest.EnumerateFilesTransitive()
                    .Where(
                        file =>
                        file.path[^1].Equals(
                            "KernelBytesPrecompiledLeavesTestModule.elm",
                            StringComparison.OrdinalIgnoreCase))
                    .Select(file => (IReadOnlyList<string>)file.path)
                    .ToList();

                var compiledEnv =
                    ElmCompiler.CompileInteractiveEnvironment(
                        treeWithTest,
                        rootFilePaths: rootFilePaths)
                    .Map(result => result.compiledEnvValue)
                    .Extract(error => throw new Exception("Failed compiling: " + error));

                return
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
                    .Extract(error => throw new Exception("Failed parsing: " + error));
            });

    private static PineValue GetTestFunction(string name) =>
        s_env.Value.Modules
        .First(module => module.moduleName is "KernelBytesPrecompiledLeavesTestModule")
        .moduleContent.FunctionDeclarations[name];

    private static Core.Interpreter.IntermediateVM.PineVM CreateVM(
        IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> precompiledLeaves) =>
        Core.Interpreter.IntermediateVM.PineVM.CreateCustom(
            evalCache: null,
            evaluationConfigDefault: null,
            reportFunctionApplication: _ => { },
            compilationEnvClasses: null,
            disableReductionInCompilation: true,
            selectPrecompiled: null,
            skipInlineForExpression: _ => false,
            enableTailRecursionOptimization: false,
            parseCache: null,
            precompiledLeaves: precompiledLeaves,
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null);

    [Fact]
    public void DecodeBlobAsCharsRec_leaf_short_circuits_recursion()
    {
        const string simple = "a";
        var complex = string.Concat(Enumerable.Repeat("ASCII-é-中-😀;", 24));

        AssertWorkShortCircuited(
            "decodeString",
            BytesValue(Encoding.UTF8.GetBytes(simple)),
            BytesValue(Encoding.UTF8.GetBytes(complex)),
            ElmValueEncoding.ElmValueAsPineValue(
                ElmValue.TagInstance("Just", [ElmValue.StringInstance(simple)])),
            ElmValueEncoding.ElmValueAsPineValue(
                ElmValue.TagInstance("Just", [ElmValue.StringInstance(complex)])));
    }

    [Fact]
    public void EncodeCharsAsBlobHelp_leaf_short_circuits_recursion()
    {
        const string simple = "a";
        var complex = string.Concat(Enumerable.Repeat("ASCII-é-中-😀;", 24));

        AssertWorkShortCircuited(
            "encodeString",
            ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance(simple)),
            ElmValueEncoding.ElmValueAsPineValue(ElmValue.StringInstance(complex)),
            BytesValue(Encoding.UTF8.GetBytes(simple)),
            BytesValue(Encoding.UTF8.GetBytes(complex)));
    }

    private static void AssertWorkShortCircuited(
        string functionName,
        PineValue simple,
        PineValue complex,
        PineValue expectedSimple,
        PineValue expectedComplex)
    {
        var function = GetTestFunction(functionName);

        var vmWithoutLeaves =
            CreateVM(ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty);

        var vmWithLeaves = CreateVM(KernelBytesPrecompiledLeaves.DefaultLeaves);

        var simpleNoLeaves = ApplyUnary(function, simple, vmWithoutLeaves);
        var complexNoLeaves = ApplyUnary(function, complex, vmWithoutLeaves);
        var simpleWithLeaves = ApplyUnary(function, simple, vmWithLeaves);
        var complexWithLeaves = ApplyUnary(function, complex, vmWithLeaves);

        simpleNoLeaves.result.Should().Be(expectedSimple);
        complexNoLeaves.result.Should().Be(expectedComplex);
        simpleWithLeaves.result.Should().Be(expectedSimple);
        complexWithLeaves.result.Should().Be(expectedComplex);

        (complexNoLeaves.counters.InvocationCount + complexNoLeaves.counters.LoopIterationCount)
            .Should().BeGreaterThan(
            simpleNoLeaves.counters.InvocationCount + simpleNoLeaves.counters.LoopIterationCount);

        complexNoLeaves.counters.InstructionCount
            .Should().BeGreaterThan(simpleNoLeaves.counters.InstructionCount);

        complexWithLeaves.counters.InvocationCount
            .Should().Be(simpleWithLeaves.counters.InvocationCount);

        complexWithLeaves.counters.LoopIterationCount
            .Should().Be(simpleWithLeaves.counters.LoopIterationCount);

        complexWithLeaves.counters.InstructionCount
            .Should().Be(simpleWithLeaves.counters.InstructionCount);
    }

    private static (PineValue result, PerformanceCounters counters) ApplyUnary(
        PineValue function,
        PineValue argument,
        Core.Interpreter.IntermediateVM.PineVM vm) =>
        CoreLibraryModule.CoreLibraryTestHelper.ApplyGenericPineWithProfiling(
            function,
            [argument],
            vm);

    private static PineValue BytesValue(byte[] bytes) =>
        ElmValueEncoding.ElmValueAsPineValue(new ElmValue.ElmBytes(bytes));
}
