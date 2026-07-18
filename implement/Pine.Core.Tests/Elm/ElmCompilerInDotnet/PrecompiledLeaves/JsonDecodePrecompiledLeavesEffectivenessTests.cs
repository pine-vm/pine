using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet;
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

public class JsonDecodePrecompiledLeavesEffectivenessTests
{
    private const string TestModuleText =
        """"
        module JsonDecodePrecompiledLeavesTestModule exposing (..)


        import Json.Decode


        parseJson =
            Json.Decode.parseJsonStringToValue
        """"
        ;

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(
            () =>
            {
                var kernelModulesTree = BundledFiles.ElmKernelModulesDefault.Value;

                var treeWithTest =
                    kernelModulesTree.SetNodeAtPathSorted(
                        ["JsonDecodePrecompiledLeavesTestModule.elm"],
                        FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

                var rootFilePaths =
                    treeWithTest.EnumerateFilesTransitive()
                    .Where(
                        file =>
                        file.path[^1].Equals(
                            "JsonDecodePrecompiledLeavesTestModule.elm",
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

    private static PineValue ParseJsonFunction =>
        s_env.Value.Modules
        .First(module => module.moduleName is "JsonDecodePrecompiledLeavesTestModule")
        .moduleContent.FunctionDeclarations["parseJson"];

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
    public void ParseValue_leaf_short_circuits_nested_array_recursion()
    {
        const int NestingDepth = 16;
        var complexJson = new string('[', NestingDepth) + "null" + new string(']', NestingDepth);

        var complexValue = JsonNullValue;

        for (var index = 0; index < NestingDepth; ++index)
        {
            complexValue = JsonArrayValue([complexValue]);
        }

        AssertRecursionShortCircuited(
            "[]",
            complexJson,
            Ok(JsonArrayValue([])),
            Ok(complexValue));
    }

    [Fact]
    public void ParseValue_leaf_short_circuits_object_field_recursion()
    {
        var complexFields =
            Enumerable.Range(0, 20)
            .Select(index => ($"field{index}", JsonIntValue(index)))
            .ToList();

        var complexJson =
            "{" +
            string.Join(",", Enumerable.Range(0, 20).Select(index => $"\"field{index}\":{index}")) +
            "}";

        AssertRecursionShortCircuited(
            """{"field":0}""",
            complexJson,
            Ok(JsonObjectValue([("field", JsonIntValue(0))])),
            Ok(JsonObjectValue(complexFields)));
    }

    private static void AssertRecursionShortCircuited(
        string simpleJson,
        string complexJson,
        ElmValue expectedSimple,
        ElmValue expectedComplex)
    {
        var vmWithoutLeaves =
            CreateVM(ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty);

        var vmWithLeaves = CreateVM(IntermediateVM.SetupVM.DefaultPrecompiledLeaves);

        var simpleNoLeaves = Apply(simpleJson, vmWithoutLeaves);
        var complexNoLeaves = Apply(complexJson, vmWithoutLeaves);
        var simpleWithLeaves = Apply(simpleJson, vmWithLeaves);
        var complexWithLeaves = Apply(complexJson, vmWithLeaves);

        simpleNoLeaves.value.Should().Be(expectedSimple);
        complexNoLeaves.value.Should().Be(expectedComplex);
        simpleWithLeaves.value.Should().Be(expectedSimple);
        complexWithLeaves.value.Should().Be(expectedComplex);

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

    private static (ElmValue value, PerformanceCounters counters) Apply(
        string json,
        Core.Interpreter.IntermediateVM.PineVM vm) =>
        CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
            ParseJsonFunction,
            ElmValue.StringInstance(json),
            vm);

    private static ElmValue Ok(ElmValue value) =>
        ElmValue.TagInstance("Ok", [value]);

    private static ElmValue JsonNullValue =>
        ElmValue.TagInstance("NullValue", []);

    private static ElmValue JsonIntValue(long value) =>
        ElmValue.TagInstance("IntValue", [ElmValue.Integer(value)]);

    private static ElmValue JsonArrayValue(IReadOnlyList<ElmValue> values) =>
        ElmValue.TagInstance("ArrayValue", [ElmValue.ListInstance(values)]);

    private static ElmValue JsonObjectValue(IReadOnlyList<(string name, ElmValue value)> fields) =>
        ElmValue.TagInstance(
            "ObjectValue",
            [
                ElmValue.ListInstance(
                    [
                    .. fields.Select(
                        field =>
                        ElmValue.ListInstance(
                            [ElmValue.StringInstance(field.name), field.value]))
                    ]),
            ]);
}
