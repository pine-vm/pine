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

public class DictPrecompiledLeavesEffectivenessTests
{
    private const string TestModuleText =
        """"
        module DictPrecompiledLeavesTestModule exposing (..)


        import Dict exposing (Dict)


        dictGet : Int -> Dict Int Int -> Maybe Int
        dictGet =
            Dict.get


        dictToList : Dict Int Int -> List ( Int, Int )
        dictToList =
            Dict.toList


        dictSize : Dict Int Int -> Int
        dictSize =
            Dict.size


        dictKeys : Dict Int Int -> List Int
        dictKeys =
            Dict.keys


        dictValues : Dict Int Int -> List Int
        dictValues =
            Dict.values


        dictInsert : Int -> Int -> Dict Int Int -> Dict Int Int
        dictInsert =
            Dict.insert
        """"
        ;

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(
            () =>
            {
                var kernelModulesTree = BundledFiles.ElmKernelModulesDefault.Value;

                var treeWithTest =
                    kernelModulesTree.SetNodeAtPathSorted(
                        ["DictPrecompiledLeavesTestModule.elm"],
                        FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

                var rootFilePaths =
                    treeWithTest.EnumerateFilesTransitive()
                    .Where(
                        file =>
                        file.path[^1].Equals(
                            "DictPrecompiledLeavesTestModule.elm",
                            StringComparison.OrdinalIgnoreCase) ||
                        file.path[^1].Equals("Dict.elm", StringComparison.OrdinalIgnoreCase))
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
        .First(module => module.moduleName is "DictPrecompiledLeavesTestModule")
        .moduleContent.FunctionDeclarations[name];

    private static PineValue GetDictFunction(string name) =>
        s_env.Value.Modules
        .First(module => module.moduleName is "Dict")
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

    private static ElmValue BuildIntIntDict(
        IEnumerable<(long key, long value)> pairs,
        Core.Interpreter.IntermediateVM.PineVM vm)
    {
        var tupleList =
            ElmValue.ListInstance(
                [
                .. pairs.Select(
                    pair =>
                    ElmValue.ListInstance(
                        [ElmValue.Integer(pair.key), ElmValue.Integer(pair.value)])),
                ]);

        return
            CoreLibraryModule.CoreLibraryTestHelper.ApplyUnary(
                GetDictFunction("fromList"),
                tupleList,
                vm);
    }

    private static (ElmValue value, PerformanceCounters counters) ApplyUnary(
        string functionName,
        ElmValue argument,
        Core.Interpreter.IntermediateVM.PineVM vm) =>
        CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
            GetTestFunction(functionName),
            argument,
            vm);

    private static void AssertRecursionShortCircuited(
        (ElmValue value, PerformanceCounters counters) simpleNoLeaves,
        (ElmValue value, PerformanceCounters counters) complexNoLeaves,
        (ElmValue value, PerformanceCounters counters) simpleWithLeaves,
        (ElmValue value, PerformanceCounters counters) complexWithLeaves,
        ElmValue expectedSimple,
        ElmValue expectedComplex)
    {
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

    [Fact]
    public void Dict_get_leaf_short_circuits_recursion_in_get()
    {
        var vmWithoutLeaves =
            CreateVM(ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty);

        var vmWithLeaves = CreateVM(IntermediateVM.SetupVM.DefaultPrecompiledLeaves);
        var dict = BuildIntIntDict(IntPairs(31), vmWithoutLeaves);
        var simpleKey = ElmValue.Integer(15);
        var complexKey = ElmValue.Integer(0);
        var function = GetTestFunction("dictGet");

        AssertRecursionShortCircuited(
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileBinary(
                function,
                simpleKey,
                dict,
                vmWithoutLeaves),
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileBinary(
                function,
                complexKey,
                dict,
                vmWithoutLeaves),
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileBinary(
                function,
                simpleKey,
                dict,
                vmWithLeaves),
            CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileBinary(
                function,
                complexKey,
                dict,
                vmWithLeaves),
            ElmValue.TagInstance("Just", [ElmValue.Integer(150)]),
            ElmValue.TagInstance("Just", [ElmValue.Integer(0)]));
    }

    [Theory]
    [InlineData("dictToList")]
    [InlineData("dictKeys")]
    [InlineData("dictValues")]
    public void Dict_list_conversion_leaf_short_circuits_recursion(string functionName)
    {
        var vmWithoutLeaves =
            CreateVM(ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty);

        var vmWithLeaves = CreateVM(IntermediateVM.SetupVM.DefaultPrecompiledLeaves);
        var simpleDict = BuildIntIntDict(IntPairs(1), vmWithoutLeaves);
        var complexDict = BuildIntIntDict(IntPairs(31), vmWithoutLeaves);

        AssertRecursionShortCircuited(
            ApplyUnary(functionName, simpleDict, vmWithoutLeaves),
            ApplyUnary(functionName, complexDict, vmWithoutLeaves),
            ApplyUnary(functionName, simpleDict, vmWithLeaves),
            ApplyUnary(functionName, complexDict, vmWithLeaves),
            ExpectedListResult(functionName, 1),
            ExpectedListResult(functionName, 31));
    }

    [Fact]
    public void Dict_size_leaf_short_circuits_recursion()
    {
        var vmWithoutLeaves =
            CreateVM(ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty);

        var vmWithLeaves = CreateVM(IntermediateVM.SetupVM.DefaultPrecompiledLeaves);
        var simpleDict = BuildIntIntDict(IntPairs(1), vmWithoutLeaves);
        var complexDict = BuildIntIntDict(IntPairs(31), vmWithoutLeaves);

        AssertRecursionShortCircuited(
            ApplyUnary("dictSize", simpleDict, vmWithoutLeaves),
            ApplyUnary("dictSize", complexDict, vmWithoutLeaves),
            ApplyUnary("dictSize", simpleDict, vmWithLeaves),
            ApplyUnary("dictSize", complexDict, vmWithLeaves),
            ElmValue.Integer(1),
            ElmValue.Integer(31));
    }

    [Fact]
    public void Dict_insert_leaf_short_circuits_recursion()
    {
        var vmWithoutLeaves =
            CreateVM(ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty);

        var vmWithLeaves = CreateVM(IntermediateVM.SetupVM.DefaultPrecompiledLeaves);
        var simpleDict = BuildIntIntDict([], vmWithoutLeaves);
        var complexDict = BuildIntIntDict(IntPairs(31), vmWithoutLeaves);
        var function = GetTestFunction("dictInsert");

        var simpleArguments =
            new[] { ElmValue.Integer(0), ElmValue.Integer(999), simpleDict };

        var complexArguments =
            new[] { ElmValue.Integer(31), ElmValue.Integer(999), complexDict };

        AssertRecursionShortCircuited(
            CoreLibraryModule.CoreLibraryTestHelper.ApplyGenericWithProfiling(
                function,
                simpleArguments,
                vmWithoutLeaves),
            CoreLibraryModule.CoreLibraryTestHelper.ApplyGenericWithProfiling(
                function,
                complexArguments,
                vmWithoutLeaves),
            CoreLibraryModule.CoreLibraryTestHelper.ApplyGenericWithProfiling(
                function,
                simpleArguments,
                vmWithLeaves),
            CoreLibraryModule.CoreLibraryTestHelper.ApplyGenericWithProfiling(
                function,
                complexArguments,
                vmWithLeaves),
            BuildIntIntDict([(0, 999)], vmWithoutLeaves),
            BuildIntIntDict(IntPairs(31).Append((31, 999)), vmWithoutLeaves));
    }

    private static IEnumerable<(long key, long value)> IntPairs(int count) =>
        Enumerable.Range(0, count).Select(index => ((long)index, (long)(index * 10)));

    private static ElmValue ExpectedListResult(string functionName, int count) =>
        ElmValue.ListInstance(
            [
            .. Enumerable.Range(0, count)
            .Select(
                index =>
                functionName switch
                {
                    "dictToList" =>
                    ElmValue.ListInstance(
                        [ElmValue.Integer(index), ElmValue.Integer(index * 10)]),

                    "dictKeys" => ElmValue.Integer(index),
                    "dictValues" => ElmValue.Integer(index * 10),

                    _ =>
                    throw new NotImplementedException(functionName),
                }),
            ]);
}
