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

public class StringPrecompiledLeavesEffectivenessTests
{
    private const string TestModuleText =
        """"
        module StringPrecompiledLeavesTestModule exposing (..)


        import String


        toList : String -> List Char
        toList =
            String.toList


        split : String -> List String
        split =
            String.split "::"


        lines : String -> List String
        lines =
            String.lines


        toFloat : String -> Maybe Float
        toFloat =
            String.toFloat


        toInt : String -> Maybe Int
        toInt =
            String.toInt


        fromInt : Int -> String
        fromInt =
            String.fromInt
        """"
        ;

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(
            () =>
            {
                var kernelModulesTree = BundledFiles.ElmKernelModulesDefault.Value;

                var treeWithTest =
                    kernelModulesTree.SetNodeAtPathSorted(
                        ["StringPrecompiledLeavesTestModule.elm"],
                        FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

                var rootFilePaths =
                    treeWithTest.EnumerateFilesTransitive()
                    .Where(
                        file =>
                        file.path[^1].Equals(
                            "StringPrecompiledLeavesTestModule.elm",
                            StringComparison.OrdinalIgnoreCase) ||
                        file.path[^1].Equals("String.elm", StringComparison.OrdinalIgnoreCase))
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
        .First(module => module.moduleName is "StringPrecompiledLeavesTestModule")
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
    public void String_toListRecursive_leaf_short_circuits_recursion()
    {
        var simple = "a";
        var complex = "abcdefghijklmnopqrstuvwxyz012345";

        AssertRecursionShortCircuited(
            "toList",
            simple,
            complex,
            ElmValue.ListInstance([.. simple.Select(character => ElmValue.CharInstance(character))]),
            ElmValue.ListInstance([.. complex.Select(character => ElmValue.CharInstance(character))]));
    }

    [Fact]
    public void String_splitHelperOnBlob_leaf_short_circuits_recursion()
    {
        var simple = "a::b";
        var complex = string.Join("::", Enumerable.Range(0, 24).Select(index => "part" + index));

        AssertRecursionShortCircuited(
            "split",
            simple,
            complex,
            StringList(simple.Split("::")),
            StringList(complex.Split("::")));
    }

    [Fact]
    public void String_linesHelper_leaf_short_circuits_recursion()
    {
        var simple = "a\nb";

        var complex =
            string.Join(
                "\r\n",
                Enumerable.Range(0, 24).Select(index => "line" + index));

        AssertRecursionShortCircuited(
            "lines",
            simple,
            complex,
            StringList(["a", "b"]),
            StringList(complex.Split("\r\n")));
    }

    [Fact]
    public void String_toFloat_leaf_short_circuits_parsing()
    {
        AssertWorkShortCircuited(
            "toFloat",
            ElmValue.StringInstance(".5"),
            ElmValue.StringInstance("1234567890.1234567890"),
            ElmValue.TagInstance(
                "Just",
                [ElmValue.ElmFloat.Normalized(5, 10)]),
            ElmValue.TagInstance(
                "Just",
                [
                    ElmValue.ElmFloat.Normalized(
                        System.Numerics.BigInteger.Parse("12345678901234567890"),
                        10_000_000_000),
                ]));
    }

    [Fact]
    public void String_toInt_leaf_short_circuits_parsing()
    {
        AssertWorkShortCircuited(
            "toInt",
            ElmValue.StringInstance("1"),
            ElmValue.StringInstance("123456789012345678901234567890"),
            ElmValue.TagInstance("Just", [ElmValue.Integer(1)]),
            ElmValue.TagInstance(
                "Just",
                [ElmValue.Integer(System.Numerics.BigInteger.Parse("123456789012345678901234567890"))]));
    }

    [Fact]
    public void String_fromInt_leaf_short_circuits_formatting()
    {
        AssertWorkShortCircuited(
            "fromInt",
            ElmValue.Integer(1),
            ElmValue.Integer(System.Numerics.BigInteger.Parse("123456789012345678901234567890")),
            ElmValue.StringInstance("1"),
            ElmValue.StringInstance("123456789012345678901234567890"));
    }

    private static void AssertRecursionShortCircuited(
        string functionName,
        string simple,
        string complex,
        ElmValue expectedSimple,
        ElmValue expectedComplex)
    {
        AssertWorkShortCircuited(
            functionName,
            ElmValue.StringInstance(simple),
            ElmValue.StringInstance(complex),
            expectedSimple,
            expectedComplex);
    }

    private static void AssertWorkShortCircuited(
        string functionName,
        ElmValue simple,
        ElmValue complex,
        ElmValue expectedSimple,
        ElmValue expectedComplex)
    {
        var function = GetTestFunction(functionName);

        var vmWithoutLeaves =
            CreateVM(ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty);

        var vmWithLeaves = CreateVM(IntermediateVM.SetupVM.DefaultPrecompiledLeaves);

        var simpleNoLeaves = ApplyUnary(function, simple, vmWithoutLeaves);
        var complexNoLeaves = ApplyUnary(function, complex, vmWithoutLeaves);
        var simpleWithLeaves = ApplyUnary(function, simple, vmWithLeaves);
        var complexWithLeaves = ApplyUnary(function, complex, vmWithLeaves);

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

    private static (ElmValue value, PerformanceCounters counters) ApplyUnary(
        PineValue function,
        ElmValue argument,
        Core.Interpreter.IntermediateVM.PineVM vm) =>
        CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
            function,
            argument,
            vm);

    private static ElmValue StringList(IEnumerable<string> strings) =>
        ElmValue.ListInstance([.. strings.Select(ElmValue.StringInstance)]);
}
