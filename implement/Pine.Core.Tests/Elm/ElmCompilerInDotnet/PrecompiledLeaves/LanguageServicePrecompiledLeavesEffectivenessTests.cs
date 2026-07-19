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

public class LanguageServicePrecompiledLeavesEffectivenessTests
{
    private const string TestModuleText =
        """"
        module LanguageServicePrecompiledLeavesTestModule exposing (..)


        import LanguageService
        import String


        trimLeft : String -> String
        trimLeft =
            String.trimLeft


        trimRight : String -> String
        trimRight =
            String.trimRight


        removeWrappingFromMultilineComment : String -> String
        removeWrappingFromMultilineComment =
            LanguageService.removeWrappingFromMultilineComment
        """"
        ;

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_env =
        new(BuildEnvironment);

    [Fact]
    public void String_trimLeftCountBytesTrimmed_leaf_short_circuits_recursion()
    {
        AssertWorkShortCircuited(
            "trimLeft",
            " a",
            new string(' ', 32) + "a",
            "a");
    }

    [Fact]
    public void String_trimRightCountBytesRemaining_leaf_short_circuits_recursion()
    {
        AssertWorkShortCircuited(
            "trimRight",
            "a ",
            "a" + new string('\t', 32),
            "a");
    }

    [Fact]
    public void LanguageService_removeWrappingFromMultilineComment_leaf_short_circuits_work()
    {
        AssertWorkShortCircuited(
            "removeWrappingFromMultilineComment",
            "{- a -}",
            "{-|" + new string(' ', 32) + "🙂comment" + new string('\u00A0', 32) + "-}",
            "a",
            "🙂comment");
    }

    private static ElmInteractiveEnvironment.ParsedInteractiveEnvironment BuildEnvironment()
    {
        var compilerSources = BundledFiles.CompilerSourceContainerFilesDefault.Value;
        var mergedTree = BundledFiles.ElmKernelModulesDefault.Value;

        foreach (var sourcePath in new[]
        {
            new[] { "elm-syntax", "src" },
            ["src"],
            ["other-library-modules"],
        })
        {
            if (compilerSources.GetNodeAtPath(sourcePath) is not { } sourceTree)
            {
                continue;
            }

            foreach (var (path, file) in sourceTree.EnumerateFilesTransitive())
            {
                mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
            }
        }

        var treeWithTest =
            mergedTree.SetNodeAtPathSorted(
                ["LanguageServicePrecompiledLeavesTestModule.elm"],
                FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

        var rootFilePaths =
            treeWithTest.EnumerateFilesTransitive()
            .Where(
                file =>
                file.path[^1].Equals(
                    "LanguageServicePrecompiledLeavesTestModule.elm",
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
    }

    private static PineValue GetTestFunction(string name) =>
        s_env.Value.Modules
        .First(module => module.moduleName is "LanguageServicePrecompiledLeavesTestModule")
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

    private static void AssertWorkShortCircuited(
        string functionName,
        string simple,
        string complex,
        string expected) =>
        AssertWorkShortCircuited(functionName, simple, complex, expected, expected);

    private static void AssertWorkShortCircuited(
        string functionName,
        string simple,
        string complex,
        string expectedSimple,
        string expectedComplex)
    {
        var function = GetTestFunction(functionName);
        var vmWithoutLeaves =
            CreateVM(ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty);
        var vmWithLeaves = CreateVM(IntermediateVM.SetupVM.DefaultPrecompiledLeaves);

        var simpleNoLeaves = ApplyUnary(function, simple, vmWithoutLeaves);
        var complexNoLeaves = ApplyUnary(function, complex, vmWithoutLeaves);
        var simpleWithLeaves = ApplyUnary(function, simple, vmWithLeaves);
        var complexWithLeaves = ApplyUnary(function, complex, vmWithLeaves);

        simpleNoLeaves.value.Should().Be(ElmValue.StringInstance(expectedSimple));
        complexNoLeaves.value.Should().Be(ElmValue.StringInstance(expectedComplex));
        simpleWithLeaves.value.Should().Be(ElmValue.StringInstance(expectedSimple));
        complexWithLeaves.value.Should().Be(ElmValue.StringInstance(expectedComplex));

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
        string argument,
        Core.Interpreter.IntermediateVM.PineVM vm) =>
        CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
            function,
            ElmValue.StringInstance(argument),
            vm);
}
