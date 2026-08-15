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


        dropWhileEmpty : List String -> List String
        dropWhileEmpty =
            LanguageService.dropWhileEmpty


        sliceRangeFromTextLines : List String -> ( Int, Int ) -> ( Int, Int ) -> List String
        sliceRangeFromTextLines textLines start end =
            LanguageService.sliceRangeFromTextLines textLines (LanguageService.Range start end)
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

    [Fact]
    public void LanguageService_dropWhileEmpty_leaf_short_circuits_recursion()
    {
        AssertWorkShortCircuited(
            "dropWhileEmpty",
            [StringList(["", "line"])],
            [StringList([.. Enumerable.Repeat("", 32), "line"])],
            StringList(["line"]),
            StringList(["line"]));
    }

    [Fact]
    public void LanguageService_sliceRangeFromTextLines_leaf_short_circuits_single_line_work()
    {
        AssertWorkReduced(
            "sliceRangeFromTextLines",
            [
            StringList(["prefix", "selected text"]),
            Position(2, 2),
            Position(2, 10),
            ],
            [
            StringList([.. Enumerable.Repeat("prefix", 32), "selected text"]),
            Position(33, 2),
            Position(33, 10),
            ],
            StringList(["elected "]),
            StringList(["elected "]));
    }

    [Fact]
    public void LanguageService_sliceRangeFromTextLines_leaf_short_circuits_multiline_work()
    {
        AssertWorkReduced(
            "sliceRangeFromTextLines",
            [
            StringList(["prefix", "first line", "middle line", "last line"]),
            Position(2, 2),
            Position(4, 5),
            ],
            [
            StringList(
                [
                .. Enumerable.Repeat("prefix", 32),
                "first line",
                "middle line",
                "last line",
                ]),
            Position(33, 2),
            Position(35, 5),
            ],
            StringList(["irst line", "middle line", "last"]),
            StringList(["irst line", "middle line", "last"]));
    }

    [Fact]
    public void LanguageService_leaves_do_not_allocate_proportional_to_skipped_prefixes()
    {
        var enteredLeafEnvironments = new Dictionary<PineValue, PineValue>();

        var vm =
            CreateVM(
                IntermediateVM.SetupVM.DefaultPrecompiledLeaves,
                (leaf, environment) => enteredLeafEnvironments.TryAdd(leaf, environment));

        _ =
            Apply(
                GetTestFunction("dropWhileEmpty"),
                [StringList(["", "line"])],
                vm);

        _ =
            Apply(
                GetTestFunction("sliceRangeFromTextLines"),
                [StringList(["prefix", "selected"]), Position(2, 1), Position(2, 9)],
                vm);

        var dropEnvironment =
            (PineValue.ListValue)enteredLeafEnvironments[
                LanguageServicePrecompiledLeaves.DropWhileEmptyLeafKey];

        AssertAllocationDoesNotGrowWithPrefix(
            LanguageServicePrecompiledLeaves.DropWhileEmptyLeafDelegate,
            EnvironmentWithArgument(dropEnvironment, 1, StringList(["", "line"])),
            EnvironmentWithArgument(
                dropEnvironment,
                1,
                StringList([.. Enumerable.Repeat("", 10_000), "line"])));

        var sliceEnvironment =
            (PineValue.ListValue)enteredLeafEnvironments[
                LanguageServicePrecompiledLeaves.SliceRangeFromTextLinesLeafKey];

        AssertAllocationDoesNotGrowWithPrefix(
            LanguageServicePrecompiledLeaves.SliceRangeFromTextLinesLeafDelegate,
            EnvironmentWithArguments(
                sliceEnvironment,
                (1, StringList(["prefix", "selected"])),
                (2, Range(Position(2, 1), Position(2, 9)))),
            EnvironmentWithArguments(
                sliceEnvironment,
                (1, StringList([.. Enumerable.Repeat("prefix", 10_000), "selected"])),
                (2, Range(Position(10_001, 1), Position(10_001, 9)))));
    }

    private static ElmInteractiveEnvironment.ParsedInteractiveEnvironment BuildEnvironment()
    {
        var compilerSources = BundledFiles.CompilerSourceContainerFilesDefault.Value;
        var mergedTree = BundledFiles.ElmKernelModulesDefault.Value;

        foreach (var sourcePath in new[]
        {
            new[] { "pine-elm-syntax", "src" },
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
        IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> precompiledLeaves,
        Action<PineValue, PineValue>? reportEnterPrecompiledLeaf = null) =>
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
            reportEnterPrecompiledLeaf,
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
        AssertWorkShortCircuited(
            functionName,
            [ElmValue.StringInstance(simple)],
            [ElmValue.StringInstance(complex)],
            ElmValue.StringInstance(expectedSimple),
            ElmValue.StringInstance(expectedComplex));
    }

    private static void AssertWorkShortCircuited(
        string functionName,
        ElmValue[] simple,
        ElmValue[] complex,
        ElmValue expectedSimple,
        ElmValue expectedComplex)
    {
        var function = GetTestFunction(functionName);

        var vmWithoutLeaves =
            CreateVM(ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty);

        var vmWithLeaves = CreateVM(IntermediateVM.SetupVM.DefaultPrecompiledLeaves);

        var simpleNoLeaves = Apply(function, simple, vmWithoutLeaves);
        var complexNoLeaves = Apply(function, complex, vmWithoutLeaves);
        var simpleWithLeaves = Apply(function, simple, vmWithLeaves);
        var complexWithLeaves = Apply(function, complex, vmWithLeaves);

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

    private static void AssertWorkReduced(
        string functionName,
        ElmValue[] simple,
        ElmValue[] complex,
        ElmValue expectedSimple,
        ElmValue expectedComplex)
    {
        var function = GetTestFunction(functionName);

        var vmWithoutLeaves =
            CreateVM(ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty);

        var vmWithLeaves = CreateVM(IntermediateVM.SetupVM.DefaultPrecompiledLeaves);

        var simpleNoLeaves = Apply(function, simple, vmWithoutLeaves);
        var complexNoLeaves = Apply(function, complex, vmWithoutLeaves);
        var simpleWithLeaves = Apply(function, simple, vmWithLeaves);
        var complexWithLeaves = Apply(function, complex, vmWithLeaves);

        simpleNoLeaves.value.Should().Be(expectedSimple);
        complexNoLeaves.value.Should().Be(expectedComplex);
        simpleWithLeaves.value.Should().Be(expectedSimple);
        complexWithLeaves.value.Should().Be(expectedComplex);

        simpleWithLeaves.counters.InstructionCount
            .Should().BeLessThan(simpleNoLeaves.counters.InstructionCount);

        complexWithLeaves.counters.InstructionCount
            .Should().BeLessThan(complexNoLeaves.counters.InstructionCount);
    }

    private static (ElmValue value, PerformanceCounters counters) Apply(
        PineValue function,
        ElmValue[] arguments,
        Core.Interpreter.IntermediateVM.PineVM vm) =>
        CoreLibraryModule.CoreLibraryTestHelper.ApplyGenericWithProfiling(
            function,
            arguments,
            vm);

    private static ElmValue StringList(IEnumerable<string> strings) =>
        ElmValue.ListInstance([.. strings.Select(ElmValue.StringInstance)]);

    private static ElmValue Position(int row, int column) =>
        ElmValue.TupleInstance(ElmValue.Integer(row), ElmValue.Integer(column));

    private static ElmValue Range(ElmValue start, ElmValue end) =>
        ElmValue.TagInstance("Range", [start, end]);

    private static PineValue EnvironmentWithArgument(
        PineValue.ListValue environment,
        int index,
        ElmValue argument) =>
        EnvironmentWithArguments(environment, (index, argument));

    private static PineValue EnvironmentWithArguments(
        PineValue.ListValue environment,
        params (int index, ElmValue argument)[] replacements)
    {
        var items = environment.Items.ToArray();

        foreach (var (index, argument) in replacements)
        {
            items[index] = ElmValueEncoding.ElmValueAsPineValue(argument);
        }

        return PineValue.List(items);
    }

    private static void AssertAllocationDoesNotGrowWithPrefix(
        Func<PineValue, PineValue?> leaf,
        PineValue shortEnvironment,
        PineValue longEnvironment)
    {
        leaf(shortEnvironment).Should().Be(leaf(longEnvironment));

        const int invocationCount = 100;
        const int measurementAttemptCount = 3;

        long shortAllocatedBytes = 0;
        long longAllocatedBytes = 0;

        for (var attempt = 0; attempt < measurementAttemptCount; ++attempt)
        {
            var shortAllocatedBefore = GC.GetAllocatedBytesForCurrentThread();

            for (var invocation = 0; invocation < invocationCount; ++invocation)
            {
                _ = leaf(shortEnvironment);
            }

            shortAllocatedBytes =
                GC.GetAllocatedBytesForCurrentThread() - shortAllocatedBefore;

            var longAllocatedBefore = GC.GetAllocatedBytesForCurrentThread();

            for (var invocation = 0; invocation < invocationCount; ++invocation)
            {
                _ = leaf(longEnvironment);
            }

            longAllocatedBytes =
                GC.GetAllocatedBytesForCurrentThread() - longAllocatedBefore;

            if (longAllocatedBytes <= shortAllocatedBytes + 1_024)
            {
                return;
            }
        }

        longAllocatedBytes.Should().BeLessThanOrEqualTo(shortAllocatedBytes + 1_024);
    }
}
