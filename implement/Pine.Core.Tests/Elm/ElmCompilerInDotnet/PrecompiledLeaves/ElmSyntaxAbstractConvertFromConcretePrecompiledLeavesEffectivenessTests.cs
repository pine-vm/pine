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

public class ElmSyntaxAbstractConvertFromConcretePrecompiledLeavesEffectivenessTests
{
    private const string TestModuleText =
        """"
        module ElmSyntaxAbstractConvertFromConcretePrecompiledLeavesTestModule exposing (exercise)

        import ElmSyntax.Abstract.ConvertFromConcrete as ConvertFromConcrete
        import ElmSyntax.Abstract.Expression as AbstractExpression
        import ElmSyntax.Concrete.Parser.FromString as FromString


        exercise _ =
            case FromString.parseExpression
                "{ z = 0, y = 1, x = 2, w = 3, v = 4, u = 5, t = 6, s = 7, r = 8, q = 9, p = 10, o = 11, n = 12, m = 13, l = 14, k = 15, j = 16, i = 17, h = 18, g = 19, f = 20, e = 21, d = 22, c = 23, b = 24, a = 25 }"
            of
                Ok expression ->
                    ConvertFromConcrete.fromExpression expression

                Err _ ->
                    AbstractExpression.UnitExpr
        """"
        ;

    private static readonly Lazy<PineValue> s_exerciseFunction =
        new(BuildExerciseFunction);

    [Fact]
    public void Merge_record_setters_leaf_preserves_results_and_reduces_VM_work()
    {
        var enteredLeaves = new HashSet<PineValue>();

        var vmWithoutLeaves =
            CreateVM(
                ImmutableDictionary<PineValue, Func<PineValue, PineValue?>>.Empty,
                null);

        var vmWithLeaves =
            CreateVM(
                IntermediateVM.SetupVM.ConvertFromConcretePrecompiledLeaves,
                (leaf, _) => enteredLeaves.Add(leaf));

        var withoutLeaves = Apply(vmWithoutLeaves);
        var withLeaves = Apply(vmWithLeaves);

        ElmValue.RenderAsElmExpression(withLeaves.value).expressionString
            .Should().Be(ElmValue.RenderAsElmExpression(withoutLeaves.value).expressionString);

        withLeaves.counters.InstructionCount.Should().BeLessThan(withoutLeaves.counters.InstructionCount);
        withLeaves.counters.InvocationCount.Should().BeLessThan(withoutLeaves.counters.InvocationCount);

        enteredLeaves.Should().Contain(
            ElmSyntaxAbstractConvertFromConcretePrecompiledLeaves.MergeRecordSettersLeafKey);
    }

    private static PineValue BuildExerciseFunction()
    {
        var mergedTree = BundledFiles.ElmKernelModulesDefault.Value;
        var compilerSourceTree = BundledFiles.CompilerSourceContainerFilesDefault.Value;

        var elmSyntaxSourceTree =
            compilerSourceTree.GetNodeAtPath(["pine-elm-syntax", "src"])
            ?? throw new Exception("Did not find pine-elm-syntax/src");

        foreach (var (path, file) in elmSyntaxSourceTree.EnumerateFilesTransitive())
        {
            mergedTree = mergedTree.SetNodeAtPathSorted(path, FileTree.File(file));
        }

        mergedTree =
            mergedTree.SetNodeAtPathSorted(
                ["ElmSyntaxAbstractConvertFromConcretePrecompiledLeavesTestModule.elm"],
                FileTree.File(Encoding.UTF8.GetBytes(TestModuleText)));

        var compiledEnv =
            ElmCompiler.CompileInteractiveEnvironment(
                mergedTree,
                rootFilePaths: [["ElmSyntaxAbstractConvertFromConcretePrecompiledLeavesTestModule.elm"]])
            .Map(result => result.compiledEnvValue)
            .Extract(error => throw new Exception("Failed compiling: " + error));

        var parsedEnv =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiledEnv)
            .Extract(error => throw new Exception("Failed parsing: " + error));

        return
            parsedEnv.Modules
            .First(module => module.moduleName is "ElmSyntaxAbstractConvertFromConcretePrecompiledLeavesTestModule")
            .moduleContent.FunctionDeclarations["exercise"];
    }

    private static Core.Interpreter.IntermediateVM.PineVM CreateVM(
        IReadOnlyDictionary<PineValue, Func<PineValue, PineValue?>> precompiledLeaves,
        Action<PineValue, PineValue>? reportEnterPrecompiledLeaf) =>
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
            reportEnterPrecompiledLeaf: reportEnterPrecompiledLeaf,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null);

    private static (ElmValue value, PerformanceCounters counters) Apply(
        Core.Interpreter.IntermediateVM.PineVM vm) =>
        CoreLibraryModule.CoreLibraryTestHelper.ApplyAndProfileUnary(
            s_exerciseFunction.Value,
            ElmValue.Integer(0),
            vm);
}
