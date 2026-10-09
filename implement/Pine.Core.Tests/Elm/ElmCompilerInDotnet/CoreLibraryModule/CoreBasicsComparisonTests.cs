using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Elm.ElmCompilerInDotnet.CoreLibraryModule;
using Pine.Core.Elm.ElmCompilerInDotnet.PrecompiledLeaves;
using Pine.Core.Elm.ElmInElm;
using Pine.Core.Files;
using Pine.Core.Internal;
using Pine.Core.Interpreter;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using Xunit;

using IntermediatePineVM = Pine.Core.Interpreter.IntermediateVM.PineVM;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.CoreLibraryModule;

public class CoreBasicsComparisonTests
{
    public static IEnumerable<object[]> ComparableLists()
    {
        yield return [Strings("A"), Strings("B"), "LT"];
        yield return [Strings("A", "B"), Strings("A", "C"), "LT"];
        yield return [Strings("A"), Strings("A", "B"), "LT"];
        yield return [Strings(), Strings("A"), "LT"];
        yield return [Strings("A"), Strings("A"), "EQ"];
        yield return [Strings(), Strings(), "EQ"];

        yield return [
            ElmValue.ListInstance([Strings("A")]),
            ElmValue.ListInstance([Strings("B")]),
            "LT"];

        yield return [
            ElmValue.ListInstance([ElmValue.ListInstance([ElmValue.Integer(1), ElmValue.StringInstance("A")])]),
            ElmValue.ListInstance([ElmValue.ListInstance([ElmValue.Integer(1), ElmValue.StringInstance("B")])]),
            "LT"];

        yield return [
            ElmValue.ListInstance([ElmValue.ElmFloat.Normalized(1, 2)]),
            ElmValue.ListInstance([ElmValue.ElmFloat.Normalized(3, 4)]),
            "LT"];

        yield return [
            ElmValue.ListInstance([ElmValue.ElmFloat.NotNormalized(2, 2), ElmValue.Integer(1)]),
            ElmValue.ListInstance([ElmValue.Integer(1), ElmValue.Integer(2)]),
            "LT"];

        yield return [
            ElmValue.ListInstance([ElmValue.Integer(-2), ElmValue.Integer(1)]),
            ElmValue.ListInstance([ElmValue.Integer(-2), ElmValue.Integer(2)]),
            "LT"];
    }

    [Theory]
    [MemberData(nameof(ComparableLists))]
    public void Generic_compare_lists_agrees_with_native_leaf_and_direct_interpreter(
        ElmValue left,
        ElmValue right,
        string expectedOrder)
    {
        var expected = ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance(expectedOrder, []));
        var reversedOrder = expectedOrder == "LT" ? "GT" : expectedOrder;
        var reversed = ElmValueEncoding.ElmValueAsPineValue(ElmValue.TagInstance(reversedOrder, []));
        var leftValue = ElmValueEncoding.ElmValueAsPineValue(left);
        var rightValue = ElmValueEncoding.ElmValueAsPineValue(right);

        AssertComparison(leftValue, rightValue, expected);
        AssertComparison(rightValue, leftValue, reversed);
    }

    private static void AssertComparison(PineValue left, PineValue right, PineValue expected)
    {
        var body = CoreBasics.Compare_InnerBodyExpression();
        var environment = PineValue.List([PineValue.EmptyList, left, right]);

        DirectInterpreter.WithoutEvalCaching(new PineVMParseCache())
            .EvaluateExpressionDefault(body, environment)
            .Should().Be(expected);

        CoreBasicsPrecompiledLeaves.CompareLeafDelegate(PineValueInProcess.Create(environment))!
            .Evaluate().Should().Be(expected);

        var invocation =
            new Expression.Eval(
                Expression.LitralInst(CoreBasics.Compare_InnerBodyEncodedValue),
                Expression.EnvironmentInstance);

        foreach (var withLeaves in new[] { false, true })
        {
            foreach (var inline in new[] { false, true })
            {
                CreateVM(withLeaves, inline)
                    .EvaluateExpression(invocation, environment)
                    .Extract(error => throw new Exception(error))
                    .Should().Be(expected);
            }
        }
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Generic_compare_equal_numbers_preserves_relational_operators(bool withLeaves, bool inline)
    {
        var vm = CreateVM(withLeaves, inline);
        var leftNumber = ElmValue.ElmFloat.NotNormalized(2, 2);
        var rightNumber = ElmValue.Integer(1);
        var trueValue = ElmValue.TagInstance("True", []);
        var falseValue = ElmValue.TagInstance("False", []);

        foreach (var (left, right) in new (ElmValue, ElmValue)[]
        {
            (leftNumber, rightNumber),
            (ElmValue.ListInstance([leftNumber]), ElmValue.ListInstance([rightNumber])),
        })
        {
            foreach (var (first, second) in new[] { (left, right), (right, left) })
            {
                CoreLibraryTestHelper.ApplyBinary(CoreBasics.Le_FunctionValue(), first, second, vm)
                    .Should().Be(trueValue);

                CoreLibraryTestHelper.ApplyBinary(CoreBasics.Ge_FunctionValue(), first, second, vm)
                    .Should().Be(trueValue);

                CoreLibraryTestHelper.ApplyBinary(CoreBasics.Lt_FunctionValue(), first, second, vm)
                    .Should().Be(falseValue);

                CoreLibraryTestHelper.ApplyBinary(CoreBasics.Gt_FunctionValue(), first, second, vm)
                    .Should().Be(falseValue);
            }
        }
    }

    [Theory]
    [InlineData(false, false)]
    [InlineData(false, true)]
    [InlineData(true, false)]
    [InlineData(true, true)]
    public void Generic_compare_lists_preserves_module_key_dictionary_lookup(bool withLeaves, bool inline)
    {
        var vm = CreateVM(withLeaves, inline);

        var function =
            s_environment.Value.Modules.Single(module => module.moduleName == "ModuleKeyRegression")
                .moduleContent.FunctionDeclarations["lookupModules"];

        foreach (var names in new[]
        {
            new[] { "A", "B", "Broken", "Main" },
            new[] { "Main", "Broken", "B", "A" },
        })
        {
            CoreLibraryTestHelper.ApplyUnary(function, Strings(names), vm).Should().Be(
                ElmValue.ListInstance(
                    [
                    ElmValue.TagInstance("Just", [ElmValue.Integer(1)]),
                    ElmValue.TagInstance("Just", [ElmValue.Integer(1)]),
                    ElmValue.TagInstance("Just", [ElmValue.Integer(6)]),
                    ElmValue.TagInstance("Just", [ElmValue.Integer(4)]),
                    ElmValue.TagInstance("Nothing", []),
                    ]));
        }
    }

    private static ElmValue Strings(params string[] values) =>
        ElmValue.ListInstance([.. values.Select(ElmValue.StringInstance)]);

    private static IntermediatePineVM CreateVM(bool withLeaves, bool inline) =>
        IntermediatePineVM.CreateCustom(
            evalCache: null,
            evaluationConfigDefault: ElmCompilerTestHelper.DefaultTestEvaluationConfig,
            reportFunctionApplication: null,
            compilationEnvClasses: null,
            disableReductionInCompilation: false,
            selectPrecompiled: null,
            skipInlineForExpression: _ => !inline,
            enableTailRecursionOptimization: true,
            parseCache: null,
            precompiledLeaves:
            withLeaves
            ?
            Core.IntermediateVM.SetupVM.DefaultPrecompiledLeaves
            :
            ImmutableDictionary<PineValue, PrecompiledLeaf>.Empty,
            reportEnterPrecompiledLeaf: null,
            reportExitPrecompiledLeaf: null,
            optimizationParametersSerial: null,
            cacheFileStore: null);

    private static readonly Lazy<ElmInteractiveEnvironment.ParsedInteractiveEnvironment> s_environment =
        new(
            () =>
            {
                var sources =
                    BundledFiles.ElmKernelModulesDefault.Value.SetNodeAtPathSorted(
                        ["ModuleKeyRegression.elm"],
                        FileTree.File(
                            Encoding.UTF8.GetBytes(
                                """
                                module ModuleKeyRegression exposing (lookupModules)

                                import Dict

                                lookupModules : List String -> List (Maybe Int)
                                lookupModules names =
                                    let
                                        files =
                                            List.foldl
                                                (\name acc -> Dict.insert [name] (String.length name) acc)
                                                Dict.empty
                                                names
                                    in
                                    List.map (\name -> Dict.get [name] files)
                                        ["A", "B", "Broken", "Main", "Missing"]
                                """)));

                var compiled =
                    ElmCompilerTestHelper.CompileInteractiveEnvironmentFromFiles(
                        sources,
                        rootFilePaths: [["ModuleKeyRegression.elm"]])
                    .Extract(error => throw new Exception(error));

                return
                    ElmInteractiveEnvironment.ParseInteractiveEnvironment(compiled.compiledEnvValue)
                    .Extract(error => throw new Exception(error));
            });
}
