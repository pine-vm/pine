using AwesomeAssertions;
using Pine.Elm019;
using System;
using System.IO;
using System.Linq;
using System.Threading.Tasks;
using Xunit;

using Syntax = Pine.Core.Elm.Syntax.Abstract;

namespace Pine.IntegrationTests.Elm;

public partial class ElmMakeEcmascriptCoverageTests
{
    private static ElmMakeScenario OptimizationScenario =>
        ElmMakeEcmascriptScenarios.SelectScenario(
            ElmMakeEcmascriptScenarios.Discover(CorpusDirectory),
            "compiler-optimizations");

    [Fact]
    public async Task ElmMakeEcmascript_optimization_witnesses_are_reachable_in_both_flavors()
    {
        var scenario = OptimizationScenario;
        var modules = ParseModules(scenario);
        modules.Keys.Should().BeEquivalentTo("Main", "Optimizations");

        scenario.Expectations.Should().NotBeEmpty().And.OnlyContain(
            expectation => !expectation.Skipped && expectation.Kind == ElmMakeExpectationKind.Output &&
                expectation.SourcePath == "src/Main.elm");

        scenario.Expectations.Select(expectation => expectation.Flavor).Distinct()
            .Should().BeEquivalentTo(new[] { ElmMakeFlavor.Development, ElmMakeFlavor.Optimize });

        foreach (var expectation in scenario.Expectations)
        {
            var output =
                await File.ReadAllTextAsync(ElmMakeEcmascriptScenarios.ExpectationPath(CorpusDirectory, expectation));

            output.Should().Contain(GlobalDeclaration("Main", "main"));

            foreach (var declaration in modules["Optimizations"].Declarations)
            {
                switch (declaration)
                {
                    case Syntax.Declaration.FunctionDeclaration function:
                        output.Should().Contain(
                            GlobalDeclaration("Optimizations", function.Function.Declaration.Name),
                            "every optimization witness must survive dead-code elimination in " +
                            expectation.RelativePath);

                        break;

                    case Syntax.Declaration.ChoiceTypeDeclaration choice:
                        foreach (var constructor in choice.TypeDeclaration.Constructors)
                            output.Should().Contain(GlobalDeclaration("Optimizations", constructor.Name));

                        break;

                    case Syntax.Declaration.AliasDeclaration:
                    case Syntax.Declaration.PortDeclaration:
                    case Syntax.Declaration.InfixDeclaration:
                        throw new InvalidDataException(
                            "Unexpected optimization fixture declaration: " + declaration.GetType().Name);

                    default:
                        throw new NotImplementedException(
                            nameof(ElmMakeEcmascript_optimization_witnesses_are_reachable_in_both_flavors) +
                            " does not handle declaration variant: " + declaration.GetType().Name);
                }
            }
        }
    }

    [Fact]
    public async Task ElmMakeEcmascript_optimization_reference_outputs_exercise_tail_call_eligibility_and_parallel_assignment()
    {
        foreach (var expectation in OptimizationScenario.Expectations)
        {
            var output =
                await File.ReadAllTextAsync(ElmMakeEcmascriptScenarios.ExpectationPath(CorpusDirectory, expectation));

            foreach (var name in new[]
            {
                "tailIf", "tailCase", "tailLet", "tailDestructure", "tailBranches",
                "tailSwap", "tailRecord", "tailArgumentPattern", "tailLocalBinding",
            })
            {
                Witness(output, name).Should().Contain(name + ":\n")
                    .And.Contain("while (true)")
                    .And.Contain("continue " + name + ";")
                    .And.Contain("$temp$");

                Witness(output, name).Should().NotContain(
                    "$author$project$Optimizations$" + name + "(",
                    "the recursive call must become a loop, not merely coexist with a label");
            }

            var swap = Witness(output, "tailSwap");
            swap.Should().Contain("$temp$left = right").And.Contain("$temp$right = left");

            swap.IndexOf("$temp$right = left", StringComparison.Ordinal)
                .Should().BeLessThan(swap.IndexOf("left = $temp$left", StringComparison.Ordinal));

            var record = Witness(output, "tailRecord");

            record.IndexOf("while (true)", StringComparison.Ordinal)
                .Should().BeLessThan(
                record.IndexOf("var remaining =", StringComparison.Ordinal),
                "destructured arguments must be rebound on each iteration");

            Witness(output, "localTail").Should().Contain("loop:\n").And.Contain("continue loop;");
            Witness(output, "tailLocalBinding").Should().Contain("adjust:\n").And.Contain("continue adjust;");

            Witness(output, "tailBranches").Split("continue tailBranches;", StringSplitOptions.None).Should().HaveCount(
                3);

            foreach (var name in new[]
            {
                "nonTail", "nonTailBinding", "nonTailWrapped", "nonTailLambda",
                "tailPipeLeft", "tailPipeRight", "mutualEven", "mutualOdd", "localMutual",
            })
            {
                Witness(output, name).Should().NotContain("while (true)").And.NotContain(
                    "continue ",
                    "non-tail, nested-lambda, mutual, and pipe-syntax recursion are not optimized by classic 0.19.1");
            }

            Witness(output, "nonTail").Should().Contain("1 + $author$project$Optimizations$nonTail(");
            Witness(output, "nonTailWrapped").Should().Contain("$elm$core$Basics$identity(");
            Witness(output, "mutualEven").Should().Contain("$author$project$Optimizations$mutualOdd(");
            Witness(output, "mutualOdd").Should().Contain("$author$project$Optimizations$mutualEven(");
        }
    }

    [Fact]
    public async Task ElmMakeEcmascript_optimization_reference_outputs_exercise_all_public_core_call_specializations()
    {
        foreach (var expectation in OptimizationScenario.Expectations)
        {
            var output =
                await File.ReadAllTextAsync(ElmMakeEcmascriptScenarios.ExpectationPath(CorpusDirectory, expectation));

            Witness(output, "numericInt").Should().Contain("(left + (right * 3)) - 2")
                .And.Contain("(left / right) | 0").And.Contain("left % right");

            Witness(output, "numericFloat")
                .Should().Contain("left / right").And.Contain("left | 0").And.Contain("-right")
                .And.NotContain("$elm$core$Basics$toFloat(");

            Witness(output, "booleanOps").Should().Contain("[!left, left && right, left || right, left !== right]");

            Witness(output, "literalEquality").Should().Contain(
                "[!number, !number, !(!number), !(!number), flag, flag, !flag, !flag, !flag, !flag, flag, flag, number === 7, number !== 7]");

            Witness(output, "genericEquality").Should().Contain("_Utils_eq(left, right)").And.Contain(
                "!_Utils_eq(left, right)");

            Witness(output, "literalStrings").Should().Contain("value === 'text'").And.Contain("value !== 'text'")
                .And.Contain("value < 'text'").And.Contain("value >= 'text'");

            Witness(output, "literalFloats").Should().Contain("value === 1.5").And.Contain("value !== 1.5")
                .And.Contain("value < 1.5").And.Contain("value >= 1.5");

            Witness(output, "literalComparisons").Should().Contain(
                "[number < 3, 3 < number, number <= 3, 3 <= number, number > 3, 3 > number, number >= 3, 3 >= number]");

            foreach (var comparison in new[] { "< 0", "< 1", "> 0", "> -1" })
                Witness(output, "genericComparisons").Should().Contain("_Utils_cmp(left, right) " + comparison);

            Witness(output, "appendStrings").Should().Contain("left + (middle + (':' + right))").And.NotContain(
                "_Utils_ap");

            Witness(output, "appendLists").Should().Contain("_Utils_ap(middle, right)").And.NotContain(
                "$elm$core$Basics$append");

            Witness(output, "appendLeftNested").Should().Contain("left + ':'").And.Contain("_Utils_ap(middle, right)")
                .And.StartWith(GlobalDeclaration("Optimizations", "appendLeftNested"));

            Witness(output, "appendUnknownStrings").Should().Contain("_Utils_ap(left, right)");
            Witness(output, "tupleAccess").Should().Contain("pair.a + pair.b").And.NotContain("$elm$core$Tuple$first(");

            Witness(output, "bitwiseOps").Should().Contain(
                "[left & right, left | right, left ^ right, ~left, left << right, left >> right, left >>> right]");

            Witness(output, "pipeLeft").Should().Contain("A2($author$project$Optimizations$arity2, number, 3)");
            Witness(output, "pipeRight").Should().Contain("A2($author$project$Optimizations$arity2, 3, number)");
            Witness(output, "accessorPipe").Should().Contain("return record.").And.NotContain("function ($)");
            Witness(output, "pipeBare").Should().Contain("return -number;").And.NotContain("$elm$core$Basics$apR");
            Witness(output, "prefixArithmetic").Should().Contain("return left + right;");
            Witness(output, "composeLeft").Should().Contain("$elm$core$Basics$composeL,");
            Witness(output, "composeRight").Should().Contain("$elm$core$Basics$composeR,");

            Witness(output, "partialCoreCall").Should().Contain("$elm$core$Bitwise$and(7)").And.Contain(
                "return masked(number)");

            Witness(output, "partialBasicsCall").Should().Contain("$elm$core$Basics$add(3)").And.Contain(
                "return plusThree(number)");

            // In 0.19.1 the specializer tests "JsArray", but core 1.0.5's private module is "Elm.JsArray".
            // Public Array witnesses reach both calls, proving these dormant branches do not fire.
            Definition(output, "$elm$core$Array$getHelp").Should().Contain("$elm$core$Elm$JsArray$unsafeGet");

            Definition(output, "$elm$core$Array$insertTailInTree").Should().Contain(
                "$elm$core$Elm$JsArray$singleton(value)");
        }
    }

    [Fact]
    public async Task ElmMakeEcmascript_optimization_reference_outputs_cover_arity_decisions_and_mode_specific_representations()
    {
        foreach (var expectation in OptimizationScenario.Expectations)
        {
            var output =
                await File.ReadAllTextAsync(ElmMakeEcmascriptScenarios.ExpectationPath(CorpusDirectory, expectation));

            var optimized = expectation.Flavor is ElmMakeFlavor.Optimize;

            for (var arity = 2; arity <= 9; ++arity)
            {
                Witness(output, "arity" + arity).Should().Contain("= F" + arity + "(");

                Witness(output, "results").Should().Contain(
                    "A" + arity + "($author$project$Optimizations$arity" + arity + ",");
            }

            Witness(output, "arity10").Should().Contain("return function (j)").And.NotContain("F10(");

            Witness(output, "results").Should().Contain(
                "$author$project$Optimizations$arity10(1)(2)(3)(4)(5)(6)(7)(8)(9)(10)");

            Witness(output, "partialCall").Should().Contain("$author$project$Optimizations$arity2(3)").And.Contain(
                "return addThree(number)");

            Witness(output, "caseFanout")
                .Should().Contain("switch (number)").And.Contain("case 0:").And.Contain("case 1:").And
                .Contain("case 2:");

            Witness(output, "booleanPattern").Should().Contain("if (value)").And.NotContain("value.$");

            Witness(output, "stringPattern").Should().Contain("switch (value)").And.Contain("case '':").And.Contain(
                "case 'match':");

            Witness(output, "decisionShared").Should().Contain("break _v0$2;").And.Contain("_v0$2:");
            Witness(output, "conditionalExpression").Should().Contain(" ? ").And.NotContain("if (");
            Witness(output, "conditionalBlock").Should().Contain("if (").And.Contain("var result =");
            Witness(output, "emptyList").Should().Contain("return _List_Nil");

            if (optimized)
            {
                Witness(output, "Wrapped").Should().Contain("= $elm$core$Basics$identity;");
                Witness(output, "unwrap").Should().Contain("var value = _v0;").And.NotContain("_v0.a");

                Witness(output, "wrappedAsValue").Should().Contain("$elm$core$Basics$identity,")
                    .And.NotContain("$author$project$Optimizations$Wrapped,");

                Witness(output, "results").Should().Contain("$author$project$Optimizations$unwrap(7)");
                Witness(output, "Red").Should().Contain("= 0;");
                Witness(output, "enumPattern").Should().Contain("switch (color)").And.NotContain("color.$");
                Witness(output, "Pair").Should().Contain("{$: 1, a: a, b: b}");
                Definition(output, "$elm$core$Dict$RBNode_elm_builtin").Should().Contain("{$: -1,");
                Definition(output, "$elm$core$Dict$RBEmpty_elm_builtin").Should().Contain("{$: -2}");

                Witness(output, "recordSpecializations").Should().Contain("_Utils_update")
                    .And.NotContain("longFieldName").And.NotContain("otherFieldName");

                Witness(output, "characterPattern").Should().Contain("'a' === character").And.NotContain("valueOf");
                Witness(output, "characterEquality").Should().Contain("[value === 'x', value !== 'x']");
                Witness(output, "literalRepresentations").Should().Contain("_Utils_Tuple2(0, 'x')");
                Witness(output, "tailRecord").Should().NotContain("model.remaining").And.NotContain("model.total");
                output.Should().NotContain("Compiled in DEV mode");
            }
            else
            {
                Witness(output, "Wrapped").Should().Contain("{$: 'Wrapped', a: a}");
                Witness(output, "unwrap").Should().Contain("var value = _v0.a;");
                Witness(output, "Red").Should().Contain("{$: 'Red'}");
                Witness(output, "enumPattern").Should().Contain("switch (color.$)");
                Witness(output, "Pair").Should().Contain("{$: 'Pair', a: a, b: b}");

                Witness(output, "recordSpecializations").Should().Contain(".longFieldName").And.Contain(
                    ".otherFieldName");

                Witness(output, "characterPattern").Should().Contain("character.valueOf()");
                Witness(output, "characterEquality").Should().Contain("_Utils_eq(").And.Contain("_Utils_chr('x')");

                Witness(output, "literalRepresentations").Should().Contain("_Utils_Tuple0").And.Contain(
                    "_Utils_chr('x')");

                output.Should().Contain("Compiled in DEV mode");
            }
        }
    }

    [Fact]
    public async Task ElmMakeEcmascript_optimization_reference_snapshots_are_reproducible()
    {
        var scenario = OptimizationScenario;

        var results =
            await ElmMakeEcmascriptScenarios.RunAsync(
                CorpusDirectory,
                [scenario],
                new ElmMakeReferenceCompiler(CorpusDirectory).CompileOutcomeAsync);

        var report = ElmMakeEcmascriptScenarios.RenderResults(results);
        Console.WriteLine("Reference reproducibility only:\n" + report);

        results.Should().HaveCount(scenario.Expectations.Count(item => !item.Skipped)).And.OnlyContain(
            result => result.Passed,
            report);
    }

    private static string Witness(string output, string name) =>
        Definition(output, "$author$project$Optimizations$" + name);

    private static string Definition(string output, string global)
    {
        var start = output.IndexOf("var " + global + " =", StringComparison.Ordinal);
        start.Should().BeGreaterThanOrEqualTo(0, "missing reachable witness " + global);
        var end = output.IndexOf("\nvar $", start + 1, StringComparison.Ordinal);
        return end < 0 ? output[start..] : output[start..end];
    }
}
