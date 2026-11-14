using AwesomeAssertions;
using Pine.Core.Elm.Syntax;
using Pine.Core.Testing;
using Pine.Elm019;
using System;
using System.Collections.Generic;
using System.Collections.Immutable;
using System.IO;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;

using Syntax = Pine.Core.Elm.Syntax.Abstract;

namespace Pine.IntegrationTests.Elm;

public partial class ElmMakeEcmascriptCoverageTests
{
    private static string CorpusDirectory => TestResultSummary.FindTestDataDirectory("ElmMakeEcmascript");

    private static ElmMakeScenario Scenario =>
        ElmMakeEcmascriptScenarios.SelectScenario(
            ElmMakeEcmascriptScenarios.Discover(CorpusDirectory),
            "expressions-and-patterns");

    [Fact]
    public async Task ElmMakeEcmascript_coverage_scenario_keeps_every_witness_in_the_reference_graph()
    {
        var scenario = Scenario;
        scenario.Expectations.Should().NotBeEmpty();

        scenario.Expectations.Should().OnlyContain(
            expectation => !expectation.Skipped && expectation.Flavor == ElmMakeFlavor.Development &&
                expectation.SourcePath == "src/Main.elm");

        var modules = ParseModules(scenario);
        var expectedModules = new[] { "Main", "Coverage", "CoverageTypes" };
        modules.Keys.Should().BeEquivalentTo(expectedModules);

        foreach (var expectation in scenario.Expectations)
        {
            var output =
                await File.ReadAllTextAsync(ElmMakeEcmascriptScenarios.ExpectationPath(CorpusDirectory, expectation));

            foreach (var name in expectedModules)
            {
                var module = modules[name];

                foreach (var function in module.Declarations.OfType<Syntax.Declaration.FunctionDeclaration>())
                {
                    output.Should().Contain(
                        GlobalDeclaration(name, function.Function.Declaration.Name),
                        $"{expectation.RelativePath} must retain every coverage witness, not just parse unused source");
                }

                foreach (var choice in module.Declarations.OfType<Syntax.Declaration.ChoiceTypeDeclaration>())
                {
                    foreach (var constructor in choice.TypeDeclaration.Constructors)
                        output.Should().Contain(GlobalDeclaration(name, constructor.Name));
                }

                foreach (var alias in module.Declarations.OfType<Syntax.Declaration.AliasDeclaration>())
                    output.Should().Contain(GlobalDeclaration(name, alias.TypeAlias.Name));
            }

        }
    }

    [Fact]
    public async Task ElmMakeEcmascript_coverage_scenario_reference_snapshots_are_reproducible()
    {
        var scenario = Scenario;

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

    [Fact]
    public async Task ElmMakeEcmascript_browser_programs_cover_each_constructor_with_active_expectations()
    {
        var scenario =
            ElmMakeEcmascriptScenarios.SelectScenario(
                ElmMakeEcmascriptScenarios.Discover(CorpusDirectory),
                "browser-programs");

        var variants =
            new Dictionary<string, string>
            {
                ["Sandbox"] = "sandbox",
                ["Element"] = "element",
                ["Document"] = "document",
                ["Application"] = "application",
            };

        var modules = ParseModules(scenario);
        modules.Keys.Should().BeEquivalentTo(variants.Keys);

        scenario.Expectations.Should().NotBeEmpty().And.OnlyContain(
            expectation => !expectation.Skipped && expectation.Kind == ElmMakeExpectationKind.Output);

        foreach (var (moduleName, constructor) in variants)
        {
            var main =
                modules[moduleName].Declarations.OfType<Syntax.Declaration.FunctionDeclaration>()
                .Single(function => function.Function.Declaration.Name == "main").Function;

            var signature = main.Signature!.TypeAnnotation.Should().BeOfType<Syntax.TypeAnnotation.Typed>().Subject;
            signature.Name.Should().Be("Program");
            signature.TypeArguments.Should().HaveCount(3);
            var application = main.Declaration.Expression.Should().BeOfType<Syntax.Expression.Application>().Subject;

            application.Function.Should().BeOfType<Syntax.Expression.Identifier>()
                .Which.QualifiedName.FullName.Should().Be("Browser." + constructor);

            var expectations =
                scenario.Expectations.Where(expectation => expectation.SourcePath == $"src/{moduleName}.elm").ToArray();

            expectations.Should().NotBeEmpty();

            foreach (var expectation in expectations)
            {
                var output =
                    await File.ReadAllTextAsync(ElmMakeEcmascriptScenarios.ExpectationPath(CorpusDirectory, expectation));

                output.Should().Contain(GlobalDeclaration(moduleName, "main"))
                    .And.Contain(GlobalDeclaration(moduleName, "view"))
                    .And.Contain(GlobalDeclaration(moduleName, "update"))
                    .And.Contain("$elm$browser$Browser$" + constructor);
            }
        }
    }

    [Fact]
    public async Task ElmMakeEcmascript_browser_programs_reference_snapshots_are_reproducible()
    {
        var scenario =
            ElmMakeEcmascriptScenarios.SelectScenario(
                ElmMakeEcmascriptScenarios.Discover(CorpusDirectory),
                "browser-programs");

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

    [Fact]
    public void ElmMakeEcmascript_coverage_scenario_contains_scoped_expression_and_valid_pattern_variants()
    {
        var expressions = new List<Syntax.Expression>();
        var patterns = new List<Syntax.Pattern>();
        var letVariants = new HashSet<Type>();

        foreach (var module in ParseModules(Scenario).Values)
        {
            foreach (var function in module.Declarations.OfType<Syntax.Declaration.FunctionDeclaration>())
            {
                foreach (var argument in function.Function.Declaration.Arguments)
                    CollectPattern(argument, patterns);

                Syntax.SyntaxAnalysis.WalkExpressionsWithScope(
                    function.Function.Declaration.Expression,
                    ImmutableHashSet<string>.Empty,
                    (expression, _) =>
                    {
                        expressions.Add(expression);

                        if (expression is Syntax.Expression.LambdaExpression lambda)
                        {
                            foreach (var argument in lambda.Arguments)
                                CollectPattern(argument, patterns);
                        }

                        if (expression is Syntax.Expression.CaseExpression caseExpression)
                        {
                            foreach (var branch in caseExpression.Cases)
                                CollectPattern(branch.Pattern, patterns);
                        }

                        if (expression is Syntax.Expression.LetExpression let)
                        {
                            foreach (var declaration in let.Declarations)
                            {
                                letVariants.Add(declaration.GetType());

                                switch (declaration)
                                {
                                    case Syntax.LetDeclaration.LetFunction local:
                                        foreach (var argument in local.Function.Declaration.Arguments)
                                            CollectPattern(argument, patterns);

                                        break;

                                    case Syntax.LetDeclaration.LetDestructuring destructuring:
                                        CollectPattern(destructuring.Pattern, patterns);
                                        break;

                                    default:
                                        throw new NotImplementedException(
                                            $"{nameof(ElmMakeEcmascript_coverage_scenario_contains_scoped_expression_and_valid_pattern_variants)} " +
                                            "does not handle let declaration variant: " + declaration.GetType().Name);
                                }
                            }
                        }
                    });
            }
        }

        expressions.Select(expression => expression.GetType()).Distinct()
            .Should().BeEquivalentTo(
            VariantsOf(typeof(Syntax.Expression)).Except([typeof(Syntax.Expression.GLSLExpression)]),
            "GLSL is deliberately outside this compiler project's supported scope");

        patterns.Select(pattern => pattern.GetType()).Distinct()
            .Should().BeEquivalentTo(
            VariantsOf(typeof(Syntax.Pattern)).Except([typeof(Syntax.Pattern.FloatPattern)]),
            "Elm 0.19.1 forbids FloatPattern, even though the syntax model can represent it");

        letVariants.Should().BeEquivalentTo(VariantsOf(typeof(Syntax.LetDeclaration)));

        expressions.OfType<Syntax.Expression.TupledExpression>().Select(tuple => tuple.Elements.Count)
            .Should().Contain([2, 3]);

        expressions.OfType<Syntax.Expression.RecordExpr>().Select(record => record.Fields.Count)
            .Should().Contain(0).And.Contain(count => count >= 3);

        expressions.OfType<Syntax.Expression.RecordUpdateExpression>()
            .Should().Contain(update => update.Fields.Count >= 2);

        expressions.OfType<Syntax.Expression.ListExpr>().Should().Contain(list => list.Elements.Count == 0)
            .And.Contain(list => list.Elements.Count >= 3);

        expressions.OfType<Syntax.Expression.Identifier>()
            .Should().Contain(identifier => identifier.QualifiedName.Namespaces.Count == 0)
            .And.Contain(identifier => identifier.QualifiedName.Namespaces.Count > 0);

        expressions.OfType<Syntax.Expression.CharLiteral>().Should().Contain(
            character => character.Value > char.MaxValue);

        expressions.OfType<Syntax.Expression.GLSLExpression>().Should().BeEmpty();

        expressions.OfType<Syntax.Expression.Application>().Should().Contain(
            application => application.Arguments.Count == 9);

        expressions.OfType<Syntax.Expression.LambdaExpression>()
            .Should().Contain(lambda => lambda.Arguments.Any(argument => argument is Syntax.Pattern.RecordPattern))
            .And.Contain(lambda => lambda.Arguments.Any(argument => argument is Syntax.Pattern.TuplePattern))
            .And.Contain(lambda => lambda.Arguments.Any(argument => argument is Syntax.Pattern.UnitPattern))
            .And.Contain(lambda => lambda.Arguments.Any(argument => argument is Syntax.Pattern.AllPattern))
            .And.Contain(lambda => lambda.Arguments.Any(argument => argument is Syntax.Pattern.AsPattern))
            .And.Contain(lambda => lambda.Arguments.Any(argument => argument is Syntax.Pattern.NamedPattern));

        patterns.OfType<Syntax.Pattern.TuplePattern>().Select(tuple => tuple.Elements.Count).Should().Contain([2, 3]);
        patterns.OfType<Syntax.Pattern.ListPattern>().Select(list => list.Elements.Count).Should().Contain([0, 1, 2]);

        patterns.OfType<Syntax.Pattern.NamedPattern>().Select(named => named.Arguments.Count).Should().Contain(
            [
            0,
            1,
            2,
            3
            ]);

        patterns.OfType<Syntax.Pattern.NamedPattern>()
            .Should().Contain(named => named.Name.ModuleName.Count > 0)
            .And.Contain(named => named.Name.ModuleName.Count == 0);

        patterns.OfType<Syntax.Pattern.AsPattern>()
            .Should().Contain(alias => alias.Pattern is Syntax.Pattern.TuplePattern)
            .And.Contain(alias => alias.Pattern is Syntax.Pattern.RecordPattern)
            .And.Contain(alias => alias.Pattern is Syntax.Pattern.UnConsPattern);

        patterns.OfType<Syntax.Pattern.CharPattern>().Should().Contain(character => character.Value > char.MaxValue);

        patterns.OfType<Syntax.Pattern.StringPattern>().Should().Contain(text => text.Value.Length == 0)
            .And.Contain(text => text.Value.Length > 0);
    }

    private static string GlobalDeclaration(string moduleName, string name) =>
        "var $author$project$" + moduleName.Replace('.', '$') + "$" + name + " =";

    private static IReadOnlyDictionary<string, Syntax.File> ParseModules(ElmMakeScenario scenario) =>
        scenario.AppFiles.EnumerateFilesTransitive()
        .Where(file => file.path[^1].EndsWith(".elm", StringComparison.Ordinal))
        .Select(
            file =>
            {
                var parsed =
                    ElmSyntaxParser.ParseModuleText(Encoding.UTF8.GetString(file.fileContent.Span))
                    .Extract(error => throw new InvalidDataException(string.Join("/", file.path) + ": " + error));

                parsed.AdditionalParseErrors.Should().BeEmpty();
                parsed.IncompleteDeclarations.Should().BeEmpty();
                return Syntax.ConvertFromConcrete.FromFile(parsed);
            })
        .ToDictionary(file => string.Join(".", Syntax.Module.GetModuleName(file.ModuleDefinition)));

    private static IEnumerable<Type> VariantsOf(Type hierarchy) =>
        hierarchy.GetNestedTypes().Where(type => !type.IsAbstract && type.BaseType == hierarchy);

    private static void CollectPattern(Syntax.Pattern pattern, List<Syntax.Pattern> found)
    {
        found.Add(pattern);

        switch (pattern)
        {
            case Syntax.Pattern.TuplePattern tuple:
                foreach (var item in tuple.Elements)
                    CollectPattern(item, found);

                break;

            case Syntax.Pattern.ListPattern list:
                foreach (var item in list.Elements)
                    CollectPattern(item, found);

                break;

            case Syntax.Pattern.UnConsPattern cons:
                CollectPattern(cons.Head, found);
                CollectPattern(cons.Tail, found);
                break;

            case Syntax.Pattern.NamedPattern named:
                foreach (var argument in named.Arguments)
                    CollectPattern(argument, found);

                break;

            case Syntax.Pattern.AsPattern alias:
                CollectPattern(alias.Pattern, found);
                break;

            case Syntax.Pattern.AllPattern:
            case Syntax.Pattern.VarPattern:
            case Syntax.Pattern.UnitPattern:
            case Syntax.Pattern.CharPattern:
            case Syntax.Pattern.StringPattern:
            case Syntax.Pattern.IntPattern:
            case Syntax.Pattern.FloatPattern:
            case Syntax.Pattern.RecordPattern:
                break;

            default:
                throw new NotImplementedException(
                    $"{nameof(CollectPattern)} does not handle pattern variant: " + pattern.GetType().Name);
        }
    }
}
