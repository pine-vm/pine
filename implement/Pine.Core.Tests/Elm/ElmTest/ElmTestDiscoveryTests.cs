using AwesomeAssertions;
using Pine.Core.Elm.Syntax;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Elm.Testing;
using Pine.Core.PineLanguage;
using System;
using System.Collections.Generic;
using System.IO;
using System.Linq;
using System.Threading;
using System.Text.Json;
using Xunit;
using Concrete = Pine.Core.Elm.Syntax.Concrete;
using ConvertFromConcrete = Pine.Core.Elm.Syntax.Abstract.ConvertFromConcrete;

namespace Pine.Core.Tests.Elm.ElmTest;

public class ElmTestDiscoveryTests
{
    [Fact]
    public void Syntax_test_discovery_resolves_test_IDs_to_exact_nested_source_expressions()
    {
        const string first = """Test.test "first" (\_ -> Expect.pass)""";
        const string second = """Test.test "second" (\_ -> Expect.pass)""";
        using var project = new ElmFuzzTests.Project(
            "suite = Test.describe \"group\" [ ((" + first + ")), " + second + " ]");

        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Select(test => test.ListedTest.Name).Should().Equal("first", "second");
        AssertSource(project, discovery, discovery.Tests[0].Id, first);
        AssertSource(project, discovery, discovery.Tests[1].Id, second);
        discovery.Tests.Select(test => test.Id).Distinct().Should().HaveCount(2);
        discovery.GetSourceLocation(discovery.Tests[0].Id).ExpressionPath.Steps.Should().NotBeEmpty();
    }

    [Theory]
    [InlineData("""suite = { z = Test.test "z" (\_ -> Expect.pass), a = Test.test "a" (\_ -> Expect.pass) }.z""",
        """Test.test "z" (\_ -> Expect.pass)""", "z")]
    [InlineData("""suite = let local = Test.test "local" (\_ -> Expect.pass) in local""",
        """Test.test "local" (\_ -> Expect.pass)""", "local")]
    [InlineData("""suite = let make label = Test.test label (\_ -> Expect.pass) in make "local function" """,
        """Test.test label (\_ -> Expect.pass)""", "local function")]
    [InlineData("""suite = let ( chosen, ignored ) = (Test.test "tuple" (\_ -> Expect.pass), 1) in chosen""",
        """Test.test "tuple" (\_ -> Expect.pass)""", "tuple")]
    [InlineData("""
        suite =
            case 1 of
                0 -> Test.test "not selected" (\_ -> Expect.pass)
                1 -> Test.test "constant branch" (\_ -> Expect.pass)
                _ -> Test.todo "fallback"
        """, """Test.test "constant branch" (\_ -> Expect.pass)""", "constant branch")]
    [InlineData("""suite = Test.test "pipeline" <| (\_ -> Expect.pass)""",
        """Test.test "pipeline" <| (\_ -> Expect.pass)""", "pipeline")]
    [InlineData("""suite = (\_ -> Expect.pass) |> Test.test "reverse pipeline" """,
        """(\_ -> Expect.pass) |> Test.test "reverse pipeline" """, "reverse pipeline")]
    public void Syntax_test_discovery_maps_normalized_AST_paths_back_to_CST(
        string declarations, string expectedExpression, string expectedName)
    {
        using var project = new ElmFuzzTests.Project(declarations);
        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Should().ContainSingle().Which.ListedTest.Name.Should().Be(expectedName);
        AssertSource(project, discovery, discovery.Tests[0].Id, expectedExpression.TrimEnd());
    }

    [Fact]
    public void Syntax_test_discovery_maps_computed_and_generated_tests_to_the_factory_source()
    {
        using var project = new ElmFuzzTests.Project(
            """
            factory index =
                Test.test ("case " ++ String.fromInt index) (\_ -> Expect.pass)

            suite =
                Test.describe "generated" (List.map factory (List.range 1 4))
            """);

        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Select(test => test.ListedTest.Name).Should().Equal("case 1", "case 2", "case 3", "case 4");
        discovery.Tests.Select(test => test.Id).Distinct().Should().HaveCount(4);
        foreach (var test in discovery.Tests)
            AssertSource(project, discovery, test.Id, """Test.test ("case " ++ String.fromInt index) (\_ -> Expect.pass)""");
    }

    [Fact]
    public void Syntax_test_discovery_preserves_imported_helper_source_provenance()
    {
        using var project = new ElmFuzzTests.Project("""suite = Helpers.make "from helper" """);
        var testFile = Path.Combine(project.Directory, "tests", "Tests.elm");
        File.WriteAllText(testFile, File.ReadAllText(testFile).Replace("import Test\n", "import Test\nimport Helpers\n"));
        const string helper =
            """
            module Helpers exposing (make)
            import Test
            import Expect

            make label =
                Test.test label (\_ -> Expect.pass)
            """;
        File.WriteAllText(Path.Combine(project.Directory, "tests", "Helpers.elm"), helper);

        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        var location = discovery.GetSourceLocation(discovery.Tests.Single().Id);
        location.FilePath.Should().Be("tests/Helpers.elm");
        location.ModuleOrigin.ModuleName.Should().Be("Helpers");
        location.DeclarationName.Should().Be("make");
        Slice(helper, location.Range).Should().Be("""Test.test label (\_ -> Expect.pass)""");
    }

    [Fact]
    public void Syntax_test_discovery_does_not_execute_assertions_or_fuzz_callbacks()
    {
        using var project = new ElmFuzzTests.Project(
            """
            loop _ = loop ()
            suite =
                Test.describe "safe discovery"
                    [ Test.test "unit" (\_ -> loop ())
                    , Test.fuzz Fuzz.int "fuzz" (\_ -> loop ())
                    , Test.todo "later"
                    ]
            """);
        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Select(test => test.Kind).Should().Equal("unit", "fuzz", "todo");
        foreach (var test in discovery.Tests)
            discovery.GetSourceLocation(test.Id).FilePath.Should().Be("tests/Tests.elm");
    }

    [Theory]
    [InlineData("""
        suite = Test.describe "group"
            [ Test.test "unit" (\_ -> Expect.pass)
            , Test.fuzz Fuzz.int "fuzz" (\_ -> Expect.pass)
            , Test.todo "later"
            ]
        """)]
    [InlineData("""
        suite = Test.describe "group"
            [ Test.skip (Test.test "skip" (\_ -> Expect.pass))
            , Test.only (Test.test "only" (\_ -> Expect.pass))
            , Test.test "ordinary" (\_ -> Expect.pass)
            ]
        """)]
    [InlineData("""
        suite = Test.concat
            [ Test.skip (Test.only (Test.test "hidden" (\_ -> Expect.pass)))
            , Test.test "visible" (\_ -> Expect.pass)
            ]
        """)]
    [InlineData("""
        suite = Test.describe "empty" []
        """)]
    [InlineData("""
        suite = Test.describe "duplicates"
            [ Test.test "same" (\_ -> Expect.pass)
            , Test.test "same" (\_ -> Expect.pass)
            ]
        """)]
    public void Syntax_test_discovery_matches_existing_compiler_listing(string declarations)
    {
        using var project = new ElmFuzzTests.Project(declarations);
        var compiled = ElmTestRunner.CompileAndRunTests(project.Directory, listTests: true)
            .Should().BeOfType<ElmTestRun.Listed>().Which;
        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Select(test => test.ListedTest).Should().Equal(compiled.Tests);
        foreach (var test in discovery.Tests)
            discovery.GetSourceLocation(test.Id).FilePath.Should().Be("tests/Tests.elm");
    }

    [Fact]
    public void Syntax_test_discovery_ID_selects_a_duplicate_label_for_existing_PineVM_execution()
    {
        using var project = new ElmFuzzTests.Project(
            """
            suite = Test.test "same" (\_ -> Expect.pass)
            second = Test.test "same" (\_ -> Expect.fail "selected second")
            """);
        var file = Path.Combine(project.Directory, "tests", "Tests.elm");
        File.WriteAllText(file, File.ReadAllText(file).Replace("exposing (suite)", "exposing (suite, second)"));
        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Should().HaveCount(2);
        discovery.Tests[0].Id.Should().NotBe(discovery.Tests[1].Id);
        AssertSource(project, discovery, discovery.Tests[1].Id, """Test.test "same" (\_ -> Expect.fail "selected second")""");
        discovery.GetExecutionFilter(discovery.Tests[1].Id).Should().Be("#2");

        var run = ElmTestRunner.CompileAndRunTests(project.Directory,
            filter: discovery.GetExecutionFilter(discovery.Tests[1].Id));
        run.Should().BeOfType<ElmTestRun.Completed>().Which.Tests.Should().ContainSingle()
            .Which.Kind.Should().Be(CompletedTestKind.Failed);
    }

    [Fact]
    public void Syntax_test_discovery_preserves_execution_ordinals_before_global_only_filtering()
    {
        using var project = new ElmFuzzTests.Project(
            """
            suite = Test.test "ordinary" (\_ -> Expect.pass)
            second = Test.only (Test.test "same" (\_ -> Expect.pass))
            third = Test.only (Test.test "same" (\_ -> Expect.fail "selected third"))
            """);
        var file = Path.Combine(project.Directory, "tests", "Tests.elm");
        File.WriteAllText(file, File.ReadAllText(file).Replace("exposing (suite)", "exposing (suite, second, third)"));
        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Select(test => test.DiscoveryIndex).Should().Equal(2, 3);
        discovery.GetExecutionFilter(discovery.Tests[1].Id).Should().Be("#3");
        var run = ElmTestRunner.CompileAndRunTests(project.Directory, filter: discovery.GetExecutionFilter(discovery.Tests[1].Id));
        run.Should().BeOfType<ElmTestRun.Completed>().Which.Tests.Should().ContainSingle()
            .Which.Kind.Should().Be(CompletedTestKind.Failed);
    }

    [Fact]
    public void Syntax_test_discovery_keeps_paths_inside_operator_operands_and_record_updates()
    {
        const string expression = """Test.test ("case " ++ String.fromInt n) (\_ -> Expect.pass)""";
        using var project = new ElmFuzzTests.Project(
            "suite = List.range 1 2 |> List.map (\\n -> let record = { z = 0, a = Test.todo \"unused\" } in " +
            "{ record | a = " + expression + " }.a) |> Test.describe \"mapped\"\n");
        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Select(test => test.ListedTest.Name).Should().Equal("case 1", "case 2");
        foreach (var test in discovery.Tests)
            AssertSource(project, discovery, test.Id, expression);
    }

    [Fact]
    public void Syntax_test_discovery_qualifies_custom_operator_targets_without_changing_paths()
    {
        using var project = new ElmFuzzTests.Project("""suite = "custom" <?> (\_ -> Expect.pass)""");
        var testFile = Path.Combine(project.Directory, "tests", "Tests.elm");
        File.WriteAllText(testFile, File.ReadAllText(testFile).Replace("import Test\n", "import Test\nimport Helpers exposing ((<?>))\n"));
        const string helper =
            """
            module Helpers exposing ((<?>))
            import Test
            import Expect
            infix left 5 (<?>) = makeTest
            makeTest label assertion = Test.test label assertion
            """;
        File.WriteAllText(Path.Combine(project.Directory, "tests", "Helpers.elm"), helper);
        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        discovery.Tests.Single().ListedTest.Name.Should().Be("custom");
        var source = discovery.GetSourceLocation(discovery.Tests[0].Id);
        source.FilePath.Should().Be("tests/Helpers.elm");
        Slice(helper, source.Range).Should().Be("Test.test label assertion");
    }

    [Fact]
    public void Syntax_test_discovery_has_explicit_unknown_ID_and_cancellation_errors()
    {
        using var project = new ElmFuzzTests.Project("""suite = Test.todo "later" """);
        var discovery = ElmTestDiscovery.DiscoverTests(project.Directory);
        var lookup = () => discovery.GetSourceLocation("unknown");
        lookup.Should().Throw<KeyNotFoundException>().WithMessage("*Unknown Elm test ID*");
        var discover = () => ElmTestDiscovery.DiscoverTests(project.Directory, cancellationToken: new CancellationToken(true));
        discover.Should().Throw<OperationCanceledException>();

        var program = ElmInterpreterEnvironment.Create(["module Loop exposing (loop)\nloop _ = loop ()"])
            .Extract(error => throw new InvalidOperationException(error));
        var interpret = () => program.Invoke(Pine.Core.CodeAnalysis.DeclQualifiedName.FromString("Loop.loop"),
            [Pine.Core.Internal.PineValueInProcess.Create(PineValue.EmptyList)],
            ElmSyntaxInterpreter.EvaluationConfig.Default with { CancellationToken = new CancellationToken(true) });
        interpret.Should().Throw<OperationCanceledException>();
    }

    [Fact]
    public void Syntax_test_discovery_reports_source_errors_and_ignores_unused_helper_bodies()
    {
        using var project = new ElmFuzzTests.Project("""suite = Test.todo "later" """);
        File.WriteAllText(Path.Combine(project.Directory, "tests", "Helpers.elm"),
            "module Helpers exposing (identity)\nidentity value = value\nunused = Missing.value\n");
        ElmTestDiscovery.DiscoverTests(project.Directory).Tests.Should().ContainSingle();
        File.WriteAllText(Path.Combine(project.Directory, "tests", "Tests.elm"),
            "module Tests exposing (suite)\nsuite = Missing.value\n");
        var discover = () => ElmTestDiscovery.DiscoverTests(project.Directory);
        discover.Should().Throw<InvalidOperationException>().WithMessage("*canonicalizing*Tests.suite*");
    }

    [Fact]
    public void Syntax_test_discovery_paths_reject_mismatched_CST_instead_of_inventing_ranges()
    {
        var concrete = ElmSyntaxParser.ParseModuleText("module Tests exposing (suite)\nsuite = (({ z = 0x10, a = 2 }))")
            .Extract(error => throw new InvalidOperationException(error.ToString()));
        var function = (Concrete.Declaration.FunctionDeclaration)concrete.Declarations.Single().Value;
        var expression = ElmSyntaxInterpreter.PrepareExpression(
            ConvertFromConcrete.FromExpression(function.Function.Declaration.Value.Expression.Value));
        var fields = ElmSyntaxInterpreter.EnumerateExpressionPaths(expression).ToArray();
        fields[1].Path.Steps[0].Should().Be(new ElmSyntaxExpressionPath.Step.RecordField("a", 0));
        fields[1].Path.Resolve(function.Function.Declaration.Value.Expression)!.Value.Should().BeOfType<Concrete.Expression.IntegerLiteral>()
            .Which.LiteralText.Should().Be("2");
        fields[2].Path.Resolve(function.Function.Declaration.Value.Expression)!.Value.Should().BeOfType<Concrete.Expression.IntegerLiteral>()
            .Which.LiteralText.Should().Be("0x10");
        ElmSyntaxExpressionPath.Root.Append(new ElmSyntaxExpressionPath.Step.Child(99))
            .Resolve(function.Function.Declaration.Value.Expression).Should().BeNull();
        ElmSyntaxExpressionPath.Root.Append(new ElmSyntaxExpressionPath.Step.RecordField("a", 0))
            .Should().Be(ElmSyntaxExpressionPath.Root.Append(new ElmSyntaxExpressionPath.Step.RecordField("a", 0)));
    }

    [Fact]
    public void Syntax_test_discovery_paths_roundtrip_JSON_without_source_ranges()
    {
        var path = ElmSyntaxExpressionPath.Root.Append(
            new ElmSyntaxExpressionPath.Step.LetDeclaration(2),
            new ElmSyntaxExpressionPath.Step.Implementation(),
            new ElmSyntaxExpressionPath.Step.Body(),
            new ElmSyntaxExpressionPath.Step.RecordField("a", 1),
            new ElmSyntaxExpressionPath.Step.Child(0));
        var json = JsonSerializer.Serialize(path);
        JsonSerializer.Deserialize<ElmSyntaxExpressionPath>(json).Should().Be(path);
        json.Should().NotContain("Range");
    }

    private static void AssertSource(ElmFuzzTests.Project project, ElmTestDiscoveryResult discovery, string id, string expected)
    {
        var location = discovery.GetSourceLocation(id);
        location.FilePath.Should().Be("tests/Tests.elm");
        Slice(File.ReadAllText(Path.Combine(project.Directory, "tests", "Tests.elm")), location.Range).Should().Be(expected);
    }

    private static string Slice(string text, Concrete.Range range)
    {
        var lines = text.Split('\n');
        var start = lines.Take(range.Start.Row - 1).Sum(line => line.Length + 1) + range.Start.Column - 1;
        var end = lines.Take(range.End.Row - 1).Sum(line => line.Length + 1) + range.End.Column - 1;
        return text[start..end];
    }
}
