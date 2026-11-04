using AwesomeAssertions;
using Pine.Core.Elm;
using Pine.Core.Elm.Syntax.Interpreter;
using Pine.Core.Files;
using Pine.Core.Functional;
using Pine.Core.Internal;
using System;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Xunit;

namespace Pine.Core.Tests.Elm.Syntax.Interpreter;

public class ElmInterpreterPreparationTests
{
    private static readonly Lazy<ElmInterpreterEnvironment> s_library =
        new(() => ElmInterpreterSourcePreparation.Prepare(null).Extract(error => throw new Exception(error)));

    private static ElmInterpreterEnvironment Declare(ElmInterpreterEnvironment environment, string text) =>
        environment.PrepareSubmission(text)
        .Extract(error => throw new Exception(error))
        .Should().BeOfType<ElmInterpreterSubmission.Declaration>().Subject.Environment;

    private static string Evaluate(ElmInterpreterEnvironment environment, string text) =>
        ElmSyntaxInterpreter.RenderAsElmExpression(
            environment.PrepareExpression(text)
            .Extract(error => throw new Exception(error))
            .Evaluate(new ElmSyntaxInterpreter.EvaluationConfig(10_000, 1_000))
            .Extract(error => throw new Exception(error.ToString())))
        .expressionString;

    private static FileTree Tree(params (string path, string text)[] files) =>
        FileTree.FromSetOfFilesWithStringPath(
            files.Select(
                file => ((System.Collections.Generic.IReadOnlyList<string>)file.path.Split('\\'),
                (ReadOnlyMemory<byte>)Encoding.UTF8.GetBytes(file.text))));

    [Fact]
    public void Library_only_preparation_supports_qualified_calls_and_default_imports()
    {
        Evaluate(s_library.Value, "String.fromInt (List.sum [ 1, 2, 3 ])").Should().Be("\"6\"");
        Evaluate(s_library.Value, "Just (identity 42)").Should().Be("Just 42");
    }

    [Fact]
    public void Shadowed_declarations_remain_indirectly_reachable()
    {
        var first = Declare(s_library.Value, "x = 10");
        var dependent = Declare(first, "old n = x + n");
        var second = Declare(dependent, "x = 20");
        var newest = Declare(second, "current n = x + n");

        Evaluate(newest, "[ old 1, current 1, x ]").Should().Be("[ 11, 21, 20 ]");
        Evaluate(first, "x").Should().Be("10");
        newest.Program.Declarations.Should().ContainKey(first.ValueBindings["x"]);
        newest.Program.Declarations.Should().ContainKey(second.ValueBindings["x"]);
        first.ValueBindings["x"].Should().NotBe(second.ValueBindings["x"]);
    }

    [Fact]
    public void Redefined_recursive_function_keeps_old_recursive_identity()
    {
        var first = Declare(s_library.Value, "f n = if n < 1 then 0 else 1 + f (n - 1)");
        var alias = Declare(first, "old = f");
        var next = Declare(alias, "f n = 100");

        Evaluate(next, "[ old 4, f 4 ]").Should().Be("[ 4, 100 ]");
    }

    [Fact]
    public void Captured_partial_application_retains_its_defining_scope()
    {
        var first = Declare(s_library.Value, "add a b = a + b");
        var partial = Declare(first, "addTen = add 10");
        var next = Declare(partial, "add a b = 999");

        Evaluate(next, "[ addTen 2, add 10 2 ]").Should().Be("[ 12, 999 ]");
        Evaluate(next, "(\\f -> f 3) addTen").Should().Be("13");
    }

    [Fact]
    public void Preparation_does_not_evaluate_or_materialize_function_values()
    {
        var environment = Declare(s_library.Value, "f x = x");
        var prepared = environment.PrepareExpression("f").Extract(error => throw new Exception(error));
        var value = prepared.Evaluate().Extract(error => throw new Exception(error.ToString()));

        ElmSyntaxInterpreter.RenderAsElmExpression(value).expressionString.Should().Contain("closure");

        var looping = Declare(environment, "loop x = loop x");
        var expression = looping.PrepareExpression("loop 0").Extract(error => throw new Exception(error));
        var result = expression.Evaluate(new ElmSyntaxInterpreter.EvaluationConfig(100, 100));
        result.IsErrOrNull()!.QuotaExceeded.Should().NotBeNull();
    }

    [Fact]
    public void Prepared_expression_and_sibling_snapshots_are_isolated()
    {
        var original = Declare(s_library.Value, "x = 1");
        var expression = original.PrepareExpression("x").Extract(error => throw new Exception(error));
        var left = Declare(original, "x = 2");
        var right = Declare(original, "x = 3");

        Evaluate(left, "x").Should().Be("2");
        Evaluate(right, "x").Should().Be("3");

        ElmSyntaxInterpreter.RenderAsElmExpression(
            expression.Evaluate().Extract(error => throw new Exception(error.ToString())))
            .expressionString.Should().Be("1");
    }

    [Theory]
    [InlineData("this is not valid syntax ]")]
    [InlineData("x = missingValue")]
    [InlineData("x = MissingModule.value")]
    [InlineData("String.nonexistent 1")]
    public void Failed_preparation_leaves_the_input_snapshot_usable(string submission)
    {
        var environment = Declare(s_library.Value, "x = 7");
        environment.PrepareSubmission(submission).IsErrOrNull().Should().NotBeNull();
        Evaluate(environment, "x").Should().Be("7");
    }

    [Fact]
    public void Shadowing_default_import_does_not_rebind_existing_library_declarations()
    {
        var environment = Declare(s_library.Value, "identity x = 999");
        Evaluate(environment, "identity 2").Should().Be("999");
        Evaluate(environment, "Basics.identity 2").Should().Be("2");
    }

    [Fact]
    public void Type_alias_and_constructor_submissions_keep_historical_references()
    {
        var type = Declare(s_library.Value, "type Item = Item Int");
        var first = Declare(type, "get (Item x) = x");
        var stored = Declare(first, "old = Item 12");
        var next = Declare(stored, "type Item = Different String");

        Evaluate(next, "get old").Should().Be("12");
        Evaluate(next, "Different \"new\"").Should().Be("Different \"new\"");

        var alias = Declare(next, "type alias Point = { x : Int, y : Int }");
        Evaluate(alias, "(Point 1 2).y").Should().Be("2");
    }

    [Fact]
    public void Leading_whitespace_declaration_and_local_shadowing_are_preserved()
    {
        var environment = Declare(s_library.Value, "       x = 7");
        Evaluate(environment, "let x = 8 in x").Should().Be("8");
        Evaluate(environment, "(\\x -> x) 9").Should().Be("9");
        Evaluate(environment, "x").Should().Be("7");
    }

    [Fact]
    public void Internal_expression_wrapper_cannot_capture_user_names()
    {
        s_library.Value.PrepareExpression("pine_interactive_expression").IsErrOrNull().Should().NotBeNull();
        var environment = Declare(s_library.Value, "pine_interactive_expression = 42");
        Evaluate(environment, "pine_interactive_expression").Should().Be("42");
    }

    [Fact]
    public void Generated_submission_module_names_do_not_replace_app_modules()
    {
        var environment =
            ElmInterpreterEnvironment.Create(
                ["module PineInteractiveSubmission0 exposing (..)\nvalue = 42"])
            .Extract(error => throw new Exception(error));

        var next = Declare(environment, "value = 7");
        Evaluate(next, "PineInteractiveSubmission0.value").Should().Be("42");
        Evaluate(next, "value").Should().Be("7");
    }

    [Fact]
    public void Source_selection_honors_manifest_and_app_overrides_by_module_name()
    {
        var tree =
            Tree(
                ("elm.json", """{"type":"application","source-directories":["src"],"elm-version":"0.19.1","dependencies":{"direct":{},"indirect":{}}}"""),
                ("src\\CustomBasics.elm", "module Basics exposing (..)\nidentity x = x\n"),
                ("src\\Main.elm", "module Main exposing (..)\nvalue = Basics.identity 42\n"),
                ("ignored\\Broken.elm", "not a module"),
                ("src\\elm-stuff\\Broken.elm", "not a module"),
                ("src\\nested\\elm.json", "{}"),
                ("src\\nested\\Broken.elm", "not a module"));

        var environment =
            ElmInterpreterSourcePreparation.Prepare(tree)
            .Extract(error => throw new Exception(error));

        Evaluate(environment, "Main.value").Should().Be("42");
        environment.Modules.Keys.Count(name => name == "Basics").Should().Be(1);
        environment.Modules.Should().NotContainKey("Broken");
    }

    [Fact]
    public void Stable_declaration_identity_can_be_invoked_after_shadowing()
    {
        var first = s_library.Value.AddDeclaration("f n = n + 1").Extract(error => throw new Exception(error));
        var newest = first.Environment.AddDeclaration("f n = n + 100").Extract(error => throw new Exception(error));

        var oldResult =
            newest.Environment.Invoke(first.Name, [PineValueInProcess.CreateInteger(5)])
            .Extract(error => throw new Exception(error.ToString()));

        ElmSyntaxInterpreter.RenderAsElmExpression(oldResult).expressionString.Should().Be("6");
        Evaluate(newest.Environment, "f 5").Should().Be("105");
    }

    [Fact]
    public void Operator_binding_keeps_its_original_function_after_shadowing()
    {
        var first = Declare(s_library.Value, "append a b = a + b");
        var infix = Declare(first, "infix right 5 (++) = append");
        var old = Declare(infix, "old n = n ++ 1");
        var next = Declare(old, "append a b = 999");
        var rebound = Declare(next, "infix right 5 (++) = append");

        Evaluate(rebound, "[ old 4, 4 ++ 1 ]").Should().Be("[ 5, 999 ]");
    }

    [Fact]
    public void Expression_and_declaration_conveniences_keep_errors_explicit()
    {
        s_library.Value.AddDeclaration("42").IsErrOrNull().Should().Contain("declaration");
        s_library.Value.AddDeclaration("port send : String -> Cmd msg").IsErrOrNull().Should().Contain("Port");
        s_library.Value.AddDeclaration("infix right 5 (++) = missing").IsErrOrNull().Should().Contain("missing");
        s_library.Value.Evaluate("missing").IsErrOrNull()!.Message.Should().Contain("missing");
        s_library.Value.Evaluate("42").IsOkOrNull()!.AsInteger().Should().Be(new System.Numerics.BigInteger(42));
    }

    [Fact]
    public void Runtime_failure_does_not_change_existing_bindings()
    {
        var environment = Declare(s_library.Value, "x = 7");
        environment.Evaluate("case 2 of\n    1 -> 0").IsErrOrNull().Should().NotBeNull();
        Evaluate(environment, "x").Should().Be("7");
    }

    [Fact]
    public void Short_circuiting_is_preserved_in_prepared_submissions()
    {
        var environment = Declare(s_library.Value, "loop x = loop x");
        Evaluate(environment, "False && loop True").Should().Be("False");
        Evaluate(environment, "True || loop False").Should().Be("True");
    }

    [Fact]
    public void Test_sources_and_empty_root_selection_are_explicit()
    {
        var tree =
            Tree(
                ("elm.json", """{"type":"application","source-directories":["src"],"elm-version":"0.19.1","dependencies":{"direct":{},"indirect":{}}}"""),
                ("src\\Main.elm", "module Main exposing (..)\nvalue = 42"),
                ("tests\\Example.elm", "module Example exposing (..)\nvalue = 7"));

        var normal = ElmInterpreterSourcePreparation.Prepare(tree).Extract(error => throw new Exception(error));

        var tests =
            ElmInterpreterSourcePreparation.Prepare(tree, includeTests: true).Extract(
                error => throw new Exception(error));

        var empty =
            ElmInterpreterSourcePreparation.Prepare(tree, rootFilePaths: []).Extract(
                error => throw new Exception(error));

        normal.Modules.Should().NotContainKey("Example");
        tests.Modules.Should().ContainKey("Example");
        empty.Modules.Should().NotContainKey("Main");
        Evaluate(empty, "identity 2").Should().Be("2");
    }

    [Fact]
    public void Empty_and_parsed_module_inputs_can_be_used_without_bundled_sources()
    {
        var empty =
            ElmInterpreterEnvironment.Create(Array.Empty<string>()).Extract(error => throw new Exception(error));

        Evaluate(empty, "42").Should().Be("42");

        var syntax =
            Core.Elm.Syntax.ElmSyntaxParser.ParseModuleText("module Custom exposing (..)\nvalue = 42")
            .Extract(error => throw new Exception(error.ToString()));

        var environment = ElmInterpreterEnvironment.Create([syntax]).Extract(error => throw new Exception(error));
        Evaluate(environment, "Custom.value").Should().Be("42");
    }

    [Fact]
    public async Task Resolved_build_preparation_reuses_package_qualified_syntax()
    {
        var tree =
            Tree(
                ("elm.json",
                """
                {"type":"application","source-directories":["src"],"elm-version":"0.19.1",
                 "dependencies":{"direct":{"elm/core":"1.0.5"},"indirect":{}},"test-dependencies":{"direct":{},"indirect":{}}}
                """),
                ("src\\Main.elm", "module Main exposing (..)\nvalue = String.fromInt 42\n"));

        var build =
            await ElmResolvedBuildPreparation.PrepareForDeclarationDemandAsync(
                tree,
                [
                "elm.json"
                ],
                [
                [
                "src",
                "Main.elm"
                ]
                ],
                ElmPackageSubstitutions.DefaultBuild.Value with { Offline = true });

        var environment =
            ElmInterpreterSourcePreparation.PrepareResolved(build)
            .Extract(error => throw new Exception(error));

        Evaluate(environment, "Main.value").Should().Be("\"42\"");
        build.CompilerModuleSyntax.Should().NotBeEmpty();
    }

    [Theory]
    [InlineData("{}")]
    [InlineData("null")]
    [InlineData("""{"type":"application","source-directories":[],"dependencies":{}}""")]
    [InlineData("""{"type":"application","source-directories":["src"],"dependencies":{},"dependencies":{}}""")]
    [InlineData("""{"type":"application","source-directories":["../../outside"],"dependencies":{}}""")]
    public void Invalid_source_selection_cannot_silently_create_a_library_only_environment(string manifest)
    {
        ElmInterpreterSourcePreparation.Prepare(Tree(("elm.json", manifest)))
            .IsErrOrNull().Should().Contain("elm.json");
    }

    [Fact]
    public void Explicit_roots_keep_import_closure_but_not_unrelated_declarations()
    {
        var tree =
            Tree(
                ("src\\Main.elm", "module Main exposing (..)\nimport Helper\nvalue = Helper.value\n"),
                ("src\\Helper.elm", "module Helper exposing (..)\nvalue = 42\n"),
                ("src\\Unused.elm", "module Unused exposing (..)\nvalue = missing\n"));

        var environment =
            ElmInterpreterSourcePreparation.Prepare(tree, [["src", "Main.elm"]])
            .Extract(error => throw new Exception(error));

        Evaluate(environment, "Main.value").Should().Be("42");
        environment.Modules.Should().ContainKey("Helper");
        environment.Modules.Should().NotContainKey("Unused");
    }

    [Theory]
    [InlineData("module Broken exposing (..)\nvalue =")]
    [InlineData("module Broken exposing (..)\nvalue = 1\n\nbad =")]
    public void Recovered_module_errors_are_explicit_failures(string text)
    {
        ElmInterpreterSourcePreparation.Prepare(Tree(("Broken.elm", text)))
            .IsErrOrNull().Should().NotBeNull();
    }

    [Fact]
    public void Missing_roots_duplicate_modules_and_bad_manifests_are_explicit_failures()
    {
        ElmInterpreterSourcePreparation.Prepare(FileTree.EmptyTree, [["Missing.elm"]])
            .IsErrOrNull().Should().Contain("Missing.elm");

        ElmInterpreterSourcePreparation.Prepare(
            Tree(("One.elm", "module One exposing (..)\nx = 1"), ("Two.elm", "module One exposing (..)\nx = 2")))
            .IsErrOrNull().Should().Contain("Duplicate");

        ElmInterpreterSourcePreparation.Prepare(Tree(("elm.json", "{")))
            .IsErrOrNull().Should().Contain("elm.json");
    }
}
