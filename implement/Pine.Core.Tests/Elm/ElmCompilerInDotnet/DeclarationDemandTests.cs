using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.ElmSyntax;
using Pine.Core.Files;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

using Syntax = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet;

public class DeclarationDemandTests
{
    private static FileTree Sources(params string[] modules) =>
        TestCase.DefaultAppWithoutPackages(modules).AsFileTree();

    private static DeclQualifiedName Name(string module, string declaration) =>
        DeclQualifiedName.Create(module.Split('.'), declaration);

    [Fact]
    public void Declaration_demand_ignores_unrelated_naming_errors_and_missing_imports()
    {
        var sources =
            Sources(
                """
                module Main exposing (..)
                import Supporting
                import Platform
                root = Supporting.used
                main = Platform.worker misspelled
                """,
                """
                module Supporting exposing (..)
                used = 42
                unused = misspelled
                """);

        var result =
            ElmCompiler.CompileInteractiveEnvironment(sources, [Name("Main", "root")])
            .Extract(error => throw new Exception(error));

        var environment =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(result.compiledEnvValue)
            .Extract(error => throw new Exception(error));

        environment.Modules.Should().ContainSingle();
        var rootModule = environment.Modules.Single();
        rootModule.moduleName.Should().Be("Main");

        rootModule.moduleContent.FunctionDeclarations.Should().ContainSingle()
            .Which.Key.Should().Be("root");

        result.pipelineStageResults.Lowered.FilteredDeclarations.Keys.Should()
            .NotContain(name => name.Equals(Name("Supporting", "unused")));

        var parseCache = new PineVMParseCache();

        var executable =
            parseCache.ParseExpression(rootModule.moduleContent.FunctionDeclarations["root"])
            .Extract(error => throw new Exception(error));

        var value =
            Core.Interpreter.DirectInterpreter.WithSharedEvalCache(
                parseCache,
                new Dictionary<Core.Interpreter.DirectInterpreter.EvalCacheEntryKey, PineValue>())
            .EvaluateExpression(executable, PineValue.EmptyList)
            .Extract(error => throw new Exception(error));

        value.Should().Be(IntegerEncoding.EncodeSignedInteger(42));
    }

    [Fact]
    public void Declaration_demand_resolves_available_names_despite_missing_wildcard_import()
    {
        var sources =
            Sources(
                """
                module Main exposing (root)
                import Good exposing (answer)
                import Missing exposing (..)
                root = answer
                """,
                """
                module Good exposing (answer)
                answer = 42
                """);

        ElmCompiler.CompileInteractiveEnvironment(sources, [Name("Main", "root")]).IsOkOrNullable().Should().NotBeNull();

        ElmCompiler.AnalyzeApplication(sources).Should()
            .Contain(diagnostic => diagnostic.Message.Contains("Missing", StringComparison.Ordinal));
    }

    [Fact]
    public void Declaration_demand_includes_signatures_aliases_and_constructor_patterns()
    {
        var sources =
            Sources(
                """
                module Main exposing (root)
                import Supporting
                root : Supporting.Status -> Int
                root (Supporting.Ready payload) = payload
                """,
                """
                module Supporting exposing (Status(..))
                type alias Payload = Int
                type Status = Ready Payload
                type alias Unused = Missing.Type
                """);

        var result =
            ElmCompiler.LowerToElmSyntaxForCompilation(sources, [Name("Main", "root")])
            .Extract(error => throw new Exception(error));

        var names = result.Lowered.FilteredDeclarations.Keys;
        names.Should().Contain(Name("Supporting", "Status"));
        names.Should().Contain(Name("Supporting", "Payload"));
        names.Should().NotContain(Name("Supporting", "Unused"));
    }

    [Fact]
    public void Declaration_demand_checks_complete_bodies_including_unused_local_bindings()
    {
        var sources =
            Sources(
                """
                module Main exposing (root)
                root =
                    let
                        unused = misspelledName
                    in
                    42
                """);

        var error = ElmCompiler.CompileInteractiveEnvironment(sources, [Name("Main", "root")]).IsErrOrNull();
        error.Should().NotBeNull();
        error.Should().Contain("misspelledName");
    }

    [Fact]
    public void Declaration_demand_missing_root_fails_and_empty_roots_do_not_analyze_sources()
    {
        var sources = Sources("module Main exposing (..)\nroot = misspelled");

        var error =
            ElmCompiler.CompileInteractiveEnvironment(
                sources,
                [Name("Main", "root"), Name("Main", "missing")]).IsErrOrNull();

        error.Should().NotBeNull();
        error.Should().Contain("missing");

        var empty =
            ElmCompiler.CompileInteractiveEnvironment(sources, [])
            .Extract(error => throw new Exception(error));

        empty.pipelineStageResults.Canonicalized.Should().BeEmpty();
        empty.compiledEnvValue.Should().Be(PineValue.EmptyList);
    }

    [Fact]
    public void Declaration_demand_compilation_does_not_evaluate_zero_parameter_roots()
    {
        var sources = Sources("module Main exposing (root)\nroot = root");

        ElmCompiler.CompileInteractiveEnvironment(
            sources,
            [Name("Main", "root")],
            syntaxOptimization: new ElmSyntaxOptimizationConfig.SyntaxOptimizationDisabled())
            .IsOkOrNullable().Should().NotBeNull();
    }

    [Fact]
    public void Declaration_demand_application_analysis_and_queries_report_independent_errors()
    {
        var sources =
            Sources(
                """
                module Main exposing (..)
                import Missing
                unused = misspelled
                wrong : String
                wrong = 42
                """,
                """
                module Disconnected exposing (..)
                unused = anotherMisspelling
                """);

        var diagnostics = ElmCompiler.AnalyzeApplication(sources);
        diagnostics.Should().Contain(diagnostic => diagnostic.Message.Contains("Missing", StringComparison.Ordinal));
        diagnostics.Should().Contain(diagnostic => diagnostic.Message.Contains("misspelled", StringComparison.Ordinal));

        diagnostics.Should().Contain(
            diagnostic => diagnostic.FilePath.EndsWith("Disconnected.elm", StringComparison.Ordinal));

        diagnostics.Should().Contain(
            diagnostic => diagnostic.Code == "elm-type" && diagnostic.Declaration == Name("Main", "wrong"));

        var query =
            ElmCompiler.QueryDeclaration(sources, Name("Main", "wrong"))
            .Extract(error => throw new Exception(error));

        query.Declaration.Range.Start.Row.Should().Be(4);
        query.Diagnostics.Should().Contain(diagnostic => diagnostic.Code == "elm-type");
    }

    [Fact]
    public void Declaration_demand_distinguishes_type_and_value_namespaces()
    {
        var sources =
            Sources(
                """
                module Main exposing (..)
                type Payload = Thunk Int
                type Tree = Payload Payload
                root : Tree -> Int
                root (Payload (Thunk value)) = value
                """);

        ElmCompiler.CompileInteractiveEnvironment(sources, [Name("Main", "root")])
            .IsOkOrNullable().Should().NotBeNull();

        ElmCompiler.AnalyzeApplication(sources).Should().BeEmpty();
    }

    [Fact]
    public void Declaration_demand_enforces_imports_and_qualified_export_visibility()
    {
        var sources =
            Sources(
                """
                module Main exposing (root)
                import Supporting
                root = Supporting.hidden
                """,
                """
                module Supporting exposing (visible)
                visible = 13
                hidden = 42
                """);

        ElmCompiler.CompileInteractiveEnvironment(sources, [Name("Main", "root")]).IsErrOrNull()
            .Should().Contain("Supporting.hidden");
    }

    [Fact]
    public void Declaration_demand_imported_types_are_not_shadowed_by_local_constructor_names()
    {
        var sources =
            Sources(
                """
                module Main exposing (root)
                import Supporting exposing (Payload)
                type Tree = Payload Int
                root : Payload -> Int
                root value = value
                """,
                """
                module Supporting exposing (Payload)
                type alias Payload = Int
                """);

        var result =
            ElmCompiler.LowerToElmSyntaxForCompilation(sources, [Name("Main", "root")])
            .Extract(error => throw new Exception(error));

        result.Lowered.FilteredDeclarations.Keys.Should().Contain(Name("Supporting", "Payload"))
            .And.NotContain(Name("Main", "Tree"));
    }

    [Fact]
    public void Declaration_demand_closed_type_exports_do_not_expose_private_constructors()
    {
        var sources =
            Sources(
                """
                module Main exposing (root)
                import Supporting
                root = Supporting.Secret 42
                """,
                """
                module Supporting exposing (Secret)
                type Secret = Secret Int
                """);

        ElmCompiler.CompileInteractiveEnvironment(sources, [Name("Main", "root")]).IsErrOrNull()
            .Should().Contain("Supporting.Secret");
    }

    [Fact]
    public void Declaration_demand_cache_reuses_bodies_and_invalidates_failed_lookups()
    {
        var original =
            ElmSyntaxParser.ParseModuleText(
                "module Main exposing (..)\nroot = missing\nother = 42")
            .Extract(error => throw new Exception(error.ToString()));

        var cache =
            new Dictionary<(Syntax.Node<Syntax.Declaration> Declaration, string Scope),
            DeclarationCanonicalizationCacheEntry>();

        var first =
            Canonicalization.CanonicalizeDeclarations([original], [Name("Main", "root")], cache: cache)
            .Extract(error => throw new Exception(error));

        var second =
            Canonicalization.CanonicalizeDeclarations(
                [original],
                [Name("Main", "root"), Name("Main", "other")],
                cache: cache)
            .Extract(error => throw new Exception(error));

        second.Declarations[Name("Main", "root")].Should().BeSameAs(first.Declarations[Name("Main", "root")]);

        var fixedSource =
            ElmSyntaxParser.ParseModuleText(
                "module Main exposing (..)\nroot = missing\nother = 42\nmissing = 7")
            .Extract(error => throw new Exception(error.ToString()));

        var corrected =
            Canonicalization.CanonicalizeDeclarations([fixedSource], [Name("Main", "root")], cache: cache)
            .Extract(error => throw new Exception(error));

        corrected.Declarations[Name("Main", "root")].Errors.Should().BeEmpty();
        corrected.Declarations.Keys.Should().Contain(Name("Main", "missing"));
    }
}
