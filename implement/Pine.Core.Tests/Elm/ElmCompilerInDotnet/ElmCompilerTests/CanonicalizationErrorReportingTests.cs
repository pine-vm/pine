using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.SyntaxModel;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class CanonicalizationErrorReportingTests
{
    private static DeclQualifiedName Name(string module, string declaration) =>
        DeclQualifiedName.Create(module.Split('.'), declaration);

    [Fact]
    public void Same_module_dependencies_and_cached_references_retain_every_declaration_and_location()
    {
        var sources =
            TestCase.FileTreeFromElmModulesWithoutPackages(
                [
                    """
                    module Main exposing (root)
                    root = middle
                    middle = leaf
                    leaf = missing
                    """
                ]);

        var parsing = new Dictionary<string, Result<Core.Elm.ElmSyntax.ElmSyntaxParseError, SyntaxTypes.File>>();

        var cache =
            new Dictionary<(SyntaxTypes.Node<SyntaxTypes.Declaration> Declaration, string Scope),
            DeclarationCanonicalizationCacheEntry>();

        var cold =
            ElmCompiler.CompileInteractiveEnvironment(
                sources,
                [Name("Main", "root")],
                sourceParseCache: parsing,
                canonicalizationCache: cache).IsErrOrNull();

        var warm =
            ElmCompiler.CompileInteractiveEnvironment(
                sources,
                [Name("Main", "root")],
                sourceParseCache: parsing,
                canonicalizationCache: cache).IsErrOrNull();

        warm.Should().Be(cold);

        warm.Should().Contain("1. Main.root (compilation root)")
            .And.Contain("2. Main.middle — referenced by Main.root")
            .And.Contain("3. Main.leaf — referenced by Main.middle")
            .And.Contain("value reference 'Main.middle' at src/Main.elm:2:8")
            .And.Contain("value reference 'Main.leaf' at src/Main.elm:3:10")
            .And.Contain("in declaration 'Main.leaf'");
    }

    [Fact]
    public void Missing_qualified_member_explains_alias_target_source_and_scope()
    {
        var sources =
            TestCase.FileTreeFromElmModulesWithoutPackages(
                [
                    """
                    module Main exposing (root)
                    import Supplied as D
                    root = helper
                    helper = D.merge
                    """,
                    """
                    module Supplied exposing (known)
                    known = 42
                    """
                ]);

        var error = ElmCompiler.CompileInteractiveEnvironment(sources, [Name("Main", "root")]).IsErrOrNull();

        error.Should().Contain("Cannot resolve reference 'D.merge'")
            .And.Contain("in declaration 'Main.helper'")
            .And.Contain("The supplied module 'Supplied' has no value declaration named 'merge'")
            .And.Contain("Target source: src/Supplied.elm:1:1")
            .And.Contain("Reference scope: import at src/Main.elm:2:1")
            .And.Contain("as D");
    }

    [Fact]
    public void Private_members_are_distinguished_from_missing_members()
    {
        var sources =
            TestCase.FileTreeFromElmModulesWithoutPackages(
                [
                    "module Main exposing (root)\nimport Supporting\nroot = Supporting.hidden",
                    "module Supporting exposing (visible)\nvisible = 1\nhidden = 42"
                ]);

        ElmCompiler.CompileInteractiveEnvironment(sources, [Name("Main", "root")]).IsErrOrNull()
            .Should().Contain("declares value 'hidden', but does not expose it")
            .And.Contain("Target source: src/Supporting.elm:3:1");
    }

    [Fact]
    public void Type_and_constructor_dependencies_identify_their_owning_declarations()
    {
        var support =
            """
            module Supporting exposing (Status(..))
            import Missing
            type Status = Ready Payload
            type alias Payload = Missing.Type
            """;

        var typed =
            TestCase.FileTreeFromElmModulesWithoutPackages(
                [
                    "module Main exposing (root)\nimport Supporting\nroot : Supporting.Status -> Int\nroot _ = 42",
                    support
                ]);

        ElmCompiler.CompileInteractiveEnvironment(typed, [Name("Main", "root")]).IsErrOrNull()
            .Should().Contain("2. Supporting.Status — referenced by Main.root")
            .And.Contain("type reference 'Supporting.Status' at src/Main.elm:3:")
            .And.Contain("3. Supporting.Payload — referenced by Supporting.Status")
            .And.Contain("Cannot resolve type reference 'Missing.Type'");

        var patterned =
            TestCase.FileTreeFromElmModulesWithoutPackages(
                [
                    "module Main exposing (root)\nimport Supporting\nroot (Supporting.Ready value) = value",
                    support
                ]);

        ElmCompiler.CompileInteractiveEnvironment(patterned, [Name("Main", "root")]).IsErrOrNull()
            .Should().Contain("2. Supporting.Status — referenced by Main.root")
            .And.Contain("value reference 'Supporting.Ready' at src/Main.elm:3:")
            .And.Contain("the referenced symbol belongs to this declaration");
    }

    [Fact]
    public void Operator_references_demand_the_mapped_function_and_retain_the_operator_location()
    {
        var sources =
            TestCase.FileTreeFromElmModulesWithoutPackages(
                [
                    "module Main exposing (root)\nimport Operators exposing ((+))\nroot = 1 + 2",
                    """
                    module Operators exposing ((+))
                    infix left 5 (+) = combine
                    combine left right = helper left right
                    helper left right = missing
                    """
                ]);

        ElmCompiler.CompileInteractiveEnvironment(sources, [Name("Main", "root")]).IsErrOrNull()
            .Should().Contain("2. Operators.combine — referenced by Main.root")
            .And.Contain("value reference 'Operators.combine' at src/Main.elm:3:10")
            .And.Contain("3. Operators.helper — referenced by Operators.combine");
    }

    [Fact]
    public void Cached_lookup_details_change_when_a_previously_missing_module_becomes_available()
    {
        var cache =
            new Dictionary<(SyntaxTypes.Node<SyntaxTypes.Declaration> Declaration, string Scope),
            DeclarationCanonicalizationCacheEntry>();

        var main = "module Main exposing (root)\nimport Missing\nroot = Missing.value";
        var absent = TestCase.FileTreeFromElmModulesWithoutPackages([main]);
        var present = TestCase.FileTreeFromElmModulesWithoutPackages([main, "module Missing exposing (..)\n"]);

        ElmCompiler.CompileInteractiveEnvironment(absent, [Name("Main", "root")], canonicalizationCache: cache)
            .IsErrOrNull()
            .Should().Contain("Module 'Missing' is unavailable in the supplied sources");

        ElmCompiler.CompileInteractiveEnvironment(present, [Name("Main", "root")], canonicalizationCache: cache)
            .IsErrOrNull()
            .Should().Contain("The supplied module 'Missing' has no value declaration named 'value'")
            .And.Contain("Target source: src/Missing.elm:1:1");
    }

    [Fact]
    public void Parse_failures_explain_why_a_requested_or_referenced_declaration_is_unavailable()
    {
        var brokenRoot =
            TestCase.FileTreeFromElmModulesWithoutPackages(
                ["module Main exposing (root)\nroot ="]);

        ElmCompiler.CompileInteractiveEnvironment(brokenRoot, [Name("Main", "root")]).IsErrOrNull()
            .Should().Contain("Cannot canonicalize root declaration 'Main.root'")
            .And.Contain("src/Main.elm").And.Contain("failed to parse");

        var brokenDependency =
            TestCase.FileTreeFromElmModulesWithoutPackages(
                [
                    "module Main exposing (root)\nimport Supporting\nroot = Supporting.value",
                    "module Supporting exposing (value)\nvalue ="
                ]);

        ElmCompiler.CompileInteractiveEnvironment(brokenDependency, [Name("Main", "root")]).IsErrOrNull()
            .Should().Contain("in declaration 'Main.root'")
            .And.Contain("src/Supporting.elm").And.Contain("failed to parse");
    }

    [Fact]
    public void Multiple_roots_do_not_attribute_failure_to_an_unrelated_root()
    {
        var sources =
            TestCase.FileTreeFromElmModulesWithoutPackages(
                ["module Main exposing (..)\naGoodRoot = 42\nzBadRoot = leaf\nleaf = missing"]);

        var error =
            ElmCompiler.CompileInteractiveEnvironment(
                sources,
                [Name("Main", "aGoodRoot"), Name("Main", "zBadRoot")]).IsErrOrNull();

        error.Should().Contain("1. Main.zBadRoot (compilation root)").And.NotContain("Main.aGoodRoot");
    }

    [Fact]
    public void Effect_module_command_stub_compiles_and_crashes_if_invoked()
    {
        var rootModule =
            """
            module Root exposing (main)

            import Task

            main value =
                Task.perform value
            """;

        var taskModule =
            """
            effect module Task where { command = MyCmd, subscription = MySub } exposing (perform, attempt)

            type MyCmd msg = MyCmd

            type MySub msg = MySub

            perform value =
                command value

            attempt value =
                subscription value
            """;

        var appCodeTree =
            TestCase.FileTreeFromElmModulesWithoutPackages([rootModule, taskModule]);

        IReadOnlyList<IReadOnlyList<string>> rootFilePaths =
            [["src", "Root.elm"]];

        var result =
            ElmCompilerTestHelper.CompileInteractiveEnvironmentFromFiles(
                appCodeTree,
                rootFilePaths);

        var compilation = result.Extract(error => throw new Exception(error));

        var effectModuleDeclarationNames =
            compilation.pipelineStageResults.Canonicalized
            .Single(module => module.ModuleDefinition.Value is SyntaxTypes.Module.EffectModule)
            .Declarations
            .Select(declaration => declaration.Value)
            .OfType<SyntaxTypes.Declaration.FunctionDeclaration>()
            .Select(declaration => declaration.Function.Declaration.Value.Name.Value);

        effectModuleDeclarationNames.Should().Contain("command");
        // The unused effect operation is not demanded merely by importing Task.
        effectModuleDeclarationNames.Should().NotContain("subscription");

        var parsedEnvironment =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compilation.compiledEnvValue)
            .Extract(error => throw new Exception(error));

        var rootValue =
            parsedEnvironment.Modules
            .Single(module => module.moduleName is "Root")
            .moduleContent.FunctionDeclarations["main"];

        var rootFunction =
            FunctionRecord.ParseFunctionRecordTagged(rootValue, new PineVMParseCache())
            .Extract(error => throw new Exception(error));

        var invokeCommand =
            ElmCompilerTestHelper.CreateFunctionInvocationDelegate(rootFunction);

        Action invokeStub =
            () => invokeCommand([PineValue.EmptyList]).evalReport.ReturnValue.Evaluate();

        invokeStub.Should()
            .Throw<NotImplementedException>()
            .WithMessage("*effect_module_command_stub_reached*");
    }

    [Fact]
    public void Unresolved_reference_reports_source_location_and_dependency_chain()
    {
        var rootModule =
            """
            module Root exposing (main)

            import Intermediate

            main =
                Intermediate.value
            """;

        var intermediateModule =
            """
            module Intermediate exposing (value)

            import Broken as Dependency exposing (value)

            value =
                Dependency.value
            """;

        var brokenModule =
            """
            module Broken exposing (value)

            value =
                command
            """;

        var appCodeTree =
            TestCase.FileTreeFromElmModulesWithoutPackages(
                [rootModule, intermediateModule, brokenModule]);

        IReadOnlyList<IReadOnlyList<string>> rootFilePaths =
            [["src", "Root.elm"]];

        var result =
            ElmCompilerTestHelper.CompileInteractiveEnvironmentFromFiles(
                appCodeTree,
                rootFilePaths);

        var error = result.IsErrOrNull();

        error.Should().NotBeNull();
        error.Should().Contain("Cannot resolve reference 'command' at src/Broken.elm:4:5-4:12");
        error.Should().Contain("in declaration 'Broken.value'");
        error.Should().Contain("Declaration dependency chain from a compilation root (references, not module imports):");
        error.Should().Contain("1. Root.main (compilation root)");
        error.Should().Contain("2. Intermediate.value — referenced by Root.main");
        error.Should().Contain("value reference 'Intermediate.value' at src/Root.elm:6:5-6:23");

        error.Should().Contain("3. Broken.value — referenced by Intermediate.value");

        error.Should().Contain("scope provided by import at src/Intermediate.elm:3:1").And.Contain(
            "as Dependency exposing (value)");

        error.Should().Contain("No local binding, value declaration, or exposed import provides 'command'");
        error.Should().NotContain("because that module is reachable");
    }

    [Fact]
    public void Missing_native_function_reports_declaration_dependency_chain()
    {
        var rootModule =
            """
            module Root exposing (main)

            import Intermediate

            main =
                Intermediate.value
            """;

        var intermediateModule =
            """
            module Intermediate exposing (value)

            import Constants

            value =
                Constants.maxDigitValue
            """;

        var constantsModule =
            """
            module Constants exposing (maxDigitValue)

            maxDigitValue =
                Basics.sin 8
            """;

        var appCodeTree =
            TestCase.FileTreeFromElmModulesWithoutPackages(
                [rootModule, intermediateModule, constantsModule]);

        IReadOnlyList<IReadOnlyList<string>> rootFilePaths =
            [["src", "Root.elm"]];

        var result =
            ElmCompilerTestHelper.CompileInteractiveEnvironmentFromFiles(
                appCodeTree,
                rootFilePaths);

        var error = result.IsErrOrNull();

        error.Should().NotBeNull();
        error.Should().Contain("Failed to compile declaration 'Constants.maxDigitValue'");

        error.Should().Contain(
            "Reason: Cannot compile reference 'Basics.sin': no executable implementation is available");

        error.Should().Contain("Declaration dependency chain from a compilation root (references, not module imports):");
        error.Should().Contain("1. Root.main (compilation root)");
        error.Should().Contain("2. Intermediate.value — referenced by Root.main");
        error.Should().Contain("3. Constants.maxDigitValue — referenced by Intermediate.value");

        error.Should().Contain("declaration at src/Constants.elm:3:1");
        error.Should().Contain("value reference 'Constants.maxDigitValue' at src/Intermediate.elm:6:5");
        error.Should().NotContain("dependency layout");
    }
}
