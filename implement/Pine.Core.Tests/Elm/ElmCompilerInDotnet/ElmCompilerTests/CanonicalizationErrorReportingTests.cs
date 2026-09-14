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
            ElmCompiler.CompileInteractiveEnvironment(
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
        effectModuleDeclarationNames.Should().Contain("subscription");

        var parsedEnvironment =
            ElmInteractiveEnvironment.ParseInteractiveEnvironment(compilation.compiledEnvValue)
            .Extract(error => throw new Exception(error));

        var commandValue =
            parsedEnvironment.Modules
            .Single(module => module.moduleName is "Task")
            .moduleContent.FunctionDeclarations["command"];

        var commandFunction =
            FunctionRecord.ParseFunctionRecordTagged(commandValue, new PineVMParseCache())
            .Extract(error => throw new Exception(error));

        var invokeCommand =
            ElmCompilerTestHelper.CreateFunctionInvocationDelegate(commandFunction);

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
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths);

        var error = result.IsErrOrNull();

        error.Should().NotBeNull();
        error.Should().Contain("Cannot resolve reference 'command' at src/Broken.elm:4:5-4:12");
        error.Should().Contain("Dependency chain from a compilation root:");
        error.Should().Contain("1. Root (src/Root.elm)");
        error.Should().Contain("2. Intermediate (src/Intermediate.elm) — explicit import at 3:1");

        error.Should().Contain(
            "3. Broken (src/Broken.elm) — explicit import at 3:1 as Dependency exposing (value)");

        error.Should().Contain(
            "searched for 'command' while canonicalizing 'Broken' because that module is reachable from root 'Root'");
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
                Basics.sqrt 8
            """;

        var appCodeTree =
            TestCase.FileTreeFromElmModulesWithoutPackages(
                [rootModule, intermediateModule, constantsModule]);

        IReadOnlyList<IReadOnlyList<string>> rootFilePaths =
            [["src", "Root.elm"]];

        var result =
            ElmCompiler.CompileInteractiveEnvironment(
                appCodeTree,
                rootFilePaths);

        var error = result.IsErrOrNull();

        error.Should().NotBeNull();
        error.Should().Contain("Failed to compile declaration 'Constants.maxDigitValue'");
        error.Should().Contain("Reason: Function 'Basics.sqrt' not found in dependency layout");
        error.Should().Contain("Declaration dependency chain from a compilation root:");
        error.Should().Contain("1. Root.main (compilation root)");
        error.Should().Contain("2. Intermediate.value — referenced by Root.main");
        error.Should().Contain("3. Constants.maxDigitValue — referenced by Intermediate.value");

        error.Should().Contain(
            "searched for 'Basics.sqrt' while compiling 'Constants.maxDigitValue' because that declaration is reachable from compilation root 'Root.main'");
    }
}
