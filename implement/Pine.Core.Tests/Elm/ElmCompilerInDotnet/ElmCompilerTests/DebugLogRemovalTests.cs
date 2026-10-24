using AwesomeAssertions;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Elm.Syntax.Abstract;
using Pine.Core.Encodings;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class DebugLogRemovalTests
{
    [Fact]
    public void Fully_applied_call_is_replaced_with_value_argument()
    {
        var valueArgument = Expression.StringLiteral.Create("value");

        var application =
            new Expression.Application(
                Expression.Identifier.Create(["Debug"], "log"),
                [
                Expression.StringLiteral.Create("message"),
                valueArgument
                ]);

        var rewritten = DebugLogRemoval.RewriteExpression(application);

        rewritten.Should().BeSameAs(valueArgument);
    }

    [Fact]
    public void Partial_application_is_preserved()
    {
        var application =
            new Expression.Application(
                Expression.Identifier.Create(["Debug"], "log"),
                [Expression.StringLiteral.Create("message")]);

        var rewritten = DebugLogRemoval.RewriteExpression(application);

        rewritten.Should().Be(application);
    }

    [Fact]
    public void Nested_fully_applied_call_is_replaced_with_value_argument()
    {
        var valueArgument = Expression.StringLiteral.Create("value");

        var application =
            new Expression.Application(
                new Expression.Application(
                    Expression.Identifier.Create(["Debug"], "log"),
                    [Expression.StringLiteral.Create("message")]),
                [valueArgument]);

        var rewritten = DebugLogRemoval.RewriteExpression(application);

        rewritten.Should().BeSameAs(valueArgument);
    }

    [Fact]
    public void Application_after_logged_function_value_is_preserved()
    {
        var functionArgument =
            Expression.Identifier.Create(["Test"], "function");

        var finalArgument = Expression.StringLiteral.Create("argument");

        var application =
            new Expression.Application(
                Expression.Identifier.Create(["Debug"], "log"),
                [
                Expression.StringLiteral.Create("message"),
                functionArgument,
                finalArgument
                ]);

        var rewritten =
            DebugLogRemoval.RewriteExpression(application)
            .Should().BeOfType<Expression.Application>().Subject;

        rewritten.Function.Should().BeSameAs(functionArgument);
        rewritten.Arguments.Should().ContainSingle().Which.Should().BeSameAs(finalArgument);
    }

    [Fact]
    public void Compiler_removes_Debug_log_by_default()
    {
        var elmModuleText =
            """
            module Test exposing (..)

            logged =
                Debug.log "message" (41 + 1)
            """;

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false).parsedEnv;

        var value =
            parsedEnv.Modules
            .Single(module => module.moduleName is "Test")
            .moduleContent.FunctionDeclarations["logged"];

        value.Should().Be(IntegerEncoding.EncodeSignedInteger(42));
    }

    [Fact]
    public void Compiler_configuration_can_preserve_Debug_log()
    {
        var elmModuleText =
            """
            module Test exposing (..)

            logged =
                Debug.log "message" 42
            """;

        var testCase = TestCase.DefaultAppWithoutPackages([elmModuleText]);

        var loweringResult =
            ElmCompiler.LowerToCompilerModel(
                testCase.AsFileTree(),
                rootDeclarations: [Core.CodeAnalysis.DeclQualifiedName.Create(["Test"], "logged")],
                syntaxOptimization:
                new ElmSyntaxOptimizationConfig.SyntaxOptimizationEnabled
                {
                    RemoveDebugLogApplications = false
                })
            .Extract(err => throw new Exception(err));

        var loggedDeclaration =
            loweringResult.CompilerProgram.Functions[
            new Core.Elm.ElmCompilerInDotnet.CompilerModel.FunctionId.Source(
                Core.CodeAnalysis.DeclQualifiedName.Create(["Test"], "logged"))];

        var application =
            loggedDeclaration.Body
            .Should().BeOfType<Core.Elm.ElmCompilerInDotnet.CompilerModel.Expression.Application>().Subject;

        var identifier =
            application.Function
            .Should().BeOfType<Core.Elm.ElmCompilerInDotnet.CompilerModel.Expression.ReferenceExpression>().Subject;

        identifier.Reference.Should().BeOfType<Core.Elm.ElmCompilerInDotnet.CompilerModel.Reference.Native>()
            .Which.Name.FullName.Should().Be("Debug.log");

        application.Arguments.Should().HaveCount(2);
    }
}
