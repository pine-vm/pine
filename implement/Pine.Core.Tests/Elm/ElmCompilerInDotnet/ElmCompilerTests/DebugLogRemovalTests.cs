using AwesomeAssertions;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm.ElmCompilerInDotnet;
using Pine.Core.Tests.Elm.ElmCompilerTests;
using System;
using System.Linq;
using Xunit;

using SyntaxTypes = Pine.Core.Elm.ElmSyntax.ElmSyntaxAbstract;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class DebugLogRemovalTests
{
    [Fact]
    public void Fully_applied_call_is_replaced_with_value_argument()
    {
        var valueArgument = SyntaxTypes.Expression.StringLiteral.Create("value");

        var application =
            new SyntaxTypes.Expression.Application(
                SyntaxTypes.Expression.Identifier.Create(["Debug"], "log"),
                [
                SyntaxTypes.Expression.StringLiteral.Create("message"),
                valueArgument
                ]);

        var rewritten = DebugLogRemoval.RewriteExpression(application);

        rewritten.Should().BeSameAs(valueArgument);
    }

    [Fact]
    public void Partial_application_is_preserved()
    {
        var application =
            new SyntaxTypes.Expression.Application(
                SyntaxTypes.Expression.Identifier.Create(["Debug"], "log"),
                [SyntaxTypes.Expression.StringLiteral.Create("message")]);

        var rewritten = DebugLogRemoval.RewriteExpression(application);

        rewritten.Should().Be(application);
    }

    [Fact]
    public void Nested_fully_applied_call_is_replaced_with_value_argument()
    {
        var valueArgument = SyntaxTypes.Expression.StringLiteral.Create("value");

        var application =
            new SyntaxTypes.Expression.Application(
                new SyntaxTypes.Expression.Application(
                    SyntaxTypes.Expression.Identifier.Create(["Debug"], "log"),
                    [SyntaxTypes.Expression.StringLiteral.Create("message")]),
                [valueArgument]);

        var rewritten = DebugLogRemoval.RewriteExpression(application);

        rewritten.Should().BeSameAs(valueArgument);
    }

    [Fact]
    public void Application_after_logged_function_value_is_preserved()
    {
        var functionArgument =
            SyntaxTypes.Expression.Identifier.Create(["Test"], "function");

        var finalArgument = SyntaxTypes.Expression.StringLiteral.Create("argument");

        var application =
            new SyntaxTypes.Expression.Application(
                SyntaxTypes.Expression.Identifier.Create(["Debug"], "log"),
                [
                SyntaxTypes.Expression.StringLiteral.Create("message"),
                functionArgument,
                finalArgument
                ]);

        var rewritten =
            DebugLogRemoval.RewriteExpression(application)
            .Should().BeOfType<SyntaxTypes.Expression.Application>().Subject;

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
            ElmCompiler.LowerToElmSyntaxForCompilation(
                testCase.AsFileTree(),
                rootFilePaths: [["src", "Test.elm"]],
                syntaxOptimization:
                new ElmSyntaxOptimizationConfig.SyntaxOptimizationEnabled
                {
                    RemoveDebugLogApplications = false
                })
            .Extract(err => throw new Exception(err));

        var loggedDeclaration =
            loweringResult.Lowered.LambdaLifted
            .SelectMany(module => module.Declarations)
            .OfType<SyntaxTypes.Declaration.FunctionDeclaration>()
            .Single(declaration => declaration.Function.Declaration.Name is "logged");

        var application =
            loggedDeclaration.Function.Declaration.Expression
            .Should().BeOfType<SyntaxTypes.Expression.Application>().Subject;

        var identifier =
            application.Function
            .Should().BeOfType<SyntaxTypes.Expression.Identifier>().Subject;

        identifier.QualifiedName.FullName.Should().Be("Debug.log");
        application.Arguments.Should().HaveCount(2);
    }
}
