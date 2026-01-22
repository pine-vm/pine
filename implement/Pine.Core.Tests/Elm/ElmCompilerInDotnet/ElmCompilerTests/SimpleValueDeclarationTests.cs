using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class SimpleValueDeclarationTests
{
    [Fact]
    public void Simple_integer_value_declaration()
    {
        var elmModuleText =
            """
            module Test exposing (..)
            
            alfa = 123
            
            """;

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: _ => true,
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.alfa =
                123

            """"
            .Trim());
    }

    [Fact]
    public void Simple_string_value_declaration()
    {
        var elmModuleText =
            """
            module Test exposing (..)
            
            alfa =
                "testing literal"
            
            """;

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: _ => true,
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.alfa =
                "testing literal"

            """"
            .Trim());
    }


    [Fact]
    public void Simple_boolean_value_declaration()
    {
        var elmModuleText =
            """
            module Test exposing (..)
            
            alfa =
                True
            
            """;

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: _ => true,
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.alfa =
                True

            """"
            .Trim());
    }

    [Fact]
    public void Two_simple_value_declarations()
    {
        var elmModuleText =
            """
            module Test exposing (..)
            
            alfa = 123

            beta = 456
            
            """;

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: _ => true,
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.alfa =
                123


            Test.beta =
                456

            """"
            .Trim());
    }
}
