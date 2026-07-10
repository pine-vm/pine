using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using System.Linq;
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

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: name => name.Namespaces.Count > 0 && name.Namespaces[0] is "Test",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"
            Test.alfa =
                123

            """"
            .Trim());
    }

    [Fact]
    public void Simple_integer_value_declaration_is_emitted_as_plain_value()
    {
        var elmModuleText =
            """
            module Test exposing (..)
            
            alfa = 123
            
            """;

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var value =
            parsedEnv.Modules
            .Single(module => module.moduleName is "Test")
            .moduleContent.FunctionDeclarations["alfa"];

        value.Should().Be(IntegerEncoding.EncodeSignedInteger(123));
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

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: name => name.Namespaces.Count > 0 && name.Namespaces[0] is "Test",
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

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: name => name.Namespaces.Count > 0 && name.Namespaces[0] is "Test",
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

        var (parsedEnv, _) =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: name => name.Namespaces.Count > 0 && name.Namespaces[0] is "Test",
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
