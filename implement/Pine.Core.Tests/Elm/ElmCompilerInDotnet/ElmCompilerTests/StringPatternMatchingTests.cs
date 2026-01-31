using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class StringPatternMatchingTests
{
    [Fact]
    public void Case_of_string_pattern_matching()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl : String -> Int
            decl s =
                case s of
                    "" ->
                        0

                    "hello" ->
                        1

                    "world" ->
                        2

                    "foo bar" ->
                        3

                    _ ->
                        -1

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "decl");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        PineValue ApplyForArgument(PineValue argument)
        {
            var (applyRunResult, _) = invokeFunction([argument]);

            return applyRunResult.ReturnValue.Evaluate();
        }

        string ResultAsExpressionString(PineValue argument)
        {
            var applyResult = ApplyForArgument(argument);

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(applyResult, null, null);

            var resultAsElmExpr =
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)));

            return resultAsElmExpr.expressionString;
        }

        {
            var emptyResult =
                ResultAsExpressionString(ElmValueEncoding.StringAsPineValue(""));

            emptyResult.Should().Be("0");
        }

        {
            var helloResult =
                ResultAsExpressionString(ElmValueEncoding.StringAsPineValue("hello"));

            helloResult.Should().Be("1");
        }

        {
            var worldResult =
                ResultAsExpressionString(ElmValueEncoding.StringAsPineValue("world"));

            worldResult.Should().Be("2");
        }

        {
            var fooBarResult =
                ResultAsExpressionString(ElmValueEncoding.StringAsPineValue("foo bar"));

            fooBarResult.Should().Be("3");
        }

        {
            var otherResult =
                ResultAsExpressionString(ElmValueEncoding.StringAsPineValue("something else"));

            otherResult.Should().Be("-1");
        }
    }

    [Fact]
    public void String_pattern_with_const_items_optimized()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl : String -> Int
            decl s =
                case s of
                    "" ->
                        0

                    "hello" ->
                        1

                    _ ->
                        -1

            """";

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.FullName is "Test.decl",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.decl param_1_0 =
                if
                    Pine_builtin.equal
                        [ param_1_0
                        , ""
                        ]
                then
                    0

                else if
                    Pine_builtin.equal
                        [ param_1_0
                        , "hello"
                        ]
                then
                    1

                else
                    -1

            """"
            .Trim());
    }
}
