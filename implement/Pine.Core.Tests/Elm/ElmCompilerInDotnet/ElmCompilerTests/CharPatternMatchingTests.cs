using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class CharPatternMatchingTests
{
    [Fact]
    public void Case_of_char_pattern_matching()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl ch =
                case ch of
                    'a' ->
                        1

                    'b' ->
                        2

                    'z' ->
                        26

                    _ ->
                        0

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
            var charAResult =
                ResultAsExpressionString(ElmValueEncoding.ElmCharAsPineValue('a'));

            charAResult.Should().Be("1");
        }

        {
            var charBResult =
                ResultAsExpressionString(ElmValueEncoding.ElmCharAsPineValue('b'));

            charBResult.Should().Be("2");
        }

        {
            var charZResult =
                ResultAsExpressionString(ElmValueEncoding.ElmCharAsPineValue('z'));

            charZResult.Should().Be("26");
        }

        {
            var charXResult =
                ResultAsExpressionString(ElmValueEncoding.ElmCharAsPineValue('x'));

            charXResult.Should().Be("0");
        }
    }

    [Fact]
    public void Char_pattern_with_const_items_optimized()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl ch =
                case ch of
                    'a' ->
                        1

                    'b' ->
                        2

                    _ ->
                        0

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
                        , 'a'
                        ]
                then
                    1

                else if
                    Pine_builtin.equal
                        [ param_1_0
                        , 'b'
                        ]
                then
                    2

                else
                    0

            """"
            .Trim());
    }
}
