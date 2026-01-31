using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class IntPatternMatchingTests
{
    [Fact]
    public void Case_of_int_pattern_matching()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl n =
                case n of
                    0 ->
                        "zero"

                    1 ->
                        "one"

                    42 ->
                        "forty-two"

                    100 ->
                        "one hundred"

                    _ ->
                        "other"

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
            var zeroResult =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(0));

            zeroResult.Should().Be("\"zero\"");
        }

        {
            var oneResult =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(1));

            oneResult.Should().Be("\"one\"");
        }

        {
            var fortyTwoResult =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(42));

            fortyTwoResult.Should().Be("\"forty-two\"");
        }

        {
            var oneHundredResult =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(100));

            oneHundredResult.Should().Be("\"one hundred\"");
        }

        {
            var otherResult =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(50));

            otherResult.Should().Be("\"other\"");
        }
    }

    [Fact]
    public void Case_of_hex_pattern_matching()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl n =
                case n of
                    0x00 ->
                        "zero"

                    0x0F ->
                        "fifteen"

                    0xFF ->
                        "two-fifty-five"

                    _ ->
                        "other"

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
            var zeroResult =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(0x00));

            zeroResult.Should().Be("\"zero\"");
        }

        {
            var fifteenResult =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(0x0F));

            fifteenResult.Should().Be("\"fifteen\"");
        }

        {
            var twoFiftyFiveResult =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(0xFF));

            twoFiftyFiveResult.Should().Be("\"two-fifty-five\"");
        }

        {
            var otherResult =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(0x10));

            otherResult.Should().Be("\"other\"");
        }
    }

    [Fact]
    public void Int_pattern_with_const_items_optimized()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl n =
                case n of
                    0 ->
                        "zero"

                    42 ->
                        "forty-two"

                    _ ->
                        "other"

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
                        , 0
                        ]
                then
                    "zero"

                else if
                    Pine_builtin.equal
                        [ param_1_0
                        , 42
                        ]
                then
                    "forty-two"

                else
                    "other"

            """"
            .Trim());
    }
}
