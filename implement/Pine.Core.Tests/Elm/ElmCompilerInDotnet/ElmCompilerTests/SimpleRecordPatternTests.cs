using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class SimpleRecordPatternTests
{
    [Fact]
    public void Record_pattern_destructuring_three_fields()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            decl { alfa, gamma, beta } =
                [ alfa, beta, gamma ]

            """;

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

        string ApplyForArgumentAsExpressionString(PineValue argument)
        {
            var applyRunResult = ApplyForArgument(argument);

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(applyRunResult, null, null);

            var resultAsElmExpr =
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)));

            return resultAsElmExpr.expressionString;
        }

        // Helper to create a record with three fields: { alfa, beta, gamma }
        // Record encoding: ["Elm_Record", [[["alfa", alfaValue], ["beta", betaValue], ["gamma", gammaValue]]]]
        // Fields are sorted alphabetically
        PineValue CreateRecord(int alfa, int beta, int gamma)
        {
            return ElmValueEncoding.ElmRecordAsPineValue(
                [
                    ("alfa", IntegerEncoding.EncodeSignedInteger(alfa)),
                    ("beta", IntegerEncoding.EncodeSignedInteger(beta)),
                    ("gamma", IntegerEncoding.EncodeSignedInteger(gamma))
                ]);
        }

        // Test case: { alfa = 1, beta = 2, gamma = 3 } -> [1, 2, 3]
        // Note: decl { alfa, gamma, beta } = [ alfa, beta, gamma ]
        // So output is [alfa, beta, gamma] which with the input becomes [1, 2, 3]
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(1, 2, 3));

            result.Should().Be("[ 1, 2, 3 ]");
        }

        // Test case: { alfa = 10, beta = 20, gamma = 30 }
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(10, 20, 30));

            result.Should().Be("[ 10, 20, 30 ]");
        }

        // Test case: { alfa = -5, beta = 0, gamma = 100 }
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(-5, 0, 100));

            result.Should().Be("[ -5, 0, 100 ]");
        }
    }

    [Fact]
    public void Record_pattern_with_two_fields()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            decl { x, y } =
                Pine_builtin.int_add [ x, y ]

            """;

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

        string ApplyForArgumentAsExpressionString(PineValue argument)
        {
            var applyRunResult = ApplyForArgument(argument);

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(applyRunResult, null, null);

            var resultAsElmExpr =
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)));

            return resultAsElmExpr.expressionString;
        }

        PineValue CreateRecord(int x, int y)
        {
            return ElmValueEncoding.ElmRecordAsPineValue(
                [
                    ("x", IntegerEncoding.EncodeSignedInteger(x)),
                    ("y", IntegerEncoding.EncodeSignedInteger(y))
                ]);
        }

        // Test case: { x = 3, y = 5 } -> 8
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(3, 5));

            result.Should().Be("8");
        }

        // Test case: { x = 10, y = 20 } -> 30
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(10, 20));

            result.Should().Be("30");
        }

        // Test case: { x = -7, y = 12 } -> 5
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(-7, 12));

            result.Should().Be("5");
        }
    }

    [Fact]
    public void Record_pattern_single_field()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            decl { value } =
                Pine_builtin.int_mul [ value, 2 ]

            """;

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: false,
                includeDeclaration: qualifiedName => qualifiedName.FullName is "Test.decl",
                parseCache: parseCache);

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

        string ApplyForArgumentAsExpressionString(PineValue argument)
        {
            var applyRunResult = ApplyForArgument(argument);

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(applyRunResult, null, null);

            var resultAsElmExpr =
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)));

            return resultAsElmExpr.expressionString;
        }

        PineValue CreateRecord(int value)
        {
            return ElmValueEncoding.ElmRecordAsPineValue(
                [
                    ("value", IntegerEncoding.EncodeSignedInteger(value))
                ]);
        }

        // Test case: { value = 7 } -> 14
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(7));

            result.Should().Be("14");
        }

        // Test case: { value = 25 } -> 50
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(25));

            result.Should().Be("50");
        }
    }

    [Fact]
    public void Record_pattern_in_let_binding()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            decl arg =
                let
                    { a, b } =
                        arg
                in
                Pine_builtin.int_mul [ a, b ]

            """;

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: false,
                includeDeclaration: qualifiedName => qualifiedName.FullName is "Test.decl",
                parseCache: parseCache);

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

        string ApplyForArgumentAsExpressionString(PineValue argument)
        {
            var applyRunResult = ApplyForArgument(argument);

            var resultAsElmValue =
                ElmValueEncoding.PineValueAsElmValue(applyRunResult, null, null);

            var resultAsElmExpr =
                ElmValue.RenderAsElmExpression(
                    resultAsElmValue
                    .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err)));

            return resultAsElmExpr.expressionString;
        }

        PineValue CreateRecord(int a, int b)
        {
            return ElmValueEncoding.ElmRecordAsPineValue(
                [
                    ("a", IntegerEncoding.EncodeSignedInteger(a)),
                    ("b", IntegerEncoding.EncodeSignedInteger(b))
                ]);
        }

        // Test case: { a = 3, b = 4 } -> 12
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(3, 4));

            result.Should().Be("12");
        }

        // Test case: { a = 5, b = 6 } -> 30
        {
            var result =
                ApplyForArgumentAsExpressionString(CreateRecord(5, 6));

            result.Should().Be("30");
        }
    }
}
