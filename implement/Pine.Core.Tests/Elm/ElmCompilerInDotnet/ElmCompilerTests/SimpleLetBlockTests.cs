using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class SimpleLetBlockTests
{
    [Fact]
    public void Let_block_reusing_single_constant()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                let
                    alfa =
                        41
                in
                [ Pine_builtin.int_add
                    [ alfa
                    , arg
                    ]
                , Pine_builtin.int_mul
                    [ alfa
                    , arg
                    ]
                ]

            """";

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
            return applyRunResult;
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
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(1));

            resultExprString.Should().Be("[ 42, 41 ]");
        }

        {
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(2));

            resultExprString.Should().Be("[ 43, 82 ]");
        }

        {
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(-1));
            resultExprString.Should().Be("[ 40, -41 ]");
        }
    }

    [Fact]
    public void Consecutive_let_blocks()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                let
                    alfa =
                        19
                in
                let
                    beta =
                        Pine_builtin.int_add
                            [ alfa
                            , 1
                            ]
                in
                [ Pine_builtin.int_add
                    [ alfa
                    , arg
                    ]
                , Pine_builtin.int_mul
                    [ beta
                    , arg
                    ]
                ]

            """";

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
            return applyRunResult;
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
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(1));

            resultExprString.Should().Be("[ 20, 20 ]");
        }

        {
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(2));

            resultExprString.Should().Be("[ 21, 40 ]");
        }

        {
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(-1));

            resultExprString.Should().Be("[ 18, -20 ]");
        }
    }

    [Fact]
    public void Let_declarations_out_of_order()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                let
                    beta =
                        Pine_builtin.int_add
                            [ alfa
                            , 1
                            ]

                    alfa =
                        19
                in
                [ Pine_builtin.int_add
                    [ alfa
                    , arg
                    ]
                , Pine_builtin.int_mul
                    [ beta
                    , arg
                    ]
                ]

            """";

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
            return applyRunResult;
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
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(1));
            resultExprString.Should().Be("[ 20, 20 ]");
        }

        {
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(2));
            resultExprString.Should().Be("[ 21, 40 ]");
        }

        {
            var resultExprString =
                ResultAsExpressionString(IntegerEncoding.EncodeSignedInteger(-1));
            resultExprString.Should().Be("[ 18, -20 ]");
        }
    }


    [Fact]
    public void Let_declaration_pattern_triple()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            decl arg =
                let
                    ( alfa, gamma, beta ) =
                        arg
                in
                [ Pine_builtin.int_add
                    [ alfa
                    , 13
                    ]
                , Pine_builtin.int_mul
                    [ beta
                    , 17
                    ]
                , Pine_builtin.int_add
                    [ gamma
                    , 19
                    ]
                ]

            """";

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
            return applyRunResult;
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
            var resultExprString =
                ResultAsExpressionString(
                    PineValue.List(
                        [
                        IntegerEncoding.EncodeSignedInteger(1),
                        IntegerEncoding.EncodeSignedInteger(3),
                        IntegerEncoding.EncodeSignedInteger(5),
                        ]));

            resultExprString.Should().Be("[ 14, 85, 22 ]");
        }

        {
            var resultExprString =
                ResultAsExpressionString(
                    PineValue.List(
                        [
                        IntegerEncoding.EncodeSignedInteger(2),
                        IntegerEncoding.EncodeSignedInteger(4),
                        IntegerEncoding.EncodeSignedInteger(6),
                        ]));

            resultExprString.Should().Be("[ 15, 102, 23 ]");
        }
    }
}
