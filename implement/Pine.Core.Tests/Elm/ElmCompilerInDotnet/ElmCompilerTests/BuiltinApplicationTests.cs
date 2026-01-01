using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.CommonEncodings;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class BuiltinApplicationTests
{
    [Fact]
    public void Int_add_binary()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int
            alfa x =
                Pine_builtin.int_add
                    [ x, 41 ]

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: true,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(
            staticProgram,
            kernelApplicationPrefix: "Pine_builtin");

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Pine_builtin.int_add
                    [ param_1_0
                    , 41
                    ]
            
            """"
            .Trim());

        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var alfaDecl =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var alfaDeclParsed =
            FunctionRecord.ParseFunctionRecordTagged(alfaDecl.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(alfaDecl) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(alfaDeclParsed);

        var (applyRunResult, invocationReports) =
            invokeFunction([IntegerEncoding.EncodeSignedInteger(13)]);

        var resultAsElmValue =
            ElmValueEncoding.PineValueAsElmValue(applyRunResult, null, null)
            .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));

        var resultAsElmExpr =
            ElmValue.RenderAsElmExpression(resultAsElmValue);

        resultAsElmExpr.expressionString.Should().Be(
            "54");
    }

    [Fact]
    public void Int_add_ternary()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa x y =
                Pine_builtin.int_add
                    [ x, 41, y ]

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: true,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(
            staticProgram,
            kernelApplicationPrefix: "Pine_builtin");

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_add
                    [ param_1_0
                    , 41
                    , param_1_1
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Int_mul_binary()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Int -> Int
            alfa x =
                Pine_builtin.int_mul
                    [ x, 17 ]

            """";

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: true,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(
            staticProgram,
            kernelApplicationPrefix: "Pine_builtin");

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Pine_builtin.int_mul
                    [ param_1_0
                    , 17
                    ]
            
            """"
            .Trim());
    }
}
