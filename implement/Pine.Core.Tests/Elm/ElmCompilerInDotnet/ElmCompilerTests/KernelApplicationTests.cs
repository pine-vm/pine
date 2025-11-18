using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using Pine.Core.Interpreter.IntermediateVM;
using Pine.Core.PopularEncodings;
using System;
using System.Collections.Generic;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class KernelApplicationTests
{
    [Fact]
    public void Kernel_application_int_add_binary()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            alfa : Int -> Int
            alfa x =
                Pine_kernel.int_add
                    [ x, 41 ]

            """;

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: true,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Pine_kernel.int_add
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

        var invocationReports = new List<EvaluationReport>();

        var vm =
            ElmCompilerTestHelper.PineVMForProfiling(invocationReports.Add);

        var applyRunResult =
            ElmInteractiveEnvironment.ApplyFunction(
                vm,
                alfaDeclParsed,
                arguments:
                [
                    IntegerEncoding.EncodeSignedInteger(13)
                ])
            .Extract(err => throw new Exception(err));

        var resultAsElmValue =
            ElmValueEncoding.PineValueAsElmValue(applyRunResult, null, null)
            .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));

        var resultAsElmExpr =
            ElmValue.RenderAsElmExpression(resultAsElmValue);

        resultAsElmExpr.expressionString.Should().Be(
            "54");
    }

    [Fact]
    public void Kernel_application_int_add_ternary()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa x y =
                Pine_kernel.int_add
                    [ x, 41, y ]

            """;

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: true,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_kernel.int_add
                    [ param_1_0
                    , 41
                    , param_1_1
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Kernel_application_int_mul_binary()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            alfa : Int -> Int
            alfa x =
                Pine_kernel.int_mul
                    [ x, 17 ]

            """;

        var parseCache = new PineVMParseCache();

        var (parsedEnv, staticProgram) =
            ElmCompilerTestHelper.StaticProgramFromElmModules(
                [elmModuleText],
                disableInlining: true,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        var wholeProgramText = StaticExpressionDisplay.RenderStaticProgram(staticProgram);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 =
                Pine_kernel.int_mul
                    [ param_1_0
                    , 17
                    ]
            
            """"
            .Trim());
    }
}
