using AwesomeAssertions;
using Pine.Core.CodeAnalysis;
using Pine.Core.Elm;
using System;
using System.Linq;
using Xunit;

namespace Pine.Core.Tests.Elm.ElmCompilerInDotnet.ElmCompilerTests;

public class SimpleOperatorApplicationTests
{
    [Fact]
    public void Int_mul_parens_int_add()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa x y =
                x * (y + 3)

            """;

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_mul
                    [ param_1_0
                    , Pine_builtin.int_add
                        [ param_1_1
                        , 3
                        ]
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Int_mul_parens_int_sub()
    {
        var elmModuleText =
            """
            module Test exposing (..)


            alfa : Int -> Int -> Int
            alfa x y =
                x * (11 - y)

            """;

        var parseCache = new PineVMParseCache();

        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: true);

        var wholeProgramText =
            ElmCompilerTestHelper.ParseAndRenderStaticProgram(
                parsedEnv,
                includeDeclaration: qualifiedName => qualifiedName.DeclName is "alfa",
                parseCache: parseCache);

        wholeProgramText.Trim().Should().Be(
            """"

            Test.alfa param_1_0 param_1_1 =
                Pine_builtin.int_mul
                    [ param_1_0
                    , Pine_builtin.int_add
                        [ 11
                        , Pine_builtin.int_mul
                            [ -1
                            , param_1_1
                            ]
                        ]
                    ]
            
            """"
            .Trim());
    }

    [Fact]
    public void Float_mixed_mul_sub_add()
    {
        var elmModuleText =
            """"
            module Test exposing (..)


            alfa : Float -> Float -> Float
            alfa x y z =
                (x - z) * (y + 3)

            """";

        var parseCache = new PineVMParseCache();

        // Use the simpler compile method that doesn't require static analysis
        var parsedEnv =
            ElmCompilerTestHelper.CompileElmModules(
                [elmModuleText],
                disableInlining: false);

        // Parsing and rendering not implemented yet, therefore no snapshot test here.


        var testModule =
            parsedEnv.Modules.FirstOrDefault(c => c.moduleName is "Test");

        var declValue =
            testModule.moduleContent.FunctionDeclarations
            .FirstOrDefault(decl => decl.Key is "alfa");

        var declParsed =
            FunctionRecord.ParseFunctionRecordTagged(declValue.Value, parseCache)
            .Extract(err => throw new Exception("Failed parsing " + nameof(declValue) + ": " + err));

        var invokeFunction = ElmCompilerTestHelper.CreateFunctionInvocationDelegate(declParsed);

        PineValue ApplyForArguments(
            PineValue x,
            PineValue y,
            PineValue z)
        {
            var (applyRunResult, _) = invokeFunction([x, y, z]);

            return applyRunResult.ReturnValue.Evaluate();
        }

        ElmValue ApplyForElmArguments(
            ElmValue x,
            ElmValue y,
            ElmValue z)
        {
            var pineValue =
                ApplyForArguments(
                    ElmValueEncoding.ElmValueAsPineValue(x),
                    ElmValueEncoding.ElmValueAsPineValue(y),
                    ElmValueEncoding.ElmValueAsPineValue(z));

            return
                ElmValueEncoding.PineValueAsElmValue(pineValue, null, null)
                .Extract(err => throw new Exception("Failed decoding result as Elm value: " + err));
        }

        {
            var resultValue =
                ApplyForElmArguments(
                    ElmValue.ElmFloat.Convert(1.3),
                    ElmValue.ElmFloat.Convert(2.7),
                    ElmValue.ElmFloat.Convert(0));

            resultValue.Should().Be(ElmValue.ElmFloat.Convert(1.3 * (2.7 + 3.0)));
        }

        {
            var resultValue =
                ApplyForElmArguments(
                    ElmValue.ElmFloat.Convert(-4.0),
                    ElmValue.ElmFloat.Convert(5.5),
                    ElmValue.ElmFloat.Convert(1.5));

            resultValue.Should().Be(ElmValue.ElmFloat.Convert((-4.0 - 1.5) * (5.5 + 3.0)));
        }

        {
            var resultValue =
             ApplyForElmArguments(
                 ElmValue.ElmFloat.Convert(0.0),
                 ElmValue.ElmFloat.Convert(-2.0),
                 ElmValue.ElmFloat.Convert(0.0));

            resultValue.Should().Be(ElmValue.Integer(0));
        }
    }
}
